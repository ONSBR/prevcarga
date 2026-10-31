#!/usr/bin/env bash
# =============================================================================
# ab_oracle_run.sh -- runner do ORACULO para o A/B do corte (M8.8; ADR-0034, ADR-0035). Dev-only, so Linux, fora do bundle.
#
#   ab_oracle_run.sh <cases.tsv> <ref_out> --data <dir> [--oracle <dir>] [--runtime docker|host] [--image <img>]
#                    [--rscript <Rscript>] [--rlib <dir>] [--allow-untracked <arquivo>] [--limit <n>]
#                    [--b-comb <dir> --b-load <dir> --b-subsystem <SS> --b-window <dias>]
#
# O oraculo (default /opt/ons-source/prevcargadessem3) e SO LEITURA: o runner calcula o checksum da arvore dele (inclusive
# models/*/models/ e .git), copia para um mktemp -d (sem data/, sem models/*/models/, sem .git), roda tudo na COPIA e, no fim, reconfere o
# checksum e o `git status --porcelain` (sem travar o indice) -- diferenca => aborta (exit 3). Um `aws` FALSO no PATH registra a chamada em
# aws.log e sai 1: o log tem que ficar vazio (0 chamadas) ou aborta (exit 3). `data/` entra so por --data (somente leitura; no modo docker
# e montado :ro). Nada e enviado a rede de producao.
#
# Runtime proprio do oraculo (R 4.2.3 + renv.lock dele):
#   --runtime docker (default): num container jammy descartavel, constroi o runtime com o `ci/build_marmita.sh` DA COPIA (apt + rig + renv::restore)
#       e roda os casos com R_LIBS_USER na rlib recem-restaurada. O container so enxerga a copia (rw), o --data (:ro) e o ref_out (rw).
#   --runtime host: usa --rscript e --rlib ja prontos (so para depurar o runner; o veredito registra `runtime: host`).
#
# Cada caso roda em modo LOCAL (LOCAL_DATA_DIR + LOCAL_RESULTS_DIR) com CUT = hora do `corte_oraculo` do cases.tsv e o PROTOCOL do job.json
# do membro (nunca forcado), numa janela FROM=O-1..TO=O (a origem O-1 traz o passo que o oraculo calcula e o dia O descarta). Saidas:
#   <ref_out>/A/results/<mdir>/<AREA>/<RUN_ID>/long.parquet (+ index.tsv, manifest.sha256), <ref_out>/B/ (--b-*), <ref_out>/aws.log,
#   <ref_out>/oracle.sha256, <ref_out>/runtime.txt. Exit: 0 ok; 1 algum caso falhou; 2 uso; 3 oraculo tocado ou aws chamado.
# =============================================================================
set -euo pipefail

PC_SELF="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"

pc_die() { printf 'ab_oracle_run: %s\n' "$*" >&2; exit "${PC_RC:-2}"; }

# hash da arvore (arquivos regulares, ordem C); data/ fica fora (e symlink de maquina)
pc_tree_sum() { (cd "$1" && find . -path ./data -prune -o -type f -print0 | LC_ALL=C sort -z | xargs -0 sha256sum) | sha256sum | cut -d' ' -f1; }

# `git status --porcelain` sem travar/gravar o indice; so as linhas FORA da lista permitida (untracked documentais conhecidos do oraculo)
pc_git_dirty() {
  local ora="$1" allow="$2" line path
  { GIT_OPTIONAL_LOCKS=0 git --no-optional-locks -C "$ora" status --porcelain || true; } | while IFS= read -r line; do
    path="${line:3}"
    if [ -n "$allow" ] && [ -f "$allow" ] && grep -qxF -- "$path" "$allow"; then continue; fi
    printf '%s\n' "$line"
  done
}

pc_sha_manifest() { (cd "$1" && find . -type f ! -name manifest.sha256 -print0 | LC_ALL=C sort -z | xargs -0 -r sha256sum) > "$1/manifest.sha256"; }

# --- roda os casos na copia: run_cases <copy> <cases.tsv> <ref_out> <data> <rscript> <work> <limit>
pc_run_cases() {
  local copy="$1" cases="$2" ref="$3" data="$4" rs="$5" work="$6" limit="$7" n=0 fail=0
  mkdir -p "$ref/A/results" "$work/job" "$work/home"
  printf 'caso\tsigla\tmdir\texogena\tarea\torigem\tresult\trc\n' > "$ref/A/index.tsv"
  # colunas de cases.tsv (ab_cases.R pc_ab_case_cols)
  while IFS=$'\t' read -r caso membro sigla mdir exogena area origem deck tz au corte protocolo split dd exec motivo; do
    [ "$caso" = "caso" ] && continue
    [ "$exogena" = "-" ] && exogena=""
    [ "$exec" = "true" ] || { printf 'ab: caso %s nao executavel: %s\n' "$caso" "$motivo" >&2; continue; }
    if [ "$limit" -gt 0 ] && [ "$n" -ge "$limit" ]; then break; fi
    n=$((n + 1))
    local cut="${corte#*T}:00" from rc=0 key=""
    from="$(date -u -d "$origem -1 day" +%F)"
    local ex=(); [ -n "$exogena" ] && ex=("EXOGENA=$exogena")
    mkdir -p "$ref/A/results"
    key="$(env -i PATH="$PATH" HOME="$work/home" TZ=UTC LC_ALL=C.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 \
        R_LIBS_USER="${PC_RLIB:-}" AB_AWS_LOG="$ref/aws.log" ${ex[@]+"${ex[@]}"} \
        MODEL="$mdir" AREA="$area" FROM="$from" TO="$origem" PROTOCOL="$protocolo" CUT="$cut" \
        LOCAL_DATA_DIR="$data" LOCAL_RESULTS_DIR="$ref/A" WORKDIR="$work/job" RSCRIPT="$rs" \
        bash "$copy/jobs/run_job.sh" 2> "$ref/A/$caso.log" | tail -n 1)" || rc=$?
    [ "$rc" -eq 0 ] || fail=$((fail + 1))
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$caso" "$sigla" "$mdir" "$exogena" "$area" "$origem" \
      "${key#"$ref/A/"}" "$rc" >> "$ref/A/index.tsv"
  done < <(awk -F'\t' -v OFS='\t' '{ for (i = 1; i <= NF; i++) if ($i == "") $i = "-"; print }' "$cases")   # tab e IFS-espaco: campo vazio nao pode sumir
  pc_sha_manifest "$ref/A"
  return "$fail"
}

# --- estagio B (opcional): o reconcile do oraculo em modo full sobre as mesmas folhas, na COPIA
pc_run_b() {
  local copy="$1" ref="$2" rs="$3" comb="$4" load="$5" ss="$6" win="$7" work="$8"
  mkdir -p "$ref/B"
  env -i PATH="$PATH" HOME="$work/home" TZ=UTC LC_ALL=C.UTF-8 R_LIBS_USER="${PC_RLIB:-}" AB_AWS_LOG="$ref/aws.log" \
    "$rs" --vanilla "$copy/scripts/reconcilia_baseline.R" --subsystem "$ss" --mode full --window-days "$win" --eval \
      --comb-dir "$comb" --data-dir "$load" --out-dir "$ref/B" > "$ref/B/reconcile.log" 2>&1
  pc_sha_manifest "$ref/B"
}

# --- modo interno do container: constroi o runtime da copia e roda os casos -----------------------------
if [ "${1:-}" = "--inner" ]; then
  shift
  copy="/work/ora"; cases="$1"; limit="$2"; bcomb="${3:-}"; bload="${4:-}"; bss="${5:-}"; bwin="${6:-}"
  export DEBIAN_FRONTEND=noninteractive
  apt-get update -qq >/dev/null && apt-get install -y -qq --no-install-recommends sudo rsync >/dev/null
  mkdir -p /work/dist /work/rt
  ( cd "$copy" && SRC="$copy" OUT=/work/dist bash ci/build_marmita.sh ) > /ref/runtime-build.log 2>&1 || { echo "build_marmita falhou" >&2; exit 4; }
  tar --zstd -xf /work/dist/prevcarga-runtime-*-linux-x86_64.tar.zst -C /work/rt 2>/dev/null || tar -xf /work/dist/prevcarga-runtime-*.tar.zst -C /work/rt
  PC_RLIB="$(find /work/rt -maxdepth 4 -type d -name rlib | head -n 1)"; export PC_RLIB
  rs="/opt/R/4.2.3/bin/Rscript"
  { "$rs" --version 2>&1; echo "rlib=$PC_RLIB"; } > /ref/runtime.txt
  rc=0; pc_run_cases "$copy" "$cases" /ref /data "$rs" /work/scratch "$limit" || rc=$?
  if [ -n "$bcomb" ]; then pc_run_b "$copy" /ref "$rs" "$bcomb" "$bload" "$bss" "$bwin" /work/scratch || rc=$?; fi
  chown -R "${HOST_UID:-0}:${HOST_GID:-0}" /ref /work 2>/dev/null || true
  exit "$rc"
fi

# --- modo externo ------------------------------------------------------------------------------------------
[ "$#" -ge 2 ] || pc_die "uso: ab_oracle_run.sh <cases.tsv> <ref_out> --data <dir> [opcoes]"
CASES="$(readlink -f "$1")"; REF_OUT="$2"; shift 2
ORA="/opt/ons-source/prevcargadessem3"; DATA=""; RUNTIME="docker"; IMAGE="ubuntu:22.04"; RSCRIPT_HOST=""; RLIB_HOST=""; ALLOW=""; LIMIT=0
BCOMB=""; BLOAD=""; BSS=""; BWIN=""
while [ "$#" -gt 0 ]; do
  case "$1" in
    --oracle) ORA="$2"; shift 2 ;;
    --data) DATA="$2"; shift 2 ;;
    --runtime) RUNTIME="$2"; shift 2 ;;
    --image) IMAGE="$2"; shift 2 ;;
    --rscript) RSCRIPT_HOST="$2"; shift 2 ;;
    --rlib) RLIB_HOST="$2"; shift 2 ;;
    --allow-untracked) ALLOW="$2"; shift 2 ;;
    --limit) LIMIT="$2"; shift 2 ;;
    --b-comb) BCOMB="$2"; shift 2 ;;
    --b-load) BLOAD="$2"; shift 2 ;;
    --b-subsystem) BSS="$2"; shift 2 ;;
    --b-window) BWIN="$2"; shift 2 ;;
    *) pc_die "opcao desconhecida: $1" ;;
  esac
done
[ -f "$CASES" ] || pc_die "cases.tsv nao existe: $CASES"
[ -d "$ORA" ] || pc_die "oraculo nao existe: $ORA"
[ -n "$DATA" ] && [ -d "$DATA" ] || pc_die "--data obrigatorio (diretorio do store)"
DATA="$(readlink -f "$DATA")"; ORA="$(readlink -f "$ORA")"
mkdir -p "$REF_OUT"; REF_OUT="$(readlink -f "$REF_OUT")"
case "$REF_OUT/" in "$ORA"/*) PC_RC=2 pc_die "ref_out dentro do oraculo: $REF_OUT" ;; esac
for t in rsync git sha256sum; do command -v "$t" >/dev/null || pc_die "ferramenta ausente: $t"; done

BEFORE="$(pc_tree_sum "$ORA")"
printf '%s\n' "$BEFORE" > "$REF_OUT/oracle.sha256"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
rsync -a --exclude 'data/' --exclude 'models/*/models/' --exclude '.git/' "$ORA/" "$WORK/ora/"
mkdir -p "$WORK/fakebin" "$WORK/scratch"
cat > "$WORK/fakebin/aws" <<'EOF'
#!/bin/sh
printf '%s\n' "$*" >> "$AB_AWS_LOG"
exit 1
EOF
chmod +x "$WORK/fakebin/aws"
: > "$REF_OUT/aws.log"
export AB_AWS_LOG="$REF_OUT/aws.log"

RC=0
case "$RUNTIME" in
  host)
    [ -x "$RSCRIPT_HOST" ] || pc_die "--runtime host exige --rscript"
    PC_RLIB="$RLIB_HOST"; export PC_RLIB
    { "$RSCRIPT_HOST" --version 2>&1; echo "runtime: host"; echo "rlib=$RLIB_HOST"; } > "$REF_OUT/runtime.txt"
    PATH="$WORK/fakebin:$PATH" pc_run_cases "$WORK/ora" "$CASES" "$REF_OUT" "$DATA" "$RSCRIPT_HOST" "$WORK/scratch" "$LIMIT" || RC=$?
    if [ -n "$BCOMB" ]; then
      PATH="$WORK/fakebin:$PATH" pc_run_b "$WORK/ora" "$REF_OUT" "$RSCRIPT_HOST" "$BCOMB" "$BLOAD" "$BSS" "$BWIN" "$WORK/scratch" || RC=$?
    fi ;;
  docker)
    command -v docker >/dev/null || pc_die "docker ausente (use --runtime host)"
    cp "$CASES" "$WORK/cases.tsv"
    # container descartavel (--rm), so os mounts abaixo; nada de rede de producao alem do apt/P3M do build_marmita.sh
    docker run --rm -e HOST_UID="$(id -u)" -e HOST_GID="$(id -g)" \
      -v "$WORK:/work" -v "$DATA:/data:ro" -v "$REF_OUT:/ref" -v "$PC_SELF:/ab/ab_oracle_run.sh:ro" \
      ${BCOMB:+-v "$BCOMB:/bcomb:ro" -v "$BLOAD:/bload:ro"} \
      -e PATH="/work/fakebin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin" -e AB_AWS_LOG=/ref/aws.log \
      "$IMAGE" bash /ab/ab_oracle_run.sh --inner /work/cases.tsv "$LIMIT" ${BCOMB:+/bcomb /bload "$BSS" "$BWIN"} || RC=$?
    ;;
  *) pc_die "--runtime deve ser docker ou host" ;;
esac

# --- reconferencia do oraculo e do aws falso (sempre, mesmo com caso falho) -----------------------------------
AFTER="$(pc_tree_sum "$ORA")"
if [ "$AFTER" != "$BEFORE" ]; then printf 'ab: oraculo alterado (checksum)\n' >&2; exit 3; fi
DIRTY="$(pc_git_dirty "$ORA" "$ALLOW")"
if [ -n "$DIRTY" ]; then printf 'ab: git status do oraculo sujo:\n%s\n' "$DIRTY" >&2; exit 3; fi
if [ -s "$REF_OUT/aws.log" ]; then printf 'ab: o aws falso foi chamado (%s linhas)\n' "$(wc -l < "$REF_OUT/aws.log")" >&2; exit 3; fi
printf '%s\n' "$AFTER" > "$REF_OUT/oracle.after.sha256"
exit "$RC"
