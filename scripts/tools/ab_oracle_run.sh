#!/usr/bin/env bash
# =============================================================================
# ab_oracle_run.sh -- runner do ORACULO para o A/B do corte (M8.8; ADR-0034, ADR-0035). Dev-only, so Linux, fora do bundle.
#
#   ab_oracle_run.sh <cases.tsv> <ref_out> --data <dir> [--oracle <dir>] [--runtime docker|host] [--image <img>]
#                    [--rscript <Rscript>] [--rlib <dir>] [--allow-untracked <arquivo>] [--limit <n>] [--patches <dir>]
#                    [--b-comb <dir> --b-load <dir> --b-subsystem <SS> --b-window <dias>]
#
# O oraculo (default /opt/ons-source/prevcargadessem3) e SO LEITURA: o runner calcula o checksum da arvore dele (inclusive
# models/*/models/ e .git), copia para um mktemp -d (sem data/, sem models/*/models/, sem .git), aplica os patches de --patches (default
# scripts/tools/ab_oracle_patches/*.patch) SO na COPIA, roda tudo na copia e, no fim, reconfere o checksum e o `git status --porcelain` (sem
# travar o indice) -- diferenca => aborta (exit 3). Um `aws` FALSO no PATH registra a chamada em aws.log e sai 1: o log tem que ficar vazio
# (0 chamadas) ou aborta (exit 3). `data/` entra so por --data (somente leitura; no modo docker e montado :ro). Nada e enviado a rede de producao.
#
# Runtime proprio do oraculo (R 4.2.3 + renv.lock dele):
#   --runtime docker (default): num container jammy descartavel, constroi o runtime com o `ci/build_marmita.sh` DA COPIA (apt + rig + renv::restore)
#       e roda os casos com R_LIBS_USER na rlib recem-restaurada. O container so enxerga a copia (rw), o --data (:ro) e o ref_out (rw). jq e
#       instalado no container: sem ele o job_lib loga "jq absent" e pula o env{} do job.json (o caso entao REPROVA, pc_env_check).
#   --runtime host: usa --rscript e --rlib ja prontos (so para depurar o runner; o veredito registra `runtime: host`).
#
# Cada caso roda POR ORIGEM, com o modo pela classe de split do clean-room (ADR-0023):
#   job   (origin_scoped/dessem/HW): run_job.sh em modo LOCAL, FROM = TO = O, CUT = hora do `corte_oraculo`, PROTOCOL do job.json (nunca
#         forcado). O dia O do HW vem do patch hw_keep_step1 (o passo 1 do fit em O, que o oraculo calcula e descarta) via AB_DAY0_DIR.
#   split (origin_agnostic: RF/LGBM): o run_pipeline.R da copia direto, como os goldens: train em O-1-H, infer em O-1 e O com o mesmo artefato.
# Todo caso confere no log que o env{} do job.json foi exportado (sem "jq absent"). Saidas:
#   <ref_out>/A/results/<mdir>/<AREA>/<RUN_ID>/*.parquet (+ index.tsv com modo e fonte_dia_O, manifest.sha256), <ref_out>/patches.tsv,
#   <ref_out>/B/ (--b-*), <ref_out>/aws.log, <ref_out>/oracle.sha256, <ref_out>/runtime.txt.
#   Exit: 0 ok; 1 algum caso falhou (ou patch nao aplica); 2 uso; 3 oraculo tocado ou aws chamado.
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

# --- env{} do job.json (ADR-0034): o oraculo so aplica o env{} do manifest com jq; sem jq o job.json loga "jq absent" e SEGUE sem o env
# (ex.: LGBM_DISABLE_LOESS=1 do lgbm). Caso cujo log tem "jq absent", ou em que alguma chave do env{} nao foi exportada/mantida, REPROVA.
pc_env_check() {
  local log="$1" manifest="$2" k miss=""
  if grep -q 'jq absent' "$log"; then
    printf '[contract.ab_oracle_env] jq ausente no runtime do oraculo: env{} de %s nao exportado (%s)\n' "$manifest" "$log"; return 1
  fi
  while IFS= read -r k; do
    [ -n "$k" ] || continue
    grep -Eq "manifest env: (export ${k}=|${k} ja setado)" "$log" || miss="$miss $k"
  done < <(jq -r '.env // {} | keys[]' "$manifest")
  if [ -n "$miss" ]; then printf '[contract.ab_oracle_env] env{} de %s nao exportado:%s (%s)\n' "$manifest" "$miss" "$log"; return 1; fi
}

# env{} do job.json como pares K=V (um por linha), com o MESMO log do job_lib (jl_manifest_export_env) no stderr: modo split (sem run_job.sh)
pc_env_pairs() {
  local manifest="$1" k v
  while IFS='=' read -r k v; do
    [ -n "$k" ] || continue
    printf '[ab] manifest env: export %s=%s (default do manifest)\n' "$k" "$v" >&2
    printf '%s=%s\n' "$k" "$v"
  done < <(jq -r '.env // {} | to_entries[] | "\(.key)=\(.value)"' "$manifest")
}

# patches da COPIA (scripts/tools/ab_oracle_patches/*.patch): cabecalho `# ab-patch: mdir=<m> fonte_dia_O=<f>`; aplicados com `patch -p1` so na
# copia descartavel (o oraculo segue byte-identico, conferido pelo checksum). patches.tsv = nome, sha256, mdir, fonte_dia_O.
PC_PATCH_DIR="$(dirname "$PC_SELF")/ab_oracle_patches"
pc_apply_patches() {
  local copy="$1" ref="$2" p m f
  printf 'patch\tsha256\tmdir\tfonte_dia_O\n' > "$ref/patches.tsv"
  [ -d "$PC_PATCH_DIR" ] || return 0
  while IFS= read -r p; do
    m="$(sed -n 's/^# ab-patch: .*mdir=\([^ ]*\).*/\1/p' "$p" | head -n 1)"
    f="$(sed -n 's/^# ab-patch: .*fonte_dia_O=\([^ ]*\).*/\1/p' "$p" | head -n 1)"
    [ -n "$m" ] && [ -n "$f" ] || { printf '[contract.ab_oracle_patch] cabecalho ab-patch ausente: %s\n' "$p" >&2; return 1; }
    patch -p1 -s -N -d "$copy" < "$p" || { printf '[contract.ab_oracle_patch] patch nao aplica na copia: %s\n' "$p" >&2; return 1; }
    printf '%s\t%s\t%s\t%s\n' "$(basename "$p")" "$(sha256sum "$p" | cut -d' ' -f1)" "$m" "$f" >> "$ref/patches.tsv"
  done < <(find "$PC_PATCH_DIR" -maxdepth 1 -name '*.patch' | LC_ALL=C sort)
}

# fonte do dia O declarada por patch para o mdir (vazio = sem patch)
pc_patch_fonte() { awk -F'\t' -v m="$1" 'NR > 1 && $3 == m { print $1 ":" $4; exit }' "$2/patches.tsv"; }

# ambiente minimo de cada filho do oraculo (env -i): o PATH do runner (aws falso na frente), o runtime e a determinacao de threads
pc_child_env() {
  printf '%s\0' PATH="$PATH" HOME="$1/home" TZ=UTC LC_ALL=C.UTF-8 OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1 R_LIBS_USER="${PC_RLIB:-}" AB_AWS_LOG="$2/aws.log"
}

# --- modo job: run_job.sh POR ORIGEM (FROM = TO = O). A janela O-1..O fazia o retrain_harness ancorar o history-start da janela inteira em
# FROM - HIST_TRAIN (origem O com 731 dias, o clean-room usa 730). Patch com sidecar (AB_DAY0_DIR): o dia O que o oraculo calcula e descarta.
# Imprime "result<TAB>fonte_dia_O" (result = caminhos relativos a <ref>/A separados por ';').
pc_case_job() {
  local copy="$1" ref="$2" data="$3" rs="$4" work="$5" caso="$6" mdir="$7" area="$8" origem="$9" cut="${10}" protocolo="${11}" exogena="${12}"
  local log="$ref/A/$caso.log" day0="$work/day0/$caso" key fonte rel dst f0
  local ex=(); [ -n "$exogena" ] && ex=("EXOGENA=$exogena")
  local envv=(); mapfile -d '' envv < <(pc_child_env "$work" "$ref")
  rm -rf "$day0"
  key="$(env -i "${envv[@]}" ${ex[@]+"${ex[@]}"} AB_DAY0_DIR="$day0" \
      MODEL="$mdir" AREA="$area" FROM="$origem" TO="$origem" PROTOCOL="$protocolo" CUT="$cut" \
      LOCAL_DATA_DIR="$data" LOCAL_RESULTS_DIR="$ref/A" WORKDIR="$work/job" RSCRIPT="$rs" \
      bash "$copy/jobs/run_job.sh" 2> "$log" | tail -n 1)" || return 1
  pc_env_check "$log" "$copy/models/$mdir/job.json" >&2 || return 1
  rel="${key#"$ref/A/"}"; fonte="O"
  f0="$(pc_patch_fonte "$mdir" "$ref")"
  if [ -n "$f0" ]; then
    [ -f "$day0/${area}_${origem}.parquet" ] || { printf '[contract.ab_oracle_patch] %s: patch %s nao gravou o dia O (%s)\n' "$caso" "$f0" "$day0" >&2; return 1; }
    dst="$(dirname "$key")/day0_${origem}.parquet"
    cp "$day0/${area}_${origem}.parquet" "$dst"
    rel="$rel;${dst#"$ref/A/"}"; fonte="O:$f0"
  fi
  printf '%s\t%s\n' "$rel" "$fonte"
}

# --- modo split (origin_agnostic: RF/LGBM): o run_pipeline.R da copia direto, como os goldens (plugin_golden.R + tests/parity/oracle_driver.R):
# --mode train UMA vez em train_origin = O-1-H (history_start = train_origin - history_days.train), depois --mode infer em O-1 e em O com o
# MESMO artefato e o MESMO history_start absoluto O - history_days.infer. O `daily` do run_job.sh re-treina por origem (O-1 em O-2-H), entao o
# dia O (d+1 da origem O-1) saia de outro artefato. env{} do job.json exportado aqui (jq) e conferido no log como no modo job.
pc_case_split() {
  local copy="$1" ref="$2" data="$3" rs="$4" work="$5" caso="$6" mdir="$7" area="$8" origem="$9" cut="${10}"
  local man="$copy/models/$mdir/job.json" log="$ref/A/$caso.log" hz htr hin tor ths ihs ent jdir odir rid t rel="" ta=()
  hz="$(jq -r '.horizon_days' "$man")"; htr="$(jq -r '.history_days.train' "$man")"; hin="$(jq -r '.history_days.infer' "$man")"
  ent="$(jq -r '.entrypoint // "run_pipeline.R"' "$man")"
  mapfile -t ta < <(jq -r '.train_args[]? // empty' "$man")
  tor="$(date -u -d "$origem -$((hz + 1)) day" +%F)"; ths="$(date -u -d "$tor -$htr day" +%F)"; ihs="$(date -u -d "$origem -$hin day" +%F)"
  rid="ab_split_${origem}_c${cut:0:2}${cut:3:2}"; jdir="$work/job/$mdir/$area/$rid"; odir="$ref/A/results/$mdir/$area/$rid"
  mkdir -p "$jdir/artdir" "$odir"
  local envv=(); mapfile -d '' envv < <(pc_child_env "$work" "$ref")
  (
    set -e
    printf '[ab] split: caso=%s train_origin=%s history_start=%s -> infer %s e %s (history_start=%s, mesmo artefato)\n' \
      "$caso" "$tor" "$ths" "$(date -u -d "$origem -1 day" +%F)" "$origem" "$ihs" >&2
    local menv=(); mapfile -t menv < <(pc_env_pairs "$man")
    run() { env -i "${envv[@]}" PREVCARGA_JOB=1 RF_ARTIFACT_COMPRESS=none ${menv[@]+"${menv[@]}"} \
              "$rs" --vanilla "$copy/models/$mdir/$ent" "$@" \
              --area "$area" --data-dir "$data" --artifact "$jdir/artifact" --artifact-dir "$jdir/artdir" >&2; }
    run --mode train --origin "$tor" --history-start "$ths" ${ta[@]+"${ta[@]}"}
    for t in "$(date -u -d "$origem -1 day" +%F)" "$origem"; do
      run --mode infer --origin "$t" --history-start "$ihs" --available-until "$t $cut" --out "$odir/long_$t.parquet"
    done
  ) 2> "$log" || return 1
  pc_env_check "$log" "$man" >&2 || return 1
  for t in "$(date -u -d "$origem -1 day" +%F)" "$origem"; do
    [ -f "$odir/long_$t.parquet" ] || { printf '[contract.ab_stage_input] %s: infer em %s sem LONG\n' "$caso" "$t" >&2; return 1; }
    rel="${rel:+$rel;}results/$mdir/$area/$rid/long_$t.parquet"
  done
  printf '%s\t%s\n' "$rel" "O-1:mesmo_artefato(train_origin=$tor)"
}

# --- roda os casos na copia: run_cases <copy> <cases.tsv> <ref_out> <data> <rscript> <work> <limit>
# modo por caso pela classe de split do clean-room (ADR-0023): origin_agnostic => split; senao => job (run_job.sh por origem, protocolo do job.json)
pc_run_cases() {
  local copy="$1" cases="$2" ref="$3" data="$4" rs="$5" work="$6" limit="$7" n=0 fail=0
  mkdir -p "$ref/A/results" "$work/job" "$work/home" "$work/day0"
  printf 'caso\tsigla\tmdir\texogena\tarea\torigem\tresult\trc\tmodo\tfonte_dia_O\n' > "$ref/A/index.tsv"
  # colunas de cases.tsv (ab_cases.R pc_ab_case_cols)
  while IFS=$'\t' read -r caso membro sigla mdir exogena area origem deck tz au corte protocolo split dd exec motivo; do
    [ "$caso" = "caso" ] && continue
    [ "$exogena" = "-" ] && exogena=""
    [ "$exec" = "true" ] || { printf 'ab: caso %s nao executavel: %s\n' "$caso" "$motivo" >&2; continue; }
    if [ "$limit" -gt 0 ] && [ "$n" -ge "$limit" ]; then break; fi
    n=$((n + 1))
    local cut="${corte#*T}:00" rc=0 out="" modo
    if [ "$split" = "origin_agnostic" ]; then
      modo="split"; out="$(pc_case_split "$copy" "$ref" "$data" "$rs" "$work" "$caso" "$mdir" "$area" "$origem" "$cut")" || rc=$?
    else
      modo="job"; out="$(pc_case_job "$copy" "$ref" "$data" "$rs" "$work" "$caso" "$mdir" "$area" "$origem" "$cut" "$protocolo" "$exogena")" || rc=$?
    fi
    if [ "$rc" -ne 0 ]; then fail=$((fail + 1)); out=$'\t'; printf 'ab: caso %s FALHOU (modo %s, rc %s; log %s)\n' "$caso" "$modo" "$rc" "$ref/A/$caso.log" >&2; fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$caso" "$sigla" "$mdir" "$exogena" "$area" "$origem" \
      "${out%%$'\t'*}" "$rc" "$modo" "${out#*$'\t'}" >> "$ref/A/index.tsv"
  done < <(awk -F'\t' -v OFS='\t' '{ for (i = 1; i <= NF; i++) if ($i == "") $i = "-"; print }' "$cases")   # tab e IFS-espaco: campo vazio nao pode sumir
  pc_sha_manifest "$ref/A"
  [ "$fail" -eq 0 ] || { printf 'ab: %s caso(s) falharam\n' "$fail" >&2; return 1; }
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
  apt-get update -qq >/dev/null && apt-get install -y -qq --no-install-recommends sudo rsync jq >/dev/null   # jq: sem ele o job_lib PULA o env{} do job.json
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
    --patches) PC_PATCH_DIR="$2"; shift 2 ;;
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
for t in rsync git sha256sum jq patch; do command -v "$t" >/dev/null || pc_die "ferramenta ausente: $t"; done

BEFORE="$(pc_tree_sum "$ORA")"
printf '%s\n' "$BEFORE" > "$REF_OUT/oracle.sha256"
WORK="$(mktemp -d)"; trap 'rm -rf "$WORK"' EXIT
rsync -a --exclude 'data/' --exclude 'models/*/models/' --exclude '.git/' "$ORA/" "$WORK/ora/"
pc_apply_patches "$WORK/ora" "$REF_OUT" || PC_RC=1 pc_die "patch da copia falhou (o oraculo nao foi tocado)"
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
