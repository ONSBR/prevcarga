#!/usr/bin/env bash
# =============================================================================
# accept_docker.sh -- aceite em Docker LOCAL a partir da imagem do bundle (spec RegDin Linear Refactor, epico E8). Dev-only, so Linux,
# fora do bundle. Nunca usa Batch, ECR, push, login nem AWS: tudo roda na maquina local.
#
# Uso:
#   accept_docker.sh --stage smoke|aceite|medicao|relatorio|all --out DIR --data STORE [--config YAML]
#                    [--ref-bundle TAR --new-bundle TAR] [--jobs 12-14] [--resume] [--format text|json]
#                    [--min-free-gb-docker N] [--min-free-gb-out N] [--receipts-dir DIR]
#   Estagios: smoke, aceite, medicao, relatorio e all (smoke -> aceite -> medicao -> relatorio; o relatorio roda mesmo apos falha de um
#   estagio anterior, para registrar o veredito; smoke falho pula aceite e medicao).
#   --config (default scripts/tools/accept_docker.yaml nos estagios aceite/medicao): jobs, memory, cpus, min_free e ref_rev saem de
#   scripts/tools/accept_cases.R --settings; precedencia flag > YAML > default embutido. Config invalida: contract.accept_bad_config.
#   Estagio aceite: accept_cases.R expande units.aceite.tsv (ondas 1..3; a onda k+1 so abre depois da onda k), cada unidade ganha
#   <out>/units/<unit>/{config.yaml,mounts.txt,overlay/}; as montagens extras sao SO :ro (overlay de topology.yaml e de run.yaml
#   extraidos da propria imagem, artefatos da unidade de treino em /artifacts, deck-inference em /deckinf). Depois accept_delta.R
#   compara backtest S ref x new (delta.tsv) e infer-model train x inference (delta.inference.tsv) e emite aceite.json.
#   Estagio medicao (E8-T3): accept_cases.R --stage medicao expande units.medicao.tsv (origens x areas x mdirs x ref/nova, backtest
#   --origin --area --model) e a fila roda tudo com --jobs. --resume pula a unidade cuja ULTIMA linha em index.tsv tem rc 0, runtime_id igual
#   ao da imagem atual e sha256 dos LONGs recalculado igual ao gravado; senao apaga units/<unit>/state e roda de novo. Recusa classificada
#   (rc 2-15 com error_id) vira celula ausente e NAO derruba a fila; so rc 1 sem envelope ou 137 contam failed (partial, exit 14).
#   Estagio relatorio: scripts/tools/accept_report.R (R do host) grava report.tsv/report.md/report.json em --out, a limpeza roda e o
#   recibo <sha256 do bundle novo>.docker-accept.json (PASS so com smoke, aceite e medicao verdes, relatorio ok, residuo zero e store
#   intacto) vai para --receipts-dir (default <repo>/tests/.receipts). stages.tsv em --out guarda o veredito de cada estagio.
#
# Bundles: sem --ref-bundle/--new-bundle, constroi por build_bundle_linux.sh (ref = --rev de24021, nova = HEAD; arvore suja recusa).
# Com eles, confere o .sha256 ao lado de cada tar.zst. Cada imagem = prevcarga-accept:<RUN_ID>-ref|-new (label prevcarga.accept.run=<RUN_ID>).
# O store (--data) so e montado como :ro e a listagem antes/depois (store.before.tsv/store.after.tsv) tem que ser igual.
#
# Saida: UM JSON em stdout (--format json, default) ou texto ASCII (--format text); mensagens em stderr. Arquivos em --out:
#   run.json, store.before.tsv, store.after.tsv, cache.snapshot, residue.json ({containers,images,cache}), index.tsv (unidades), stages.tsv,
#   report.tsv/report.md/report.json (relatorio). O JSON unico traz report e receipt (caminhos) ao fim do estagio relatorio.
#
# Exits (classe do id em scripts/lib/errors/accept.tsv; tabela em scripts/lib/errors.R):
#   0 ok; 2 contract.accept_bad_args | contract.accept_bad_config | contract.accept_out_forbidden | contract.accept_disk_low |
#   contract.accept_report_input;
#   6 bundle.accept_image_build | bundle.accept_image_smoke; 9 integrity.accept_residue | integrity.accept_store_changed;
#   12 repro.accept_delta_nonzero | repro.accept_unexpected_error | repro.accept_infer_not_faster; 14 partial.accept_units_failed; 130 INT; 143 TERM. (runtime.accept_oom_killed e o error_id de uma unidade rc 137 em index.tsv.)
#
# Regra de limpeza (maquina compartilhada, E8-T4): as imagens saem de um builder buildx DEDICADO por RUN_ID (pc-accept-<RUN_ID>, driver
# docker-container, docker buildx build --load), entao o build cache e do proprio builder e nao se mistura com o do builder default
# (outras sessoes podem estar construindo ao mesmo tempo). O trap pc_accept_cleanup (EXIT/INT/TERM, instalado apos o preflight e antes
# do primeiro build, idempotente) mata os jobs de fundo e remove SO o que esta execucao criou: containers e imagens com o label do
# RUN_ID, o builder dedicado (docker buildx rm --force: container e volume de estado juntos) e a imagem moby/buildkit se ela nao
# existia antes (rmi sem -f: se outro builder a usa, a remocao falha e ela fica). NAO faz prune algum (nem de cache, nem global, nem
# docker system prune) e nunca toca imagem preexistente. Residuo de cache = builder, container ou volume do builder deste RUN_ID
# ainda existentes; cache.snapshot (ids do cache default) e so informativo. Sobra de container/imagem/cache => exit 9
# (integrity.accept_residue), que prevalece sobre o rc original. Nenhuma variavel de ambiente e lida.
# =============================================================================
set -euo pipefail

PC_SELF="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
PC_SELF_DIR="$(dirname "$PC_SELF")"
PC_ACCEPT_DOCKERFILE="$PC_SELF_DIR/accept_docker.Dockerfile"
PC_ACCEPT_REF_REV="de24021"
PC_TOOLS="$PC_SELF_DIR"

declare -A RUNTIME_ID=()
declare -A BUNDLE_PATH=()
declare -A BUNDLE_SHA=()
STAGE="all"; OUT=""; DATA=""; CONFIG=""; REF_BUNDLE=""; NEW_BUNDLE=""; JOBS=""; RESUME=0; FORMAT="json"
MIN_FREE_DOCKER=""; MIN_FREE_OUT=""; MEMORY=8g; CPUS=1; OVERLAY_MDIRS=""; RECEIPTS_DIR=""
REPORT_PATH=""; RECEIPT_PATH=""; REPORT_ID=""; STATE_FILE=""; STAGE_RC=0
DELTA_MAX=""; DELTA_KEYS=""; UNITS_RC=0
RUN_ID=""; BASE=""; BASE_PREEXISTENTE=0; DROOT=""; REPO=""
BUILDER=""; BK_IMAGE="moby/buildkit:buildx-stable-1"; BK_PREEXISTENTE=1
CLEANED=0; TRAP_ARMED=0; OUT_READY=0; FINAL_ID=""; FINAL_RC=0; FINAL_MSG=""
RES_C=0; RES_I=0; RES_K=0; STORE_UNCHANGED=true
UNITS_TOTAL=0; UNITS_OK=0; UNITS_REFUSED=0; UNITS_FAILED=0
CTX_DIRS=()
UNIT_ORIGEM="-"; UNIT_AREA="-"; UNIT_MDIR="-"

# --- erros: id -> classe -> exit (tabela de scripts/lib/errors.R) ------------------------------------------------------
pc_accept_class_exit() {
  case "${1%%.*}" in
    contract) echo 2 ;; bundle) echo 6 ;; runtime) echo 7 ;; integrity) echo 9 ;; repro) echo 12 ;; partial) echo 14 ;;
    *) echo 1 ;;
  esac
}

# monta o JSON unico (jq -c) e escreve em stdout (ou o texto ASCII em --format text)
pc_accept_emit() {
  local rc="$1" verdict=FAIL eid="${FINAL_ID:-}"
  [ "$rc" = 0 ] && verdict=PASS
  if [ "$FORMAT" = text ]; then
    printf 'run_id: %s\nstage: %s\nverdict: %s\nerror_id: %s\nresidue: containers=%s images=%s cache=%s\nstore_unchanged: %s\nunits: total=%s ok=%s refused=%s failed=%s\n' \
      "$RUN_ID" "$STAGE" "$verdict" "${eid:--}" "$RES_C" "$RES_I" "$RES_K" "$STORE_UNCHANGED" \
      "$UNITS_TOTAL" "$UNITS_OK" "$UNITS_REFUSED" "$UNITS_FAILED"
    return 0
  fi
  local imgref="" imgnew="" ridref="${RUNTIME_ID[ref]:-}" ridnew="${RUNTIME_ID[new]:-}"
  [ -n "${BUNDLE_SHA[ref]:-}" ] && imgref="prevcarga-accept:$RUN_ID-ref"
  [ -n "${BUNDLE_SHA[new]:-}" ] && imgnew="prevcarga-accept:$RUN_ID-new"
  jq -nc --arg run_id "$RUN_ID" --arg stage "$STAGE" --arg ir "$imgref" --arg in "$imgnew" --arg rr "$ridref" --arg rn "$ridnew" \
    --argjson ut "$UNITS_TOTAL" --argjson uo "$UNITS_OK" --argjson ur "$UNITS_REFUSED" --argjson uf "$UNITS_FAILED" \
    --argjson c "$RES_C" --argjson i "$RES_I" --argjson k "$RES_K" --argjson su "$STORE_UNCHANGED" \
    --arg verdict "$verdict" --arg eid "$eid" --arg dm "$DELTA_MAX" --arg dk "$DELTA_KEYS" --arg rpt "$REPORT_PATH" --arg rcp "$RECEIPT_PATH" \
    '{run_id:$run_id, stage:$stage,
      images:{ref:(if $ir=="" then null else $ir end), new:(if $in=="" then null else $in end)},
      runtime_id:{ref:(if $rr=="" then null else $rr end), new:(if $rn=="" then null else $rn end)},
      units:{total:$ut, ok:$uo, refused:$ur, failed:$uf},
      delta:(if $dm=="" then null else {max_abs:($dm|tonumber), keys_only:($dk|tonumber)} end),
      residue:{containers:$c, images:$i, cache:$k}, store_unchanged:$su,
      report:(if $rpt=="" then null else $rpt end), receipt:(if $rcp=="" then null else $rcp end),
      verdict:$verdict, error_id:(if $eid=="" then null else $eid end)}'
}

# registra o erro e sai pelo exit da classe. Antes do trap (preflight/args) emite o envelope aqui; depois, o trap EXIT emite.
pc_accept_die() {
  local id="$1" detail="${2:-}" rc
  rc="$(pc_accept_class_exit "$id")"
  FINAL_ID="$id"; FINAL_RC="$rc"
  printf 'accept_docker: [%s] %s\n' "$id" "$detail" >&2
  if [ "$TRAP_ARMED" = 0 ]; then
    if [ "$OUT_READY" = 1 ] && [ ! -e "$OUT/residue.json" ]; then
      jq -n '{containers:0, images:0, cache:0}' > "$OUT/residue.json"
    fi
    pc_accept_emit "$rc"
  fi
  exit "$rc"
}

# --- args ---------------------------------------------------------------------------------------------------------------
pc_accept_parse_args() {
  local a v
  while [ $# -gt 0 ]; do
    a="$1"
    case "$a" in
      --resume) RESUME=1; shift; continue ;;
      --stage|--out|--config|--data|--ref-bundle|--new-bundle|--jobs|--format|--min-free-gb-docker|--min-free-gb-out|--receipts-dir) ;;
      *) pc_accept_die contract.accept_bad_args "opcao desconhecida: $a" ;;
    esac
    [ $# -ge 2 ] || pc_accept_die contract.accept_bad_args "$a sem valor"
    v="$2"; shift 2
    case "$a" in
      --stage) STAGE="$v" ;; --out) OUT="$v" ;; --config) CONFIG="$v" ;; --data) DATA="$v" ;;
      --ref-bundle) REF_BUNDLE="$v" ;; --new-bundle) NEW_BUNDLE="$v" ;; --jobs) JOBS="$v" ;; --format) FORMAT="$v" ;;
      --min-free-gb-docker) MIN_FREE_DOCKER="$v" ;; --min-free-gb-out) MIN_FREE_OUT="$v" ;;
      --receipts-dir) RECEIPTS_DIR="$v" ;;
    esac
  done
  case "$STAGE" in smoke|aceite|medicao|relatorio|all) ;; *) pc_accept_die contract.accept_bad_args "--stage invalido: $STAGE" ;; esac
  case "$FORMAT" in text|json) ;; *) pc_accept_die contract.accept_bad_args "--format invalido: $FORMAT" ;; esac
  case "$JOBS" in ''|12|13|14) ;; *) pc_accept_die contract.accept_bad_args "--jobs deve ser 12, 13 ou 14: $JOBS" ;; esac
  case "$MIN_FREE_DOCKER" in *[!0-9]*) pc_accept_die contract.accept_bad_args "--min-free-gb-docker invalido" ;; esac
  case "$MIN_FREE_OUT" in *[!0-9]*) pc_accept_die contract.accept_bad_args "--min-free-gb-out invalido" ;; esac
  [ -n "$OUT" ] || pc_accept_die contract.accept_bad_args "--out e obrigatorio"
  [ -n "$DATA" ] || pc_accept_die contract.accept_bad_args "--data e obrigatorio"
  [ -d "$DATA" ] || pc_accept_die contract.accept_bad_args "--data nao e diretorio: $DATA"
  if [ -n "$REF_BUNDLE$NEW_BUNDLE" ]; then
    [ -n "$REF_BUNDLE" ] && [ -n "$NEW_BUNDLE" ] || pc_accept_die contract.accept_bad_args "--ref-bundle e --new-bundle andam juntos"
  fi
}

# flag > YAML > default embutido. Sem --config e com estagio smoke nao ha leitura de YAML (defaults); nos demais estagios o YAML
# default fica ao lado do script. accept_cases.R valida o YAML (contract.accept_bad_config) e imprime k=v.
pc_accept_load_settings() {
  local k v line j=13 mem=8g cpu=1 mfd=50 mfo=20 rev="$PC_ACCEPT_REF_REV" ovl=""
  [ -n "$CONFIG" ] || case "$STAGE" in smoke) ;; *) CONFIG="$PC_SELF_DIR/accept_docker.yaml" ;; esac
  if [ -n "$CONFIG" ]; then
    local errf; errf="$(mktemp)"
    line="$(Rscript --vanilla "$PC_TOOLS/accept_cases.R" --config "$CONFIG" --settings 2> "$errf")" \
      || { cat "$errf" >&2; rm -f "$errf"; pc_accept_die contract.accept_bad_config "config invalida: $CONFIG"; }
    rm -f "$errf"
    while IFS='=' read -r k v; do
      case "$k" in
        jobs) j="$v" ;; memory) mem="$v" ;; cpus) cpu="$v" ;; min_free_gb_docker) mfd="$v" ;; min_free_gb_out) mfo="$v" ;;
        ref_rev) rev="$v" ;; overlay_mdirs) ovl="$v" ;;
      esac
    done <<< "$line"
  fi
  [ -n "$JOBS" ] || JOBS="$j"
  [ -n "$MIN_FREE_DOCKER" ] || MIN_FREE_DOCKER="$mfd"
  [ -n "$MIN_FREE_OUT" ] || MIN_FREE_OUT="$mfo"
  MEMORY="$mem"; CPUS="$cpu"; PC_ACCEPT_REF_REV="$rev"; OVERLAY_MDIRS="$ovl"
  case "$JOBS" in 12|13|14) ;; *) pc_accept_die contract.accept_bad_args "--jobs deve ser 12, 13 ou 14: $JOBS" ;; esac
}

# <UTC %Y%m%dT%H%M%SZ>-<8 hex>
pc_accept_run_id() { printf '%s-%s' "$(date -u +%Y%m%dT%H%M%SZ)" "$(od -An -N4 -tx1 /dev/urandom | tr -d ' \n')"; }

# --- preflight: nenhum objeto Docker e criado aqui ------------------------------------------------------------------------
pc_accept_inside() { case "$1/" in "$2"/*|"$2/") return 0 ;; esac; return 1; }

# listagem do store: caminho, tamanho, sha256; LC_ALL=C sort; NAO segue link de diretorio (o store real tem ciclos, ex. .bench_walk)
pc_accept_store_listing() {
  local d="$1" f rel
  find -P "$d" \( -type f -o -type l \) -print0 | LC_ALL=C sort -z | while IFS= read -r -d '' f; do
    rel="${f#"$d"/}"
    if [ -L "$f" ] && [ -d "$f" ]; then
      printf '%s -> %s\t0\t%s\n' "$rel" "$(readlink "$f")" "$(printf '%s' "$(readlink "$f")" | sha256sum | cut -c1-64)"
    elif [ -f "$f" ]; then
      printf '%s\t%s\t%s\n' "$rel" "$(stat -c %s "$f")" "$(sha256sum "$f" | cut -c1-64)"
    else
      printf '%s\t0\t-\n' "$rel"
    fi
  done
}

pc_accept_cache_ids() { docker buildx du --verbose 2>/dev/null | awk '/^ID:/{print $2}' | LC_ALL=C sort; }

pc_accept_free_gb() {
  local p="$1"
  while [ ! -e "$p" ] && [ "$p" != / ]; do p="$(dirname "$p")"; done
  df -Pk "$p" 2>/dev/null | awk 'NR==2{print int($4/1048576)}'
}

pc_accept_preflight() {
  local t p
  for t in docker jq sha256sum Rscript flock; do
    command -v "$t" >/dev/null 2>&1 || pc_accept_die contract.accept_bad_args "ferramenta ausente no host: $t"
  done
  [ -f "$PC_TOOLS/accept_cases.R" ] || pc_accept_die contract.accept_bad_args "accept_cases.R ausente em $PC_TOOLS"
  pc_accept_load_settings
  OUT="$(readlink -m "$OUT")"; DATA="$(readlink -f "$DATA")"
  REPO="$(git -C "$PC_SELF_DIR" rev-parse --show-toplevel)"
  DROOT="$(readlink -m "$(docker info --format '{{.DockerRootDir}}')")"
  for p in "$DATA" "$REPO" "$DROOT"; do
    if pc_accept_inside "$OUT" "$p"; then pc_accept_die contract.accept_out_forbidden "--out=$OUT dentro de $p"; fi
  done
  mkdir -p "$OUT"; OUT_READY=1
  local fd fo
  fd="$(pc_accept_free_gb "$DROOT")"; fo="$(pc_accept_free_gb "$OUT")"
  [ "${fd:-0}" -ge "$MIN_FREE_DOCKER" ] || pc_accept_die contract.accept_disk_low "DockerRootDir $DROOT livre ${fd:-0} GB"
  [ "${fo:-0}" -ge "$MIN_FREE_OUT" ] || pc_accept_die contract.accept_disk_low "--out $OUT livre ${fo:-0} GB"
  pc_accept_store_listing "$DATA" > "$OUT/store.before.tsv"
  BASE="$(sed -n 's/^ARG BASE=//p' "$PC_ACCEPT_DOCKERFILE")"
  if docker image inspect "$BASE" >/dev/null 2>&1; then BASE_PREEXISTENTE=1; else BASE_PREEXISTENTE=0; fi
  if docker image inspect "$BK_IMAGE" >/dev/null 2>&1; then BK_PREEXISTENTE=1; else BK_PREEXISTENTE=0; fi
  pc_accept_cache_ids > "$OUT/cache.snapshot"   # informativo: o build cache do builder default nao e nosso nem entra no residuo
}

# --- limpeza -------------------------------------------------------------------------------------------------------------
# builder dedicado, container e volume de estado deste RUN_ID ainda existentes (o cache do builder default NAO conta)
pc_accept_builder_residue() {
  local n=0 c v
  [ -n "$BUILDER" ] || { echo 0; return 0; }
  if docker buildx ls 2>/dev/null | awk -v b="$BUILDER" '$1==b{f=1} END{exit f?0:1}'; then n=$((n + 1)); fi
  c="$(docker ps -aq --filter "name=buildx_buildkit_${BUILDER}" 2>/dev/null | wc -l)"
  v="$(docker volume ls -q --filter "name=buildx_buildkit_${BUILDER}" 2>/dev/null | wc -l)"
  echo $((n + ${c// /} + ${v// /}))
}

pc_accept_residue() {
  local f="label=prevcarga.accept.run=$RUN_ID"
  RES_C="$(docker ps -aq --filter "$f" 2>/dev/null | wc -l)"
  RES_I="$({ docker images -q --filter "$f"; docker images -q --filter "reference=prevcarga-accept:$RUN_ID-*"; } 2>/dev/null | LC_ALL=C sort -u | wc -l)"
  RES_K="$(pc_accept_builder_residue)"
  RES_C="${RES_C// /}"; RES_I="${RES_I// /}"; RES_K="${RES_K// /}"
  jq -n --argjson c "$RES_C" --argjson i "$RES_I" --argjson k "$RES_K" '{containers:$c, images:$i, cache:$k}' > "$OUT/residue.json"
  if [ -s "$OUT/store.before.tsv" ] || [ -e "$OUT/store.before.tsv" ]; then
    pc_accept_store_listing "$DATA" > "$OUT/store.after.tsv"
    if ! cmp -s "$OUT/store.before.tsv" "$OUT/store.after.tsv"; then STORE_UNCHANGED=false; fi
  fi
  return 0
}

pc_accept_cleanup() {
  [ "${CLEANED:-0}" = 1 ] && return 0
  CLEANED=1
  local f="label=prevcarga.accept.run=$RUN_ID" d
  jobs -p | xargs -r kill 2>/dev/null || true
  docker ps -aq --filter "$f" 2>/dev/null | xargs -r docker rm -f >/dev/null 2>&1 || true
  { docker images -q --filter "$f"; docker images -q --filter "reference=prevcarga-accept:$RUN_ID-*"; } 2>/dev/null \
    | LC_ALL=C sort -u | xargs -r docker rmi -f >/dev/null 2>&1 || true
  if [ -n "$BUILDER" ]; then docker buildx rm --force "$BUILDER" >/dev/null 2>&1 || true; fi
  if [ "$BK_PREEXISTENTE" = 0 ]; then docker rmi "$BK_IMAGE" >/dev/null 2>&1 || true; fi   # sem -f: se outro builder a usa, fica
  for d in ${CTX_DIRS[@]+"${CTX_DIRS[@]}"}; do rm -rf "$d"; done
  pc_accept_residue
}

# trap EXIT: rc original -> limpeza -> reconferencia -> exit final (9 prevalece sobre o rc original)
pc_accept_on_exit() {
  local rc=$?
  trap - TERM INT EXIT
  set +e
  pc_accept_cleanup
  if [ $((RES_C + RES_I + RES_K)) -gt 0 ]; then
    FINAL_ID=integrity.accept_residue; rc=9
    printf 'accept_docker: [integrity.accept_residue] containers=%s images=%s cache=%s (veja %s/residue.json)\n' "$RES_C" "$RES_I" "$RES_K" "$OUT" >&2
  elif [ "$STORE_UNCHANGED" = false ]; then
    FINAL_ID=integrity.accept_store_changed; rc=9
    printf 'accept_docker: [integrity.accept_store_changed] compare %s/store.before.tsv e store.after.tsv\n' "$OUT" >&2
  fi
  pc_accept_emit "$rc"
  exit "$rc"
}

pc_accept_on_signal() {
  case "$1" in INT) exit 130 ;; *) exit 143 ;; esac
}

# roda um comando em segundo plano e espera: o trap de sinal dispara na hora (em primeiro plano o bash adia o trap)
pc_accept_bg() {
  local p rc=0
  "$@" &
  p=$!
  wait "$p" || rc=$?
  return "$rc"
}

# --- bundles e imagens --------------------------------------------------------------------------------------------------
pc_accept_check_bundle() {
  local tar="$1" sha="$1.sha256"
  [ -f "$tar" ] && [ -f "$sha" ] || pc_accept_die contract.accept_bad_args "bundle ou .sha256 ausente: $tar"
  (cd "$(dirname "$tar")" && sha256sum -c "$(basename "$sha")" >/dev/null 2>&1) \
    || pc_accept_die contract.accept_bad_args "sha256 do bundle nao confere: $tar"
}

# runtime_id do bundle: o campo .runtime_id do RUNTIME.json, se houver; senao prevcarga_runtime_id() (runtime.R do repo) sobre o RUNTIME.json
pc_accept_runtime_id() {
  local f="$1" id
  id="$(jq -r '.runtime_id // empty' "$f" 2>/dev/null || true)"
  if [ -z "$id" ] && [ -f "$f" ]; then
    id="$(Rscript --vanilla -e 'a <- commandArgs(TRUE); source(a[1]); cat(prevcarga_runtime_id(a[2]), "\n", sep = "")' \
      "$REPO/scripts/lib/runtime.R" "$f" 2>/dev/null | tail -n 1 || true)"
  fi
  printf '%s' "${id:--}"
}

pc_accept_bundles() {
  local ver tar
  if [ -n "$REF_BUNDLE" ]; then
    BUNDLE_PATH[ref]="$(readlink -f "$REF_BUNDLE")"; BUNDLE_PATH[new]="$(readlink -f "$NEW_BUNDLE")"
  else
    mkdir -p "$OUT/bundles/ref" "$OUT/bundles/new"
    pc_accept_bg "$PC_SELF_DIR/build_bundle_linux.sh" accept-ref --out "$OUT/bundles/ref" --rev "$PC_ACCEPT_REF_REV" >&2 \
      || pc_accept_die bundle.accept_image_build "build do bundle ref ($PC_ACCEPT_REF_REV) falhou"
    pc_accept_bg "$PC_SELF_DIR/build_bundle_linux.sh" accept-new --out "$OUT/bundles/new" --rev HEAD >&2 \
      || pc_accept_die bundle.accept_image_build "build do bundle nova (HEAD) falhou"
    BUNDLE_PATH[ref]="$OUT/bundles/ref/prevcarga-accept-ref-linux-x86_64.tar.zst"
    BUNDLE_PATH[new]="$OUT/bundles/new/prevcarga-accept-new-linux-x86_64.tar.zst"
  fi
  for ver in ref new; do
    tar="${BUNDLE_PATH[$ver]}"
    pc_accept_check_bundle "$tar"
    BUNDLE_SHA[$ver]="$(cut -c1-64 "$tar.sha256" | head -n 1)"
    RUNTIME_ID[$ver]="$(pc_accept_runtime_id "${tar%.tar.zst}.RUNTIME.json")"
  done
}

pc_accept_build_image() {
  local ver="$1" ctx img="prevcarga-accept:$RUN_ID-$1"
  ctx="$(mktemp -d)"; CTX_DIRS+=("$ctx")
  cp "${BUNDLE_PATH[$ver]}" "$ctx/bundle.tar.zst"
  printf '%s  bundle.tar.zst\n' "${BUNDLE_SHA[$ver]}" > "$ctx/bundle.tar.zst.sha256"
  pc_accept_bg docker buildx build --builder "$BUILDER" --load --label "prevcarga.accept.run=$RUN_ID" -t "$img" -f "$PC_ACCEPT_DOCKERFILE" "$ctx" >&2 \
    || pc_accept_die bundle.accept_image_build "docker buildx build da imagem $img falhou (builder $BUILDER)"
  rm -rf "$ctx"
}

# builder buildx dedicado deste RUN_ID (driver docker-container); o build cache fica dentro dele e sai com o buildx rm do cleanup
pc_accept_builder_create() {
  BUILDER="pc-accept-$RUN_ID"
  pc_accept_bg docker buildx create --name "$BUILDER" --driver docker-container >/dev/null \
    || pc_accept_die bundle.accept_image_build "docker buildx create do builder $BUILDER falhou"
}

pc_accept_write_run_json() {
  jq -n --arg run_id "$RUN_ID" --arg base "$BASE" --argjson pre "$BASE_PREEXISTENTE" \
    --arg rp "${BUNDLE_PATH[ref]:-}" --arg rs "${BUNDLE_SHA[ref]:-}" --arg np "${BUNDLE_PATH[new]:-}" --arg ns "${BUNDLE_SHA[new]:-}" \
    --arg ir "prevcarga-accept:$RUN_ID-ref" --arg in "prevcarga-accept:$RUN_ID-new" \
    --arg rr "${RUNTIME_ID[ref]:--}" --arg rn "${RUNTIME_ID[new]:--}" \
    '{run_id:$run_id, base:$base, base_preexistente:($pre==1), bundles:{ref:{path:$rp, sha256:$rs}, new:{path:$np, sha256:$ns}},
      images:{ref:$ir, new:$in}, runtime_id:{ref:$rr, new:$rn}}' > "$OUT/run.json"
}

# --- executor de unidade e fila ------------------------------------------------------------------------------------------
# pc_accept_run_unit <unit_dir> <versao> <n> <mounts_file> <verbo...>   (usa DATA, OUT, RUN_ID, RUNTIME_ID, UNIT_ORIGEM/AREA/MDIR)
pc_accept_run_unit() {
  local u="$1" ver="$2" n="$3" mf="$4" img rc=0 m t0 eid longs sha secs
  shift 4
  mkdir -p "$u/tmp" "$u/state"
  img="prevcarga-accept:$RUN_ID-$ver"
  local extra=()
  if [ -n "$mf" ] && [ -s "$mf" ]; then
    while IFS= read -r m; do
      [ -n "$m" ] || continue
      case "$m" in *:ro) extra+=(-v "$m") ;; *) return 2 ;; esac
      if [ ! -e "${m%%:*}" ]; then   # docker -v criaria o diretorio ausente como root: a unidade falha sem rodar
        printf 'accept_docker: montagem ausente no host: %s\n' "${m%%:*}" > "$u/stderr.log"; rc=1
      fi
    done < "$mf"
  fi
  t0="$(date -u +%s)"
  if [ "$rc" = 0 ]; then
    docker run --rm --name "pc-accept-$RUN_ID-$n" --label "prevcarga.accept.run=$RUN_ID" \
      --network none --memory "$MEMORY" --cpus "$CPUS" --user "$(id -u):$(id -g)" \
      -v "$DATA:/data:ro" -v "$u:/out" -v "$u/tmp:/tmp" ${extra[@]+"${extra[@]}"} "$img" "$@" \
      --config=/out/config.yaml --state=/out/state --format=json < /dev/null > "$u/result.json" 2> "$u/stderr.log" || rc=$?
  else
    : > "$u/result.json"
  fi
  secs=$(( $(date -u +%s) - t0 ))
  eid="$(jq -r '.error.id // "-"' "$u/result.json" 2>/dev/null || true)"; [ -n "$eid" ] || eid="-"
  [ "$rc" = 137 ] && eid="runtime.accept_oom_killed"
  longs="$(cd "$u" && find state -type f -name '*.parquet' 2>/dev/null | LC_ALL=C sort | paste -sd, -)"
  if [ -n "$longs" ]; then
    sha="$(cd "$u" && printf '%s\n' "${longs//,/$'\n'}" | xargs -d '\n' sha256sum | sha256sum | cut -c1-64)"
  else
    sha="-"
  fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$(basename "$u")" "$ver" "$img" "${RUNTIME_ID[$ver]:--}" \
    "$UNIT_ORIGEM" "$UNIT_AREA" "$UNIT_MDIR" "$rc" "$eid" "$secs" "${longs:--}" "$sha" > "$u/index.row"
  flock "$OUT/index.lock" sh -c '[ -s "$2" ] || printf "unit\tversao\timage\truntime_id\torigem\tarea\tmdir\trc\terror_id\tsegundos\tlong_paths\tsha256\n" >> "$2"; cat "$1" >> "$2"' \
    _ "$u/index.row" "$OUT/index.tsv"
  return 0
}

# pc_accept_queue <units.tsv> <jobs>
# units.tsv (sem cabecalho, TAB): wave unit_dir versao n mounts_file origem area mdir verbo(+args, separados por espaco)
pc_accept_queue() {
  local tsv="$1" jobs_max="$2" wave w unit ver n mf origem area mdir verb rc row_before running=0
  local waves; waves="$(cut -f1 "$tsv" | LC_ALL=C sort -un)"
  row_before="$(wc -l < "$OUT/index.tsv" 2>/dev/null || echo 0)"
  UNITS_TOTAL=0
  for w in $waves; do
    while IFS=$'\t' read -r wave unit ver n mf origem area mdir verb; do
      [ "$wave" = "$w" ] || continue
      UNITS_TOTAL=$((UNITS_TOTAL + 1))
      while [ "$running" -ge "$jobs_max" ]; do wait -n || true; running=$((running - 1)); done
      (
        UNIT_ORIGEM="$origem"; UNIT_AREA="$area"; UNIT_MDIR="$mdir"
        # shellcheck disable=SC2086
        pc_accept_run_unit "$unit" "$ver" "$n" "$mf" $verb
      ) &
      running=$((running + 1))
    done < "$tsv"
    while [ "$running" -gt 0 ]; do wait -n || true; running=$((running - 1)); done
  done
  UNITS_OK=0; UNITS_REFUSED=0; UNITS_FAILED=0
  while IFS=$'\t' read -r _ _ _ _ _ _ _ rc eid _; do
    if [ "$rc" = 0 ]; then UNITS_OK=$((UNITS_OK + 1))
    elif [ "$rc" -ge 2 ] && [ "$rc" -le 15 ] && [ "$eid" != "-" ]; then UNITS_REFUSED=$((UNITS_REFUSED + 1))
    else UNITS_FAILED=$((UNITS_FAILED + 1)); fi
  done < <(tail -n +"$((row_before + 1))" "$OUT/index.tsv" 2>/dev/null | grep -v '^unit	' || true)
  [ "$UNITS_FAILED" -eq 0 ] || return 14
  return 0
}

# --- estagio smoke ------------------------------------------------------------------------------------------------------
pc_accept_stage_smoke() {
  local ver img c net lbl="prevcarga.accept.run=$RUN_ID"
  for ver in ref new; do
    img="prevcarga-accept:$RUN_ID-$ver"; c="pc-accept-$RUN_ID-smoke-$ver"
    pc_accept_bg docker create --name "$c" --label "$lbl" --network none "$img" doctor --format=json >/dev/null \
      || pc_accept_die bundle.accept_image_smoke "docker create do doctor falhou ($ver)"
    net="$(docker inspect --format '{{.HostConfig.NetworkMode}}' "$c" 2>/dev/null || true)"
    [ "$net" = none ] || pc_accept_die bundle.accept_image_smoke "container do smoke sem --network none ($ver): $net"
    pc_accept_bg docker start -a "$c" > "$OUT/smoke-$ver.json" 2> "$OUT/smoke-$ver.err" \
      || pc_accept_die bundle.accept_image_smoke "prevcarga doctor falhou na imagem $ver (veja $OUT/smoke-$ver.err)"
    docker rm -f "$c" >/dev/null 2>&1 || true
    pc_accept_bg docker run --rm --name "pc-accept-$RUN_ID-tz-$ver" --label "$lbl" --network none --entrypoint sh "$img" \
      -c 'test -e /usr/share/zoneinfo/America/Sao_Paulo && ! command -v aws' >/dev/null 2>&1 \
      || pc_accept_die bundle.accept_image_smoke "imagem $ver sem tzdata (America/Sao_Paulo) ou com aws"
  done
}

# --- estagio aceite -----------------------------------------------------------------------------------------------------
# argumentos do verbo de uma unidade (sem --config/--state/--format, que o executor acrescenta)
# pc_accept_unit_argv <versao> <verbo> <origem> <area> <mdir> <data_flag> <artifacts_from>
pc_accept_unit_argv() {
  local ver="$1" verbo="$2" origem="$3" area="$4" mdir="$5" flag="$6" art="$7" a data=/data
  [ "$flag" = --data-inference ] && data=/deckinf
  case "$verbo" in
    backtest) a="backtest --origin=$origem --area=$area"; [ "$mdir" = - ] || a="$a --model=$mdir" ;;
    train-models) a="train-models --origin=$origem --area=$area --model=$mdir" ;;
    infer-model) a="infer-model --origin=$origem --area=$area --model=${mdir%%/*} --variant=${mdir#*/} --out=/out/long.parquet" ;;
    deck-inference) a="deck-inference --origin=$origem --area=$area --out=/out/deck" ;;
    *) pc_accept_die contract.accept_bad_config "verbo desconhecido em units: $verbo" ;;
  esac
  a="$a $flag=$data"
  if [ "$art" != - ]; then
    if [ "$ver" = ref ]; then a="$a --artifacts-dir=/artifacts"; else a="$a --data-artifacts=/artifacts"; fi
  fi
  printf '%s' "$a"
}

# extrai da PROPRIA imagem o arquivo <caminho no bundle> para <destino> (docker run --rm --network none --entrypoint cat)
pc_accept_extract() {
  local ver="$1" src="$2" dst="$3"
  [ -s "$dst" ] && return 0
  mkdir -p "$(dirname "$dst")"
  pc_accept_bg docker run --rm --name "pc-accept-$RUN_ID-x$(basename "$(dirname "$dst")")-$(basename "$dst" .yaml)" --label "prevcarga.accept.run=$RUN_ID" \
    --network none --entrypoint cat "prevcarga-accept:$RUN_ID-$ver" "$src" > "$dst" 2>/dev/null \
    || pc_accept_die bundle.accept_image_smoke "nao extraiu $src da imagem $ver"
  [ -s "$dst" ] || pc_accept_die bundle.accept_image_smoke "arquivo vazio extraido da imagem $ver: $src"
}

# overlay somente leitura de uma unidade: pc_accept_overlay <kind> <entrada> <saida>
pc_accept_overlay() {
  Rscript --vanilla "$PC_TOOLS/accept_cases.R" --overlay "$1" --in "$2" --out "$3" >/dev/null \
    || pc_accept_die contract.accept_bad_config "overlay $1 falhou para $2"
}

# prepara <out>/units/<unit>: config.yaml (schema 1, format json), overlays e mounts.txt (SO :ro)
# pc_accept_prepare_unit <unit> <versao> <stage> <origem> <area> <topologia> <janela> <data_flag> <artifacts_from>
pc_accept_prepare_unit() {
  local unit="$1" ver="$2" stage="$3" origem="$4" area="$5" topo="$6" janela="$7" flag="$8" art="$9" u m
  u="$OUT/units/$unit"
  mkdir -p "$u/tmp" "$u/state" "$u/overlay"
  printf 'schema_version: 1\nformat: json\n' > "$u/config.yaml"
  : > "$u/mounts.txt"
  if [ "$topo" = sem_lin_uni ]; then
    pc_accept_extract "$ver" /opt/prevcarga/config/topology.yaml "$OUT/overlay-src/$ver/topology.yaml"
    pc_accept_overlay topologia "$OUT/overlay-src/$ver/topology.yaml" "$u/overlay/topology.yaml"
    printf '%s\n' "$u/overlay/topology.yaml:/opt/prevcarga/config/topology.yaml:ro" >> "$u/mounts.txt"
  fi
  if [ "$janela" = referencia ]; then
    for m in ${OVERLAY_MDIRS//,/ }; do
      pc_accept_extract "$ver" "/opt/prevcarga/models/$m/run.yaml" "$OUT/overlay-src/$ver/$m/run.yaml"
      pc_accept_overlay janela "$OUT/overlay-src/$ver/$m/run.yaml" "$u/overlay/$m.run.yaml"
      printf '%s\n' "$u/overlay/$m.run.yaml:/opt/prevcarga/models/$m/run.yaml:ro" >> "$u/mounts.txt"
    done
  fi
  [ "$art" = - ] || printf '%s\n' "$OUT/units/$art/state/artifacts:/artifacts:ro" >> "$u/mounts.txt"
  [ "$flag" = --data-inference ] || return 0
  printf '%s\n' "$OUT/units/$stage-new-deck-inference-$area-$origem/deck:/deckinf:ro" >> "$u/mounts.txt"
}

# preparo de todas as unidades de um units.tsv e a fila (sem cabecalho, ver pc_accept_queue): pc_accept_prepare_all <units.tsv> <fila.tsv>
pc_accept_prepare_all() {
  local tsv="$1" q="$2" n=0 unit stage wave ver origem area mdir verbo topo janela flag art
  : > "$q"
  while IFS=$'\t' read -r unit stage wave ver origem area mdir verbo topo janela flag art; do
    [ "$unit" = unit ] && continue
    n=$((n + 1))
    pc_accept_prepare_unit "$unit" "$ver" "$stage" "$origem" "$area" "$topo" "$janela" "$flag" "$art"
    printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$wave" "$OUT/units/$unit" "$ver" "$n" "$OUT/units/$unit/mounts.txt" \
      "$origem" "$area" "$mdir" "$(pc_accept_unit_argv "$ver" "$verbo" "$origem" "$area" "$mdir" "$flag" "$art")" >> "$q"
  done < "$tsv"
}

# recusa classificada (rc 2..15, exceto 14 = partial do REDUCE, com error_id) e RESULTADO, nao defeito: nao entra no delta
pc_accept_unit_refused() {
  awk -F'\t' -v u="$1" '$1==u && $8>=2 && $8<=15 && $8!=14 && $9!="-"{f=1} END{exit f?0:1}' "$OUT/index.tsv" 2>/dev/null
}

# max_abs e keys_only agregados dos delta*.tsv de <out> para o JSON unico
pc_accept_delta_totals() {
  local f; local files=()
  for f in "$OUT/delta.tsv" "$OUT/delta.inference.tsv"; do [ -s "$f" ] && files+=("$f"); done
  [ "${#files[@]}" -gt 0 ] || return 0
  read -r DELTA_MAX DELTA_KEYS < <(awk -F'\t' 'FNR>1{v=($7=="Inf")?1e308:$7+0; if(v>m)m=v; k+=$5+$6} END{printf "%.17g %d\n", m+0, k+0}' "${files[@]}")
}

# um par do delta: pc_accept_delta_pair <unit ref> <unit new> <saida tsv> -> rc 0|12|outro; id do erro em DELTA_ID
pc_accept_delta_pair() {
  local ur="$1" un="$2" out="$3" rc=0 res="$3.result.json"
  if pc_accept_unit_refused "$ur" || pc_accept_unit_refused "$un"; then
    printf 'accept_docker: par %s x %s com recusa classificada; fora do delta\n' "$ur" "$un" >&2
    return 0
  fi
  Rscript --vanilla "$PC_TOOLS/accept_delta.R" --ref "$OUT/units/$ur" --new "$OUT/units/$un" --out "$out" > "$res" || rc=$?
  [ "$rc" = 0 ] || DELTA_ID="$(jq -r '.error_id // empty' "$res" 2>/dev/null || true)"
  return "$rc"
}

pc_accept_stage_aceite() {
  local tsv="$OUT/units.aceite.tsv" q="$OUT/queue.aceite.tsv" rc1=0 rc2=0 rc3=0 id="" detail="" o sub ta
  Rscript --vanilla "$PC_TOOLS/accept_cases.R" --config "$CONFIG" --stage aceite --out "$tsv" >/dev/null \
    || pc_accept_die contract.accept_bad_config "accept_cases.R recusou $CONFIG"
  pc_accept_prepare_all "$tsv" "$q"
  UNITS_RC=0
  pc_accept_queue "$q" "$JOBS" || UNITS_RC=$?
  read -r o sub < <(awk -F'\t' 'NR>1 && $8=="backtest" && $4=="ref"{print $5, $6; exit}' "$tsv")
  read -r ta < <(awk -F'\t' 'NR>1 && $8=="infer-model" && $11=="--data-inference"{print $6; exit}' "$tsv")
  DELTA_ID=""
  pc_accept_delta_pair "aceite-ref-backtest-$sub-$o" "aceite-new-backtest-$sub-$o" "$OUT/delta.tsv" || rc1=$?
  [ -n "$DELTA_ID" ] && id="$DELTA_ID"; DELTA_ID=""
  pc_accept_delta_pair "aceite-new-infer-model-$ta-$o-train" "aceite-new-infer-model-$ta-$o-inference" "$OUT/delta.inference.tsv" || rc2=$?
  [ -z "$id" ] && id="$DELTA_ID"
  Rscript --vanilla "$PC_TOOLS/accept_delta.R" --verdict --index "$OUT/index.tsv" --units "$tsv" --out "$OUT/aceite.json" \
    > "$OUT/aceite.result.json" || rc3=$?
  pc_accept_delta_totals
  if [ "$rc3" != 0 ] && [ "$rc3" != 12 ]; then pc_accept_die contract.accept_bad_args "accept_delta.R --verdict rc=$rc3"; fi
  if [ "$rc3" = 12 ]; then
    detail="$(jq -r '.error_id // empty' "$OUT/aceite.result.json" 2>/dev/null || true)"
    pc_accept_die "${detail:-repro.accept_unexpected_error}" "veredito do aceite (veja $OUT/aceite.json)"
  fi
  if [ "$rc1" = 12 ] || [ "$rc2" = 12 ]; then pc_accept_die "${id:-repro.accept_delta_nonzero}" "delta ref x new (veja $OUT/delta.tsv)"; fi
  if [ "$rc1" != 0 ] || [ "$rc2" != 0 ]; then pc_accept_die contract.accept_bad_args "accept_delta.R rc=$rc1/$rc2 (veja $OUT/delta.tsv.result.json)"; fi
  if [ "$UNITS_RC" = 14 ]; then pc_accept_die partial.accept_units_failed "unidades failed (veja $OUT/index.tsv)"; fi
  return 0
}

# --- estagio medicao ------------------------------------------------------------------------------------------------------
# $1 = unit; 0 = pular (ultima linha em index.tsv com rc 0, runtime_id da imagem atual e sha256 dos LONGs recalculado igual ao gravado).
# Divergencia: apaga units/<unit>/state e devolve 1 (a unidade roda de novo).
pc_accept_resume_skip() {
  local u="$1" row ver rid rc longs sha now
  row="$(awk -F'\t' -v u="$u" '$1==u' "$OUT/index.tsv" 2>/dev/null | tail -n 1)"
  [ -n "$row" ] || return 1
  IFS=$'\t' read -r _ ver _ rid _ _ _ rc _ _ longs sha <<< "$row"
  [ "$rc" = 0 ] && [ "$rid" = "${RUNTIME_ID[$ver]:-}" ] && [ "${longs:--}" != - ] || return 1
  now="$(cd "$OUT/units/$u" 2>/dev/null && printf '%s\n' "${longs//,/$'\n'}" | xargs -d '\n' sha256sum 2>/dev/null | sha256sum | cut -c1-64)" || now=""
  [ -n "$now" ] && [ "$now" = "$sha" ] && return 0
  rm -rf "$OUT/units/$u/state"
  return 1
}

# totais de <units.tsv> pela ULTIMA linha de cada unidade em index.tsv: UNITS_TOTAL/OK/REFUSED/FAILED (sem linha = failed)
pc_accept_units_totals() {
  local tsv="$1"
  read -r UNITS_TOTAL UNITS_OK UNITS_REFUSED UNITS_FAILED < <(awk -F'\t' '
    NR==FNR { if (FNR>1) want[$1]=1; next }
    FNR>1 && ($1 in want) { rc[$1]=$8; eid[$1]=$9 }
    END { t=0; o=0; r=0; f=0
      for (u in want) { t++
        if (!(u in rc)) f++
        else if (rc[u]==0) o++
        else if (rc[u]>=2 && rc[u]<=15 && eid[u]!="-") r++
        else f++ }
      print t, o, r, f }' "$tsv" "$OUT/index.tsv" 2>/dev/null || echo "0 0 0 0")
}

pc_accept_stage_medicao() {
  local tsv="$OUT/units.medicao.tsv" pend="$OUT/units.medicao.pending.tsv" q="$OUT/queue.medicao.tsv" line unit
  Rscript --vanilla "$PC_TOOLS/accept_cases.R" --config "$CONFIG" --stage medicao --out "$tsv" >/dev/null \
    || pc_accept_die contract.accept_bad_config "accept_cases.R recusou $CONFIG"
  head -n 1 "$tsv" > "$pend"
  while IFS= read -r line; do
    unit="${line%%$'\t'*}"
    [ "$unit" = unit ] && continue
    if [ "$RESUME" = 1 ] && pc_accept_resume_skip "$unit"; then continue; fi
    rm -rf "$OUT/units/$unit/state"
    printf '%s\n' "$line" >> "$pend"
  done < "$tsv"
  pc_accept_prepare_all "$pend" "$q"
  UNITS_RC=0
  pc_accept_queue "$q" "$JOBS" || UNITS_RC=$?
  pc_accept_units_totals "$tsv"
  if [ "$UNITS_FAILED" -gt 0 ]; then pc_accept_die partial.accept_units_failed "unidades failed na medicao (veja $OUT/index.tsv)"; fi
  return 0
}

# --- veredito por estagio, relatorio e recibo ---------------------------------------------------------------------------------
# stages.tsv (stage<TAB>rc<TAB>error_id): uma linha por estagio; 255 = nao concluiu
pc_accept_stage_mark() {
  local s="$1" rc="$2" id="${3:--}" f="$OUT/stages.tsv" t
  t="$(mktemp "$OUT/.stages-XXXXXX")"
  { [ -f "$f" ] && awk -F'\t' -v s="$s" '$1!=s' "$f"; printf '%s\t%s\t%s\n' "$s" "$rc" "$id"; } > "$t" || true
  mv -f "$t" "$f"
}

# 0 se o estagio $1 terminou com rc 0 segundo stages.tsv
pc_accept_stage_green() { awk -F'\t' -v s="$1" '$1==s && $2==0{f=1} END{exit f?0:1}' "$OUT/stages.tsv" 2>/dev/null; }

# estado do estagio rodado em subshell (todas as variaveis que o JSON unico usa), gravado pelo trap EXIT do subshell
pc_accept_dump_state() {
  local rc=$? v
  for v in UNITS_TOTAL UNITS_OK UNITS_REFUSED UNITS_FAILED DELTA_MAX DELTA_KEYS FINAL_ID; do printf '%s=%q\n' "$v" "${!v}"; done > "$STATE_FILE"
  printf 'STAGE_RC=%s\n' "$rc" >> "$STATE_FILE"
}

pc_accept_isolated() {
  trap pc_accept_dump_state EXIT
  set -e
  "$1"
}

# roda o estagio $1 em subshell de fundo (o trap de sinal do pai segue vivo), recarrega o estado e devolve o rc em STAGE_RC
pc_accept_run_isolated() {
  local st="$1"
  STATE_FILE="$(mktemp "$OUT/.state-XXXXXX")"
  FINAL_ID=""
  set +e
  pc_accept_bg pc_accept_isolated "pc_accept_stage_$st"
  set -e
  # shellcheck disable=SC1090
  source "$STATE_FILE"
  rm -f "$STATE_FILE"
}

pc_accept_write_receipt() {   # $1 = PASS|FAIL
  local dir="${RECEIPTS_DIR:-$REPO/tests/.receipts}" bp="${BUNDLE_PATH[new]:-}" rid="${RUNTIME_ID[new]:--}"
  [ -n "$bp" ] && [ -f "$bp" ] || return 1
  RECEIPT_PATH="$(Rscript --vanilla -e 'a <- commandArgs(TRUE); source(a[1])
    cat(pc_receipt_write(gate = "docker-accept", bundle_path = a[2], platform = "linux-x86_64", resultado = a[3],
                         runtime_id = if (a[4] %in% c("", "-")) NULL else a[4], dir = a[5]), "\n", sep = "")' \
    "$PC_TOOLS/receipts.R" "$bp" "$1" "$rid" "$dir" 2>/dev/null | tail -n 1)" || RECEIPT_PATH=""
  return 0
}

# relatorio e recibo; NAO sai em falha do relatorio (o recibo FAIL precisa ser gravado): deixa o id em REPORT_ID
pc_accept_stage_relatorio() {
  local rc=0 res="$OUT/report.result.json" verdict=PASS s
  REPORT_ID=""
  Rscript --vanilla "$PC_TOOLS/accept_report.R" --out-dir "$OUT" --data "$DATA" > "$res" 2> "$OUT/report.stderr.log" || rc=$?
  cat "$OUT/report.stderr.log" >&2
  case "$rc" in
    0) REPORT_PATH="$OUT/report.tsv" ;;
    2) REPORT_ID=contract.accept_report_input ;;
    9) REPORT_ID=integrity.accept_store_changed ;;
    *) REPORT_ID=contract.accept_bad_args ;;
  esac
  pc_accept_cleanup   # residuo ANTES do recibo: o PASS exige residuo zero e store intacto
  for s in smoke aceite medicao; do pc_accept_stage_green "$s" || verdict=FAIL; done
  [ -z "$REPORT_ID" ] || verdict=FAIL
  [ $((RES_C + RES_I + RES_K)) -eq 0 ] && [ "$STORE_UNCHANGED" = true ] || verdict=FAIL
  pc_accept_write_receipt "$verdict"
  pc_accept_stage_mark relatorio "$([ -z "$REPORT_ID" ] && echo 0 || echo 1)" "${REPORT_ID:--}"
  return 0
}

# smoke -> aceite -> medicao -> relatorio. Estagio falho nao interrompe o relatorio; smoke falho pula aceite e medicao.
pc_accept_all() {
  local st first_id="" first_rc=0 t=0 o=0 r=0 f=0 skip=0
  for st in smoke aceite medicao; do
    if [ "$skip" = 1 ]; then pc_accept_stage_mark "$st" 255 -; continue; fi
    pc_accept_stage_mark "$st" 255 incompleto
    pc_accept_run_isolated "$st"
    if [ "$STAGE_RC" = 0 ]; then
      pc_accept_stage_mark "$st" 0 -
    else
      pc_accept_stage_mark "$st" "$STAGE_RC" "${FINAL_ID:--}"
      if [ -z "$first_id" ]; then first_id="${FINAL_ID:-contract.accept_bad_args}"; first_rc="$STAGE_RC"; fi
      [ "$st" = smoke ] && skip=1
    fi
    t=$((t + UNITS_TOTAL)); o=$((o + UNITS_OK)); r=$((r + UNITS_REFUSED)); f=$((f + UNITS_FAILED))
  done
  UNITS_TOTAL=$t; UNITS_OK=$o; UNITS_REFUSED=$r; UNITS_FAILED=$f
  FINAL_ID=""
  pc_accept_stage_mark relatorio 255 incompleto
  pc_accept_stage_relatorio
  [ -n "$first_id" ] || first_id="$REPORT_ID"
  [ -z "$first_id" ] || pc_accept_die "$first_id" "estagio falhou (veja $OUT/stages.tsv e $OUT/report.md)"
  return 0
}

# estagio unico: marca 255 antes e 0 depois (falha deixa 255 em stages.tsv)
pc_accept_single() {
  pc_accept_stage_mark "$1" 255 incompleto
  "pc_accept_stage_$1"
  pc_accept_stage_mark "$1" 0 -
}

# standalone relatorio: bundles, runtime_id e RUN_ID do run.json do estagio anterior (nenhum objeto Docker e criado)
pc_accept_load_run_json() {
  local j="$OUT/run.json" ver
  [ -f "$j" ] || pc_accept_die contract.accept_bad_args "run.json ausente em $OUT (rode antes o estagio medicao)"
  RUN_ID="$(jq -r '.run_id' "$j")"
  BUILDER="pc-accept-$RUN_ID"   # nada e criado no relatorio; so a conferencia de residuo
  for ver in ref new; do
    BUNDLE_PATH[$ver]="$(jq -r ".bundles.$ver.path" "$j")"
    BUNDLE_SHA[$ver]="$(jq -r ".bundles.$ver.sha256" "$j")"
    RUNTIME_ID[$ver]="$(jq -r ".runtime_id.$ver" "$j")"
  done
}

# --- despacho -----------------------------------------------------------------------------------------------------------
pc_accept_main() {
  RUN_ID="$(pc_accept_run_id)"
  pc_accept_parse_args "$@"
  pc_accept_preflight
  trap 'pc_accept_on_signal TERM' TERM
  trap 'pc_accept_on_signal INT' INT
  trap pc_accept_on_exit EXIT
  TRAP_ARMED=1
  if [ "$STAGE" = relatorio ]; then
    pc_accept_load_run_json
  else
    pc_accept_bundles
    pc_accept_builder_create
    pc_accept_build_image ref
    pc_accept_build_image new
    pc_accept_write_run_json
  fi
  case "$STAGE" in
    smoke|aceite|medicao) pc_accept_single "$STAGE" ;;
    relatorio) pc_accept_stage_relatorio; [ -z "$REPORT_ID" ] || pc_accept_die "$REPORT_ID" "relatorio falhou (veja $OUT/report.stderr.log)" ;;
    all) pc_accept_all ;;
  esac
  exit 0
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then pc_accept_main "$@"; fi
