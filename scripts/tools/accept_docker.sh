#!/usr/bin/env bash
# =============================================================================
# accept_docker.sh -- aceite em Docker LOCAL a partir da imagem do bundle (spec RegDin Linear Refactor, epico E8). Dev-only, so Linux,
# fora do bundle. Nunca usa Batch, ECR, push, login nem AWS: tudo roda na maquina local.
#
# Uso:
#   accept_docker.sh --stage smoke|aceite|medicao|relatorio|all --out DIR --data STORE [--config YAML]
#                    [--ref-bundle TAR --new-bundle TAR] [--jobs 12-14] [--resume] [--format text|json]
#                    [--min-free-gb-docker N] [--min-free-gb-out N]
#   Estagios implementados aqui: smoke. aceite/medicao/relatorio/all sao despachos que os tickets seguintes preenchem; ate la
#   recusam com contract.accept_bad_args (antes de qualquer objeto Docker).
#
# Bundles: sem --ref-bundle/--new-bundle, constroi por build_bundle_linux.sh (ref = --rev de24021, nova = HEAD; arvore suja recusa).
# Com eles, confere o .sha256 ao lado de cada tar.zst. Cada imagem = prevcarga-accept:<RUN_ID>-ref|-new (label prevcarga.accept.run=<RUN_ID>).
# O store (--data) so e montado como :ro e a listagem antes/depois (store.before.tsv/store.after.tsv) tem que ser igual.
#
# Saida: UM JSON em stdout (--format json, default) ou texto ASCII (--format text); mensagens em stderr. Arquivos em --out:
#   run.json, store.before.tsv, store.after.tsv, cache.snapshot, residue.json ({containers,images,cache}), index.tsv (unidades).
#
# Exits (classe do id em scripts/lib/errors/accept.tsv; tabela em scripts/lib/errors.R):
#   0 ok; 2 contract.accept_bad_args | contract.accept_out_forbidden | contract.accept_disk_low;
#   6 bundle.accept_image_build | bundle.accept_image_smoke; 9 integrity.accept_residue | integrity.accept_store_changed;
#   14 partial.accept_units_failed; 130 INT; 143 TERM. (runtime.accept_oom_killed e o error_id de uma unidade rc 137 em index.tsv.)
#
# Regra de limpeza (maquina compartilhada): o trap pc_accept_cleanup (EXIT/INT/TERM, instalado apos o preflight e antes do primeiro
# docker build, idempotente) mata os jobs de fundo e remove SO o que esta execucao criou: containers e imagens com o label do RUN_ID,
# a base pinada se nao existia antes, e os ids de build cache ausentes de cache.snapshot (docker builder prune -f --filter id=<id>).
# NUNCA usa prune sem filtro nem docker system prune, e nunca toca imagem preexistente. Sobra de container/imagem/cache => exit 9
# (integrity.accept_residue), que prevalece sobre o rc original. Nenhuma variavel de ambiente e lida.
# =============================================================================
set -euo pipefail

PC_SELF="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
PC_SELF_DIR="$(dirname "$PC_SELF")"
PC_ACCEPT_DOCKERFILE="$PC_SELF_DIR/accept_docker.Dockerfile"
PC_ACCEPT_REF_REV="de24021"
PC_ACCEPT_STAGES_IMPL="smoke"

declare -A RUNTIME_ID=()
declare -A BUNDLE_PATH=()
declare -A BUNDLE_SHA=()
STAGE="all"; OUT=""; DATA=""; CONFIG=""; REF_BUNDLE=""; NEW_BUNDLE=""; JOBS=13; RESUME=0; FORMAT="json"
MIN_FREE_DOCKER=50; MIN_FREE_OUT=20
RUN_ID=""; BASE=""; BASE_PREEXISTENTE=0; DROOT=""; REPO=""
CLEANED=0; TRAP_ARMED=0; OUT_READY=0; FINAL_ID=""; FINAL_RC=0; FINAL_MSG=""
RES_C=0; RES_I=0; RES_K=0; STORE_UNCHANGED=true
UNITS_TOTAL=0; UNITS_OK=0; UNITS_REFUSED=0; UNITS_FAILED=0
CTX_DIRS=()
UNIT_ORIGEM="-"; UNIT_AREA="-"; UNIT_MDIR="-"

# --- erros: id -> classe -> exit (tabela de scripts/lib/errors.R) ------------------------------------------------------
pc_accept_class_exit() {
  case "${1%%.*}" in
    contract) echo 2 ;; bundle) echo 6 ;; runtime) echo 7 ;; integrity) echo 9 ;; partial) echo 14 ;;
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
    --arg verdict "$verdict" --arg eid "$eid" \
    '{run_id:$run_id, stage:$stage,
      images:{ref:(if $ir=="" then null else $ir end), new:(if $in=="" then null else $in end)},
      runtime_id:{ref:(if $rr=="" then null else $rr end), new:(if $rn=="" then null else $rn end)},
      units:{total:$ut, ok:$uo, refused:$ur, failed:$uf}, delta:null,
      residue:{containers:$c, images:$i, cache:$k}, store_unchanged:$su, report:null, receipt:null,
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
      --stage|--out|--config|--data|--ref-bundle|--new-bundle|--jobs|--format|--min-free-gb-docker|--min-free-gb-out) ;;
      *) pc_accept_die contract.accept_bad_args "opcao desconhecida: $a" ;;
    esac
    [ $# -ge 2 ] || pc_accept_die contract.accept_bad_args "$a sem valor"
    v="$2"; shift 2
    case "$a" in
      --stage) STAGE="$v" ;; --out) OUT="$v" ;; --config) CONFIG="$v" ;; --data) DATA="$v" ;;
      --ref-bundle) REF_BUNDLE="$v" ;; --new-bundle) NEW_BUNDLE="$v" ;; --jobs) JOBS="$v" ;; --format) FORMAT="$v" ;;
      --min-free-gb-docker) MIN_FREE_DOCKER="$v" ;; --min-free-gb-out) MIN_FREE_OUT="$v" ;;
    esac
  done
  case "$STAGE" in smoke|aceite|medicao|relatorio|all) ;; *) pc_accept_die contract.accept_bad_args "--stage invalido: $STAGE" ;; esac
  case "$FORMAT" in text|json) ;; *) pc_accept_die contract.accept_bad_args "--format invalido: $FORMAT" ;; esac
  case "$JOBS" in 12|13|14) ;; *) pc_accept_die contract.accept_bad_args "--jobs deve ser 12, 13 ou 14: $JOBS" ;; esac
  case "$MIN_FREE_DOCKER" in ''|*[!0-9]*) pc_accept_die contract.accept_bad_args "--min-free-gb-docker invalido" ;; esac
  case "$MIN_FREE_OUT" in ''|*[!0-9]*) pc_accept_die contract.accept_bad_args "--min-free-gb-out invalido" ;; esac
  [ -n "$OUT" ] || pc_accept_die contract.accept_bad_args "--out e obrigatorio"
  [ -n "$DATA" ] || pc_accept_die contract.accept_bad_args "--data e obrigatorio"
  [ -d "$DATA" ] || pc_accept_die contract.accept_bad_args "--data nao e diretorio: $DATA"
  if [ -n "$REF_BUNDLE$NEW_BUNDLE" ]; then
    [ -n "$REF_BUNDLE" ] && [ -n "$NEW_BUNDLE" ] || pc_accept_die contract.accept_bad_args "--ref-bundle e --new-bundle andam juntos"
  fi
  case " $PC_ACCEPT_STAGES_IMPL " in
    *" $STAGE "*) ;;
    *) pc_accept_die contract.accept_bad_args "estagio $STAGE ainda nao implementado" ;;
  esac
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
  pc_accept_cache_ids > "$OUT/cache.snapshot"
}

# --- limpeza -------------------------------------------------------------------------------------------------------------
pc_accept_residue() {
  local f="label=prevcarga.accept.run=$RUN_ID"
  RES_C="$(docker ps -aq --filter "$f" 2>/dev/null | wc -l)"
  RES_I="$({ docker images -q --filter "$f"; docker images -q --filter "reference=prevcarga-accept:$RUN_ID-*"; } 2>/dev/null | LC_ALL=C sort -u | wc -l)"
  RES_K="$(pc_accept_cache_ids | LC_ALL=C comm -13 "$OUT/cache.snapshot" - | wc -l)"
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
  local f="label=prevcarga.accept.run=$RUN_ID" id d
  jobs -p | xargs -r kill 2>/dev/null || true
  docker ps -aq --filter "$f" 2>/dev/null | xargs -r docker rm -f >/dev/null 2>&1 || true
  { docker images -q --filter "$f"; docker images -q --filter "reference=prevcarga-accept:$RUN_ID-*"; } 2>/dev/null \
    | LC_ALL=C sort -u | xargs -r docker rmi -f >/dev/null 2>&1 || true
  if [ "$BASE_PREEXISTENTE" = 0 ] && [ -n "$BASE" ]; then docker rmi "$BASE" >/dev/null 2>&1 || true; fi
  pc_accept_cache_ids | LC_ALL=C comm -13 "$OUT/cache.snapshot" - | while read -r id; do
    [ -n "$id" ] && { docker builder prune -f --filter "id=$id" >/dev/null 2>&1 || true; }
  done
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
    RUNTIME_ID[$ver]="$(jq -r '.runtime_id // "-"' "${tar%.tar.zst}.RUNTIME.json" 2>/dev/null || echo -)"
  done
}

pc_accept_build_image() {
  local ver="$1" ctx img="prevcarga-accept:$RUN_ID-$1"
  ctx="$(mktemp -d)"; CTX_DIRS+=("$ctx")
  cp "${BUNDLE_PATH[$ver]}" "$ctx/bundle.tar.zst"
  printf '%s  bundle.tar.zst\n' "${BUNDLE_SHA[$ver]}" > "$ctx/bundle.tar.zst.sha256"
  pc_accept_bg docker build --label "prevcarga.accept.run=$RUN_ID" -t "$img" -f "$PC_ACCEPT_DOCKERFILE" "$ctx" >&2 \
    || pc_accept_die bundle.accept_image_build "docker build da imagem $img falhou"
  rm -rf "$ctx"
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
    done < "$mf"
  fi
  t0="$(date -u +%s)"
  docker run --rm --name "pc-accept-$RUN_ID-$n" --label "prevcarga.accept.run=$RUN_ID" \
    --network none --memory 8g --cpus 1 --user "$(id -u):$(id -g)" \
    -v "$DATA:/data:ro" -v "$u:/out" -v "$u/tmp:/tmp" ${extra[@]+"${extra[@]}"} "$img" "$@" \
    --config=/out/config.yaml --state=/out/state --format=json < /dev/null > "$u/result.json" 2> "$u/stderr.log" || rc=$?
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

# --- despacho -----------------------------------------------------------------------------------------------------------
pc_accept_main() {
  RUN_ID="$(pc_accept_run_id)"
  pc_accept_parse_args "$@"
  pc_accept_preflight
  trap 'pc_accept_on_signal TERM' TERM
  trap 'pc_accept_on_signal INT' INT
  trap pc_accept_on_exit EXIT
  TRAP_ARMED=1
  pc_accept_bundles
  pc_accept_build_image ref
  pc_accept_build_image new
  pc_accept_write_run_json
  case "$STAGE" in
    smoke) pc_accept_stage_smoke ;;
    aceite|medicao|relatorio|all) pc_accept_die contract.accept_bad_args "estagio $STAGE ainda nao implementado" ;;
  esac
  exit 0
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then pc_accept_main "$@"; fi
