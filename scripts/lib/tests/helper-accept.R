## helper-accept.R -- docker FALSO e fixtures para o teste do accept_docker.sh (E8-T1). Nao e codigo de produto.
## O docker falso e um script sh em tempdir: registra cada chamada em <dir>/calls.log e mantem o estado em <dir>/state/{containers,images,cache}.
## Todos os caminhos ficam embutidos no proprio script (sem variavel de ambiente). Nenhum teste chama docker real nem AWS.
##
## Modos: ok, smoke_tz (zoneinfo ausente na imagem nova), sticky (rm -f nao remove), hang (run dorme), oom (unidade rc 137),
##        base_present (base ja presente), cache_pre (registro de cache preexistente), build_fail (docker build falha),
##        unit_refuse (unidade rc 2 com envelope), unit_fail (unidade rc 1 sem envelope), store_touch (run altera o store).
## E8-T2: long_fixture (docker run copia para /out/state um LONG sintetico de 8 colunas conforme o verbo; extrai topology.yaml e
##        run.yaml do "bundle" por --entrypoint cat; a ref demora 2 s no infer-model para a nova ser estritamente mais rapida),
##        diverge (long_fixture + um y_pred alterado no backtest S da nova) e holiday_unexpected (long_fixture + o backtest de
##        feriado AM da nova termina rc 1 com generic.unexpected).

acc_script <- function() file.path(pc_root(), "scripts", "tools", "accept_docker.sh")
acc_dockerfile <- function() file.path(pc_root(), "scripts", "tools", "accept_docker.Dockerfile")

acc_fake_cases <- function() r"--(
ref_file() { case "$1" in ubuntu:*) echo base ;; id-*) echo "${1#id-}" ;; *) printf '%s' "$1" | tr ':/@*' '____' ;; esac; }
label_of() { sed -n 2p "$1" 2>/dev/null; }
case "$1" in
  info) echo "$ROOT" ;;
  buildx)
    for f in "$ST"/cache/*; do [ -e "$f" ] || continue; printf 'ID:\t\t%s\nCreated at:\tx\n\n' "$(basename "$f")"; done ;;
  build)
    shift; tag=; label=
    while [ $# -gt 0 ]; do case "$1" in -t) tag=$2; shift 2 ;; --label) label=$2; shift 2 ;; -f) shift 2 ;; *) shift ;; esac; done
    [ -e "$ST/images/base" ] || printf 'base\n\n' > "$ST/images/base"
    : > "$ST/cache/build-$$-$(date +%s%N)"
    [ "$MODE" = build_fail ] && exit 1
    printf '%s\n%s\n' "$tag" "$label" > "$ST/images/$(ref_file "$tag")" ;;
  create)
    shift; name=; label=; net=default
    while [ $# -gt 0 ]; do case "$1" in --name) name=$2; shift 2 ;; --label) label=$2; shift 2 ;; --network) net=$2; shift 2 ;; -*) shift ;; *) break ;; esac; done
    printf '%s\n%s\n' "$label" "$net" > "$ST/containers/$name"; echo "$name" ;;
  start)
    shift; [ "$1" = "-a" ] && shift
    [ -e "$ST/containers/$1" ] || exit 1
    echo '{"ok":true}' ;;
  inspect)
    f="$ST/containers/$4"; [ -e "$f" ] || exit 1; sed -n 2p "$f" ;;
  run)
    shift; name=; label=; ep=0; epcmd=; vols=
    while [ $# -gt 0 ]; do
      case "$1" in
        --rm) shift ;; --name) name=$2; shift 2 ;; --label) label=$2; shift 2 ;;
        --entrypoint) ep=1; epcmd=$2; shift 2 ;; -v) vols="$vols $2"; shift 2 ;; --network|--memory|--cpus|--user|-e|--env) shift 2 ;;
        -*) shift ;; *) img=$1; shift; break ;;
      esac
    done
    case "$MODE" in long_fixture|diverge|holiday_unexpected) LFX=1 ;; *) LFX=0 ;; esac
    if [ "$LFX" = 1 ]; then
      printf '%s\nnone\n' "$label" > "$ST/containers/$name"
      if [ "$ep" = 1 ]; then
        if [ "$epcmd" = cat ]; then
          case "$1" in */topology.yaml) cat "$ASSETS/topology.yaml" ;; */run.yaml) cat "$ASSETS/$(basename "$(dirname "$1")").run.yaml" ;; esac
        fi
        rm -f "$ST/containers/$name"; exit 0
      fi
      OUTD=; for v in $vols; do case "$v" in *:/out) OUTD=${v%%:*} ;; esac; done
      verb=$1; area=; origin=; model=; outarg=
      for a in "$@"; do case "$a" in --area=*) area=${a#--area=} ;; --origin=*) origin=${a#--origin=} ;; --model=*) model=${a#--model=} ;; --out=*) outarg=${a#--out=} ;; esac; done
      st=0; res='{"ok":true}'
      case "$verb" in
        backtest)
          R="$OUTD/state/backtest/run1"; leaf=$area; [ "$area" = S ] && leaf=RS
          for m in LIN_MED LIN_MAX; do mkdir -p "$R/members/$m/$leaf/$origin/0800"; cp "$FX/long.parquet" "$R/members/$m/$leaf/$origin/0800/long.parquet"; done
          mkdir -p "$R/combined" "$R/reconciled/subsistemas"; cp "$FX/long.parquet" "$R/combined/EQW.parquet"
          if [ "$area" = S ]; then
            cp "$FX/long.parquet" "$R/reconciled/subsistemas/S.parquet"
            case "$img" in *-new) [ "$MODE" = diverge ] && cp "$FX/long_div.parquet" "$R/reconciled/subsistemas/S.parquet" ;; esac
          fi
          if [ "$MODE" = holiday_unexpected ] && [ "$area" = AM ]; then
            case "$img" in *-new) st=1; res='{"ok":false,"error":{"id":"generic.unexpected"}}' ;; esac
          fi ;;
        train-models) mkdir -p "$OUTD/state/artifacts/v_fake"; echo x > "$OUTD/state/artifacts/v_fake/model.rds" ;;
        infer-model)
          case "$img" in *-ref) sleep 2 ;; esac
          [ -n "$outarg" ] && cp "$FX/long.parquet" "$OUTD/$(basename "$outarg")" ;;
        deck-inference) mkdir -p "$OUTD/deck"; echo x > "$OUTD/deck/manifest.json" ;;
      esac
      echo "$res"
      rm -f "$ST/containers/$name"; exit "$st"
    fi
    printf '%s\nnone\n' "$label" > "$ST/containers/$name"
    [ "$MODE" = store_touch ] && : > "$STORE/changed-by-run.txt"
    [ "$MODE" = hang ] && exec sleep 60
    rc=0
    if [ "$ep" = 1 ]; then
      [ "$MODE" = smoke_tz ] && case "$img" in *-new) rc=1 ;; esac
    else
      case "$MODE" in
        oom) rc=137 ;; unit_fail) rc=1 ;;
        unit_refuse) echo '{"error":{"id":"contract.fake_refusal"}}'; rc=2 ;;
        *) echo '{"ok":true}' ;;
      esac
    fi
    [ "$MODE" = sticky ] || rm -f "$ST/containers/$name"
    exit "$rc" ;;
  ps)
    want=; shift
    while [ $# -gt 0 ]; do case "$1" in --filter) want=${2#label=}; shift 2 ;; *) shift ;; esac; done
    for f in "$ST"/containers/*; do [ -e "$f" ] || continue; [ "$(sed -n 1p "$f")" = "$want" ] && basename "$f"; done; exit 0 ;;
  images)
    shift; lab=; ref=
    while [ $# -gt 0 ]; do
      case "$1" in --filter) case "$2" in label=*) lab=${2#label=} ;; reference=*) ref=${2#reference=} ;; esac; shift 2 ;; *) shift ;; esac
    done
    for f in "$ST"/images/*; do
      [ -e "$f" ] || continue; tag=$(sed -n 1p "$f"); l=$(sed -n 2p "$f")
      if [ -n "$lab" ]; then [ "$l" = "$lab" ] && echo "id-$(basename "$f")"
      elif [ -n "$ref" ]; then case "$tag" in $ref) echo "id-$(basename "$f")" ;; esac
      else echo "id-$(basename "$f")"; fi
    done; exit 0 ;;
  rm)
    shift; [ "$MODE" = sticky ] && exit 0
    for a in "$@"; do case "$a" in -*) ;; *) rm -f "$ST/containers/$a" ;; esac; done ;;
  rmi)
    shift; for a in "$@"; do case "$a" in -*) ;; *) rm -f "$ST/images/$(ref_file "$a")" ;; esac; done ;;
  image)
    [ "$2" = inspect ] || exit 0
    case "$3" in ubuntu:*) [ -e "$ST/images/base" ] ;; *) [ -e "$ST/images/$(ref_file "$3")" ] ;; esac ;;
  builder)
    id=; shift
    while [ $# -gt 0 ]; do case "$1" in --filter) id=${2#id=}; shift 2 ;; *) shift ;; esac; done
    [ -n "$id" ] && rm -f "$ST/cache/$id" ;;
  *) exit 0 ;;
esac
)--"

## LONG sintetico de 8 colunas (ADR-0002); `bump` soma a y_pred da primeira linha (divergencia injetada); `tz` so muda a exibicao
acc_long_fixture <- function(path, n = 6L, bump = 0, area = "RS", tz = "UTC", origin = "2023-06-01") {
  t0 <- as.POSIXct("2023-06-01 01:00:00", tz = "UTC")
  d <- data.frame(area = area, forecast_origin = origin, target_datetime = structure(t0 + 3600 * (seq_len(n) - 1L), tzone = tz),
                  lead = seq_len(n), y_pred = 100 + seq_len(n) * 1.5, y_true = 100 + seq_len(n), d_plus = 0L,
                  granularity_min = 60L, stringsAsFactors = FALSE)
  d$lead <- as.integer(d$lead)
  d$y_pred[1L] <- d$y_pred[1L] + bump
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(d, path)
  invisible(path)
}

## YAMLs do aceite em tempdir: o real (valido) e variantes invalidas (area ZZ, chave desconhecida, origem 2023-13-01, N sem aspas)
acc_cfg_fixture <- function() {
  d <- tempfile("acc-cfg-"); dir.create(d)
  real <- yaml::read_yaml(file.path(pc_root(), "scripts", "tools", "accept_docker.yaml"))
  w <- function(name, fn) { f <- file.path(d, name); yaml::write_yaml(fn(real), f, handlers = list(logical = yaml::verbatim_logical)); f }
  list(dir = d,
       valid = w("valid.yaml", identity),
       area_zz = w("area_zz.yaml", function(x) { x$medicao$areas[[1L]] <- "ZZ"; x }),
       chave = w("chave.yaml", function(x) { x$nao_existe <- 1L; x }),
       chave_bloco = w("chave_bloco.yaml", function(x) { x$aceite$extra <- "x"; x }),
       origem = w("origem.yaml", function(x) { x$aceite$origem <- "2023-13-01"; x }),
       origem_med = w("origem_med.yaml", function(x) { x$medicao$origens[[3L]] <- "2023-02-30"; x }),
       jobs = w("jobs.yaml", function(x) { x$jobs <- 20L; x }),
       n_sem_aspas = { f <- file.path(d, "n.yaml"); writeLines(c("schema_version: 1", "aceite:", "  origem: \"2023-06-01\"", "  subsistema: S",
                     "  areas_feriado: [AM, N]", "  area_tempo: RJ", "  membro_tempo: dessem-linear/MED", "medicao:", "  origens: [\"2023-02-19\"]",
                     "  areas: [RJ]", "  mdirs: [dessem-linear]"), f); f })
}

acc_fake_docker <- function(dir = tempfile("fakedocker-"), mode = "ok", store = NULL) {
  dir.create(file.path(dir, "bin"), recursive = TRUE)
  st <- file.path(dir, "state"); root <- file.path(dir, "dockerroot")
  for (s in c("containers", "images", "cache")) dir.create(file.path(st, s), recursive = TRUE)
  dir.create(root)
  if (identical(mode, "cache_pre")) file.create(file.path(st, "cache", "pre0001"))
  if (identical(mode, "base_present")) writeLines(c("base", ""), file.path(st, "images", "base"))
  fx <- file.path(dir, "fx"); assets <- file.path(dir, "assets")
  if (mode %in% c("long_fixture", "diverge", "holiday_unexpected")) {
    acc_long_fixture(file.path(fx, "long.parquet"))
    acc_long_fixture(file.path(fx, "long_div.parquet"), bump = 1e-9)
    dir.create(assets)
    file.copy(file.path(pc_root(), "config", "topology.yaml"), file.path(assets, "topology.yaml"))
    for (m in c("dessem-linear", "dessem-regdin"))
      file.copy(file.path(pc_root(), "models", m, "run.yaml"), file.path(assets, paste0(m, ".run.yaml")))
  }
  body <- c("#!/bin/sh",
            sprintf("ST='%s'; MODE='%s'; LOG='%s'; ROOT='%s'; STORE='%s'; FX='%s'; ASSETS='%s'", st, mode, file.path(dir, "calls.log"), root,
                    if (is.null(store)) "" else store, fx, assets),
            'printf "%s\\n" "$*" >> "$LOG"',
            acc_fake_cases())
  f <- file.path(dir, "bin", "docker")
  writeLines(body, f); Sys.chmod(f, "0755")
  list(dir = dir, bin = dirname(f), log = file.path(dir, "calls.log"), state = st, root = root, mode = mode)
}

acc_path <- function(fake) paste(c(fake$bin, R.home("bin"), Sys.getenv("PATH")), collapse = ":")

## store sintetico pequeno (com um link de diretorio ciclico, como o store real; o nome "ciclo" evita o gate de grep do diretorio de dados)
acc_store <- function() {
  d <- tempfile("acc-store-"); dir.create(file.path(d, "sub"), recursive = TRUE)
  writeLines("a", file.path(d, "a.txt")); writeLines("b", file.path(d, "sub", "b.txt"))
  file.symlink(".", file.path(d, "ciclo"))
  d
}

acc_bundle_fixture <- function() {
  d <- tempfile("acc-bundles-"); out <- list()
  for (v in c("ref", "new")) {
    dir.create(file.path(d, v), recursive = TRUE)
    tar <- file.path(d, v, sprintf("prevcarga-accept-%s-linux-x86_64.tar.zst", v))
    writeLines(paste("fake bundle", v), tar)
    writeLines(sprintf("%s  %s", digest::digest(file = tar, algo = "sha256"), basename(tar)), paste0(tar, ".sha256"))
    writeLines(sprintf('{"runtime_id":"rt-%s"}', v), file.path(d, v, sprintf("prevcarga-accept-%s-linux-x86_64.RUNTIME.json", v)))
    out[[v]] <- tar
  }
  out
}

## roda bash accept_docker.sh <args> com o docker falso na frente do PATH; stdout (JSON unico) e stderr separados
acc_run <- function(args, fake) {
  se <- tempfile("acc-stderr-")
  so <- suppressWarnings(system2("bash", c(acc_script(), args), stdout = TRUE, stderr = se,
                                 env = paste0("PATH=", acc_path(fake))))
  st <- attr(so, "status"); if (is.null(st)) st <- 0L
  txt <- paste(so, collapse = "\n")
  list(status = as.integer(st), stdout = txt,
       json = if (nzchar(txt)) tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL) else NULL,
       stderr = readLines(se, warn = FALSE),
       calls = if (file.exists(fake$log)) readLines(fake$log, warn = FALSE) else character())
}

## argumentos comuns do estagio smoke com bundles prontos
acc_args <- function(out, store, bundles, extra = character()) {
  c("--stage", "smoke", "--out", out, "--data", store, "--ref-bundle", bundles$ref, "--new-bundle", bundles$new,
    "--min-free-gb-docker", "0", "--min-free-gb-out", "0", "--format", "json", extra)
}

acc_residue <- function(out) jsonlite::read_json(file.path(out, "residue.json"))

acc_state_files <- function(fake, what) list.files(file.path(fake$state, what))

acc_wait_for <- function(pred, timeout = 20) {
  t0 <- Sys.time()
  while (!isTRUE(pred())) {
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("acc_wait_for: tempo esgotado")
    Sys.sleep(0.1)
  }
  invisible(TRUE)
}
