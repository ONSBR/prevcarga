## helper-accept.R -- docker FALSO e fixtures para o teste do accept_docker.sh (E8-T1). Nao e codigo de produto.
## O docker falso e um script sh em tempdir: registra cada chamada em <dir>/calls.log e mantem o estado em <dir>/state/{containers,images,cache}.
## Todos os caminhos ficam embutidos no proprio script (sem variavel de ambiente). Nenhum teste chama docker real nem AWS.
##
## Modos: ok, smoke_tz (zoneinfo ausente na imagem nova), sticky (rm -f nao remove), hang (run dorme), oom (unidade rc 137),
##        base_present (base ja presente), cache_pre (registro de cache preexistente), build_fail (docker build falha),
##        unit_refuse (unidade rc 2 com envelope), unit_fail (unidade rc 1 sem envelope), store_touch (run altera o store).

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
    shift; name=; label=; ep=0
    while [ $# -gt 0 ]; do
      case "$1" in
        --rm) shift ;; --name) name=$2; shift 2 ;; --label) label=$2; shift 2 ;;
        --entrypoint) ep=1; shift 2 ;; --network|--memory|--cpus|--user|-v|-e|--env) shift 2 ;;
        -*) shift ;; *) img=$1; shift; break ;;
      esac
    done
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

acc_fake_docker <- function(dir = tempfile("fakedocker-"), mode = "ok", store = NULL) {
  dir.create(file.path(dir, "bin"), recursive = TRUE)
  st <- file.path(dir, "state"); root <- file.path(dir, "dockerroot")
  for (s in c("containers", "images", "cache")) dir.create(file.path(st, s), recursive = TRUE)
  dir.create(root)
  if (identical(mode, "cache_pre")) file.create(file.path(st, "cache", "pre0001"))
  if (identical(mode, "base_present")) writeLines(c("base", ""), file.path(st, "images", "base"))
  body <- c("#!/bin/sh",
            sprintf("ST='%s'; MODE='%s'; LOG='%s'; ROOT='%s'; STORE='%s'", st, mode, file.path(dir, "calls.log"), root,
                    if (is.null(store)) "" else store),
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
