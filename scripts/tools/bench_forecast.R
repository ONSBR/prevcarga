#!/usr/bin/env Rscript
## bench_forecast.R -- bench do NFR de 30 min do `forecast` completo (M6.6, AC 31). Ferramenta de DESENVOLVIMENTO/operador (fora do bundle).
##
##   Rscript --vanilla scripts/tools/bench_forecast.R --origin AAAA-MM-DD --reference-seconds S [--format text|json]
##       [--data <dir>] [--state <dir>] [--config <arquivo>]
##
## Cronometra (wall-clock, proc.time()[["elapsed"]]) UMA chamada de prevcarga_forecast(ctx, origin = O, report = TRUE): todas as folhas da topologia,
## todos os membros, report html, treino so dos origin_scoped da origem (origin_agnostic congelados e k-hat `latest` ja treinado pelo Reconcile:
## o forecast nunca os treina). `--reference-seconds` e o tempo do comando de previsao do ORACULO medido pelo operador na mesma maquina; o bench
## NAO executa o oraculo (so calcula a razao).
##
## - data/ (`--data`, ou data_dir do config.yaml) e SO LEITURA: a listagem (nome, tamanho, sha256) e tirada antes e depois; diferente => erro
##   integrity.bench_data_changed (exit 9).
## - o estado da rodada vive em tempdir(): `--state <dir>` (artefatos origin_agnostic, k-hat, pesos da combina) e COPIADO para la antes do
##   cronometro (a copia nao entra no tempo) e nunca e escrito. Sem --state, o estado comeca vazio (o forecast aborta exit 13 sem artefatos).
## - alvo fixo de 1800 s (30 min): estouro => veredito `bloqueante` e exit 1 com a mensagem "NFR 30 min estourado". NAO existe flag para
##   afrouxar o alvo; so a funcao bench_run() aceita `budget_s` (testes).
## Saida: 0 ok; 1 bloqueante; 2 args; 9 data/ alterado; demais exits do catalogo (ADR-0040) propagados do forecast.
## JSON: {segundos, referencia_s, razao, alvo_s, host, r_version, n_folhas, n_membros, treinou_origin_scoped, veredito}.

BENCH_BUDGET_S <- 1800
BENCH_LIBS <- c("config", "topology", "provenance", "schema_long", "evaluation", "deck_reader", "dates", "validation", "features",
                "registry", "training", "inference", "combination", "build_long_output", "reconcile", "exports", "reporting", "forecast")

## --- listagem de data/ (nome, tamanho, sha256), ordem radix ---------------------------------------------------------------------
## Percorre sem seguir symlink de DIRETORIO (o store real tem ciclos, ex.: data/data -> .): o link entra na listagem como `nome -> alvo`
## (tamanho 0, sha256 do texto do alvo); arquivo (inclusive via symlink) entra com tamanho e sha256 do conteudo.
.bench_walk <- function(dir, rel = "") {
  nm <- sort(list.files(file.path(dir, rel), all.files = TRUE, no.. = TRUE), method = "radix")
  out <- list()
  for (n in nm) {
    r <- if (nzchar(rel)) paste(rel, n, sep = "/") else n
    p <- file.path(dir, r)
    lk <- Sys.readlink(p)
    if (dir.exists(p) && nzchar(lk)) out[[length(out) + 1L]] <- c(r, "link", lk)
    else if (dir.exists(p)) out <- c(out, .bench_walk(dir, r))
    else out[[length(out) + 1L]] <- c(r, "file", p)
  }
  out
}

.bench_data_listing <- function(dir) {
  w <- if (is.null(dir) || !dir.exists(dir)) list() else .bench_walk(dir)
  nome <- vapply(w, `[[`, "", 1L)
  kind <- vapply(w, `[[`, "", 2L)
  val <- vapply(w, `[[`, "", 3L)
  data.frame(nome = nome,
             tamanho = ifelse(kind == "file", as.numeric(file.size(val)), 0),
             sha256 = ifelse(kind == "file",
                             vapply(seq_along(val), function(i) if (kind[[i]] == "file") digest::digest(file = val[[i]], algo = "sha256") else "", ""),
                             vapply(val, function(v) digest::digest(v, algo = "sha256", serialize = FALSE), "", USE.NAMES = FALSE)),
             stringsAsFactors = FALSE)
}

## --- execucao cronometrada ----------------------------------------------------------------------------------------------------------
## forecast_fn(ctx, origin) -> list(result = list(covered, stray)); members_fn(ctx) -> n de membros; train_counter = env com `$train`
## (inteiro; o main embrulha prevcarga_train). `clock` e `budget_s` sao injetaveis so por chamada de funcao (testes).
bench_run <- function(ctx, origin, reference_seconds, forecast_fn, members_fn, train_counter = NULL,
                      budget_s = BENCH_BUDGET_S, clock = function() proc.time()[["elapsed"]],
                      host = Sys.info()[["nodename"]], r_version = R.version.string) {
  before <- .bench_data_listing(ctx$data_dir)
  t0 <- clock()
  out <- forecast_fn(ctx, origin)
  secs <- clock() - t0
  after <- .bench_data_listing(ctx$data_dir)
  if (!identical(before, after))
    prevcarga_abort("integrity.bench_data_changed", where = ctx$data_dir,
                    details = list(antes = nrow(before), depois = nrow(after)))
  trained <- !is.null(train_counter) && isTRUE(train_counter$train > 0L)
  list(segundos = round(secs, 1), referencia_s = reference_seconds, razao = round(secs / reference_seconds, 3),
       alvo_s = budget_s, host = host, r_version = r_version,
       n_folhas = length(c(out$result$covered, out$result$stray)), n_membros = as.integer(members_fn(ctx)),
       treinou_origin_scoped = trained, veredito = if (secs <= budget_s) "ok" else "bloqueante")
}

bench_render <- function(res, format = "text") {
  if (identical(format, "json"))
    return(as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null", digits = NA, na = "null")))
  c(sprintf("%-22s %s", paste0(names(res), ":"), vapply(res, function(x) as.character(x)[[1L]], "")),
    if (identical(res$veredito, "ok")) "OK" else sprintf("NFR 30 min estourado: %s s > %s s (veredito bloqueante)", res$segundos, res$alvo_s))
}

## --- argv ---------------------------------------------------------------------------------------------------------------------------
.bench_known <- c("--origin", "--reference-seconds", "--format", "--data", "--state", "--config")

.bench_parse <- function(argv) {
  bad <- function(why) prevcarga_abort("contract.bench_bad_args", where = why)
  opts <- list()
  i <- 1L
  while (i <= length(argv)) {
    a <- argv[[i]]
    nm <- sub("=.*$", "", a)
    if (!(nm %in% .bench_known)) bad(a)
    if (grepl("=", a, fixed = TRUE)) { opts[[nm]] <- sub("^[^=]*=", "", a); i <- i + 1L }
    else if (i < length(argv)) { opts[[nm]] <- argv[[i + 1L]]; i <- i + 2L }
    else bad(a)
  }
  if (is.null(opts[["--origin"]])) bad("--origin")
  rs <- suppressWarnings(as.numeric(opts[["--reference-seconds"]]))
  if (length(rs) != 1L || is.na(rs) || !is.finite(rs) || rs <= 0) bad("--reference-seconds")
  fmt <- if (is.null(opts[["--format"]])) "text" else opts[["--format"]]
  if (!(fmt %in% c("text", "json"))) bad("--format")
  opts[["--reference-seconds"]] <- rs
  opts[["--format"]] <- fmt
  opts
}

.bench_copy_state <- function(src, dst) {
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  if (is.null(src)) return(invisible(dst))
  if (!dir.exists(src)) prevcarga_abort("contract.bench_bad_args", where = "--state")
  ok <- file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  if (!all(ok)) prevcarga_abort("contract.bench_bad_args", where = "--state")
  invisible(dst)
}

.bench_generated_at <- function(run_id) {
  t <- lubridate::ymd_hms(sub("-.*$", "", as.character(run_id)), tz = "UTC", quiet = TRUE)
  if (is.na(t)) NA_character_ else format(t, "%Y-%m-%dT%H:%M:%SZ")
}

## Carrega a instalacao pelo caminho do script (como scripts/prevcarga) no globalenv (o plugin engine procura a API ali).
bench_main <- function(argv = commandArgs(TRUE), root = NULL) {
  if (is.null(root)) {
    fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    if (length(fa) != 1L) stop("bench_forecast.R: rode pelo caminho do script", call. = FALSE)
    root <- dirname(dirname(dirname(normalizePath(fa, winslash = "/"))))
  }
  options(prevcarga.lib = file.path(root, "scripts", "lib"))
  for (m in c("errors", "runtime")) source(file.path(root, "scripts", "lib", paste0(m, ".R")), local = FALSE)
  emit_err <- function(cond) writeLines(gsub("[^\\x20-\\x7E]", "?", prevcarga_render_error(cond, "text", verb = "bench-forecast"), perl = TRUE),
                                        con = stderr())
  rt <- prevcarga_with_errors(prevcarga_runtime_enforce())
  if (!is.null(rt$cond)) { emit_err(rt$cond); return(rt$exit_code) }
  out <- prevcarga_with_errors({
    for (m in BENCH_LIBS) sys.source(file.path(root, "scripts", "lib", paste0(m, ".R")), envir = globalenv())
    opts <- .bench_parse(argv)
    args <- list(`--origin` = opts[["--origin"]], `--data` = opts[["--data"]], `--config` = opts[["--config"]])
    args <- args[!vapply(args, is.null, NA)]
    state <- tempfile("bench-state-")
    args[["--state"]] <- state
    ctx <- prevcarga_ctx(args, root, "forecast")
    ctx$generated_at <- .bench_generated_at(ctx$run_id)
    prevcarga_require_data_dir(ctx)
    .bench_copy_state(opts[["--state"]], state)
    on.exit(unlink(state, recursive = TRUE), add = TRUE)
    counter <- new.env(parent = emptyenv()); counter$train <- 0L
    g <- globalenv()
    orig_train <- get("prevcarga_train", envir = g)
    assign("prevcarga_train", function(...) { counter$train <- counter$train + 1L; orig_train(...) }, envir = g)
    on.exit(assign("prevcarga_train", orig_train, envir = g), add = TRUE)
    res <- bench_run(ctx, ctx$origin, opts[["--reference-seconds"]],
                     forecast_fn = function(ctx, origin) prevcarga_forecast(ctx, origin = origin, report = TRUE),
                     members_fn = function(ctx) nrow(prevcarga_members(root = ctx$root)), train_counter = counter)
    list(res = res, format = opts[["--format"]])
  })
  if (!is.null(out$cond)) { emit_err(out$cond); return(out$exit_code) }
  writeLines(bench_render(out$value$res, out$value$format), con = stdout())
  if (!identical(out$value$res$veredito, "ok")) {
    writeLines("NFR 30 min estourado", con = stderr())
    return(1L)
  }
  0L
}

if (!interactive() && any(grepl("^--file=.*bench_forecast\\.R$", commandArgs(FALSE)))) quit(save = "no", status = bench_main())
