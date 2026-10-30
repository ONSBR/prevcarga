## orchestrate.R -- matriz de celulas, ledger local, lock e despacho do backtest (M7.1; ADR-0023, ADR-0040).
##
## prevcarga_plan() monta a matriz membro x folha x origem x corte de forma COUNT-AGNOSTICA: tudo vem de topology.R
## (membros, folhas, run.yaml) e do descriptor do plugin; nao existe lista de modelos nem literal de contagem aqui.
## A classe de cada celula e `split` do run.yaml + o protocolo efetivo (--protocol > backtest.protocol_default),
## validados em .m7_class(). Os cortes de treino NUNCA sao calculados aqui: train_origin / last_data_day saem de
## dates.R (prevcarga_train_cut, com o data_lag_days do descriptor); o corte intradiario available_until e a origem no
## horario do --corte, no fuso declarado pelo deck. A chave esperada de cada celula e a de cache.R (kind member_long,
## chaveada nos dois cortes).
##
## prevcarga_orchestrate() despacha as celulas (serial; --jobs > 1 PSOCK e do M7.6) sob run.lock/ (dir.create exclusivo),
## com ledger DERIVADO DE PRESENCA: celula `done` <=> `long.parquet` + `.key` iguais a chave esperada; falhas em
## ledger/failures.jsonl, escrito so pelo processo pai. O ciclo de vida de artefatos continua em registry.R.
## Sem variaveis de ambiente e sem relogio fora do ctx (ts da falha = prefixo de data de ctx$run_id); nunca chama quit().
## Requer errors.R, topology.R, provenance.R, deck_reader.R, dates.R, cache.R, registry.R (plugins) e training.R
## (seletores de membro/area) carregados antes.

## --- 1. Classe x protocolo ------------------------------------------------------------------------------
.m7_compat <- list(origin_agnostic = c("fixed", "daily"),
                   origin_scoped   = c("daily", "retrain_harness"))
.m7_protocols <- c("fixed", "daily", "retrain_harness")

.m7_class <- function(spec, protocol = NULL) {
  split <- spec$split
  if (is.null(split) || !is.character(split) || length(split) != 1L || !split %in% names(.m7_compat))
    prevcarga_abort("contract.run_yaml_split", where = as.character(spec$name)[[1L]])
  proto <- if (is.null(protocol)) spec$backtest$protocol_default else protocol
  if (is.null(proto))
    prevcarga_abort("contract.run_yaml_protocol", where = as.character(spec$name)[[1L]])
  if (!proto %in% .m7_compat[[split]])
    prevcarga_abort("contract.protocol_incompatible", where = as.character(spec$name)[[1L]],
                    details = list(split = split, protocol = proto))
  list(split = split, protocol = proto,
       trains_per_origin = !identical(proto, "fixed"), coupled_forecast = identical(proto, "retrain_harness"))
}

## --- 2. Matriz --------------------------------------------------------------------------------------------
.m7_members <- function(ctx, model = NULL) .prevcarga_tr_select_members(ctx, model)

## --area folha ou no de subsistema (expande para as folhas-filhas); fora da topologia => topology.area_undeclared.
.m7_leaves <- function(ctx, area = NULL) {
  topo <- .prevcarga_tr_topo(ctx)
  if (is.null(area)) return(topo$leaves)
  a <- prevcarga_resolve_areas(area = area, ctx = ctx)
  if (a %in% topo$order) topo$subsystems[[a]]$areas else a
}

.m7_corte_re <- "^([01][0-9]|2[0-3]):(00|30)$"

## --corte "08:00,09:30" (ou NULL => default do config); fora da grade de 30 min => config.bad_corte (exit 8).
.m7_parse_cortes <- function(x, default) {
  v <- if (is.null(x)) default else trimws(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  if (is.null(v) || !length(v)) prevcarga_abort("config.bad_corte", where = "--corte")
  bad <- v[!grepl(.m7_corte_re, v)]
  if (length(bad)) prevcarga_abort("config.bad_corte", where = "--corte", details = list(got = bad[[1L]]))
  unique(v)
}

## Janela --origin A..B (ou uma data) => vetor de Datas diarias; ausente => config.origin_range_required.
.m7_origins <- function(ctx) {
  o <- ctx$origin
  if (is.null(o)) prevcarga_abort("config.origin_range_required", where = "--origin")
  if (inherits(o, "Date")) return(o)
  seq(o$from, o$to, by = "day")
}

.m7_identity_inputs <- function(ctx) {
  man <- prevcarga_deck_manifest(prevcarga_require_data_dir(ctx)$data_dir)
  lab <- prevcarga_resolve_load_label(ctx, man)
  fp <- if (is.null(ctx$data_hash)) prevcarga_data_hash(ctx$data_dir, man, lab$value) else ctx$data_hash
  code <- if (is.null(ctx$code_commit)) prevcarga_code_commit(ctx$root) else ctx$code_commit
  lock <- if (is.null(ctx$renv_lock_hash)) prevcarga_renv_lock_hash(ctx$root) else ctx$renv_lock_hash
  list(manifest = man, load_label = lab$value, tz = man$timezone$load, data_fingerprint = fp, code_version = code,
       lock_hash = if (is.null(lock)) "no-renv-lock" else lock)
}

## Identidade do pedido: flags operacionais (--jobs, --resume, --keep-going, --format, --report*, --break-lock) ficam
## de fora; o run_id muda com janela, cortes, protocolo efetivo por membro, membros/folhas, load_label,
## weather_issue_cutoff, fuso e codigo. Usa as 8 chaves do contrato de prevcarga_version_hash.
.m7_run_id <- function(ctx, origins, cortes, cells, ident) {
  by_member <- unique(cells[, c("membro", "mdir", "exogena", "split", "protocol")])
  by_member <- by_member[order(by_member$membro, method = "radix"), ]
  prevcarga_version_hash(list(
    model_id = "backtest-run",
    data_fingerprint = "deck-agnostic",
    cutoff = list(window = format(sort(origins), "%Y-%m-%d"), cortes = sort(cortes, method = "radix")),
    feature_config = list(
      members = lapply(seq_len(nrow(by_member)), function(i) list(
        sigla = by_member$membro[[i]], mdir = by_member$mdir[[i]],
        exogena = if (is.na(by_member$exogena[[i]])) NULL else by_member$exogena[[i]],
        split = by_member$split[[i]], protocol = by_member$protocol[[i]])),
      folhas = sort(unique(cells$folha), method = "radix"),
      weather_issue_cutoff = if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff),
    code_commit = ident$code_version,
    renv_lock_hash = ident$lock_hash,
    load_label = ident$load_label,
    load_tz = ident$tz))
}

prevcarga_plan <- function(ctx, origins, cortes, members = NULL, folhas = NULL, protocol = NULL, filters = list()) {
  if (is.null(members)) members <- .m7_members(ctx, filters$model)
  if (is.null(folhas)) folhas <- .m7_leaves(ctx, filters$area)
  if (!is.null(protocol) && !protocol %in% .m7_protocols)
    prevcarga_abort("config.bad_protocol", where = "--protocol", details = list(got = as.character(protocol)[[1L]]))
  origins <- sort(unique(lubridate::as_date(origins)))
  mem <- data.table::as.data.table(members)
  mem <- mem[, c("sigla", "mdir", "exogena")]
  data.table::setnames(mem, "sigla", "membro")

  ## classe por diretorio de plugin: valida a matriz de compatibilidade antes de qualquer celula
  mdirs <- unique(mem$mdir)
  specs <- stats::setNames(lapply(mdirs, function(m) prevcarga_model_spec(m, ctx$root)), mdirs)
  cls <- lapply(specs, .m7_class, protocol = protocol)

  ident <- .m7_identity_inputs(ctx)
  weather <- if (is.null(ctx$weather_issue_cutoff)) "available_until" else ctx$weather_issue_cutoff
  ## data_lag_days do descriptor; os cortes de treino vem prontos do dates.R (nunca calculados aqui)
  lags <- list()
  for (m in mdirs) {
    plugin <- prevcarga_plugin_load(m, prevcarga_models_root(ctx$root))
    lags[[m]] <- as.integer(plugin$descriptor$data_lag_days)
  }

  g <- data.table::CJ(i = seq_len(nrow(mem)), folha = as.character(folhas), origem = origins, corte_hm = cortes,
                      sorted = FALSE)
  cells <- cbind(mem[g$i], g[, c("folha", "origem", "corte_hm")])
  cells[, c("split", "protocol") := list(vapply(mdir, function(m) cls[[m]]$split, ""),
                                         vapply(mdir, function(m) cls[[m]]$protocol, ""))]
  cells[, granularity_min := vapply(mdir, function(m) as.integer(specs[[m]]$granularity_min), 0L)]
  cells[, data_lag_days := vapply(mdir, function(m) lags[[m]], 0L)]
  ## fixed treina uma vez, na primeira origem da janela; os demais protocolos, na origem da celula
  cells[, cut_origin := data.table::fifelse(protocol == "fixed", min(origins), origem)]
  cutrows <- unique(cells[, c("mdir", "cut_origin")])
  cutrows[, c("train_origin", "last_data_day") := {
    rc <- prevcarga_resolve_cuts(ctx, cut_origin, model = mdir, data_lag_days = stats::setNames(lags[[mdir]], mdir))
    row <- rc$cuts[rc$cuts$model == mdir, ]
    list(row$train_origin, row$last_data_day)
  }, by = seq_len(nrow(cutrows))]
  cells <- cutrows[cells, on = c("mdir", "cut_origin")]
  cells[, cut_origin := NULL]
  cells[, available_until := lubridate::ymd_hm(paste(format(origem, "%Y-%m-%d"), corte_hm), tz = ident$tz)]
  cells[, cache_key := vapply(seq_len(.N), function(i) prevcarga_cache_key(
    "member_long", list(mdir = mdir[[i]], exogena = exogena[[i]]), specs[[mdir[[i]]]],
    ident$data_fingerprint, ident$load_label, ident$tz, weather_issue_cutoff = weather,
    origin = origem[[i]], available_until = available_until[[i]],
    code_version = ident$code_version, lock_hash = ident$lock_hash), "")]
  data.table::setorder(cells, membro, folha, origem, corte_hm)
  list(run_id = .m7_run_id(ctx, origins, cortes, cells, ident), cells = cells, origins = origins, cortes = cortes,
       protocol = protocol, load_label = ident$load_label, tz = ident$tz, ident = ident)
}

## --- 3. Plano canonico em disco -----------------------------------------------------------------------------
.m7_run_dir <- function(ctx, run_id) file.path(ctx$state_dir, "backtest", run_id)

.m7_write_atomic <- function(path, writer) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  writer(tmp)
  if (!file.rename(tmp, path)) prevcarga_abort("integrity.cache_put_conflict", where = path, details = list(reason = "rename"))
  invisible(path)
}

.m7_plan_doc <- function(plan) {
  cells <- plan$cells
  list(schema_version = 1L, run_id = plan$run_id,
       window = format(range(plan$origins), "%Y-%m-%d"), cortes = plan$cortes,
       protocol = if (is.null(plan$protocol)) NULL else plan$protocol, total = nrow(cells),
       cells = lapply(seq_len(nrow(cells)), function(i) list(
         membro = cells$membro[[i]], folha = cells$folha[[i]], origem = format(cells$origem[[i]], "%Y-%m-%d"),
         corte_hm = cells$corte_hm[[i]], split = cells$split[[i]], protocol = cells$protocol[[i]],
         cache_key = cells$cache_key[[i]])))
}

.m7_write_plan <- function(ctx, plan, resume = FALSE) {
  f <- file.path(.m7_run_dir(ctx, plan$run_id), "plan.json")
  if (isTRUE(resume) && file.exists(f)) {
    old <- tryCatch(jsonlite::read_json(f)$run_id, error = function(e) NULL)
    if (!identical(old, plan$run_id))
      prevcarga_abort("contract.plan_run_id_mismatch", where = f,
                      details = list(on_disk = if (is.null(old)) NA_character_ else old, requested = plan$run_id))
  }
  .m7_write_atomic(f, function(tmp)
    jsonlite::write_json(.m7_plan_doc(plan), tmp, auto_unbox = TRUE, null = "null", digits = NA))
}

## --- 4. Ledger derivado de presenca ---------------------------------------------------------------------------
.m7_cell_dir <- function(ctx, run_id, cell)
  file.path(.m7_run_dir(ctx, run_id), "members", cell$membro, cell$folha, format(cell$origem, "%Y-%m-%d"),
            gsub(":", "", cell$corte_hm, fixed = TRUE))

.m7_cell_id <- function(membro, folha, origem, corte_hm)
  paste(membro, folha, format(origem, "%Y-%m-%d"), corte_hm, sep = "|")

.m7_failures_path <- function(ctx, run_id) file.path(.m7_run_dir(ctx, run_id), "ledger", "failures.jsonl")

.m7_read_failures <- function(ctx, run_id) {
  f <- .m7_failures_path(ctx, run_id)
  if (!file.exists(f)) return(list())
  ln <- readLines(f, warn = FALSE)
  ln <- ln[nzchar(ln)]
  lapply(ln, function(l) jsonlite::fromJSON(l, simplifyVector = FALSE))
}

.m7_cell_state <- function(ctx, run_id, cell, failures = list()) {
  d <- .m7_cell_dir(ctx, run_id, cell)
  pq <- file.path(d, "long.parquet"); kf <- file.path(d, ".key")
  if (file.exists(pq) && file.exists(kf)) {
    k <- tryCatch(readLines(kf, n = 1L, warn = FALSE), error = function(e) character())
    if (length(k) == 1L && identical(k, cell$cache_key)) return("done")
  }
  id <- .m7_cell_id(cell$membro, cell$folha, cell$origem, cell$corte_hm)
  mine <- Filter(function(r) identical(r$cell, id), failures)
  if (length(mine) && identical(mine[[length(mine)]]$state, "failed")) "failed" else "pending"
}

.m7_publish_cell <- function(ctx, run_id, cell, long) {
  if (!is.data.frame(long)) prevcarga_abort("contract.run_cell_result", where = .m7_cell_id(cell$membro, cell$folha, cell$origem, cell$corte_hm))
  d <- .m7_cell_dir(ctx, run_id, cell)
  .m7_write_atomic(file.path(d, "long.parquet"), function(tmp) arrow::write_parquet(long, tmp))
  .m7_write_atomic(file.path(d, ".key"), function(tmp) writeLines(cell$cache_key, tmp))   # a chave publica por ultimo
  invisible(d)
}

## So o processo pai escreve (depois de recolher os resultados).
.m7_append_failures <- function(ctx, run_id, rows) {
  if (!length(rows)) return(invisible(NULL))
  f <- .m7_failures_path(ctx, run_id)
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  ln <- vapply(rows, function(r) as.character(jsonlite::toJSON(r, auto_unbox = TRUE, null = "null", na = "null")), "")
  cat(paste0(ln, "\n"), file = f, sep = "", append = TRUE)
  invisible(f)
}

## --- 5. Lock por diretorio exclusivo ------------------------------------------------------------------------
.m7_lock_acquire <- function(ctx, run_id, break_lock = FALSE) {
  lock <- file.path(.m7_run_dir(ctx, run_id), "run.lock")
  dir.create(dirname(lock), recursive = TRUE, showWarnings = FALSE)
  if (!dir.create(lock, showWarnings = FALSE)) {
    if (!isTRUE(break_lock))
      prevcarga_abort("state.lock_held", where = lock, details = list(run_id = run_id))
    prevcarga_warn("warning.lock_broken", where = run_id)
  }
  jsonlite::write_json(list(pid = Sys.getpid(), host = Sys.info()[["nodename"]]),
                       file.path(lock, "owner.json"), auto_unbox = TRUE)
  invisible(lock)
}

.m7_lock_release <- function(ctx, run_id) {
  unlink(file.path(.m7_run_dir(ctx, run_id), "run.lock"), recursive = TRUE)
  invisible(NULL)
}

## --- 6. Despacho e politica de falha -----------------------------------------------------------------------
## classe do erro: condicao do pacote (class_name) ou prefixo "[prevcarga/<classe>]" da mensagem.
.m7_err_class <- function(e) {
  if (!is.null(e$class_name)) return(e$class_name)
  m <- regmatches(conditionMessage(e), regexec("^\\[prevcarga/([a-z_]+)\\]", conditionMessage(e)))[[1L]]
  if (length(m) == 2L) m[[2L]] else "generic"
}

.m7_progress <- function(ctx, done, total) {
  if (is.function(ctx$progress)) ctx$progress(done, total)
  else message(sprintf("backtest: %d/%d", done, total))
  invisible(NULL)
}

## ordem de despacho: origem mais recente primeiro (o backtest anda para tras), depois membro, folha, corte.
.m7_dispatch_order <- function(idx, cells) idx[order(-as.numeric(cells$origem[idx]), cells$membro[idx],
                                                    cells$folha[idx], cells$corte_hm[idx], method = "radix")]

.m7_dispatch_serial <- function(ctx, run_id, cells, todo, run_cell, keep_going, state, done0, total) {
  stop_now <- FALSE
  done <- done0
  fails <- list()
  for (i in todo) {
    if (stop_now) { state[[i]] <- "skipped"; next }
    cell <- cells[i]
    res <- tryCatch({ .m7_publish_cell(ctx, run_id, cell, run_cell(cell)); NULL },
                    error = function(e) list(error_class = .m7_err_class(e),
                                             error_id = if (!is.null(e$id)) e$id else NA_character_,
                                             mensagem = conditionMessage(e)))
    if (is.null(res)) {
      state[[i]] <- "done"; done <- done + 1L
    } else {
      state[[i]] <- "failed"
      res$i <- i
      fails[[length(fails) + 1L]] <- res
      if (!isTRUE(keep_going)) stop_now <- TRUE
    }
    .m7_progress(ctx, done, total)
  }
  list(state = state, fails = fails)
}

prevcarga_orchestrate <- function(ctx, plan, run_cell, jobs = 1L, keep_going = FALSE, resume = FALSE,
                                  break_lock = FALSE) {
  if (!is.numeric(jobs) || length(jobs) != 1L || is.na(jobs) || jobs < 1L)
    prevcarga_abort("config.bad_type", where = "--jobs", details = list(expected = "inteiro >= 1"))
  if (jobs > 1L)
    prevcarga_abort("todo.not_implemented", where = "--jobs > 1", details = list(hint = "cluster PSOCK: M7.6"))
  run_id <- plan$run_id
  cells <- plan$cells
  total <- nrow(cells)
  .m7_lock_acquire(ctx, run_id, break_lock = break_lock)
  on.exit(.m7_lock_release(ctx, run_id), add = TRUE)
  .m7_write_plan(ctx, plan, resume = resume)

  failures <- .m7_read_failures(ctx, run_id)
  state <- vapply(seq_len(total), function(i) {
    if (isTRUE(resume)) .m7_cell_state(ctx, run_id, cells[i], failures) else "pending"
  }, "")
  done0 <- sum(state == "done")
  todo <- .m7_dispatch_order(which(state != "done"), cells)
  r <- .m7_dispatch_serial(ctx, run_id, cells, todo, run_cell, keep_going, state, done0, total)
  state <- r$state

  ts <- sub("-[0-9]+$", "", as.character(ctx$run_id))
  rows <- lapply(r$fails, function(f) {
    i <- f$i
    id <- .m7_cell_id(cells$membro[[i]], cells$folha[[i]], cells$origem[[i]], cells$corte_hm[[i]])
    prior <- sum(vapply(failures, function(x) identical(x$cell, id), NA))
    list(ts = ts, cell = id, membro = cells$membro[[i]], folha = cells$folha[[i]],
         origem = format(cells$origem[[i]], "%Y-%m-%d"), corte_hm = cells$corte_hm[[i]],
         available_until = format(cells$available_until[[i]], "%Y-%m-%dT%H:%M:%S%z", tz = lubridate::tz(cells$available_until[[i]])),
         classe = paste(cells$split[[i]], cells$protocol[[i]], sep = "/"), error_class = f$error_class,
         error_id = f$error_id, mensagem = f$mensagem, attempt = prior + 1L, state = "failed")
  })
  .m7_append_failures(ctx, run_id, rows)

  status <- data.table::copy(cells)
  status[, state := state]
  status[, c("error_id", "mensagem") := list(rep(NA_character_, total), rep(NA_character_, total))]
  for (f in r$fails) data.table::set(status, f$i, c("error_id", "mensagem"), list(f$error_id, f$mensagem))
  fcells <- which(state == "failed")
  ft <- data.table::data.table(
    membro = cells$membro[fcells], folha = cells$folha[fcells], origem = cells$origem[fcells],
    available_until = cells$available_until[fcells],
    classe = paste(cells$split[fcells], cells$protocol[fcells], sep = "/"),
    error_id = status$error_id[fcells], mensagem = status$mensagem[fcells])
  ok <- sum(state == "done")
  failed <- sum(state == "failed"); skipped <- sum(state == "skipped")
  verdict <- if (failed == 0L && skipped == 0L && sum(state == "pending") == 0L) "ok"
             else if (ok > 0L) "partial" else "failed"
  list(status = status, run_id = run_id,
       summary = list(ok = ok, total = total, failed = failed, skipped = skipped, failures = ft, verdict = verdict))
}

## --- 7. Fronteira dos verbos plan / backtest (flags -> plano -> result) ------------------------------------------
## Validacao de flags ANTES de ler o deck: --corte fora da grade, --protocol invalido e janela ausente saem como config (8).
prevcarga_plan_request <- function(ctx, args) {
  flag <- function(n) .prevcarga_tr_arg(args, n)
  origins <- .m7_origins(ctx)
  cortes <- .m7_parse_cortes(flag("--corte"), ctx$backtest$corte_default)
  proto <- flag("--protocol")
  if (!is.null(proto) && !proto %in% .m7_protocols)
    prevcarga_abort("config.bad_protocol", where = "--protocol", details = list(got = as.character(proto)[[1L]]))
  prevcarga_plan(ctx, origins, cortes, protocol = proto,
                 filters = list(model = flag("--model"), area = flag("--area")))
}

## `result` do plano: contagens e a classe de cada sigla; nenhum artefato e criado.
prevcarga_plan_result <- function(plan) {
  cells <- plan$cells
  sg <- unique(cells[, c("membro", "mdir", "exogena", "split", "protocol", "granularity_min")])
  sg <- sg[order(sg$membro, method = "radix"), ]
  by_member <- stats::setNames(lapply(seq_len(nrow(sg)), function(i) list(
    mdir = sg$mdir[[i]], exogena = if (is.na(sg$exogena[[i]])) NULL else sg$exogena[[i]],
    split = sg$split[[i]], protocol = sg$protocol[[i]], granularity_min = sg$granularity_min[[i]],
    cells = sum(cells$membro == sg$membro[[i]]))), sg$membro)
  list(schema_version = 1L, run_id = plan$run_id,
       window = list(from = format(min(plan$origins), "%Y-%m-%d"), to = format(max(plan$origins), "%Y-%m-%d"),
                     origins = length(plan$origins)),
       cortes = as.list(plan$cortes), folhas = length(unique(cells$folha)), total = nrow(cells),
       by_member = by_member)
}

.m7_plan_text <- function(res) {
  c(sprintf("plan run_id=%s janela=%s..%s origens=%d cortes=%s folhas=%d total=%d", res$run_id, res$window$from,
            res$window$to, res$window$origins, paste(unlist(res$cortes), collapse = ","), res$folhas, res$total),
    vapply(names(res$by_member), function(s) {
      m <- res$by_member[[s]]
      sprintf("%-10s %-16s %-14s %-4d celulas=%d", s, m$split, m$protocol, m$granularity_min, m$cells)
    }, ""))
}

## Celula do backtest: o M7.3 define `prevcarga_backtest_cell_runner(ctx, plan)` (train + infer por origem); sem ele, falha classificada.
.m7_run_cell_for <- function(ctx, plan) {
  mk <- get0("prevcarga_backtest_cell_runner", mode = "function")
  if (is.function(mk)) return(mk(ctx, plan))
  function(cell) prevcarga_abort("todo.not_implemented", where = "backtest cell -> M7.3")
}

.m7_failure_lines <- function(s) {
  ft <- s$failures
  c(sprintf("backtest ok=%d/%d failed=%d skipped=%d", s$ok, s$total, s$failed, s$skipped),
    vapply(seq_len(nrow(ft)), function(i) sprintf("falha %s %s %s %s %s %s", ft$membro[[i]], ft$folha[[i]],
      format(ft$origem[[i]], "%Y-%m-%d"), format(ft$available_until[[i]], "%H:%M"), ft$classe[[i]], ft$mensagem[[i]]), ""))
}

prevcarga_backtest_run <- function(ctx, args) {
  plan <- prevcarga_plan_request(ctx, args)
  jobs <- .prevcarga_tr_arg(args, "--jobs")
  jobs <- if (is.null(jobs)) 1L else suppressWarnings(as.integer(jobs))
  out <- prevcarga_orchestrate(ctx, plan, .m7_run_cell_for(ctx, plan), jobs = jobs,
                               keep_going = isTRUE(args[["--keep-going"]]), resume = isTRUE(args[["--resume"]]),
                               break_lock = isTRUE(args[["--break-lock"]]))
  s <- out$summary
  res <- list(schema_version = 1L, run_id = out$run_id, verdict = s$verdict,
              summary = list(ok = s$ok, total = s$total, failed = s$failed, skipped = s$skipped,
                             failures = lapply(seq_len(nrow(s$failures)), function(i) list(
                               membro = s$failures$membro[[i]], folha = s$failures$folha[[i]],
                               origem = format(s$failures$origem[[i]], "%Y-%m-%d"),
                               available_until = format(s$failures$available_until[[i]], "%Y-%m-%dT%H:%M:%S%z"),
                               classe = s$failures$classe[[i]], error_id = s$failures$error_id[[i]],
                               mensagem = s$failures$mensagem[[i]]))))
  if (!identical(s$verdict, "ok")) {
    if (!identical(ctx$format, "json")) writeLines(iconv(.m7_failure_lines(s), from = "UTF-8", to = "ASCII", sub = "?"), con = stderr())
    prevcarga_abort("partial.backtest_delivery", where = sprintf("ok=%d/%d", s$ok, s$total), result = res)
  }
  res
}
