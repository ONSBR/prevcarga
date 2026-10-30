#!/usr/bin/env Rscript
## plugin_gates.R -- runner comum dos gates de plugin + agregador (P.0h). Ferramenta de DESENVOLVIMENTO (fora do bundle).
##
## 1. Biblioteca: cada models/<m>/tests/run_all_gates.R (3 linhas, criado pelo epico do modelo) faz
##      source(<repo>/scripts/tools/plugin_gates.R)
##      gate_table <- <data.frame(gate, owner, tier)>
##      prevcarga_gates_run(gate_table, tier = "fast", model = "<m>")
##    e `--tier fast|parity|parity-full`, `--units <grupo>` (grupo de units.yaml; testes leem `pc_gate_units()`), `--format text|json`
##    na linha de comando do runner.
##    O runtime (TZ/locale/BLAS) e fixado in-process ANTES de tudo; nao fixavel => exit 7 com stdout vazio.
##    Os testes de models/<m>/tests/testthat rotulam cada teste `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:`.
##    Alem da gate_table do plugin, o runner DESCOBRE sub-gates em scripts/tools/gates/*.R (barrel por descoberta,
##    ADR-0040 item 8): ordem radix; cada pack define `prevcarga_gate_pack_<dono>(model, ctx, tier)` e devolve
##    linhas (gate, owner, tier_min, applies_to, test); `applies_to` = model_ids ou "all"; `test` = funcao que o
##    runner executa (erro => FAIL, testthat::skip => SKIP, silencio => PASS). Os scripts autonomos da pasta (m1..m4.R:
##    chamam quit(), nunca sao carregados in-process) nao definem pack e sao ignorados.
##    Exit: 0 sse todo gate bloqueante (dono nao-DEFERRED) do tier passou; 1 gate falhando (tools.gate_failed) ou
##    sem dono (tools.gate_unowned); 2 tier invalido (tools.bad_tier); 7 runtime nao fixavel.
##
## 2. Agregador: Rscript --vanilla scripts/tools/plugin_gates.R --models <m[,m2]|all> [--tier T] [--root <models>]
##      [--topology <yaml>]
##    `all` = unique(prevcarga_members()$mdir) (topologia). Cada runner roda como processo separado (system2); uma
##    linha por modelo; sai com o pior (maior) codigo. Runner ausente => FAIL tools.gate_failed com o caminho.

.PC_GATE_TIERS <- c(fast = 1L, parity = 2L, `parity-full` = 3L)
.PC_GATE_LABEL <- "^\\[(PLUGINS|DEFERRED M[0-9]+)\\] ([a-z0-9_]+):"

## --- localizacao do repositorio (captura no source; ou --file= quando e o script principal) ---------------------
.pc_gates_state <- new.env(parent = emptyenv())
.pc_gates_state$tier <- "fast"
.pc_gates_state$units <- NULL
.pc_gates_state$repo <- local({
  of <- tryCatch(sys.frame(1L)$ofile, error = function(e) NULL)
  if (!(is.character(of) && length(of) == 1L && file.exists(of))) {
    a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    of <- if (length(a) == 1L && grepl("plugin_gates\\.R$", a)) a else NULL
  }
  if (is.null(of)) NULL else dirname(dirname(dirname(normalizePath(of, winslash = "/"))))
})

.pc_gates_repo <- function() {
  if (is.null(.pc_gates_state$repo)) stop("plugin_gates.R: raiz do repositorio nao resolvida (use source(<arquivo>))", call. = FALSE)
  .pc_gates_state$repo
}

if (!exists("prevcarga_abort", mode = "function")) {
  options(prevcarga.lib = file.path(.pc_gates_repo(), "scripts", "lib"))
  for (.m in c("errors", "runtime")) source(file.path(.pc_gates_repo(), "scripts", "lib", paste0(.m, ".R")))
  rm(.m)
}

## --- helpers para os testes dos plugins ------------------------------------------------------------------------
pc_gate_deferred <- function(owner, reason) testthat::skip(sprintf("[DEFERRED %s] %s", owner, reason))

pc_gate_tier <- function(min) {
  if (.PC_GATE_TIERS[[.pc_gates_state$tier]] < .PC_GATE_TIERS[[min]])
    testthat::skip(sprintf("[tier] gate so roda no tier %s (atual: %s)", min, .pc_gates_state$tier))
  invisible(TRUE)
}

## grupo de unidades de paridade pedido por `--units <grupo>` no runner (NULL => o default do teste, por tier).
pc_gate_units <- function(default = NULL) if (is.null(.pc_gates_state$units)) default else .pc_gates_state$units

## golden ausente no tier parity FALHA (fail(), nunca skip). Golden = tests/parity/golden.lock versionado do modelo.
pc_gate_require_golden <- function(model, unit = NULL, root = file.path(.pc_gates_repo(), "models")) {
  lock <- file.path(root, model, "tests", "parity", "golden.lock")
  if (!file.exists(lock))
    testthat::fail(sprintf("golden ausente para %s%s (esperado %s)", model,
                           if (is.null(unit)) "" else paste0(" unidade ", unit), lock))
  invisible(lock)
}

## --- argv ------------------------------------------------------------------------------------------------------
.pc_gates_opt <- function(argv, name, default = NULL) {
  eq <- grep(paste0("^", name, "="), argv, value = TRUE)
  if (length(eq)) return(sub(paste0("^", name, "="), "", eq[[1L]]))
  i <- which(argv == name)
  if (length(i) && i[[1L]] < length(argv)) return(argv[[i[[1L]] + 1L]])
  default
}

.pc_gates_tier <- function(argv, tier) {
  t <- .pc_gates_opt(argv, "--tier", if (is.null(tier)) "fast" else tier)
  if (!is.character(t) || length(t) != 1L || is.na(t) || !(t %in% names(.PC_GATE_TIERS)))
    prevcarga_abort("tools.bad_tier", where = "--tier",
                    details = list(tier = if (is.character(t) && length(t)) t[[1L]] else "?", known = names(.PC_GATE_TIERS)))
  t
}

.pc_gates_ascii <- function(x) {
  x <- gsub("[\r\n\t]+", " ", as.character(x))
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "?")
  x <- iconv(x, "UTF-8", "ASCII", sub = "?")
  gsub("[^\\x20-\\x7E]", "?", x, perl = TRUE)
}

## --- normaliza uma linha de gate -------------------------------------------------------------------------------
.pc_gates_row <- function(gate, owner, tier_min, applies_to = "all", test = NULL, source, where = source) {
  if (!is.character(gate) || length(gate) != 1L || is.na(gate) || !grepl("^[a-z0-9_]+$", gate) ||
      !is.character(owner) || length(owner) != 1L || is.na(owner) || !nzchar(owner) ||
      !is.character(tier_min) || length(tier_min) != 1L || !(tier_min %in% names(.PC_GATE_TIERS)))
    prevcarga_abort("tools.gate_unowned", where = where,
                    details = list(gate = as.character(gate)[1L], reason = "linha de gate invalida (gate, owner, tier_min)"))
  list(gate = gate, owner = owner, tier_min = tier_min, applies_to = as.character(unlist(applies_to)),
       test = if (is.function(test)) test else NULL, source = source)
}

.pc_gates_table_rows <- function(gate_table) {
  gt <- as.data.frame(gate_table, stringsAsFactors = FALSE)
  tcol <- if ("tier_min" %in% names(gt)) "tier_min" else "tier"
  if (!all(c("gate", "owner", tcol) %in% names(gt)))
    prevcarga_abort("tools.gate_unowned", where = "gate_table",
                    details = list(reason = "gate_table precisa das colunas gate, owner, tier"))
  lapply(seq_len(nrow(gt)), function(i)
    .pc_gates_row(gt$gate[[i]], gt$owner[[i]], gt[[tcol]][[i]], source = "gate_table"))
}

## --- descoberta dos packs ---------------------------------------------------------------------------------------
.pc_gates_pack_files <- function(gates_dir) {
  if (is.null(gates_dir) || !dir.exists(gates_dir)) return(character())
  sort(list.files(gates_dir, pattern = "\\.R$", full.names = TRUE), method = "radix")
}

## Inspeciona o arquivo SEM executa-lo: "pack" (define a funcao), "standalone" (chama quit(): nunca carregar) ou "none".
.pc_gates_pack_kind <- function(f, fname) {
  ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
  if (is.null(ex)) return("none")
  defines <- any(vapply(ex, function(e) is.call(e) && as.character(e[[1L]])[1L] %in% c("<-", "=") && length(e) >= 2L &&
                          is.symbol(e[[2L]]) && identical(as.character(e[[2L]]), fname), NA))
  if (defines) return("pack")
  quits <- any(vapply(ex, function(e) any(c("quit", "q") %in% all.names(e)), NA))
  if (quits) "standalone" else "none"
}

.pc_gates_packs <- function(model, ctx, tier, gates_dir) {
  rows <- list()
  for (f in .pc_gates_pack_files(gates_dir)) {
    owner <- sub("\\.R$", "", basename(f))
    fname <- paste0("prevcarga_gate_pack_", owner)
    kind <- .pc_gates_pack_kind(f, fname)
    if (kind == "standalone") next
    if (kind != "pack")
      prevcarga_abort("tools.gate_unowned", where = basename(f),
                      details = list(file = basename(f), reason = sprintf("defina %s(model, ctx, tier)", fname)))
    env <- new.env(parent = baseenv())
    sys.source(f, envir = env)
    fn <- get0(fname, envir = env, inherits = FALSE)
    if (!is.function(fn))
      prevcarga_abort("tools.gate_unowned", where = basename(f), details = list(file = basename(f)))
    pack <- as.data.frame(fn(model, ctx, tier), stringsAsFactors = FALSE)
    if (!all(c("gate", "owner", "tier_min", "applies_to") %in% names(pack)))
      prevcarga_abort("tools.gate_unowned", where = basename(f),
                      details = list(file = basename(f), reason = "pack sem colunas gate, owner, tier_min, applies_to"))
    for (i in seq_len(nrow(pack))) {
      a <- as.character(unlist(pack$applies_to[[i]]))
      if (!("all" %in% a || model %in% a)) next
      tst <- if ("test" %in% names(pack)) pack$test[[i]] else NULL
      rows[[length(rows) + 1L]] <- .pc_gates_row(pack$gate[[i]], pack$owner[[i]], pack$tier_min[[i]], a, tst,
                                                 source = basename(f), where = basename(f))
    }
  }
  rows
}

## --- execucao dos testes e classificacao -----------------------------------------------------------------------
.pc_gates_msg <- function(results_cell, cls) {
  for (x in results_cell) if (inherits(x, cls)) return(conditionMessage(x))
  ""
}

.pc_gates_run_tests <- function(tests_dir) {
  if (!dir.exists(tests_dir)) return(data.frame(test = character(), failed = integer(), skipped = logical(),
                                                error = logical(), msg = character(), stringsAsFactors = FALSE))
  rep <- testthat::ListReporter$new()
  utils::capture.output(res <- testthat::test_dir(tests_dir, reporter = rep, stop_on_failure = FALSE,
                                                  stop_on_warning = FALSE))
  df <- as.data.frame(res)
  msg <- vapply(seq_len(nrow(df)), function(i) {
    m <- .pc_gates_msg(df$result[[i]], "expectation_failure")
    if (!nzchar(m)) m <- .pc_gates_msg(df$result[[i]], "expectation_error")
    if (!nzchar(m)) m <- .pc_gates_msg(df$result[[i]], "expectation_skip")
    m
  }, "")
  data.frame(test = df$test, failed = df$failed, skipped = df$skipped, error = df$error, msg = msg,
             stringsAsFactors = FALSE)
}

.pc_gates_run_fn <- function(fn) {
  tryCatch({ fn(); list(status = "PASS", detail = "") },
           skip = function(s) list(status = "SKIP", detail = conditionMessage(s)),
           error = function(e) list(status = "FAIL", detail = conditionMessage(e)))
}

.pc_gates_classify <- function(rows, tests, tier) {
  dups <- unique(vapply(rows, `[[`, "", "gate"))
  for (g in dups) {
    src <- vapply(rows[vapply(rows, function(r) identical(r$gate, g), NA)], `[[`, "", "source")
    if (length(src) > 1L)
      prevcarga_abort("tools.gate_unowned", where = sprintf("gate %s duplicado: %s", g, paste(src, collapse = " x ")),
                      details = list(gate = g, owners = src))
  }
  lab <- regmatches(tests$test, regexec(.PC_GATE_LABEL, tests$test))
  t_owner <- vapply(lab, function(m) if (length(m)) m[[2L]] else NA_character_, "")
  t_gate <- vapply(lab, function(m) if (length(m)) m[[3L]] else NA_character_, "")
  if (anyNA(t_gate))
    prevcarga_abort("tools.gate_unowned", where = tests$test[is.na(t_gate)][[1L]],
                    details = list(test = tests$test[is.na(t_gate)][[1L]], reason = "teste sem rotulo [PLUGINS]/[DEFERRED Mx] <gate>:"))
  table_gates <- vapply(rows, `[[`, "", "gate")
  fn_gates <- table_gates[vapply(rows, function(r) is.function(r$test), NA)]
  orphan <- setdiff(t_gate, table_gates)
  if (length(orphan))
    prevcarga_abort("tools.gate_unowned", where = sprintf("teste do gate %s sem linha na tabela", orphan[[1L]]),
                    details = list(gate = orphan[[1L]]))
  need <- .PC_GATE_TIERS[[tier]]
  out <- lapply(rows, function(r) {
    idx <- which(t_gate == r$gate)
    if (!length(idx) && !is.function(r$test))
      prevcarga_abort("tools.gate_unowned", where = sprintf("gate %s sem teste (%s)", r$gate, r$source),
                      details = list(gate = r$gate, owner = r$source))
    if (length(idx) && !all(t_owner[idx] == r$owner))
      prevcarga_abort("tools.gate_unowned", where = sprintf("gate %s: rotulo %s x tabela %s", r$gate, t_owner[idx][[1L]], r$owner),
                      details = list(gate = r$gate))
    if (.PC_GATE_TIERS[[r$tier_min]] > need)
      return(list(status = "N/A(tier)", detail = sprintf("so roda no tier %s", r$tier_min)))
    if (is.function(r$test)) return(.pc_gates_run_fn(r$test))
    t <- tests[idx, , drop = FALSE]
    bad <- t$failed > 0L | t$error
    if (any(bad)) return(list(status = "FAIL", detail = t$msg[bad][[1L]]))
    sk <- t$skipped
    if (all(sk)) {
      m <- t$msg[[1L]]
      ## skip de um gate do tier so e aceito como `[tier]` ou como DEFERRED; qualquer outro skip conta como FAIL
      if (grepl("^\\[tier\\]", m)) return(list(status = "N/A(tier)", detail = m))
      if (grepl("^DEFERRED", r$owner)) return(list(status = "SKIP", detail = m))
      return(list(status = "FAIL", detail = paste("skip nao permitido em gate do tier:", m)))
    }
    list(status = "PASS", detail = "")
  })
  data.frame(gate = table_gates, owner = vapply(rows, `[[`, "", "owner"),
             tier = vapply(rows, `[[`, "", "tier_min"),
             status = vapply(out, `[[`, "", "status"),
             detail = .pc_gates_ascii(vapply(out, `[[`, "", "detail")), stringsAsFactors = FALSE)
}

## --- render ----------------------------------------------------------------------------------------------------
.pc_gates_render_text <- function(tab) {
  det <- ifelse(nchar(tab$detail) > 70L, paste0(substr(tab$detail, 1L, 67L), "..."), tab$detail)
  c("gate | dono | status | detalhe",
    sprintf("%-16s | %-14s | %-9s | %s", tab$gate, paste0("[", tab$owner, "]"), tab$status, det))
}

.pc_gates_blocking <- function(tab) tab$status == "FAIL" & !grepl("^DEFERRED", tab$owner)

.pc_gates_main <- function(gate_table, tier, model, argv, gates_dir, tests_dir) {
  tier <- .pc_gates_tier(argv, tier)
  .pc_gates_state$tier <- tier
  .pc_gates_state$units <- .pc_gates_opt(argv, "--units", NULL)
  ctx <- list(model = model, tier = tier, tests_dir = tests_dir, gates_dir = gates_dir)
  rows <- c(.pc_gates_table_rows(gate_table), .pc_gates_packs(model, ctx, tier, gates_dir))
  tab <- .pc_gates_classify(rows, .pc_gates_run_tests(tests_dir), tier)
  res <- list(model = model, tier = tier, gates = tab)
  bad <- tab$gate[.pc_gates_blocking(tab)]
  if (length(bad))
    prevcarga_abort("tools.gate_failed", where = paste(bad, collapse = ","), details = list(gate = bad), result = res)
  res
}

.pc_gates_runner_tests_dir <- function() {
  a <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(a) != 1L) stop("plugin_gates.R: runner sem --file= (informe tests_dir)", call. = FALSE)
  file.path(dirname(normalizePath(a, winslash = "/")), "testthat")
}

prevcarga_gates_run <- function(gate_table, tier = NULL, model, argv = commandArgs(TRUE),
                                runtime_enforce = prevcarga_runtime_enforce,
                                gates_dir = file.path(.pc_gates_repo(), "scripts", "tools", "gates"),
                                tests_dir = .pc_gates_runner_tests_dir(), exit = TRUE) {
  fmt <- .pc_gates_opt(argv, "--format", "text")
  fin <- function(code) if (isTRUE(exit)) quit(save = "no", status = code) else invisible(code)
  ## 1a instrucao: runtime in-process; falha => exit 7, nada em stdout (erro so em stderr, em texto)
  rt <- prevcarga_with_errors(runtime_enforce())
  if (!is.null(rt$cond)) {
    writeLines(.pc_gates_ascii(prevcarga_render_error(rt$cond, "text", verb = "plugin-gates")), con = stderr())
    return(fin(rt$exit_code))
  }
  out <- prevcarga_with_errors(.pc_gates_main(gate_table, tier, model, argv, gates_dir, tests_dir))
  cond <- out$cond
  if (identical(fmt, "json")) {
    env <- if (is.null(cond)) prevcarga_envelope(verb = "plugin-gates", result = out$value)
           else prevcarga_envelope(verb = "plugin-gates", result = cond$result, cond = cond)
    writeLines(.pc_gates_ascii(as.character(jsonlite::toJSON(env, auto_unbox = TRUE, null = "null", digits = NA,
                                                             na = "null"))), con = stdout())
  } else {
    gates <- if (is.null(cond)) out$value$gates else cond$result$gates
    if (!is.null(gates)) writeLines(.pc_gates_render_text(gates), con = stdout())
    if (!is.null(cond)) writeLines(.pc_gates_ascii(prevcarga_render_error(cond, "text", verb = "plugin-gates")), con = stderr())
  }
  fin(out$exit_code)
}

## --- agregador -------------------------------------------------------------------------------------------------
pc_gates_aggregate <- function(models, root, tier, rscript = file.path(R.home("bin"), "Rscript")) {
  codes <- vapply(models, function(m) {
    runner <- file.path(root, m, "tests", "run_all_gates.R")
    if (!file.exists(runner)) {
      cat(sprintf("%-20s FAIL tools.gate_failed: runner ausente (esperado %s)\n", m, runner))
      return(1L)
    }
    out <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(runner), "--tier", tier), stdout = TRUE, stderr = TRUE))
    st <- attr(out, "status")
    st <- if (is.null(st)) 0L else as.integer(st)
    cat(sprintf("%-20s %s (exit %d)\n", m, if (st == 0L) "PASS" else "FAIL", st))
    if (st != 0L) writeLines(paste0("  | ", .pc_gates_ascii(out)), con = stderr())
    st
  }, integer(1))
  if (length(codes)) max(codes) else 0L
}

.pc_gates_aggregate_main <- function(argv) {
  repo <- .pc_gates_repo()
  tier <- .pc_gates_tier(argv, "fast")
  root <- .pc_gates_opt(argv, "--root", file.path(repo, "models"))
  topo_root <- dirname(normalizePath(root, winslash = "/", mustWork = FALSE))
  topo <- .pc_gates_opt(argv, "--topology", file.path(topo_root, "config", "topology.yaml"))
  if (!exists("prevcarga_members", mode = "function")) source(file.path(repo, "scripts", "lib", "topology.R"))
  want <- .pc_gates_opt(argv, "--models", NULL)
  if (is.null(want) || !nzchar(want))
    prevcarga_abort("tools.bad_args", where = "--models", details = list(reason = "informe --models <m[,m2]|all>"))
  known <- unique(prevcarga_members(path = topo, root = topo_root)$mdir)
  models <- if (identical(want, "all")) known else strsplit(want, ",", fixed = TRUE)[[1L]]
  unk <- setdiff(models, known)
  if (length(unk))
    prevcarga_abort("tools.bad_args", where = "--models", details = list(unknown = unk, known = known))
  pc_gates_aggregate(models, root, tier)
}

pc_gates_cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
  rt <- prevcarga_with_errors(prevcarga_runtime_enforce())
  if (!is.null(rt$cond)) {
    writeLines(.pc_gates_ascii(prevcarga_render_error(rt$cond, "text", verb = "plugin-gates")), con = stderr())
    quit(save = "no", status = rt$exit_code)
  }
  out <- prevcarga_with_errors(.pc_gates_aggregate_main(argv))
  if (!is.null(out$cond))
    writeLines(.pc_gates_ascii(prevcarga_render_error(out$cond, "text", verb = "plugin-gates")), con = stderr())
  quit(save = "no", status = if (is.null(out$cond)) as.integer(out$value) else out$exit_code)
}

if (!interactive() && any(grepl("^--file=.*plugin_gates\\.R$", commandArgs(FALSE)))) pc_gates_cli()
