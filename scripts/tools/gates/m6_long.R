## scripts/tools/gates/m6_long.R -- pack de sub-gates [M6] do painel LONG single-origin (M6.6), descoberto por prevcarga_gates_run (P.0h).
##
## Barrel por descoberta (ADR-0040 item 8): o runner de cada models/<m>/tests/run_all_gates.R chama so prevcarga_gates_run, que varre
## scripts/tools/gates/*.R (sort radix) e chama prevcarga_gate_pack_m6_long(model, ctx, tier). As linhas devolvidas (dono `M6`)
## SUBSTITUEM os slots `[DEFERRED M6]` das tabelas dos runners; nenhum runner e editado. `harness_*` ([DEFERRED M7]) seguem fora.
##
## O painel sai de prevcarga_build_long (M6.1) sobre uma entrega SINTETICA semi-horaria na grade REAL do membro: o fim da faixa
## (`delivery_days`) vem do descriptor do plugin (lido num `Rscript --vanilla` filho: a API da fundacao fica no globalenv do filho e o
## processo do runner nao e sujado). Nada de deck, treino ou inferencia: o gate cobre o CONTRATO do LONG, nao o modelo.
##
## Ids de gate (charset do runner: [a-z0-9_]): m6_schema_long, m6_ytrue_real, m6_anti_leak, m6_no_dayshift, m6_primeira_entrega,
## m6_fim_da_faixa. Cada um e um teste (erro => FAIL) sobre `prevcarga_long_subgates(panel, member, cuts)`.
## Sem variaveis de ambiente; datas so por lubridate com tz explicito; ASCII.

M6L_MODELS <- c("random-forest", "lgbm", "holt-winters", "dessem-linear", "dessem-regdin")
M6L_SUBGATES <- c("schema_long", "ytrue_real", "anti_leak", "no_dayshift", "primeira_entrega", "fim_da_faixa")
M6L_GATES <- c(paste0("m6_", M6L_SUBGATES), "m6_nowcast_dplus0")
M6L_TZ <- "Etc/GMT+3"
M6L_LIBS <- c("errors", "schema_long", "features", "build_long_output")
M6L_FOUNDATION <- c("runtime", "errors", "config", "provenance", "topology", "schema_long", "deck_reader", "validation", "dates",
                    "registry", "features", "training", "inference")

.m6l_state <- new.env(parent = emptyenv())

## Biblioteca de leitura do pack, num env proprio (nunca no globalenv do runner).
.m6l_lib <- function(libdir = NULL) {
  if (!is.null(libdir)) .m6l_state$libdir <- libdir
  if (is.null(.m6l_state$libdir)) .m6l_state$libdir <- getOption("prevcarga.lib")
  if (is.null(.m6l_state$libdir)) stop("m6_long: diretorio scripts/lib nao resolvido (options(prevcarga.lib))", call. = FALSE)
  if (is.null(.m6l_state$lib)) {
    L <- new.env(parent = globalenv())
    for (m in M6L_LIBS) sys.source(file.path(.m6l_state$libdir, paste0(m, ".R")), envir = L)
    .m6l_state$lib <- L
  }
  .m6l_state$lib
}

## --- sub-gates (puros; o painel e o unico insumo) ----------------------------------------------------------------------------
.m6l_row <- function(gate, ok, detail = "") data.frame(gate = gate, passed = isTRUE(ok), detail = detail, stringsAsFactors = FALSE)

.m6l_slot <- function(panel, L, tz) {
  st <- L$prevcarga_long_interval_start(panel$target_datetime, 30L, tz)
  (lubridate::hour(st) * 60L + lubridate::minute(st)) %/% 30L + 1L
}

.m6l_mape <- function(y, p) {
  ok <- !is.na(y) & !is.na(p) & y != 0
  if (!any(ok)) return(NA_real_)
  mean(abs((y[ok] - p[ok]) / y[ok]))
}

## Correlacao de y_pred com a verdade deslocada de k dias (48 leads): k = 0 e o mesmo dia.
.m6l_cor_shift <- function(panel, k) {
  p <- panel[order(panel$lead), , drop = FALSE]
  n <- nrow(p)
  i <- seq_len(n)
  j <- i + 48L * k
  keep <- j >= 1L & j <= n
  if (sum(keep) < 10L) return(NA_real_)
  a <- p$y_pred[i[keep]]; b <- p$y_true[j[keep]]
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < 10L || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) return(NA_real_)
  stats::cor(a[ok], b[ok])
}

## Devolve data.frame(gate, passed, detail), 1 linha por sub-gate (ids = M6L_SUBGATES), para UM painel single-origin.
## member: list(name, delivery_days); cuts: list(origin_day, available_until, tz).
prevcarga_long_subgates <- function(panel, member, cuts, libdir = NULL) {
  L <- .m6l_lib(libdir)
  tz <- cuts$tz
  grid <- L$prevcarga_delivery_grid(cuts, member$delivery_days)
  schema_ok <- tryCatch({ L$prevcarga_schema_long_check(panel); TRUE }, error = function(e) FALSE)
  types_ok <- schema_ok && all(vapply(names(L$prevcarga_schema_long),
                                      function(n) inherits(panel[[n]], L$prevcarga_schema_long[[n]]) ||
                                        (identical(L$prevcarga_schema_long[[n]], "double") && is.double(panel[[n]])) ||
                                        (identical(L$prevcarga_schema_long[[n]], "integer") && is.integer(panel[[n]])), NA))
  lead_ok <- types_ok && nrow(panel) > 0L && all(panel$lead == panel$d_plus * 48L + .m6l_slot(panel, L, tz))
  yt <- panel$y_true
  real_ok <- schema_ok && sum(!is.na(yt)) > 0L && !all(yt == 0, na.rm = TRUE) && stats::median(yt, na.rm = TRUE) > 5000
  mape <- if (schema_ok) .m6l_mape(panel$y_true, panel$y_pred) else NA_real_
  cs <- if (schema_ok) .m6l_cor_shift(panel, 0L) else NA_real_
  cm <- if (schema_ok) .m6l_cor_shift(panel, -1L) else NA_real_
  cp <- if (schema_ok) .m6l_cor_shift(panel, 1L) else NA_real_
  others <- c(cm, cp)[!is.na(c(cm, cp))]
  day_ok <- !is.na(cs) && cs > 0 && (!length(others) || cs > max(others))
  first_ok <- schema_ok && nrow(panel) > 0L && min(panel$target_datetime) == grid[[1L]]
  last_ok <- schema_ok && nrow(panel) == length(grid) && max(panel$target_datetime) == grid[[length(grid)]] &&
    identical(as.numeric(sort(panel$target_datetime)), as.numeric(grid))
  rbind(
    .m6l_row("schema_long", lead_ok, if (lead_ok) "" else "colunas/tipos do LONG ou lead != d_plus*48 + slot"),
    .m6l_row("ytrue_real", real_ok, if (real_ok) "" else "y_true ausente, todo zero ou mediana <= 5000"),
    .m6l_row("anti_leak", !is.na(mape) && mape > 0, sprintf("MAPE(y_true, y_pred) = %s", format(mape, digits = 4))),
    .m6l_row("no_dayshift", day_ok, sprintf("cor mesmo dia %s; D-1 %s; D+1 %s", format(cs, digits = 4), format(cm, digits = 4), format(cp, digits = 4))),
    .m6l_row("primeira_entrega", first_ok, if (first_ok) "" else "1o rotulo != available_until (+ 1 slot)"),
    .m6l_row("fim_da_faixa", last_ok, if (last_ok) "" else sprintf("faixa != grade de entrega (delivery_days = %d)", as.integer(member$delivery_days))))
}

## nowcast D+0 (oraculo gate_nowcast_dplus0): com corte intradiario, d_plus = 0 e o SUFIXO do dia O apos o corte (leads k+1..48, k = slots
## ate available_until); sem informacao do dia O (corte 00:00) o dia O sai inteiro (leads 1..48). Devolve data.frame(gate, passed, detail).
prevcarga_nowcast_check <- function(panel, cuts) {
  au <- cuts$available_until
  k <- (lubridate::hour(au) * 60L + lubridate::minute(au)) %/% 30L
  want <- if (k >= 48L) integer() else seq.int(k + 1L, 48L)
  got <- sort(as.integer(panel$lead[panel$d_plus == 0L]))
  ok <- identical(got, as.integer(want))
  .m6l_row("nowcast_dplus0", ok, sprintf("d_plus=0: leads %s..%s, esperado %s..%s", if (length(got)) min(got) else "-",
                                         if (length(got)) max(got) else "-", if (length(want)) min(want) else "-", if (length(want)) max(want) else "-"))
}

## --- fixture sintetica (pura: funcao de origem, corte e delivery_days) ----------------------------------------------------------
## Sinal sem periodicidade diaria exata (nivel e amplitude variam por dia), para o no_dayshift ter dente. y_pred = y_true * (1 + erro
## pequeno e deterministico). Posicao k (ordinal do rotulo em passos de 30 min desde O 00:00) so serve de fase do sinal.
.m6l_signal <- function(k) {
  d <- (k - 1L) %/% 48L
  s <- (k - 1L) %% 48L
  (9000 + 800 * cos(2.1 * d + 0.5)) + (2500 + 700 * sin(1.3 * d + 0.7)) * sin(2 * pi * (s - 8 + 5 * sin(2.9 * d + 1.1)) / 48)
}

m6l_fixture <- function(delivery_days, origin = lubridate::ymd("2026-01-15"), available_hm = "09:00", tz = M6L_TZ, libdir = NULL) {
  L <- .m6l_lib(libdir)
  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), tz)
  au <- lubridate::ymd_hm(paste(origin, available_hm), tz = tz)
  cuts <- list(origin_day = origin, available_until = au, tz = tz)
  grid <- L$prevcarga_delivery_grid(cuts, delivery_days)
  k <- as.integer(round(lubridate::time_length(grid - t0, "minute") / 30))
  truth <- data.table::data.table(interval_end = grid, load_mwh = .m6l_signal(k))
  pred <- truth$load_mwh * (1 + 0.02 * sin(0.37 * k))
  member <- list(name = "m6l", delivery_days = as.integer(delivery_days), granularity_min = 30L)
  panel <- L$prevcarga_build_long(data.table::data.table(interval_end = grid, y_pred = pred), member, "SIN", origin, au, truth,
                                  list(tz = tz))
  list(panel = panel, member = member, cuts = cuts, truth = truth)
}

## delivery_days do descriptor do plugin, lido num filho (nao suja o runner). Memoizado por (raiz, modelo).
.m6l_delivery_days <- function(model, root) {
  key <- paste(root, model)
  if (!is.null(.m6l_state$dd[[key]])) return(.m6l_state$dd[[key]])
  work <- tempfile("m6l-"); dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  script <- file.path(work, "child.R")
  out <- file.path(work, "dd.txt")
  writeLines(c("a <- commandArgs(TRUE)", "root <- a[[1L]]",
               "options(prevcarga.lib = file.path(root, 'scripts', 'lib'))",
               sprintf("for (m in %s) sys.source(file.path(root, 'scripts', 'lib', paste0(m, '.R')), envir = globalenv())",
                       paste(deparse(M6L_FOUNDATION), collapse = "")),
               "d <- prevcarga_plugin_descriptor(a[[2L]], prevcarga_models_root(root))",
               "writeLines(as.character(as.integer(d$delivery_days)), a[[3L]])"), script)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("--vanilla", shQuote(script), shQuote(root), shQuote(model), shQuote(out)), stdout = TRUE, stderr = TRUE))
  if (!file.exists(out))
    stop(sprintf("%s: descriptor nao lido (delivery_days): %s", model, paste(utils::tail(log, 3L), collapse = " | ")), call. = FALSE)
  dd <- as.integer(readLines(out, warn = FALSE)[[1L]])
  if (is.null(.m6l_state$dd)) .m6l_state$dd <- list()
  .m6l_state$dd[[key]] <- dd
  dd
}

## --- pack ------------------------------------------------------------------------------------------------------------------
prevcarga_gate_pack_m6_long <- function(model, ctx, tier) {
  root <- normalizePath(file.path(ctx$gates_dir, "..", "..", ".."), winslash = "/", mustWork = TRUE)
  libdir <- file.path(root, "scripts", "lib")
  memo <- new.env(parent = emptyenv())
  subgates <- function() {
    if (is.null(memo$res)) {
      dd <- .m6l_delivery_days(model, root)
      fx <- m6l_fixture(dd, libdir = libdir)
      memo$res <- prevcarga_long_subgates(fx$panel, fx$member, fx$cuts, libdir = libdir)
    }
    memo$res
  }
  nowcast <- function() {
    dd <- .m6l_delivery_days(model, root)
    for (hm in c("00:00", "12:00", "13:30")) {
      fx <- m6l_fixture(dd, available_hm = hm, libdir = libdir)
      r <- prevcarga_nowcast_check(fx$panel, fx$cuts)
      if (!r$passed) stop(sprintf("nowcast_dplus0 corte %s: %s", hm, r$detail), call. = FALSE)
    }
    invisible(TRUE)
  }
  mk <- function(g) function() {
    r <- subgates()
    r <- r[r$gate == g, , drop = FALSE]
    if (nrow(r) != 1L) stop(sprintf("sub-gate %s ausente", g), call. = FALSE)
    if (!r$passed) stop(sprintf("%s: %s", g, r$detail), call. = FALSE)
    invisible(TRUE)
  }
  df <- data.frame(gate = M6L_GATES, owner = "M6", tier_min = "fast", stringsAsFactors = FALSE)
  df$applies_to <- rep(list(M6L_MODELS), nrow(df))
  df$test <- c(lapply(M6L_SUBGATES, mk), list(nowcast))
  df
}
