## test-backtest-report.R -- M7.4: `backtest --report` (reporting.R secao .rpt_bt_*): acuracia, skill O-1/O-7 e SIN fresco em HTML+PDF portavel.
## O report so renderiza o painel avaliado (M7.7): as metricas vem de evaluation.R (aqui tambem, no preparo do insumo). Cada FORBIDDEN (delta 0)
## tem o seu CONTROL (delta != 0). Tudo em tempdir; sem variaveis de ambiente; PDF por grDevices::pdf (sem python3, sem dependencia de sistema).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "schema_long", "evaluation", "honesty_gate",
                       "reporting", "orchestrate", "backtest"))
suppressMessages(env$prevcarga_runtime_enforce())

TZ <- PC_M7_TZ
O <- lubridate::ymd("2026-10-05")
at <- function(d, hm) lubridate::ymd_hm(paste(format(lubridate::as_date(d)), hm), tz = TZ)
sha <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)
expect_pc <- function(expr, id, exit = 2L) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (!is.null(cond)) { expect_identical(cond$id, id); expect_identical(cond$exit_code, as.integer(exit)) }
  invisible(cond)
}

## --- insumo: LONG avaliado por formula (SIN e subsistemas X, Y) -> scorecards via evaluation.R -----------------------------
## y_true(t) = 1000 + 50 sin(h/3) + 10 d; previsao = y_true * (1 + eps_area); o baseline O-1 e o proprio y_true deslocado.
mk_eval_long <- function(ytrue_shift = c(X = 0, Y = 0), pred_scale = 1.04, areas = c("X", "Y")) {
  ie <- at(O, "00:00") + lubridate::hours(1:48)
  d <- do.call(rbind, lapply(areas, function(a) data.frame(
    area = a, forecast_origin = format(O), target_datetime = ie, lead = 1:48, d_plus = rep(0:1, each = 24L),
    y_true = 1000 + 50 * sin((1:48) / 3) + ytrue_shift[[a]], granularity_min = 60L, stringsAsFactors = FALSE)))
  d$y_pred <- 1000 + 50 * sin((d$lead) / 3)
  d$y_pred <- d$y_pred * pred_scale
  d$interval_start <- d$target_datetime - lubridate::hours(1)
  d
}

## monta `scores` como o REDUCE: SIN = soma dos subsistemas (previsao e verdade), scorecards so de evaluation.R
mk_scores <- function(ytrue_shift = c(X = 0, Y = 0), baseline_pred = NULL, awaiting = FALSE, pred_scale = 1.04) {
  d <- mk_eval_long(ytrue_shift, pred_scale)
  s <- stats::aggregate(cbind(y_true, y_pred) ~ forecast_origin + target_datetime + lead + d_plus + interval_start + granularity_min, d, sum)
  s$area <- "SIN"; s <- s[names(d)]
  all <- rbind(d, s); all$corte_hm <- "08:00"
  bl <- all
  bl$y_pred <- if (is.null(baseline_pred)) all$y_true * 0.97 else baseline_pred(all)
  by <- c("corte_hm", "d_plus", "area")
  sc <- env$prevcarga_scorecard(all, by = by)
  sc$skill_persistencia_O1 <- env$prevcarga_scorecard(all, by = by, baseline_long = bl)$skill
  sc$skill_sazonal_O7 <- NA_real_
  sc <- sc[sc$area != "SIN" | TRUE, ]
  sinc <- sc[sc$area %in% c("X", "Y", "SIN"), ]
  mem <- env$prevcarga_scorecard(transform(d, member = "RF", corte_hm = "08:00"), by = c("corte_hm", "member", "d_plus", "area"))
  mem$skill_persistencia_O1 <- env$prevcarga_scorecard(transform(d, member = "RF", corte_hm = "08:00"),
                                                       by = c("corte_hm", "member", "d_plus", "area"),
                                                       baseline_long = transform(bl[bl$area != "SIN", ], member = "RF"))$skill
  sin_rows <- all[all$area == "SIN", ]
  list(member = mem, combined = sc[sc$area %in% c("X", "Y"), ], sin = sinc[sinc$area %in% c("X", "Y", "SIN"), ],
       peak_fixed_18_22 = data.frame(sinc[c("corte_hm", "area", "d_plus")], wmape = sinc$peak, bias_signed = sinc$bias),
       sin_fresh = list(wmape = env$prevcarga_wmape(sin_rows$y_true, sin_rows$y_pred), sum_check = 0),
       awaiting_truth = data.frame(origem = c(O, O + 1L), corte_hm = "08:00", available_until = c(at(O, "08:00"), at(O + 1L, "08:00")),
                                   awaiting = c(FALSE, awaiting)),
       fallback_months = data.frame(folha = character(), corte_hm = character(), mes = character()),
       by_available_until = TRUE, dplus = TRUE)
}

mk_meta <- function() list(
  run_id = "bt-run-0123", origins = list(from = O, to = O + 1L, n = 2L),
  cortes = data.table::data.table(corte_hm = "08:00", available_until = at(O, "08:00")),
  weather_issue_cutoff = "available_until",
  members = data.table::data.table(sigla = "RF", split = "origin_agnostic", protocol = "daily", granularity_min = 60L),
  load_label = "start", timezone = TZ, version = "c0ffee")

render <- function(scores = mk_scores(), meta = mk_meta(), format = "html", .frame = parent.frame()) {
  out <- withr::local_tempdir("bt-rep-", .local_envir = .frame)
  p <- env$prevcarga_backtest_report(list(), scores, meta, format, out)
  list(path = p, dir = out)
}
html_of <- function(...) { r <- render(...); rawToChar(readBin(r$path, "raw", file.size(r$path))) }

test_that("[AC1] HTML: blocos de acuracia, pico+vies com sinal, skill O-1/O-7, SIN fresco com sum_check, cortes e regra do clima", {
  h <- html_of()
  for (id in c("proveniencia", "horizonte", "pico", "skill", "sin", "fallback", "aguardando"))
    expect_true(grepl(sprintf("<section id=\"%s\">", id), h, fixed = TRUE), info = id)
  expect_true(grepl("WMAPE por horizonte (d_plus)", h, fixed = TRUE))
  expect_true(grepl("janela 18h-22h sobre interval_start", h, fixed = TRUE))
  expect_true(grepl("vies com sinal", h, fixed = TRUE))
  expect_true(grepl("skill O-1", h, fixed = TRUE)); expect_true(grepl("skill O-7", h, fixed = TRUE))
  expect_true(grepl("SIN = soma dos subsistemas", h, fixed = TRUE)); expect_true(grepl("sum_check", h, fixed = TRUE))
  expect_true(grepl("<td>ok</td>", h, fixed = TRUE))
  expect_true(grepl("available_until simulado", h, fixed = TRUE)); expect_true(grepl("2026-10-05 08:00", h, fixed = TRUE))
  expect_true(grepl("weather_issue_cutoff", h, fixed = TRUE)); expect_true(grepl("<td>available_until</td>", h, fixed = TRUE))
  expect_true(grepl("<td>+", h, fixed = TRUE))                         # vies de pico com sinal explicito
  expect_true(grepl("<svg ", h, fixed = TRUE))
  for (tok in c("plotly", "http", "<script")) expect_false(grepl(tok, h, ignore.case = TRUE), info = tok)
  expect_false(grepl("\r", h, fixed = TRUE))
  expect_true(validUTF8(h))
  expect_identical(basename(render()$path), "backtest.html")
})

test_that("[AC1] PDF valido (%PDF-, >= 1 pagina), byte-deterministico, sem python3 nem binario de sistema", {
  r1 <- render(format = "pdf"); r2 <- render(format = "pdf")
  b <- readBin(r1$path, "raw", file.size(r1$path))
  expect_identical(rawToChar(b[1:5]), "%PDF-")
  txt <- iconv(rawToChar(b[b != as.raw(0)]), "latin1", "ASCII", sub = "?")
  expect_gte(lengths(regmatches(txt, gregexpr("/Type /Page\\b", txt))), 1L)
  expect_true(grepl("WMAPE por horizonte", txt, fixed = TRUE))
  expect_identical(sha(readBin(r1$path, "raw", file.size(r1$path))), sha(readBin(r2$path, "raw", file.size(r2$path))))
  expect_true(file.exists(file.path(r1$dir, "backtest.html")))         # o HTML sai antes do PDF
  expect_identical(basename(r1$path), "backtest.pdf")
  src <- readLines(file.path(pc_root(), "scripts", "lib", "reporting.R"))
  expect_false(any(grepl("python|system2?\\(|Sys\\.which|plotly|kaleido|pandoc", src, ignore.case = TRUE)))
  ## CONTROL: o PDF muda quando o dado muda
  p3 <- render(mk_scores(pred_scale = 1.2), format = "pdf")
  expect_false(identical(sha(readBin(p3$path, "raw", file.size(p3$path))), sha(readBin(r1$path, "raw", file.size(r1$path)))))
})

test_that("[AC2] CONTROL: membro == baseline O-1 => |skill| <= 1e-9; FORBIDDEN-vizinho: baseline pior => skill != 0", {
  sc <- mk_scores(baseline_pred = function(all) all$y_pred)
  expect_lte(max(abs(sc$combined$skill_persistencia_O1)), 1e-9)
  h <- html_of(sc)
  expect_true(grepl("<td>0.0000</td>", h, fixed = TRUE))
  sc2 <- mk_scores()
  expect_gt(max(abs(sc2$combined$skill_persistencia_O1)), 1e-3)
  expect_false(grepl("<td>0.0000</td><td>sem baseline</td>", html_of(sc2), fixed = TRUE))
})

test_that("[AC2] sem verdade fresca: 'aguardando verdade' sem numero e nunca 0; sem baseline vira texto", {
  sc <- mk_scores(awaiting = TRUE)
  sc$combined$wmape[sc$combined$d_plus == 1L] <- NA_real_
  sc$sin$wmape[sc$sin$d_plus == 1L] <- NA_real_
  h <- html_of(sc)
  expect_true(grepl("<td>2026-10-06</td><td>08:00</td><td>2026-10-06 08:00</td><td>aguardando verdade</td>", h, fixed = TRUE))
  expect_false(grepl("<td>2026-10-05</td><td>08:00</td><td>2026-10-05 08:00</td><td>aguardando verdade</td>", h, fixed = TRUE))
  ## linhas do d_plus 1 do combinado: texto, nunca "0.00"
  hsec <- sub("(?s).*<section id=\"horizonte\">(.*?)</section>.*", "\\1", h, perl = TRUE)
  rows <- regmatches(hsec, gregexpr("<tr><td>X</td><td>08:00</td><td>1</td>[^\n]*", hsec))[[1L]]
  expect_true(length(rows) >= 1L)
  expect_true(all(grepl("aguardando verdade", rows, fixed = TRUE)))
  expect_false(any(grepl("<td>0\\.00</td>", rows)))
  expect_identical(env$.rpt_bt_value(NA_real_), "aguardando verdade")
  expect_identical(env$.rpt_bt_value(NULL), "aguardando verdade")
  expect_identical(env$.rpt_bt_value(NA, missing = "sem baseline"), "sem baseline")
  expect_identical(env$.rpt_bt_value(0), "0.00")                      # zero real continua sendo numero
  expect_true(grepl("sem baseline", html_of(utils::modifyList(mk_scores(), list(combined = {
    d <- mk_scores()$combined; d$skill_persistencia_O1 <- NULL; d$skill_sazonal_O7 <- NULL; d }))), fixed = TRUE))
  ## CONTROL: com verdade nas duas origens ninguem e listado como aguardando
  expect_true(grepl("<td>nenhuma</td>", html_of(mk_scores(awaiting = FALSE)), fixed = TRUE))
})

test_that("[AC2] SIN fresco: perturbar y_true de UM subsistema muda o SIN mostrado; sum_check acima da tol e sinalizado", {
  sin_wmape <- function(shift) {
    d <- mk_eval_long(ytrue_shift = shift)
    s <- stats::aggregate(cbind(y_true, y_pred) ~ lead, d, sum)
    env$prevcarga_wmape(s$y_true, s$y_pred)
  }
  base <- mk_scores(); pert <- mk_scores(ytrue_shift = c(X = 80, Y = 0))
  expect_false(isTRUE(all.equal(sin_wmape(c(X = 0, Y = 0)), sin_wmape(c(X = 80, Y = 0)))))
  expect_false(identical(html_of(base), html_of(pert)))
  get_sin <- function(h) regmatches(h, regexpr("SIN fresco wmape %</td><td>[0-9.]+", h))
  expect_false(identical(get_sin(html_of(base)), get_sin(html_of(pert))))
  ## a verdade do SIN do REDUCE e a soma dos subsistemas (.bt_sin_truth): perturbar X move a verdade do SIN
  topo <- list(order = c("X", "Y"))
  rec <- function(shift) {
    d <- mk_eval_long(ytrue_shift = shift); s <- d[d$area == "X", ]; s$area <- "SIN"
    env$.bt_sin_truth(data.table::as.data.table(rbind(d, s)), topo)[area == "SIN"]$y_true
  }
  expect_equal(rec(c(X = 0, Y = 0)), 2 * (1000 + 50 * sin((1:48) / 3)))
  expect_gt(max(abs(rec(c(X = 80, Y = 0)) - rec(c(X = 0, Y = 0)))), 79)
  bad <- mk_scores(); bad$sin_fresh$sum_check <- 1e-3
  expect_true(grepl("<td>FALHOU</td>", html_of(bad), fixed = TRUE))
})

test_that("[AC2] meses em fallback n_min aparecem carimbados", {
  sc <- mk_scores()
  sc$fallback_months <- data.frame(folha = c("A", "B"), corte_hm = "08:00", mes = c("2026-09", "2026-10"))
  h <- html_of(sc)
  expect_true(grepl("<td>A</td><td>08:00</td><td>2026-09</td>", h, fixed = TRUE))
  expect_true(grepl("<td>B</td><td>08:00</td><td>2026-10</td>", h, fixed = TRUE))
  expect_true(grepl("<td>nenhum</td>", html_of(mk_scores()), fixed = TRUE))   # CONTROL: sem fallback, sem carimbo
})

test_that("[AC3] sem verdade nos scores => [prevcarga/contract] exit 2; run_meta incompleto => contract.report_input", {
  sc <- mk_scores()
  no_truth <- sc[setdiff(names(sc), c("sin_fresh", "awaiting_truth"))]
  expect_pc(env$prevcarga_backtest_report(list(), no_truth, mk_meta(), "html", withr::local_tempdir()), "contract.report_without_truth")
  nowm <- sc; nowm$combined <- nowm$combined[setdiff(names(nowm$combined), "wmape")]
  expect_pc(env$prevcarga_backtest_report(list(), nowm, mk_meta(), "html", withr::local_tempdir()), "contract.report_without_truth")
  m <- mk_meta(); m$weather_issue_cutoff <- NULL
  expect_pc(env$prevcarga_backtest_report(list(), sc, m, "html", withr::local_tempdir()), "contract.report_input")
  expect_error(env$prevcarga_backtest_report(list(), sc, mk_meta(), "docx", withr::local_tempdir()))
  ## nada escrito quando a entrada e recusada
  out <- withr::local_tempdir()
  try(env$prevcarga_backtest_report(list(), no_truth, mk_meta(), "pdf", out), silent = TRUE)
  expect_length(list.files(out), 0L)
  tsv <- utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "backtest.tsv"), stringsAsFactors = FALSE)
  expect_identical(tsv$class[tsv$id == "contract.report_without_truth"], "contract")
})

## --- honestidade do baseline (honesty_gate): baseline sabotado (le target >= O) x baseline real de evaluation.R ------------------
hg_panel <- function() {
  ie <- seq(at(O - 12L, "01:00"), at(O + 11L, "00:00"), by = "hour")
  data.frame(interval_start = ie - lubridate::hours(1), interval_end = ie, y_true = 1000 + 100 * sin(as.numeric(ie) / 20000))
}
hg_long <- function() {
  ie <- at(O, "00:00") + lubridate::hours(1:240)
  data.frame(area = "A", forecast_origin = format(O), target_datetime = ie, lead = 1:240, d_plus = (0:239) %/% 24L,
             y_pred = NA_real_, stringsAsFactors = FALSE)
}
real_baseline_fn <- function(kind) {
  ev <- new.env(parent = env)
  ev$prevcarga_read_actual <- function(ctx, area, from = NULL, to = NULL)
    data.frame(interval_end = ev$.panel$interval_end, y_true = ev$.panel$y_true)  # o leitor nao filtra: a honestidade e do baseline
  function(panel) {
    ev$.panel <- panel
    fn <- env$prevcarga_baseline
    environment(fn) <- ev
    fn(list(tz = TZ), "A", format(O), kind, hg_long())
  }
}
sabotaged_fn <- function(panel) {                                    # le carga com target >= O (a verdade do proprio alvo)
  l <- hg_long()
  l$y_pred <- panel$y_true[match(as.numeric(l$target_datetime), as.numeric(panel$interval_end))]
  l
}

test_that("[AC3] honesty-gate do report: baseline sabotado => VAZA (FORBIDDEN pega); baseline real => HONESTO (delta 0 / CONTROL delta != 0)", {
  cut <- at(O, "00:00")
  bad <- env$prevcarga_honesty_gate(sabotaged_fn, hg_panel(), cut, 9L)
  expect_identical(bad$verdict, "VAZA")
  expect_gt(bad$forbidden_delta, 1e-5)
  for (kind in c("persistence", "seasonal_naive")) {
    ok <- env$prevcarga_honesty_gate(real_baseline_fn(kind), hg_panel(), cut, 9L)
    expect_identical(ok$verdict, "HONESTO", info = kind)
    expect_lte(ok$forbidden_delta, 1e-5)
    expect_gt(ok$control_delta, 1e-5)
    expect_true(ok$reach_ok)
  }
})

## --- nao-regressao do forecast --report (AC4) ---------------------------------------------------------------------------
test_that("[AC4] nenhum helper de acuracia e alcancavel a partir de prevcarga_report; o HTML do forecast nao tem WMAPE/skill/aguardando verdade", {
  fenv <- env
  fns <- Filter(function(n) is.function(get(n, envir = fenv)), ls(fenv, all.names = TRUE))
  in_rep <- fns[vapply(fns, function(n) identical(environment(get(n, envir = fenv)), fenv), NA)]
  reach <- "prevcarga_report"; todo <- reach
  while (length(todo)) {
    g <- codetools::findGlobals(get(todo[[1L]], envir = fenv), merge = TRUE)
    nxt <- setdiff(intersect(g, in_rep), reach)
    reach <- c(reach, nxt); todo <- c(todo[-1L], nxt)
  }
  expect_true(all(c("prevcarga_report", ".report_pdf", ".pdf_open", "prevcarga_html_page") %in% reach))   # o grafo foi de fato percorrido
  expect_false(any(grepl("^\\.rpt_bt_|backtest|wmape|skill|y_true", reach)), info = paste(reach, collapse = ","))
  ## CONTROL: o mesmo percurso a partir do report do backtest alcanca os helpers de acuracia
  reach2 <- "prevcarga_backtest_report"; todo <- reach2
  while (length(todo)) {
    g <- codetools::findGlobals(get(todo[[1L]], envir = fenv), merge = TRUE)
    nxt <- setdiff(intersect(g, in_rep), reach2)
    reach2 <- c(reach2, nxt); todo <- c(todo[-1L], nxt)
  }
  expect_true(any(grepl("^\\.rpt_bt_", reach2)))
  ## HTML e PDF do forecast
  t0 <- lubridate::force_tz(lubridate::ymd("2026-01-15"), TZ)
  long <- do.call(rbind, lapply(c("SIN", "SS1", "A1"), function(a) data.frame(
    area = a, forecast_origin = "2026-01-15", target_datetime = t0 + lubridate::minutes(30L * (1:48)), lead = 1:48,
    y_pred = 100 + 10 * sin((1:48) / 8), y_true = NA_real_, d_plus = 0L, granularity_min = 30L)))
  d <- long[long$area == "A1", ]
  base <- list(persistence_o1 = data.frame(area = d$area, target_datetime = d$target_datetime, y_base = d$y_pred - 5),
               seasonal_o7 = data.frame(area = d$area, target_datetime = d$target_datetime, y_base = d$y_pred + 7))
  prov <- list(versao = "abc123abc123", origem = "2026-01-15", available_until = "2026-01-15T09:00",
               available_until_por_area = list(A1 = "2026-01-15T09:00"), load_label = "end", timezone = TZ,
               weather_issue_cutoff = "available_until", subsistemas = "SS1")
  w <- data.frame(area = c("A1", "A1"), member = c("RF", "HW"), weight = c(0.6, 0.4), intercept = c(0, 0))
  out <- withr::local_tempdir()
  p <- env$prevcarga_report(long, w, list(), prov, base, format = "html", out = out)
  h <- rawToChar(readBin(p, "raw", file.size(p)))
  for (tok in c("WMAPE", "skill", "aguardando verdade")) expect_false(grepl(tok, h, fixed = TRUE), info = tok)
  expect_true(grepl("<section id=\"baselines\">", h, fixed = TRUE))  # CONTROL: o forecast ainda renderiza o que e dele
  ## greps no codigo
  src <- readLines(file.path(pc_root(), "scripts", "lib", "reporting.R"))
  expect_length(grep("wmape <- function", src, fixed = TRUE), 0L)
  expect_length(grep("plotly", src, ignore.case = TRUE), 0L)
})

## --- fio no verbo --------------------------------------------------------------------------------------------------------
test_that("[AC1] fio: .bt_run_meta do plano e .bt_report_step ok/failed (a falha vira partial.report_failed no verbo, exit 14)", {
  cells <- data.table::data.table(
    membro = c("RF", "HW"), split = c("origin_agnostic", "origin_scoped"), protocol = c("daily", "retrain_harness"),
    granularity_min = 60L, corte_hm = "08:00", available_until = at(O, "08:00"))
  plan <- list(run_id = "bt-run-0123", cells = cells, origins = c(O, O + 1L), weather_issue_cutoff = "origin_midnight_utc",
               load_label = "start", tz = TZ, ident = list(code_version = "c0ffee"))
  m <- env$.bt_run_meta(plan)
  expect_identical(m$weather_issue_cutoff, "origin_midnight_utc")
  expect_identical(m$members$sigla, c("HW", "RF"))
  expect_identical(m$origins$n, 2L)
  expect_identical(nrow(m$cortes), 1L)
  paths <- list(report = file.path(withr::local_tempdir(), "report"))
  ok <- env$.bt_report_step(list(), plan, mk_scores(), paths, "html")
  expect_identical(ok$status, "ok")
  expect_true(file.exists(ok$path)); expect_identical(dirname(ok$path), paths$report)
  expect_true(grepl("origin_midnight_utc", rawToChar(readBin(ok$path, "raw", file.size(ok$path))), fixed = TRUE))
  bad <- env$.bt_report_step(list(), plan, NULL, paths, "html")
  expect_identical(bad$status, "failed"); expect_null(bad$path)
  expect_match(attr(bad, "cause"), "")
  ## o verbo (orchestrate.R) chama o passo depois do REDUCE e converte falha em partial.report_failed (exit 14), com o result
  src <- paste(readLines(file.path(pc_root(), "scripts", "lib", "orchestrate.R")), collapse = "\n")
  expect_true(grepl("partial.report_failed", src, fixed = TRUE))
  expect_true(grepl("report_path = rep$path", src, fixed = TRUE))
  ## relatorio so no formato validado pelo verbo
  cli <- paste(readLines(file.path(pc_root(), "scripts", "lib", "cli", "backtest.R")), collapse = "\n")
  expect_true(grepl("\"--report-format\"", cli, fixed = TRUE))
})
