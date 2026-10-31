## test-reporting.R -- reporting.R + `forecast --report` (M6.5): relatorio DIAGNOSTICO, sem acuracia, deterministico e escapado.
## Blocos puros com insumo sintetico (rapido) + rodada de forecast com report = TRUE sobre o fixture do M6.4 (3 membros reais, espioes).
## Cada FORBIDDEN (delta 0) tem o seu CONTROL (delta != 0). Tudo em tempdir; sem variaveis de ambiente.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "reporting", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
t0 <- lubridate::force_tz(lubridate::ymd("2026-01-15"), TZ)
mk_long <- function(areas = c("SIN", "SS1", "A1"), y_true = NA_real_) {
  do.call(rbind, lapply(seq_along(areas), function(i) data.frame(
    area = areas[[i]], forecast_origin = "2026-01-15", target_datetime = t0 + lubridate::minutes(30L * (1:48)),
    lead = 1:48, y_pred = 100 * i + 10 * sin((1:48) / 8), y_true = y_true, d_plus = 0L, granularity_min = 30L)))
}
mk_weights <- function(w = 0.6) data.frame(area = c("A1", "A1"), member = c("RF", "HW"), weight = c(w, 1 - w), intercept = c(0, 0))
mk_khat <- function() list(list(ss = "SS1", versao = "v_0123456789abcdef", khat_origin = "2026-01-14", khat_age_days = 1L))
mk_prov <- function(au = "2026-01-15T09:00", extra = list(), area_name = "A1") utils::modifyList(list(
  versao = "abc123abc123", origem = "2026-01-15", available_until = au,
  available_until_por_area = stats::setNames(list(au, "2026-01-15T08:30"), c(area_name, "SS1")), load_label = "end",
  timezone = TZ, weather_issue_cutoff = "available_until", subsistemas = "SS1"), extra)
mk_base <- function(long, off = 0, area = "A1") {
  d <- long[long$area == area, ]
  list(persistence_o1 = data.frame(area = d$area, target_datetime = d$target_datetime, y_base = d$y_pred - 5 + off),
       seasonal_o7 = data.frame(area = d$area, target_datetime = d$target_datetime, y_base = d$y_pred + 7 + off))
}
rep_html <- function(long = mk_long(), weights = mk_weights(), khat = mk_khat(), prov = mk_prov(), base = mk_base(long),
                     .frame = parent.frame()) {
  out <- withr::local_tempdir("rep-", .local_envir = .frame)
  p <- env$prevcarga_report(long, weights, khat, prov, base, format = "html", out = out)
  rawToChar(readBin(p, "raw", file.size(p)))
}
sha <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)

test_that("[AC1] seis blocos, available_until global e por area, khat copiado do reconcile, UTF-8 sem fonte externa", {
  h <- rep_html()
  for (id in c("curva", "available-until", "pesos", "khat", "baselines", "proveniencia"))
    expect_true(grepl(sprintf("<section id=\"%s\">", id), h, fixed = TRUE), info = id)
  expect_true(grepl("available_until (global): 2026-01-15T09:00", h, fixed = TRUE))
  expect_true(grepl("<td>A1</td><td>2026-01-15T09:00</td>", h, fixed = TRUE))
  expect_true(grepl("<td>SS1</td><td>2026-01-15T08:30</td>", h, fixed = TRUE))
  expect_true(grepl("<td>SS1</td><td>v_0123456789abcdef</td><td>2026-01-14</td><td>1</td>", h, fixed = TRUE))
  expect_true(grepl("<svg ", h, fixed = TRUE))
  for (tok in c("plotly", "cdn", "http", "<script")) expect_false(grepl(tok, h, ignore.case = TRUE), info = tok)
  expect_true(grepl("charset=\"utf-8\"", h, fixed = TRUE))
  expect_false(grepl("\r", h, fixed = TRUE))
  out <- withr::local_tempdir("rep-")
  p <- env$prevcarga_report(mk_long(), mk_weights(), mk_khat(), mk_prov(), mk_base(mk_long()), out = out)
  raw <- readBin(p, "raw", file.size(p))
  expect_identical(Encoding(rawToChar(raw)), "unknown")
  expect_true(validUTF8(rawToChar(raw)))
  expect_identical(basename(p), "report.html")
  expect_pc <- function(expr) expect_error(expr, class = "prevcarga_error")
  expect_pc(env$prevcarga_report(mk_long()[, 1:2], mk_weights(), mk_khat(), mk_prov(), mk_base(mk_long()), out = out))
})

test_that("[AC2] CONTROL: peso ou available_until perturbado muda o HTML; insumo inalterado e byte-identico (sem relogio)", {
  h0 <- rep_html()
  expect_identical(sha(h0), sha(rep_html()))
  hw <- rep_html(weights = mk_weights(0.61))
  expect_false(identical(h0, hw))
  expect_true(grepl("0.6100", hw, fixed = TRUE)); expect_false(grepl("0.6100", h0, fixed = TRUE))
  ha <- rep_html(prov = mk_prov(au = "2026-01-15T09:30"))
  expect_false(identical(h0, ha))
  expect_true(grepl("2026-01-15T09:30", ha, fixed = TRUE)); expect_false(grepl("2026-01-15T09:30", h0, fixed = TRUE))
  ## khat perturbado (o bloco copia, nao recalcula)
  k <- mk_khat(); k[[1]]$khat_age_days <- 3L
  expect_true(grepl("<td>2026-01-14</td><td>3</td>", rep_html(khat = k), fixed = TRUE))
})

test_that("[AC3] FORBIDDEN: y_true arbitrario no painel e carga depois do corte => report byte-identical; CONTROL antes do corte muda", {
  h0 <- rep_html()
  expect_identical(h0, rep_html(long = mk_long(y_true = 1e6)))
  expect_identical(h0, rep_html(long = mk_long(y_true = seq_len(48))))
  ## grep: o relatorio nunca le a verdade nem emite metrica de acerto (CONTROL: o proprio teste enxerga o token)
  ## a secao do relatorio do backtest (M7.4, `.rpt_bt_*`) fica depois do marcador e e coberta por test-backtest-report.R (alcance a partir de prevcarga_report)
  s <- paste(readLines(file.path(pc_root(), "scripts", "lib", "reporting.R"), warn = FALSE), collapse = "\n")
  s <- substr(s, 1L, regexpr("## --- Relatorio do backtest (M7.4)", s, fixed = TRUE) - 1L)
  expect_true(nchar(s) > 5000L)
  for (tok in c("y_true", "wmape", "mape", "skill", "plotly", "http")) expect_false(grepl(tok, s, ignore.case = TRUE), info = tok)
  expect_true(grepl("y_true", paste(readLines(file.path(testthat::test_path(), "test-reporting.R"), warn = FALSE), collapse = "\n"), fixed = TRUE))
  f <- paste(readLines(file.path(pc_root(), "scripts", "lib", "forecast.R"), warn = FALSE), collapse = "\n")
  step <- substr(f, regexpr(".forecast_weights_table <-", f, fixed = TRUE), regexpr("## --- API publica", f, fixed = TRUE))
  expect_true(nchar(step) > 200L)
  for (tok in c("y_true", "wmape", "mape", "skill", "read_actual")) expect_false(grepl(tok, step, ignore.case = TRUE), info = tok)
})

test_that("[AC4] texto hostil (area e proveniencia) e escapado; nenhuma tag script", {
  evil <- "<script>alert(1)</script> & \"aspas\""
  esc <- "&lt;script&gt;alert(1)&lt;/script&gt; &amp; &quot;aspas&quot;"
  expect_identical(env$prevcarga_html_escape(evil), esc)
  expect_identical(env$prevcarga_html_escape("it's"), "it&#39;s")
  long <- mk_long(c("SIN", "SS1", evil))
  w <- mk_weights(); w$area <- evil
  b <- mk_base(long, area = evil)
  prov <- mk_prov(extra = list(caminho = evil), area_name = evil)
  h <- rep_html(long = long, weights = w, prov = prov, base = b)
  expect_true(grepl(esc, h, fixed = TRUE))
  expect_false(grepl("<script", h, fixed = TRUE))
  expect_false(grepl(evil, h, fixed = TRUE))
  k <- mk_khat(); k[[1]]$ss <- evil
  expect_false(grepl("<script", rep_html(khat = k), fixed = TRUE))
})

test_that("primitivas: heatmap por faixa em R, svg inline e pagina sem script", {
  dt <- data.frame(r = c("a", "a", "b", "b"), c = c("x", "y", "x", "y"), v = c(0, 1, 0.5, NA))
  h <- env$prevcarga_html_heatmap(dt, "r", "c", "v", "%.2f")
  expect_true(grepl("<td style=\"background:#ffffff\">0.00</td>", h, fixed = TRUE))
  expect_true(grepl(">-</td>", h, fixed = TRUE))
  expect_identical(h, env$prevcarga_html_heatmap(dt, "r", "c", "v", "%.2f"))
  s <- env$prevcarga_svg_profile(1:4, c(1, 2, NA, 4), c("a", "b", "c", "d"))
  expect_true(startsWith(s, "<svg ")); expect_true(grepl("polyline", s, fixed = TRUE))
  expect_true(grepl("sem dados", env$prevcarga_svg_profile(1:2, c(NA_real_, NA_real_))))
  pg <- env$prevcarga_html_page("t <x>", list(a = "<p>ok</p>"))
  expect_true(grepl("<title>t &lt;x&gt;</title>", pg, fixed = TRUE)); expect_false(grepl("<script", pg, fixed = TRUE))
})

## --- rodada de forecast com report ----------------------------------------------------------------------------
O <- M6F_ORIGIN
base <- withr::local_tempdir("rp-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)
fx_copy <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("rp-state-", .local_envir = .frame)
  file.copy(list.files(FX$state_dir, full.names = TRUE), st, recursive = TRUE)
  m6f_ctx(DECK, st, TOPO)
}
spied <- function(.frame = parent.frame()) {
  log <- new.env(); log$infer <- list(); log$train <- list()
  for (n in c("prevcarga_infer_cell", "prevcarga_train")) local({
    nm <- n; old <- env[[nm]]; withr::defer(assign(nm, old, envir = env), envir = .frame)
  })
  assign("prevcarga_infer_cell", m6f_infer_spy(env, log), envir = env)
  assign("prevcarga_train", m6f_train_spy(env, log), envir = env)
}
tree <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(x = d, x), algo = "sha256"), ""), f)
}

test_that("[AC5] forecast report = TRUE: caminho em reports/ fora de forecasts/, entrega identica com e sem report; blocos do reconcile", {
  spied()
  c0 <- fx_copy(); c1 <- fx_copy()
  r0 <- env$prevcarga_forecast(c0, skip_validate = TRUE)
  r1 <- env$prevcarga_forecast(c1, skip_validate = TRUE, report = TRUE)
  expect_identical(r0$result$report, list(status = "skipped", path = NULL))
  rp <- r1$result$report
  expect_identical(rp$status, "ok")
  want <- file.path(c1$state_dir, "reports", "2026-01-15", "20260115T0900", r1$result$versao, "report.html")
  expect_identical(rp$path, want)
  expect_true(file.exists(want))
  expect_false(startsWith(rp$path, file.path(c1$state_dir, "forecasts")))
  expect_identical(tree(file.path(c0$state_dir, "forecasts")), tree(file.path(c1$state_dir, "forecasts")))
  expect_identical(r0$result$versao, r1$result$versao)
  h <- rawToChar(readBin(want, "raw", file.size(want)))
  for (k in r1$result$khat)
    expect_true(grepl(sprintf("<td>%s</td><td>%s</td><td>%s</td><td>%d</td>", k$ss, k$versao, k$khat_origin, k$khat_age_days), h, fixed = TRUE))
  for (a in c("SIN", "SS1", "SS2", "A1")) expect_true(grepl(sprintf("<h3>[A-Za-z]+: %s</h3>", a), h), info = a)
  expect_true(grepl("available_until (global): 2026-01-15T09:00", h, fixed = TRUE))
  expect_true(grepl("persistencia O-1", h, fixed = TRUE))
  ## rodada repetida (noop) => mesmo relatorio byte a byte
  r2 <- env$prevcarga_forecast(c1, skip_validate = TRUE, report = TRUE)
  expect_identical(digest::digest(file = r2$result$report$path, algo = "sha256"), digest::digest(file = want, algo = "sha256"))
  ## a entrega nunca depende do report
  expect_false(any(grepl("^reports/", names(tree(file.path(c1$state_dir, "forecasts"))))))
})

test_that("[AC3] FORBIDDEN: baselines so leem carga <= O 00:00 (deck com o dia O inteiro == deck cortado); CONTROL: carga anterior muda", {
  long <- mk_long(c("A1", "SS1"))
  rctx <- function(deck) { ctx <- m6f_ctx(deck, file.path(base, "st-x"), TOPO); ctx$tz <- TZ; ctx$load_label <- "end"; ctx }
  deck_full <- m6f_deck(env, file.path(base, "deck-full"), until = "24:00")
  deck_other <- m6f_deck(env, file.path(base, "deck-seed"), seed = 12)
  b0 <- env$.forecast_baselines(rctx(DECK), long, O)
  b1 <- env$.forecast_baselines(rctx(deck_full), long, O)
  expect_true(nrow(b0$persistence_o1) > 0L && nrow(b0$seasonal_o7) > 0L)
  expect_identical(b0, b1)
  b2 <- env$.forecast_baselines(rctx(deck_other), long, O)
  expect_false(identical(b0, b2))
})
