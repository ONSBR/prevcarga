## test-report_pdf.R -- motor PDF portavel do report (M6.10, ADR-0033) + falha parcial exit 14 (partial.report_failed).
## grDevices::pdf (base R), sem dependencia de sistema; so tempdir e paths via file.path (roda em Linux e Windows nativo).
## Cada FORBIDDEN tem o seu CONTROL. Sem variaveis de ambiente.
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
mk_long <- function(areas = c("SIN", "SS1", "A1")) {
  do.call(rbind, lapply(seq_along(areas), function(i) data.frame(
    area = areas[[i]], forecast_origin = "2026-01-15", target_datetime = t0 + lubridate::minutes(30L * (1:48)),
    lead = 1:48, y_pred = 100 * i + 10 * sin((1:48) / 8), y_true = NA_real_, d_plus = 0L, granularity_min = 30L)))
}
mk_weights <- function(w = 0.6) data.frame(area = c("A1", "A1"), member = c("RF", "HW"), weight = c(w, 1 - w), intercept = c(0, 0))
mk_khat <- function(versao = "v_0123456789abcdef", origin = "2026-01-14")
  list(list(ss = "SS1", versao = versao, khat_origin = origin, khat_age_days = 1L),
       list(ss = "SS2", versao = "v_fedcba9876543210", khat_origin = "2026-01-13", khat_age_days = 2L))
mk_prov <- function(au = "2026-01-15T09:00") list(
  versao = "abc123abc123", origem = "2026-01-15", available_until = au,
  available_until_por_area = list(A1 = au, SS1 = "2026-01-15T08:30"), load_label = "end",
  timezone = TZ, weather_issue_cutoff = "available_until", subsistemas = "SS1")
mk_base <- function(long, area = "A1") {
  d <- long[long$area == area, ]
  list(persistence_o1 = data.frame(area = d$area, target_datetime = d$target_datetime, y_base = d$y_pred - 5),
       seasonal_o7 = data.frame(area = d$area, target_datetime = d$target_datetime, y_base = d$y_pred + 7))
}
pdf_path <- function(long = mk_long(), weights = mk_weights(), khat = mk_khat(), prov = mk_prov(), .frame = parent.frame()) {
  out <- withr::local_tempdir("pdf-", .local_envir = .frame)
  env$prevcarga_report(long, weights, khat, prov, mk_base(long), format = "pdf", out = out)
}
pdf_text <- function(p) iconv(rawToChar(readBin(p, "raw", file.size(p))), "latin1", "UTF-8")

test_that("[AC1] report.pdf existe, comeca com %PDF- e nao e HTML renomeado", {
  p <- pdf_path()
  expect_identical(basename(p), "report.pdf")
  expect_true(file.exists(p))
  expect_identical(rawToChar(readBin(p, "raw", 5L)), "%PDF-")
  expect_false(grepl("<html", pdf_text(p), fixed = TRUE))
})

test_that("[AC1] grep em reporting.R: nenhum motor de sistema; CONTROL: o proprio teste enxerga o token", {
  s <- paste(readLines(file.path(pc_root(), "scripts", "lib", "reporting.R"), warn = FALSE), collapse = "\n")
  bad <- c("system(", "system2(", "processx", "pagedown", "chromote", "rmarkdown", "pandoc", "latex", "python")
  for (tok in bad) expect_false(grepl(tok, tolower(s), fixed = TRUE), info = tok)
  expect_true(grepl("system(", "x <- system('ls')", fixed = TRUE))
  expect_true(grepl("grDevices::pdf", s, fixed = TRUE))
})

test_that("[AC2] seis titulos, versao v_<hash> e khat_origin de cada subsistema legiveis nos bytes; CONTROL: trocar a versao muda", {
  txt <- pdf_text(pdf_path())
  for (t in c("Curva do dia por nivel", "available_until", "Pesos dos membros", "k-hat do reconcile",
              "Diferenca vs persistencia O-1 e sazonal O-7", "Proveniencia"))
    expect_true(grepl(t, txt, fixed = TRUE), info = t)
  for (tok in c("v_0123456789abcdef", "2026-01-14", "v_fedcba9876543210", "2026-01-13", "available_until \\(global\\): 2026-01-15T09:00"))
    expect_true(grepl(tok, txt, fixed = TRUE), info = tok)
  ## CONTROL
  txt2 <- pdf_text(pdf_path(khat = mk_khat(versao = "v_aaaaaaaaaaaaaaaa", origin = "2026-01-10")))
  expect_true(grepl("v_aaaaaaaaaaaaaaaa", txt2, fixed = TRUE)); expect_false(grepl("v_0123456789abcdef", txt2, fixed = TRUE))
  expect_true(grepl("2026-01-10", txt2, fixed = TRUE))
  txt3 <- pdf_text(pdf_path(weights = mk_weights(0.61)))
  expect_true(grepl("0.6100", txt3, fixed = TRUE)); expect_false(grepl("0.6100", txt, fixed = TRUE))
})

test_that("texto nao ASCII nao quebra o PDF (vira ?) e tabelas longas paginam", {
  p <- pdf_path(long = mk_long(c("SIN", "SS1", "Ação")), weights = data.frame(area = "Ação", member = "RF", weight = 1, intercept = 0))
  expect_identical(rawToChar(readBin(p, "raw", 5L)), "%PDF-")
  expect_true(grepl("A??o", pdf_text(p), fixed = TRUE))
  areas <- sprintf("L%03d", 1:80)
  p2 <- pdf_path(prov = utils::modifyList(mk_prov(), list(available_until_por_area = stats::setNames(as.list(rep("2026-01-15T09:00", 80)), areas))))
  expect_true(grepl("L080", pdf_text(p2), fixed = TRUE))
})

## --- rodada de forecast com report PDF e falha parcial -------------------------------------------------------------
O <- M6F_ORIGIN
base <- withr::local_tempdir("rpdf-base-")
DECK <- m6f_deck(env, file.path(base, "deck"))
TOPO <- m6f_topology_file(base)
FX <- m6f_ctx(DECK, file.path(base, "state"), TOPO)
dir.create(FX$state_dir, recursive = TRUE)
m6f_prepare_origin(env, FX)
fx_copy <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("rpdf-state-", .local_envir = .frame)
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
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), ""), f)
}

test_that("[AC1][AC4] forecast --report pdf grava reports/<origem>/<available_until>/<versao>/report.pdf; html sem regressao", {
  spied()
  c1 <- fx_copy(); c2 <- fx_copy()
  r1 <- env$prevcarga_forecast(c1, skip_validate = TRUE, report = TRUE, report_format = "pdf")
  want <- file.path(c1$state_dir, "reports", "2026-01-15", "20260115T0900", r1$result$versao, "report.pdf")
  expect_identical(r1$result$report, list(status = "ok", path = want))
  expect_identical(rawToChar(readBin(want, "raw", 5L)), "%PDF-")
  for (k in r1$result$khat) expect_true(grepl(k$versao, pdf_text(want), fixed = TRUE))
  r2 <- env$prevcarga_forecast(c2, skip_validate = TRUE, report = TRUE, report_format = "html")
  expect_identical(r2$result$report$status, "ok")
  expect_identical(basename(r2$result$report$path), "report.html")
  expect_true(file.exists(r2$result$report$path))
  expect_identical(tree(file.path(c1$state_dir, "forecasts")), tree(file.path(c2$state_dir, "forecasts")))
})

test_that("[AC3] falha injetada no adaptador PDF => exit 14 partial.report_failed, entrega identica a uma rodada sem report", {
  spied()
  c0 <- fx_copy(); c1 <- fx_copy()
  r0 <- env$prevcarga_forecast(c0, skip_validate = TRUE)
  old <- env$.report_pdf
  withr::defer(assign(".report_pdf", old, envir = env))
  assign(".report_pdf", function(...) stop("falha injetada"), envir = env)
  cond <- tryCatch(env$prevcarga_forecast(c1, skip_validate = TRUE, report = TRUE, report_format = "pdf"),
                   prevcarga_error = identity)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "partial.report_failed")
  expect_identical(cond$exit_code, 14L)
  expect_identical(cond$result$report, list(status = "failed", path = NULL))
  expect_identical(cond$result$versao, r0$result$versao)
  expect_true(grepl("falha injetada", cond$details$cause, fixed = TRUE))
  expect_identical(tree(file.path(c0$state_dir, "forecasts")), tree(file.path(c1$state_dir, "forecasts")))
  expect_false(file.exists(file.path(c1$state_dir, "reports", "2026-01-15", "20260115T0900", r0$result$versao, "report.pdf")))
  ## CONTROL: sem a falha injetada a mesma rodada nao aborta
  assign(".report_pdf", old, envir = env)
  r <- env$prevcarga_forecast(fx_copy(), skip_validate = TRUE, report = TRUE, report_format = "pdf")
  expect_identical(r$result$report$status, "ok")
})

test_that("catalogo: partial.report_failed declarado em errors/report.tsv, classe partial, ASCII", {
  tsv <- file.path(pc_root(), "scripts", "lib", "errors", "report.tsv")
  expect_true(file.exists(tsv))
  raw <- readBin(tsv, "raw", file.size(tsv))
  expect_true(all(as.integer(raw) < 128L))
  expect_false(any(raw == as.raw(13L)))
  row <- env$prevcarga_error_lookup("partial.report_failed")
  expect_identical(row$class, "partial")
})

test_that("[determinismo] mesmo insumo => PDF byte-identico (sem relogio); CONTROL: dado diferente muda os bytes", {
  sha <- function(p) digest::digest(file = p, algo = "sha256")
  a <- pdf_path(); Sys.sleep(1.1); b <- pdf_path()
  expect_identical(sha(a), sha(b))
  expect_false(grepl("CreationDate|ModDate", pdf_text(a)))
  expect_false(identical(sha(a), sha(pdf_path(weights = mk_weights(0.61)))))
  expect_false(identical(sha(a), sha(pdf_path(khat = mk_khat(versao = "v_aaaaaaaaaaaaaaaa")))))
})

test_that("[AC3] exit 14 carrega os warnings da rodada em result$warnings (list(id, message))", {
  spied()
  old_s <- env$.forecast_warn_sink; old <- env$.report_pdf
  withr::defer({ assign(".forecast_warn_sink", old_s, envir = env); assign(".report_pdf", old, envir = env) })
  assign(".forecast_warn_sink", function() {
    s <- old_s()
    s$add("warning.teste_injetado", "aviso injetado")
    s
  }, envir = env)
  assign(".report_pdf", function(...) stop("falha injetada"), envir = env)
  cond <- tryCatch(env$prevcarga_forecast(fx_copy(), skip_validate = TRUE, report = TRUE, report_format = "pdf"),
                   prevcarga_error = identity)
  expect_identical(cond$id, "partial.report_failed")
  ids <- vapply(cond$result$warnings, function(x) x$id, "")
  expect_true("warning.teste_injetado" %in% ids)
  expect_true(all(vapply(cond$result$warnings, function(x) all(c("id", "message") %in% names(x)), NA)))
  ## CONTROL: rodada sem report devolve os mesmos warnings em $warnings
  assign(".report_pdf", old, envir = env)
  r <- env$prevcarga_forecast(fx_copy(), skip_validate = TRUE)
  expect_true("warning.teste_injetado" %in% vapply(r$warnings, function(x) x$id, ""))
})
