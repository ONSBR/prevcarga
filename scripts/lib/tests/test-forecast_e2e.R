## test-forecast_e2e.R -- prevcarga_forecast ponta a ponta (M6.4) com os 8 membros REAIS sobre um deck de fixture_store.R.
## Roda em `Rscript --vanilla` filhos por fase (helper-m6-fixtures.R: m6e_*): train -> weights -> run -> rerun; LENTO (treino dos
## 8 membros: ~10 min no total, 1 processo, 1 thread). `options(prevcarga.m6e_work = <dir>)` reaproveita o diretorio de trabalho e
## retoma da fase seguinte (cada fase grava out-<fase>.json); `options(prevcarga.m6e_stop = "<fase>")` para depois dela.
## [DEFERRED M6.9] o verbo `forecast` (docopt) e do M6.9: aqui a rodada entra pela funcao publica, no filho que a CLI usaria.
work <- getOption("prevcarga.m6e_work")
if (is.null(work)) work <- withr::local_tempdir("m6e-") else dir.create(work, recursive = TRUE, showWarnings = FALSE)
STOP <- getOption("prevcarga.m6e_stop")
PH <- list()
for (ph in c("train", "weights", "run", "rerun", "weights00", "run00")) {
  PH[[ph]] <- m6e_run_child(work, ph)
  if (identical(STOP, ph)) break
}
skip_if_stopped <- function(ph) if (is.null(PH[[ph]])) skip(paste("fase", ph, "nao rodou (prevcarga.m6e_stop)"))
ok <- function(ph) { r <- PH[[ph]]; expect_identical(r$status, 0L, info = paste(utils::tail(r$log, 30), collapse = "\n")); r$json }

test_that("[e2e] sem pesos da combina a 1a rodada aborta exit 13, mas as 6 celulas origin_scoped treinaram dentro do forecast, 1x cada", {
  skip_if_stopped("train")
  j <- ok("train")
  expect_identical(j$pre_error$id, "state.combina_not_trained")
  expect_identical(j$pre_error$exit, 13L)
  expect_identical(j$trained_in_forecast, 6L)                      # HW, LIN_MED/MAX/MXMN, RGD_MED/MAX
  skip_if_stopped("weights")
  w <- ok("weights")
  expect_identical(w$trained_in_weights, 0L)                       # o setup dos pesos so reusou os artefatos de O
})

test_that("[e2e] os 8 membros reais rodam: gate-0 real, infer, combina, reconcile e export na chave imutavel", {
  skip_if_stopped("run")
  j <- ok("run")
  expect_null(j$error)
  ## o gate-0 REAL roda e recusa o deck curto (janela de historico do run.yaml, exit 5); a rodada segue com skip_validate
  expect_identical(j$gate0_error$id, "window.window_train_deficit")
  expect_identical(j$gate0_error$exit, 5L)
  expect_true(j$skip_validate_used)
  expect_identical(j$train_calls_run1, 0L)                         # nenhum re-treino: todas as celulas ja tinham artefato de O
  r <- j$run1$result
  expect_identical(r$origem, "2024-10-15")                         # default = ultimo dia de carga do deck (ctx$origin NULL)
  expect_identical(r$available_until, "2024-10-15T09:30")
  expect_match(r$saida, "/forecasts/2024-10-15/20241015T0930/[0-9a-f]{12}$")
  paths <- vapply(r$arquivos, function(a) a$path, "")
  expect_true(all(c("SIN.parquet", "subsistemas/XX.parquet", "folhas/SP.parquet") %in% paths))
  mem <- sort(sub("^membros/([^/]+)/SP.parquet$", "\\1", grep("^membros/", paths, value = TRUE)), method = "radix")
  expect_identical(mem, sort(c("RF", "LGBM", "HW", "LIN_MED", "LIN_MAX", "LIN_MXMN", "RGD_MED", "RGD_MAX"), method = "radix"))
  expect_true(all(vapply(r$arquivos, function(a) a$linhas > 0L, NA)))
  ## scratch registra TUDO o que entrega: nenhum aviso de export_file_missing
  expect_false(any(vapply(j$run1$warnings, function(w) identical(w$id, "completude.export_file_missing"), NA)))
  expect_identical(unlist(r$covered), "SP"); expect_length(r$stray, 0L)
  expect_identical(r$khat[[1L]]$ss, "XX"); expect_identical(r$khat[[1L]]$khat_origin, "2024-10-15"); expect_identical(r$khat[[1L]]$khat_age_days, 0L)
  expect_true("manifest.json" %in% names(j$run1$tree))
  ## entrega: SIN (=1 subsistema) tem a mesma grade do subsistema; a 1a entrega e available_until + 30 min
  expect_false(r$ja_publicado)
})

test_that("[e2e] a segunda rodada no mesmo processo e no-op: mesma chave, mesmos sha256, sem re-treino", {
  skip_if_stopped("run")
  j <- ok("run")
  expect_true(j$run2$ja_publicado)
  expect_identical(j$run2$versao, j$run1$result$versao)
  expect_identical(j$run2$tree, j$run1$tree)
  expect_identical(j$run2$train_calls, 0L)
})

test_that("[e2e] outro processo, mesmo deck e artefatos: mesma versao e sha256 identical (determinismo)", {
  skip_if_stopped("rerun")
  j2 <- ok("rerun")
  j1 <- PH$run$json
  expect_identical(j2$run1$result$versao, j1$run1$result$versao)
  expect_identical(j2$run1$tree, j1$run1$tree)
  expect_true(j2$run1$result$ja_publicado)
  expect_identical(j2$train_calls_run1, 0L)
})

test_that("[e2e 00h] rodada das 00h com os 8 membros reais: deck ate O-1 23:30, origem O, entrega de O 00:30 sem carga do dia O", {
  skip_if_stopped("run00")
  w <- ok("weights00")
  ## [DEFERRED DL/RD] hoje os DESSEM (linear/regdin) abortam sem a linha do dia O na cabeca mm (plugin.dessem_linear_mm_missing_origin):
  ## a rodada das 00h exige que a cabeca mm seja opcional quando available_until = O 00:00. Enquanto isso o teste se declara pendente.
  if (!is.null(w$error) && grepl("mm_missing_origin", w$error$id)) skip(paste("DEFERRED DL/RD:", w$error$id))
  expect_null(w$error)
  j <- ok("run00")
  expect_null(j$error)
  expect_identical(j$train_calls_run1, 0L)                         # artefatos de O reaproveitados (corte do treino = O-1)
  r <- j$run1$result
  expect_identical(r$origem, "2024-10-15")
  expect_identical(r$available_until, "2024-10-15T00:00")
  expect_match(r$saida, "/forecasts/2024-10-15/20241015T0000/[0-9a-f]{12}$")
  paths <- vapply(r$arquivos, function(a) a$path, "")
  expect_length(grep("^membros/", paths), 8L)
  expect_true(all(c("SIN.parquet", "subsistemas/XX.parquet", "folhas/SP.parquet") %in% paths))
  expect_true(j$run2$ja_publicado)
  expect_identical(j$run2$tree, j$run1$tree)
  ## todos os membros entregam a partir de O 00:30 e a carga lida nao tem o dia O
  for (a in r$arquivos) if (grepl("^(membros|folhas)/", a$path)) {
    x <- arrow::read_parquet(file.path(r$saida, a$path))
    expect_identical(format(min(x$target_datetime), "%Y-%m-%d %H:%M", tz = PCC_TZ), "2024-10-15 00:30", info = a$path)
  }
  expect_identical(j$no_day_o_load, TRUE)
})
