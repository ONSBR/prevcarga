## test-accept-feriado.R -- classificacao de dias do pos-processamento accept_feriado.R (E8-T4, item 11). Unidade pura, sem docker,
## sem store real: so pc_fer_class (feriado / vizinho / comum) e a leitura do fallback por stderr.log sintetico.

.fer_env <- new.env()
sys.source(file.path(pc_root(), "scripts", "tools", "accept_feriado.R"), envir = .fer_env)

test_that("classe do dia: feriado, vizinho (+-1) e comum, por area", {
  hol <- data.table::data.table(area = c("RJ", "RJ", "SP"), date = as.Date(c("2023-06-08", "2023-06-09", "2023-06-20")))
  day <- as.Date(c("2023-06-07", "2023-06-08", "2023-06-09", "2023-06-10", "2023-06-12", "2023-06-07", "2023-06-19", "2023-06-20"))
  area <- c(rep("RJ", 5), "SP", "SP", "SP")
  got <- .fer_env$pc_fer_class(area, day, hol)
  ## RJ: 06-07 vizinho (antes de 06-08), 06-08 e 06-09 feriados (um feriado nao e vizinho do outro), 06-10 vizinho, 06-12 comum
  expect_identical(got, c("vizinho", "feriado", "feriado", "vizinho", "comum", "comum", "vizinho", "feriado"))
})

test_that("fallback: so as linhas warning.engine_tune_fallback do stderr.log, com a celula", {
  out <- tempfile("fer-"); dir.create(file.path(out, "units", "u1"), recursive = TRUE)
  dir.create(file.path(out, "units", "u2"), recursive = TRUE)
  un <- data.frame(unit = c("u1", "u2"), versao = c("new", "ref"), origem = "2023-06-07", area = c("CE", "CE"), mdir = "dessem-linear")
  utils::write.table(un, file.path(out, "units.medicao.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  writeLines(c("[engine.dessem] warning.engine_tune_fallback: tune sem solucao (x); treino sem tune cost=1 epsilon=0.1 (amostra=4) celula: data=2023-06-08 hora=5 tipo_dia_especial=1,2",
               "[engine.dessem] warning.engine_cross_clamped: cross clampado 10->3 (amostra=4)"), file.path(out, "units", "u1", "stderr.log"))
  writeLines("backtest: 1/1", file.path(out, "units", "u2", "stderr.log"))
  fb <- .fer_env$pc_fer_fallback(out)
  expect_identical(nrow(fb), 1L)
  expect_identical(fb$versao, "new"); expect_identical(fb$area, "CE")
  expect_match(fb$celula, "^data=2023-06-08 hora=5 ")
})
