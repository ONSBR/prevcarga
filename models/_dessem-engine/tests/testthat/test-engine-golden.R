## test-engine-golden.R -- DL.1: equivalencia numerica com o motor do oraculo (AC11, AC12) e formula sem `mm` (AC9).
## O oraculo (/opt/ons-source/prevcargadessem3, so leitura) roda em processo filho; sem ele ou sem R 4.4.3 o golden e pulado.

ORACLE_VENDOR <- "/opt/ons-source/prevcargadessem3/models/dessem-linear/vendor"
ORACLE_RSCRIPT <- "/opt/R/4.4.3/bin/Rscript"
MODEL_KEY <- "Linear.TmpMedia"
ORIGIN <- "2019-02-28"

skip_if_no_oracle <- function() {
  skip_if_not(file.exists(file.path(ORACLE_VENDOR, "_load_engine.R")) && file.exists(ORACLE_RSCRIPT),
              "oraculo (prevcargadessem3 + R 4.4.3) ausente")
}

run_oracle <- function(seed, deck) {
  dk <- tempfile(fileext = ".rds"); out <- tempfile(fileext = ".rds")
  saveRDS(deck, dk)
  child <- file.path(testthat::test_path(), "fixtures", "oracle_child.R")
  args <- c(ORACLE_VENDOR, dk, out, seed, ORIGIN, MODEL_KEY)
  expr <- sprintf(".oracle_args <- %s; source(%s)", paste(deparse(args), collapse = ""),
                  deparse(normalizePath(child, winslash = "/")))
  rc <- suppressWarnings(system2(ORACLE_RSCRIPT, c("--vanilla", "-e", shQuote(expr)), stdout = FALSE, stderr = FALSE))
  expect_identical(rc, 0L)
  readRDS(out)
}

## motor clean-room (env do loader) sobre o mesmo deck
run_clean <- function(seed, deck, eng, offset_min = 60L) {
  dk <- suppressMessages(eng$TransformaDeck(eng_model_deck(deck, offset_min), ORIGIN, "10DIAS", tz = "Etc/GMT+3"))
  cfg <- eng$getConfigModel(MODEL_KEY)
  if (!identical(seed, "none")) set.seed(as.integer(seed))
  hor <- suppressMessages(eng$processaModelosDMPerfil(dk, cfg))
  list(seriecomp = dk$SERIECOMP, horaria = hor, diasprev = dk$DIASPREV,
       semi = eng$desagregaModeloSH("M", data.table::copy(hor)[, Modelo := "M"]), eng = eng, dk = dk, cfg = cfg)
}

maxdiff <- function(a, b) max(abs(a$Carga - b$Carga))

test_that("ac-11/12 processaModelosDMPerfil e desagregaModeloSH == oraculo (<= 1e-5 MW) no deck fixo", {
  skip_if_no_oracle()
  deck <- eng_deck()
  eng <- eng_env(eng_load(eng_plugin_root()))
  ref <- run_oracle("42", deck)
  mine <- run_clean("42", deck, eng)
  expect_equal(mine$diasprev, ref$diasprev)
  expect_identical(nrow(mine$horaria), length(ref$diasprev) * 24L)
  expect_equal(mine$seriecomp, ref$seriecomp, tolerance = 0)
  expect_identical(lubridate::tz(mine$horaria$DataHora), "Etc/GMT+3")
  ## o motor devolve o frame de modelagem; o despachante devolve ao frame de exibicao (+60 min) e o oraculo
  ## ja entrega a hora de inicio: mesmos instantes
  expect_identical(mine$horaria$DataHora, ref$horaria$DataHora)
  expect_true(all(is.finite(mine$horaria$Carga)))
  expect_lte(maxdiff(mine$horaria, ref$horaria), 1e-5)
  ## AC12: desagregacao semi-horaria
  expect_identical(nrow(mine$semi), 2L * nrow(mine$horaria))
  expect_identical(mine$semi$Passo, seq_len(2L * nrow(mine$horaria)))
  expect_identical(eng_semi_stamp(mine$semi), ref$semi$DataHora)
  expect_lte(maxdiff(mine$semi, ref$semi), 1e-5)
  ## CONTROL (AC12): splinefun fmm no lugar de monoH.FC afasta a saida
  fmm <- eval(parse(text = gsub("monoH.FC", "fmm", paste(deparse(eng$desagregaModeloSH), collapse = "\n"), fixed = TRUE)),
              envir = eng)
  alt <- fmm("M", data.table::copy(mine$horaria)[, Modelo := "M"])
  expect_gt(maxdiff(alt, ref$semi), 1e-5)
  ## CONTROL (AC2 DL.3): motor alimentado sem o to_model_frame do despachante (offset 0) completa a montagem da
  ## serie, mas a carga fica 1 h deslocada em relacao ao oraculo (series diferem muito alem de 1e-5)
  dk0 <- suppressMessages(eng$TransformaDeck(eng_model_deck(deck, 0L), ORIGIN, "10DIAS", tz = "Etc/GMT+3"))
  expect_s3_class(dk0$SERIECOMP, "data.table")
  a <- dk0$SERIECOMP[Prev == FALSE][order(DataHora)]
  b <- ref$seriecomp[Prev == FALSE][order(DataHora)]
  expect_false(identical(a$DataHora, b$DataHora) && isTRUE(all.equal(a$Carga, b$Carga, tolerance = 0)))
  m <- merge(a[, .(DataHora, Carga)], b[, .(DataHora, Carga)], by = "DataHora")
  expect_gt(max(abs(m$Carga.x - m$Carga.y), na.rm = TRUE), 1e-5)
})

test_that("ac-11 CONTROL: outra semente no clean-room afasta a saida do oraculo (> 1e-5 MW)", {
  skip_if_no_oracle()
  deck <- eng_deck()
  eng <- eng_env(eng_load(eng_plugin_root()))
  ref <- run_oracle("42", deck)
  other <- run_clean("7", deck, eng)
  expect_gt(maxdiff(other$horaria, ref$horaria), 1e-5)
})

test_that("ac-9 obterEntradaFormulaDM do motor e a do oraculo: formula sem o termo mm", {
  deck <- eng_deck()
  eng <- eng_env(eng_load(eng_plugin_root()))
  dk <- suppressMessages(eng$TransformaDeck(eng_model_deck(deck), ORIGIN, "10DIAS", tz = "Etc/GMT+3"))
  cfg <- eng$getConfigModel(MODEL_KEY)
  serieDM <- eng$aggregaSerie(serieHist = data.table::copy(dk$SERIECOMP), tipotemp = cfg$exogenaOper, normaliza = TRUE,
                              constnorm = eng$getNormalizaCarga(cfg$normalizacao$dm$carga, dk$SERIECOMP[Prev == FALSE]),
                              constnormtemp = eng$getNormalizaExogena(cfg$normalizacao$dm$exogena, dk$SERIECOMP[Prev == FALSE]))
  inp <- eng$obterEntradaFormulaDM(dk$DIASPREV[[1L]], serieDM, cfg)
  expect_false("mm" %in% all.vars(inp$formulaMDL))
  expect_false("mm" %in% names(inp$dadosEntrada$serieDiariaModelo))
  expect_true(all(c("Exo_Temperatura", "LAGEXO_Temperatura_1") %in% all.vars(inp$formulaMDL)) ||
              any(grepl("LAGEXO", all.vars(inp$formulaMDL))))
})
