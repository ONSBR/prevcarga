## test-exogena-oper.R -- agregacao diaria da exogena de temperatura (`aggregaSerie` com exogenaOper MAX e MXMN, `getFuncTemp`, `mxmn`) do motor
## DESSEM unico contra o ORACULO (DL.10). O fan-out (test-fanout.R) exerceu MAX e MXMN contra o oraculo pela primeira vez: SECO x 5 origens e
## RS/CE/PA fecharam em <= 1e-5 sem divergencia isolavel na agregacao (a de hora parcial no clima previsto e a divergencia deliberada ja nomeada
## no DL.9), logo NENHUMA correcao foi feita em models/_dessem-engine/R/funcoes_apoio.R: estes testes FIXAM o comportamento ja equivalente. O
## oraculo so e lido (as 3 funcoes sao lidas do vendor do oraculo e avaliadas num env proprio, nunca executadas la). Sem golden, sem ambiente.
suppressWarnings(suppressMessages(library(testthat)))

DLE_ORACLE_SRC <- "/opt/ons-source/prevcargadessem3/models/dessem-linear/vendor/pkgfun_funcoesApoio.R"

## funcoes do ORACULO (aggregaSerie, dividepor, mxmn, getFuncTemp, rangelst) num env proprio com data.table
dle_oracle_env <- local({
  e <- NULL
  function() {
    if (!is.null(e)) return(e)
    expect_true(file.exists(DLE_ORACLE_SRC), info = "oraculo ausente: nada a comparar")
    e <<- new.env(parent = globalenv())
    for (x in parse(DLE_ORACLE_SRC, keep.source = FALSE))
      if (is.call(x) && identical(as.character(x[[1L]]), "<-") &&
          as.character(x[[2L]])[[1L]] %in% c("aggregaSerie", "dividepor", "mxmn", "getFuncTemp", "rangelst")) eval(x, e)
    e
  }
})

## serie horaria: 3 dias, o dia 1 com EXTREMO EMPATADO (maximo e minimo repetidos em varias horas), o dia 2 constante (max == min) e o dia 3 comum
dle_fx <- function() {
  d <- as.Date("2024-10-01") + 0:2
  h <- rep(0:23, 3L)
  t1 <- c(20, 20, 19, 19, 19, 20, 22, 25, 28, 30, 31, 33, 33, 33, 32, 31, 30, 28, 26, 24, 22, 21, 20, 19)
  t2 <- rep(25, 24L)
  t3 <- 22 + 6 * sin(2 * pi * (0:23 - 9) / 24)
  data.table::data.table(Data = rep(d, each = 24L), DataHora = lubridate::force_tz(lubridate::as_datetime(rep(d, each = 24L)), "UTC") +
                           lubridate::hours(h), Carga = 9000 + 100 * h + 50 * rep(0:2, each = 24L), Exo_Temperatura = c(t1, t2, t3))
}
dle_agg <- function(env, tipo, fx = dle_fx()) env$aggregaSerie(serieHist = data.table::copy(fx), tipotemp = tipo, normaliza = TRUE, constnorm = 10000, constnormtemp = 30)
dle_num <- function(x) as.matrix(as.data.frame(x)[, vapply(as.data.frame(x), is.numeric, NA), drop = FALSE])

test_that("[PLUGINS] exogena_oper: aggregaSerie MAX e MXMN do motor reproduzem o oraculo (<= 1e-5), inclusive no dia de extremo empatado e no dia constante", {
  eng <- dl_engine_env(); orc <- dle_oracle_env()
  for (tipo in c("MAX", "MXMN", "MED", "SEM")) {
    got <- dle_agg(eng, tipo); ref <- dle_agg(orc, tipo)
    expect_identical(names(got), names(ref), info = tipo)
    expect_identical(dim(got), dim(ref), info = tipo)
    expect_lte(max(abs(dle_num(got) - dle_num(ref))), 1e-5, label = paste("max|D|", tipo))
  }
  ## valores a mao (normalizados: Exo / 30, Carga / 10000): dia 1 max 33, min 19; dia 2 constante 25; MXMN = (MAX, MIN) por dia
  mx <- dle_agg(eng, "MAX")
  expect_equal(mx$Exo_Temperatura, c(33, 25, 28) / 30, tolerance = 1e-12)
  mm <- dle_agg(eng, "MXMN")
  cols <- grep("^Exo_", names(mm), value = TRUE)
  expect_length(cols, 2L)                                                     # uma coluna MAX e uma MIN por exogena
  expect_equal(mm[[grep("MAX", cols, value = TRUE)]], c(33, 25, 28) / 30, tolerance = 1e-12)
  expect_equal(mm[[grep("MIN", cols, value = TRUE)]], c(19, 25, 16) / 30, tolerance = 1e-12)
  expect_equal(mm$Carga, mx$Carga, tolerance = 1e-12)                          # a carga e a media do dia em qualquer variante
  ## codigos numericos do deck (0 SEM, 1 MAX, 2 MED, 3 MXMN) e getFuncTemp
  expect_equal(dle_num(dle_agg(eng, 1L)), dle_num(mx)); expect_equal(dle_num(dle_agg(eng, 3L)), dle_num(mm))
  fx <- dle_fx()$Exo_Temperatura[1:24]
  expect_identical(eng$getFuncTemp("MAX")(fx), 33); expect_identical(eng$getFuncTemp("MED")(fx), mean(fx))
  expect_identical(unlist(eng$getFuncTemp("MXMN")(fx)), unlist(orc$getFuncTemp("MXMN")(fx)))
  expect_error(eng$aggregaSerie(dle_fx(), "XXX", TRUE, 1, 1), "fora dos padroes")
})

test_that("[PLUGINS] exogena_oper: CONTROL -- uma agregacao regredida (MAX trocado por media, MXMN sem o minimo) diverge do oraculo > 1e-5", {
  eng <- dl_engine_env(); orc <- dle_oracle_env()
  ## versao "pre-correcao" fabricada: o ramo MAX usa a media em vez do maximo (o erro que o fan-out de MAX detectaria)
  bad <- orc$aggregaSerie
  body(bad) <- parse(text = sub("lapply(.SD, max)", "lapply(.SD, mean)", paste(deparse(body(bad)), collapse = "\n"), fixed = TRUE), keep.source = FALSE)[[1L]]
  env <- list2env(list(aggregaSerie = bad, dividepor = orc$dividepor, mxmn = orc$mxmn), parent = globalenv())
  expect_gt(max(abs(dle_num(dle_agg(env, "MAX")) - dle_num(dle_agg(eng, "MAX")))), 1e-5)
  ## MXMN regredido: mxmn devolve so o maximo => uma coluna a menos e valores distintos
  bad2 <- list2env(list(dividepor = orc$dividepor, mxmn = function(x) list(MAX = max(x), MIN = max(x))), parent = globalenv())
  bad2$aggregaSerie <- orc$aggregaSerie; environment(bad2$aggregaSerie) <- bad2             # a agregacao enxerga o mxmn regredido
  expect_gt(max(abs(dle_num(dle_agg(bad2, "MXMN")) - dle_num(dle_agg(eng, "MXMN")))), 1e-5)
  ## sanidade do CONTROL: o motor real segue igual ao oraculo na mesma entrada
  expect_lte(max(abs(dle_num(dle_agg(orc, "MAX")) - dle_num(dle_agg(eng, "MAX")))), 1e-5)
})
