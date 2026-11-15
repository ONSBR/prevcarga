## test-engine-ilhas.R -- E3-T3: coluna Bloco e lags por ilha no motor. Series horarias sinteticas (sem store).

ilh_eng <- function() {
  eng <- eng_env(eng_load(eng_plugin_root()))
  assign("engine_abort", function(id, where = NULL, details = NULL) eng_lib$prevcarga_abort(id, where = where, details = details),
         envir = eng)
  eng
}
ilh_tz <- "Etc/GMT+3"
ilh_hours <- function(d0, d1) {   # horas inteiras de [d0, d1] (dias inclusive)
  a <- lubridate::force_tz(lubridate::as_datetime(lubridate::ymd(d0)), ilh_tz)
  seq(a, by = "hour", length.out = (as.integer(lubridate::ymd(d1) - lubridate::ymd(d0)) + 1L) * 24L)
}
ilh_carga <- function(dh) {
  d <- as.numeric(lubridate::as_date(dh) - lubridate::ymd("2019-01-01")); h <- lubridate::hour(dh)
  8000 + 500 * sin(2 * pi * h / 24) + 30 * (d %% 7) + 3 * sin(d * 1.3 + h)
}
ilh_exo <- function(dh) 22 + 4 * sin(2 * pi * lubridate::hour(dh) / 24)

## Deck pequeno: ilhas 2019-06-13..20 e 2019-11-08..15, janela comum 2020-06-01..2020-06-20, prev 3 dias.
ilh_deck <- function(annex = TRUE, fer_tz = ilh_tz) {
  hc <- ilh_hours("2020-06-01", "2020-06-20")
  pv <- ilh_hours("2020-06-21", "2020-06-23")
  d <- list(
    CARGAHIST = data.table::data.table(DataHora = hc, Carga = ilh_carga(hc)),
    EXOGENAHIST = data.table::data.table(DataHora = hc, Exo_Temperatura = ilh_exo(hc)),
    EXOGENAPREV = data.table::data.table(DataHora = pv, Exo_Temperatura = ilh_exo(pv)),
    FERIADOS = data.table::data.table(Data = lubridate::ymd(c("2019-06-20", "2019-11-15")), Tipo = c(2L, 10L)))
  if (annex) {
    hf <- c(ilh_hours("2019-06-13", "2019-06-20"), ilh_hours("2019-11-08", "2019-11-15"))
    hf <- lubridate::with_tz(hf, fer_tz)
    d$CARGAFER <- data.table::data.table(DataHora = hf, Carga = ilh_carga(hf))
    d$EXOGENAFER <- data.table::data.table(DataHora = hf, Exo_Temperatura = ilh_exo(hf))
  }
  d
}
ilh_series <- function(annex = TRUE) {
  eng <- ilh_eng()
  suppressMessages(eng$TransformaDeck(ilh_deck(annex), lubridate::ymd("2020-06-21"), "10DIAS", tz = ilh_tz))$SERIECOMP
}

test_that("ac-1 sem anexo: Bloco 0 em todas as linhas e o resto do SERIECOMP e identical ao de antes", {
  s <- ilh_series(FALSE)
  expect_identical(names(s)[ncol(s)], "Bloco")
  expect_true(all(s$Bloco == 0L)); expect_type(s$Bloco, "integer")
  ## anexo vazio (0 linhas) tambem e tudo 0
  eng <- ilh_eng()
  d <- ilh_deck(FALSE); d$CARGAFER <- d$CARGAHIST[0L]; d$EXOGENAFER <- d$EXOGENAHIST[0L]
  s2 <- suppressMessages(eng$TransformaDeck(d, lubridate::ymd("2020-06-21"), "10DIAS", tz = ilh_tz))$SERIECOMP
  expect_identical(s2, s)
  ## o resto e o mesmo que a montagem sem as colunas novas: a carga e a exogena da janela estao la e na ordem
  r <- data.table::copy(s)[, Bloco := NULL]
  expect_identical(names(r), c("Data", "DiaEspecial", "DataHora", "Prev", "Carga", "Exo_Temperatura", "HV"))
  expect_identical(r$DataHora, sort(r$DataHora))
  ## contra a saida do TransformaDeck de antes da mudanca (HEAD de24021), gravada fora do repo na conferencia
})

test_that("ac-2 duas ilhas => Bloco 1 e 2; comum e horizonte 0; tz divergente aborta engine.dessem_tz_mismatch", {
  s <- ilh_series(TRUE)
  expect_identical(sort(unique(s$Bloco)), 0L:2L)
  expect_true(all(s[Data >= lubridate::ymd("2019-06-13") & Data <= lubridate::ymd("2019-06-20"), Bloco] == 1L))
  expect_true(all(s[Data >= lubridate::ymd("2019-11-08") & Data <= lubridate::ymd("2019-11-15"), Bloco] == 2L))
  expect_true(all(s[Data >= lubridate::ymd("2020-06-01"), Bloco] == 0L))
  expect_true(all(s[Prev == TRUE, Bloco] == 0L))
  expect_identical(nrow(s[Bloco == 1L]), 8L * 24L)
  ## dia de anexo adjacente a janela comum e bloco proprio (nao e juntado a Bloco 0)
  eng <- ilh_eng()
  d <- ilh_deck(TRUE)
  adj <- ilh_hours("2020-05-30", "2020-05-31")
  d$CARGAFER <- data.table::data.table(DataHora = adj, Carga = ilh_carga(adj))
  d$EXOGENAFER <- data.table::data.table(DataHora = adj, Exo_Temperatura = ilh_exo(adj))
  sa <- suppressMessages(eng$TransformaDeck(d, lubridate::ymd("2020-06-21"), "10DIAS", tz = ilh_tz))$SERIECOMP
  expect_true(all(sa[Data < lubridate::ymd("2020-06-01"), Bloco] == 1L))
  ## CONTROL: tzone divergente
  bad <- ilh_deck(TRUE, fer_tz = "UTC")
  cond <- tryCatch(eng$TransformaDeck(bad, lubridate::ymd("2020-06-21"), "10DIAS", tz = ilh_tz), error = function(e) e)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "engine.dessem_tz_mismatch")
})

## serie sintetica de lags: ilhas 2019-06-13..20 e 2019-11-08..15 + comum; Bloco 0 do comum com carga enorme
ilh_lagseries <- function(full = FALSE, comum_big = FALSE) {
  isl <- c(ilh_hours("2019-06-13", "2019-06-20"), ilh_hours("2019-11-08", "2019-11-15"))
  com <- ilh_hours("2020-01-01", "2020-02-15")
  dh <- if (full) ilh_hours("2019-06-01", "2020-02-15") else c(isl, com)
  s <- data.table::data.table(DataHora = dh, Data = lubridate::as_date(dh), Prev = FALSE, Carga = ilh_carga(dh),
                              Exo_Temperatura = ilh_exo(dh), DiaEspecial = 0, HV = 0)
  s[Data == lubridate::ymd("2019-06-20"), DiaEspecial := 2]
  s[Data == lubridate::ymd("2019-11-15"), DiaEspecial := 10]
  s[, Bloco := 0L]
  if (!full) {
    s[Data >= lubridate::ymd("2019-06-13") & Data <= lubridate::ymd("2019-06-20"), Bloco := 1L]
    s[Data >= lubridate::ymd("2019-11-08") & Data <= lubridate::ymd("2019-11-15"), Bloco := 2L]
    if (comum_big) s[Bloco == 0L, Carga := 1e6]
  }
  s
}

test_that("ac-3 linhas de feriado iguais na serie contigua e na reduzida; Bloco fora de colunas e formula; sem vazamento entre blocos", {
  e <- ilh_eng()
  full <- ilh_lagseries(TRUE); red <- ilh_lagseries(FALSE)
  a <- e$entradaPerfilFeriadoReg(data.table::copy(full), dslag = c(1, 7, 2), hrlag = 1, tpFer = c(2, 10))
  b <- e$entradaPerfilFeriadoReg(data.table::copy(red), dslag = c(1, 7, 2), hrlag = 1, tpFer = c(2, 10))
  expect_false("Bloco" %in% names(a)); expect_false("Bloco" %in% names(b))
  expect_true(all(c("LAGDS_1", "LAGDS_7", "LAGDS_2", "LAG_1") %in% names(b)))
  ra <- a[Data %in% lubridate::ymd(c("2019-06-20", "2019-11-15"))]
  rb <- b[Data %in% lubridate::ymd(c("2019-06-20", "2019-11-15"))]
  expect_identical(nrow(rb), 48L)
  expect_identical(ra, rb)
  ## nenhum LAG de ilha usa valor de outro bloco: comum com carga 1e6 nao aparece nas linhas de feriado
  big <- e$entradaPerfilFeriadoReg(data.table::copy(ilh_lagseries(FALSE, comum_big = TRUE)), dslag = c(1, 7, 2), hrlag = 1, tpFer = c(2, 10))
  rg <- big[Data %in% lubridate::ymd(c("2019-06-20", "2019-11-15"))]
  expect_identical(rg, rb)
  expect_lt(max(unlist(rg[, c("LAGDS_1", "LAGDS_7", "LAGDS_2", "LAG_1")])), 1e5)
})

test_that("Bloco ausente de names() e da formula do dcast em obtemLagsHora e obtemLagsDiaSemana", {
  e <- ilh_eng()
  red <- ilh_lagseries(FALSE)
  h <- e$obtemLagsHora(data.table::copy(red), c(1, 24), diaEsp = TRUE)
  expect_false("Bloco" %in% names(h))
  expect_true(all(c("LAG_1", "LAG_24") %in% names(h)))
  ds <- e$obtemLagsDiaSemana(data.table::copy(red), c(1, 7), 1:366, 0:1, c(2, 10))
  expect_false("Bloco" %in% names(ds))
  expect_true(all(c("LAGDS_1", "LAGDS_7") %in% names(ds)))
  ## ilha de 8 dias: o primeiro dia da ilha nao tem LAG_24 (cai pelo complete.cases), nada vem do bloco anterior
  expect_false(any(h$Data == lubridate::ymd("2019-06-13") & h$DataHora < lubridate::force_tz(lubridate::ymd_hms("2019-06-14 00:00:00"), ilh_tz)))
})

## copias das funcoes ANTES da mudanca (identidade com um bloco)
ilh_old_env <- function(eng) {
  o <- new.env(parent = eng)
  src <- '
obtemLagsHora <- function(seriehist,lags,dia = 1:7,hora = 0:23,diaAno = 1:366,hv = 0:1,diaEsp = F,diaEspLag = F){
   dados <- do.call("rbind", lapply(lags,obtemLagHora,seriehist,dia,hora,diaAno,hv,diaEsp,diaEspLag))
   dados <- dcast(dados,as.formula(paste0(paste(names(dados[,-c("LAG","VALLAG")]),collapse = "+"),"~LAG")), value.var = "VALLAG")
   dados <- dados[Prev == T | complete.cases(dados)]
   return(dados)
}
obtemLagsDiaSemana <- function(seriehist,dsLags,diaAno,hv,tpFer,hora = 0:23){
   dados <- do.call("rbind", lapply(dsLags,obtemLagDiaSemana,seriehist,diaAno,hv,tpFer,hora))
   dados <- dcast(dados,as.formula(paste0(paste(names(dados[,-c("LAG","VALLAG")]),collapse = "+"),"~LAG")), value.var = "VALLAG")
   dados <- dados[Prev == T | complete.cases(dados)]
   return(dados)
}
obtemLagDiaSemana <- function(lag,seriehist,diaAno,hv,tpFer,hora){
   seriehistD <- seriehist[8:nrow(seriehist)]
   seriehistD <- seriehistD[DiaEspecial %in% tpFer & yday(Data) %in% diaAno & hour(DataHora) %in% hora]
   seriehistD[,LAG := paste0("LAGDS_",lag)]
   seriehistD[lag < wday(Data),DataLag := paste(Data - (wday(Data)-lag),hour(DataHora))]
   seriehistD[lag >= wday(Data),DataLag := paste(Data - (7 - (lag - wday(Data))),hour(DataHora))]
   seriehist <- copy(seriehist)
   seriehist[,DataLag := paste(Data,hour(DataHora))]
   seriehistD <- seriehist[,.(DataLag,VALLAG = Carga)][seriehistD, on = "DataLag"]
   seriehistD[,DataLag := NULL]
   return(seriehistD)
}
entradaPerfilDefault <- function(seriehist,lagsmodelo,diaSemana = 1:7,hora = 0:23,diaAno = 1:366,hv = 0:1,diaEsp = FALSE,diaEspLag = FALSE){
   seriehist_lags <- obtemLagsHora(seriehist,lagsmodelo,diaSemana,hora,diaAno,hv,diaEsp,diaEspLag)
   return(seriehist_lags)
}
entradaPerfilFeriadoReg <- function(seriehist,dslag = c(1,7),hrlag = NULL,hora = 0:23,diaAno = 1:366,hv = 0:1,tpFer = 1:12,addHourFac = TRUE){
   if(!is.null(hrlag)){
      seriehist <- obtemLagsHora(seriehist,lags = hrlag,hora = hora,diaAno = diaAno,diaEsp = T)
   }
   seriehist_lags <- obtemLagsDiaSemana(seriehist,dslag,diaAno,hv,tpFer,hora)
   if(addHourFac){
      seriehist_lags[,Hora := factor(hour(DataHora),labels = sprintf("%02d",0:23))]
   }
   if(length(unique(seriehist_lags$DiaEspecial)) > 1){
      seriehist_lags[,DiaEspecial := factor(paste0("F",sprintf("%02d",DiaEspecial)),levels = paste0("F",sprintf("%02d",0:12)))]
   } else {
      seriehist_lags[,DiaEspecial := NULL]
   }
   seriehist_lags[,HV := NULL]
   return(seriehist_lags)
}'
  eval(parse(text = src), envir = o)
  o
}

test_that("ac-4 um bloco so: obtemLags*, entradaPerfilDefault e entradaPerfilFeriadoReg identical() as funcoes antigas", {
  e <- ilh_eng(); o <- ilh_old_env(e)
  one <- ilh_lagseries(TRUE)                       # Bloco 0 em tudo
  plain <- data.table::copy(one)[, Bloco := NULL]  # a serie de antes da mudanca
  expect_identical(e$obtemLagsHora(data.table::copy(one), c(1, 24)), o$obtemLagsHora(data.table::copy(plain), c(1, 24)))
  expect_identical(e$obtemLagsHora(data.table::copy(plain), c(1, 24)), o$obtemLagsHora(data.table::copy(plain), c(1, 24)))
  expect_identical(e$obtemLagsDiaSemana(data.table::copy(one), c(1, 7, 2), 1:366, 0:1, c(2, 10)),
                   o$obtemLagsDiaSemana(data.table::copy(plain), c(1, 7, 2), 1:366, 0:1, c(2, 10)))
  expect_identical(e$entradaPerfilDefault(data.table::copy(one), c(1, 24), hv = 0L),
                   o$entradaPerfilDefault(data.table::copy(plain), c(1, 24), hv = 0L))
  for (x in list(one, plain)) {
    expect_identical(e$entradaPerfilFeriadoReg(data.table::copy(x), dslag = c(1, 7, 2), hrlag = 1, tpFer = c(2, 10)),
                     o$entradaPerfilFeriadoReg(data.table::copy(plain), dslag = c(1, 7, 2), hrlag = 1, tpFer = c(2, 10)))
  }
  ## .engine_by_block com um bloco chama fn uma vez na serie inteira
  n <- 0L
  r <- e$.engine_by_block(one, function(s) { n <<- n + 1L; s })
  expect_identical(n, 1L); expect_identical(r, one)
  n <- 0L; e$.engine_by_block(plain, function(s) { n <<- n + 1L; s }); expect_identical(n, 1L)
})

test_that("ac-5 entradaPerfilDefault so Bloco 0; inclui_CargaMediaRef so do Bloco 0; buraco em ilha aborta com a linha", {
  e <- ilh_eng()
  red <- ilh_lagseries(FALSE)
  red[Bloco != 0L, Carga := Carga + 5000]        # ilhas com nivel diferente: a constante nao pode vir delas
  def <- e$entradaPerfilDefault(data.table::copy(red), c(1, 24), hv = 0L)
  expect_true(all(def$Data >= lubridate::ymd("2020-01-01")))
  expect_false("Bloco" %in% names(def))
  nz <- list(oper = "mean", flag = TRUE)
  a <- data.table::copy(red); e$inclui_CargaMediaRef(a, nz)
  b <- data.table::copy(red[Bloco == 0L]); e$inclui_CargaMediaRef(b, nz)
  ca <- a[Bloco == 0L, CargaMed]; cb <- b$CargaMed
  expect_equal(ca, cb)
  ## a constante e a media do Bloco 0 (e nao a das ilhas)
  expect_equal(a[Bloco == 0L][1L, CargaMed] * mean(red[Bloco == 0L, Carga]),
               mean(red[Bloco == 0L & Data == lubridate::ymd("2020-01-01"), Carga]))
  ## CONTROL: buraco dentro da ilha 1 => engine.dessem_lag_grid_gap nomeando a linha
  gap <- data.table::copy(red)
  i <- which(gap$Bloco == 1L)[50L]
  gap <- gap[-i]
  cond <- tryCatch(e$obtemLagsHora(gap, c(1, 24)), error = function(x) x)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "engine.dessem_lag_grid_gap")
  expect_identical(cond$details$row, 50L)
})

test_that("aggregaSerie nao agrupa por Bloco: a serie diaria do DM fica sem a coluna", {
  e <- ilh_eng()
  red <- ilh_lagseries(FALSE)
  d <- e$aggregaSerie(data.table::copy(red), "MED", FALSE, 1, 1)
  expect_false("Bloco" %in% names(d))
  d0 <- e$aggregaSerie(data.table::copy(red)[, Bloco := NULL], "MED", FALSE, 1, 1)
  expect_identical(d, d0)
})

test_that("constantes de normalizacao da demanda media vem so do Bloco 0 (ADR-046 item 1): ilhas nao as mudam", {
  e <- ilh_eng()
  cfg <- e$getConfigModel("Linear.TmpMedia")$normalizacao
  com <- ilh_series(FALSE)                          # so janela comum + horizonte (Bloco 0)
  ilh <- ilh_series(TRUE)                           # + duas ilhas de nivel diferente
  ilh[Bloco != 0L, `:=`(Carga = Carga + 4000, Exo_Temperatura = Exo_Temperatura + 15)]
  expect_true(any(ilh$Bloco != 0L))
  expect_identical(nrow(e$.serie_comum(ilh)), nrow(com[Prev == FALSE]))
  expect_true(all(e$.serie_comum(ilh)$Bloco == 0L))
  for (n in list(cfg$dm$carga, cfg$carga, cfg$perfil$cargadm)) {
    if (is.null(n)) next
    expect_identical(e$getNormalizaCarga(n, e$.serie_comum(ilh)), e$getNormalizaCarga(n, e$.serie_comum(com)))
  }
  for (n in list(cfg$dm$exogena, cfg$exogena, cfg$perfil$exogena)) {
    if (is.null(n)) next
    expect_identical(e$getNormalizaExogena(n, e$.serie_comum(ilh)), e$getNormalizaExogena(n, e$.serie_comum(com)))
  }
  ## CONTROL: sem o filtro, as ilhas mudariam a constante de carga
  n <- cfg$perfil$cargadm
  if (isTRUE(n$flag) && !is.null(n$oper))
    expect_false(identical(e$getNormalizaCarga(n, ilh[Prev == FALSE]), e$getNormalizaCarga(n, com[Prev == FALSE])))
  ## sem a coluna Bloco: caminho anterior (todo o historico observado)
  sem <- data.table::copy(com)[, Bloco := NULL]
  expect_identical(e$.serie_comum(sem), sem[Prev == FALSE])
})
