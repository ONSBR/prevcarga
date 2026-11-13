## test-exo-prev-cut.R -- F3.7: .rd_exo(kind "prev") com cuts$delivery_days (horizonte cortado pelo clima previsto). Tabelas minimas
## (clima horario sintetico), nenhum treino. Dia ENTREGUE nunca sai de copia; so o dia de apoio do spline segue a regra da cauda.
RDX_TZ <- "Etc/GMT+3"
rdx_weather <- function(days_full, short = integer(), keep = 5L, O = lubridate::ymd("2025-06-20")) {
  vt <- lapply(seq_len(days_full) - 1L, function(i) {
    n <- if (i %in% short) keep else 24L
    lubridate::force_tz(lubridate::as_datetime(O + i), RDX_TZ) + lubridate::hours(seq_len(n) - 1L)
  })
  vt <- do.call(c, vt)
  data.table::data.table(area_code = "SE", issue_time = lubridate::force_tz(lubridate::as_datetime(O), "UTC"),
                         valid_time = vt, temperature = as.numeric(seq_along(vt)) %% 17 + 20)
}
rdx_cuts <- function(delivery_days = NULL, O = lubridate::ymd("2025-06-20")) {
  c(list(origin_day = O, forecast_origin = O, tz = RDX_TZ), if (!is.null(delivery_days)) list(delivery_days = as.integer(delivery_days)))
}
rdx_exo <- function(w, cuts, horizon_days) {
  p <- rd_load()
  msgs <- character()
  out <- tryCatch(withCallingHandlers(rd_with_cli(p$env$.rd_exo(w, "prev", cuts, horizon_days, "SE")),
                                      message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }),
                  prevcarga_error = function(e) e)
  list(out = out, msgs = msgs)
}

test_that("[F3.7] clima horario ate O+3 e delivery_days = 4: sem linha >= O+4 e nenhuma copia de dia entregue", {
  O <- lubridate::ymd("2025-06-20")
  w <- rdx_weather(4L)                                    # O..O+3 completos; O+4 inexistente
  r <- rdx_exo(w, rdx_cuts(4L), horizon_days = 4L)         # horizonte do deck = entregues (4), sem apoio
  expect_s3_class(r$out$DataHora, "POSIXct")
  expect_identical(nrow(r$out), 4L * 24L)
  expect_true(all(lubridate::as_date(r$out$DataHora) <= O + 3L))
  expect_length(r$msgs, 0L)
  expect_false(any(grepl("persistido", r$msgs)))
  ## dia de apoio do spline (O+4, fora da entrega) ausente: a regra antiga da cauda o persiste; os 4 dias entregues seguem do clima
  r2 <- rdx_exo(w, rdx_cuts(4L), horizon_days = 5L)
  expect_false(inherits(r2$out, "condition"))
  expect_identical(nrow(r2$out), 5L * 24L)
  expect_identical(r2$out[lubridate::as_date(DataHora) <= O + 3L], r$out)
  expect_length(r2$msgs, 1L)
  expect_true(grepl(format(O + 4L), r2$msgs[[1L]], fixed = TRUE))
})

test_that("[F3.7] dia ENTREGUE curto nao e copiado (contrato recusa); CONTROL sem cuts$delivery_days: a persistencia antiga ainda age", {
  O <- lubridate::ymd("2025-06-20")
  w <- rdx_weather(5L, short = 2L)                         # O+2 curto (5 horas): dentro das 4 entregues
  r <- rdx_exo(w, rdx_cuts(4L), horizon_days = 5L)
  expect_s3_class(r$out, "prevcarga_error")
  expect_identical(r$out$id, "plugin.regdin_panel_contract")
  expect_length(r$msgs, 0L)
  ## CONTROL: sem delivery_days (treino / chamada antiga) o dia curto e persistido do ultimo completo
  c1 <- rdx_exo(w, rdx_cuts(NULL), horizon_days = 5L)
  expect_false(inherits(c1$out, "condition"))
  expect_identical(nrow(c1$out), 5L * 24L)
  expect_true(any(grepl("persistido do ultimo dia completo", c1$msgs, fixed = TRUE)))
})

test_that("[F3.7] FORBIDDEN clima completo: saida identica com e sem cuts$delivery_days; dia de apoio curto (> protect_until) segue a regra antiga", {
  w <- rdx_weather(5L)
  a <- rdx_exo(w, rdx_cuts(NULL), 5L)$out
  b <- rdx_exo(w, rdx_cuts(4L), 5L)$out
  expect_identical(a, b)
  ws <- rdx_weather(5L, short = 4L)                        # so o dia de apoio (O+4) curto
  s <- rdx_exo(ws, rdx_cuts(4L), 5L)
  expect_false(inherits(s$out, "condition"))
  expect_identical(nrow(s$out), 5L * 24L)
  expect_true(any(grepl("persistido do ultimo dia completo", s$msgs, fixed = TRUE)))
})

test_that("[F3.7] corte profundo (clima ate O+1, 2 dias entregues): a cauda NAO entregue alimenta o motor; os dias entregues nunca saem de copia", {
  O <- lubridate::ymd("2025-06-20")
  w <- rdx_weather(2L)
  r <- rdx_exo(w, rdx_cuts(2L), horizon_days = 10L)        # deck do motor com o horizonte fixo (9 + 1)
  expect_false(inherits(r$out, "condition"))
  expect_identical(nrow(r$out), 10L * 24L)
  ## O..O+1 sao o clima (identico); O+2.. so persistidos do ultimo dia coberto
  expect_identical(r$out[lubridate::as_date(DataHora) <= O + 1L]$Exo_Temperatura, w$temperature)
  expect_true(any(grepl("persistido do ultimo dia completo", r$msgs, fixed = TRUE)))
  ## CONTROL: sem cuts$delivery_days a mesma tabela excede max_tail_fill_days e o contrato recusa
  c1 <- rdx_exo(w, rdx_cuts(NULL), horizon_days = 10L)
  expect_s3_class(c1$out, "prevcarga_error")
})
