## test-exo-prev-cut.R -- F3.7: dl_exo_prev (exogena prevista do deck) com protect_until = ultimo dia entregue. Tabelas minimas,
## nenhum treino. Dia ENTREGUE curto nunca sai de copia (o contrato do plugin recusa); so o dia de apoio segue a regra da cauda.
DLX_TZ <- "Etc/GMT+3"
dlx_weather <- function(days, short = integer(), keep = 5L, O = lubridate::ymd("2025-06-20")) {
  vt <- do.call(c, lapply(seq_len(days) - 1L, function(i) {
    n <- if (i %in% short) keep else 24L
    lubridate::force_tz(lubridate::as_datetime(O + i), DLX_TZ) + lubridate::hours(seq_len(n) - 1L)
  }))
  data.table::data.table(area_code = "SE", issue_time = lubridate::force_tz(lubridate::as_datetime(O), "UTC"),
                         valid_time = vt, temperature = as.numeric(seq_along(vt)) %% 17 + 20)
}
dlx_run <- function(w, last_day, protect_until) {
  p <- dl_plugin()
  O <- lubridate::ymd("2025-06-20")
  msgs <- character()
  out <- withCallingHandlers(dl_with_cli(p$env$dl_exo_prev(w, "SE", O, last_day, p$env$plugin_constants(), dl_engine_env(p),
                                                          protect_until = protect_until)),
                             message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  list(out = out, msgs = msgs)
}

test_that("[F3.7] clima ate O+3 com last_day = O+3 e protect_until = O+3: sem linha >= O+4 e sem copia", {
  O <- lubridate::ymd("2025-06-20")
  r <- dlx_run(dlx_weather(6L), O + 3L, O + 3L)           # o deck tem mais dias; a janela corta em O+3
  expect_identical(nrow(r$out), 4L * 24L)
  expect_true(all(lubridate::as_date(r$out$DataHora) <= O + 3L))
  expect_length(r$msgs, 0L)
  r2 <- dlx_run(dlx_weather(4L), O + 3L, O + 3L)
  expect_identical(nrow(r2$out), 4L * 24L)
  expect_length(r2$msgs, 0L)
})

test_that("[F3.7] dia entregue curto <= protect_until nao e copiado; CONTROL sem protect_until persiste; dia > protect_until segue a regra", {
  O <- lubridate::ymd("2025-06-20")
  w <- dlx_weather(5L, short = 2L)                        # O+2 curto, dentro do horizonte entregue
  p <- dlx_run(w, O + 4L, O + 3L)
  expect_identical(nrow(p$out), nrow(w))                  # devolvido como esta
  expect_false(any(grepl("persistido", p$msgs)))
  expect_identical(sum(lubridate::as_date(p$out$DataHora) == O + 2L), 5L)   # o buraco segue la: o contrato recusa depois
  c1 <- dlx_run(w, O + 4L, NULL)
  expect_true(any(grepl("persistido do ultimo dia completo", c1$msgs, fixed = TRUE)))
  expect_identical(sum(lubridate::as_date(c1$out$DataHora) == O + 2L), 24L)
  ## dia de apoio (O+4, > protect_until) curto: persistido como antes
  s <- dlx_run(dlx_weather(5L, short = 4L), O + 4L, O + 3L)
  expect_true(any(grepl("persistido do ultimo dia completo", s$msgs, fixed = TRUE)))
  expect_identical(nrow(s$out), 5L * 24L)
})

test_that("[F3.7] corte profundo (clima ate O+1): a cauda nao entregue e persistida alem de max_tail_fill_days; os 2 dias entregues ficam do clima", {
  O <- lubridate::ymd("2025-06-20")
  w <- dlx_weather(2L)
  r <- dlx_run(w, O + 9L, O + 1L)
  expect_identical(nrow(r$out), 10L * 24L)
  expect_identical(r$out[lubridate::as_date(DataHora) <= O + 1L]$Exo_Temperatura, w$temperature)
  ## CONTROL: sem protect_until o limite e o antigo (max_tail_fill_days) e a cauda de 8 dias nao e preenchida
  c1 <- dlx_run(w, O + 9L, NULL)
  expect_identical(nrow(c1$out), 2L * 24L)
})
