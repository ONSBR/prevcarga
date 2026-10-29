## test-sentinel.R -- gate [PLUGINS] sentinela do Holt-Winters (HW.6; AC 5). O deck sentinela do fixture_store.R codifica no
## VALOR de cada intervalo o proprio interval_start (minutos desde 2026-09-01 00:00 no fuso). O decodificador exige que
## H01 = 00:00-00:30 por interval_start, no dia certo; a delivery e rotulada por interval_end no fuso da carga.
## CONTROL: variante-fixture que agrupa por interval_end (so no teste, nunca no plugin) desloca a 1a coluna e reprova.

## decodifica a matriz H01..H48 e compara com o interval_start esperado (coluna k = 00:00 + 30*(k-1) min, no dia da linha)
hv_sentinel_ok <- function(m, days, tz) {
  dec <- hw_sentinel_decode(as.matrix(m), tz)
  want <- lubridate::force_tz(lubridate::as_datetime(rep(days, 48L)), tz) + lubridate::minutes(30L * rep(0:47, each = length(days)))
  list(ok = isTRUE(all(dec == want)), dec = dec, want = want)
}

## CONTROL: pivota H01..H48 pelo interval_end (dia e coluna do relogio do FIM do intervalo) em vez de interval_start:
## o intervalo 23:30-00:00 cai em H01 do dia seguinte, o 00:00-00:30 em H02 => a 1a coluna decodifica 23:30 do dia anterior
hw_fx_variant_group_by_end <- function(load, cuts) {
  x <- data.table::copy(load)
  x[, Data := lubridate::as_date(interval_end, tz = cuts$tz)]
  x[, slot := as.integer(lubridate::hour(interval_end)) * 2L + as.integer(lubridate::minute(interval_end)) %/% 30L + 1L]
  days <- sort(unique(x$Data))
  m <- matrix(NA_real_, length(days), 48L, dimnames = list(NULL, hv_env()$hw_slot_cols()))
  m[cbind(match(x$Data, days), x$slot)] <- x$load_mwh
  list(days = days, m = m)
}

test_that("[PLUGINS] sentinela: H01 = 00:00-00:30 por interval_start e delivery por interval_end no fuso da carga; CONTROL por interval_end reprova", {
  env <- hv_env()
  ## available_until minimo = 00:30 (o leitor exige o 1o intervalo do dia O; com 00:00 o recorte nao tem carga: completude.no_load)
  st <- hw_fx_store(sentinel = TRUE, load_until = hv_au("00:30")); cuts <- hv_cuts(st)
  expect_identical(cuts$available_until, hv_au("00:30"))
  panel <- hv_panel(st, cuts, "train")
  deck <- suppressWarnings(env$plugin_ingest(panel, cuts))
  ctv <- deck$CargaTempVer
  ## cada valor cai na coluna Hkk e no dia que codifica
  r <- hv_sentinel_ok(ctv[, env$hw_slot_cols()], ctv$Data, cuts$tz)
  expect_true(r$ok)
  expect_true(all(lubridate::hour(r$dec[seq_len(nrow(ctv))]) == 0L & lubridate::minute(r$dec[seq_len(nrow(ctv))]) == 0L))   # H01 = 00:00
  expect_identical(max(ctv$Data), cuts$last_data_day)
  ## o clima observado cai por valid_time no mesmo mapeamento
  expect_true(hv_sentinel_ok(ctv[, env$hw_temp_cols()], ctv$Data, cuts$tz)$ok)
  ## CONTROL: agrupar por interval_end desloca a 1a coluna e o MESMO decodificador reprova
  hist <- panel$load[lubridate::as_date(interval_start, tz = cuts$tz) <= cuts$last_data_day]
  bad <- hw_fx_variant_group_by_end(hist, cuts)
  rb <- hv_sentinel_ok(bad$m[stats::complete.cases(bad$m), , drop = FALSE], bad$days[stats::complete.cases(bad$m)], cuts$tz)
  expect_false(rb$ok)
  first <- rb$dec[seq_len(sum(stats::complete.cases(bad$m)))]
  expect_false(all(lubridate::hour(first) == 0L & lubridate::minute(first) == 0L))      # H01 deixou de ser 00:00
  expect_true(all(lubridate::hour(first) == 23L & lubridate::minute(first) == 30L))     # virou 23:30 do dia anterior

  ## delivery (mesmo corte 00:30, carga plausivel): rotulada por interval_end, tzone == cuts$tz, 1o intervalo de cada dia termina 00:30
  sn <- hv_store("00:30"); cn <- hv_cuts(sn)
  expect_identical(cn$available_until, hv_au("00:30")); expect_identical(cn$tz, cuts$tz)
  out <- hv_infer(hv_train(hv_panel(sn, cn, "train"), cn), hv_panel(sn, cn, "infer"), cn)
  ie <- out$delivery$interval_end
  expect_identical(attr(ie, "tzone"), cn$tz)
  expect_identical(ie, hw_lib$prevcarga_delivery_grid(cn, 9))
  expect_identical(format(ie[[1L]], "%H:%M"), "01:00")                            # dia O: 1o intervalo apos available_until 00:30
  day <- lubridate::as_date(ie - lubridate::minutes(30L), tz = cn$tz)             # dia do interval_start
  firsts <- ie[!duplicated(day)]
  expect_length(firsts, 9L)
  ## o 1o intervalo de cada dia seguinte (O+1..O+8) e 00:00-00:30: interval_end 00:30
  expect_true(all(lubridate::hour(firsts[-1L]) == 0L & lubridate::minute(firsts[-1L]) == 30L))
  ## y_pred H01 do dia O+1 e o intervalo 00:00-00:30 (interval_end 00:30), nao 00:30-01:00
  k <- match(lubridate::ymd_hm("2024-05-16 00:30", tz = cn$tz), ie)
  expect_identical(out$delivery$y_pred[[k]], out$values[1L, 1L])
  expect_false(identical(out$delivery$y_pred[[k]], out$values[1L, 2L]))
})
