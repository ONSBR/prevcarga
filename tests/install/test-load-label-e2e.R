## test-load-label-e2e.R -- `load_label` ponta a ponta no bundle INSTALADO (M8.9, AC 1; ADR-0012/0019/0035/0037).
## O deck de fixture declarado `end` e o MESMO deck pre-deslocado -30 min declarado `start` produzem a mesma entrega pelo lancador
## instalado (`prevcarga forecast --origin <O> --format json`); todo `target_datetime` e o `interval_end`. CONTROL: o pre-deslocado
## declarado `end` desloca a entrega em um intervalo. O mundo (bundle instalado, artefatos oficiais sincronizados) vem de helper-e2e.R.
## So Linux nesta maquina; a perna Windows e do gate windows-vm do M8 (ver tests/ab/test-cross-platform.R). Tudo em sandbox/ (gitignored).
library(testthat)
pci_skip_unless_ready()
W <- pc_e2e_world()
O <- lubridate::ymd(W$origin)

E <- pc_e2e_world_end(W)                                                      # artefatos treinados com carga rotulada `end`
DECK_END   <- E$deck                                                         # rotulo `end`: carimbo = interval_end
DECK_START <- pc_fixture_deck_relabel(DECK_END, "start")                     # o mesmo deck, carga -30 min, declarado `start`
DECK_WRONG <- pc_fixture_deck_relabel(DECK_END, "start", declare = "end")    # CONTROL: carimbos de `start` declarados `end`
## CONTROL com entrega: o mesmo erro de rotulo, mas com UM intervalo a mais de carga (09:00-09:30, carimbo 09:00) para o available_until continuar
## em 09:00 (o corte de 08:30 nao tem pesos da combina treinados); cada valor de carga passa a ser lido um intervalo mais cedo
DECK_WRONG_DELIVERY <- pc_fixture_deck_relabel(pc_e2e_deck(file.path(pci_work_dir(), "deck-start-0930"), W$origin, until = "09:30", label = "start"),
                                               "start", declare = "end")

## cada rotulo roda com os artefatos treinados nele (o artefato recusa outro rotulo); o deck `start` e o do mundo base
RUN <- list(end = pc_e2e_forecast(W, DECK_END, from = E$state), start = pc_e2e_forecast(W, DECK_START),
            wrong = pc_e2e_forecast(W, DECK_WRONG_DELIVERY, from = E$state))

test_that("o re-rotulo e fiel: o deck `end` pre-deslocado e byte a byte o deck `start` gerado nativamente", {
  rd <- function(d) {                                           # a carga de todas as areas, na ordem canonica
    x <- data.table::rbindlist(lapply(sort(Sys.glob(file.path(d, "load", "area_code=*", "year=*", "*.parquet")), method = "radix"),
                                      function(f) as.data.frame(arrow::read_parquet(f))))
    as.data.frame(x[order(x$timestamp), ])
  }
  expect_identical(rd(DECK_START), rd(W$deck))
  expect_identical(jsonlite::read_json(file.path(DECK_START, "manifest.json")), jsonlite::read_json(file.path(W$deck, "manifest.json")))
  expect_false(identical(rd(DECK_END)$timestamp, rd(DECK_START)$timestamp))                 # CONTROL: o deck `end` NAO tem os mesmos carimbos
})

long <- function(r, f = "SIN.parquet") as.data.frame(arrow::read_parquet(file.path(r$saida, f)))
parquets <- function(r) { s <- pc_e2e_delivery_shas(r$saida); s[grepl("\\.parquet$", names(s))] }
manifest <- function(r) jsonlite::read_json(file.path(r$saida, "manifest.json"))

test_that("[FORBIDDEN] deck `end` e o mesmo deck pre-deslocado `start`: entregas identicas byte a byte (Delta = 0) pelo lancador instalado", {
  for (r in RUN[c("end", "start")]) expect_identical(r$cli$status, 0L)
  a <- parquets(RUN$end); b <- parquets(RUN$start)
  expect_gte(length(a), 3L)                                   # SIN + subsistemas (+ folhas soltas)
  expect_identical(names(a), names(b))
  expect_identical(unname(a), unname(b))                      # cada LONG, byte a byte
  expect_identical(manifest(RUN$end)$available_until, manifest(RUN$start)$available_until)
  ## o rotulo declarado e a unica diferenca declarada entre as duas rodadas
  expect_identical(manifest(RUN$end)$load_label, "end"); expect_identical(manifest(RUN$start)$load_label, "start")
})

test_that("[FORBIDDEN] todo target_datetime e interval_end: o primeiro intervalo previsto termina 30 min apos o available_until e a grade e de 30 min", {
  for (r in RUN[c("end", "start")]) {
    x <- long(r)
    tz <- attr(x$target_datetime, "tzone")
    au <- lubridate::ymd_hm(manifest(r)$available_until, tz = tz)
    expect_identical(sort(x$target_datetime)[[1L]], au + lubridate::minutes(30L))        # interval_start do 1o previsto = available_until
    expect_true(all(diff(as.numeric(x$target_datetime)) == 1800))
    expect_identical(x$lead[[1L]], 19L)                                                 # 09:30 = 19o intervalo do dia O (lead 1 = 00:30)
    expect_true(all(lubridate::minute(x$target_datetime) %in% c(0L, 30L)) && all(lubridate::second(x$target_datetime) == 0))
  }
  ## o available_until do deck e o interval_end da ultima carga do deck (09:00): o mesmo nos dois rotulos
  expect_identical(manifest(RUN$start)$available_until, paste0(W$origin, "T09:00"))
})

avail <- function(deck) {                    # available_until que o leitor deriva do deck (plano do train-models, sem treinar)
  r <- pce_ok(pc_e2e_cli(W$root, pce_args("train-models", deck, tempfile("plan-"), "--origin", W$origin, "--model=random-forest", "--dry-run", "--skip-validate")), "plan")
  lubridate::ymd_hms(sub("T", " ", sub("-03:00$", "", r$json$result$available_until)), tz = PCE_TZ)
}

test_that("[CONTROL] pre-deslocado declarado `end` desloca a carga em exatamente um intervalo: o corte recua 30 min e a entrega muda (Delta != 0)", {
  au <- list(end = avail(DECK_END), start = avail(DECK_START), wrong = avail(DECK_WRONG))
  expect_identical(au$end, au$start)                                                                  # rotulos certos: o mesmo corte
  expect_identical(as.numeric(difftime(au$start, au$wrong, units = "mins")), 30)                        # rotulo errado: 1 intervalo mais cedo
  w <- RUN$wrong
  expect_identical(w$cli$status, 0L)
  expect_identical(manifest(w)$available_until, manifest(RUN$start)$available_until)                   # mesmo corte, mesma grade de entrega...
  x0 <- long(RUN$start); x1 <- long(w)
  expect_identical(x1$target_datetime, x0$target_datetime)
  expect_false(identical(parquets(w), parquets(RUN$start)))                                            # ...mas Delta != 0: o teste tem dentes
  expect_gt(max(abs(x1$y_pred - x0$y_pred)), 0)
})

