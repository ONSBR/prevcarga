## test-dl-nowcast.R -- DL.5 AC4 (mm so por horas completas) e AC5 (dl_dm_head sem estado, com mm so em O).

O_ <- lubridate::ymd("2024-12-23")
au_at <- function(h, m = 0L) lubridate::force_tz(lubridate::as_datetime(O_), DL_TZ) + lubridate::hours(h) + lubridate::minutes(m)

mm_for <- function(panel, au_display) {
  cuts <- dl_cuts(available_until = au_display)
  m <- dl_model_panel(panel, cuts)
  p <- dl_load()
  deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, dl_spec("MED", 40L))))
  list(mm = p$env$dl_morning_means(deck$hourly_load, deck$available_until, deck$dataPrev), deck = deck)
}

test_that("[PLUGINS] nowcast_mm: ac-4 mm: meia hora isolada nao forma hora completa (09:00 x 09:30 identical) e mm[d] = media das horas completas", {
  a <- mm_for(dl_panel(days = 60L, until = au_at(9L)), au_at(9L))
  b <- mm_for(dl_panel(days = 60L, until = au_at(9L, 30L)), au_at(9L, 30L))
  expect_identical(a$mm, b$mm)
  expect_identical(nrow(a$mm), 42L)                       # 41 dias (40 + aquecimento) + O
  ## definicao independente sobre o painel de exibicao: slots com interval_end em (00:00, 09:00] de cada dia
  pan <- dl_panel(days = 60L, until = au_at(9L))
  ld <- pan$load
  ref <- ld[, {
    d <- lubridate::as_date(interval_start)
    t0 <- lubridate::force_tz(lubridate::as_datetime(d), DL_TZ)
    sel <- interval_end > t0 & interval_end <= t0 + lubridate::hours(9L)
    list(mm = mean(load_mwh[sel]))
  }, by = .(Data = lubridate::as_date(interval_start))]
  ref <- ref[Data >= O_ - 41L]
  expect_equal(a$mm$mm, ref$mm, tolerance = 1e-9)
  expect_identical(a$mm$Data, ref$Data)
  expect_identical(a$mm$Data[[42L]], O_)
})

test_that("[PLUGINS] nowcast_mm: ac-4 FORBIDDEN: carga com interval_end > 09:00 nao muda mm; CONTROL: perturbar 08:00-09:00 de O muda mm[O]", {
  base <- mm_for(dl_panel(days = 60L, until = au_at(9L)), au_at(9L))$mm
  pan <- dl_panel(days = 60L, until = au_at(9L))
  ## interval_end > 09:00 do relogio do dia (tarde/noite) e 00:00 do dia seguinte e fim do dia anterior => fora do corte
  tod <- lubridate::hour(pan$load$interval_end) * 60L + lubridate::minute(pan$load$interval_end)
  late <- tod > 540L | tod == 0L
  expect_gt(sum(late), 1000L)
  pan$load[late, load_mwh := load_mwh + 5000]
  pert <- mm_for(pan, au_at(9L))$mm
  expect_identical(pert$mm, base$mm)
  ## CONTROL
  pan2 <- dl_panel(days = 60L, until = au_at(9L))
  win <- pan2$load$interval_start >= au_at(8L) & pan2$load$interval_start < au_at(9L) &
    lubridate::as_date(pan2$load$interval_start) == O_
  expect_identical(sum(win), 2L)
  pan2$load[win, load_mwh := load_mwh + 100]
  pert2 <- mm_for(pan2, au_at(9L))$mm
  expect_gt(abs(pert2$mm[pert2$Data == O_] - base$mm[base$Data == O_]), 1e-5)
  expect_identical(pert2$mm[pert2$Data != O_], base$mm[base$Data != O_])
})

test_that("[PLUGINS] nowcast_mm: ac-4 corte antes do dia O no frame de modelagem (available_until 00:30): nenhuma hora entra", {
  p <- dl_load()
  h <- data.table::data.table(interval_end = lubridate::force_tz(lubridate::as_datetime(O_ - 1L), DL_TZ) + lubridate::hours(0:23),
                              Carga = 1:24 + 0)
  au_model <- lubridate::force_tz(lubridate::as_datetime(O_ - 1L), DL_TZ) + lubridate::hours(23) + lubridate::minutes(30)
  expect_identical(nrow(p$env$dl_morning_means(h, au_model, O_)), 0L)
})

## ---- AC5 -------------------------------------------------------------------------------------------------
dm_fixture <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    p <- dl_load()
    panel <- dl_panel(days = 420L, until = au_at(9L))
    cuts <- dl_cuts(); m <- dl_model_panel(panel, cuts)
    sp <- dl_spec("MED", 400L)
    deck <- dl_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, sp)))
    feats <- dl_with_cli(suppressMessages(p$env$plugin_build_features(deck, sp)))
    cache <<- list(p = p, eng = parent.env(p$env), deck = deck, feats = feats)
    cache
  }
})

test_that("[PLUGINS] nowcast_mm: plugin_build_features devolve as series do motor, 9 dias de previsao, mensagens esperadas silenciadas", {
  fx <- dm_fixture()
  f <- fx$feats
  expect_identical(names(f)[1:5], c("processado", "serieDM", "seriePF", "config_mdl", "norm"))
  expect_length(f$processado$DIASPREV, 10L)      # O..O+9 (9 entregues + 1 do motor)
  expect_identical(f$processado$NDIASPREV, 10L)
  expect_identical(f$config_mdl$nomModelo, "Linear.TmpMedia")
  expect_true(all(c("dm_carga", "dm_exo", "pf_cargadm", "pf_exo") %in% names(f$norm)))
  expect_identical(nrow(f$seriePF[Prev == TRUE]), 10L * 24L)
  expect_identical(f$deck$variant, "MED")
  msgs <- character()
  withCallingHandlers(
    dl_with_cli(fx$p$env$plugin_build_features(fx$deck, dl_spec("MED", 400L))),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_false(any(grepl("Exogena prevista nao disponivel|Ajustanto horizonte", msgs)))
})

test_that("[PLUGINS] nowcast_mm: ac-5 dl_dm_head: mm so em DM(O), tune_wsvm do env do motor, sem estado, determinismo", {
  fx <- dm_fixture()
  p <- fx$p; eng <- fx$eng; f <- fx$feats; deck <- fx$deck
  mm <- p$env$dl_morning_means(deck$hourly_load, deck$available_until, deck$dataPrev)
  expect_true(O_ %in% mm$Data)
  ## formula: DM(O) tem `mm`; DM(O+1) do motor nao
  inp <- p$env$dl_dm_head_input(eng, f$serieDM, f$config_mdl, mm, O_, constnorm = f$norm$dm_carga)
  expect_true("mm" %in% all.vars(inp$formulaMDL))
  expect_false("mm" %in% all.vars(eng$obterEntradaFormulaDM(format(O_ + 1L), f$serieDM, f$config_mdl)$formulaMDL))
  expect_false("mm" %in% all.vars(eng$obterEntradaFormulaDM(format(O_), f$serieDM, f$config_mdl)$formulaMDL))
  ## mm normalizada pela constnorm da DM
  expect_equal(inp$dadosEntrada$serieDiariaModelo[Data == O_]$mm, mm[Data == O_]$mm / f$norm$dm_carga)
  ## espiao no tune_wsvm do env do motor
  seen <- new.env(); seen$n <- 0L
  real <- eng$tune_wsvm
  assign("tune_wsvm", function(formula, ...) { seen$n <- seen$n + 1L; seen$formula <- formula; real(formula, ...) }, envir = eng)
  on.exit(assign("tune_wsvm", real, envir = eng), add = TRUE)
  g0 <- ls(globalenv()); e0 <- ls(eng, all.names = TRUE)
  m1 <- suppressMessages(p$env$dl_dm_head(eng, f$serieDM, f$config_mdl, mm, O_, seed = 42L, constnorm = f$norm$dm_carga))
  m2 <- suppressMessages(p$env$dl_dm_head(eng, f$serieDM, f$config_mdl, mm, O_, seed = 42L, constnorm = f$norm$dm_carga))
  expect_identical(seen$n, 2L)
  expect_true("mm" %in% all.vars(seen$formula))
  expect_s3_class(m1, "svm")
  expect_identical(ls(globalenv()), g0)
  expect_identical(ls(eng, all.names = TRUE), e0)
  expect_identical(m1, m2)
  expect_true("mm" %in% attr(m1$terms, "term.labels"))
  ## CONTROL: mm sem a linha de O => [prevcarga/contract..completude] plugin.dessem_linear_mm_missing_origin
  mm_no <- mm[Data != O_]
  cond <- expect_dl_error(p$env$dl_dm_head(eng, f$serieDM, f$config_mdl, mm_no, O_, seed = 42L), "plugin.dessem_linear_mm_missing_origin")
  expect_identical(cond$exit_code, 3L)
  expect_identical(seen$n, 2L)                              # nao ajustou
  ## treino: require_origin = FALSE aceita mm sem a linha de O (a linha-alvo e descartada)
  m3 <- suppressMessages(p$env$dl_dm_head(eng, f$serieDM, f$config_mdl, mm_no, O_, require_origin = FALSE, seed = 42L,
                                          constnorm = f$norm$dm_carga))
  expect_s3_class(m3, "svm")
})
