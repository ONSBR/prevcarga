## test-train-honesty.R -- DL.8 (AC5 do epico): honestidade do TREINO do dessem-linear (ADR-0023, origin_scoped: o treino
## ve ate O-1). Molde de 4 bracos do honesty_gate.R (M3.2) sobre o deck sintetico: FORBIDDEN = perturbar o dia O => artefato
## byte-identico (sha256); CONTROL = perturbar O-1 => sha256 muda. O artefato nao depende do corte intradiario (09:00 x 12:00).

DL_TH_STORE_09 <- function() dl_fx_store("09:00")
DL_TH_STORE_12 <- function() dl_fx_store("12:00")

test_that("[PLUGINS] corte_treino: sha256 do artefato igual com available_until 09:00 e 12:00 (nada do corte entra no artefato)", {
  s09 <- DL_TH_STORE_09(); s12 <- DL_TH_STORE_12()
  c09 <- dl_fx_cuts(s09); c12 <- dl_fx_cuts(s12)
  expect_identical(c09$available_until, dl_au("09:00")); expect_identical(c12$available_until, dl_au("12:00"))
  expect_identical(c09$train_origin, DL8_O); expect_identical(c09$last_data_day, DL8_O - 1L)
  a09 <- dl_base_art(); a12 <- dl_art12()      # treino real sob o corte de 12:00
  expect_identical(dl_sha(a12), dl_sha(a09))
  expect_identical(a12, a09)
  ## o artefato e dado puro: sem corte, relogio nem caminho; carimbo = origem do treino (nao o corte intradiario)
  expect_true(dl_lib$prevcarga_plugin_scan_pure(a09))
  expect_identical(a09$stamp$train_origin, DL8_O); expect_identical(a09$stamp$origin_day, DL8_O)
  nm <- function(x) c(names(x), unlist(lapply(x, function(y) if (is.list(y) && !is.data.frame(y)) nm(y))))
  expect_false(any(c("available_until", "trained_at", "cut", "weather_issue_limit") %in% nm(a09)))
  expect_identical(a09$payload$n_leak, 0L)
})

test_that("[PLUGINS] corte_treino: FORBIDDEN dia O (carga e clima observado) => sha256 igual; CONTROL O-1 => muda; molde HONESTO", {
  st <- DL_TH_STORE_09(); cuts <- dl_fx_cuts(st); O <- cuts$origin_day
  d0 <- dl_day_start(O); d1 <- dl_day_start(O - 1L)
  base <- dl_base_art(); sha0 <- dl_sha(base)
  ## FORBIDDEN: o dia O inteiro perturbado no store (carga e temperatura observada) nao chega ao painel de treino
  forb <- dl_fx_perturb(dl_fx_perturb(st, "load", after = d0 - 1), "weather_observed", after = d0 - 1)
  expect_identical(dl_fx_panel(forb, cuts, "train")[c("load", "weather_observed")],
                   dl_fx_panel(st, cuts, "train")[c("load", "weather_observed")])
  fb <- dl_train_memo(forb, cuts)
  expect_identical(dl_sha(fb), sha0)
  ## CONTROL: O-1 inteiro perturbado (+30% +1) => o artefato muda
  ctrl_st <- dl_fx_perturb(st, "load", after = d1 - 1, at_or_before = d0 - 1)
  ctrl <- dl_train_memo(ctrl_st, cuts)
  expect_false(identical(dl_sha(ctrl), sha0))
  expect_false(identical(ctrl$payload$norm, base$payload$norm))
  ## molde M3.2 (4 bracos): cut = fim de last_data_day; treino pelo despachante (memo por painel), infer limpo
  ipan <- dl_fx_panel(st, cuts, "infer")
  gp <- data.frame(area_code = ipan$load$area_code, interval_start = ipan$load$interval_start,
                   interval_end = ipan$load$interval_end, y_true = ipan$load$load_mwh, stringsAsFactors = FALSE)
  run_fn <- function(p) {
    ch <- which(abs(p$y_true - gp$y_true) > 0)
    s2 <- if (!length(ch)) st else dl_fx_perturb(st, "load", after = min(p$interval_start[ch]) - lubridate::minutes(30L),
                                                  at_or_before = max(p$interval_start[ch]))
    dl_infer(dl_train_memo(s2, cuts), st, cuts)$delivery
  }
  g <- dl_lib$prevcarga_honesty_gate(run_fn, gp, d0, cuts$horizon_days)
  expect_identical(g$verdict, "HONESTO")
  expect_true(g$reach_ok)
  expect_lte(g$clean2_delta, DL8_TOL); expect_lte(g$forbidden_delta, DL8_TOL); expect_gt(g$control_delta, DL8_TOL)
  expect_gt(g$arms$forbidden$n_perturbed, 0L); expect_gt(g$arms$control$n_perturbed, 0L)
})

test_that("[PLUGINS] corte_treino: painel de treino com o dia O => contract (despachante) e train_sees_day_o (estagio), sem ajuste de SVM", {
  st <- DL_TH_STORE_09(); cuts <- dl_fx_cuts(st); O <- cuts$origin_day
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)
  ## despachante: o painel do infer (com a carga de O ate 09:00) no treino e recusado (confere, nao corta)
  e <- dl_err(dl_train(st, cuts, panel = dl_fx_panel(st, cuts, "infer")))
  expect_dl_class(e, "contract", 2L, id = "plugin.train_beyond_cut")
  expect_match(conditionMessage(e), format(O), fixed = TRUE)
  ## estagio: a guarda propria do plugin (deck com linha do dia O) aborta antes do 1o ajuste
  env <- dl_plugin()$env
  m <- dl_model_panel(dl_fx_panel(st, cuts, "train"), cuts)
  spec <- dl_fx_spec()
  frame <- dl_with_cli(suppressMessages(env$plugin_ingest(m$panel, m$cuts, NULL)))
  feats <- dl_with_cli(suppressMessages(env$plugin_build_features(frame, spec)))
  row <- data.table::data.table(DataHora = dl_day_start(O) + lubridate::hours(3L), Carga = 9000)
  bad <- feats
  bad$deck$CARGAHIST <- data.table::rbindlist(list(bad$deck$CARGAHIST, row))
  e2 <- dl_err(env$dl_training(bad, spec$params, spec$seed))
  expect_dl_class(e2, "contract", 2L, id = "plugin.dessem_linear_train_sees_day_o")
  expect_match(conditionMessage(e2), format(O), fixed = TRUE)
  bad2 <- feats
  bad2$deck$origin_load <- data.table::data.table(interval_end = row$DataHora, Carga = 9000)
  e3 <- dl_err(env$plugin_train(bad2, spec))
  expect_identical(e3$id, "plugin.dessem_linear_train_sees_day_o")
  expect_identical(spy$log$n, 0L)                                   # nenhum SVM ajustado
  ## CONTROL: o deck sem a linha de O passa na guarda
  expect_true(env$dl_assert_train_cut(feats$deck))
})
