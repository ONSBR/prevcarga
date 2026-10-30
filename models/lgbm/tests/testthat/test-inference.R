## test-inference.R -- LGBM.4: inferencia BLF de duas passadas 100% do artefato (G-ART). Deck sintetico P.0e, area SP (heat_area real),
## O = 2026-10-01; o artefato e treinado UMA vez pelo despachante (lgbm_fx_base). Cada gate traz FORBIDDEN e CONTROL.
## Paridade com o oraculo (A/B) e do LGBM.5; aqui: registro dos 5 estagios, gate05/07/12, window/contract/completude.
library(testthat)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
D <- lubridate::ymd
P <- lgbm_source_plugin()
K <- lgbm_consts(P)
b <- lgbm_fx_base()
O <- LGBM_O

## cadeia direta dos estagios (sem despachante): ingestao -> features de infer -> infer, sempre pelo payload do artefato
inf_direct <- function(o, panel = o$panel, cuts = o$cuts) {
  frame <- P$plugin_ingest(panel, cuts)
  inf <- P$plugin_inference_features(frame, o$art$payload)
  list(frame = frame, inf = inf, out = P$plugin_infer(o$art$payload, inf, cuts))
}

## ---- AC1: registro dos 5 estagios pelo loader --------------------------------------------------------------------------
test_that("[PLUGINS] loader_estagios: [AC1] loader: 5 estagios Tier A qualificados, 2a carga identica (memo), nada em globalenv/search; sem params/spec", {
  g0 <- ls(globalenv(), all.names = TRUE); s0 <- search()
  lgbm_lib$prevcarga_plugin_unload_all()
  root <- file.path(lgbm_root(), "models")
  p1 <- lgbm_lib$prevcarga_plugin_load("lgbm", root)
  p2 <- lgbm_lib$prevcarga_plugin_load("lgbm", root)
  expect_identical(p1, p2)
  reg <- lgbm_lib$prevcarga_registry()
  for (st in c("ingestion", "feature_engineering", "training", "inference_features", "inference")) {
    r <- reg[[paste0("lgbm/", st)]]
    expect_false(is.null(r), info = st)
    expect_identical(r$tier, "A", info = st)
    expect_gt(length(r$requires), 0L); expect_gt(length(r$provides), 0L)
    expect_true(all(grepl("^(lgbm::|data[.])", c(r$requires, r$provides))), info = st)
  }
  expect_identical(ls(globalenv(), all.names = TRUE), g0); expect_identical(search(), s0)
  expect_identical(names(formals(P$plugin_inference_features)), c("frame", "artifact", "cuts"))
  expect_identical(names(formals(P$plugin_infer)), c("artifact", "inf_feat", "cuts"))
  for (f in list(P$plugin_inference_features, P$plugin_infer)) expect_false(any(c("params", "spec") %in% names(formals(f))))
  expect_identical(names(formals(p1$descriptor$stages$inference$fn)), c("payload", "inf_features", "cuts"))
})

test_that("[PLUGINS] loader_estagios: [AC1] CONTROL: estagio de infer com formal `params` e recusado pelo loader [prevcarga/contract]", {
  r <- lgbm_fx_runyaml()
  writeLines("lgbm_stage_inference <- function(payload, inf_features, cuts, params) NULL", file.path(r, "lgbm", "R", "zz_params.R"))
  lgbm_lib$prevcarga_plugin_unload_all()
  e <- fail(lgbm_lib$prevcarga_plugin_load("lgbm", r))
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  expect_identical(e$id, "plugin.params_formal")
  ## CONTROL do CONTROL: a copia sem o override carrega
  expect_null(fail(lgbm_lib$prevcarga_plugin_load("lgbm", lgbm_fx_runyaml())))
})

## ---- AC5: gate07 (Stage A = 48 sem reciclagem) -------------------------------------------------------------------------
test_that("[PLUGINS] gate07_stage_a_48: [AC5] gate07: Stage A devolve exatamente 48 previsoes; matriz de 47 linhas => contract (sem reciclagem)", {
  r <- inf_direct(b)
  expect_identical(dim(r$inf$dt_D), c(48L, ncol(r$inf$dt_D))); expect_identical(nrow(r$inf$dt_D1), 48L)
  art <- P$plugin_thaw(b$art$payload, b$cuts)
  dn <- data.table::copy(r$inf$dt_D); P$lgbm_normalise_dt(dn, art$min_max_BLF)
  X <- as.matrix(dn[, art$colunas_entrada_BLF, with = FALSE])
  expect_length(P$lgbm_stage_a(art$booster, X, art$best_iter), 48L)
  e <- fail(P$lgbm_stage_a(art$booster, X[1:47, , drop = FALSE], art$best_iter))
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, "contract"); expect_match(conditionMessage(e), "gate07")
  expect_identical(r$out$meta$params_source, "artifact")
  expect_identical(r$out$meta$best_iter, art$best_iter)
})

## ---- AC5: gate05 (1o de jul/out/jan: 48 finitos em O+1 com o dia O cheio) --------------------------------------------
test_that("[PLUGINS] gate05_sem_crash: [AC5] gate05: origem no 1o de jul/out/jan, dia O cheio => 48 valores finitos em O+1 e values 1x48", {
  for (o in list(D("2026-07-01"), D("2026-10-01"), D("2027-01-01"))) {
    until <- lubridate::force_tz(lubridate::as_datetime(o + 1L), LGBM_TZ)
    deck <- lgbm_fx_deck(days = 120L, O = o, available_until = until)
    org <- lgbm_fx_origin(deck)
    expect_identical(org$cuts$available_until, until)
    env <- lgbm_fx_infer(org)
    expect_identical(nrow(env$delivery), 48L, info = format(o))
    expect_true(all(is.finite(env$delivery$y_pred)), info = format(o))
    expect_identical(dim(env$values), c(1L, 48L), info = format(o))
    expect_identical(env$values[1L, ], env$delivery$y_pred, info = format(o))
    expect_identical(env$meta$n_real_D, 48L)
  }
})

## ---- AC5: gate12 + parity_construcao_d1 (features de O+1 == treino que inclui O+1) --------------------------------------
test_that("[PLUGINS] parity_construcao_d1: [AC5] gate12/parity_construcao_d1: features e lag de O+1 do infer == plugin_build_features de um treino que inclui o dia (<= 1e-9)", {
  at <- lgbm_fx_at(lubridate::ymd_hm("2026-10-02 00:00", tz = LGBM_TZ))     # dia O cheio
  r <- inf_direct(b, at$panel, at$cuts)
  ## treino que INCLUI O+1: carga crua (alem do corte do leitor) do store ragged ate O+1; mesmo clima/feriados
  rag <- lgbm_fx_ragged(); ctx <- lgbm_fx_ctx(rag)
  raw <- lgbm_lib$prevcarga_read_load(ctx, LGBM_HEAT, from = O - 59L, to = O + 1L)
  tp <- lgbm_fx_dpanel(rag, ctx, "infer", at$cuts, history_days = 60L)   # mesmo corte de clima do infer
  tp$load <- raw
  ## o clima de O+1 do infer e lead 1 (emissoes ate o dia O, UTC); o treino usa lead 0 => restringe as emissoes ao dia O
  tp$weather_forecast <- tp$weather_forecast[lubridate::as_date(lubridate::with_tz(issue_time, "UTC")) <= O]
  cuts_t <- at$cuts; cuts_t$last_data_day <- O + 1L; cuts_t$origin_day <- O + 1L   # clima de O+1 so entra ate a origem
  frame_t <- P$lgbm_ingest(tp, cuts_t, NULL, "train", K)
  pay <- b$art$payload
  feats <- P$lgbm_build_features(frame_t, list(params = pay$params, seed = NULL), cuts_t, K)
  tr <- feats$feature_table[Data == O + 1L][order(Hora)]
  expect_identical(nrow(tr), 48L)
  d1 <- r$inf$dt_D1[order(Hora)]
  cols <- c(grep("^(CarWav|TempWav)_", names(d1), value = TRUE), "Temperatura_prevista_BLF", "Hora_seno",
            grep("^(DiaSemana|DiaEspecial_|Mes_)", names(d1), value = TRUE), "PreFeriado", "PosFeriado")
  expect_gt(length(cols), 120L)
  mx <- vapply(cols, function(cn) max(abs(as.numeric(d1[[cn]]) - as.numeric(tr[[cn]]))), 0)
  expect_lte(max(mx), 1e-9)
  ## Sazonalidade: so do artefato (consulta por (TipoDia, Hora)), nunca recomputada no infer
  sz <- pay$sazonalidade
  want <- sz$Sazonalidade[match(paste(d1$TipoDia, d1$Hora), paste(sz$TipoDia, sz$Hora))]
  expect_identical(d1$Sazonalidade, want)
  ## parity_construcao_d1: lag de O+1 (Passada 2, dia O cheio) == lag do treino (disable_loess)
  art <- P$plugin_thaw(pay, at$cuts)
  dn <- data.table::copy(r$inf$dt_D); P$lgbm_normalise_dt(dn, art$min_max_BLF)
  pA <- P$lgbm_stage_a(art$booster, as.matrix(dn[, art$colunas_entrada_BLF, with = FALSE]), art$best_iter)
  comp <- P$lgbm_stage_b(pA, r$inf$dt_D[order(Hora)], art)
  expect_lte(max(abs(comp - tr$CargaGlobal_anterior_BLF)), 1e-9)
  expect_lte(max(abs(d1$CargaGlobal_anterior_BLF - tr$CargaGlobal_anterior_BLF)), 1e-9)
  ## CONTROL: com o dia O so ate 09:30 o lag completado NAO bate com o do treino (a tarde e estimada)
  r2 <- inf_direct(b)
  dn2 <- data.table::copy(r2$inf$dt_D); P$lgbm_normalise_dt(dn2, art$min_max_BLF)
  pA2 <- P$lgbm_stage_a(art$booster, as.matrix(dn2[, art$colunas_entrada_BLF, with = FALSE]), art$best_iter)
  comp2 <- P$lgbm_stage_b(pA2, r2$inf$dt_D[order(Hora)], art)
  expect_gt(max(abs(comp2 - tr$CargaGlobal_anterior_BLF)), 1e-3)
  ## e a Passada 2 so troca o lag: as demais colunas de O+1 do plugin_inference_features seguem intactas (CONSERTO A)
  p2 <- data.table::copy(r2$inf$dt_D1)[order(Hora)]
  p2[, CargaGlobal_anterior_BLF := comp2]
  expect_identical(p2[, cols, with = FALSE], r2$inf$dt_D1[order(Hora)][, cols, with = FALSE])
})

## ---- AC6: window / contract / completude -------------------------------------------------------------------------------
test_that("[PLUGINS] infer_contrato: [AC6] historia curta => window; sem load_mwh => contract; furo antes do corte => completude; nunca silencioso", {
  expect_hv <- function(e, cls, exit) {
    expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, cls); expect_identical(e$exit_code, exit)
  }
  pan <- b$panel
  short <- data.table::copy(pan); short$load <- pan$load[lubridate::as_date(interval_start, tz = b$cuts$tz) >= O - 2L]
  e <- fail(lgbm_fx_infer(b, short)); expect_hv(e, "window", 5L)
  ## plugin_inference_features direto: frame com 2 dias completos antes de O
  frame <- P$plugin_ingest(pan, b$cuts)
  fr2 <- frame[Data >= O - 2L]; attributes(fr2) <- c(attributes(fr2)[c("names", "row.names", "class", ".internal.selfref")],
                                                     attributes(frame)[c("feriados", "area")])
  expect_hv(fail(P$plugin_inference_features(fr2, b$art$payload)), "window", 5L)
  nol <- data.table::copy(pan); nol$load <- data.table::copy(pan$load)[, load_mwh := NULL]
  e <- fail(lgbm_fx_infer(b, nol)); expect_hv(e, "contract", 2L); expect_match(conditionMessage(e), "load_mwh")
  hole <- data.table::copy(pan); hole$load <- pan$load[format(interval_start, "%H:%M") != "05:00" |
                                                         lubridate::as_date(interval_start, tz = b$cuts$tz) != O]
  e <- fail(lgbm_fx_infer(b, hole)); expect_hv(e, "completude", 3L)
  ## feature do artefato ausente na matriz do Stage A / da Passada 2 => contract (nunca preenche com 0)
  pay <- b$art$payload; pay$colunas_entrada_BLF <- c(pay$colunas_entrada_BLF, "Feature_inexistente")
  inf <- inf_direct(b)$inf
  e <- fail(P$plugin_infer(pay, inf, b$cuts)); expect_hv(e, "contract", 2L); expect_match(conditionMessage(e), "Feature_inexistente")
  art <- P$plugin_thaw(b$art$payload, b$cuts)
  e <- fail(P$lgbm_predict_d1(art$booster, inf$dt_D1[, setdiff(names(inf$dt_D1), "Mes_5"), with = FALSE], rep(1000, 48), art))
  expect_hv(e, "contract", 2L); expect_match(conditionMessage(e), "Mes_5")
  ## G-ART no estagio de features: artefato sem seed/sazonalidade => contract
  bad <- b$art$payload; bad$seed <- NULL
  expect_hv(fail(P$plugin_inference_features(frame, bad)), "contract", 2L)
  ## CONTROL: o painel completo do leitor roda ate a entrega; globalenv e .Random.seed do chamador intactos
  set.seed(7); seed0 <- .Random.seed; g0 <- ls(globalenv(), all.names = TRUE); s0 <- search()
  env <- lgbm_fx_infer(b)
  expect_identical(nrow(env$delivery), 77L); expect_true(all(is.finite(env$delivery$y_pred)))
  expect_identical(.Random.seed, seed0); expect_identical(ls(globalenv(), all.names = TRUE), g0); expect_identical(search(), s0)
  r <- inf_direct(b)
  expect_identical(.Random.seed, seed0)
  expect_identical(r$out$delivery$y_pred, env$delivery$y_pred)
  expect_identical(env$layout, "lgbm_day_48"); expect_identical(env$model_id, "lgbm")
})

test_that("[PLUGINS] infer_contrato: [AC6] Stage B sem carga real em O: so as razoes do Stage A, aviso ASCII e perfil finito", {
  r <- inf_direct(b)
  art <- P$plugin_thaw(b$art$payload, b$cuts)
  dn <- data.table::copy(r$inf$dt_D); P$lgbm_normalise_dt(dn, art$min_max_BLF)
  pA <- P$lgbm_stage_a(art$booster, as.matrix(dn[, art$colunas_entrada_BLF, with = FALSE]), art$best_iter)
  d0 <- data.table::copy(r$inf$dt_D)[order(Hora)]
  d0[, `:=`(CargaGlobal = NA_real_, is_synthetic = TRUE)]
  w <- NULL
  out <- withCallingHandlers(P$lgbm_stage_b(pA, d0, art), warning = function(x) { w <<- conditionMessage(x); invokeRestart("muffleWarning") })
  expect_match(w, "sem carga real"); expect_true(grepl("^[ -~]*$", w))
  expect_length(out, 48L); expect_true(all(is.finite(out)))
  ## CONTROL: com carga real o aviso nao aparece e as 19 primeiras posicoes sao a carga real (disable_loess)
  expect_silent(P$lgbm_stage_b(pA, data.table::copy(r$inf$dt_D)[order(Hora)], art))
  real <- P$lgbm_stage_b(pA, data.table::copy(r$inf$dt_D)[order(Hora)], art)
  expect_lte(max(abs(real[1:19] - r$inf$dt_D[order(Hora)]$CargaGlobal[1:19])), 1e-9)
  expect_gt(max(abs(real[20:48] - out[20:48])), 1e-6)
})
