## test-features-facade.R -- fachada prevcarga_build_features + verificacao wire-time de assinatura (M5.1).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("runtime", "errors", "provenance", "topology", "deck_reader", "validation", "dates", "registry",
                "features"), env = environment())
prevcarga_registry_reset(); prevcarga_plugin_unload_all()

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
CUTS <- pc_toy_cuts(O, lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
PANEL <- pc_sentinel_panel(O - 12L, 13L, "SE")

expect_cond <- function(expr, id, text = NULL) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  if (is.null(cond)) return(invisible(NULL))
  expect_identical(cond$id, id); expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  expect_match(conditionMessage(cond), "[prevcarga/contract]", fixed = TRUE)
  for (t in text) expect_true(grepl(t, paste(conditionMessage(cond), paste(unlist(cond$details), collapse = " "),
                                             cond$where), fixed = TRUE), info = t)
  invisible(cond)
}

toys <- function() list(pc_toy_disp("toy-30"), pc_toy_disp("toy-60"))
frame_of <- function(p) p$descriptor$stages$ingestion$fn(pc_train_slice(PANEL, CUTS), CUTS)
payload_of <- function(p) {
  d <- p$descriptor; spec <- p$spec
  d$freeze(d$stages$training$fn(d$stages$feature_engineering$fn(frame_of(p), spec$params), spec$params, spec$seed))
}

## Plugins reais presentes na arvore (models/<m>/R/plugin.R); os 5 chegam com os epicos de modelo.
real_plugins <- function() {
  root <- file.path(pc_root(), "models")
  ms <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  ms <- ms[file.exists(file.path(root, ms, "R", "plugin.R"))]
  lapply(ms, function(m) prevcarga_plugin_load(m, root))
}

test_that("ac-0 check_signature: plugins validos passam; copias fora do contrato falham antes do treino (CONTROL)", {
  for (p in c(toys(), real_plugins())) expect_silent(prevcarga_plugin_check_signature(p))
  p <- pc_toy_disp("toy-30")
  sp <- pc5_spy(p, "training")
  miss <- pc5_plugin_copy(sp, drop = "inference_features")
  expect_cond(prevcarga_plugin_check_signature(miss), "contract.plugin_signature",
              c("toy-30", "inference_features", "ausente"))
  par <- pc5_plugin_copy(sp, formals_of = list(inference = c("payload", "inf_features", "params")))
  c2 <- expect_cond(prevcarga_plugin_check_signature(par), "contract.plugin_signature",
                    c("toy-30", "inference", "params"))
  expect_true(all(utf8ToInt(conditionMessage(c2)) < 128L))
  ## acumula todas as lacunas e aborta uma vez
  both <- pc5_plugin_copy(sp, drop = "thaw", formals_of = list(freeze = c("a", "b")))
  c3 <- expect_cond(prevcarga_plugin_check_signature(both), "contract.plugin_signature")
  expect_length(c3$details$gaps, 2L)
  expect_identical(sp$spy$calls, 0L)
  ## o original nao foi alterado por nenhuma copia
  expect_silent(prevcarga_plugin_check_signature(p))
})

test_that("ac-1 guard de colunas: ausente aborta nomeando coluna e etapa; frame completo passa (CONTROL)", {
  ov <- function(d) { d$stages$ingestion$provides <- c("frame.raw", "temp_obs", "feriado")
              d$stages$feature_engineering$requires <- c("frame.raw", "temp_obs", "feriado"); d }
  root <- pc_toy_variant("toy-30", descriptor = ov)
  p <- prevcarga_plugin_load("toy-30", root)
  spec <- p$spec
  fr <- frame_of(p)
  c1 <- expect_cond(prevcarga_build_features(p, fr, CUTS, spec, list(), "train"), "contract.column_missing",
                    c("temp_obs", "feriado", "feature_engineering"))
  expect_identical(c1$details$missing, c("feriado", "temp_obs"))
  full <- c(fr, list(temp_obs = 1, feriado = 0))
  expect_silent(out <- prevcarga_build_features(p, full, CUTS, spec, list(), "train"))
  expect_true(is.list(out))
  ## o guard confere a etapa do stage pedido: infer usa o requires de inference_features
  pay <- payload_of(p)
  expect_silent(prevcarga_build_features(p, fr, CUTS, spec, list(), "infer", artifact = pay))
  expect_cond(prevcarga_build_features(p, fr, CUTS, spec, list(), "infer"), "contract.artifact_required",
              "inference_features")
  ov2 <- function(d) { d$stages$ingestion$provides <- c("frame.raw", "temp_obs")
              d$stages$inference_features$requires <- c("frame.raw", "artifact.model", "temp_obs"); d }
  p2 <- prevcarga_plugin_load("toy-30", pc_toy_variant("toy-30", descriptor = ov2))
  expect_cond(prevcarga_build_features(p2, fr, CUTS, spec, list(), "infer", artifact = pay),
              "contract.column_missing", c("temp_obs", "inference_features"))
  expect_silent(prevcarga_build_features(p2, fr, CUTS, spec, list(), "train"))
})

test_that("ac-2 fachada transparente: identical ao plugin direto, perturbacao muda a saida (CONTROL)", {
  for (p in c(toys(), real_plugins())) {
    st <- p$descriptor$stages; spec <- p$spec
    fr <- frame_of(p); pay <- payload_of(p)
    expect_identical(prevcarga_build_features(p, fr, CUTS, spec, list(), "train"),
                     st$feature_engineering$fn(fr, spec$params))
    expect_identical(prevcarga_build_features(p, fr, CUTS, spec, list(), "infer", artifact = pay),
                     st$inference_features$fn(fr, pay))
    ## CONTROL: 1 valor de load_mwh dentro do corte muda a saida da fachada (o teste nao e vacuo)
    fr2 <- fr; fr2$load <- data.table::copy(fr$load); fr2$load[100L, load_mwh := load_mwh + 1]
    expect_false(identical(prevcarga_build_features(p, fr2, CUTS, spec, list(), "train"),
                           prevcarga_build_features(p, fr, CUTS, spec, list(), "train")))
  }
})

test_that("ac-3 fachada e check_signature nao carregam nome de modelo, switch nem calculo de FE", {
  for (fn in list(prevcarga_build_features, prevcarga_plugin_check_signature, prevcarga_assert_stage_requires)) {
    src <- paste(deparse(body(fn)), collapse = "\n")
    for (tok in c("random-forest", "lgbm", "holt-winters", "dessem", "switch(", "loess", "wavelet", "m_sql"))
      expect_false(grepl(tok, src, fixed = TRUE), info = tok)
    expect_false(grepl("(switch|grepl)\\([^)]*model_id", src))
  }
  expect_identical(unname(prevcarga_fe_stage), c("feature_engineering", "inference_features"))
})

test_that("catalogo features_facade.tsv carrega sem id duplicado e resolve os 3 ids (classe contract, ASCII)", {
  cat <- prevcarga_errors_load()
  expect_false(anyDuplicated(cat$id) > 0L)
  for (id in c("contract.plugin_signature", "contract.column_missing", "contract.artifact_required")) {
    row <- cat[cat$id == id, ]
    expect_identical(nrow(row), 1L)
    expect_identical(row$class, "contract")
    expect_identical(prevcarga_exit_codes[["contract"]], 2L)
    expect_true(all(utf8ToInt(paste(row$message, row$hint)) < 128L))
  }
  raw <- readBin(file.path(pc_root(), "scripts", "lib", "errors", "features_facade.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(as.raw(13L) %in% raw)
})
