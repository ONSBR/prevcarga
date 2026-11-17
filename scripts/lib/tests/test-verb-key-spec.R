## test-verb-key-spec.R -- .prevcarga_tr_key_spec (E5-T4) tem de existir com os modulos do PROPRIO verbo (E8-T4).
## Regressao: a funcao vivia so em training.R, mas inference.R/forecast.R a chamam; deck-inference, features, infer-model e
## validate-deck nao carregam training.R e davam generic.unexpected. Cada verbo e carregado so com o seu `modules`, sem training.R.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
VERBS <- c("deck-inference", "features", "infer-model", "validate-deck", "forecast")
BASE <- c("errors", "runtime", "config")

vk_modules <- function(verb) {
  e <- new.env(parent = globalenv())
  sys.source(file.path(pc_root(), "scripts", "lib", "cli", paste0(verb, ".R")), envir = e)
  obj <- get(paste0("prevcarga_verb_", gsub("-", "_", verb)), envir = e)
  sub("\\.R$", "", obj$modules)
}
vk_root <- function() {
  rt <- tempfile("vk-root-"); dir.create(file.path(rt, "models"), recursive = TRUE)
  mk <- function(nm, desc) {
    vr <- pc_toy_variant("toy-30", run_yaml = list(backtest = list(protocol_default = "fixed"), params = list(k = 3L, tag = "MED")),
                         descriptor = desc, files = list("R/zz_dispatch.R" = .pc_toy_dispatch_lines("toy-30", native_min = 0L)), name = nm)
    file.copy(file.path(vr, nm), file.path(rt, "models"), recursive = TRUE)
  }
  sch <- list(k = list(type = "integer", min = 1L), tag = list(type = "character"))
  mk("toy-plain", list(uses_rng = TRUE, variant_param = "tag", params_schema = sch))
  mk("toy-kp", list(uses_rng = TRUE, variant_param = "tag", params_schema = sch, key_params = list(k = "1")))
  file.copy(file.path(pc_toy_root(), "_toy-engine"), file.path(rt, "models"), recursive = TRUE)
  rt
}

test_that("a funcao mora em registry.R e nao em training.R", {
  rl <- readLines(file.path(pc_root(), "scripts", "lib", "registry.R"))
  tl <- readLines(file.path(pc_root(), "scripts", "lib", "training.R"))
  expect_true(any(grepl("^\\.prevcarga_tr_key_spec <- function", rl)))
  expect_false(any(grepl("^\\.prevcarga_tr_(key_spec|overrides|variant) <- function", tl)))
})

for (v in VERBS) test_that(paste0("verbo ", v, ": key_spec resolve com os modulos do proprio verbo"), {
  mods <- vk_modules(v)
  if (!identical(v, "forecast")) expect_false("training.R" %in% paste0(mods, ".R"))
  e <- pc_source_lib(c(BASE, setdiff(mods, BASE)))
  rt <- vk_root()
  ctx <- list(root = rt)
  expect_true(is.function(e$.prevcarga_tr_key_spec))
  expect_null(e$.prevcarga_tr_key_spec(list(mdir = "toy-plain", exogena = NA_character_), ctx))
  sp <- e$.prevcarga_tr_key_spec(list(mdir = "toy-kp", exogena = "MED"), ctx)
  expect_false(is.null(sp))
  expect_identical(sp$params$k, 3L)
})
