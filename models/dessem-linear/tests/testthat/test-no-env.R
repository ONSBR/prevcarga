## test-no-env.R -- DL.8 (AC15 do epico): o plugin nao le variavel de ambiente (config = flags > config.yaml > default).
## Ambiente sujo (EXOGENA=MAX, DESSEM_EXOG_TAIL_FILL=0, como no oraculo) => saida identical() a limpa, treino com o mesmo
## sha256, `.Random.seed` do chamador intacto, e nenhum estagio de infer/forecast tem o formal `params` (tudo vem do artefato).

test_that("[PLUGINS] no_env: infer com ambiente sujo e identical() ao limpo; .Random.seed do chamador intacto; sem formal params", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); p <- dl_plugin()
  clean <- dl_infer(art, st, cuts)
  set.seed(11); r0 <- get(".Random.seed", globalenv())
  withr::local_envvar(EXOGENA = "MAX", DESSEM_EXOG_TAIL_FILL = "0")
  expect_identical(Sys.getenv("EXOGENA"), "MAX")
  dirty <- dl_infer(art, st, cuts)
  r1 <- get(".Random.seed", globalenv())
  expect_identical(r1, r0)
  expect_identical(dirty, clean)
  expect_identical(dirty$variant, "MED")                               # a variante e a do artefato, nao a do ambiente
  expect_identical(dirty$values, clean$values)
  ## formals: nenhum estagio de inferencia/forecast tem `params`; a assinatura passa pela conferencia do loader
  for (fn in c("plugin_infer", "dl_inference_features", "dl_inference", "dl_forecast", "plugin_forecast"))
    expect_false("params" %in% names(formals(get(fn, envir = p$env))), info = fn)
  expect_true(dl_lib$prevcarga_plugin_check_signature(p))
})

test_that("[PLUGINS] no_env: treino com ambiente sujo tem o mesmo sha256 e a variante do spec (nao a do ambiente)", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); base <- dl_base_art()
  withr::local_envvar(EXOGENA = "MAX", DESSEM_EXOG_TAIL_FILL = "0")
  dirty <- dl_train(st, cuts)                                          # treino real (sem memoizacao)
  expect_identical(dl_sha(dirty), dl_sha(base))
  expect_identical(dirty$stamp$variant, "MED")
  expect_identical(dirty$payload$model_key, "Linear.TmpMedia")
})
