## test-mm-head.R -- DL.8 (AC10 do epico, item 16a): premissa do RNG preservado. O treino ajusta a cabeca DM(O) de
## REFERENCIA (com o `mm` do dia O no corte de referencia) so para consumir o gerador na posicao do oraculo e a descarta.
## Prova: (1) trocar os VALORES do mm de referencia (mesmas linhas) nao muda o sha256 do artefato => o consumo do RNG nao
## depende dos valores; (2) CONTROL: treino sem a cabeca de referencia desloca o RNG e muda os SVMs de O+1..; (3) se (1) cair,
## o teste so passa com `rng_deviation` NAO-nulo no descriptor (plano B declarado) e falha caso contrario.

## troca temporaria de uma funcao do ENV DO PLUGIN (o que o treino resolve por nome); restaura no on.exit do chamador
dl_with_plugin_fn <- function(name, fn, code, env = dl_plugin()$env) {
  orig <- get(name, envir = env, inherits = FALSE)
  assign(name, fn(orig), envir = env)
  on.exit(assign(name, orig, envir = env), add = TRUE)
  force(code)
}

test_that("[PLUGINS] mm_head: trocar os valores do mm de referencia nao muda o sha256 (RNG independe dos valores); CONTROL sem a cabeca desloca o RNG de O+1..", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); env <- dl_plugin()$env
  a <- dl_base_art(); sha_a <- dl_sha(a)
  expect_identical(a$payload$rng_mode, "preserve")
  expect_null(env$plugin_descriptor()$rng_deviation)
  spy <- dl_spy_tune(); on.exit(spy$restore(), add = TRUE)

  ## treino de referencia sob espiao: 250 ajustes (cabeca DM(O) + 9 DM + 10 x 24 perfis); a posicao do RNG em cada um
  a_spy <- dl_train(st, cuts)
  st_a <- spy$log$state
  expect_identical(spy$log$n, 250L)
  expect_identical(grep("+ mm", spy$log$fml, fixed = TRUE), 1L)      # so o 1o ajuste (a cabeca) leva o mm
  expect_identical(dl_sha(a_spy), sha_a)                              # o treino sob espiao e o do cache (determinismo)

  ## (1) mm de referencia com valores trocados (mesmas linhas, mesmo dia): a cabeca e ajustada com outros valores
  spy$reset(); n_ref <- 0L
  b <- dl_with_plugin_fn("dl_morning_means", function(real) function(...) {
    n_ref <<- n_ref + 1L
    m <- real(...)
    m[, mm := rev(mm) * 1.7 + 321]
    m[]
  }, dl_train(st, cuts))
  expect_gt(n_ref, 0L)                                                # a troca valeu: o treino leu o mm de referencia
  expect_identical(spy$log$state, st_a)                               # o gerador chega igual a cada um dos 250 ajustes
  same <- identical(dl_sha(b), sha_a)
  if (!same) {
    ## FORBIDDEN caiu: o consumo de RNG depende dos valores. So passa com o plano B ativo e declarado.
    expect_false(is.null(env$plugin_descriptor()$rng_deviation),
                 label = "RNG depende de mm: ativar o plano B (descriptor$rng_deviation) e listar no A/B (M8)")
  } else {
    expect_true(same)
  }

  ## (2) CONTROL: sem a cabeca de referencia o gerador chega deslocado a TODOS os ajustes de O em diante (perfis de O e
  ## DM/perfis de O+1..). Obs.: nos hiperparametros do CV o deck sintetico nao muda de escolha (o grid degenera em
  ## epsilon = 0.1), entao a prova do deslocamento e a posicao do RNG por ajuste, nao os bytes dos SVMs (ver docs/estado).
  spy$reset()
  c0 <- dl_with_plugin_fn("dl_dm_head", function(real) function(...) NULL, dl_train(st, cuts))
  st_c <- spy$log$state
  expect_identical(spy$log$n, 249L)                                   # 9 DM + 10 x 24 perfis: so a cabeca falta
  expect_false(any(grepl("+ mm", spy$log$fml, fixed = TRUE)))
  expect_identical(st_c[[1L]], st_a[[1L]])                            # ambos partem de set.seed(seed)
  expect_false(any(st_c == st_a[-1L]))                                # nenhum ajuste de O+1.. enxerga a mesma posicao
  expect_identical(spy$log$n - 1L, length(unique(st_c)) - 1L)         # e cada ajuste segue consumindo o gerador
})

test_that("[PLUGINS] mm_head: a cabeca de referencia so consome o gerador: nao entra no artefato; plano B reseed e declarado", {
  env <- dl_plugin()$env; a <- dl_base_art()
  expect_false("mm" %in% names(a$payload))
  expect_setequal(names(a$payload$svm_dm), format(DL8_O + 1:9))      # DM(O) nao e guardada: o infer a refaz com o mm real
  expect_setequal(names(a$payload$svm_perfil), format(DL8_O + 0:9))
  ## sem rng_deviation o modo e preserve e silencioso; com ele (plano B) e reseed com aviso classificado
  expect_identical(env$dl_rng_mode(deviation = NULL), "preserve")
  w <- NULL
  mode <- withCallingHandlers(dl_with_cli(env$dl_rng_mode(deviation = "consumo de RNG diverge do oraculo")),
                              warning = function(x) { w <<- x; invokeRestart("muffleWarning") })
  expect_identical(mode, "reseed")
  expect_identical(w$id, "warning.dessem_linear_rng_deviation")
  expect_true(dl_ascii(conditionMessage(w)))
})

test_that("[PLUGINS] mm_head: deck sensivel ao CV (400 dias): valores do mm => sha256 igual; CONTROL sem a cabeca muda os SVMs de O+1..", {
  st <- dl_fx_cv_store(); cuts <- dl_fx_cuts(st); env <- dl_plugin()$env
  a <- dl_train_memo(st, cuts); sha_a <- dl_sha(a)
  ## FORBIDDEN: valores do mm de referencia trocados (mesmas linhas)
  b <- dl_with_plugin_fn("dl_morning_means", function(real) function(...) { m <- real(...); m[, mm := rev(mm) * 1.7 + 321]; m[] },
                         dl_train(st, cuts))
  if (!identical(dl_sha(b), sha_a)) {
    expect_false(is.null(env$plugin_descriptor()$rng_deviation),
                 label = "RNG depende de mm: ativar o plano B (descriptor$rng_deviation) e listar no A/B (M8)")
  } else expect_identical(dl_sha(b), sha_a)
  ## CONTROL: sem a cabeca de referencia, os SVMs de O+1.. mudam (o CV deste deck reage a posicao do gerador)
  c0 <- dl_with_plugin_fn("dl_dm_head", function(real) function(...) NULL, dl_train(st, cuts))
  expect_false(identical(dl_sha(c0), sha_a))
  differs <- function(x, y) !identical(x, y)
  chg <- c(vapply(names(a$payload$svm_dm), function(d) differs(a$payload$svm_dm[[d]], c0$payload$svm_dm[[d]]), NA),
           vapply(setdiff(names(a$payload$svm_perfil), format(DL8_O)),
                  function(d) differs(a$payload$svm_perfil[[d]], c0$payload$svm_perfil[[d]]), NA))
  expect_true(any(chg))
})
