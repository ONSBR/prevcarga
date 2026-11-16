## test-registry-key-params.R -- descriptor$key_params e chave do registry por parametro (E5-T4). Descriptor e ctx sinteticos em
## tempdir (plugin-brinquedo toy-60 com variantes), nenhum plugin real, nenhum nome de modelo de producao.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "provenance", "topology", "deck_reader", "validation", "registry", "features"), env = environment())

kp_reset <- function() { prevcarga_registry_reset(); prevcarga_plugin_unload_all() }
kp_err <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)

## raiz de projeto sintetica: <root>/models/toy-kp (variantes MED/UNI; clima_hist opcional com default observado)
kp_schema <- list(exogena = list(type = "character", allowed = c("MED", "UNI")), clima_hist = list(type = "character"))
kp_ctx <- function(descriptor = list(), params = list(exogena = "MED")) {
  d <- list(params_schema = kp_schema, variant_param = "exogena", key_params = list(clima_hist = "observado"),
            variants = list(UNI = list(ignores_params = "clima_hist")))
  for (n in names(descriptor)) d[n] <- list(descriptor[[n]])       # troca por campo (sem mesclar recursivamente)
  d <- d[!vapply(d, is.null, NA)]
  src <- pc_toy_variant("toy-60", run_yaml = list(params = params), descriptor = d, name = "toy-kp")
  root <- tempfile("kproot-"); dir.create(file.path(root, "models"), recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), file.path(root, "models"), recursive = TRUE)
  st <- tempfile("kpstate-"); dir.create(st)
  list(root = root, state_dir = st)
}
kp_member <- function(exo = "MED") list(mdir = "toy-kp", exogena = exo)
kp_spec <- function(...) list(params = list(...))

test_that("[ac-6] key_params invalido => plugin.bad_descriptor nomeando key_params$<param>", {
  bad <- list(
    "key_params$fora" = list(fora = "x"),                                   # param fora do params_schema
    "key_params$clima_hist" = list(clima_hist = 1L),                        # default nao character
    "key_params$clima_hist" = list(clima_hist = c("a", "b")),               # default de tamanho 2
    "key_params$clima_hist" = list(clima_hist = NA_character_),             # default NA
    "key_params" = c("clima_hist"))                                         # nao e lista nomeada
  for (i in seq_along(bad)) {
    kp_reset()
    ctx <- kp_ctx(list(key_params = bad[[i]]))
    e <- kp_err(prevcarga_plugin_load("toy-kp", file.path(ctx$root, "models")))
    expect_false(is.null(e))
    expect_identical(e$id, "plugin.bad_descriptor")
    expect_identical(e$exit_code, 2L)
    expect_true(grepl(names(bad)[[i]], conditionMessage(e), fixed = TRUE), info = names(bad)[[i]])
  }
  ## key_params num plugin sem o param no schema
  kp_reset()
  ctx <- kp_ctx(list(params_schema = list(exogena = kp_schema$exogena), variants = list()))
  e <- kp_err(prevcarga_plugin_load("toy-kp", file.path(ctx$root, "models")))
  expect_identical(e$id, "plugin.bad_descriptor")
  expect_true(grepl("key_params$clima_hist", conditionMessage(e), fixed = TRUE))
})

test_that("[ac-6] descriptor sem key_params normaliza com list() e a chave com spec == a chave sem spec", {
  kp_reset()
  ctx <- kp_ctx(list(key_params = NULL, variants = list(), params_schema = list(exogena = kp_schema$exogena)))
  d <- prevcarga_plugin_load("toy-kp", file.path(ctx$root, "models"))$descriptor
  expect_identical(d$key_params, list())
  m <- kp_member()
  expect_identical(prevcarga_registry_key(m, "SP", ctx, spec = kp_spec(exogena = "MED", clima_hist = "previsto_lead0")),
                   prevcarga_registry_key(m, "SP", ctx))
})

test_that("[ac-3] spec NULL e spec com default => caminho identico ao atual; fora do default => segmento entre variante e area", {
  kp_reset(); ctx <- kp_ctx(); m <- kp_member()
  base <- prevcarga_registry_key(m, "SP", ctx)
  expect_identical(basename(dirname(base)), "MED")
  expect_identical(prevcarga_registry_key(m, "SP", ctx, spec = NULL), base)
  expect_identical(prevcarga_registry_key(m, "SP", ctx, spec = kp_spec(exogena = "MED")), base)
  expect_identical(prevcarga_registry_key(m, "SP", ctx, spec = kp_spec(exogena = "MED", clima_hist = "observado")), base)
  lead <- prevcarga_registry_key(m, "SP", ctx, spec = kp_spec(exogena = "MED", clima_hist = "previsto_lead0"))
  expect_identical(lead, file.path(dirname(base), "clima_hist-previsto_lead0", "SP"))
  ## sem nome de modelo no corpo
  body <- paste(deparse(body(prevcarga_registry_key)), collapse = "\n")
  expect_false(grepl("clima_hist|dessem|random-forest|lgbm|holt-winters", body))
})

test_that("[ac-3] variante com ignores_params => sem segmento; membro sem variante ignora so se declarado", {
  kp_reset(); ctx <- kp_ctx()
  uni <- prevcarga_registry_key(kp_member("UNI"), "SP", ctx)
  expect_identical(prevcarga_registry_key(kp_member("UNI"), "SP", ctx,
                                          spec = kp_spec(exogena = "UNI", clima_hist = "previsto_lead0")), uni)
  expect_identical(basename(dirname(uni)), "UNI")
})

test_that("[ac-3] dois artefatos em chaves diferentes, cada um com latest.txt proprio, e cell_key aceita o segmento", {
  kp_reset(); ctx <- kp_ctx(); m <- kp_member()
  k0 <- prevcarga_registry_key(m, "SP", ctx)
  k1 <- prevcarga_registry_key(m, "SP", ctx, spec = kp_spec(exogena = "MED", clima_hist = "previsto_lead0"))
  pub <- function(key, v, x) {
    prevcarga_registry_publish(list(a = x), list(note = v), key, v, ctx = ctx)
    prevcarga_registry_promote_latest(key, v, ctx = ctx)
  }
  pub(k0, "v_aaaaaaaaaaaaaaaa", 1); pub(k1, "v_bbbbbbbbbbbbbbbb", 2)
  expect_identical(prevcarga_resolve_version(k0, "latest"), "v_aaaaaaaaaaaaaaaa")
  expect_identical(prevcarga_resolve_version(k1, "latest"), "v_bbbbbbbbbbbbbbbb")
  expect_identical(prevcarga_load_artifact(k0, "v_aaaaaaaaaaaaaaaa"), list(a = 1))
  expect_identical(prevcarga_load_artifact(k1, "v_bbbbbbbbbbbbbbbb"), list(a = 2))
  ## novo treino no modo default nao mexe no latest do outro modo
  pub(k0, "v_cccccccccccccccc", 3)
  expect_identical(prevcarga_resolve_version(k0, "latest"), "v_cccccccccccccccc")
  expect_identical(prevcarga_resolve_version(k1, "latest"), "v_bbbbbbbbbbbbbbbb")
  expect_identical(prevcarga_registry_cell_key(ctx, "toy-kp/MED/clima_hist-previsto_lead0", "SP"), k1)
  expect_identical(prevcarga_registry_cell_key(ctx, "toy-kp/MED", "SP"), k0)
  e <- kp_err(prevcarga_registry_cell_key(ctx, "toy-kp/MED/clima_hist-previsto_lead0", "RJ"))
  expect_identical(e$id, "state.cell_missing")
})

test_that("[ac-3] param de key_params ausente do run.yaml nao aborta plugin.bad_run_yaml; presente e conferido pelo tipo", {
  kp_reset(); ctx <- kp_ctx(params = list(exogena = "MED"))
  p <- prevcarga_plugin_load("toy-kp", file.path(ctx$root, "models"))
  expect_identical(p$spec$params, list(exogena = "MED"))
  ## o default nunca e injetado (o spec lido continua so com o que o run.yaml declara)
  expect_null(p$spec$params$clima_hist)
  ## param obrigatorio (fora de key_params) ausente continua abortando; tipo errado do opcional tambem
  sch <- kp_schema
  e1 <- kp_err(prevcarga_plugin_check_params(list(clima_hist = "observado"), sch, "toy-kp", optional = "clima_hist"))
  expect_identical(e1$id, "plugin.bad_run_yaml"); expect_true(grepl("params$exogena", conditionMessage(e1), fixed = TRUE))
  e2 <- kp_err(prevcarga_plugin_check_params(list(exogena = "MED", clima_hist = 3L), sch, "toy-kp", optional = "clima_hist"))
  expect_identical(e2$id, "plugin.bad_run_yaml"); expect_true(grepl("params$clima_hist", conditionMessage(e2), fixed = TRUE))
  expect_true(prevcarga_plugin_check_params(list(exogena = "MED"), sch, "toy-kp", optional = "clima_hist"))
  e3 <- kp_err(prevcarga_plugin_check_params(list(exogena = "MED"), sch, "toy-kp"))
  expect_identical(e3$id, "plugin.bad_run_yaml")
})
