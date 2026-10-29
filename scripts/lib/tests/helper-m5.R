## helper-m5.R -- fixtures comuns das suites do M5 (M5.1 cria; M5.2+ acrescentam). Auto-carregado por testthat.
## Nao e codigo de produto. Sem variaveis de ambiente; tudo em tempdir; datas por lubridate com tz explicito.

## state_dir temporario com `artifacts/` vazio; limpeza ao fim do frame `.local_envir` do chamador.
pc5_state_tmp <- function(.local_envir = parent.frame()) {
  d <- tempfile("pc5-state-")
  dir.create(file.path(d, "artifacts"), recursive = TRUE)
  withr::defer(unlink(d, recursive = TRUE), envir = .local_envir)
  d
}

## ctx de fixture via arquivo de config em tempdir (nunca por env): lista com `state_dir`, `config_path`.
pc5_ctx <- function(state, ..., .local_envir = parent.frame()) {
  cfg <- tempfile("pc5-config-", fileext = ".yaml")
  yaml::write_yaml(list(state_dir = state, ...), cfg)
  withr::defer(unlink(cfg), envir = .local_envir)
  c(list(state_dir = state, config_path = cfg), list(...))
}

## Espiao de chamadas de uma etapa do plugin (envolve descriptor$stages[[etapa]]$fn); le `p$spy$calls`.
## Preserva os formals da etapa (o check de assinatura enxerga a funcao real, nao um `...`).
pc5_spy <- function(plugin, stage) {
  spy <- new.env(); spy$calls <- 0L
  orig <- plugin$descriptor$stages[[stage]]$fn
  w <- orig
  body(w) <- quote({
    spy$calls <- spy$calls + 1L
    do.call(orig, mget(names(formals(sys.function()))))
  })
  environment(w) <- list2env(list(spy = spy, orig = orig), parent = environment(orig))
  plugin$descriptor$stages[[stage]]$fn <- w
  plugin$spy <- spy
  plugin
}

## Substitui `file.rename` no env informado (o do chamador por padrao) por uma versao que falha (devolve FALSE)
## ou delega ao original; desfeito ao fim do frame do chamador.
pc5_mock_rename <- function(fail = TRUE, env = parent.frame()) {
  had <- exists("file.rename", envir = env, inherits = FALSE)
  old <- if (had) get("file.rename", envir = env, inherits = FALSE) else NULL
  assign("file.rename", function(from, to) if (fail) FALSE else base::file.rename(from, to), envir = env)
  withr::defer(if (had) assign("file.rename", old, envir = env) else rm("file.rename", envir = env), envir = env)
  invisible(NULL)
}

## Copia do plugin carregado com uma etapa removida (`drop`) ou formals trocados (`formals_of`: etapa -> nomes).
## A copia e em memoria (o loader recusa a copia em disco antes do check wire-time); o original fica intacto.
pc5_plugin_copy <- function(plugin, drop = character(), formals_of = list()) {
  p <- plugin
  for (s in drop) {
    if (s %in% names(p$descriptor$stages)) p$descriptor$stages[[s]] <- NULL else p$descriptor[[s]] <- NULL
  }
  for (s in names(formals_of)) {
    fn <- if (s %in% names(p$descriptor$stages)) p$descriptor$stages[[s]]$fn else p$descriptor[[s]]
    nm <- formals_of[[s]]
    fm <- rep(list(quote(expr = )), length(nm)); names(fm) <- nm
    formals(fn) <- fm
    if (s %in% names(p$descriptor$stages)) p$descriptor$stages[[s]]$fn <- fn else p$descriptor[[s]] <- fn
  }
  p
}

## sha256 da arvore (caminho relativo + conteudo, ordem radix): identico <=> arvore identica.
pc5_tree_hash <- function(dir) {
  fs <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  fs <- fs[order(fs, method = "radix")]
  hs <- vapply(fs, function(f) digest::digest(file = file.path(dir, f), algo = "sha256"), "")
  digest::digest(paste(fs, hs, collapse = "\n"), algo = "sha256", serialize = FALSE)
}
