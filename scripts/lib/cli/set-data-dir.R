## scripts/lib/cli/set-data-dir.R - verbo `set-data-dir`: grava as chaves de dados (treino, inferencia, artefatos) no config.yaml.
## A escrita e de config.R (prevcarga_config_set_keys / prevcarga_set_data_dir_interactive); aqui so o registro e o texto da saida.
prevcarga_verb_set_data_dir <- list(
  name    = "set-data-dir",
  usage   = "prevcarga set-data-dir [<dir>] [--train=<dir>] [--inference=<dir>] [--artifacts=<dir>]",
  options = c("--train=<dir>          Grava data_train_dir (o deck de treino; tem de existir)",
              "--inference=<dir>      Grava data_inference_dir (o deck de inferencia)",
              "--artifacts=<dir>      Grava artifacts_dir (a raiz de artefatos)"),
  handler = function(ctx, args) {
    opt <- function(n) {
      v <- args[[n]]
      if (is.null(v) || !length(v) || identical(v, FALSE) || all(is.na(v))) NULL else as.character(v)[1L]
    }
    vals <- Filter(Negate(is.null), list(data_train_dir = opt("--train"), data_inference_dir = opt("--inference"),
                                         artifacts_dir = opt("--artifacts")))
    res <- if (length(vals)) c(prevcarga_config_set_keys(ctx, vals), list(gravado = TRUE)) else {
      base <- opt("<dir>")
      if (is.null(base)) base <- ctx$data_train_dir
      if (is.null(base)) base <- getwd()
      prevcarga_set_data_dir_interactive(ctx, base)
    }
    if (identical(ctx$format, "json")) return(c(list(schema_version = 1L), res))
    c(vapply(res$changes, function(x) sprintf("%s: %s -> %s", x$key, if (is.null(x$old)) "-" else x$old, x$new), ""),
      if (isTRUE(res$gravado)) c(sprintf("gravado em: %s", res$config_path),
                                 "confira o deck com: prevcarga validate-deck --origin <AAAA-MM-DD>") else "nada gravado")
  },
  deck_roles = c("train", "inference"),   # so grava chaves: nao recusa por papel
  owner   = "M8",
  exempt  = FALSE,
  modules = character()
)
