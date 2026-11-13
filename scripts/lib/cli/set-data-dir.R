## scripts/lib/cli/set-data-dir.R - verbo `set-data-dir`: grava o data_dir (o deck) no config.yaml da instalacao.
## A escrita e de config.R (prevcarga_config_set_data_dir); aqui so o registro e o texto da saida.
prevcarga_verb_set_data_dir <- list(
  name    = "set-data-dir",
  usage   = "prevcarga set-data-dir <dir>",
  options = character(),
  handler = function(ctx, args) {
    res <- prevcarga_config_set_data_dir(ctx, args[["<dir>"]])
    if (identical(ctx$format, "json")) return(c(list(schema_version = 1L), res))
    c(sprintf("data_dir: %s", res$data_dir),
      sprintf("gravado em: %s", res$config_path),
      if (!is.null(res$previous) && !identical(res$previous, res$data_dir)) sprintf("antes: %s", res$previous),
      "confira o deck com: prevcarga validate-deck --origin <AAAA-MM-DD>")
  },
  owner   = "M8",
  exempt  = FALSE,
  modules = character()
)
