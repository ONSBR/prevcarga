## scripts/lib/cli/features.R - verbo auto-registrado `features` (M6.9; dono M6; descoberto por scripts/lib/cli.R, barrel por descoberta).
##
## Passthrough fino da FE-fachada do M5 (prevcarga_build_features, etapa de inferencia): materializa a feature_table de UM modelo x UMA
## folha na origem O, sem treinar nem inferir (nunca chama prevcarga_train/prevcarga_infer/estagios training e inference). O artefato
## so e LIDO do registry (o estagio inference_features exige o payload; ausente => state.artifact_missing, exit 13). Painel e cortes
## vem do leitor unico e do dates.R pelos mesmos passos da inferencia (infer_read_panel + cortes da celula); o ajuste de hora do
## modelo e do despachante (stage_input), nunca daqui. Saida: <state_dir>/scratch/features/<m>/<a>/features.parquet (escrita
## atomica) + caminho e dimensoes no result. Modelo ou area fora da topologia => exit 4. --model e --origin sao flags globais.
## Requer errors.R, config.R (da fachada) e topology.R ... inference.R (lista em `modules`).

.prevcarga_ft_opt <- function(args, name) {
  v <- args[[name]]
  if (is.null(v) || !length(v) || identical(v, FALSE) || (length(v) == 1L && is.na(v))) NULL else as.character(v)[[1L]]
}

.prevcarga_ft_table <- function(x, model) {
  if (data.table::is.data.table(x)) x <- as.data.frame(x)
  if (is.data.frame(x)) return(x)
  tab <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(tab)) prevcarga_abort("contract.features_not_tabular", where = model)
  tab
}

.prevcarga_ft_write <- function(tab, ctx, model, area) {
  dir <- file.path(ctx$state_dir, "scratch", "features", model, area)
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE))
    prevcarga_abort("io.out_unwritable", where = dir, details = list(reason = "nao criou o diretorio"))
  dst <- file.path(dir, "features.parquet")
  tmp <- tempfile(".features-", tmpdir = dir, fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch({ arrow::write_parquet(tab, tmp); TRUE }, error = function(e) FALSE)
  if (!ok || !file.rename(tmp, dst))
    prevcarga_abort("io.out_unwritable", where = dst, details = list(reason = "escrita atomica falhou"))
  normalizePath(dst, winslash = "/", mustWork = FALSE)
}

.prevcarga_ft_text <- function(r)
  c(sprintf("features  model=%s  area=%s  origin=%s  linhas=%d  colunas=%d", r$model, r$area, r$origin, r$linhas, r$colunas),
    paste0("saida: ", r$saida),
    paste0("colunas: ", paste(r$nomes, collapse = " ")),
    "OK")

cmd_features <- function(ctx, args) {
  model <- .prevcarga_ft_opt(args, "--model")
  area <- .prevcarga_ft_opt(args, "--area")
  if (is.null(model)) prevcarga_abort("config.usage", where = "--model")
  if (is.null(area)) prevcarga_abort("config.usage", where = "--area")
  prevcarga_require_data_dir(ctx)
  topo <- if (is.null(ctx$topology_path)) prevcarga_topology(root = ctx$root) else
    prevcarga_topology(path = ctx$topology_path, root = ctx$root)
  mem <- if (is.null(ctx$topology_path)) prevcarga_members(root = ctx$root) else
    prevcarga_members(path = ctx$topology_path, root = ctx$root)
  if (!model %in% mem$mdir) prevcarga_abort("topology.model_undeclared", where = model)
  if (!area %in% topo$leaves) prevcarga_abort("topology.area_undeclared", where = area)
  member <- list(sigla = mem$sigla[mem$mdir == model][[1L]], mdir = model, exogena = mem$exogena[mem$mdir == model][[1L]])
  origin <- prevcarga_resolve_origin(ctx)$origin
  if (is.null(ctx$weather_issue_cutoff)) ctx$weather_issue_cutoff <- "available_until"

  plugin <- prevcarga_plugin_load(model, prevcarga_models_root(ctx$root))
  prevcarga_plugin_check_signature(plugin)
  vp <- plugin$descriptor$variant_param
  res <- prevcarga_infer_resolve(member, area, ctx, version = NULL, pin = FALSE,
                                 needs_variant = is.character(vp) && length(vp) == 1L && !is.na(vp))
  art <- res$artifact
  spec <- art$spec_efetivo
  cuts <- .prevcarga_inf_cuts(plugin, spec, origin, ctx, model)
  rp <- prevcarga_infer_read_panel(plugin, spec, cuts, area, origin, ctx)
  plugin$spec <- spec
  m <- .prevcarga_plugin_stage_input(rp$panel, rp$cuts, plugin$descriptor, spec, "infer")
  feats <- .prevcarga_plugin_guarded(plugin, {
    frame <- plugin$descriptor$stages$ingestion$fn(m$panel, m$cuts)
    prevcarga_build_features(plugin, frame, m$cuts, spec, ctx, stage = "infer", artifact = art$fit)
  })
  tab <- .prevcarga_ft_table(feats, model)
  out <- list(model = model, area = area, origin = format(origin, "%Y-%m-%d"), version = res$version,
              saida = .prevcarga_ft_write(tab, ctx, model, area),
              linhas = nrow(tab), colunas = ncol(tab), nomes = as.list(names(tab)))
  if (identical(ctx$format, "json")) out else .prevcarga_ft_text(out)
}

prevcarga_verb_features <- list(
  name    = "features",
  usage   = "prevcarga features --model=<m> --area=<a>",
  options = character(),   # --model e --origin sao globais; --area ja consta no bloco Options (validate-deck): a usage a exige
  handler = function(ctx, args) cmd_features(ctx, args),
  owner   = "M6",
  exempt  = FALSE,
  modules = c("topology.R", "provenance.R", "deck_reader.R", "dates.R", "validation.R", "features.R", "registry.R", "inference.R")
)
