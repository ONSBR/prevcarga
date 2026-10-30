## freeze.R -- freeze/thaw do plugin LGBM sobre o contrato comum do P.0d (LGBM.3). SO definicoes.
## O artefato e DADO PURO: o booster vira texto (save_model_to_string, a mesma selecao de arvores do lgb.save do oraculo,
## utils.R:339), ASCII com LF, com sha256 gravado. Sem saveRDS, lgb.save, relogio, caminho ou lgb.Booster no artefato; a
## persistencia (bytes, layout v_<hash>) e do registry (M5). O despachante carimba (prevcarga_plugin_stamp) o payload de
## `lgbm_freeze`; `plugin_freeze` e a forma completa que ja devolve o payload carimbado (cortes ISO, train_origin).

.lgbm_sha256 <- function(txt) digest::digest(txt, algo = "sha256", serialize = FALSE)

## Numero de arvores do texto do modelo (linhas "Tree=<i>").
lgbm_n_trees <- function(booster_txt) sum(grepl("^Tree=[0-9]+$", strsplit(booster_txt, "\n", fixed = TRUE)[[1L]]))

.lgbm_plain_df <- function(x) {
  if (is.null(x)) return(NULL)
  out <- as.data.frame(x)
  attr(out, ".internal.selfref") <- NULL
  out
}

.lgbm_iso <- function(d) as.character(d)

## Payload puro a partir do modelo em memoria. Assinatura canonica do despachante: (model).
lgbm_freeze <- function(model) {
  if (!inherits(model$booster, "lgb.Booster")) lgbm_abort("lgbm.contract", "modelo sem lgb.Booster para congelar")
  txt <- model$booster$save_model_to_string(num_iteration = NULL)
  txt <- gsub("\r\n", "\n", paste(txt, collapse = "\n"), fixed = TRUE)
  if (!is.character(txt) || length(txt) != 1L || is.na(iconv(txt, "UTF-8", "ASCII")))
    lgbm_abort("lgbm.contract", "booster_txt nao e ASCII")
  fb <- model$sazonalidade_fallbacks
  list(booster_txt = txt, booster_sha256 = .lgbm_sha256(txt), best_iter = as.integer(model$best_iter),
       train_mode = model$train_mode, cv_nfolds = model$cv_nfolds, min_max_BLF = model$min_max_BLF,
       sazonalidade = .lgbm_plain_df(model$sazonalidade),
       sazonalidade_fallbacks = if (is.null(fb)) NULL else lapply(fb, .lgbm_plain_df),
       colunas_entrada_BLF = model$colunas_entrada_BLF, colunas_nao_normalizar = model$colunas_nao_normalizar,
       colunas_normalizar_BLF = model$colunas_normalizar_BLF, coluna_numerica_BLF = model$coluna_numerica_BLF,
       coluna_categorica_BLF = as.integer(model$coluna_categorica_BLF), target_col = model$target_col,
       n_train_rows = as.integer(model$n_train_rows),
       train_cutoff = .lgbm_iso(model$train_cutoff), val_cutoff = .lgbm_iso(model$val_cutoff),
       feature_config = model$feature_config, params = model$params, seed = as.integer(model$seed),
       engine_version = list(lightgbm = as.character(utils::packageVersion("lightgbm"))))
}

## Forma completa: payload + carimbos de prevcarga_plugin_stamp (model_id, variant, split, granularity_min, load_label,
## timezone, weather_issue_cutoff, contract_version, train_origin). Varre a pureza antes de devolver.
plugin_freeze <- function(model, spec, cuts, ctx = list()) {
  stamp_fn <- .lgbm_foundation("prevcarga_plugin_stamp")
  scan <- .lgbm_foundation("prevcarga_plugin_scan_pure")
  if (is.null(stamp_fn) || is.null(scan)) lgbm_abort("lgbm.contract", "prevcarga_plugin_stamp indisponivel (fundacao)")
  payload <- lgbm_freeze(model)
  st <- stamp_fn(payload, list(descriptor = plugin_descriptor()), spec, cuts, ctx)$stamp
  art <- c(payload[setdiff(names(payload), names(st))], st)
  scan(art)
  art
}

## Descongela: versao do lightgbm (contract), sha256 e leitura do texto (integrity), arvores == best_iter (integrity).
## Devolve o artefato com o lgb.Booster SO em memoria. Assinatura canonica do despachante: (payload).
lgbm_thaw <- function(payload) {
  if (!is.list(payload) || !is.character(payload$booster_txt) || length(payload$booster_txt) != 1L)
    lgbm_abort("lgbm.integrity", "booster_txt ausente no artefato")
  v <- as.character(utils::packageVersion("lightgbm"))
  have <- payload$engine_version$lightgbm
  if (!identical(v, have))
    lgbm_abort("lgbm.engine_version", sprintf("lightgbm %s na sessao != %s no artefato", v, if (is.null(have)) "NA" else have))
  if (!identical(.lgbm_sha256(payload$booster_txt), payload$booster_sha256))
    lgbm_abort("lgbm.integrity", "booster_txt difere do booster_sha256 gravado (adulterado)")
  ## o lgb.load aborta o PROCESSO (terminate) em texto truncado: confere a estrutura ANTES de entregar o texto ao motor
  txt <- payload$booster_txt
  if (!grepl("^tree\nversion=", txt) || !grepl("\nend of trees\n", txt, fixed = TRUE) ||
      !grepl("\nend of parameters\n", txt, fixed = TRUE))
    lgbm_abort("lgbm.integrity", "booster_txt truncado ou ilegivel")
  if (lgbm_n_trees(txt) != payload$best_iter)
    lgbm_abort("lgbm.integrity", sprintf("numero de arvores %d != best_iter %d", lgbm_n_trees(txt), payload$best_iter))
  b <- tryCatch(lightgbm::lgb.load(model_str = txt), error = function(e) NULL)
  if (is.null(b)) lgbm_abort("lgbm.integrity", "booster_txt ilegivel")
  c(payload, list(booster = b))
}

## Forma completa: aceita o artefato carimbado do despachante (stamp + payload) ou o payload plano de plugin_freeze;
## com `cuts`, confere train_origin <= cuts$train_origin (contract). Carimbo ausente nao e conferido aqui (o despachante
## ja chamou prevcarga_plugin_check_artifact antes do estagio).
plugin_thaw <- function(artifact, cuts = NULL) {
  wrapped <- inherits(artifact, "prevcarga_plugin_artifact")
  payload <- if (wrapped) artifact$payload else artifact
  stamp <- if (wrapped) artifact$stamp else artifact
  if (!is.null(cuts) && !is.null(stamp$train_origin)) {
    to <- stamp$train_origin
    if (!inherits(to, "Date") || length(to) != 1L || is.na(to) || to > cuts$train_origin)
      lgbm_abort("lgbm.contract", sprintf("train_origin %s do artefato alem de %s", format(to), format(cuts$train_origin)))
  }
  if (!is.null(stamp$model_id) && !identical(stamp$model_id, "lgbm"))
    lgbm_abort("lgbm.contract", sprintf("artefato de outro modelo (%s)", stamp$model_id))
  lgbm_thaw(payload)
}
