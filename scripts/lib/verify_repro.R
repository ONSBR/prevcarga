## verify_repro.R -- verificacao de reprodutibilidade de duas entregas do `forecast` (M8.7; ADR-0040, ADR-0036).
##
## Pipeline ESTRITO em 4 fases, cada uma abortando por prevcarga_abort() antes da seguinte (nenhum valor e lido antes de a
## integridade passar): integridade (sha256 do manifest.json da entrega, exit 9) -> identidade (run_manifest.json, exit 2) ->
## pareamento por `path` no layout nativo (exit 2) -> valores por trilho (exit 12). Dois trilhos:
##   mesma-plataforma  : |delta| <= 1e-5 absoluto
##   entre-plataformas : |delta| <= 0,001 * |ref| relativo (piso absoluto 1e-5 quando ref == 0)
##   alvo (--alvo + --limiares assinado): |delta| <= limiar_mw do alvo
## Nao existe --tol nem --corte. Sem rede, sem env, sem relogio: so le arquivos sob `saida`/`referencia` (e, no --oficial, a
## chave sincronizada sob ctx$state_dir). Parquet so pelo leitor unico do LONG (prevcarga_read_long, evaluation.R).
## A entrega e a do M6.2/M6.7: manifest.json (arquivos[] = {path, sha256, ...}) + parquets; o run_manifest.json (provenance.R)
## carrega a identidade; sem ele a identidade nao e verificavel (contract.identity_mismatch).
## Requer errors.R, schema_long.R, provenance.R, evaluation.R, exports.R (prevcarga_export_key), jsonlite, digest, yaml, lubridate.

pc_vr_trilhos <- c("mesma-plataforma", "entre-plataformas")
pc_vr_abs_tol <- 1e-5
pc_vr_rel_tol <- 1e-3
pc_vr_key_cols <- c("area", "forecast_origin", "target_datetime", "lead")

.pc_vr_sorted <- function(x) sort(as.character(x), method = "radix")

.pc_vr_read_json <- function(f) tryCatch(jsonlite::read_json(f, simplifyVector = FALSE), error = function(e) NULL)

## run_manifest.json pelo leitor do provenance.R; ausente/ilegivel => NULL
.pc_vr_run_manifest <- function(dir) {
  if (!file.exists(file.path(dir, "run_manifest.json"))) return(NULL)
  tryCatch(prevcarga_read_run_manifest(dir), error = function(e) NULL)
}

## --- Fase 1: integridade -------------------------------------------------------------------------------
## Todo arquivo listado em manifest.json existe e tem o sha256 listado; nenhum arquivo fora da lista (manifest.json e
## run_manifest.json ficam de fora). Devolve a tabela (path, sha256) da lista, em ordem radix.
pc_vr_integrity <- function(dir) {
  mf <- file.path(dir, "manifest.json")
  man <- if (file.exists(mf)) .pc_vr_read_json(mf) else NULL
  ent <- if (is.null(man)) NULL else if (!is.null(man$arquivos)) man$arquivos else man$files
  if (is.null(ent))
    prevcarga_abort("integrity.manifest_sha", where = dir,
                    details = list(reason = "manifest.json ausente ou ilegivel", divergentes = list("manifest.json"),
                                   nao_listados = list()))
  paths <- vapply(ent, function(e) as.character(e$path), "")
  shas <- vapply(ent, function(e) as.character(e$sha256), "")
  ## SHA256SUMS na raiz = indice de transporte que o release-pack grava em cada chave e o prevcarga-sync copia (nao e conteudo da entrega)
  on_disk <- setdiff(list.files(dir, recursive = TRUE, all.files = TRUE), c("manifest.json", "run_manifest.json", "SHA256SUMS"))
  extra <- .pc_vr_sorted(setdiff(on_disk, paths))
  got <- vapply(paths, function(p) {
    f <- file.path(dir, p)
    if (!file.exists(f)) NA_character_ else digest::digest(file = f, algo = "sha256")
  }, "", USE.NAMES = FALSE)
  bad <- .pc_vr_sorted(paths[is.na(got) | got != shas])
  if (length(bad) || length(extra))
    prevcarga_abort("integrity.manifest_sha", where = if (length(bad)) bad[[1L]] else extra[[1L]],
                    details = list(entrega = dir, divergentes = as.list(bad), nao_listados = as.list(extra)))
  o <- order(paths, method = "radix")
  data.frame(path = paths[o], sha256 = shas[o], stringsAsFactors = FALSE)
}

## Limiares assinados: sha256 de yaml::as.yaml(limiares) deve bater com `sha256` do arquivo (integridade != autenticidade).
pc_vr_limiares <- function(arq, alvo) {
  if (!file.exists(arq)) prevcarga_abort("config.usage", where = "--limiares", details = list(hint = "arquivo nao existe"))
  y <- tryCatch(yaml::read_yaml(arq), error = function(e) NULL)
  if (!is.list(y) || is.null(y$limiares) || is.null(y$sha256))
    prevcarga_abort("integrity.limiares_signature", where = arq, details = list(reason = "limiares ou sha256 ausente"))
  got <- digest::digest(yaml::as.yaml(y$limiares), algo = "sha256", serialize = FALSE)
  if (!identical(got, as.character(y$sha256)))
    prevcarga_abort("integrity.limiares_signature", where = arq, details = list(esperado = as.character(y$sha256), obtido = got))
  hit <- Filter(function(l) identical(as.character(l$alvo), alvo), y$limiares)
  if (!length(hit))
    prevcarga_abort("config.usage", where = "--alvo", details = list(hint = "alvo ausente em --limiares", alvo = alvo))
  lim <- suppressWarnings(as.numeric(hit[[1L]]$limiar_mw))
  if (length(lim) != 1L || !is.finite(lim) || lim < 0)
    prevcarga_abort("config.usage", where = "--limiares", details = list(hint = "limiar_mw invalido", alvo = alvo))
  lim
}

## --- Fase 2: identidade --------------------------------------------------------------------------------
.pc_vr_norm_mv <- function(x) if (is.null(x) || !length(x)) NULL else as.list(unlist(x)[order(names(unlist(x)), method = "radix")])

.pc_vr_id_fields <- function(dir, which) {
  rm <- .pc_vr_run_manifest(dir)
  if (is.null(rm))
    prevcarga_abort("contract.identity_mismatch", where = "run_manifest.json",
                    details = list(campo = "run_manifest.json", lado = which, reason = "run_manifest.json ausente ou ilegivel"))
  mf <- .pc_vr_read_json(file.path(dir, "manifest.json"))
  list(data_hash = rm$data_hash, bundle_id = rm$bundle_id, model_versions = .pc_vr_norm_mv(rm$model_versions),
       runtime_id = if (!is.null(rm$runtime_id)) rm$runtime_id else mf$runtime_id, platform = rm$platform, rm = rm, mf = mf)
}

pc_vr_identity <- function(s, r) {
  a <- .pc_vr_id_fields(s, "saida"); b <- .pc_vr_id_fields(r, "referencia")
  for (campo in c("data_hash", "bundle_id", "model_versions")) {
    x <- a[[campo]]; y <- b[[campo]]
    if (is.null(x) || is.null(y) || !identical(x, y))
      prevcarga_abort("contract.identity_mismatch", where = campo,
                      details = list(campo = campo, saida = x, referencia = y))
  }
  avisos <- if (!identical(a$runtime_id, b$runtime_id)) "repro.runtime_id_differs" else character()
  list(platforms = list(saida = a$platform, referencia = b$platform), avisos = avisos, saida = a, referencia = b)
}

## Trilho default: plataformas conhecidas e iguais -> mesma; senao entre. Forcar entre com plataformas iguais e recusado.
pc_vr_trilho <- function(trilho, platforms) {
  same <- !is.null(platforms$saida) && !is.null(platforms$referencia) && identical(platforms$saida, platforms$referencia)
  if (is.null(trilho)) return(if (same) "mesma-plataforma" else "entre-plataformas")
  if (!trilho %in% pc_vr_trilhos)
    prevcarga_abort("config.usage", where = paste0("--trilho=", trilho), details = list(hint = "mesma-plataforma ou entre-plataformas"))
  if (identical(trilho, "entre-plataformas") && same)
    prevcarga_abort("config.verify_trilho_forced", where = "--trilho", details = list(plataforma = platforms$saida))
  trilho
}

## --- Fase 3: pareamento --------------------------------------------------------------------------------
pc_vr_pair <- function(s, r, sem_membros = FALSE) {
  lst <- function(d) {
    p <- pc_vr_integrity_paths(d)
    if (isTRUE(sem_membros)) p <- p[!grepl("(^|/)membros/", p)]
    p
  }
  ps <- lst(s); pr <- lst(r)
  miss <- .pc_vr_sorted(setdiff(pr, ps)); more <- .pc_vr_sorted(setdiff(ps, pr))
  if (length(miss) || length(more))
    prevcarga_abort("contract.pairing", where = if (length(miss)) miss[[1L]] else more[[1L]],
                    details = list(ausentes = as.list(miss), a_mais = as.list(more)))
  lapply(.pc_vr_sorted(ps), function(p) list(path = p, saida = file.path(s, p), referencia = file.path(r, p)))
}

pc_vr_integrity_paths <- function(dir) {
  man <- .pc_vr_read_json(file.path(dir, "manifest.json"))
  ent <- if (!is.null(man$arquivos)) man$arquivos else man$files
  vapply(ent, function(e) as.character(e$path), "")
}

## --- Fase 4: valores -----------------------------------------------------------------------------------
## tol = list(trilho = <mesma-plataforma|entre-plataformas|alvo>, limiar_mw = <dbl, so no alvo>)
pc_vr_within <- function(delta, ref, tol) {
  lim <- switch(tol$trilho,
    "mesma-plataforma"  = rep(pc_vr_abs_tol, length(ref)),
    "entre-plataformas" = pmax(pc_vr_rel_tol * abs(ref), ifelse(ref == 0, pc_vr_abs_tol, 0)),
    "alvo"              = rep(tol$limiar_mw, length(ref)))
  abs(delta) <= lim
}

pc_vr_values <- function(s, r, tol, ctx) {
  a <- prevcarga_read_long(s, ctx); b <- prevcarga_read_long(r, ctx)
  keep <- c(pc_vr_key_cols, "y_pred")
  j <- merge(a[keep], b[keep], by = pc_vr_key_cols, suffixes = c(".s", ".r"), all = TRUE)
  if (anyNA(j$y_pred.s) || anyNA(j$y_pred.r))
    prevcarga_abort("repro.key_mismatch", where = s,
                    details = list(so_saida = sum(is.na(j$y_pred.r)), so_referencia = sum(is.na(j$y_pred.s))))
  d <- j$y_pred.s - j$y_pred.r
  ok <- pc_vr_within(d, j$y_pred.r, tol)
  data.frame(path = NA_character_, pior_delta = if (length(d)) max(abs(d)) else 0, n_fora = as.integer(sum(!ok)),
             stringsAsFactors = FALSE)
}

## --- Resolucao da referencia oficial -------------------------------------------------------------------
## Chave (origem, available_until, versao) do run_manifest.json da saida (manifest.json como complemento) ->
## <state_dir>/forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/ (layout do M6.7; formatacao por prevcarga_export_key, lubridate+tz).
pc_vr_resolve_oficial <- function(saida, ctx) {
  rm <- .pc_vr_run_manifest(saida)
  mf <- .pc_vr_read_json(file.path(saida, "manifest.json"))
  pick <- function(...) for (k in c(...)) {
    v <- if (!is.null(rm[[k]])) rm[[k]] else mf[[k]]
    if (is.character(v) && length(v) == 1L && nzchar(v)) return(v)
  }
  tz <- pick("load_tz", "timezone")
  if (is.null(tz)) tz <- ctx$tz
  origin <- pick("forecast_origin", "origin", "origem")
  au <- pick("available_until")
  versao <- pick("versao")
  if (is.null(tz) || is.null(origin) || is.null(au) || is.null(versao))
    prevcarga_abort("state.official_forecast_missing", where = saida,
                    details = list(reason = "chave (origem, available_until, versao) incompleta no run_manifest.json"))
  t <- lubridate::parse_date_time(au, orders = c("YmdHMSz", "YmdHMz", "YmdHMS", "YmdHM"), tz = tz, quiet = TRUE)
  if (is.na(t))
    prevcarga_abort("state.official_forecast_missing", where = saida, details = list(reason = "available_until ilegivel", valor = au))
  key <- prevcarga_export_key(lubridate::ymd(origin, tz = tz), t, versao, tz)
  dir <- file.path(ctx$state_dir, key$rel)
  if (!dir.exists(dir))
    prevcarga_abort("state.official_forecast_missing", where = dir, details = list(esperado = dir))
  dir
}

## --- Orquestracao --------------------------------------------------------------------------------------
## `compare` e injetavel (espiao nos testes): compare(arquivo_saida, arquivo_referencia, tol, ctx) -> data.frame(pior_delta, n_fora).
prevcarga_verify_repro <- function(saida, referencia, trilho = NULL, alvo = NULL, limiares = NULL, sem_membros = FALSE, ctx,
                                   compare = pc_vr_values) {
  if (xor(is.null(alvo), is.null(limiares)))
    prevcarga_abort("config.usage", where = if (is.null(alvo)) "--alvo" else "--limiares",
                    details = list(hint = "--alvo e --limiares andam juntos"))
  pc_vr_integrity(saida); pc_vr_integrity(referencia)                  # exit 9 antes de qualquer valor
  lim <- if (!is.null(limiares)) pc_vr_limiares(limiares, alvo)         # exit 9 se a assinatura nao confere
  idn <- pc_vr_identity(saida, referencia)                              # exit 2; runtime_id -> aviso
  trilho <- pc_vr_trilho(trilho, idn$platforms)                         # exit 8 se forcado indevidamente
  pares <- pc_vr_pair(saida, referencia, sem_membros)                   # exit 2 ausente/a mais
  tol <- if (is.null(lim)) list(trilho = trilho, valor = if (identical(trilho, "mesma-plataforma")) pc_vr_abs_tol else pc_vr_rel_tol,
                                origem = trilho)
         else list(trilho = "alvo", limiar_mw = lim, valor = lim, origem = paste0("alvo:", alvo))
  if (is.null(ctx$tz)) {
    tz <- if (!is.null(idn$saida$rm$load_tz)) idn$saida$rm$load_tz else idn$saida$mf$timezone
    if (is.null(tz)) prevcarga_abort("contract.identity_mismatch", where = "load_tz", details = list(campo = "load_tz"))
    ctx$tz <- tz
  }
  linhas <- lapply(pares, function(p) {
    d <- compare(p$saida, p$referencia, tol, ctx)
    data.frame(path = p$path, pior_delta = as.numeric(d$pior_delta), n_fora = as.integer(d$n_fora), stringsAsFactors = FALSE)
  })
  arquivos <- if (length(linhas)) do.call(rbind, linhas)
              else data.frame(path = character(), pior_delta = numeric(), n_fora = integer(), stringsAsFactors = FALSE)
  res <- list(resultado = "PASS", trilho = trilho, tol = tol$valor, origem_tol = tol$origem, arquivos = arquivos,
              avisos = idn$avisos)
  if (any(arquivos$n_fora > 0L)) {
    res$resultado <- "FAIL"
    prevcarga_abort("repro.value_out_of_tol", where = arquivos$path[arquivos$n_fora > 0L][[1L]],
                    details = list(trilho = trilho, tol = tol$valor), result = res)
  }
  res
}
