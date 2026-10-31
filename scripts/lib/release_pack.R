## release_pack.R -- montagem LOCAL do pacote de publicacao (M8.5; ADR-0039, ADR-0040, ADR-0030).
##
## `prevcarga_release_pack` monta um diretorio no layout do bucket publico (releases/, artifacts/, forecasts/) a partir de
## bundles do M8.1/M8.2 (runtime), do registry local (artifacts) ou das entregas locais do forecast (forecasts). Nunca fala
## rede, nunca le variavel de ambiente: so caminhos do `ctx` (root, state_dir, run_id) e dos argumentos. O envio ao bucket e
## do utilitario externo scripts/tools/upload (unico arquivo do repo que conhece o servico), que NAO decide conteudo nem calcula
## hash: confia no SHA256SUMS montado aqui.
##
## Montagem atomica: tudo vai para um stage irmao `<out>.stage-<run_id>` (mesmo volume); so com ZERO recusas o stage vira `<out>`
## por `file.rename`; qualquer erro remove o stage. `<out>` precisa estar ausente ou vazio.
## Recusas acumulam em `recusados` (path, motivo): fora_da_allowlist, proibido, chave_publicada, recibo_ausente:<gate>,
## recibo_outro_sha:<gate>, recibo_fail:<gate>, versao_divergente, runtime_json_ausente, run_manifest_ausente, chave_divergente,
## deck_manifest. Um unico erro `integrity` (exit 9) lista todas, e nada e escrito.
## Allowlist primeiro: um arquivo fora dela e `fora_da_allowlist` (inclui deck, dados e config.yaml soltos); `proibido` e o
## conteudo sensivel DENTRO de uma arvore permitida (config.yaml, credencial, parquet de entrada do deck sob v_<hash>/ ou numa chave).
## `latest.txt` e a unica chave mutavel do bucket e so e escrito com `latest = TRUE`.
## Requer errors.R, config.R (da fachada), registry.R (leitor do registry local), topology.R, exports.R (chave de forecasts),
## digest, jsonlite, lubridate.

.pc_rp_what <- c("runtime", "artifacts", "forecasts")
.pc_rp_bundle_rx <- "^prevcarga-([^/]+)-(linux-x86_64\\.tar\\.zst|windows-x86_64\\.zip)$"
.pc_rp_companion_rx <- "^prevcarga-[^/]+-(linux-x86_64\\.tar\\.zst|windows-x86_64\\.zip)\\.(sha256|RUNTIME\\.json)$"
.pc_rp_cell_rx <- "(combina|reconcile|[^/]+(/[^/]+)?)/[^/]+"

.pc_rp_allow <- list(
  runtime = c("^releases/[^/]+/prevcarga-[^/]+-(linux-x86_64\\.tar\\.zst|windows-x86_64\\.zip)$",
              "^releases/[^/]+/RUNTIME-(linux|windows)-x86_64\\.json$", "^releases/[^/]+/SHA256SUMS$",
              "^releases/latest\\.txt$", "^SHA256SUMS$"),
  artifacts = c(paste0("^artifacts/", .pc_rp_cell_rx, "/v_[0-9a-f]{12,64}/.+$"),
                paste0("^artifacts/", .pc_rp_cell_rx, "/latest\\.txt$"), "^SHA256SUMS$"),
  forecasts = c("^forecasts/[0-9]{4}-[0-9]{2}-[0-9]{2}/[0-9]{8}T[0-9]{4}/[^/]+/.+$", "^SHA256SUMS$"))

.pc_rp_forbidden <- c("(^|/)config\\.yaml$", "(^|/)(load|weather_observed|weather_forecast)[^/]*\\.parquet$",
                      "(^|/)auxiliary/", "\\.(pem|key)$", "(^|/)credentials$", "(^|/)\\.aws/", "(^|/)\\.env$")

## gates cujo recibo PASS (sha exato do bundle) e exigido por plataforma; ab-corte exige tambem veredito CORTE-OK
.pc_rp_gates <- list(`linux-x86_64` = c("install-e2e-linux", "ab-corte"),
                     `windows-x86_64` = c("install-e2e-windows", "windows-vm", "ab-corte"))

.pc_rp_norm <- function(p) gsub("\\", "/", p, fixed = TRUE)
.pc_rp_sha <- function(f) digest::digest(file = f, algo = "sha256")
.pc_rp_empty <- function() data.frame(path = character(), motivo = character(), stringsAsFactors = FALSE)
.pc_rp_rows <- function(path, motivo)
  if (!length(path)) .pc_rp_empty() else data.frame(path = path, motivo = motivo, stringsAsFactors = FALSE)
.pc_rp_bind <- function(...) do.call(rbind, c(list(.pc_rp_empty()), list(...)))
.pc_rp_files <- function(dir) {
  fs <- list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  fs[order(fs, method = "radix")]
}
.pc_rp_usage <- function(where, reason) prevcarga_abort("config.usage", where = where, details = list(reason = reason))

## --- copia (ponto unico de E/S de escrita: o teste injeta falha aqui) ----------------------------------------
.pc_rp_copy <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  isTRUE(file.copy(from, to, overwrite = FALSE))
}
.pc_rp_copy_or_die <- function(from, to) {
  if (!.pc_rp_copy(from, to)) prevcarga_abort("integrity.copy_failed", where = to, details = list(reason = "copia falhou"))
  invisible(to)
}
.pc_rp_write_bytes <- function(path, txt) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(txt)), con)
  invisible(path)
}

## --- chaves ja publicadas ---------------------------------------------------------------------------------------
## `--published`: um arquivo, uma chave por linha (a saida de `upload --list`). `latest.txt` e mutavel e nunca conta.
.pc_rp_read_published <- function(path) {
  if (is.null(path) || !length(path) || is.na(path) || !nzchar(path)) return(character())
  if (!file.exists(path)) .pc_rp_usage("--published", "arquivo inexistente")
  x <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  x <- sub("^(\\./|/)+", "", .pc_rp_norm(x[nzchar(x) & !startsWith(x, "#")]))
  x[!grepl("(^|/)latest\\.txt$", x)]
}
## `key` termina em "/": publicada se algum item publicado esta sob ela (ou e ela).
.pc_rp_is_published <- function(key, pub) any(startsWith(pub, key) | pub == sub("/$", "", key))

## --- allowlist + conteudo proibido ------------------------------------------------------------------------------
## `paths`: caminhos RELATIVOS ao <out> (separador /). Devolve data.frame(path, ok, motivo).
pc_rp_classify <- function(paths, what) {
  if (!(is.character(what) && length(what) == 1L && what %in% .pc_rp_what)) .pc_rp_usage("--what", "runtime, artifacts ou forecasts")
  paths <- .pc_rp_norm(as.character(paths))
  hit <- function(rx) vapply(paths, function(p) any(vapply(rx, function(r) grepl(r, p, perl = TRUE), NA)), NA, USE.NAMES = FALSE)
  allowed <- if (length(paths)) hit(.pc_rp_allow[[what]]) else logical()
  forb <- if (length(paths)) hit(.pc_rp_forbidden) else logical()
  motivo <- ifelse(!allowed, "fora_da_allowlist", ifelse(forb, "proibido", NA_character_))
  data.frame(path = paths, ok = is.na(motivo), motivo = motivo, stringsAsFactors = FALSE)
}

## manifest.json do DECK (insumo) nunca vai: o da entrega do forecast nao tem timezone$load nem lag de clima.
.pc_rp_is_deck_manifest <- function(file) {
  m <- tryCatch(jsonlite::read_json(file, simplifyVector = FALSE), error = function(e) NULL)
  is.list(m) && (!is.null(m$timezone$load) || !is.null(m$weather_forecast_publication_lag_hours))
}
## (o nome vem de uma constante: aqui o arquivo e so SONDADO para ser RECUSADO; quem le o manifest do deck como insumo e o leitor unico)
.pc_rp_mname <- paste0("manifest", ".json")
.pc_rp_deck_manifests <- function(src, rel) {
  i <- basename(rel) == .pc_rp_mname & vapply(src, function(f) .pc_rp_is_deck_manifest(f), NA, USE.NAMES = FALSE)
  .pc_rp_rows(rel[i], "deck_manifest")
}

## refusa as `items` (data.frame src, rel) pela allowlist/proibidos de `what` + manifest de deck
.pc_rp_refuse_items <- function(items, what) {
  cl <- pc_rp_classify(items$rel, what)
  .pc_rp_bind(.pc_rp_rows(cl$path[!cl$ok], cl$motivo[!cl$ok]), .pc_rp_deck_manifests(items$src, items$rel))
}

## --- SHA256SUMS -------------------------------------------------------------------------------------------------
## '<sha256>  <caminho relativo com />' por arquivo de `dir` (recursivo), ordenado radix, LF; exclui o proprio SHA256SUMS da raiz de
## `dir`. `keep_existing`: um SHA256SUMS ja presente (o do registry em v_<hash>/) e mantido byte a byte.
pc_rp_sha256sums <- function(dir, keep_existing = FALSE) {
  out <- file.path(dir, "SHA256SUMS")
  if (keep_existing && file.exists(out)) return("SHA256SUMS")
  fs <- setdiff(.pc_rp_files(dir), "SHA256SUMS")
  sha <- vapply(file.path(dir, fs), .pc_rp_sha, "", USE.NAMES = FALSE)
  .pc_rp_write_bytes(out, paste0(sha, "  ", fs, "\n", collapse = ""))
  "SHA256SUMS"
}

## --- ponteiros mutaveis (so com latest = TRUE) --------------------------------------------------------------------
pc_rp_latest <- function(stage, what, chaves) {
  for (k in chaves) .pc_rp_write_bytes(file.path(stage, k$path), paste0(k$value, "\n"))
  invisible(chaves)
}

## --- runtime ----------------------------------------------------------------------------------------------------
## recibos tests/.receipts/<sha>.<gate>.json (receipts.R, ferramenta de dev); lidos aqui com jsonlite.
pc_rp_check_receipts <- function(bundle, gates, receipts_dir) {
  sha <- .pc_rp_sha(bundle)
  bad <- lapply(gates, function(g) {
    f <- file.path(receipts_dir, sprintf("%s.%s.json", sha, g))
    if (!file.exists(f)) return(.pc_rp_rows(basename(bundle), paste0("recibo_ausente:", g)))
    r <- tryCatch(jsonlite::read_json(f, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(r)) return(.pc_rp_rows(basename(bundle), paste0("recibo_fail:", g)))
    if (!identical(r$sha256, sha)) return(.pc_rp_rows(basename(bundle), paste0("recibo_outro_sha:", g)))
    if (!identical(r$resultado, "PASS") || (identical(g, "ab-corte") && !identical(r$veredito, "CORTE-OK")))
      return(.pc_rp_rows(basename(bundle), paste0("recibo_fail:", g)))
    NULL
  })
  do.call(.pc_rp_bind, bad)
}

## RUNTIME.json do bundle: extraido do proprio arquivo (zip por utils::unzip, tar.zst pelo tar do sistema); se nao der, o
## `<bundle>.RUNTIME.json` ao lado (gerado pelo build). Devolve o caminho de um arquivo temporario ou NA.
.pc_rp_runtime_json <- function(bundle, td) {
  zip <- grepl("\\.zip$", bundle)
  got <- suppressWarnings(tryCatch({
    if (zip) {
      nm <- utils::unzip(bundle, list = TRUE)$Name
      nm <- nm[grepl("(^|/)RUNTIME\\.json$", nm)]
      nm <- nm[order(nchar(nm), method = "radix")]
      if (length(nm)) utils::unzip(bundle, files = nm[[1L]], exdir = td, junkpaths = TRUE)
    } else if (nzchar(Sys.which("tar"))) {
      suppressWarnings(system2("tar", c("--zstd", "-xf", shQuote(bundle), "-C", shQuote(td), "--strip-components=1",
                                        "prevcarga/RUNTIME.json"), stdout = FALSE, stderr = FALSE))
    }
    file.path(td, "RUNTIME.json")
  }, error = function(e) NA_character_))
  if (!is.na(got) && file.exists(got)) return(got)
  sib <- paste0(bundle, ".RUNTIME.json")
  if (file.exists(sib)) sib else NA_character_
}

pc_rp_runtime <- function(stage, pub, ctx, bundles, version = NULL, receipts_dir = NULL, latest = FALSE) {
  if (!is.character(bundles) || length(bundles) != 1L || is.na(bundles) || !dir.exists(bundles))
    .pc_rp_usage("--bundles", "diretorio de bundles inexistente")
  if (is.null(receipts_dir)) receipts_dir <- file.path(ctx$root, "tests", ".receipts")
  files <- .pc_rp_files(bundles)
  is_bundle <- grepl(.pc_rp_bundle_rx, files)
  companion <- grepl(.pc_rp_companion_rx, files)
  vers <- unique(sub(.pc_rp_bundle_rx, "\\1", files[is_bundle]))
  v <- if (!is.null(version)) version else if (length(vers) == 1L) vers else
    .pc_rp_usage("--release", "informe a versao: os bundles nao trazem uma unica versao")
  if (!grepl("^[A-Za-z0-9._+-]+$", v) || v %in% c(".", "..")) .pc_rp_usage("--release", "versao invalida")
  key <- paste0("releases/", v, "/")
  rec <- .pc_rp_empty()
  cand <- files[!companion]
  cl <- pc_rp_classify(paste0("releases/", v, "/", cand), "runtime")
  bad <- !cl$ok | !is_bundle[!companion]               # so bundles com nome canonico; o resto e recusado
  rec <- .pc_rp_bind(rec, .pc_rp_rows(cand[bad], ifelse(cl$ok[bad], "fora_da_allowlist", cl$motivo[bad])))
  mine <- files[is_bundle]
  other <- sub(.pc_rp_bundle_rx, "\\1", mine) != v
  rec <- .pc_rp_bind(rec, .pc_rp_rows(mine[other], "versao_divergente"))
  mine <- mine[!other]
  if (!length(mine) && !nrow(rec)) .pc_rp_usage("--bundles", "nenhum bundle prevcarga-<v>-<plataforma> no diretorio")
  if (.pc_rp_is_published(key, pub)) rec <- .pc_rp_bind(rec, .pc_rp_rows(key, "chave_publicada"))
  td <- tempfile("rp-rt-")
  dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  rj <- list()
  for (b in mine) {
    plat <- sub("\\.(tar\\.zst|zip)$", "", sub(.pc_rp_bundle_rx, "\\2", b))
    rec <- .pc_rp_bind(rec, pc_rp_check_receipts(file.path(bundles, b), .pc_rp_gates[[plat]], receipts_dir))
    tdp <- file.path(td, plat)
    dir.create(tdp)
    j <- .pc_rp_runtime_json(file.path(bundles, b), tdp)
    if (is.na(j)) rec <- .pc_rp_bind(rec, .pc_rp_rows(b, "runtime_json_ausente")) else rj[[plat]] <- j
  }
  if (nrow(rec)) return(list(id = NULL, recusados = rec, chaves = character()))
  dest <- file.path(stage, "releases", v)
  for (b in mine) .pc_rp_copy_or_die(file.path(bundles, b), file.path(dest, b))
  for (plat in names(rj)) .pc_rp_copy_or_die(rj[[plat]], file.path(dest, paste0("RUNTIME-", sub("-.*$", "", plat), "-x86_64.json")))
  pc_rp_sha256sums(dest)
  if (isTRUE(latest)) pc_rp_latest(stage, "runtime", list(list(path = "releases/latest.txt", value = v)))
  list(id = NULL, recusados = .pc_rp_empty(), chaves = key)
}

## --- artifacts --------------------------------------------------------------------------------------------------
## Celulas do registry local com `latest.txt`: `models` (siglas/mdir) filtra as de modelo; combina/ e reconcile/ entram sempre.
.pc_rp_cells <- function(ctx, models) {
  rows <- if (is.null(models)) prevcarga_registry_ls(ctx) else {
    sel <- c("combina", "reconcile", models)
    rr <- unlist(lapply(sel, function(m) prevcarga_registry_ls(ctx, model = m)), recursive = FALSE)
    for (m in models) if (!length(prevcarga_registry_ls(ctx, model = m)))
      .pc_rp_usage("--model", paste0("modelo sem celula no registry: ", m))
    rr
  }
  keys <- vapply(rows, function(r) r$key, "")
  rows <- rows[!duplicated(keys) & vapply(rows, function(r) !is.na(r$latest), NA)]
  rows[order(vapply(rows, function(r) r$key, ""), method = "radix")]
}

pc_rp_artifacts <- function(stage, pub, ctx, models = NULL, latest = FALSE) {
  root <- prevcarga_registry_root(ctx)
  cells <- .pc_rp_cells(ctx, models)
  have <- vapply(cells, function(r) r$key, "")
  topo <- prevcarga_topology(root = ctx$root)
  miss <- setdiff(c(paste0("combina/", topo$leaves), paste0("reconcile/", topo$order)), have)
  items <- list()
  rec <- .pc_rp_empty()
  for (r in cells) {
    vdir <- file.path(root, r$key, r$latest)
    fs <- .pc_rp_files(vdir)
    items[[length(items) + 1L]] <- data.frame(src = file.path(vdir, fs), rel = paste("artifacts", r$key, r$latest, fs, sep = "/"),
                                              stringsAsFactors = FALSE)
    k <- paste0("artifacts/", r$key, "/", r$latest, "/")
    if (.pc_rp_is_published(k, pub)) rec <- .pc_rp_bind(rec, .pc_rp_rows(k, "chave_publicada"))
  }
  items <- if (length(items)) do.call(rbind, items) else data.frame(src = character(), rel = character(), stringsAsFactors = FALSE)
  rec <- .pc_rp_bind(.pc_rp_refuse_items(items, "artifacts"), rec)
  if (nrow(rec)) return(list(id = NULL, recusados = rec, chaves = character()))
  if (length(miss))
    prevcarga_abort("state.official_set_incomplete", where = "artifacts",
                    details = list(faltam = as.list(miss)))
  for (i in seq_len(nrow(items))) .pc_rp_copy_or_die(items$src[[i]], file.path(stage, items$rel[[i]]))
  chaves <- vapply(cells, function(r) paste0("artifacts/", r$key, "/", r$latest, "/"), "")
  for (k in chaves) pc_rp_sha256sums(file.path(stage, k), keep_existing = TRUE)
  if (isTRUE(latest))
    pc_rp_latest(stage, "artifacts", lapply(cells, function(r)
      list(path = paste0("artifacts/", r$key, "/latest.txt"), value = r$latest)))
  list(id = NULL, recusados = .pc_rp_empty(), chaves = chaves)
}

## --- forecasts --------------------------------------------------------------------------------------------------
## Entregas locais `<state_dir>/forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/` da origem pedida, copiadas byte a byte. A chave do
## caminho tem de coincidir com a recomputada do run_manifest.json pela funcao de chave do M6.7.
.pc_rp_key_matches <- function(rmf, rel) {
  m <- tryCatch(jsonlite::read_json(rmf, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(m)) return("run_manifest_invalido")
  og <- lubridate::ymd(as.character(m$origin), quiet = TRUE)
  tz <- as.character(m$load_tz)
  au <- lubridate::ymd_hms(as.character(m$available_until), tz = "UTC", quiet = TRUE)
  if (length(og) != 1L || is.na(og) || length(au) != 1L || is.na(au) || length(tz) != 1L || !tz %in% OlsonNames() ||
      !is.character(m$versao) || length(m$versao) != 1L) return("run_manifest_invalido")
  if (identical(prevcarga_export_key(og, au, m$versao, tz)$rel, rel)) NA_character_ else "chave_divergente"
}

pc_rp_forecasts <- function(stage, pub, ctx, origin = NULL, latest = FALSE) {
  o <- if (is.null(origin)) NA else lubridate::ymd(as.character(origin), quiet = TRUE)
  if (length(o) != 1L || is.na(o)) .pc_rp_usage("--origin", "informe --origin=AAAA-MM-DD")
  day <- as.character(o)
  base <- file.path(ctx$state_dir, "forecasts", day)
  aus <- if (dir.exists(base)) list.dirs(base, recursive = FALSE, full.names = FALSE) else character()
  aus <- sort(aus[grepl("^[0-9]{8}T[0-9]{4}$", aus)], method = "radix")
  keys <- unlist(lapply(aus, function(a) {
    vs <- list.dirs(file.path(base, a), recursive = FALSE, full.names = FALSE)
    if (length(vs)) paste("forecasts", day, a, sort(vs[!startsWith(vs, ".")], method = "radix"), sep = "/")
  }))
  if (!length(keys))
    prevcarga_abort("state.nothing_to_deliver", where = "--origin", details = list(reason = "nenhuma entrega local da origem", origin = day))
  items <- list()
  rec <- .pc_rp_empty()
  for (k in keys) {
    src <- file.path(ctx$state_dir, k)
    rmf <- file.path(src, "run_manifest.json")
    if (!file.exists(rmf)) rec <- .pc_rp_bind(rec, .pc_rp_rows(paste0(k, "/"), "run_manifest_ausente"))
    else {
      bad <- .pc_rp_key_matches(rmf, k)
      if (!is.na(bad)) rec <- .pc_rp_bind(rec, .pc_rp_rows(paste0(k, "/"), bad))
    }
    fs <- .pc_rp_files(src)
    items[[length(items) + 1L]] <- data.frame(src = file.path(src, fs), rel = paste(k, fs, sep = "/"), stringsAsFactors = FALSE)
    if (.pc_rp_is_published(paste0(k, "/"), pub)) rec <- .pc_rp_bind(rec, .pc_rp_rows(paste0(k, "/"), "chave_publicada"))
  }
  items <- do.call(rbind, items)
  rec <- .pc_rp_bind(.pc_rp_refuse_items(items, "forecasts"), rec)
  if (nrow(rec)) return(list(id = NULL, recusados = rec, chaves = character()))
  for (i in seq_len(nrow(items))) .pc_rp_copy_or_die(items$src[[i]], file.path(stage, items$rel[[i]]))
  for (k in keys) pc_rp_sha256sums(file.path(stage, k), keep_existing = TRUE)
  list(id = NULL, recusados = .pc_rp_empty(), chaves = paste0(keys, "/"))
}

## --- despachante -------------------------------------------------------------------------------------------------
.pc_rp_refusal_id <- function(motivo) {
  if (any(motivo %in% c("fora_da_allowlist", "proibido", "deck_manifest"))) "integrity.release_forbidden_content"
  else if (any(motivo == "chave_publicada")) "integrity.release_key_published"
  else if (any(grepl("^recibo_", motivo))) "integrity.release_receipt"
  else "integrity.release_inconsistent"
}

prevcarga_release_pack <- function(what, out, published, ctx, version = NULL, bundles = NULL, models = NULL, origin = NULL,
                                   latest = FALSE, receipts_dir = NULL) {
  if (!is.character(what) || length(what) != 1L || is.na(what) || !(what %in% .pc_rp_what))
    .pc_rp_usage("--what", "runtime, artifacts ou forecasts")
  if (!is.character(out) || length(out) != 1L || is.na(out) || !nzchar(out)) .pc_rp_usage("--out", "diretorio de saida ausente")
  out <- sub("/+$", "", normalizePath(out, winslash = "/", mustWork = FALSE))
  if (file.exists(out) && (!dir.exists(out) || length(list.files(out, all.files = TRUE, no.. = TRUE))))
    prevcarga_abort("config.release_out_not_empty", where = out)
  pub <- .pc_rp_read_published(published)
  stage <- paste0(out, ".stage-", gsub("[^A-Za-z0-9_.-]", "_", as.character(ctx$run_id)))
  done <- FALSE
  on.exit(if (!done) unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  if (!dir.create(stage, recursive = TRUE, showWarnings = FALSE))
    prevcarga_abort("integrity.copy_failed", where = stage, details = list(reason = "nao criou o stage"))
  r <- switch(what,
    runtime   = pc_rp_runtime(stage, pub, ctx, bundles = bundles, version = version, receipts_dir = receipts_dir, latest = latest),
    artifacts = pc_rp_artifacts(stage, pub, ctx, models = models, latest = latest),
    forecasts = pc_rp_forecasts(stage, pub, ctx, origin = origin, latest = latest))
  if (nrow(r$recusados))
    prevcarga_abort(.pc_rp_refusal_id(r$recusados$motivo), where = out,
                    result = list(what = what, chaves = I(character()), recusados = r$recusados))
  fs <- .pc_rp_files(stage)                                  # autoconferencia: o que sai tem de passar na allowlist
  cl <- pc_rp_classify(fs, what)
  if (any(!cl$ok))
    prevcarga_abort("integrity.release_forbidden_content", where = out,
                    result = list(what = what, chaves = I(character()), recusados = .pc_rp_rows(cl$path[!cl$ok], cl$motivo[!cl$ok])))
  sums <- pc_rp_sha256sums(stage)
  if (dir.exists(out)) unlink(out, recursive = TRUE)
  if (!isTRUE(file.rename(stage, out)))
    prevcarga_abort("integrity.copy_failed", where = out, details = list(reason = "rename do stage falhou"))
  done <- TRUE
  list(what = what, out = out, chaves = I(as.character(r$chaves)), sha256sums = sums, latest = isTRUE(latest),
       arquivos = length(fs) + 1L, recusados = .pc_rp_empty())
}
