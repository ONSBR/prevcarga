## helper-sync.R -- helper dos testes do prevcarga-sync (M8.6; auto-carregado por testthat::test_dir, depois do helper-install.R).
## Nao e codigo de produto. NUNCA fala com bucket real: o "bucket" e um diretorio local servido por um servidor HTTP de teste
## (httpuv num processo R filho, 127.0.0.1, porta livre). Sem credencial e sem variavel de ambiente: o utilitario roda sob `env -i`.
## Tudo em sandbox/ (gitignored, via pci_work_dir) e apagado ao fim.

PCS_TZ <- "Etc/GMT+3"
PCS_LEAVES <- c("A1", "A2", "B1")
PCS_SS <- c("SS1", "SS2")
PCS_CELLS <- c("random-forest/A1", "lgbm/A1", "lgbm/exo1/A1", "holt-winters/A1",   # 2 classes de split, 1 com variante
               paste0("combina/", PCS_LEAVES), paste0("reconcile/", PCS_SS))

## libs do produto usadas para MONTAR o bucket com o release-pack real (o mesmo caminho do M8.5)
pcs_env <- local({
  e <- NULL
  function() {
    if (is.null(e)) {
      lib <- file.path(pci_root(), "scripts", "lib")
      options(prevcarga.lib = lib)
      e <<- new.env(parent = globalenv())
      for (m in c("errors", "topology", "provenance", "registry", "exports", "release_pack")) sys.source(file.path(lib, paste0(m, ".R")), envir = e)
    }
    e
  }
})

pcs_put <- function(path, txt) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeBin(charToRaw(txt), path); path }
pcs_sha <- function(f) digest::digest(file = f, algo = "sha256")

## Raiz de produto minima (topologia A1 A2 B1 / SS1 SS2) para o release-pack.
pcs_root <- function(dir) {
  dir.create(file.path(dir, "config"), recursive = TRUE)
  writeLines(c("schema_version: 1", "khat_range: [0.1, 5.0]", "subsystems:",
               "  SS1: {areas: [A1, A2], loss: P1}", "  SS2: {areas: [B1], loss: P2}",
               "order: [SS1, SS2]", "members:",
               "  - {sigla: RF, mdir: random-forest, exogena: null}",
               "  - {sigla: LGBM, mdir: lgbm, exogena: null}",
               "  - {sigla: HW, mdir: holt-winters, exogena: null}"), file.path(dir, "config", "topology.yaml"))
  dir
}

## Uma celula do registry local: <state>/artifacts/<cell>/{latest.txt, v_<hash>/{artifact.rds, run_manifest.json, SHA256SUMS}}.
pcs_cell <- function(state, cell, hex, payload = paste("rds", cell, hex)) {
  v <- paste0("v_", hex)
  d <- file.path(state, "artifacts", cell, v)
  pcs_put(file.path(d, "artifact.rds"), payload)
  pcs_put(file.path(d, "run_manifest.json"), paste0("{\"cell\":\"", cell, "\"}"))
  fs <- c("artifact.rds", "run_manifest.json")
  pcs_put(file.path(d, "SHA256SUMS"), paste0(vapply(file.path(d, fs), pcs_sha, ""), "  ", fs, "\n", collapse = ""))
  pcs_put(file.path(state, "artifacts", cell, "latest.txt"), paste0(v, "\n"))
  invisible(v)
}

## Entrega local de forecast (a chave vem de prevcarga_export_key, como no release-pack).
pcs_delivery <- function(state, origin = "2026-01-15", available_until = "2026-01-15 09:00:00", versao = "0123456789ab") {
  e <- pcs_env()
  og <- lubridate::ymd(origin)
  au <- lubridate::ymd_hms(available_until, tz = "UTC")
  rel <- e$prevcarga_export_key(og, au, versao, PCS_TZ)$rel
  d <- file.path(state, rel)
  pcs_put(file.path(d, "forecast.csv"), "area,valor\nA1,1\n")
  pcs_put(file.path(d, "report", "resumo.txt"), paste("entrega", rel))
  pcs_put(file.path(d, "run_manifest.json"),
          jsonlite::toJSON(list(origin = origin, load_tz = PCS_TZ, available_until = format(au, "%Y-%m-%dT%H:%M:%SZ"), versao = versao), auto_unbox = TRUE))
  rel
}

## Monta um bucket LOCAL com `prevcarga_release_pack(what = "artifacts", latest = TRUE)` (+ `forecasts`) sobre um registry de fixture.
## `forecasts`: o release-pack ainda nao publica o ponteiro forecasts/<O>/latest.txt (uma `<available_until>/<versao>` por linha, mutavel);
## o fixture o escreve a mao (ver o discovery do M8.6).
pc_fixture_bucket <- function(dir = NULL, forecasts = TRUE, env = parent.frame()) {
  e <- pcs_env()
  base <- if (is.null(dir)) pci_work_dir(env, "pc-bucket-") else dir
  root <- pcs_root(file.path(base, "root"))
  state <- file.path(base, "state"); dir.create(state)
  for (i in seq_along(PCS_CELLS)) pcs_cell(state, PCS_CELLS[[i]], sprintf("%012x", 4096L + i))
  ctx <- list(root = root, state_dir = state, run_id = "pcs-fixture")
  pub <- tempfile("pub-"); writeLines(character(), pub); on.exit(unlink(pub), add = TRUE)
  bucket <- file.path(base, "bucket")
  e$prevcarga_release_pack("artifacts", bucket, pub, ctx, latest = TRUE)
  keys <- character()
  if (isTRUE(forecasts)) {
    rel1 <- pcs_delivery(state, available_until = "2026-01-15 09:00:00")
    rel2 <- pcs_delivery(state, available_until = "2026-01-15 21:00:00", versao = "ba9876543210")
    fo <- file.path(base, "fo")
    e$prevcarga_release_pack("forecasts", fo, pub, ctx, origin = "2026-01-15")
    for (f in list.files(file.path(fo, "forecasts"), recursive = TRUE, all.files = TRUE)) {
      dst <- file.path(bucket, "forecasts", f); dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      stopifnot(file.copy(file.path(fo, "forecasts", f), dst))
    }
    keys <- sub("^forecasts/2026-01-15/", "", c(rel1, rel2))
    pcs_put(file.path(bucket, "forecasts", "2026-01-15", "latest.txt"), paste0(paste(keys, collapse = "\n"), "\n"))
  }
  list(dir = bucket, state = state, root = root, base = base, origin = "2026-01-15", forecast_keys = keys)
}

## --- servidor HTTP de teste -------------------------------------------------------------------------------------------
## mode: "ok"        serve o diretorio como esta
##       "cut_after" apos `n` objetos servidos (404 nao conta) toda requisicao falha no servidor (500 / conexao derrubada)
##       "tamper"    troca 1 byte (1o) de todo objeto cujo caminho casa `target` (default: tudo menos SHA256SUMS e latest.txt)
## Devolve list(url, stop, served): `served()` = caminhos servidos (log do processo filho).
pc_serve_bucket <- function(dir, mode = c("ok", "cut_after", "tamper"), n = 3L, target = NULL, env = parent.frame()) {
  mode <- match.arg(mode)
  port <- httpuv::randomPort()
  logf <- tempfile("pcs-served-")
  bg <- callr::r_bg(function(dir, mode, n, port, target, logf) {
    k <- 0L
    httpuv::runServer("127.0.0.1", port, list(call = function(req) {
      rel <- utils::URLdecode(sub("^/", "", req$PATH_INFO))
      f <- file.path(dir, rel)
      if (!file.exists(f) || dir.exists(f)) return(list(status = 404L, headers = list("Content-Type" = "text/plain"), body = "nao existe"))
      k <<- k + 1L
      cat(rel, "\n", file = logf, append = TRUE, sep = "")
      b <- readBin(f, "raw", file.size(f))
      if (mode == "cut_after" && k > n) stop("conexao cortada")
      hit <- if (is.null(target)) !grepl("(SHA256SUMS|latest\\.txt)$", rel) else grepl(target, rel)
      if (mode == "tamper" && hit && length(b)) b[1] <- as.raw(bitwXor(as.integer(b[1]), 1L))
      list(status = 200L, headers = list("Content-Type" = "application/octet-stream"), body = b)
    }))
  }, args = list(dir = normalizePath(dir), mode = mode, n = as.integer(n), port = port, target = target, logf = logf), supervise = TRUE)
  url <- sprintf("http://127.0.0.1:%d", port)
  for (i in seq_len(100L)) {                       # espera o servidor aceitar conexao (ate ~10 s)
    ok <- tryCatch({ s <- suppressWarnings(socketConnection("127.0.0.1", port, open = "r+", blocking = TRUE, timeout = 1)); close(s); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) break
    Sys.sleep(0.1)
  }
  stopper <- function() if (bg$is_alive()) bg$kill()
  withr::defer(stopper(), envir = env)
  list(url = url, port = port, stop = stopper,
       served = function() if (file.exists(logf)) readLines(logf, warn = FALSE) else character())
}

## Porta fechada (nada escutando): o curl recusa a conexao => sync.network.
pc_dead_url <- function() {
  port <- httpuv::randomPort()
  sprintf("http://127.0.0.1:%d", port)
}

## --- utilitario instalado de teste ----------------------------------------------------------------------------------------
## JSON canonico (mesmo formato do CLI real) do conjunto oficial do fixture: membros RF, LGBM, LGBM/exo1 (variante) e HW; folhas A1 A2 B1.
PCS_MODELS_JSON <- paste0(
  '{"ok":true,"verb":"models","exit_code":0,"error":null,"warnings":[],"result":{"schema_version":1,"members":[',
  '{"sigla":"RF","mdir":"random-forest","exogena":null,"split":"origin_agnostic","granularity_min":60,"horizon_days":9,"history_days":{"train":365,"infer":45}},',
  '{"sigla":"LGBM","mdir":"lgbm","exogena":null,"split":"origin_agnostic","granularity_min":30,"horizon_days":1,"history_days":{"train":730,"infer":45}},',
  '{"sigla":"LGBMX","mdir":"lgbm","exogena":"exo1","split":"origin_agnostic","granularity_min":30,"horizon_days":1,"history_days":{"train":730,"infer":45}},',
  '{"sigla":"HW","mdir":"holt-winters","exogena":null,"split":"origin_scoped","granularity_min":30,"horizon_days":8,"history_days":{"train":730,"infer":45}}',
  ']},"run_id":"x","version":"0.0.0-dev"}')
PCS_AREAS_JSON <- paste0(
  '{"ok":true,"verb":"areas","exit_code":0,"error":null,"warnings":[],"result":{"schema_version":1,"sin":"SIN","order":["SS1","SS2"],"subsystems":[',
  '{"name":"SS1","loss":"P1","khat_range":[1,1.15],"areas":["A1","A2"]},{"name":"SS2","loss":"P2","khat_range":[1,1.15],"areas":["B1"]}]},"run_id":"x","version":"0.0.0-dev"}')

## Instala o utilitario como o install.sh o faz: <d>/bin/prevcarga-sync (copia do .sh do repo, ou `script`), <d>/bin/prevcarga (CLI de
## teste que imprime o JSON canonico) e <d>/bin/error-catalog.tsv (o catalogo GERADO). `catalog = FALSE` omite o catalogo (fallback embutido).
## Devolve o caminho do utilitario.
pc_sync_install <- function(env = parent.frame(), script = file.path(pci_root(), "install", "prevcarga-sync.sh"), catalog = TRUE,
                            models_json = PCS_MODELS_JSON, areas_json = PCS_AREAS_JSON) {
  d <- pci_work_dir(env, "pc-sync-bin-")
  bin <- file.path(d, "bin"); dir.create(bin)
  file.copy(script, file.path(bin, "prevcarga-sync"))
  Sys.chmod(file.path(bin, "prevcarga-sync"), "0755")
  writeLines(c("#!/bin/sh", "case \"$1\" in",
               paste0("  models) cat <<'EOF'\n", models_json, "\nEOF\n  ;;"),
               paste0("  areas) cat <<'EOF'\n", areas_json, "\nEOF\n  ;;"),
               "  *) exit 2;;", "esac"), file.path(bin, "prevcarga"))
  Sys.chmod(file.path(bin, "prevcarga"), "0755")
  if (isTRUE(catalog)) pci_sh(sprintf("cd %s && %s --vanilla scripts/tools/export_error_catalog.R --out %s", shQuote(pci_root()),
                                      shQuote(pci_rscript()), shQuote(file.path(bin, "error-catalog.tsv"))))
  file.path(bin, "prevcarga-sync")
}

## Roda o utilitario sob `env -i` (so PATH minimo). Devolve list(status, stdout, stderr).
pc_sync_run <- function(script, args) pci_run(script, args)

## `path  sha256` de TODOS os arquivos da arvore (ordenado radix); base da comparacao arvore == bucket e sh x ps1.
pc_listing <- function(dir) {
  if (!dir.exists(dir)) return(character())
  rel <- sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")
  vapply(rel, function(p) paste(p, pcs_sha(file.path(dir, p))), "", USE.NAMES = FALSE)
}

## Restos de staging (`.tmp-*`) em qualquer nivel da arvore.
pc_tmp_left <- function(dir) {
  if (!dir.exists(dir)) return(character())
  all <- list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)
  all[grepl("(^|/)\\.tmp-", all)]
}
