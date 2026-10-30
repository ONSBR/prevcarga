## receipts.R -- recibos de gate por bundle (M8.3). Ferramenta de DEV (fora do bundle). Sem variavel de ambiente.
##
## Um recibo diz "o gate <g> passou/falhou sobre ESTE bundle" e e chaveado pelo sha256 do arquivo do bundle:
##   tests/.receipts/<sha256>.<gate>.json   (gitignored; consumido pelo release-pack e pelo agregador do A/B)
## Campos: gate, sha256, platform, runtime_id (do RUNTIME.json do bundle extraido), resultado (PASS|FAIL),
## veredito (CORTE-OK|CORTE-BLOQUEADO|null), data (UTC, ISO-8601 com Z; lubridate).
## Argumento invalido e erro de programacao do chamador (stopifnot), nao contrato de usuario. Requer runtime.R so para ler o runtime_id.

.pc_receipt_sha <- function(path) digest::digest(file = path, algo = "sha256")

## bundle_path = tarball do bundle; extracted_root = raiz extraida (onde mora RUNTIME.json) para ler o runtime_id.
## `runtime_id` pode ser informado direto (NULL => calculado de <extracted_root>/RUNTIME.json via runtime.R, se carregado).
pc_receipt_write <- function(gate, bundle_path, platform, resultado, veredito = NULL, dir = "tests/.receipts",
                             extracted_root = NULL, runtime_id = NULL) {
  stopifnot(is.character(gate), length(gate) == 1L, nzchar(gate), grepl("^[a-z0-9._-]+$", gate),
            is.character(platform), length(platform) == 1L,
            resultado %in% c("PASS", "FAIL"),
            is.null(veredito) || veredito %in% c("CORTE-OK", "CORTE-BLOQUEADO"),
            file.exists(bundle_path))
  if (is.null(runtime_id) && !is.null(extracted_root)) {
    rj <- file.path(extracted_root, "RUNTIME.json")
    if (!file.exists(rj)) stop("pc_receipt_write: RUNTIME.json ausente em ", extracted_root, call. = FALSE)
    runtime_id <- prevcarga_runtime_id(rj)
  }
  sha <- .pc_receipt_sha(bundle_path)
  rec <- list(gate = gate, sha256 = sha, platform = platform, runtime_id = runtime_id,
              resultado = resultado, veredito = veredito,
              data = format(lubridate::now(tzone = "UTC"), "%Y-%m-%dT%H:%M:%SZ"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(dir, paste0(sha, ".", gate, ".json"))
  tmp <- tempfile(pattern = ".receipt-", tmpdir = dir, fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(rec, tmp, auto_unbox = TRUE, null = "null", pretty = TRUE)
  if (!file.rename(tmp, out)) stop("pc_receipt_write: rename falhou para ", out, call. = FALSE)
  invisible(out)
}

## NULL se nao existe recibo para (sha, gate); senao a lista do recibo.
pc_receipt_read <- function(sha, gate, dir = "tests/.receipts") {
  f <- file.path(dir, paste0(sha, ".", gate, ".json"))
  if (!file.exists(f)) return(NULL)
  jsonlite::read_json(f, simplifyVector = FALSE)
}
