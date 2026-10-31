## write-receipt.R -- grava um recibo de gate (formato de scripts/tools/receipts.R) a partir do gate da VM Windows (M8.11).
## Roda pelo Rscript.exe do BUNDLE INSTALADO (nao ha R na VM alem dele); usa so pacotes do bundle (digest, jsonlite, lubridate).
## Sem variavel de ambiente. Uso (todos obrigatorios, exceto os dois ultimos):
##   Rscript.exe --vanilla write-receipt.R --repo=<pacote do gate: scripts\tools\receipts.R> [--code=<raiz com scripts\lib\runtime.R>] --gate=<g> --bundle=<arquivo> --platform=<p> --resultado=PASS|FAIL
##       --runtime-json=<RUNTIME.json do bundle extraido> --dir=<tests\.receipts> [--veredito=CORTE-OK|CORTE-BLOQUEADO] [--extras=<json>]
## `--extras` e um arquivo JSON (objeto) cujos campos entram no recibo alem dos comuns (shells, bytes_gt_127, passo_falho, ...).
## Imprime o caminho do recibo e sai 0; erro de uso ou de escrita sai != 0 (stop).
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k, default = NULL) {
  m <- grep(paste0("^--", k, "="), args, value = TRUE)
  if (length(m)) sub(paste0("^--", k, "="), "", m[[1L]]) else default
}
need <- function(k) { v <- opt(k); if (is.null(v) || !nzchar(v)) stop("write-receipt.R: falta --", k, call. = FALSE); v }
repo <- need("repo"); gate <- need("gate"); bundle <- need("bundle"); platform <- need("platform")
resultado <- need("resultado"); rj <- need("runtime-json"); dir <- need("dir")
veredito <- opt("veredito"); extras <- opt("extras"); code <- opt("code", repo)

e <- new.env()
sys.source(file.path(code, "scripts", "lib", "runtime.R"), envir = e)   # o bundle instalado (code\) traz runtime.R; receipts.R e so do repo/pacote
sys.source(file.path(repo, "scripts", "tools", "receipts.R"), envir = e)
stopifnot(file.exists(rj))
rid <- e$prevcarga_runtime_id(rj)
out <- e$pc_receipt_write(gate, bundle, platform, resultado, veredito = veredito, dir = dir, runtime_id = rid)
if (!is.null(extras) && nzchar(extras)) {
  r <- jsonlite::read_json(out)
  x <- jsonlite::read_json(extras)
  for (k in names(x)) r[k] <- list(x[[k]])
  jsonlite::write_json(r, out, auto_unbox = TRUE, null = "null", pretty = TRUE)
}
cat(normalizePath(out, winslash = "/"), "\n", sep = "")
