## control-perturb.R -- CONTROL do braco cross-platform do gate da VM Windows (M8.11): copia uma entrega do `forecast` e altera +0,2% um y_pred,
## regerando o sha256 do arquivo no manifest.json (a integridade passa; o valor fica fora da tolerancia entre-plataformas => `verificar` exit 12).
## Roda pelo Rscript.exe do BUNDLE INSTALADO (arrow, jsonlite e digest vem do bundle). Sem variavel de ambiente.
##   Rscript.exe --vanilla control-perturb.R --saida=<entrega> --out=<dir novo>
args <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { m <- grep(paste0("^--", k, "="), args, value = TRUE); if (length(m)) sub(paste0("^--", k, "="), "", m[[1L]]) else stop("control-perturb.R: falta --", k, call. = FALSE) }
saida <- opt("saida"); out <- opt("out")
stopifnot(dir.exists(saida), !dir.exists(out))
dir.create(out, recursive = TRUE)
stopifnot(all(file.copy(list.files(saida, full.names = TRUE, all.files = TRUE, no.. = TRUE), out, recursive = TRUE)))
mf <- jsonlite::read_json(file.path(out, "manifest.json"))
paths <- vapply(mf$arquivos, function(a) a$path, "")
alvo <- if ("SIN.parquet" %in% paths) "SIN.parquet" else paths[grepl("\\.parquet$", paths)][[1L]]
p <- file.path(out, alvo)
x <- arrow::read_parquet(p)
x$y_pred[[1L]] <- x$y_pred[[1L]] * 1.002
arrow::write_parquet(x, p)
for (i in seq_along(mf$arquivos)) if (identical(mf$arquivos[[i]]$path, alvo)) mf$arquivos[[i]]$sha256 <- digest::digest(file = p, algo = "sha256")
jsonlite::write_json(mf, file.path(out, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
cat(alvo, "\n", sep = "")
