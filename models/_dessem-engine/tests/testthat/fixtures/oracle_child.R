## Filho de referencia: roda o motor do ORACULO (vendor/_load_engine.R, so leitura) sobre o deck serializado.
## .oracle_args: <vendor_dir> <deck.rds> <saida.rds> <seed|none> <origem> <modelo>
a <- .oracle_args  # definido pelo chamador (-e); o loader do oraculo se localiza por source(), nao por --file
.libPaths(c("/home/gabri/R/library", .libPaths()))
suppressMessages({ library(data.table); library(lubridate); library(jsonlite); library(rlist); library(WeightSVM); library(e1071) })
setwd(a[1])
suppressMessages(suppressWarnings(source("_load_engine.R", chdir = TRUE)))
deck <- readRDS(a[2])
dk <- suppressMessages(TransformaDeck(deck, a[5], "10DIAS"))
cfg <- getConfigModel(a[6])
if (a[4] != "none") set.seed(as.integer(a[4]))
hor <- suppressMessages(processaModelosDMPerfil(dk, cfg))
dm <- copy(hor)[, Modelo := "M"]
sh <- desagregaModeloSH("M", dm)
saveRDS(list(seriecomp = dk$SERIECOMP, horaria = hor, semi = sh, diasprev = dk$DIASPREV), a[3])
