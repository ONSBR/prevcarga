## test-facade-window-no-model-names.R -- teste estrutural do E2 (ADR-0009, ADR-0031): o codigo de janela da fachada e do deck de
## inferencia nao cita id nem sigla de modelo fora de comentarios. A lista vem de models/*/run.yaml (name) e de config/topology.yaml
## (members[].sigla e mdir), nunca de literal neste teste. Escopo: inference_needs.R, deck_inference.R, cli/deck-inference.R e as
## funcoes de janela prevcarga_infer_read_panel (inference.R), prevcarga_train_inputs e prevcarga_artifact_build (training.R) e
## .prevcarga_check_inference_needs (validation.R). CONTROL positivo: o regex acha o id injetado num texto de controle.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "provenance", "topology", "holiday_islands", "deck_reader", "validation",
                       "dates", "registry", "features", "inference_needs", "training", "inference"))

## ids e siglas lidos do repositorio
nm_ids <- function() {
  fs <- Sys.glob(file.path(pc_root(), "models", "*", "run.yaml"))
  ids <- vapply(fs, function(f) as.character(yaml::read_yaml(f)$name), "")
  topo <- env$prevcarga_topology(root = pc_root())
  mem <- env$prevcarga_members(root = pc_root())
  unique(c(ids, basename(dirname(fs)), as.character(mem$sigla), as.character(mem$mdir),
           as.character(topo$members$sigla)))
}
nm_rx <- function(ids) {
  ids <- ids[!is.na(ids) & nzchar(ids)]
  esc <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", ids)
  paste0("(?<![A-Za-z0-9_])(", paste(esc[order(-nchar(esc))], collapse = "|"), ")(?![A-Za-z0-9_])")
}
nm_strip <- function(x) sub("#.*$", "", x)                                   # comentario de linha e sufixo
nm_file <- function(rel) nm_strip(readLines(file.path(pc_root(), "scripts", "lib", rel), warn = FALSE))
nm_fn <- function(fn_name) {
  f <- get(fn_name, envir = env)
  nm_strip(deparse(body(f), width.cutoff = 500L))                            # deparse nao carrega comentarios; strip cobre `#` em string
}
nm_hits <- function(code, rx) code[grepl(rx, code, perl = TRUE)]

NM_FILES <- c("inference_needs.R", "deck_inference.R", file.path("cli", "deck-inference.R"))
NM_FNS <- c("prevcarga_infer_read_panel", "prevcarga_train_inputs", "prevcarga_artifact_build", ".prevcarga_check_inference_needs")

test_that("a lista de ids e siglas vem do repositorio e cobre os modelos e a topologia", {
  ids <- nm_ids()
  expect_gte(length(ids), 6L)
  expect_true(all(c("holt-winters", "lgbm", "random-forest") %in% ids))
  expect_true(all(c("HW", "LGBM", "RF") %in% ids))
})

test_that("[AC3] arquivos e funcoes de janela da fachada nao citam id nem sigla de modelo fora de comentarios", {
  rx <- nm_rx(nm_ids())
  for (rel in NM_FILES) {
    code <- nm_file(rel)
    expect_gt(length(code), 10L)
    expect_identical(nm_hits(code, rx), character(), info = rel)
  }
  for (fn in NM_FNS) {
    code <- nm_fn(fn)
    expect_gt(length(code), 3L)
    expect_identical(nm_hits(code, rx), character(), info = fn)
  }
})

test_that("[AC3] CONTROL: o regex acha o id e a sigla injetados no texto de controle; comentario nao conta", {
  ids <- nm_ids()
  rx <- nm_rx(ids)
  for (id in ids) expect_length(nm_hits(sprintf("x <- '%s'", id), rx), 1L)
  expect_length(nm_hits(nm_strip("y <- 1  # lgbm e HW so em comentario"), rx), 0L)
  expect_length(nm_hits("prevcarga_lgbm_helper <- 1", rx), 0L)                 # palavra inteira: nao casa dentro de identificador
  code <- c(nm_file("inference_needs.R"), "z <- 'random-forest'")
  expect_length(nm_hits(code, rx), 1L)                                         # injecao num arquivo real e vista
})
