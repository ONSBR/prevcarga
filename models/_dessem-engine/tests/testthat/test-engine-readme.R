## test-engine-readme.R -- DL.1: README do motor mapeia cada required_function ao pkgfun_*.R:linha do oraculo (AC14).

readme_lines <- function() readLines(file.path(eng_dir(), "README.md"), warn = FALSE)
manifest_of <- function() { e <- new.env(); sys.source(file.path(eng_dir(), "R", "manifest.R"), envir = e); e$engine_manifest() }

## funcoes de `fns` sem linha `funcao -> arquivo -> pkgfun_*.R:linha` em `lines`
missing_map <- function(fns, lines) {
  fns[!vapply(fns, function(f) any(grepl(paste0("^", f, " -> [A-Za-z_]+[.]R -> pkgfun_[A-Za-z]+[.]R:[0-9]+$"), lines)), NA)]
}

test_that("ac-14 toda required_function tem linha funcao -> arquivo -> pkgfun_*.R:linha e o arquivo e do manifest", {
  m <- manifest_of()
  lines <- readme_lines()
  expect_length(missing_map(m$required_functions, lines), 0L)
  for (f in m$required_functions) {
    l <- lines[grepl(paste0("^", f, " -> "), lines)][[1L]]
    file <- strsplit(l, " -> ", fixed = TRUE)[[1L]][[2L]]
    expect_true(file %in% m$files, info = f)
    src <- readLines(file.path(eng_dir(), "R", file))
    expect_true(any(grepl(paste0("^", f, " <- function"), src)), info = f)
  }
})

test_that("ac-14 as linhas do mapa batem com o oraculo quando ele esta presente", {
  V <- "/opt/ons-source/prevcargadessem3/models/dessem-linear/vendor"
  skip_if_not(dir.exists(V), "oraculo ausente")
  for (l in grep("^[A-Za-z_.0-9]+ -> .* -> pkgfun_[A-Za-z]+[.]R:[0-9]+$", readme_lines(), value = TRUE)) {
    p <- strsplit(l, " -> ", fixed = TRUE)[[1L]]
    ref <- strsplit(p[[3L]], ":", fixed = TRUE)[[1L]]
    src <- readLines(file.path(V, ref[[1L]]))
    expect_true(grepl(paste0("^", p[[1L]], " <- function"), src[[as.integer(ref[[2L]])]]), info = l)
  }
})

test_that("ac-14 o README lista o que nao foi portado e a regra de nowcast", {
  txt <- paste(readme_lines(), collapse = "\n")
  for (k in c("aplicaPesos", "writeOutDP", "writeOutCSV", "plotOutputData", "RN_DemandaMedia", "paralelismo",
              "self-location", "kill-switches por ENV"))
    expect_true(grepl(k, txt, fixed = TRUE), info = k)
  expect_true(grepl("cabeca de nowcast e do plugin", txt, fixed = TRUE))
  expect_false(grepl("[^\\x01-\\x7F]", txt, perl = TRUE))
})

test_that("ac-14 CONTROL: remover uma linha do mapa faz o teste apontar a funcao faltante", {
  lines <- readme_lines()
  cut <- lines[!grepl("^desagregaModeloSH -> ", lines)]
  expect_identical(missing_map(manifest_of()$required_functions, cut), "desagregaModeloSH")
})
