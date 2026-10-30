## test-m5-purity.R -- gate de pureza do M5 (M5.9; ADR-0023, ADR-0035, ADR-0038, ADR-0040; CLAUDE.md princ. 2, 4 e 7).
## Greps FALSIFICAVEIS sobre o codigo do M5: fachada de features.R (prevcarga_build_features/prevcarga_assert_stage_requires),
## training.R, inference.R e o nucleo/ciclo de vida de registry.R (so os verbos do registry; o loader de plugins e do P.0).
## Cada regra tem FORBIDDEN (zero hit na arvore real) e CONTROL (isca plantada numa copia em tempdir e acusada com
## arquivo:linha; a isca so-comentario NAO e acusada). Nada aqui le variavel de ambiente. Roda igual em Linux e Windows.
library(testthat)

pc5_lib <- function(f) file.path(pc_root(), "scripts", "lib", f)

## ---- escopo do M5: arquivo -> regex dos nomes de funcao de topo (NULL = arquivo inteiro) --------------------------------
M5_SCOPE <- list(
  "features.R"  = "^prevcarga_(build_features|assert_stage_requires)$",
  "training.R"  = NULL,
  "inference.R" = NULL,
  "registry.R"  = "^[.]?prevcarga_(registry_(root|contain|key|publish|promote|promote_latest|rollback|pin|unpin|ls|show|gc|cell_key|run)|reg_|resolve_version$|load_artifact$)")

## ---- regras (id, regex perl, isencao por substring da linha, isca) ----------------------------------------------------------
M5_PURITY_RULES <- data.table::data.table(
  id    = c("fe_compute", "name_switch", "window_literal", "window_assign",
            "env", "clock", "base_dates", "fork", "shell", "symlink", "tmp_path", "python", "network"),
  regex = c("loess\\(|wavelet|haar|m_sql",
            "switch\\(model|grepl\\(\"dessem|%in% c\\(\"",
            "\\b(1825|730|45L?)\\b",
            "horizon_days *= *[0-9]|(?<!spec\\$)granularity_min *== *[0-9]+",
            "Sys\\.getenv",
            "Sys\\.(time|Date)",
            "as\\.POSIXct|strptime|as\\.Date\\(",
            "mclapply",
            "(^|[^[:alnum:]_.])system2?\\(",
            "file\\.symlink",
            "\"/tmp",
            "python3|subprocess",
            "httr|curl::|aws\\.s3|download\\.file"),
  ## isencoes (substring da linha): `"latest.txt"` = checagem de arquivos do registry (nao e nome de modelo); `trained_at` = relogio lateral
  exempt = c(NA_character_, "\"latest.txt\"", rep(NA_character_, 3L), "trained_at", rep(NA_character_, 7L)),
  bait   = c("x <- loess(y ~ x)", "if (model == 1) switch(model, a = 1)", "n <- 1825", "h <- list(horizon_days = 7)",
             "e <- Sys.getenv(\"HOME\")", "t0 <- Sys.time()", "d <- as.Date(\"2026-10-01\")", "r <- parallel::mclapply(1:2, identity)",
             "system(\"ls\")", "file.symlink(\"a\", \"b\")", "p <- \"/tmp/x\"", "r <- system2(\"python3\", \"a.py\")",
             "u <- httr::GET(\"x\")"))

## ---- motor ---------------------------------------------------------------------------------------------------------------------
## Linhas de CODIGO de um arquivo: comentarios (inteiros ou de fim de linha) removidos pelo parser; sem comentario no texto.
pc5_code_lines <- function(f) {
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  pd <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"), includeText = NA)
  cm <- pd[pd$token == "COMMENT", ]
  for (i in seq_len(nrow(cm))) x[cm$line1[[i]]] <- substr(x[cm$line1[[i]]], 1L, cm$col1[[i]] - 1L)
  x
}

## Linhas (inicio, fim) das funcoes de topo cujo nome casa `fn_regex` (NULL = todas as linhas do arquivo).
pc5_scope_lines <- function(f, fn_regex) {
  n <- length(readLines(f, encoding = "UTF-8", warn = FALSE))
  if (is.null(fn_regex)) return(seq_len(n))
  ex <- parse(f, keep.source = TRUE, encoding = "UTF-8")
  sr <- attr(ex, "srcref")
  keep <- integer()
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && as.character(e[[1L]]) %in% c("<-", "=") && is.name(e[[2L]]) && is.call(e[[3L]]) &&
        identical(as.character(e[[3L]][[1L]]), "function") && grepl(fn_regex, as.character(e[[2L]])))
      keep <- c(keep, seq(sr[[i]][[1L]], sr[[i]][[3L]]))
  }
  keep
}

## Devolve "arquivo:linha:texto"; `scope` = lista basename -> regex de funcoes (arquivo fora da lista = inteiro).
pc5_grep <- function(regex, files, exempt = NA, scope = M5_SCOPE) {
  hits <- character()
  for (f in files) {
    x <- pc5_code_lines(f)
    sc <- if (basename(f) %in% names(scope)) scope[[basename(f)]] else NULL
    in_scope <- seq_along(x) %in% pc5_scope_lines(f, sc)
    hit <- which(in_scope & grepl(regex, x, perl = TRUE))
    if (!is.na(exempt)) hit <- hit[!grepl(exempt, x[hit], fixed = TRUE)]
    if (length(hit)) hits <- c(hits, sprintf("%s:%d:%s", basename(f), hit, trimws(x[hit])))
  }
  hits
}

M5_FILES <- vapply(c("features.R", "training.R", "inference.R", "registry.R"), pc5_lib, "")

## Copia de `src` em tempdir com `text` inserido antes do fechamento da funcao de topo `fn` (isca DENTRO do escopo).
pc5_plant <- function(src, fn, text, comment = FALSE, .local_envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = .local_envir)
  dst <- file.path(d, basename(src))
  ex <- parse(src, keep.source = TRUE, encoding = "UTF-8"); sr <- attr(ex, "srcref")
  end <- NA_integer_
  for (i in seq_along(ex)) {
    e <- ex[[i]]
    if (is.call(e) && is.name(e[[2L]]) && identical(as.character(e[[2L]]), fn)) end <- sr[[i]][[3L]]
  }
  stopifnot(!is.na(end))
  x <- readLines(src, encoding = "UTF-8", warn = FALSE)
  line <- if (comment) paste0("  # ", text) else paste0("  ", text)
  writeLines(c(x[seq_len(end - 1L)], line, x[end:length(x)]), dst, useBytes = TRUE)
  list(path = dst, line = end)
}

## ---- sanidade do escopo ----------------------------------------------------------------------------------------------------
test_that("o escopo do M5 acha as funcoes esperadas (CONTROL: escopo vazio nao vale como pureza)", {
  fe <- pc5_scope_lines(pc5_lib("features.R"), M5_SCOPE[["features.R"]])
  expect_gt(length(fe), 10L)
  expect_lt(length(fe), length(readLines(pc5_lib("features.R"))))          # nao e o arquivo inteiro (to_hourly etc. ficam fora)
  rg <- pc5_scope_lines(pc5_lib("registry.R"), M5_SCOPE[["registry.R"]])
  expect_gt(length(rg), 100L)
  expect_lt(length(rg), length(readLines(pc5_lib("registry.R"))))          # o loader de plugins (P.0) fica fora
  expect_length(pc5_scope_lines(pc5_lib("registry.R"), "^nome_que_nao_existe$"), 0L)
  for (fn in c("prevcarga_registry_gc", "prevcarga_registry_run", "prevcarga_registry_pin"))
    expect_true(any(grepl(fn, readLines(pc5_lib("registry.R"))[rg], fixed = TRUE)))
})

## ---- FORBIDDEN: um test_that por regra, vazio na arvore real ---------------------------------------------------------
for (i in seq_len(nrow(M5_PURITY_RULES))) local({
  r <- M5_PURITY_RULES[i]
  test_that(sprintf("[FORBIDDEN] %s: zero hit no codigo do M5", r$id), {
    expect_identical(pc5_grep(r$regex, M5_FILES, r$exempt), character())
  })
})

## ---- CONTROL: a isca plantada em cada arquivo e acusada, com arquivo:linha ---------------------------------------------
for (i in seq_len(nrow(M5_PURITY_RULES))) local({
  r <- M5_PURITY_RULES[i]
  test_that(sprintf("[CONTROL] %s: isca plantada em copia (tempdir) e acusada", r$id), {
    targets <- list(list(f = "features.R", fn = "prevcarga_build_features"),
                    list(f = "features.R", fn = "prevcarga_assert_stage_requires"),
                    list(f = "training.R", fn = "prevcarga_train"),
                    list(f = "inference.R", fn = "prevcarga_infer_cell"),
                    list(f = "registry.R", fn = "prevcarga_registry_gc"))
    for (t in targets) {
      p <- pc5_plant(pc5_lib(t$f), t$fn, r$bait)
      hits <- pc5_grep(r$regex, p$path, r$exempt)
      expect_length(hits, 1L)
      expect_match(hits, sprintf("^%s:%d:", t$f, p$line), info = paste(r$id, t$f))
    }
    ## a mesma isca como COMENTARIO nao conta
    pc <- pc5_plant(pc5_lib("training.R"), "prevcarga_train", r$bait, comment = TRUE)
    expect_identical(pc5_grep(r$regex, pc$path, r$exempt), character())
  })
})

test_that("[CONTROL] fora do escopo de funcoes o grep nao acusa (to_hourly em features.R); dentro acusa", {
  p <- pc5_plant(pc5_lib("features.R"), "to_hourly", "x <- loess(y ~ x)")
  expect_identical(pc5_grep(M5_PURITY_RULES[id == "fe_compute", regex], p$path), character())
  expect_length(pc5_grep(M5_PURITY_RULES[id == "fe_compute", regex], p$path, scope = list()), 1L)
})

test_that("[CONTROL] Sys.time na linha de trained_at e a unica isencao do relogio", {
  p <- pc5_plant(pc5_lib("training.R"), "prevcarga_train", "m$trained_at <- Sys.time()")
  r <- M5_PURITY_RULES[id == "clock"]
  expect_length(pc5_grep(r$regex, p$path), 1L)
  expect_identical(pc5_grep(r$regex, p$path, r$exempt), character())
})

test_that("[CONTROL] granularidade literal so e aceita na comparacao com o spec; a isencao do registry e so a linha dos arquivos", {
  rx <- M5_PURITY_RULES[id == "window_assign", regex]
  expect_false(grepl(rx, "if (spec$granularity_min == 60L) x", perl = TRUE))
  expect_true(grepl(rx, "if (granularity_min == 60L) x", perl = TRUE))
  p <- pc5_plant(pc5_lib("registry.R"), "prevcarga_registry_gc", "ok <- m %in% c(\"lgbm\", \"holt-winters\")")
  r <- M5_PURITY_RULES[id == "name_switch"]
  expect_length(pc5_grep(r$regex, p$path, r$exempt), 1L)
})

test_that("[CONTROL] o literal de janela so vale como palavra inteira (14500 / x45a nao casam; 45L casa)", {
  rx <- M5_PURITY_RULES[id == "window_literal", regex]
  expect_false(any(grepl(rx, c("n <- 14500", "x45a <- 1", "id_730x"), perl = TRUE)))
  expect_true(all(grepl(rx, c("n <- 45L", "n <- 45", "w <- 730", "w <- 1825"), perl = TRUE)))
})

## ---- dependencias: todo pacote usado pelo M5 consta do renv.lock ----------------------------------------------------------
M5_BASE_PKGS <- c("base", "stats", "utils", "methods", "parallel", "tools", "compiler", "grDevices", "graphics", "datasets")

pc5_pkgs_used <- function(files, scope = M5_SCOPE) {
  used <- character()
  for (f in files) {
    x <- pc5_code_lines(f)
    sc <- if (basename(f) %in% names(scope)) scope[[basename(f)]] else NULL
    x <- x[seq_along(x) %in% pc5_scope_lines(f, sc)]
    used <- c(used, unlist(regmatches(x, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", x, perl = TRUE))),
              unlist(regmatches(x, gregexpr("(?<=requireNamespace\\(\")[A-Za-z][A-Za-z0-9.]*", x, perl = TRUE))),
              unlist(regmatches(x, gregexpr("(?<=library\\()[A-Za-z][A-Za-z0-9.]*", x, perl = TRUE))))
  }
  sort(setdiff(unique(used), M5_BASE_PKGS), method = "radix")
}

## Pacotes declarados: `Packages` do renv.lock lido como JSON. Estrito desde o M8.1 (o repo tem renv.lock): sem lock o teste FALHA
## (nao ha mais o conjunto das bibliotecas instaladas como substituto). O CONTROL abaixo garante que o teste tem dente.
pc5_declared_pkgs <- function(lock = file.path(pc_root(), "renv.lock")) {
  if (!file.exists(lock)) stop("m5-purity: renv.lock AUSENTE (", lock, "); a checagem de dependencias do M5 e estrita", call. = FALSE)
  names(jsonlite::fromJSON(lock, simplifyVector = FALSE)$Packages)
}
pc5_pkgs_missing <- function(used, declared) setdiff(used, declared)

test_that("[FORBIDDEN] todo pacote usado (::, requireNamespace, library) pelo codigo do M5 esta declarado", {
  used <- pc5_pkgs_used(M5_FILES)
  expect_true(all(c("data.table", "lubridate") %in% used))                 # o extrator enxerga os usos reais
  expect_identical(pc5_pkgs_missing(used, pc5_declared_pkgs()), character())
})

test_that("[CONTROL] pacote fora do lock e acusado; renv.lock em JSON e lido pelo Packages", {
  p <- pc5_plant(pc5_lib("training.R"), "prevcarga_train", "x <- naoexistepkg::f(1)")
  expect_true("naoexistepkg" %in% pc5_pkgs_used(p$path))
  expect_identical(pc5_pkgs_missing("naoexistepkg", pc5_declared_pkgs()), "naoexistepkg")
  lock <- withr::local_tempfile(fileext = ".lock")
  writeLines('{"R": {"Version": "4.5.3"}, "Packages": {"data.table": {"Package": "data.table"}, "lubridate": {"Package": "lubridate"}}}', lock)
  expect_setequal(pc5_declared_pkgs(lock), c("data.table", "lubridate"))
  expect_identical(pc5_pkgs_missing(c("data.table", "digest"), pc5_declared_pkgs(lock)), "digest")
})

test_that("[CONTROL] lock ausente reprova (sem fallback para as bibliotecas instaladas); o lock do repo cobre os usos do M5", {
  expect_error(pc5_declared_pkgs(file.path(tempfile("sem-lock"), "renv.lock")), "renv.lock AUSENTE")
  expect_true(all(c("data.table", "lubridate", "jsonlite", "digest", "arrow") %in% pc5_declared_pkgs()))
})
