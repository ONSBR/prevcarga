## test-bundle-linux.R -- conteudo do bundle Linux, renv.lock e catalogo de erros (M8.1; AC 5, 6 e 7). Gate [BUNDLE] bundle_linux.
## Braco do tarball (etc/ldpaths, library() pelo Rscript do bundle): so quando dist/ tem um candidato; senao skip com motivo.

e <- bb_env()
root <- bb_root()

test_that("[FORBIDDEN] [BUNDLE] code/: inclui scripts/prevcarga, scripts/lib/, models/, config/, renv.lock, LICENSE e NOTICE; exclui tools, tests, *.md, models/*/models", {
  st <- bb_stage(e)
  fs <- list.files(file.path(st, "code"), recursive = TRUE, all.files = TRUE)
  expect_true("scripts/prevcarga" %in% fs)
  expect_true(any(startsWith(fs, "scripts/lib/")) && "scripts/lib/runtime.R" %in% fs)
  expect_true(any(startsWith(fs, "models/")) && "models/random-forest/run.yaml" %in% fs)
  expect_true(any(startsWith(fs, "config/")))
  expect_true("renv.lock" %in% fs)
  expect_true(all(c("LICENSE", "NOTICE") %in% fs))
  expect_true(any(grepl("Apache License", readLines(file.path(st, "code", "LICENSE"), n = 5L))))
  expect_false(any(grepl("^scripts/tools/|^scripts/data-prep/", fs)))
  expect_false(any(grepl("(^|/)tests/", fs)))
  expect_false(any(grepl("\\.md$", fs)))
  expect_false(any(grepl("^models/[^/]+/models/", fs)))
})

test_that("[CONTROL] [BUNDLE] code/: iscas (tools, data-prep, tests, README.md, models/<m>/models) plantadas na origem ficam de fora", {
  src <- withr::local_tempdir()
  for (d in c("scripts/lib", "scripts/tools", "scripts/data-prep", "scripts/lib/tests", "models/m1/R", "models/m1/tests",
              "models/m1/models", "config")) dir.create(file.path(src, d), recursive = TRUE)
  writeLines("{}", file.path(src, "renv.lock")); for (l in c("LICENSE", "NOTICE")) writeLines("x", file.path(src, l))
  keep <- c("scripts/prevcarga", "scripts/lib/a.R", "models/m1/R/b.R", "models/m1/run.yaml", "config/config.yaml")
  drop <- c("scripts/tools/t.R", "scripts/data-prep/d.R", "scripts/lib/tests/test-a.R", "models/m1/tests/t.R",
            "models/m1/models/artefato.rds", "scripts/lib/README.md", "models/m1/README.md")
  for (p in c(keep, drop)) writeLines("x", file.path(src, p))
  dest <- withr::local_tempdir()
  got <- e$pc_code_tree(src, dest)
  expect_setequal(got, c(keep, "renv.lock", "LICENSE", "NOTICE"))
  expect_setequal(list.files(dest, recursive = TRUE), c(keep, "renv.lock", "LICENSE", "NOTICE"))
})

test_that("[FORBIDDEN] [BUNDLE] bundle.tsv: 3 ids da classe bundle (exit 6), ASCII, LF, ordenado, sem duplicata no catalogo", {
  f <- file.path(root, "scripts", "lib", "errors", "bundle.tsv")
  raw <- readBin(f, "raw", 1e6)
  expect_true(all(as.integer(raw) < 128L))
  expect_false(as.raw(13L) %in% raw)
  d <- utils::read.delim(f, stringsAsFactors = FALSE, quote = "")
  expect_identical(names(d), c("id", "class", "message", "hint"))
  expect_identical(d$id, c("bundle.manifest_mismatch", "bundle.package_missing", "bundle.platform_mismatch"))
  expect_true(all(d$class == "bundle"))
  cat <- e$prevcarga_errors_load(dir = file.path(root, "scripts", "lib", "errors"), refresh = TRUE)   # rejeita id duplicado
  expect_true(all(d$id %in% cat$id))
  expect_true(all(unname(e$prevcarga_exit_codes[cat$class[match(d$id, cat$id)]]) == 6L))
})

## catalogo consolidado gerado por scripts/tools/export_error_catalog.R sobre uma arvore copiada (CONTROL remove 1 id do TSV)
bb_export_catalog <- function(drop_id = NULL) {
  t <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(t, "scripts", "tools"), recursive = TRUE)
  file.copy(file.path(root, "scripts", "tools", "export_error_catalog.R"), file.path(t, "scripts", "tools"))
  file.copy(file.path(root, "scripts", "lib"), file.path(t, "scripts"), recursive = TRUE)
  unlink(file.path(t, "scripts", "lib", "tests"), recursive = TRUE)
  if (!is.null(drop_id)) {
    f <- file.path(t, "scripts", "lib", "errors", "bundle.tsv")
    l <- readLines(f); writeLines(l[!startsWith(l, drop_id)], f)
  }
  out <- file.path(t, "code", "install", "error-catalog.tsv")
  st <- system2(bb_rscript(), c("--vanilla", shQuote(file.path(t, "scripts", "tools", "export_error_catalog.R")), "--out", shQuote(out)),
                stdout = FALSE, stderr = FALSE)
  expect_identical(st, 0L)
  utils::read.delim(out, stringsAsFactors = FALSE, quote = "")
}

test_that("[FORBIDDEN] [BUNDLE] catalogo gerado no stage contem os tres ids do bundle.tsv; [CONTROL] id removido do TSV some", {
  ids <- c("bundle.manifest_mismatch", "bundle.package_missing", "bundle.platform_mismatch")
  expect_true(all(ids %in% bb_export_catalog()$id))
  expect_false("bundle.platform_mismatch" %in% bb_export_catalog(drop_id = "bundle.platform_mismatch")$id)
})

test_that("[FORBIDDEN] [BUNDLE] pacote ausente: prevcarga_bundle_require aborta exit 6 [prevcarga/bundle], bundle.package_missing, ASCII", {
  lib <- file.path(root, "scripts", "lib")
  drv <- withr::local_tempfile(fileext = ".R")
  writeLines(c(
    sprintf("options(prevcarga.lib = '%s')", lib),
    sprintf("for (m in c('errors.R', 'runtime.R')) source(file.path('%s', m))", lib),
    "pkg <- commandArgs(TRUE)[1]",
    "r <- tryCatch({ prevcarga_bundle_require(pkg); NULL }, prevcarga_error = function(c) c)",
    "if (is.null(r)) { cat('ok\\n'); quit(status = 0L, save = 'no') }",
    "cat(sprintf('exit=%d\\nid=%s\\nclass=%s\\npackage=%s\\nmessage=%s\\n', r$exit_code, r$id, r$class_name, r$details$package, conditionMessage(r)))",
    "quit(status = r$exit_code, save = 'no')"), drv)
  empty <- withr::local_tempdir()
  run <- function(pkg, libs_user) {
    out <- suppressWarnings(system2(bb_rscript(), c("--vanilla", shQuote(drv), pkg), stdout = TRUE, stderr = TRUE,
                                    env = c(paste0("R_LIBS_USER=", libs_user), "R_LIBS=", "R_LIBS_SITE=")))
    list(status = attr(out, "status") %||% 0L, out = out)
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a
  ## biblioteca sem data.table (so base+recomendados do R + diretorio vazio) => exit 6
  bad <- run("data.table", empty)
  skip_if(identical(bad$status, 0L), "data.table esta na library base do R do teste: nao ha como simular a ausencia")
  expect_identical(bad$status, 6L)
  expect_identical(as.character(bad$out), c("exit=6", "id=bundle.package_missing", "class=bundle", "package=data.table",
                              "message=[prevcarga/bundle] pacote R obrigatorio ausente no bundle"))
  expect_true(all(as.integer(charToRaw(paste(bad$out, collapse = "\n"))) < 128L))
  ## CONTROL: pacote presente (base) => segue e sai 0
  ok <- run("stats", empty)
  expect_identical(ok$status, 0L)
  expect_identical(as.character(ok$out), "ok")
})

## --- renv.lock x referencia ---------------------------------------------------------------------------------
ref_lock <- "/opt/ons-source/prevcargadessem3/renv.lock"
lock <- jsonlite::read_json(file.path(root, "renv.lock"))
desv <- yaml::read_yaml(file.path(root, "tests", "bundle", "lock_desvios.yaml"))$desvios
ENGINES <- c("randomForest", "lightgbm", "forecast", "e1071", "fable", "fabletools", "data.table", "arrow")

test_that("[FORBIDDEN] [BUNDLE] renv.lock: R 4.5.3, repositorio pinado, sem pacotes de rede, engines presentes", {
  expect_identical(lock$R$Version, "4.5.3")
  expect_match(lock$R$Repositories[[1L]]$URL, "^https://packagemanager\\.posit\\.co/cran/[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  expect_false(any(c("paws", "paws.storage", "httr", "httr2", "aws.s3", "curl") %in% names(lock$Packages)))
  expect_true(all(ENGINES %in% names(lock$Packages)))
  expect_true(all(c("lubridate", "jsonlite", "digest", "yaml", "docopt", "RhpcBLASctl") %in% names(lock$Packages)))
  expect_true(all(vapply(lock$Packages, function(p) identical(p$Source, "Repository"), NA)))
})

test_that("[FORBIDDEN] [BUNDLE] renv.lock x referencia: todo pacote comum com versao diferente esta em lock_desvios.yaml com motivo", {
  skip_if(!file.exists(ref_lock), "renv.lock da referencia (prevcargadessem3) ausente neste host")
  ref <- jsonlite::read_json(ref_lock)
  expect_identical(ref$R$Version, "4.2.3")
  expect_identical(bb_lock_offenders(lock, ref, desv), character())
  ## a lista e fechada e honesta: cada desvio tem os dois lados verdadeiros e motivo
  for (d in desv) {
    expect_identical(d$versao_adotada, lock$Packages[[d$pacote]]$Version, info = d$pacote)
    expect_identical(d$versao_referencia, ref$Packages[[d$pacote]]$Version, info = d$pacote)
    expect_true(nzchar(d$motivo), info = d$pacote)
    expect_identical(d$adr, "ADR-0036")
  }
  expect_identical(anyDuplicated(vapply(desv, function(d) d$pacote, "")), 0L)
})

test_that("[CONTROL] [BUNDLE] renv.lock x referencia: desvio fora da lista reprova", {
  skip_if(!file.exists(ref_lock), "renv.lock da referencia (prevcargadessem3) ausente neste host")
  ref <- jsonlite::read_json(ref_lock)
  sem_rf <- Filter(function(d) d$pacote != "randomForest", desv)
  expect_identical(bb_lock_offenders(lock, ref, sem_rf), "randomForest")
  drift <- lock; drift$Packages$lightgbm$Version <- "9.9.9"
  expect_identical(bb_lock_offenders(drift, ref, Filter(function(d) d$pacote != "lightgbm", desv)), "lightgbm")
  same <- lock; same$Packages$randomForest$Version <- ref$Packages$randomForest$Version
  expect_identical(bb_lock_offenders(same, ref, sem_rf), character())     # sem diferenca de versao nao precisa de desvio
})

## --- braco do tarball candidato -------------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a
test_that("[BUNDLE] tarball: etc/ldpaths inclui syslib/; library() de todo pacote do lock pelo Rscript do bundle sem R_LIBS_*", {
  tb <- bb_dist_tarball()
  skip_if(is.na(tb), "sem tarball em dist/: rode scripts/tools/build_bundle_linux.sh <v> (braco do container nao executado)")
  skip_if(!nzchar(Sys.which("zstd")), "zstd ausente no host")
  b <- bb_extract(tb)
  expect_true(any(grepl("syslib", readLines(file.path(b, "runtime", "lib", "R", "etc", "ldpaths")), fixed = TRUE)))
  expect_true(dir.exists(file.path(b, "syslib")))
  for (p in c("scripts/prevcarga", "scripts/lib", "models", "config", "renv.lock", "install/error-catalog.tsv"))
    expect_true(file.exists(file.path(b, "code", p)), info = p)
  expect_false(any(grepl("^scripts/tools|(^|/)tests/|\\.md$|^models/[^/]+/models/", list.files(file.path(b, "code"), recursive = TRUE))))
  rs <- file.path(b, "runtime", "bin", "Rscript")
  pk <- names(lock$Packages)
  expr <- sprintf("pk <- c(%s); bad <- pk[!vapply(pk, function(p) requireNamespace(p, quietly = TRUE), NA)]; cat(R.version.string, '\\n'); cat(vapply(.libPaths(), normalizePath, ''), sep = '\\n'); cat('BAD=', paste(bad, collapse = ','), '\\n', sep = '')",
                  paste0("'", pk, "'", collapse = ","))
  out <- system2(rs, c("--vanilla", "-e", shQuote(expr)), stdout = TRUE, stderr = TRUE,
                 env = c("R_HOME=", "R_LIBS=", "R_LIBS_USER=", "R_LIBS_SITE=", "LD_LIBRARY_PATH="))
  expect_match(out[1L], "R version 4.5.3")
  expect_identical(out[length(out)], "BAD=")
  libs <- out[-c(1L, length(out))]
  expect_true(all(startsWith(libs, normalizePath(file.path(b, "runtime")))))
  ## a CLI roda pelo R do bundle (relocavel: R_HOME vem do proprio caminho) e o doctor sai 0
  cli <- system2(rs, c("--vanilla", shQuote(file.path(b, "code", "scripts", "prevcarga")), "--version"), stdout = TRUE, stderr = TRUE,
                 env = c("R_HOME=", "R_LIBS=", "R_LIBS_USER=", "R_LIBS_SITE="))
  expect_identical(attr(cli, "status") %||% 0L, 0L)
  ## o manifesto do tarball confere por inteiro
  expect_true(e$prevcarga_runtime_validate(b)$ok)
})
