## test-python-free-release.R -- prova final sobre a release candidata (M8.12; AC 2): os DOIS bundles de dist/ (linux-x86_64 tar.zst e windows-x86_64 zip,
## os produzidos por scripts/tools/build_bundle_linux.sh e build_bundle_windows.ps1) extraidos; Python-free, grep de rede/env e portabilidade com zero hits,
## RUNTIME.json valido com r_version 4.5.3 e o MESMO code_commit nos dois. CONTROL: x.py e download.file( injetados num stage copiado de cada bundle reprovam.
## Braco windows-x86_64: o zip so existe depois do build no Windows [DEFERRED windows-vm do M8, dono: Gabriel; gate W17 de docs/reference/windows-gate.md];
## sem ele o braco faz SKIP `nao-windows` (nunca verde silencioso) e a execucao nativa fica com o gate da VM Windows. dist/ nunca e versionado.

e <- bb_env()

pr_pick <- function(glob) {
  fs <- c(Sys.glob(file.path(bb_root(), "dist", glob)), Sys.glob(file.path(bb_root(), "dist", "repro-1", glob)))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}
pr_extract <- function(path, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  if (grepl("\\.zip$", path)) utils::unzip(path, exdir = d)
  else if (!identical(system2("tar", c("--zstd", "-xf", shQuote(path), "-C", shQuote(d)), stdout = FALSE, stderr = FALSE), 0L)) stop("tar --zstd falhou")
  file.path(d, "prevcarga")
}
PR_BUNDLES <- list(`linux-x86_64` = quote(pr_pick("prevcarga-*-linux-x86_64.tar.zst")), `windows-x86_64` = quote(pr_pick("prevcarga-*-windows-x86_64.zip")))

## Cada bundle extraido uma vez (cache do arquivo): raiz ou NA
pr_roots <- local({
  cache <- list()
  function(b, env = parent.frame()) {
    if (!is.null(cache[[b]])) return(cache[[b]])
    p <- eval(PR_BUNDLES[[b]])
    if (is.na(p) || (grepl("\\.tar\\.zst$", p) && !nzchar(Sys.which("zstd")))) return(NA_character_)
    d <- tempfile("pr-bundle-"); dir.create(d)
    withr::defer(unlink(d, recursive = TRUE), testthat::teardown_env())
    if (grepl("\\.zip$", p)) utils::unzip(p, exdir = d) else stopifnot(identical(system2("tar", c("--zstd", "-xf", shQuote(p), "-C", shQuote(d)), stdout = FALSE, stderr = FALSE), 0L))
    cache[[b]] <<- file.path(d, "prevcarga")
    cache[[b]]
  }
})

pr_skip_missing <- function(b) {
  root <- pr_roots(b)
  if (is.na(root)) {
    if (identical(b, "windows-x86_64")) skip("nao-windows: bundle windows-x86_64 ausente em dist/ (build no Windows) [DEFERRED windows-vm do M8, dono: Gabriel; gate W17]")
    skip(sprintf("sem bundle %s em dist/ (rode scripts/tools/build_bundle_linux.sh <v>) ou sem zstd no host", b))
  }
  root
}

for (b in names(PR_BUNDLES)) {
  test_that(sprintf("[FORBIDDEN] [BUNDLE] python_free + net/env + portabilidade: bundle %s extraido tem zero hits", b), {
    root <- pr_skip_missing(b)
    expect_identical(e$pc_bundle_python_free(root)$path, character(), info = b)
    expect_equal(nrow(e$pc_bundle_net_grep(file.path(root, "code"))), 0L, info = b)
    expect_equal(nrow(e$pc_bundle_portability_grep(file.path(root, "code"))), 0L, info = b)
  })

  test_that(sprintf("[AC] [BUNDLE] RUNTIME.json do bundle %s valida, r_version 4.5.3 e plataforma coerente", b), {
    root <- pr_skip_missing(b)
    rj <- jsonlite::read_json(file.path(root, "RUNTIME.json"))
    expect_identical(rj$r_version, "4.5.3", info = b)
    expect_identical(rj$platform, b)
    expect_match(rj$code_commit, "^[0-9a-f]{40}$")
    v <- e$prevcarga_runtime_validate(root)
    expect_true(v$ok, info = paste(b, paste(utils::head(v$divergentes, 5L), collapse = ",")))
  })

  test_that(sprintf("[CONTROL] [BUNDLE] injecao de x.py e download.file( num stage copiado do bundle %s reprova", b), {
    root <- pr_skip_missing(b)
    stage <- file.path(withr::local_tempdir(), "prevcarga")
    dir.create(stage)
    expect_true(file.copy(file.path(root, "code"), stage, recursive = TRUE))
    expect_identical(e$pc_bundle_python_free(stage)$path, character())          # baseline do stage copiado: limpo
    expect_equal(nrow(e$pc_bundle_net_grep(file.path(stage, "code"))), 0L)
    writeLines("print(1)", file.path(stage, "code", "x.py"))
    expect_identical(e$pc_bundle_python_free(stage)$path, "code/x.py")
    f <- file.path(stage, "code", "scripts", "lib", "dates.R")
    l <- readLines(f); writeLines(c(l[1:3], "x <- download.file('http://exemplo.invalido/x', 'y')", l[-(1:3)]), f)
    h <- e$pc_bundle_net_grep(file.path(stage, "code"))
    expect_identical(h$path, "scripts/lib/dates.R"); expect_identical(h$linha, 4L)
  })
}

test_that("[AC] [BUNDLE] os dois bundles candidatos tem o MESMO code_commit (e o mesmo r_version)", {
  roots <- vapply(names(PR_BUNDLES), pr_roots, "")
  if (anyNA(roots)) {
    faltam <- names(roots)[is.na(roots)]
    skip(paste0(if ("windows-x86_64" %in% faltam) "nao-windows: " else "", "bundle(s) ausente(s) em dist/: ", paste(faltam, collapse = ", "),
                " [DEFERRED windows-vm do M8, dono: Gabriel; gate W17] -- a igualdade de code_commit entre os dois so se prova com os dois"))
  }
  rj <- lapply(roots, function(r) jsonlite::read_json(file.path(r, "RUNTIME.json")))
  expect_identical(rj[[1L]]$code_commit, rj[[2L]]$code_commit)
  expect_identical(rj[[1L]]$r_version, rj[[2L]]$r_version)
})

test_that("[CONTROL] os detectores acusam num stage SINTETICO (independente de dist/): x.py, download.file( e hit de portabilidade", {
  root <- bb_stage(e)
  writeLines("print(1)", file.path(root, "code", "x.py"))
  expect_identical(e$pc_bundle_python_free(root)$path, "code/x.py")
  f <- file.path(root, "code", "scripts", "lib", "dates.R")
  l <- readLines(f); writeLines(c(l[1:3], "x <- download.file('u', 'y')", "y <- parallel::mclapply(1:2, identity)", l[-(1:3)]), f)
  expect_identical(e$pc_bundle_net_grep(file.path(root, "code"))$linha, 4L)
  expect_identical(e$pc_bundle_portability_grep(file.path(root, "code"))$linha, 5L)
})
