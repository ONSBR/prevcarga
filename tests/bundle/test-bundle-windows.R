## test-bundle-windows.R -- bundle windows-x86_64 (M8.2; ADR-0038; AC 1-3). Gate [BUNDLE] windows.
## O build (scripts/tools/build_bundle_windows.ps1) so roda num host Windows: aqui valem (a) as FUNCOES reais do build exercitadas
## offline no Linux (restauracao do lock com fetch/installer simulados, manifesto, zip reprodutivel), (b) o pino de binarios
## (windows_binaries.tsv x renv.lock) e (c) o braco do zip real de dist/ (skip com motivo se nao existe) e do host Windows
## (skip `nao-windows`). Nunca verde silencioso: todo skip nomeia o que falta.

e <- bb_env()
root <- bb_root()
lock <- jsonlite::read_json(file.path(root, "renv.lock"))
`%||%` <- function(a, b) if (is.null(a)) b else a

on_windows <- identical(.Platform$OS.type, "windows")
have_zip <- requireNamespace("zip", quietly = TRUE)

bw_win_zip <- function() {
  fs <- c(Sys.glob(file.path(root, "dist", "prevcarga-*-windows-x86_64.zip")),
          Sys.glob(file.path(root, "dist", "repro-1", "prevcarga-*-windows-x86_64.zip")))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}

## Divergencias entre a library de um R (<lib>/<pkg>/DESCRIPTION) e o renv.lock: "pkg: instalado X, lock Y".
bw_lib_offenders <- function(lib, lk) {
  out <- vapply(lk$Packages, function(p) {
    f <- file.path(lib, p$Package, "DESCRIPTION")
    v <- if (file.exists(f)) unname(read.dcf(f, fields = "Version")[1L, 1L]) else "<ausente>"
    if (identical(v, p$Version)) "" else sprintf("%s: instalado %s, lock %s", p$Package, v, p$Version)
  }, "")
  unname(out[nzchar(out)])
}

## Lock sintetico de 3 pacotes + fetch/installer simulados (o installer grava <lib>/<pkg>/DESCRIPTION com a versao do nome do arquivo).
bw_fake <- function(fail_bin = character(), wrong = list()) {
  lk <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  pk <- lapply(c(aaa = "1.0.0", bbb = "2.1-3", ccc = "0.9.1"), function(v) NULL)
  ver <- c(aaa = "1.0.0", bbb = "2.1-3", ccc = "0.9.1")
  jsonlite::write_json(list(R = list(Version = "4.5.3"),
                            Packages = lapply(setNames(names(ver), names(ver)), function(n) list(Package = n, Version = ver[[n]]))),
                       lk, auto_unbox = TRUE)
  calls <- new.env(); calls$types <- character()
  fetch <- function(url, dest) {
    nm <- sub("_.*$", "", basename(url))
    if (grepl("/bin/windows/", url, fixed = TRUE) && nm %in% fail_bin) stop("HTTP 404")
    writeLines(paste("conteudo", basename(url)), dest)
  }
  installer <- function(path, lib, type) {
    b <- sub("\\.(zip|tar\\.gz)$", "", basename(path))
    nm <- sub("_.*$", "", b); v <- sub("^[^_]*_", "", b)
    calls$types <- c(calls$types, paste(nm, type))
    dir.create(file.path(lib, nm), recursive = TRUE, showWarnings = FALSE)
    writeLines(c(paste("Package:", nm), paste("Version:", wrong[[nm]] %||% v)), file.path(lib, nm, "DESCRIPTION"))
  }
  hashes <- data.frame(pacote = names(ver), versao = unname(ver), stringsAsFactors = FALSE)
  hashes$sha256 <- vapply(seq_len(nrow(hashes)), function(i) {
    tf <- tempfile(); on.exit(unlink(tf))
    writeLines(paste("conteudo", sprintf("%s_%s.zip", hashes$pacote[i], hashes$versao[i])), tf)
    digest::digest(file = tf, algo = "sha256")
  }, "")
  list(lock = lk, fetch = fetch, installer = installer, hashes = hashes, calls = calls)
}

bw_restore <- function(f, lib, hashes = f$hashes)
  e$pc_lock_install_windows(f$lock, lib, "https://repo.invalido/bin", "https://repo.invalido/src",
                            hashes = hashes, fetch = f$fetch, installer = f$installer)

## --- AC1: lock restaurado, versoes == lock, compilados declarados -----------------------------------------------------

test_that("[FORBIDDEN] [BUNDLE] windows: pc_lock_install_windows instala o binario da versao EXATA do lock e nao declara compilados", {
  f <- bw_fake(); lib <- withr::local_tempdir()
  comp <- bw_restore(f, lib)
  expect_s3_class(comp, "data.frame")
  expect_identical(names(comp), c("pacote", "versao", "motivo"))
  expect_identical(nrow(comp), 0L)
  expect_identical(f$calls$types, c("aaa win.binary", "bbb win.binary", "ccc win.binary"))
  expect_identical(bw_lib_offenders(lib, jsonlite::read_json(f$lock)), character())
})

test_that("[FORBIDDEN] [BUNDLE] windows: pacote sem binario cai no fonte (Rtools) e entra em compilados com pacote, versao e motivo", {
  skip_if_not(isTRUE(tryCatch(pkgbuild::has_build_tools(debug = FALSE), error = function(e) FALSE)),
              "sem ferramentas de compilacao neste host: o ramo do fonte exige has_build_tools()")
  f <- bw_fake(fail_bin = "bbb"); lib <- withr::local_tempdir()
  comp <- bw_restore(f, lib)
  expect_identical(comp$pacote, "bbb")
  expect_identical(comp$versao, "2.1-3")
  expect_match(comp$motivo, "sem binario CRAN para R 4.5.3")
  expect_identical(f$calls$types, c("aaa win.binary", "bbb source", "ccc win.binary"))
  ## o RUNTIME.json recebe o data.frame como lista de objetos {pacote, versao, motivo}
  expect_identical(e$pc_compilados_as_list(comp), list(list(pacote = "bbb", versao = "2.1-3", motivo = comp$motivo)))
  expect_identical(e$pc_compilados_as_list(comp[0, ]), list())
})

test_that("[CONTROL] [BUNDLE] windows: 1 pacote instalado numa versao diferente da do lock reprova citando o pacote", {
  f <- bw_fake(wrong = list(ccc = "9.9.9")); lib <- withr::local_tempdir()
  expect_error(bw_restore(f, lib), "ccc: instalado 9.9.9, lock 0.9.1", fixed = TRUE)
  ## o comparador do teste do zip real acusa a mesma divergencia
  expect_identical(bw_lib_offenders(lib, jsonlite::read_json(f$lock)), "ccc: instalado 9.9.9, lock 0.9.1")
})

test_that("[CONTROL] [BUNDLE] windows: sha256 do binario diferente do pinado (integrity) ou sem pino aborta antes de instalar", {
  f <- bw_fake(); lib <- withr::local_tempdir()
  h <- f$hashes; h$sha256[h$pacote == "bbb"] <- strrep("0", 64)
  expect_error(bw_restore(f, lib, hashes = h), "bbb 2.1-3 difere do pinado")
  expect_false(dir.exists(file.path(lib, "bbb")))
  expect_error(bw_restore(f, withr::local_tempdir(), hashes = f$hashes[f$hashes$pacote != "ccc", ]), "sem hash pinado para ccc 0.9.1")
})

test_that("[FORBIDDEN] [BUNDLE] windows: windows_binaries.tsv cobre exatamente o renv.lock (versoes iguais, sha256 hex) + so a ferramenta zip", {
  h <- utils::read.delim(file.path(root, "scripts", "tools", "windows_binaries.tsv"), colClasses = "character", quote = "")
  expect_identical(names(h), c("pacote", "versao", "grupo", "sha256"))
  expect_match(h$sha256, "^[0-9a-f]{64}$")
  expect_identical(anyDuplicated(h$pacote), 0L)
  lk <- h[h$grupo == "lock", ]
  expect_setequal(lk$pacote, names(lock$Packages))
  expect_identical(unname(lk$versao[match(names(lock$Packages), lk$pacote)]), unname(vapply(lock$Packages, function(p) p$Version, "")))
  expect_identical(h$pacote[h$grupo == "ferramenta"], "zip")
  ## o build so usa jsonlite, digest e zip antes de restaurar o lock: os tres tem linha unica
  expect_true(all(c("jsonlite", "digest") %in% lk$pacote))
})

## --- AC2: esquema do RUNTIME.json Windows == Linux -----------------------------------------------------------------------

bw_types <- function(x) vapply(x, function(v) if (is.null(v)) "null" else class(v)[1L], "")

bw_write <- function(root_b, ...) {
  args <- list(platform = "windows-x86_64", apt_snapshot = NULL, built_at = "2026-10-27T12:00:00Z", ...)
  do.call(bb_write_runtime, c(list(e, root_b), args))
}

test_that("[FORBIDDEN] [BUNDLE] windows: RUNTIME.json tem as mesmas chaves e tipos do Linux; platform, r_version, apt_snapshot nulo, paths com /", {
  skip_if_not(identical(paste(R.version$major, R.version$minor, sep = "."), "4.5.3"), "R do teste nao e 4.5.3 (PATH rule)")
  rl <- bb_stage(e); bb_write_runtime(e, rl, built_at = "2026-10-27T12:00:00Z")
  rw <- bb_stage(e)
  dir.create(file.path(rw, "runtime", "R", "bin"), recursive = TRUE)
  writeLines("MZ-de-brinquedo", file.path(rw, "runtime", "R", "bin", "Rscript.exe"))
  comp <- data.frame(pacote = "bbb", versao = "2.1-3", motivo = "sem binario CRAN para R 4.5.3", stringsAsFactors = FALSE)
  bw_write(rw, compilados = comp)
  xl <- jsonlite::read_json(file.path(rl, "RUNTIME.json")); xw <- jsonlite::read_json(file.path(rw, "RUNTIME.json"))
  expect_identical(names(xw), names(xl))
  tl <- bw_types(xl); tw <- bw_types(xw)
  expect_identical(tw[setdiff(names(tw), c("apt_snapshot", "compilados"))], tl[setdiff(names(tl), c("apt_snapshot", "compilados"))])
  expect_identical(tl[["apt_snapshot"]], "character"); expect_identical(tw[["apt_snapshot"]], "null")
  expect_identical(xw$platform, "windows-x86_64")
  expect_identical(xw$r_version, "4.5.3")
  expect_null(xw$apt_snapshot)
  expect_identical(names(xw$compilados[[1L]]), c("motivo", "pacote", "versao"))        # chaves ordenadas pelo canonico
  expect_identical(vapply(xw$compilados[[1L]], class, ""), c(motivo = "character", pacote = "character", versao = "character"))
  paths <- vapply(xw$files, `[[`, "", "path")
  expect_false(any(grepl("\\", paths, fixed = TRUE)))
  expect_true("runtime/R/bin/Rscript.exe" %in% paths)
  expect_true(all(vapply(xw$files, function(f) identical(digest::digest(file = file.path(rw, f$path), algo = "sha256"), f$sha256), NA)))
  ## o id recalculado pelo codigo do runtime.R confere com o do escritor
  expect_identical(e$prevcarga_runtime_id(file.path(rw, "RUNTIME.json")), bw_write(rw, compilados = comp))
})

test_that("[CONTROL] [BUNDLE] windows: 1 byte alterado em code/ => prevcarga_runtime_validate falha com bundle.manifest_mismatch (exit 6)", {
  rw <- bb_stage(e); bw_write(rw)
  expect_true(e$prevcarga_runtime_validate(rw)$ok)
  fl <- file.path(rw, "code", "scripts", "lib", "topology.R")
  b <- readBin(fl, "raw", 1e8); b[1L] <- as.raw((as.integer(b[1L]) + 1L) %% 256L); writeBin(b, fl)
  cond <- tryCatch(e$prevcarga_runtime_validate(rw, strict = TRUE), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "bundle.manifest_mismatch")
  expect_identical(cond$exit_code, 6L)
})

## --- AC3: reprodutibilidade do runtime_id e do zip ------------------------------------------------------------------------

test_that("[FORBIDDEN] [BUNDLE] windows: dois manifestos do mesmo stage dao runtime_id e bytes iguais; [CONTROL] 1 linha em scripts/lib muda o id", {
  rw <- bb_stage(e)
  id1 <- bw_write(rw); b1 <- readBin(file.path(rw, "RUNTIME.json"), "raw", 1e8)
  id2 <- bw_write(rw); b2 <- readBin(file.path(rw, "RUNTIME.json"), "raw", 1e8)
  expect_identical(id1, id2); expect_identical(b1, b2)
  cat("## uma linha a mais\n", file = file.path(rw, "code", "scripts", "lib", "dates.R"), append = TRUE)
  expect_false(identical(bw_write(rw), id1))
})

test_that("[FORBIDDEN] [BUNDLE] windows: pc_zip_reproducible da o mesmo sha256 para o mesmo conteudo com mtimes diferentes, paths com / ordenados", {
  skip_if_not(have_zip, "pacote zip ausente neste R (e ferramenta do build, fora do lock): instale-o em uma library de scratch e rode com R_LIBS")
  mk <- function(mtime) {
    st <- withr::local_tempdir(.local_envir = parent.frame())
    rw <- file.path(st, "prevcarga"); dir.create(file.path(rw, "code", "scripts"), recursive = TRUE)
    writeLines(c("a", "b"), file.path(rw, "code", "scripts", "z.R")); writeLines("c", file.path(rw, "RUNTIME.json"))
    writeLines("d", file.path(rw, "code", "a.txt"))
    Sys.setFileTime(list.files(st, recursive = TRUE, full.names = TRUE), as.POSIXct(mtime, tz = "UTC"))
    st
  }
  out1 <- withr::local_tempfile(fileext = ".zip"); out2 <- withr::local_tempfile(fileext = ".zip")
  s1 <- e$pc_zip_reproducible(mk("2020-01-01 00:00:00"), out1, "1793000000")
  s2 <- e$pc_zip_reproducible(mk("2025-06-06 06:06:06"), out2, "1793000000")
  expect_identical(s1, s2)
  expect_identical(unname(tools::md5sum(out1)), unname(tools::md5sum(out2)))
  ent <- zip::zip_list(out1)$filename
  expect_identical(ent, sort(ent, method = "radix"))
  expect_identical(ent, c("prevcarga/RUNTIME.json", "prevcarga/code/a.txt", "prevcarga/code/scripts/z.R"))
  ## CONTROL: 1 linha a mais no conteudo => outro sha256
  st3 <- mk("2020-01-01 00:00:00"); cat("e\n", file = file.path(st3, "prevcarga", "code", "a.txt"), append = TRUE)
  expect_false(identical(e$pc_zip_reproducible(st3, withr::local_tempfile(fileext = ".zip"), "1793000000"), s1))
})

## --- braco do zip REAL (dist/prevcarga-<v>-windows-x86_64.zip, construido no host Windows) ----------------------------------

bw_extract <- function(zipfile, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  utils::unzip(zipfile, exdir = d)
  file.path(d, "prevcarga")
}

test_that("[FORBIDDEN] [BUNDLE] windows: zip real extraido: library == renv.lock, compilados declarados, esquema e files[] conferem, LF no code/", {
  zf <- bw_win_zip()
  skip_if(is.na(zf), "sem dist/prevcarga-<v>-windows-x86_64.zip: rode scripts/tools/build_bundle_windows.ps1 num host Windows [DEFERRED M8.4/M8.6]")
  rt <- bw_extract(zf)
  x <- jsonlite::read_json(file.path(rt, "RUNTIME.json"))
  expect_identical(x$platform, "windows-x86_64"); expect_identical(x$r_version, "4.5.3"); expect_null(x$apt_snapshot)
  off <- bw_lib_offenders(file.path(rt, "runtime", "R", "library"), lock)
  comp <- vapply(x$compilados, function(c) c$pacote, "")
  expect_identical(sort(off, method = "radix"), character())
  expect_true(all(vapply(x$compilados, function(c) all(c("pacote", "versao", "motivo") %in% names(c)), NA)))
  expect_true(all(comp %in% names(lock$Packages)))
  expect_identical(x$renv_lock_hash, digest::digest(file = file.path(rt, "code", "renv.lock"), algo = "sha256"))
  expect_true(e$prevcarga_runtime_validate(rt)$ok)
  expect_identical(x$files[[1L]]$path, sort(vapply(x$files, `[[`, "", "path"), method = "radix")[1L])
  expect_false(any(grepl("\\", vapply(x$files, `[[`, "", "path"), fixed = TRUE)))
  expect_true(file.exists(file.path(rt, "runtime", "R", "bin", "Rscript.exe")))
  expect_true(file.exists(file.path(rt, "code", "install", "error-catalog.tsv")))
  r_txt <- list.files(file.path(rt, "code"), pattern = "\\.(R|tsv|yaml|json)$", recursive = TRUE, full.names = TRUE)
  cr <- Filter(function(p) any(readBin(p, "raw", file.size(p)) == as.raw(13L)), r_txt)
  expect_identical(cr, character())
})

test_that("[CONTROL] [BUNDLE] windows: library do zip real com 1 versao trocada reprova citando o pacote", {
  zf <- bw_win_zip()
  skip_if(is.na(zf), "sem dist/prevcarga-<v>-windows-x86_64.zip [DEFERRED M8.4/M8.6]")
  rt <- bw_extract(zf)
  p <- file.path(rt, "runtime", "R", "library", "jsonlite", "DESCRIPTION")
  writeLines(sub("^Version: .*$", "Version: 0.0.1", readLines(p)), p)
  expect_match(bw_lib_offenders(file.path(rt, "runtime", "R", "library"), lock), "^jsonlite: instalado 0.0.1")
})

test_that("[FORBIDDEN] [BUNDLE] windows: dois builds do mesmo commit (dist/repro-1, repro-2) dao RUNTIME.json e runtime_id identicos", {
  a <- Sys.glob(file.path(root, "dist", "repro-1", "prevcarga-*-windows-x86_64.RUNTIME.json"))
  b <- Sys.glob(file.path(root, "dist", "repro-2", "prevcarga-*-windows-x86_64.RUNTIME.json"))
  skip_if(!length(a) || !length(b), "sem dist/repro-1 e dist/repro-2 (--Out dist\\repro-N, mesmo commit, host Windows) [DEFERRED M8.4/M8.6]")
  expect_identical(e$prevcarga_runtime_id(a[1L]), e$prevcarga_runtime_id(b[1L]))
})

test_that("[BUNDLE] windows: library do bundle, pelo Rscript.exe do proprio bundle, tem toda versao do lock (host Windows)", {
  skip_if_not(on_windows, "nao-windows")
  zf <- bw_win_zip()
  skip_if(is.na(zf), "sem dist/prevcarga-<v>-windows-x86_64.zip")
  rt <- bw_extract(zf)
  out <- system2(file.path(rt, "runtime", "R", "bin", "Rscript.exe"),
                 c("--vanilla", "-e", shQuote("for (p in rownames(installed.packages(.libPaths()[1]))) cat(p, as.character(packageVersion(p)), '\\n')")),
                 stdout = TRUE)
  got <- strsplit(trimws(out), " ", fixed = TRUE)
  inst <- setNames(vapply(got, `[`, "", 2L), vapply(got, `[`, "", 1L))
  want <- vapply(lock$Packages, function(p) p$Version, "")
  expect_identical(inst[names(want)], want)
})
