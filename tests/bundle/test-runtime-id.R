## test-runtime-id.R -- reprodutibilidade e manifesto do RUNTIME.json (M8.1; AC 1 e 2). Gate [BUNDLE] runtime_id.
## Sem container: dois pc_runtime_json_write sobre o MESMO stage. O braco do container compara dois builds reais de dist/.

e <- bb_env()

test_that("[FORBIDDEN] [BUNDLE] runtime_id: duas gravacoes sobre o mesmo stage dao runtime_id e bytes iguais; r_version 4.5.3", {
  skip_if_not(identical(paste(R.version$major, R.version$minor, sep = "."), "4.5.3"), "R do teste nao e 4.5.3 (PATH rule)")
  root <- bb_stage(e)
  id1 <- bb_write_runtime(e, root, built_at = "2026-10-27T12:00:00Z")
  b1 <- readBin(file.path(root, "RUNTIME.json"), "raw", 1e8)
  id2 <- bb_write_runtime(e, root, built_at = "2026-10-27T12:00:00Z")
  b2 <- readBin(file.path(root, "RUNTIME.json"), "raw", 1e8)
  expect_identical(id1, id2)
  expect_identical(b1, b2)
  expect_match(id1, "^[0-9a-f]{64}$")
  expect_identical(e$prevcarga_runtime_id(file.path(root, "RUNTIME.json")), id1)
  x <- jsonlite::read_json(file.path(root, "RUNTIME.json"))
  expect_identical(x$r_version, "4.5.3")
  expect_identical(x$platform, "linux-x86_64")
  expect_identical(x$code_commit, strrep("a", 40))
  expect_match(x$renv_lock_hash, "^[0-9a-f]{64}$")
  expect_identical(x$renv_lock_hash, digest::digest(file = file.path(root, "code", "renv.lock"), algo = "sha256"))
  paths <- vapply(x$files, `[[`, "", "path")
  expect_identical(paths, sort(paths, method = "radix"))
  expect_false("RUNTIME.json" %in% paths)
  expect_true(all(c("code/scripts/prevcarga", "code/scripts/lib/runtime.R", "code/renv.lock", "syslib/libtoy.so.1") %in% paths))
  ## todo files[].sha256 confere com o arquivo
  ok <- vapply(x$files, function(f) identical(digest::digest(file = file.path(root, f$path), algo = "sha256"), f$sha256), NA)
  expect_true(all(ok))
})

test_that("[FORBIDDEN] [BUNDLE] runtime_id: built_at e lateral (fora do id) e nao ha relogio quando omitido", {
  root <- bb_stage(e)
  id_a <- bb_write_runtime(e, root, built_at = "2026-10-27T12:00:00Z")
  id_b <- bb_write_runtime(e, root, built_at = "2027-01-01T00:00:00Z")
  expect_identical(id_a, id_b)
  bb_write_runtime(e, root)                                  # sem built_at: o arquivo nao carrega relogio
  expect_null(jsonlite::read_json(file.path(root, "RUNTIME.json"))$built_at)
  expect_identical(e$prevcarga_runtime_id(file.path(root, "RUNTIME.json")), id_a)
  expect_null(e$prevcarga_runtime_id(file.path(root, "nao-existe", "RUNTIME.json")))   # arvore de dev
})

test_that("[CONTROL] [BUNDLE] runtime_id: 1 linha num scripts/lib/*.R do stage muda o id", {
  root <- bb_stage(e)
  id0 <- bb_write_runtime(e, root)
  f <- file.path(root, "code", "scripts", "lib", "dates.R")
  cat("## uma linha a mais\n", file = f, append = TRUE)
  id1 <- bb_write_runtime(e, root)
  expect_false(identical(id0, id1))
  ## o id tambem muda com o commit, a versao e o apt_snapshot (identidade do conteudo declarado)
  expect_false(identical(id1, bb_write_runtime(e, root, code_commit = strrep("b", 40))))
  expect_false(identical(id1, bb_write_runtime(e, root, apt_snapshot = "20261028T000000Z")))
})

test_that("[CONTROL] [BUNDLE] runtime_id: R do build diferente do R.Version do renv.lock aborta", {
  root <- bb_stage(e)
  expect_error(bb_write_runtime(e, root, r_version = "4.2.3"), "difere de R.Version")
})

test_that("[FORBIDDEN] [BUNDLE] manifesto: bundle intocado valida ok com zero divergencias", {
  root <- bb_stage(e)
  bb_write_runtime(e, root)
  v <- e$prevcarga_runtime_validate(root)
  expect_true(v$ok)
  expect_identical(v$divergentes, character())
  expect_silent(e$prevcarga_runtime_validate(root, strict = TRUE))
})

test_that("[CONTROL] [BUNDLE] manifesto: 1 byte em code/ => bundle.manifest_mismatch (exit 6) listando o path", {
  root <- bb_stage(e)
  bb_write_runtime(e, root)
  f <- file.path(root, "code", "scripts", "lib", "topology.R")
  b <- readBin(f, "raw", 1e8); b[1L] <- as.raw((as.integer(b[1L]) + 1L) %% 256L)
  writeBin(b, f)
  v <- e$prevcarga_runtime_validate(root)
  expect_false(v$ok)
  expect_identical(v$divergentes, "code/scripts/lib/topology.R")
  cond <- tryCatch(e$prevcarga_runtime_validate(root, strict = TRUE), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "bundle.manifest_mismatch")
  expect_identical(cond$exit_code, 6L)
  expect_match(conditionMessage(cond), "^\\[prevcarga/bundle\\]")
  expect_true("code/scripts/lib/topology.R" %in% unlist(cond$details$paths))
  ## arquivo removido tambem diverge; sem RUNTIME.json nao e bundle
  unlink(file.path(root, "syslib", "libtoy.so.1"))
  expect_setequal(e$prevcarga_runtime_validate(root)$divergentes, c("code/scripts/lib/topology.R", "syslib/libtoy.so.1"))
  unlink(file.path(root, "RUNTIME.json"))
  expect_identical(e$prevcarga_runtime_validate(root)$divergentes, "RUNTIME.json")
})

test_that("[CONTROL] [BUNDLE] runtime_id: link pendente no stage aborta a gravacao (nunca entra no manifesto)", {
  skip_on_os("windows")
  root <- bb_stage(e)
  file.symlink("nao-existe.so", file.path(root, "syslib", "libpendente.so"))
  expect_error(bb_write_runtime(e, root), "link pendente")
})

test_that("[BUNDLE] container: dois builds reais em dist/repro-1 e dist/repro-2 tem RUNTIME.json e runtime_id identicos", {
  skip_if(!nzchar(Sys.which("docker")), "docker ausente no host: braco do container nao executado (nunca verde silencioso)")
  a <- Sys.glob(file.path(bb_root(), "dist", "repro-1", "prevcarga-*-linux-x86_64.RUNTIME.json"))
  b <- Sys.glob(file.path(bb_root(), "dist", "repro-2", "prevcarga-*-linux-x86_64.RUNTIME.json"))
  skip_if(!length(a) || !length(b), "rode scripts/tools/build_bundle_linux.sh <v> --out dist/repro-1 e --out dist/repro-2 (mesmo commit)")
  expect_identical(e$prevcarga_runtime_id(a[[1L]]), e$prevcarga_runtime_id(b[[1L]]))
  expect_identical(unname(tools::md5sum(a[[1L]])), unname(tools::md5sum(b[[1L]])))
  expect_identical(jsonlite::read_json(a[[1L]])$r_version, "4.5.3")
  expect_identical(jsonlite::read_json(a[[1L]])$platform, "linux-x86_64")
  sums <- sub(" .*", "", readLines(sub("RUNTIME.json$", "tar.zst.sha256", a[[1L]])))
  expect_identical(sums, sub(" .*", "", readLines(sub("RUNTIME.json$", "tar.zst.sha256", b[[1L]]))))
})
