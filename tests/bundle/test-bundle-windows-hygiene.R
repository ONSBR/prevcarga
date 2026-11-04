## test-bundle-windows-hygiene.R -- higiene do bundle Windows (M8.2; AC 4). Gate [BUNDLE] windows_hygiene.
## pc_bundle_python_free, pc_bundle_net_grep (M8.1) e pc_bundle_portability_grep sobre o stage real do code/ e, quando existe, sobre o
## zip Windows extraido de dist/. Funcoes puras sobre caminhos: rodam no Linux sem Windows. CONTROL: injecao num stage copiado.

e <- bb_env()
root <- bb_root()

bh_win_zip <- function() {
  fs <- c(Sys.glob(file.path(root, "dist", "prevcarga-*-windows-x86_64.zip")),
          Sys.glob(file.path(root, "dist", "repro-1", "prevcarga-*-windows-x86_64.zip")))
  if (length(fs)) sort(fs, method = "radix")[length(fs)] else NA_character_
}

test_that("[FORBIDDEN] [BUNDLE] windows_hygiene: code/ real do bundle tem zero Python, zero rede/env e zero fork/symlink/shell/caminho Unix", {
  stage <- bb_stage(e)
  expect_equal(nrow(e$pc_bundle_python_free(stage)), 0L)
  expect_equal(nrow(e$pc_bundle_net_grep(file.path(stage, "code"))), 0L)
  h <- e$pc_bundle_portability_grep(file.path(stage, "code"))
  expect_identical(names(h), c("path", "linha", "texto"))
  expect_equal(nrow(h), 0L)
})

test_that("[FORBIDDEN] [BUNDLE] windows_hygiene: o zip Windows extraido (dist/) passa os tres gates", {
  zf <- bh_win_zip()
  skip_if(is.na(zf), "sem dist/prevcarga-<v>-windows-x86_64.zip: rode scripts/tools/build_bundle_windows.ps1 num host Windows [DEFERRED M8.4/M8.6]")
  d <- withr::local_tempdir(); utils::unzip(zf, exdir = d)
  rt <- file.path(d, "prevcarga")
  expect_identical(e$pc_bundle_python_free(rt)$path, character())
  expect_equal(nrow(e$pc_bundle_net_grep(file.path(rt, "code"))), 0L)
  expect_equal(nrow(e$pc_bundle_portability_grep(file.path(rt, "code"))), 0L)
})

test_that("[CONTROL] [BUNDLE] windows_hygiene: x.py, download.file( e parallel::mclapply( injetados num stage copiado, cada gate lista exatamente o injetado", {
  stage <- bb_stage(e)
  cd <- file.path(stage, "code")
  writeLines("print(1)", file.path(cd, "scripts", "x.py"))
  f <- file.path(cd, "scripts", "lib", "dates.R")
  l <- readLines(f)
  writeLines(c(l[1:3], "x <- download.file('http://exemplo.invalido/x', 'y')", "y <- parallel::mclapply(1:2, identity)", l[-(1:3)]), f)
  expect_identical(e$pc_bundle_python_free(stage)$path, "code/scripts/x.py")
  n <- e$pc_bundle_net_grep(cd)
  expect_identical(c(n$path, n$linha), c("scripts/lib/dates.R", "4"))
  expect_match(n$texto, "download.file(", fixed = TRUE)
  p <- e$pc_bundle_portability_grep(cd)
  expect_identical(p$path, "scripts/lib/dates.R")
  expect_identical(p$linha, 5L)
  expect_match(p$texto, "parallel::mclapply(", fixed = TRUE)
})

test_that("[CONTROL] [BUNDLE] windows_hygiene: cada padrao de portabilidade e acusado; comentario inteiro e fora de scripts/ e models/ nao contam", {
  stage <- bb_stage(e)
  cd <- file.path(stage, "code")
  bait <- c("mclapply(x, f)", "mcparallel(f)", "Sys.symlink('a', 'b')", "file.symlink('a', 'b')", "system(\"bash -c x\")",
            "system2(\"bash\", 'x')", "'/bin/sh'", "'/tmp/x'", "'/opt/R'")
  writeLines(c(paste0("f", seq_along(bait), " <- function() ", bait),
               "## comentario com /opt/ e mclapply nao conta"), file.path(cd, "models", "iscas.R"))
  dir.create(file.path(cd, "outro"), showWarnings = FALSE)
  writeLines("x <- '/tmp/fora-do-codigo-de-produto'", file.path(cd, "outro", "ok.R"))
  h <- e$pc_bundle_portability_grep(cd)
  expect_identical(h$path, rep("models/iscas.R", length(bait)))
  expect_identical(h$linha, seq_along(bait))
  ## o lancador scripts/prevcarga (sem extensao) tambem e varrido
  cat("x <- '/opt/R'\n", file = file.path(cd, "scripts", "prevcarga"), append = TRUE)
  expect_true("scripts/prevcarga" %in% e$pc_bundle_portability_grep(cd)$path)
})

test_that("[FORBIDDEN] [BUNDLE] windows_hygiene: build_bundle_windows.ps1 e ASCII, pina o instalador por sha256 antes de executar e nunca altera variavel de ambiente persistente", {
  ps <- file.path(root, "scripts", "tools", "build_bundle_windows.ps1")
  b <- readBin(ps, "raw", file.size(ps))
  expect_true(all(as.integer(b) < 128L))
  expect_false(as.raw(13L) %in% b)                                  # LF
  l <- readLines(ps)
  code <- l[!grepl("^\\s*#", l)]
  expect_false(any(grepl("SetEnvironmentVariable|\\bsetx\\b", code)))
  expect_false(any(grepl("\\$env:\\w+\\s*=", code)))
  i_sha <- grep("Get-FileHash", l)[1L]; i_run <- grep("Start-Process", l)[1L]
  expect_true(!is.na(i_sha) && !is.na(i_run) && i_sha < i_run)       # o hash e conferido antes de executar o instalador
  expect_true(any(grepl("^\\$RSha256\\s*=\\s*'[0-9a-f]{64}'", l)))
  expect_true(any(grepl("R-4\\.5\\.3-win\\.exe", l)))
  expect_true(any(grepl("/CURRENTUSER", l, fixed = TRUE)))
})

## O install.ps1 instala o lancador e o prevcarga-sync a partir de code\install\ do bundle. Na 0.1.0 o build nao os copiava e o CI nao viu:
## ele rodava o install\install.ps1 do repo, cujo fallback acha install\prevcarga.cmd ao lado. Aqui: o build copia os tres antes do RUNTIME.json.
bh_win_launchers <- c("prevcarga.cmd", "prevcarga-sync.ps1", "prevcarga-sync.cmd")
bh_copied_before_write <- function(l) {
  i_cp <- grep("foreach \\(\\$n in 'prevcarga.cmd', 'prevcarga-sync.ps1', 'prevcarga-sync.cmd'\\)", l)
  i_do <- grep("Copy-Item -LiteralPath (Join-Path $ArchSrc ('install\\' + $n)) -Destination (Join-Path $CodeDir ('install\\' + $n))", l, fixed = TRUE)
  i_wr <- grep("$BM, 'write'", l, fixed = TRUE)
  length(i_cp) == 1L && length(i_do) == 1L && length(i_wr) == 1L && i_cp < i_do && i_do < i_wr
}
test_that("[FORBIDDEN] [BUNDLE] windows_hygiene: build_bundle_windows.ps1 poe prevcarga.cmd e prevcarga-sync.{ps1,cmd} em code/install antes do RUNTIME.json; CONTROL sem a copia", {
  l <- readLines(file.path(root, "scripts", "tools", "build_bundle_windows.ps1"))
  expect_true(bh_copied_before_write(l))
  expect_false(bh_copied_before_write(l[!grepl("Copy-Item -LiteralPath (Join-Path $ArchSrc ('install", l, fixed = TRUE)]))   # CONTROL
  for (n in bh_win_launchers) expect_true(file.exists(file.path(root, "install", n)), info = n)
  ps1 <- readLines(file.path(root, "install", "install.ps1"))
  for (n in bh_win_launchers) expect_true(any(grepl(n, ps1, fixed = TRUE)), info = n)   # o instalador procura os mesmos nomes
})

test_that("[FORBIDDEN] [BUNDLE] windows_hygiene: o zip Windows de dist/ traz o lancador e o prevcarga-sync em code/install", {
  z <- bh_win_zip()
  skip_if(is.na(z), "sem zip Windows em dist/")
  nm <- utils::unzip(z, list = TRUE)$Name
  for (n in bh_win_launchers) expect_true(paste0("prevcarga/code/install/", n) %in% nm, info = n)
})
