## test-install-github-releases.R -- layout de download do instalador: GitHub Releases x bucket (install.sh executado; install.ps1 estatico).
## O `curl` do filho e um stub no PATH que serve arquivos de um diretorio local e registra cada URL pedida: nenhuma rede real.
## O bundle de teste tem sha256 errado de proposito, entao toda execucao para em install.sha_mismatch (exit 9) depois de baixar
## o bundle e o SHA256SUMS -- o que prova o layout das URLs sem extrair nada nem escrever na raiz.
library(testthat)

pcg_world <- function() {
  d <- tempfile("pcg-"); dir.create(d)
  srv <- file.path(d, "srv"); bin <- file.path(d, "bin"); dir.create(srv); dir.create(bin)
  log <- file.path(d, "urls.log"); file.create(log)
  stub <- c("#!/bin/sh",
            "## stub de curl: serve <srv>/<host>/<caminho> no -o e registra a URL (ultimo argumento)",
            "out=''; url=''",
            "while [ $# -gt 0 ]; do case \"$1\" in -o) out=\"$2\"; shift 2;; --proto) shift 2;; -*) shift;; *) url=\"$1\"; shift;; esac; done",
            sprintf("printf '%%s\\n' \"$url\" >> '%s'", log),
            sprintf("f='%s'/\"${url#https://}\"", srv),
            "[ -f \"$f\" ] || exit 22",
            "cp \"$f\" \"$out\"")
  writeLines(stub, file.path(bin, "curl")); Sys.chmod(file.path(bin, "curl"), "0755")
  list(dir = d, srv = srv, bin = bin, log = log)
}

pcg_put <- function(w, url, txt) {
  f <- file.path(w$srv, sub("^https://", "", url)); dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines(txt, f)
}

## Publica a versao v num layout ("github" ou "bucket"); SHA256SUMS com hash errado (forca exit 9 depois dos downloads).
pcg_publish <- function(w, base, v, layout) {
  nome <- sprintf("prevcarga-%s-linux-x86_64.tar.zst", v)
  rel <- if (layout == "github") sprintf("%s/releases/download/v%s", base, v) else sprintf("%s/releases/%s", base, v)
  pcg_put(w, paste0(rel, "/", nome), "bundle-falso")
  pcg_put(w, paste0(rel, "/SHA256SUMS"), paste0(strrep("0", 64), "  ", nome))
  pcg_put(w, if (layout == "github") paste0(base, "/releases/latest/download/latest.txt") else paste0(base, "/releases/latest.txt"), v)
}

pcg_run <- function(w, args) {
  so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
  envs <- c(paste0("PATH=", w$bin, ":/usr/local/bin:/usr/bin:/bin"), paste0("HOME=", w$dir))
  st <- system2("env", c("-i", envs, "sh", shQuote(pci_installer()), vapply(c(args, "--skip-platform-check", "--format", "json"), shQuote, "")),
                stdout = so, stderr = se, env = character())
  list(status = as.integer(st), json = pci_json(paste(readLines(so, warn = FALSE), collapse = "\n")), urls = readLines(w$log, warn = FALSE))
}

test_that("GitHub Releases: latest pelo redirect /releases/latest/download/latest.txt e assets da tag v<v>", {
  pci_ensure_catalog()
  w <- pcg_world(); base <- "https://github.com/ons/prevcarga"
  pcg_publish(w, base, "1.2.3", "github")
  R <- file.path(w$dir, "R")
  r <- pcg_run(w, c("--release-base", base, "--root", R))
  expect_identical(r$status, 9L); expect_identical(r$json$error$id, "install.sha_mismatch")
  expect_identical(r$urls, c("https://github.com/ons/prevcarga/releases/latest/download/latest.txt",
                             "https://github.com/ons/prevcarga/releases/download/v1.2.3/prevcarga-1.2.3-linux-x86_64.tar.zst",
                             "https://github.com/ons/prevcarga/releases/download/v1.2.3/SHA256SUMS"))
  expect_false(dir.exists(R))                                                       # nada escrito na raiz antes do sha256
})

test_that("GitHub Releases: --version explicita nao consulta latest; barra final da base e tolerada", {
  pci_ensure_catalog()
  w <- pcg_world(); base <- "https://github.com/ons/prevcarga"
  pcg_publish(w, base, "2.0.0", "github")
  r <- pcg_run(w, c("--version", "2.0.0", "--release-base", paste0(base, "/"), "--root", file.path(w$dir, "R")))
  expect_identical(r$status, 9L)
  expect_identical(r$urls, c("https://github.com/ons/prevcarga/releases/download/v2.0.0/prevcarga-2.0.0-linux-x86_64.tar.zst",
                             "https://github.com/ons/prevcarga/releases/download/v2.0.0/SHA256SUMS"))
})

test_that("bucket: qualquer outra base mantem <base>/releases/latest.txt e <base>/releases/<v>/", {
  pci_ensure_catalog()
  w <- pcg_world(); base <- "https://bucket.example"
  pcg_publish(w, base, "1.2.3", "bucket")
  r <- pcg_run(w, c("--release-base", base, "--root", file.path(w$dir, "R")))
  expect_identical(r$status, 9L)
  expect_identical(r$urls, c("https://bucket.example/releases/latest.txt",
                             "https://bucket.example/releases/1.2.3/prevcarga-1.2.3-linux-x86_64.tar.zst",
                             "https://bucket.example/releases/1.2.3/SHA256SUMS"))
})

test_that("CONTROL: base github.com com caminho mais fundo nao e repositorio e cai no layout de bucket", {
  pci_ensure_catalog()
  w <- pcg_world(); base <- "https://github.com/ons/prevcarga/extra"
  pcg_publish(w, base, "1.2.3", "bucket")
  r <- pcg_run(w, c("--release-base", base, "--root", file.path(w$dir, "R")))
  expect_identical(r$urls[[1L]], "https://github.com/ons/prevcarga/extra/releases/latest.txt")
})

test_that("GitHub Releases: asset ausente sai como install.download_failed (exit 10) citando a URL do asset", {
  pci_ensure_catalog()
  w <- pcg_world(); base <- "https://github.com/ons/prevcarga"
  pcg_put(w, paste0(base, "/releases/latest/download/latest.txt"), "9.9.9")
  r <- pcg_run(w, c("--release-base", base, "--root", file.path(w$dir, "R")))
  expect_identical(r$status, 10L); expect_identical(r$json$error$id, "install.download_failed")
  expect_match(r$json$error$where, "releases/download/v9.9.9/prevcarga-9.9.9-linux-x86_64.tar.zst", fixed = TRUE)
})

test_that("default sem --release-base: GitHub Releases de onsbr/prevcarga", {
  pci_ensure_catalog()
  w <- pcg_world(); base <- "https://github.com/onsbr/prevcarga"
  pcg_publish(w, base, "1.4.0", "github")
  r <- pcg_run(w, c("--root", file.path(w$dir, "R")))
  expect_identical(r$status, 9L)
  expect_identical(r$urls, c("https://github.com/onsbr/prevcarga/releases/latest/download/latest.txt",
                             "https://github.com/onsbr/prevcarga/releases/download/v1.4.0/prevcarga-1.4.0-linux-x86_64.tar.zst",
                             "https://github.com/onsbr/prevcarga/releases/download/v1.4.0/SHA256SUMS"))
})

test_that("install.ps1 (estatico): mesmo layout -- regex de repositorio, latest por /releases/latest/download/ e tag v<v>", {
  ps1 <- readLines(file.path(pci_root(), "install", "install.ps1"), warn = FALSE)
  expect_true(any(grepl("'^https://github\\.com/[^/]+/[^/]+$'", ps1, fixed = TRUE)))
  expect_true(any(grepl("'/releases/latest/download/latest.txt'", ps1, fixed = TRUE)))
  expect_true(any(grepl("'/releases/download/v' + $script:Version + '/'", ps1, fixed = TRUE)))
  expect_true(any(grepl("'/releases/' + $script:Version + '/'", ps1, fixed = TRUE)))      # bucket continua
  expect_true(any(grepl("$base = 'https://github.com/onsbr/prevcarga'", ps1, fixed = TRUE))) # mesmo default do install.sh
})
