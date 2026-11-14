## test-install-etapas-linux.R -- banner, etapas 'prevcarga: [n/6] ...', explicacoes e proximos passos do install.sh: so em texto e
## com stderr num terminal. Com stderr redirecionado (testes, CI, scripts) ou em --format json: stderr VAZIO e stdout so a linha
## final / o JSON (o contrato de stderr do ADR-0040 continua).
## Bundle FALSO minimo (RUNTIME.json + Rscript stub que sai 0): nao precisa do candidato de dist/, nem de rede. CONTROL: sob um pty
## (`script`), o mesmo instalador imprime banner, as 6 etapas e os proximos passos -- o detector tem dentes.
library(testthat)
testthat::skip_on_os(c("windows", "mac"))
if (!all(nzchar(Sys.which(c("zstd", "tar", "sha256sum", "sh"))))) testthat::skip("zstd/tar/sha256sum ausentes no host")

fake_bundle <- function(env = parent.frame()) {
  d <- pci_work_dir(env, "pc-etapas-")
  top <- file.path(d, "src", "prevcarga")
  dir.create(file.path(top, "runtime", "bin"), recursive = TRUE)
  writeLines('{"files":[{"path":"runtime/bin/Rscript","sha256":"x"}],"version":"9.9.9"}', file.path(top, "RUNTIME.json"))
  writeLines(c("#!/bin/sh", "exit 0"), file.path(top, "runtime", "bin", "Rscript"))
  Sys.chmod(file.path(top, "runtime", "bin", "Rscript"), "0755")
  tb <- file.path(d, "prevcarga-9.9.9-linux-x86_64.tar.zst")
  pci_sh(sprintf("cd %s && tar --zstd -cf %s prevcarga", shQuote(file.path(d, "src")), shQuote(tb)))
  sha <- strsplit(pci_sh(sprintf("sha256sum %s", shQuote(tb))), " ", fixed = TRUE)[[1]][1]
  list(path = tb, sha256 = sha, root = file.path(d, "R"))
}
args_for <- function(b, fmt) c("--bundle", b$path, "--sha256", b$sha256, "--root", b$root, "--skip-platform-check", "--format", fmt)
step_lines <- function(x) grep("^prevcarga: \\[[0-9]/6\\] ", gsub("\r", "", unlist(strsplit(x, "\n", fixed = TRUE))), value = TRUE)
out_lines <- function(x) gsub("\r", "", unlist(strsplit(x, "\n", fixed = TRUE)))
STATIC <- local({ e <- new.env(); sys.source(file.path(pci_root(), "scripts", "lib", "banner.R"), envir = e); e$.prevcarga_banner_static() })

test_that("stderr redirecionado (texto e json): instala e NENHUMA linha 'prevcarga: [' em stderr", {
  for (fmt in c("text", "json")) {
    b <- fake_bundle()
    r <- pc_install(args_for(b, fmt))
    expect_identical(r$status, 0L, info = paste(fmt, r$stderr))
    expect_length(step_lines(r$stderr), 0L)
    expect_false(grepl("prevcarga: [", r$stderr, fixed = TRUE), info = fmt)
    # banner, explicacoes e proximos passos seguem a MESMA regra: com stderr redirecionado, stderr fica vazio
    expect_identical(r$stderr, "", info = fmt)
    o <- out_lines(r$stdout); o <- o[nzchar(o)]
    expect_length(o, 1L)
    if (fmt == "text") expect_match(o, "^prevcarga: instalado 9\\.9\\.9 em ") else expect_match(o, '^\\{"ok":true')
    expect_false(any(grepl("proximos passos|o que vai acontecer|Previsao de carga do SIN|\\| instalador \\|", c(r$stdout, r$stderr))), info = fmt)
    expect_false(any(STATIC[nzchar(STATIC)] %in% out_lines(paste(r$stdout, r$stderr))), info = fmt)
    expect_identical(readLines(file.path(b$root, "current"), warn = FALSE), "9.9.9")
  }
})

test_that("CONTROL: com stderr num pty (script) o texto mostra as 6 etapas em ordem; json continua sem etapa", {
  if (!nzchar(Sys.which("script"))) skip("util-linux script ausente: sem pty para o CONTROL")
  run_pty <- function(a) {
    cmd <- paste("env -i PATH=/usr/local/bin:/usr/bin:/bin sh", shQuote(pci_installer()), paste(vapply(a, shQuote, ""), collapse = " "))
    out <- suppressWarnings(system2("script", c("-qec", shQuote(cmd), "/dev/null"), stdout = TRUE, stderr = TRUE))
    paste(out, collapse = "\n")
  }
  b <- fake_bundle()
  st <- step_lines(run_pty(args_for(b, "text")))
  expect_identical(sub("^prevcarga: \\[([0-9])/6\\].*$", "\\1", st), as.character(1:6))
  expect_match(st[1], "bundle local prevcarga-9.9.9-linux-x86_64.tar.zst", fixed = TRUE)
  expect_match(st[4], "validando 1 arquivos do RUNTIME.json", fixed = TRUE)
  expect_match(st[5], "ativando a versao 9.9.9", fixed = TRUE)
  b2 <- fake_bundle()
  pj <- run_pty(args_for(b2, "json"))
  expect_length(step_lines(pj), 0L)
  expect_false(grepl("proximos passos|o que vai acontecer|Previsao de carga do SIN|\\| instalador \\|", pj))
})

test_that("CONTROL: sob pty o texto mostra o banner da CLI (nome + curva + eixo + identidade + versao), o que vai acontecer, explicacoes e proximos passos", {
  if (!nzchar(Sys.which("script"))) skip("util-linux script ausente: sem pty para o CONTROL")
  run_pty <- function(a) {
    cmd <- paste("env -i PATH=/usr/local/bin:/usr/bin:/bin sh", shQuote(pci_installer()), paste(vapply(a, shQuote, ""), collapse = " "))
    out_lines(paste(suppressWarnings(system2("script", c("-qec", shQuote(cmd), "/dev/null"), stdout = TRUE, stderr = TRUE)), collapse = "\n"))
  }
  b <- fake_bundle()
  x <- run_pty(args_for(b, "text"))
  i <- which(x == STATIC[1]); expect_length(i, 1L)
  expect_identical(x[i + 0:13], STATIC)                                # nome + curva + eixo + identidade, na ordem (banner da CLI)
  expect_identical(x[i + 14L], "  prevcarga 9.9.9 | instalador | linux-x86_64")   # versao inferida do nome do bundle
  io <- grep("^o que vai acontecer: instala sem administrador em ", x); expect_length(io, 1L)
  expect_true(grepl(b$root, x[io], fixed = TRUE))
  s1 <- grep("^prevcarga: \\[1/6\\]", x); expect_lt(io, s1)                  # banner e intro antes da etapa 1
  expect_true(any(grepl("confere o sha256 de cada arquivo do bundle antes de ativar", x, fixed = TRUE)))
  fin <- grep("^prevcarga: instalado 9\\.9\\.9 em ", x); expect_length(fin, 1L)
  pp <- which(x == "proximos passos:"); expect_length(pp, 1L); expect_lt(fin, pp)
  tail <- x[pp:length(x)]
  expect_true(any(grepl("prevcarga doctor", tail, fixed = TRUE)))
  expect_true(any(grepl(paste0("ponha ", b$root, "/bin no PATH"), tail, fixed = TRUE)))   # sem --add-to-path
  expect_true(any(grepl("aponte o deck: prevcarga set-data-dir (grava data_train_dir", tail, fixed = TRUE)))   # sem data_train_dir
  # deck ja configurado (--data-train): nao sugere configurar; mostra o data_train_dir
  b2 <- fake_bundle(); dd <- file.path(dirname(b2$root), "deck"); dir.create(dd)
  y <- run_pty(c(args_for(b2, "text"), "--data-train", dd))
  expect_false(any(grepl("aponte o deck", y, fixed = TRUE)))
  expect_true(any(grepl(paste0("deck configurado: data_train_dir = ", dd), y, fixed = TRUE)))
  # 2a versao: a anterior fica guardada para --rollback
  b3 <- fake_bundle()
  run_pty(c(args_for(b3, "text"), "--version", "9.9.10"))
  z <- run_pty(args_for(b3, "text"))
  expect_true(any(grepl("a versao anterior (9.9.10) fica guardada: install.sh --rollback", z, fixed = TRUE)))
  # --add-to-path: aponta o perfil (lido so no login), sem prometer que um terminal novo resolve; upgrade SEM a flag reconhece o
  # bloco gravado antes e nao manda configurar de novo. CONTROL: perfil sem o bloco volta a sugerir o export.
  b4 <- fake_bundle(); pf <- file.path(dirname(b4$root), "profile")
  w1 <- run_pty(c(args_for(b4, "text"), "--add-to-path", "--profile", pf))
  w2 <- run_pty(c(args_for(b4, "text"), "--version", "9.9.10", "--profile", pf))
  for (w in list(w1, w2)) {
    expect_true(any(grepl(paste0(b4$root, "/bin esta no PATH pelo bloco em ", pf), w, fixed = TRUE)))
    expect_true(any(grepl(paste0("neste terminal rode: . ", pf), w, fixed = TRUE)))
    expect_false(any(grepl("ponha .*/bin no PATH|terminal novo", w)))
  }
  writeLines("# perfil sem o bloco do prevcarga", pf)
  w3 <- run_pty(c(args_for(b4, "text"), "--version", "9.9.11", "--profile", pf))
  expect_true(any(grepl(paste0("ponha ", b4$root, "/bin no PATH"), w3, fixed = TRUE)))
  expect_false(any(grepl("esta no PATH pelo bloco", w3, fixed = TRUE)))
  expect_true(all(utf8ToInt(paste(c(x, y, z, w1, w2, w3), collapse = "")) <= 127L))   # ASCII puro
})

test_that("[AC4] install.sh grava data_train_dir/artifacts_dir/data_inference_dir; --data e apelido de --data-train com aviso ASCII em stderr", {
  b <- fake_bundle(); w <- dirname(b$root)
  dt <- file.path(w, "t"); da <- file.path(w, "a"); di <- file.path(w, "i")
  r <- pc_install(c(args_for(b, "json"), "--data-train", dt, "--data-artifacts", da, "--data-inference", di))
  expect_identical(r$status, 0L, info = r$stderr)
  y <- readLines(file.path(b$root, "config.yaml"))
  expect_true(all(c(paste0("data_train_dir: '", dt, "'"), paste0("artifacts_dir: '", da, "'"),
                    paste0("data_inference_dir: '", di, "'")) %in% y))
  expect_false(any(startsWith(y, "data_dir:")))
  expect_false(grepl("apelido", r$stderr, fixed = TRUE))
  b2 <- fake_bundle(); d2 <- file.path(dirname(b2$root), "d")
  r2 <- pc_install(c(args_for(b2, "json"), "--data", d2))              # CONTROL: o apelido avisa e grava a chave nova
  expect_identical(r2$status, 0L, info = r2$stderr)
  expect_match(r2$stderr, "--data e apelido de --data-train", fixed = TRUE)
  expect_true(all(utf8ToInt(r2$stderr) <= 127L))
  y2 <- readLines(file.path(b2$root, "config.yaml"))
  expect_true(paste0("data_train_dir: '", d2, "'") %in% y2); expect_false(any(startsWith(y2, "data_dir:")))
  r3 <- pc_install(c(args_for(b2, "json"), "--data-train", file.path(d2, "x"), "--data", d2))   # conflito => install.usage
  expect_false(identical(r3$status, 0L)); expect_match(r3$stdout, "install.usage", fixed = TRUE)
})
