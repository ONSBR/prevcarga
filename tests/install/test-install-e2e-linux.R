## test-install-e2e-linux.R -- instalador Linux (M8.3): hash obrigatorio, manifesto invalido, state/ intocado, retencao N=2 e
## rollback, config.yaml/PATH/doctor, lancador sem `current`, contrato de erros (ADR-0040). Cada FORBIDDEN tem um CONTROL.
## Roda `install/install.sh` REAL em raizes temporarias dentro de sandbox/ (nunca ~/.local, nunca a rede, nunca o upload);
## os bundles sao reempacotamentos do candidato de dist/ (helper-install.R). Ao passar grava o recibo `install-e2e-linux`.
library(testthat)
pci_skip_unless_ready()
pci_ensure_catalog()
CATALOG_IDS <- utils::read.delim(file.path(pci_root(), "install", "error-catalog.tsv"), sep = "\t", quote = "", comment.char = "",
                                 colClasses = "character")$id

B <- list(v1 = pc_fixture_bundle("v1"), v2 = pc_fixture_bundle("v2"), v3 = pc_fixture_bundle("v3"),
          v3bad = pc_fixture_bundle("v3", corrupt = "manifest"), v3cut = pc_fixture_bundle("v3", corrupt = "truncate"))
inst <- function(root, b, ..., sha = b$sha256, script = pci_installer()) {
  a <- c("--bundle", b$path, if (!is.null(sha)) c("--sha256", sha), "--root", root, "--format", "json", ...)
  r <- pc_install(a, script = script)
  r$json <- tryCatch(pci_json(r$stdout), error = function(e) NULL)
  r
}
cur <- function(root) if (file.exists(file.path(root, "current"))) readLines(file.path(root, "current"), warn = FALSE) else NA_character_
vers <- function(root) sort(list.files(file.path(root, "versions"), all.files = TRUE, no.. = TRUE), method = "radix")

test_that("[AC1] sem sha256 ou com sha256 errado: exit 9 integrity, mesma classe, raiz byte-identica; CONTROL: sha certo instala v3", {
  R <- pc_fixture_install_root()
  expect_identical(inst(R, B$v2)$status, 0L)
  expect_identical(cur(R), "v2")
  antes <- pc_tree_sha(R)
  a1 <- inst(R, B$v3, sha = NULL)                                       # braco 1: sem --sha256 e sem SHA256SUMS ao lado
  a2 <- inst(R, B$v3, sha = strrep("0", 64L))                           # braco 2: sha errado
  for (a in list(a1, a2)) {
    expect_identical(a$status, 9L); expect_identical(a$stderr, "")
    expect_false(a$json$ok); expect_identical(a$json$exit_code, 9L); expect_identical(a$json$error$class, "integrity")
  }
  expect_identical(a1$json$error$id, "install.sha_missing")
  expect_identical(a2$json$error$id, "install.sha_mismatch")
  expect_identical(a1$json$error$class, a2$json$error$class)
  expect_identical(pc_tree_sha(R), antes)                               # FORBIDDEN: nada extraido, nada escrito
  expect_identical(cur(R), "v2")
  ctl <- inst(R, B$v3)                                                  # CONTROL
  expect_identical(ctl$status, 0L); expect_identical(cur(R), "v3")
  expect_false(identical(pc_tree_sha(R), antes))                        # o checksum de arvore acusa uma instalacao real
  ## SHA256SUMS ao lado do bundle vale como fonte do sha
  d <- pci_work_dir(); file.copy(B$v1$path, d)
  writeLines(sprintf("%s  %s", B$v1$sha256, basename(B$v1$path)), file.path(d, "SHA256SUMS"))
  R2 <- pc_fixture_install_root()
  r <- pc_install(c("--bundle", file.path(d, basename(B$v1$path)), "--root", R2, "--format", "json"))
  expect_identical(r$status, 0L); expect_identical(cur(R2), "v1")
})

test_that("[AC2] manifesto com files[].sha256 errado: exit 6 bundle, current intacto, sem versions/v3 nem stage; CONTROL: variante swap-first reprova", {
  inv <- function(script, R) {                                           # a invariante do AC, falsificavel

    expect_identical(inst(R, B$v2, script = script)$status, 0L)
    r <- inst(R, B$v3bad, script = script)
    list(r = r, R = R, ok = identical(r$status, 6L) && identical(r$json$error$id, "install.manifest_invalid") &&
           identical(cur(R), "v2") && !dir.exists(file.path(R, "versions", "v3")) && !dir.exists(file.path(R, "versions", ".stage-v3")))
  }
  real <- inv(pci_installer(), pc_fixture_install_root())
  expect_true(real$ok)
  expect_identical(real$r$json$error$class, "bundle"); expect_identical(real$r$stderr, "")
  expect_identical(vers(real$R), "v2")
  bad <- inv(pc_installer_variant_swap_first(), pc_fixture_install_root())                         # CONTROL: o teste TEM dentes
  expect_false(bad$ok)
  ## extracao truncada (sha confere, tar falha): mesmo contrato, stage removido
  R <- pc_fixture_install_root(); inst(R, B$v2)
  r <- inst(R, B$v3cut)
  expect_identical(r$status, 6L); expect_identical(r$json$error$id, "install.manifest_invalid")
  expect_identical(cur(R), "v2"); expect_identical(vers(R), "v2")
})

test_that("[AC3] state/ intocado em upgrade, rollback e uninstall; --purge o remove; --state em versions/ => exit 8; CONTROL: 1 arquivo muda o checksum", {
  R <- pc_fixture_install_root()
  expect_identical(inst(R, B$v1)$status, 0L)
  st <- file.path(R, "state")
  dir.create(file.path(st, "artifacts", "RF", "v_abc"), recursive = TRUE)
  writeLines("x", file.path(st, "artifacts", "RF", "v_abc", "params.json")); writeLines("v_abc", file.path(st, "artifacts", "RF", "latest.txt"))
  t0 <- pc_tree_sha(st)
  expect_identical(inst(R, B$v2)$status, 0L);                            expect_identical(pc_tree_sha(st), t0)   # upgrade
  rb <- pc_install(c("--rollback", "--root", R, "--format", "json"))
  expect_identical(rb$status, 0L); expect_identical(cur(R), "v1");       expect_identical(pc_tree_sha(st), t0)   # rollback
  un <- pc_install(c("--uninstall", "--yes", "--root", R, "--format", "json"))
  expect_identical(un$status, 0L)
  expect_true(dir.exists(st)); expect_identical(pc_tree_sha(st), t0)                                           # uninstall sem purge
  expect_false(any(file.exists(file.path(R, c("current", "config.yaml")))))
  expect_false(any(dir.exists(file.path(R, c("versions", "bin")))))
  ## CONTROL: o checksum acusa 1 arquivo gravado entre antes e depois
  writeLines("y", file.path(st, "intruso.txt"))
  expect_false(identical(pc_tree_sha(st), t0))
  ## --purge remove state/
  R2 <- pc_fixture_install_root(); inst(R2, B$v1)
  dir.create(file.path(R2, "state", "artifacts"), recursive = TRUE)
  expect_identical(pc_install(c("--uninstall", "--yes", "--purge", "--root", R2, "--format", "json"))$status, 0L)
  expect_false(dir.exists(file.path(R2, "state")))
  ## uninstall sem --yes e recusado (uso)
  R3 <- pc_fixture_install_root(); inst(R3, B$v1)
  r <- pc_install(c("--uninstall", "--root", R3, "--format", "json"))
  expect_identical(r$status, 8L); expect_true(dir.exists(file.path(R3, "versions")))
  ## --state dentro de versions/ (antes de qualquer escrita)
  R4 <- pc_fixture_install_root(); inst(R4, B$v2)
  a <- pc_tree_sha(R4)
  r <- inst(R4, B$v2, "--state", file.path(R4, "versions", "v2", "x"))
  expect_identical(r$status, 8L); expect_identical(r$json$error$id, "install.state_inside_versions"); expect_identical(r$json$error$class, "config")
  expect_identical(pc_tree_sha(R4), a)
})

test_that("[AC4] v1,v2,v3 em sequencia: so v2 e v3 (N=2); rollback => v2 e `--version` v2; 2o rollback exit 8 sem mexer em current", {
  R <- pc_fixture_install_root()
  for (v in c("v1", "v2", "v3")) expect_identical(inst(R, B[[v]])$status, 0L)
  expect_identical(vers(R), c("v2", "v3"))
  expect_identical(cur(R), "v3")
  launcher <- file.path(R, "bin", "prevcarga")
  expect_match(pci_run(launcher, "--version")$stdout, "v3", fixed = TRUE)
  rb <- pc_install(c("--rollback", "--root", R, "--format", "json"))
  expect_identical(rb$status, 0L); expect_identical(cur(R), "v2")
  expect_match(pci_run(launcher, "--version")$stdout, "v2", fixed = TRUE)
  rb2 <- pc_install(c("--rollback", "--root", R, "--format", "json"))
  expect_identical(rb2$status, 8L); expect_identical(pci_json(rb2$stdout)$error$id, "install.no_previous_version")
  expect_identical(cur(R), "v2")
  ## instalar de novo apos o rollback: v4 vira current, v2 vira a anterior e v3 (abandonada) sai
  expect_identical(inst(R, pc_fixture_bundle("v4"))$status, 0L)
  expect_identical(vers(R), c("v2", "v4")); expect_identical(cur(R), "v4")
  ## reinstalar a mesma versao nao perde a anterior
  expect_identical(inst(R, pc_fixture_bundle("v4"))$status, 0L)
  expect_identical(vers(R), c("v2", "v4"))
})

test_that("[AC6] --data/--state/--add-to-path: config.yaml valido (schema M1.4a), PATH idempotente, doctor da instalacao; CONTROL: sem current o lancador sai 6", {
  R <- pc_fixture_install_root(); w <- dirname(R)
  D <- file.path(w, "deck"); S <- file.path(w, "meu-estado"); P <- file.path(w, "profile")
  writeLines("# perfil do usuario", P)
  go <- function() inst(R, B$v2, "--data", D, "--state", S, "--add-to-path", "--profile", P)
  r <- go()
  expect_identical(r$status, 0L)
  e <- pci_env(); sys.source(file.path(pci_root(), "scripts", "lib", "config.R"), envir = e)
  cfg <- e$prevcarga_config_load(file.path(R, "config.yaml"))
  expect_identical(cfg$data_dir, D); expect_identical(cfg$state_dir, S)
  expect_true(dir.exists(S))
  prof <- readLines(P)
  expect_identical(sum(prof == "# >>> prevcarga >>>"), 1L); expect_true("# perfil do usuario" %in% prof)
  expect_true(any(grepl(file.path(R, "bin"), prof, fixed = TRUE)))
  expect_identical(go()$status, 0L)                                    # reinstalar: idempotente
  expect_identical(sum(readLines(P) == "# >>> prevcarga >>>"), 1L)
  cfg2 <- e$prevcarga_config_load(file.path(R, "config.yaml"))        # upgrade preserva as chaves
  expect_identical(cfg2$data_dir, D); expect_identical(cfg2$state_dir, S)
  ## doctor pelo lancador e o veredito do instalador
  d <- pci_run(file.path(R, "bin", "prevcarga"), c("doctor", "--format", "json"))
  expect_identical(d$status, 0L)
  dj <- pci_json(d$stdout)$result
  expect_identical(dj$install_root, R); expect_identical(dj$state_dir, S)
  expect_true(r$json$result$doctor$ok)
  expect_identical(r$json$result$doctor$result$install_root, R)
  ## CONTROL: sem current o lancador recusa (exit 6), 1a linha de stderr ASCII fixa
  unlink(file.path(R, "current"))
  l <- pci_run(file.path(R, "bin", "prevcarga"), "doctor")
  expect_identical(l$status, 6L); expect_identical(l$stdout, "")
  expect_identical(strsplit(l$stderr, "\n")[[1L]][1L], "prevcarga: erro [bundle] (exit 6)")
  expect_false(any(as.integer(charToRaw(l$stderr)) > 127L))
  ## current apontando para versao inexistente
  writeLines("v9", file.path(R, "current"))
  l2 <- pci_run(file.path(R, "bin", "prevcarga"), "doctor")
  expect_identical(l2$status, 6L); expect_identical(strsplit(l2$stderr, "\n")[[1L]][1L], "prevcarga: erro [bundle] (exit 6)")
  ## uninstall remove o bloco de PATH
  expect_identical(pc_install(c("--uninstall", "--yes", "--root", R, "--profile", P))$status, 0L)
  expect_false(any(grepl("prevcarga", readLines(P), fixed = TRUE)))
})

test_that("[AC7] exits 8/9/10 com --format json: um JSON, ok=false, exit_code == status, error.id no catalogo gerado, stderr vazio; texto: ASCII e 1a linha fixa", {
  R <- pc_fixture_install_root()
  casos <- list(
    c("8",  "--bogus"),
    c("9",  "--bundle", B$v1$path, "--root", R),                                    # sem sha256
    c("10", "--version", "v9", "--release-base", "https://127.0.0.1:9", "--root", R))
  for (cs in casos) {
    code <- as.integer(cs[[1L]]); args <- cs[-1L]
    j <- pc_install(c(args, "--format", "json"))
    expect_identical(j$status, code)
    expect_identical(j$stderr, "")
    one <- pci_json(j$stdout)                                                      # JSON unico (fromJSON falha com mais de um)
    expect_false(one$ok); expect_identical(one$exit_code, code)
    expect_true(one$error$id %in% CATALOG_IDS)
    expect_false(any(as.integer(charToRaw(j$stdout)) > 127L))
    t <- pc_install(args)
    expect_identical(t$status, code); expect_identical(t$stdout, "")
    l1 <- strsplit(t$stderr, "\n")[[1L]][1L]
    expect_match(l1, sprintf("^prevcarga: erro \\[(config|integrity|network)\\] \\(exit %d\\)$", code))
    expect_false(any(as.integer(charToRaw(t$stderr)) > 127L))
  }
  expect_false(dir.exists(R))                                                      # nenhuma falha escreveu na raiz
  ## classes por exit: 8 config, 9 integrity, 10 network
  expect_identical(pci_json(pc_install(c("--bogus", "--format", "json"))$stdout)$error$id, "install.usage")
  expect_identical(pci_json(pc_install(c("--version", "v9", "--release-base", "https://127.0.0.1:9", "--root", R, "--format", "json"))$stdout)$error$id,
                   "install.download_failed")
})

test_that("[AC7] catalogo: install.tsv e unico (loader), todo id install.* esta no catalogo gerado e o fallback embutido tem o mesmo texto", {
  tsv <- file.path(pci_root(), "scripts", "lib", "errors", "install.tsv")
  e <- pci_env(); sys.source(file.path(pci_root(), "scripts", "lib", "errors.R"), envir = e)
  cat_df <- e$prevcarga_errors_load(dir = file.path(pci_root(), "scripts", "lib", "errors"), refresh = TRUE)
  mine <- utils::read.delim(tsv, sep = "\t", quote = "", comment.char = "", colClasses = "character")
  expect_true(all(mine$id %in% cat_df$id)); expect_true(all(mine$id %in% CATALOG_IDS))
  expect_false(any(as.integer(readBin(tsv, "raw", file.size(tsv))) > 127L))
  src <- readLines(pci_installer())
  fb <- regmatches(src, regexpr("printf 'install\\.[a-z_]+\\\\t.*\\\\n'", src))
  fb <- sub("^printf '", "", sub("\\\\n'$", "", fb))
  fb <- gsub("\\\\t", "\t", fb)
  expect_setequal(sub("\t.*$", "", fb), mine$id)
  expect_setequal(fb, apply(mine, 1L, paste, collapse = "\t"))
  ## o preflight recusa plataforma sem zstd (PATH sem a ferramenta) com exit 6; --skip-platform-check vira aviso
  bin <- pci_work_dir(); dir.create(file.path(bin, "p"))
  for (t in c("dirname", "readlink", "awk", "cut", "sed", "tr", "uname", "ldd", "grep", "getconf", "sort", "head", "cat", "tar", "sha256sum", "mktemp", "basename", "rm"))
    if (nzchar(w <- Sys.which(t))) file.symlink(w, file.path(bin, "p", t))
  run <- function(args) {
    so <- tempfile(); se <- tempfile(); on.exit(unlink(c(so, se)))
    st <- system2("env", c("-i", paste0("PATH=", file.path(bin, "p")), "/bin/sh", shQuote(pci_installer()), vapply(args, shQuote, "")), stdout = so, stderr = se)
    list(status = as.integer(st), stdout = readLines(so, warn = FALSE), stderr = readLines(se, warn = FALSE))
  }
  r <- run(c("--bundle", B$v1$path, "--root", file.path(bin, "R"), "--format", "json"))
  expect_identical(r$status, 6L); expect_identical(pci_json(paste(r$stdout, collapse = ""))$error$id, "install.preflight_failed")
  expect_false(dir.exists(file.path(bin, "R")))
  r2 <- run(c("--bundle", B$v1$path, "--root", file.path(bin, "R"), "--skip-platform-check"))
  expect_match(r2$stderr[1L], "^prevcarga: aviso: preflight ignorado")
  expect_identical(r2$status, 9L)                                                  # seguiu adiante: parou no sha256 ausente
})

## --- recibo ------------------------------------------------------------------------------------------------------------
.pci_find_results <- function(rep) {
  if (inherits(rep, "ListReporter")) return(rep$get_results())
  for (r in rep$reporters) { x <- .pci_find_results(r); if (!is.null(x)) return(x) }
  NULL
}
test_that("[recibo] install-e2e-linux gravado com o veredito dos testes deste arquivo (sem recibo se algum foi pulado)", {
  res <- .pci_find_results(testthat::get_reporter())
  skip_if(is.null(res), "reporter sem ListReporter: recibo nao gravado (rode via testthat::test_file(..., reporter = ListReporter$new()))")
  mine <- Filter(function(r) !grepl("^\\[recibo\\]", r$test), res)
  bad <- vapply(mine, function(r) any(vapply(r$results, function(x) inherits(x, c("expectation_failure", "expectation_error")), NA)), NA)
  skip_if(any(vapply(mine, function(r) any(vapply(r$results, function(x) inherits(x, "expectation_skip"), NA)), NA)),
          "algum teste foi pulado: sem recibo")
  e <- pci_env(); sys.source(file.path(pci_root(), "scripts", "tools", "receipts.R"), envir = e)
  ## runtime_id do candidato REAL (nao da base reempacotada): extrai so o RUNTIME.json do tarball
  x <- pci_work_dir()
  pci_sh(sprintf("zstd -dc %s | tar -x -C %s prevcarga/RUNTIME.json", shQuote(pci_candidate()), shQuote(x)))
  out <- e$pc_receipt_write("install-e2e-linux", pci_candidate(), "linux-x86_64", if (any(bad)) "FAIL" else "PASS",
                            dir = file.path(pci_root(), "tests", ".receipts"), extracted_root = file.path(x, "prevcarga"))
  expect_true(file.exists(out))
  expect_identical(e$pc_receipt_read(digest::digest(file = pci_candidate(), algo = "sha256"), "install-e2e-linux",
                                   dir = file.path(pci_root(), "tests", ".receipts"))$resultado, if (any(bad)) "FAIL" else "PASS")
})
