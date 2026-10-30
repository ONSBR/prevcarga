## test-doctor-bundle.R -- extensoes do doctor (M8.7): plataforma, integridade do bundle instalado, runtime_id, versao do R e artefatos
## sincronizados (prevcarga_platform_check em runtime.R; cli/doctor.R). Processo real sobre instalacao-copia em tempdir(); o bundle
## instalado e uma FIXTURE (RUNTIME.json + files[] + state), nunca um bundle de verdade. Sem env vars no codigo de produto.

H <- pc_source_lib(c("errors", "runtime"))
host <- H$.prevcarga_host_platform()
rver <- as.character(getRversion())
root <- pc_install_copy()
state <- file.path(dirname(root), "state")
parse_json <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
row <- function(j, nm) Filter(function(x) identical(x$check, nm), j$result$checks)[[1L]]
bundle_items <- c("bundle.plataforma", "bundle.r_version", "bundle.integridade", "bundle.versao", "bundle.current",
                  "artefatos.presenca", "artefatos.sha")
doctor <- function(fmt = "json") pc_cli(root, c("doctor", paste0("--format=", fmt), paste0("--state=", state)))

write_runtime <- function(platform = host, r_version = rver, version = "1.2.3", bad_file = FALSE) {
  f <- "config/VERSION"
  sha <- if (file.exists(file.path(root, f))) digest::digest(file = file.path(root, f), algo = "sha256")
         else { dir.create(file.path(root, "config"), showWarnings = FALSE); writeLines("1.2.3", file.path(root, f)); digest::digest(file = file.path(root, f), algo = "sha256") }
  if (bad_file) sha <- strrep("0", 64L)
  x <- list(version = version, platform = platform, r_version = r_version, files = list(list(path = f, sha256 = sha)))
  writeLines(as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE)), file.path(root, "RUNTIME.json"))
}
put_artifact <- function(missing = FALSE, bad_sha = FALSE) {
  unlink(state, recursive = TRUE)
  a <- file.path(state, "artifacts", "lgbm", "SECO")
  v <- file.path(a, "v_0123456789abcdef")
  dir.create(v, recursive = TRUE)
  writeLines("v_0123456789abcdef", file.path(a, "latest.txt"))
  writeLines("conteudo", file.path(v, "artifact.rds"))
  sha <- if (bad_sha) strrep("f", 64L) else digest::digest(file = file.path(v, "artifact.rds"), algo = "sha256")
  writeLines(sprintf("%s  artifact.rds", sha), file.path(v, "SHA256SUMS"))
  if (missing) unlink(v, recursive = TRUE)
}

test_that("arvore de dev: itens de bundle N/A (nao-instalado), exit 0, texto ASCII com banner", {
  unlink(file.path(root, "RUNTIME.json"))
  r <- doctor(); j <- parse_json(r)
  expect_identical(r$status, 0L)
  for (n in bundle_items) {
    expect_identical(row(j, n)$status, "N/A", info = n)
    expect_identical(row(j, n)$found, "N/A (nao-instalado)", info = n)
  }
  expect_null(j$result$runtime_id)
  expect_identical(j$result$platform, host); expect_identical(j$result$install_root, root)
  t <- doctor("text"); expect_identical(t$status, 0L)
  pc_assert_ascii(t$raw)
  expect_true(any(grepl("N/A (nao-instalado)", t$stdout, fixed = TRUE)))
  expect_true(length(t$stdout) > 3L && any(grepl("prevcarga", t$stdout[1:3], ignore.case = TRUE)))   # banner
})

test_that("bundle instalado conforme: campos JSON, itens PASS, exit 0", {
  write_runtime(); put_artifact()
  r <- doctor(); j <- parse_json(r)
  expect_identical(r$status, 0L)
  for (n in c("bundle.plataforma", "bundle.r_version", "bundle.integridade", "bundle.versao", "artefatos.presenca", "artefatos.sha"))
    expect_identical(row(j, n)$status, "PASS", info = n)
  res <- j$result
  expect_identical(res$version, "1.2.3"); expect_identical(res$platform, host)
  expect_match(res$runtime_id, "^[0-9a-f]{64}$"); expect_identical(res$install_root, root)
  expect_identical(res$state_dir, normalizePath(state, winslash = "/", mustWork = FALSE))
  expect_length(res$artefatos, 1L); expect_identical(res$artefatos[[1L]]$status, "ok")
  expect_identical(res$artefatos[[1L]]$versao, "v_0123456789abcdef")
  expect_true(is.numeric(res$state_bytes$artifacts) && res$state_bytes$artifacts > 0)
  t <- doctor("text"); pc_assert_ascii(t$raw)
})

test_that("[FORBIDDEN] latest.txt aponta para v_<hash> ausente => FAIL classe 13 e exit 13 [CONTROL: presente passa]", {
  write_runtime(); put_artifact(missing = TRUE)
  r <- doctor(); j <- parse_json(r)
  expect_identical(row(j, "artefatos.presenca")$status, "FAIL"); expect_identical(r$status, 13L)
  expect_identical(j$result$artefatos[[1L]]$status, "ausente")
  put_artifact(); expect_identical(doctor()$status, 0L)
})

test_that("[FORBIDDEN] sha divergente do SHA256SUMS local => FAIL classe 9 e exit 9", {
  write_runtime(); put_artifact(bad_sha = TRUE)
  r <- doctor(); j <- parse_json(r)
  expect_identical(row(j, "artefatos.sha")$status, "FAIL"); expect_identical(r$status, 9L)
  expect_identical(j$result$artefatos[[1L]]$arquivo, "artifact.rds")
})

test_that("[FORBIDDEN] platform/r_version do RUNTIME.json divergentes e arquivo adulterado => FAIL classe 6 (exit 6)", {
  put_artifact()
  write_runtime(platform = "windows-x86_64-fake")
  r <- doctor(); expect_identical(row(parse_json(r), "bundle.plataforma")$status, "FAIL"); expect_identical(r$status, 6L)
  write_runtime(r_version = "3.0.0")
  r <- doctor(); expect_identical(row(parse_json(r), "bundle.r_version")$status, "FAIL"); expect_identical(r$status, 6L)
  write_runtime(bad_file = TRUE)
  r <- doctor(); expect_identical(row(parse_json(r), "bundle.integridade")$status, "FAIL"); expect_identical(r$status, 6L)
  write_runtime(); expect_identical(doctor()$status, 0L)                                  # CONTROL
})

test_that("platform_check in-process: glibc < 2.35 reprova no Linux; current divergente avisa", {
  d <- tempfile("pc-pcheck-"); b <- file.path(d, "versions", "9.9.9"); dir.create(file.path(b, "code"), recursive = TRUE)
  writeLines(as.character(jsonlite::toJSON(list(version = "9.9.9", platform = "linux-x86_64", r_version = rver, files = list()),
                                           auto_unbox = TRUE)), file.path(b, "RUNTIME.json"))
  ctx <- list(root = file.path(b, "code"), state_dir = file.path(d, "state"), version = "x")
  bad <- H$prevcarga_platform_check(ctx, host = "linux-x86_64", glibc = "2.31", r_version = rver)
  expect_identical(bad$rows$status[bad$rows$check == "bundle.plataforma"], "FAIL")
  ok <- H$prevcarga_platform_check(ctx, host = "linux-x86_64", glibc = "2.35", r_version = rver)
  expect_identical(ok$rows$status[ok$rows$check == "bundle.plataforma"], "PASS")
  writeLines("9.9.9", file.path(d, "current"))
  same <- H$prevcarga_platform_check(ctx, host = "linux-x86_64", glibc = "2.39", r_version = rver)
  expect_identical(same$rows$status[same$rows$check == "bundle.current"], "PASS")
  writeLines("1.0.0", file.path(d, "current"))
  cur <- H$prevcarga_platform_check(ctx, host = "linux-x86_64", glibc = "2.39", r_version = rver)
  expect_identical(cur$rows$status[cur$rows$check == "bundle.current"], "WARN")
  expect_identical(cur$campos$install_root, b)
})
