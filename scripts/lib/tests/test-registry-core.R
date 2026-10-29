## test-registry-core.R -- nucleo do registry local no layout do bucket (M5.2). Tudo em tempdir.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "provenance", "topology", "registry"), env = environment())
reg_env <- environment()   # onde vivem as funcoes: o mock de file.rename precisa entrar aqui

reg_ctx <- function(state) list(state_dir = state)
reg_art <- function(x = 1) list(a = x, b = c("u", "v"))
reg_pub <- function(key, v, x = 1, force = FALSE)
  prevcarga_registry_publish(reg_art(x), list(note = paste0("m-", v)), key, v, force = force)
expect_reg <- function(expr, id, exit) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$exit_code, as.integer(exit))
  invisible(cond)
}

test_that("AC1: chave com variante so quando member$exogena nao e vazia; corpo sem nome de modelo", {
  st <- pc5_state_tmp(); ctx <- reg_ctx(st)
  root <- normalizePath(file.path(st, "artifacts"), winslash = "/")
  fx <- list(sigla = "XX", mdir = "toy-med", exogena = "MED")
  expect_identical(prevcarga_registry_key(fx, "SECO", ctx), file.path(root, "toy-med", "MED", "SECO"))
  for (md in c("random-forest", "lgbm", "holt-winters")) {
    k <- prevcarga_registry_key(data.frame(sigla = "R", mdir = md, exogena = NA_character_), "SECO", ctx)
    expect_identical(k, file.path(root, md, "SECO"))
  }
  expect_identical(prevcarga_registry_key(list(mdir = "m", exogena = ""), "S", ctx), file.path(root, "m", "S"))
  expect_reg(prevcarga_registry_key(list(mdir = "m"), "S", ctx), "contract.variant_required", 2L)
  body <- paste(deparse(body(prevcarga_registry_key)), collapse = "\n")
  expect_false(grepl("model_id|grepl|switch|random-forest|lgbm|holt-winters|dessem", body))
  expect_reg(prevcarga_registry_key(list(mdir = "m", exogena = NA_character_), "../..", ctx),
             "contract.path_outside_root", 2L)
})

test_that("AC2: resolve latest/version/pin; inexistente (13) e malformada (2); nunca vazio", {
  st <- pc5_state_tmp(); key <- prevcarga_registry_key(list(mdir = "m", exogena = NA_character_), "A", reg_ctx(st))
  dir.create(file.path(key, "v_aa"), recursive = TRUE); dir.create(file.path(key, "v_bb"))
  writeLines("v_aa", file.path(key, "latest.txt")); writeLines("v_bb", file.path(key, ".pin"))
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_aa")
  expect_identical(prevcarga_resolve_version(key, "version", "v_bb"), "v_bb")
  expect_identical(prevcarga_resolve_version(key, "pin"), "v_bb")
  expect_reg(prevcarga_resolve_version(key, "version", "v_00"), "state.artifact_missing", 13L)
  expect_reg(prevcarga_resolve_version(key, "version", "../x"), "contract.version_malformed", 2L)
  expect_reg(prevcarga_resolve_version(key, "version", NULL), "contract.version_malformed", 2L)
  expect_reg(prevcarga_resolve_version(key, "version", "v_AA"), "contract.version_malformed", 2L)
  expect_reg(prevcarga_resolve_version(file.path(st, "artifacts", "m", "ZZ"), "latest"), "state.artifact_missing", 13L)
  unlink(file.path(key, ".pin"))
  expect_reg(prevcarga_resolve_version(key, "pin"), "state.pin_missing", 13L)
  expect_reg(prevcarga_resolve_version(file.path(st, "..", "fora"), "latest", ctx = reg_ctx(st)),
             "contract.path_outside_root", 2L)
})

test_that("AC3: load integro identical; 1 byte alterado => exit 9 nomeando o arquivo (CONTROL)", {
  st <- pc5_state_tmp(); key <- file.path(st, "artifacts", "m", "A")
  r <- reg_pub(key, "v_0123abcd", 7)
  expect_false(r$reused)
  expect_identical(prevcarga_load_artifact(key, "v_0123abcd"), reg_art(7))
  expect_true(all(c("artifact.rds", "run_manifest.json", "SHA256SUMS") %in% list.files(r$path)))
  cp <- file.path(pc5_state_tmp(), "artifacts", "m", "A")
  dir.create(dirname(cp), recursive = TRUE); file.copy(key, dirname(cp), recursive = TRUE)
  f <- file.path(cp, "v_0123abcd", "artifact.rds")
  b <- readBin(f, "raw", file.size(f)); b[length(b)] <- as.raw(bitwXor(as.integer(b[length(b)]), 1L))
  writeBin(b, f)
  cond <- expect_reg(prevcarga_load_artifact(cp, "v_0123abcd"), "integrity.artifact_hash", 9L)
  expect_match(cond$where, "artifact.rds", fixed = TRUE)
})

test_that("AC4: rename que falha deixa o anterior intacto; CONTROL sem mock promove e grava .history", {
  st <- pc5_state_tmp(); key <- file.path(st, "artifacts", "m", "A")
  reg_pub(key, "v_aa", 1); reg_pub(key, "v_bb", 2)
  prevcarga_registry_promote_latest(key, "v_aa")
  before <- pc5_tree_hash(key)
  assign("file.rename", function(from, to) FALSE, envir = reg_env)       # = pc5_mock_rename(fail = TRUE) no env do registry
  expect_error(reg_pub(key, "v_aa", 99, force = TRUE), class = "prevcarga_error")
  expect_error(prevcarga_registry_promote_latest(key, "v_bb"), class = "prevcarga_error")
  rm("file.rename", envir = reg_env)
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_aa")
  orphans <- list.files(key, pattern = "^[.]tmp-", all.files = TRUE)
  unlink(file.path(key, orphans), recursive = TRUE)
  expect_identical(pc5_tree_hash(key), before)
  expect_identical(prevcarga_load_artifact(key, "v_aa"), reg_art(1))
})

test_that("AC4b: falha so no rename final do force restaura o anterior; staging orfao limpo", {
  st <- pc5_state_tmp(); key <- file.path(st, "artifacts", "m", "A")
  reg_pub(key, "v_aa", 1)
  before <- pc5_tree_hash(key)
  assign("file.rename", function(from, to) {   # falha so o novo(.tmp-*) -> v_aa; antigo->.tmp-old e a restauracao passam
    if (basename(to) == "v_aa" && !grepl("tmp-old", from, fixed = TRUE)) FALSE else base::file.rename(from, to)
  }, envir = reg_env)
  expect_reg(reg_pub(key, "v_aa", 99, force = TRUE), "state.promote_failed", 13L)
  rm("file.rename", envir = reg_env)
  expect_identical(pc5_tree_hash(key), before)
  expect_identical(list.files(key, pattern = "^[.]tmp-", all.files = TRUE), character())
})

test_that("AC4c CONTROL: sem mock publish usa staging irmao, force troca, promote atualiza latest e .history", {
  st <- pc5_state_tmp(); key <- file.path(st, "artifacts", "m", "A")
  reg_pub(key, "v_aa", 1)
  expect_true(reg_pub(key, "v_aa", 5)$reused)
  expect_identical(prevcarga_load_artifact(key, "v_aa"), reg_art(1))
  expect_false(reg_pub(key, "v_aa", 5, force = TRUE)$reused)
  expect_identical(prevcarga_load_artifact(key, "v_aa"), reg_art(5))
  reg_pub(key, "v_bb", 2)
  prevcarga_registry_promote_latest(key, "v_aa"); prevcarga_registry_promote_latest(key, "v_bb")
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_bb")
  expect_identical(readLines(file.path(key, ".history")), c("v_aa", "v_bb"))
  expect_identical(list.files(key, pattern = "^[.]tmp-", all.files = TRUE), character())
  expect_reg(prevcarga_registry_promote_latest(key, "v_cc"), "state.artifact_missing", 13L)
  expect_identical(prevcarga_resolve_version(key, "latest"), "v_bb")
})

test_that("AC5: copia byte a byte (layout do bucket, sem .pin/.history) resolve e carrega identical; sem CR", {
  st <- pc5_state_tmp(); key <- file.path(st, "artifacts", "m", "MED", "A")
  reg_pub(key, "v_0f", 3); prevcarga_registry_promote_latest(key, "v_0f")
  dst <- pc5_state_tmp(); k2 <- file.path(dst, "artifacts", "m", "MED", "A")
  dir.create(k2, recursive = TRUE)
  file.copy(file.path(key, c("latest.txt", "v_0f")), k2, recursive = TRUE)
  expect_false(file.exists(file.path(k2, ".history")))
  v <- prevcarga_resolve_version(k2, "latest", ctx = reg_ctx(dst))
  expect_identical(v, "v_0f")
  expect_identical(prevcarga_load_artifact(k2, v, ctx = reg_ctx(dst)), prevcarga_load_artifact(key, v))
  for (f in c(file.path(key, c("latest.txt", ".history")), file.path(key, "v_0f", c("SHA256SUMS", "run_manifest.json")))) {
    b <- readBin(f, "raw", file.size(f))
    expect_false(as.raw(13L) %in% b, info = f)
    expect_identical(b[length(b)], as.raw(10L), info = f)
  }
})

test_that("catalogo registry_core.tsv carrega sem id duplicado, 7 ids nas classes 13/2/9", {
  cat <- prevcarga_errors_load()
  want <- c(state.artifact_missing = "state", state.pin_missing = "state", state.promote_failed = "state",
            contract.version_malformed = "contract", contract.variant_required = "contract",
            contract.path_outside_root = "contract", integrity.artifact_hash = "integrity")
  expect_false(anyDuplicated(cat$id) > 0)
  expect_identical(unname(cat$class[match(names(want), cat$id)]), unname(want))
  code <- prevcarga_exit_codes[unname(want)]
  expect_identical(unname(code), c(13L, 13L, 13L, 2L, 2L, 2L, 9L)[c(1, 2, 3, 4, 5, 6, 7)])
  raw <- readBin(file.path(pc_root(), "scripts", "lib", "errors", "registry_core.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(as.raw(13L) %in% raw)
})
