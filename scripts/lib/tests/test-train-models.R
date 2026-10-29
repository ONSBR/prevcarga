## test-train-models.R -- verbo `train-models` (M5.4; ADR-0023, ADR-0038, ADR-0040): seletores de subset, --dry-run,
## --jobs PSOCK e runtime in-process. Gates de PROCESSO (`Rscript --vanilla <copia>/scripts/prevcarga train-models ...`)
## sobre instalacao-copia em tempdir + deck sintetico (fixture_store.R); hooks de worker so por script-filho (ctx$worker_hooks;
## o pacote nunca le env). Tudo em tempdir (nunca em models/*/models/ nem data/). Cada FORBIDDEN (delta 0) tem o seu CONTROL.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
lib <- file.path(pc_root(), "scripts", "lib")

fx <- new.env()
sys.source(file.path(lib, "errors.R"), envir = fx)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = fx)

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
AREAS <- c("A", "B", "C", "D")
RF <- "random-forest"   # nome real so para os seletores da matriz (topologia real)

## --- instalacao-copia: topologia {X: A..D} e um unico membro (toy-rng) -------------------------------
## O RF real ainda nao treina via prevcarga_train (docs/estado/training.md INTERINO F1: cuts$spec e janela history_days vs
## train_origin - history_days); os plugins-brinquedo sao os do M5.3. `toy-rng` = toy-30 origin_agnostic que CONSOME o RNG
## com a semente do spec (uses_rng), para o braco CONTROL da semente por worker.
MD <- "toy-rng"
toy_rng_files <- list("R/zz_rng.R" = c(
  "toy_training <- function(features, params, seed) {",
  "  if (!isTRUE(getOption('tm.skip_seed'))) set.seed(seed)   # hook de teste: worker que omite a semente",
  "  m <- features$m; k <- min(params$k, nrow(m))",
  "  colMeans(m[seq(nrow(m) - k + 1L, nrow(m)), , drop = FALSE]) + stats::rnorm(ncol(m))",
  "}"))
mk_root <- function(extra = function(r) NULL) {
  pc_install_copy(mutate = function(r) {
    tv <- pc_toy_variant("toy-30", run_yaml = list(backtest = list(protocol_default = "fixed"), horizon_days = 2L, history_days = list(train = 10L, infer = 3L)),
                         descriptor = list(uses_rng = TRUE), files = toy_rng_files, name = MD)
    file.copy(c(file.path(tv, MD), file.path(tv, "_toy-engine")), file.path(r, "models"), recursive = TRUE)
    writeLines(c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:",
                 sprintf("  X: {areas: [%s], loss: PX}", paste(AREAS, collapse = ", ")),
                 "order: [X]", "members:", sprintf("  - {sigla: M1, mdir: %s, exogena: null}", MD)),
               file.path(r, "config", "topology.yaml"))
    extra(r)
  })
}
root_small <- mk_root()
root_real <- pc_install_copy()

deck <- local({
  d <- tempfile("tm-deck-")
  fx$prevcarga_fixture_deck(d, seed = 11, areas = AREAS, days = seq(O - 29L, O, by = "day"),
                            load_until = lubridate::ymd_hm("2026-10-01 09:00", tz = TZ))
  d
})

tm <- function(args, root = root_small, state = pc5_state_tmp(parent.frame()), data = TRUE, env = character(),
               fmt = "json") {
  a <- c("train-models", args, paste0("--state=", state), if (isTRUE(data)) paste0("--data=", deck),
         paste0("--format=", fmt))
  r <- pc_cli(root, a, env = env)
  r$state <- state
  r$json <- if (identical(fmt, "json") && length(r$stdout)) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE) else NULL
  r
}
cells_of <- function(r) r$json$result$cells
keys_of <- function(state) {
  lp <- list.files(file.path(state, "artifacts"), pattern = "^latest\\.txt$", recursive = TRUE)
  sort(dirname(lp), method = "radix")
}
art_hashes <- function(state) {
  fs <- list.files(file.path(state, "artifacts"), pattern = "^artifact\\.rds$", recursive = TRUE)
  fs <- fs[order(fs, method = "radix")]
  stats::setNames(vapply(fs, function(f) digest::digest(file = file.path(state, "artifacts", f), algo = "sha256"), ""),
                  sub("/v_[0-9a-f]+/artifact\\.rds$", "", fs))
}

## --- descoberta, gramatica e catalogo -------------------------------------------------------------------------------
test_that("[FORBIDDEN] prevcarga_verbs contem train-models e o Usage monta a gramatica do verbo", {
  e <- new.env(); for (m in c("errors", "cli")) sys.source(file.path(lib, paste0(m, ".R")), envir = e)
  v <- e$prevcarga_verbs_discover(dir = file.path(lib, "cli"))
  expect_true("train-models" %in% v$verb)
  expect_identical(v$owner[v$verb == "train-models"], "M5")
  u <- e$prevcarga_usage(v)
  for (tok in c("train-models", "--subsistema=<s>", "--areas=<a,b>", "--dry-run", "--jobs=<n>", "--force", "--horizon=<h>",
                "--exogena=<e>"))
    expect_true(grepl(tok, u, fixed = TRUE), info = tok)
})
test_that("[CONTROL] sem o arquivo do verbo a gramatica nao o traz", {
  e <- new.env(); for (m in c("errors", "cli")) sys.source(file.path(lib, paste0(m, ".R")), envir = e)
  d <- tempfile("cli-"); dir.create(d)
  v <- e$prevcarga_verbs_discover(dir = d, planned = character())
  expect_false(grepl("--dry-run", e$prevcarga_usage(v), fixed = TRUE))
})
test_that("errors/train_models.tsv carrega via prevcarga_errors_load sem id duplicado (ASCII, LF)", {
  e <- new.env(); sys.source(file.path(lib, "errors.R"), envir = e)
  cat <- e$prevcarga_errors_load(dir = file.path(lib, "errors"), refresh = TRUE)
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_identical(cat$class[cat$id == "contract.area_selector_conflict"], "contract")
  expect_identical(cat$class[cat$id == "runtime.worker_unfixable"], "runtime")
  raw <- readBin(file.path(lib, "errors", "train_models.tsv"), "raw", 1e5)
  expect_true(all(as.integer(raw) < 128L)); expect_false(13L %in% as.integer(raw))
})

## --- seletores (matriz in-process sobre a topologia real; nenhuma sigla literal) ---------------------------------------
mod <- new.env()
for (m in c("errors", "runtime", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
            "features", "registry", "training")) sys.source(file.path(lib, paste0(m, ".R")), envir = mod)
topo <- mod$prevcarga_topology(root = pc_root())
real_ctx <- list(root = pc_root())
mat <- function(...) mod$prevcarga_train_matrix(list(...), real_ctx)

test_that("seletores: --subsistema = folhas-filhas, --areas = lista, nenhum = 24 folhas, e o token do subsistema nunca entra", {
  s1 <- topo$order[[1L]]
  m1 <- mat(`--subsistema` = s1, `--model` = RF)
  expect_setequal(m1$area, topo$subsystems[[s1]]$areas)
  expect_false(s1 %in% m1$area)
  tri <- topo$leaves[c(1L, 3L, 5L)]
  expect_setequal(mat(`--areas` = paste(tri, collapse = ","), `--model` = RF)$area, tri)
  expect_setequal(unique(mat(`--model` = RF)$area), topo$leaves)
  expect_length(topo$leaves, 24L)
  one <- mat(`--area` = topo$order[[1L]], `--model` = RF)          # --area aceita o no (a propria chave)
  expect_identical(one$area, topo$order[[1L]])
})
test_that("[CONTROL] matriz default cobre todos os membros e e ordenada por (model, variant, area)", {
  m <- mat()
  mem <- mod$prevcarga_members(root = pc_root())
  expect_identical(nrow(m), nrow(mem) * length(topo$leaves))
  ord <- order(m$model, ifelse(is.na(m$variant), "", m$variant), m$area, method = "radix")
  expect_identical(ord, seq_len(nrow(m)))
  expect_setequal(mat(`--model` = "dessem-linear/MED")$variant, "MED")
})
test_that("combinacao de seletores => exit 2 contract.area_selector_conflict; area inexistente => exit 4 topology", {
  bad <- function(args) tm(c(args, "--dry-run"), root = root_real, data = FALSE)
  r <- bad(c(paste0("--area=", topo$leaves[[1L]]), paste0("--subsistema=", topo$order[[1L]])))
  expect_identical(r$status, 2L)
  expect_identical(r$json$error$id, "contract.area_selector_conflict")
  r <- bad(c(paste0("--area=", topo$leaves[[1L]]), paste0("--areas=", topo$leaves[[2L]])))
  expect_identical(r$status, 2L)
  r <- bad("--area=INEXISTENTE")
  expect_identical(r$status, 4L)
  expect_identical(r$json$error$id, "topology.area_undeclared")
  expect_true(any(grepl("[topology] (exit 4)", tm(c("--area=INEXISTENTE", "--dry-run"), root = root_real, data = FALSE,
                                                  fmt = "text")$stderr, fixed = TRUE)))
  expect_identical(bad("--subsistema=INEXISTENTE")$status, 4L)
  expect_identical(bad("--model=nao-existe")$status, unname(fx$prevcarga_exit_codes[["config"]]))
})

## --- AC1: escopo de 1 celula ------------------------------------------------------------------------------------------
test_that("[FORBIDDEN] --model random-forest --area A treina so random-forest/A e latest.txt aponta v_<hash> existente", {
  r <- tm(c(paste0("--model=", MD), "--area=A", paste0("--origin=", O)))
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  expect_setequal(keys_of(r$state), file.path(MD, "A"))
  lp <- file.path(r$state, "artifacts", MD, "A", "latest.txt")
  v <- trimws(readLines(lp, warn = FALSE))
  expect_true(grepl("^v_", v)); expect_true(dir.exists(file.path(dirname(lp), v)))
  cl <- cells_of(r)
  expect_length(cl, 1L)
  expect_identical(cl[[1L]]$status, "trained"); expect_identical(cl[[1L]]$area, "A")
  expect_identical(cl[[1L]]$split, "origin_agnostic")
  ## origin_agnostic: train_origin = O-1-H (H=2), last_data_day = O-1 (lag 0)
  expect_identical(cl[[1L]]$train_origin, as.character(O - 3L)); expect_identical(cl[[1L]]$last_data_day, as.character(O - 1L))
  r2 <- tm(c(paste0("--model=", MD), "--area=A", paste0("--origin=", O)), state = r$state)
  expect_identical(cells_of(r2)[[1L]]$status, "reused")
})
test_that("[CONTROL] sem --area o verbo treina as 4 areas (uma impl que ignore --area reprovaria o teste acima)", {
  r <- tm(c(paste0("--model=", MD), paste0("--origin=", O)))
  expect_identical(r$status, 0L)
  expect_setequal(keys_of(r$state), file.path(MD, AREAS))
})

## --- AC3: --dry-run ----------------------------------------------------------------------------------------------------
test_that("[FORBIDDEN] --dry-run: um JSON, cells ordenadas e planned, state intacto; versao-alvo = a do treino real", {
  st <- pc5_state_tmp()
  before <- pc5_tree_hash(st)
  r <- tm(c("--dry-run", paste0("--origin=", O)), state = st)
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  expect_length(r$stdout, 1L)
  expect_identical(pc5_tree_hash(st), before)
  expect_length(list.files(st, recursive = TRUE, all.files = TRUE), 0L)
  cl <- cells_of(r)
  expect_identical(vapply(cl, function(c) c$area, ""), AREAS)
  expect_true(all(vapply(cl, function(c) identical(c$status, "planned"), NA)))
  expect_true(all(c("model", "area", "split", "train_origin", "last_data_day", "version", "status") %in% names(cl[[1L]])))
  expect_null(cl[[1L]]$variant)
  expect_identical(r$json$result$dry_run, TRUE); expect_identical(r$json$result$origin, as.character(O))
  real <- tm(paste0("--origin=", O))
  expect_identical(vapply(cells_of(real), function(c) c$version, ""), vapply(cl, function(c) c$version, ""))
})
test_that("[CONTROL] sem --dry-run o mesmo comando grava no state", {
  r <- tm(paste0("--origin=", O, collapse = ""))
  expect_gt(length(list.files(r$state, recursive = TRUE)), 0L)
})
test_that("--origin omitido = dia do ultimo intervalo de carga do deck (default do M2)", {
  r <- tm("--dry-run")
  expect_identical(r$status, 0L)
  expect_identical(r$json$result$origin, as.character(O))
})

## --- AC4: --jobs 1 x --jobs 2 e hooks de worker (script-filho; ctx$worker_hooks) ------------------------------------------
test_that("[FORBIDDEN] --jobs 1 e --jobs 2 (PSOCK): mesmo sha256 por artifact.rds e mesma ordem de cells", {
  a <- tm(c(paste0("--model=", MD), paste0("--origin=", O), "--jobs=1"))
  b <- tm(c(paste0("--model=", MD), paste0("--origin=", O), "--jobs=2"))
  expect_identical(a$status, 0L); expect_identical(b$status, 0L)
  expect_length(art_hashes(a$state), 4L)
  expect_identical(art_hashes(a$state), art_hashes(b$state))
  ord <- function(r) vapply(cells_of(r), function(c) paste(c$model, c$area), "")
  expect_identical(ord(a), ord(b))
  expect_identical(ord(b), paste(MD, AREAS))
})

child <- function(mode, jobs = 2L, state = pc5_state_tmp(parent.frame())) {
  s <- tempfile("tm-child-", fileext = ".R")
  hook <- c(
    noseed = "list(pre_runtime = function() options(tm.skip_seed = TRUE))",
    unfixable = "list(pre_runtime = function() assign('prevcarga_runtime_enforce', function(...) prevcarga_abort('runtime.tz_unfixable', where = 'TZ'), envir = globalenv()))",
    none = "NULL")[[mode]]
  writeLines(c(
    sprintf("lib <- %s; options(prevcarga.lib = lib)", deparse(lib)),
    "for (m in c('errors.R','runtime.R','config.R','topology.R','schema_long.R','provenance.R','deck_reader.R','dates.R','validation.R','features.R','registry.R','training.R')) source(file.path(lib, m))",
    "prevcarga_runtime_enforce()",
    sprintf("ctx <- prevcarga_ctx(list(`--data` = %s, `--state` = %s), %s, 'train-models')", deparse(deck), deparse(state), deparse(root_small)),
    "ctx$lib_dir <- lib; ctx$code_commit <- 'c0ffee'; ctx$renv_lock_hash <- 'ab12'",
    sprintf("ctx$worker_hooks <- %s", hook),
    sprintf("cells <- prevcarga_train_matrix(list(`--model` = %s), ctx)", deparse(MD)),
    sprintf("r <- tryCatch(prevcarga_train_models(cells, ctx, origin = lubridate::ymd(%s), jobs = %dL), prevcarga_error = function(e) e)",
            deparse(as.character(O)), as.integer(jobs)),
    "if (inherits(r, 'prevcarga_error')) { writeLines(paste('ERR', r$id, r$exit_code, conditionMessage(r))); quit(status = 0) }",
    "writeLines(paste('OK', nrow(r)))"), s)
  out <- pc_rscript(s)
  unlink(s)
  c(out, list(state = state))
}

test_that("[FORBIDDEN] hook neutro no worker: --jobs 2 do script-filho == serial (delta 0)", {
  a <- child("none", jobs = 1L); b <- child("none", jobs = 2L)
  expect_identical(a$stdout, "OK 4"); expect_identical(b$stdout, "OK 4")
  expect_identical(art_hashes(a$state), art_hashes(b$state))
})
test_that("[CONTROL] worker sem set.seed: o toy-rng diverge do serial (a semente por worker e o que garante delta 0)", {
  a <- child("none", jobs = 1L); b <- child("noseed", jobs = 2L)
  expect_identical(b$stdout, "OK 4")
  expect_false(identical(art_hashes(a$state), art_hashes(b$state)))
})
test_that("runtime nao fixavel no worker => condicao [prevcarga/runtime] original no pai, exit 7, nada gravado", {
  r <- child("unfixable", jobs = 2L)
  expect_match(r$stdout[[1L]], "^ERR runtime\\.tz_unfixable 7 \\[prevcarga/runtime\\]")
  expect_length(list.files(r$state, recursive = TRUE, all.files = TRUE), 0L)
})

## --- AC5: runtime in-process --------------------------------------------------------------------------------------------
test_that("[FORBIDDEN] shell sujo (TZ/LC_ALL/OPENBLAS) == shell limpo: mesmo sha256 do artifact.rds", {
  dirty <- pc_dirty_env()
  skip_if(is.na(dirty[["LC_ALL"]]), "host sem locale UTF-8 alternativo para sujar LC_ALL")
  clean <- c(TZ = NA, LC_ALL = NA, OMP_NUM_THREADS = NA, OPENBLAS_NUM_THREADS = NA)
  args <- c(paste0("--model=", MD), "--area=A", paste0("--origin=", O))
  a <- tm(args, env = dirty); b <- tm(args, env = clean)
  expect_identical(a$status, 0L); expect_identical(b$status, 0L)
  expect_length(art_hashes(a$state), 1L)
  expect_identical(art_hashes(a$state), art_hashes(b$state))
})
test_that("[CONTROL] runtime nao fixavel no pai (copia com setter de TZ quebrado): exit 7, stdout vazio, nada gravado", {
  root_bad <- pc_install_copy(broken_tz = TRUE)
  st <- pc5_state_tmp()
  r <- tm(c(paste0("--model=", MD), "--area=A", paste0("--origin=", O)), root = root_bad, state = st, fmt = "text")
  expect_identical(r$status, 7L)
  expect_length(r$stdout, 0L)
  expect_length(list.files(st, recursive = TRUE), 0L)
})
