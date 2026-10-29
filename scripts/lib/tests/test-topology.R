## test-topology.R -- topology.R (M1.2): baseline, injecao por argumento (CONTROL), invalidos, khat_range, run.yaml, members.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "topology"), env = environment())

fx <- function(f) testthat::test_path("fixtures", f)
topo <- function(...) prevcarga_topology(..., root = pc_root())
expect_pc_error <- function(expr, id, where) {
  cond <- tryCatch({ force(expr); NULL }, prevcarga_error = function(e) e)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$class_name, "contract")
  expect_identical(cond$exit_code, 2L)
  expect_match(conditionMessage(cond), "\\[prevcarga/contract\\]")
  expect_match(cond$where, where, fixed = TRUE)
}
## copia do repo real (config + models) em tempdir, para gerar desvios sem tocar o repo
tmp_root <- function() {
  d <- tempfile("pcroot"); dir.create(d)
  file.copy(file.path(pc_root(), "config"), d, recursive = TRUE)
  file.copy(file.path(pc_root(), "models"), d, recursive = TRUE)
  d
}
edit_yaml <- function(file, fn) { x <- yaml::read_yaml(file); yaml::write_yaml(fn(x), file) }

test_that("baseline: 24 folhas, 4 subsistemas, order, khat efetivo, 8 membros", {
  t <- topo()
  expect_length(t$leaves, 24L); expect_false(anyDuplicated(t$leaves) > 0)
  expect_identical(names(t$subsystems), c("SECO", "S", "N", "NE"))
  expect_identical(unname(vapply(t$subsystems, function(s) s$loss, "")), c("PESE", "PES", "PEN", "PENE"))
  expect_identical(t$order, c("SECO", "S", "N", "NE"))
  expect_identical(t$sin$children, t$order)
  for (s in t$order) expect_identical(t$subsystems[[s]]$khat_range, c(1.00, 1.15))
  m <- prevcarga_members(root = pc_root())
  expect_identical(nrow(m), 8L)
  expect_identical(m$sigla, c("RF", "LGBM", "HW", "RGD_MED", "RGD_MAX", "LIN_MED", "LIN_MAX", "LIN_MXMN"))
  expect_identical(m$mdir, c("random-forest", "lgbm", "holt-winters", rep("dessem-regdin", 2), rep("dessem-linear", 3)))
  expect_identical(m$exogena, c(NA, NA, NA, "MED", "MAX", "MED", "MAX", "MXMN"))
})

test_that("area extra por argumento: 25 folhas; [CONTROL] pela raiz volta a 24", {
  t <- topo(path = fx("topology_extra_area.yaml"))
  expect_length(t$leaves, 25L); expect_true("ZZFICT" %in% t$leaves)
  expect_identical(tail(t$subsystems$SECO$areas, 1L), "ZZFICT")
  expect_length(topo()$leaves, 24L)
})

test_that("8 invalidos falham contract nomeando o ofensor; o seed valido carrega", {
  expect_no_error(topo())
  expect_pc_error(topo(path = fx("topology_dup_area.yaml")), "contract.topology_area_duplicated", "RS")
  expect_pc_error(topo(path = fx("topology_no_loss.yaml")), "contract.topology_loss_missing", "S")
  expect_pc_error(topo(path = fx("topology_dup_loss.yaml")), "contract.topology_loss_duplicated", "PESE")
  expect_pc_error(topo(path = fx("topology_khat_inverted.yaml")), "contract.khat_range_invalid", "khat_range")
  base <- file.path(pc_root(), "config", "topology.yaml")
  v <- function(fn) { f <- tempfile(fileext = ".yaml"); x <- yaml::read_yaml(base); yaml::write_yaml(fn(x), f); f }
  expect_pc_error(topo(path = v(function(x) { x$order <- c("SECO", "S", "N"); x })), "contract.topology_order_mismatch", "NE")
  expect_pc_error(topo(path = v(function(x) { x$extra <- 1; x })), "contract.topology_unknown_key", "extra")
  expect_pc_error(topo(path = v(function(x) { x$schema_version <- 2L; x })), "contract.topology_schema_version", "schema_version")
  ausente <- file.path(tempdir(), "nao-existe.yaml")
  expect_pc_error(topo(path = ausente), "contract.topology_file_missing", "nao-existe.yaml")
})

test_that("[CONTROL] override de khat_range so em S", {
  f <- fx("topology_khat_override_s.yaml")
  t1 <- topo(path = f)
  expect_identical(t1$subsystems$S$khat_range, c(0.98, 1.20))
  for (s in c("SECO", "N", "NE")) expect_identical(t1$subsystems[[s]]$khat_range, c(1.00, 1.15))
  raw <- yaml::read_yaml(f); raw$subsystems$S$khat_range <- NULL
  tf <- tempfile(fileext = ".yaml"); yaml::write_yaml(raw, tf)
  expect_identical(topo(path = tf)$subsystems$S$khat_range, c(1.00, 1.15))
  ## override invalido em S nomeia o subsistema
  raw$subsystems$S$khat_range <- c(1.3, 1.2); yaml::write_yaml(raw, tf)
  expect_pc_error(topo(path = tf), "contract.khat_range_invalid", "subsystems.S.khat_range")
})

test_that("nenhum literal de faixa/topologia nos .R de scripts/lib", {
  src <- list.files(file.path(pc_root(), "scripts", "lib"), pattern = "\\.R$", full.names = TRUE)
  for (f in src) expect_false(any(grepl("1\\.15|0\\.98", readLines(f, warn = FALSE))), info = f)
  tl <- readLines(file.path(pc_root(), "scripts", "lib", "topology.R"), warn = FALSE)
  expect_false(any(grepl("\\bSECO\\b|\\bPESE\\b|RGD_MED|dessem-|Sys\\.getenv", tl)))
})

test_that("os 5 run.yaml entregues tem os valores do contrato", {
  sp <- function(m) prevcarga_model_spec(m, root = pc_root())
  ms <- c("random-forest", "lgbm", "holt-winters", "dessem-linear", "dessem-regdin")
  S <- stats::setNames(lapply(ms, sp), ms)
  expect_identical(vapply(S, function(s) s$split, ""),
    c("random-forest" = "origin_agnostic", lgbm = "origin_agnostic", "holt-winters" = "origin_scoped",
      "dessem-linear" = "origin_scoped", "dessem-regdin" = "origin_scoped"))
  expect_identical(vapply(S, function(s) s$granularity_min, 1L),
    c("random-forest" = 60L, lgbm = 30L, "holt-winters" = 30L, "dessem-linear" = 60L, "dessem-regdin" = 60L))
  expect_identical(vapply(S, function(s) s$horizon_days, 1L),
    c("random-forest" = 9L, lgbm = 1L, "holt-winters" = 8L, "dessem-linear" = 9L, "dessem-regdin" = 9L))
  expect_identical(vapply(S, function(s) s$seed, 1L),
    c("random-forest" = 123L, lgbm = 123L, "holt-winters" = 1234L, "dessem-linear" = 42L, "dessem-regdin" = 42L))
  expect_identical(S[["holt-winters"]]$history_days, list(train = 730L, infer = 45L))
  expect_identical(S[["random-forest"]]$history_days, list(train = 365L, infer = 45L))
  expect_identical(S[["lgbm"]]$history_days, list(train = 730L, infer = 45L))
  expect_identical(S[["dessem-linear"]]$history_days, list(train = 1825L, infer = 1825L))
  expect_identical(S[["dessem-regdin"]]$history_days, list(train = 1825L, infer = 1825L))
  for (m in ms) {
    fc <- "forecast" %in% S[[m]]$modes
    expect_identical(fc, m %in% c("holt-winters", "dessem-linear", "dessem-regdin"))
    expect_true(all(c("train", "infer") %in% S[[m]]$modes))
    expect_identical(S[[m]]$backtest$protocol_default,
                     if (m %in% c("random-forest", "lgbm")) "daily" else "retrain_harness")
    expect_false(any(grepl("cut_invariant|resources|r_deps|entrypoint|train_args",
                           readLines(file.path(pc_root(), "models", m, "run.yaml")))))
  }
  ## LGBM.1: os 7 params que substituem as env flags LGBM_* do oraculo
  expect_identical(S$lgbm$params, list(disable_loess = TRUE, calendar_yearend = TRUE, train_cv = TRUE,
                                       tipodia_legacy = FALSE, cutoff_scheme = "v2_old",
                                       sazonalidade_scope = "train", prevespera_1230 = FALSE))
  expect_true(S[["dessem-regdin"]]$constraints$infer_history_equals_train)
})

test_that("run.yaml invalidos (tempdir) falham contract; horizon 1->3 e lido", {
  r <- tmp_root(); on.exit(unlink(r, recursive = TRUE))
  ry <- function(m) file.path(r, "models", m, "run.yaml")
  reset <- function(m) file.copy(file.path(pc_root(), "models", m, "run.yaml"), ry(m), overwrite = TRUE)
  try_edit <- function(m, fn) { reset(m); edit_yaml(ry(m), fn); prevcarga_model_spec(m, root = r) }

  expect_identical(try_edit("lgbm", function(x) { x$horizon_days <- 3L; x })$horizon_days, 3L)
  expect_pc_error(try_edit("lgbm", function(x) { x$seed <- NULL; x }), "contract.seed_missing", "seed")
  expect_pc_error(try_edit("lgbm", function(x) { x$granularity_min <- 15L; x }), "contract.run_yaml_bad_value", "granularity_min")
  expect_pc_error(try_edit("lgbm", function(x) { x$split <- "daily"; x }), "contract.run_yaml_bad_value", "split")
  expect_pc_error(try_edit("dessem-regdin", function(x) { x$history_days$infer <- 45L; x }),
                  "contract.constraint_violated", "infer_history_equals_train")
  expect_pc_error(try_edit("lgbm", function(x) { x$resources <- list(vcpu = 2L); x }), "contract.run_yaml_forbidden_key", "resources")
  expect_pc_error(try_edit("lgbm", function(x) { x$name <- "outro"; x }), "contract.run_yaml_name_mismatch", "outro")
  expect_pc_error(try_edit("lgbm", function(x) { x$cut_invariant <- TRUE; x }), "contract.run_yaml_forbidden_key", "cut_invariant")
  expect_pc_error(try_edit("lgbm", function(x) { x$modes <- "train"; x }), "contract.run_yaml_bad_value", "modes")
  expect_pc_error(try_edit("lgbm", function(x) { x$foo <- 1L; x }), "contract.run_yaml_unknown_key", "foo")
  expect_pc_error(prevcarga_model_spec("inexistente", root = r), "contract.run_yaml_missing", "inexistente")
  ## CONTROL: restaurado, volta a carregar com o horizonte original
  reset("lgbm")
  expect_identical(prevcarga_model_spec("lgbm", root = r)$horizon_days, 1L)
})

test_that("members invalidos: mdir sem run.yaml e sigla repetida", {
  r <- tmp_root(); on.exit(unlink(r, recursive = TRUE))
  tp <- file.path(r, "config", "topology.yaml")
  expect_identical(nrow(prevcarga_members(path = tp, root = r)), 8L)
  edit_yaml(tp, function(x) { x$members[[1L]]$mdir <- "nao-existe"; x })
  expect_pc_error(prevcarga_members(path = tp, root = r), "contract.topology_member_invalid", "nao-existe")
  file.copy(file.path(pc_root(), "config", "topology.yaml"), tp, overwrite = TRUE)
  edit_yaml(tp, function(x) { x$members[[2L]]$sigla <- "RF"; x })
  expect_pc_error(prevcarga_members(path = tp, root = r), "contract.topology_member_invalid", "RF")
})
