## test-parity.R -- paridade do plugin random-forest contra o ORACULO (RF.4), Delta = 0 (limiar 1e-5 MW absoluto, mesma plataforma).
## Tiers: `[PLUGINS] paridade:` e `paridade_control:` so rodam em --tier parity|parity-full (pc_gate_tier). O golden (.state/golden, nunca
## versionado) e o `golden.lock` (versionado) vem de scripts/tools/plugin_golden.R; golden ausente ou fingerprint divergente FALHA com
## "regere o golden" (nunca skip, nunca troca de limiar). A comparacao roda scripts/tools/plugin_parity.R em PROCESSO FILHO (como o CLI:
## fundacao no global, plugin pelo loader): leitor -> cortes -> treino em train_origin = O-10 -> freeze -> thaw -> infer em O (as duas origens
## nativas O-1 e O), com os cortes LITERAIS da unidade. Por LAYOUT: `values` 2 x 216 contra native.rds; `delivery` contra long_O (O+1..O+8) e o
## d_plus 1 de long_Om1 (dia O), interval_end = timestamp + 30 min; o O+9 do oraculo sai como `fora_da_faixa`.
suppressWarnings(suppressMessages(library(testthat)))

RFP_ROOT <- rf_root()
RFP_MODEL <- "random-forest"
RFP_PARITY <- file.path(RFP_ROOT, "models", RFP_MODEL, "tests", "parity")
RFP_UNITS <- file.path(RFP_PARITY, "units.yaml")
RFP_LOCK <- file.path(RFP_PARITY, "golden.lock")
RFP_GOLDEN <- file.path(RFP_ROOT, ".state", "golden")
RFP_TOOL <- file.path(RFP_ROOT, "scripts", "tools", "plugin_parity.R")
RFP_RSCRIPT <- file.path(R.home("bin"), "Rscript")

## store com manifest (overlay em .state/deck-store: symlinks para data/ + manifest.json do store); nunca escreve em data/
rfp_store <- function() {
  dir <- file.path(RFP_ROOT, ".state", "deck-store")
  src <- file.path(RFP_ROOT, "data")
  if (!file.exists(file.path(dir, "manifest.json"))) {
    if (!dir.exists(src)) return(NA_character_)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    for (d in c("load", "weather", "auxiliary"))
      if (!file.exists(file.path(dir, d))) file.symlink(normalizePath(file.path(src, d)), file.path(dir, d))
    file.copy(file.path(RFP_ROOT, "config", "deck-manifest.store.json"), file.path(dir, "manifest.json"))
  }
  dir
}

rfp_gate_tier <- function(min) if (exists("pc_gate_tier", mode = "function")) pc_gate_tier(min) else invisible(TRUE)
rfp_group <- function(default) if (exists("pc_gate_units", mode = "function")) pc_gate_units(default) else default
rfp_tier <- function() if (exists(".pc_gates_state", envir = globalenv())) get(".pc_gates_state", envir = globalenv())$tier else "parity"

## plugin_parity.R em processo filho. `units` = grupo ou caminho de units.yaml. Devolve status, stderr, relatorio (JSON do --out).
rfp_run <- function(units, golden = RFP_GOLDEN, lock = RFP_LOCK, report = tempfile("parity-report-", fileext = ".json")) {
  store <- rfp_store()
  if (is.na(store)) return(list(status = 1L, err = "store de dados ausente", report = NULL, path = report))
  args <- c("--vanilla", RFP_TOOL, "--model", RFP_MODEL, "--units", units, "--golden", golden, "--data", store, "--lock", lock,
            "--rscript", RFP_RSCRIPT, "--out", report)
  err <- tempfile("parity-err-")
  st <- suppressWarnings(system2(RFP_RSCRIPT, args, stdout = FALSE, stderr = err))
  rep <- if (file.exists(report)) jsonlite::read_json(report, simplifyVector = FALSE) else NULL
  list(status = as.integer(st), err = paste(readLines(err, warn = FALSE), collapse = "\n"), report = rep, path = report)
}

## golden ausente/velho: o filho sai com tools.golden_missing / tools.golden_stale => "regere o golden"
rfp_stale <- function(r) r$status != 0L && grepl("golden_missing|golden_stale|store de dados ausente", r$err)

## copia de units.yaml so com a unidade pedida, sem `rules`/`groups` (o CONTROL muda params/seed, que `rules.equals` proibe)
rfp_units_copy <- function(id, mutate) {
  doc <- yaml::read_yaml(RFP_UNITS)
  u <- Filter(function(x) identical(x$id, id), doc$units)[[1L]]
  doc <- list(packages = doc$packages, units = list(mutate(u)))
  f <- tempfile("units-", fileext = ".yaml")
  yaml::write_yaml(doc, f)
  f
}

test_that("[PLUGINS] paridade: values 2x216 e delivery O..O+8 vs oraculo, max|d| <= 1e-5 MW (Delta=0)", {
  rfp_gate_tier("parity")
  grp <- rfp_group(if (identical(rfp_tier(), "parity-full")) "foundation" else "smoke")
  rep <- file.path(RFP_GOLDEN, sprintf("parity_report_%s.json", grp))
  dir.create(RFP_GOLDEN, recursive = TRUE, showWarnings = FALSE)
  r <- rfp_run(grp, report = rep)
  if (rfp_stale(r)) fail(sprintf("regere o golden (scripts/tools/plugin_golden.R --model %s --units %s): %s", RFP_MODEL, grp, r$err))
  expect_false(is.null(r$report), info = r$err)
  us <- r$report$units
  expect_gt(length(us), 0L)
  for (x in us) {
    expect_identical(x$layout, "rf_wide_hourly_216")
    expect_true(isTRUE(x$shape_ok) && isTRUE(x$na_ok), info = x$unit)                  # values 2 x 216: mesma forma e mesmo padrao de NA
    expect_true(isTRUE(x$delivery_ok), info = x$unit)
    expect_identical(as.integer(x$delivery_missing), 0L)                                # nenhum intervalo entregue sem par no golden
    expect_true(isTRUE(x$cut_ok), info = x$unit)
    expect_lte(x$max_abs_delta, 1e-5)                                                  # Delta = 0 (limiar fixo)
    expect_true(isTRUE(x$ok), info = sprintf("%s max_abs=%.3g worst=%s", x$unit, x$max_abs_delta, paste(unlist(x$worst), collapse = ",")))
    expect_identical(as.integer(x$fora_da_faixa_long), 48L)                             # O+9 do oraculo (d_plus 9 de long_O): fora_da_faixa
    expect_identical(x$day0_source, "oracle_origin_minus_1")
  }
  expect_identical(r$status, 0L, info = r$err)
  message(sprintf("parity_report %s sha256=%s unidades=%d max_abs=%.3g", grp, digest::digest(rep, algo = "sha256", file = TRUE),
                  length(us), max(vapply(us, function(x) x$max_abs_delta, 0))))
})

test_that("[PLUGINS] paridade_control: ntree 99 e seed 124 acusam > 1e-5; golden ausente e fingerprint adulterado falham (regere o golden)", {
  rfp_gate_tier("parity")
  id <- "seco-2024-10-01"
  base <- rfp_run(rfp_units_copy(id, identity))                                        # golden valido: sem ele o controle nao prova nada
  if (rfp_stale(base)) fail(sprintf("regere o golden (scripts/tools/plugin_golden.R --model %s --units smoke): %s", RFP_MODEL, base$err))
  expect_identical(base$status, 0L, info = base$err)
  for (mut in list(function(u) { u$params$ntree <- 99L; u }, function(u) { u$seed <- 124L; u })) {
    r <- rfp_run(rfp_units_copy(id, mut))
    expect_identical(r$status, 1L)                                                      # tools.parity_failed
    expect_false(is.null(r$report), info = r$err)
    expect_false(isTRUE(r$report$units[[1L]]$ok)); expect_gt(r$report$units[[1L]]$max_abs_delta, 1e-5)
  }
  ## golden ausente (diretorio vazio no lugar do golden)
  r <- rfp_run(rfp_units_copy(id, identity), golden = withr::local_tempdir())
  expect_true(rfp_stale(r)); expect_identical(r$status, 1L)
  ## golden.lock com o fingerprint de randomForest adulterado
  lock <- jsonlite::read_json(RFP_LOCK, simplifyVector = FALSE)
  lock$units[[id]]$fingerprint$packages$randomForest <- "0.0-0"
  bad <- withr::local_tempfile(fileext = ".lock")
  writeLines(jsonlite::toJSON(lock, auto_unbox = TRUE, null = "null", digits = NA), bad)
  r <- rfp_run(rfp_units_copy(id, identity), lock = bad)
  expect_true(rfp_stale(r)); expect_identical(r$status, 1L); expect_match(r$err, "randomForest")
  expect_match(r$err, "regere o golden")
})
