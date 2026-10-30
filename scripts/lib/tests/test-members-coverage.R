## test-members-coverage.R -- cobertura dos 8 membros da topologia (M5.11; AC38). So VERIFICA: nao escreve codigo de modelo.
## Matriz: cada linha de prevcarga_members() resolve para um plugin carregado + variante aceita; o conjunto de `mdir` e o de
## models/*/run.yaml; cada membro treina e infere pelas fachadas sobre o deck sintetico de fixture_store.R e entrega a grade
## prevcarga_delivery_grid. Os cenarios rodam em `Rscript --vanilla` filho (helper-porte-closure.R); a topologia-fixture vai por
## um config.yaml em tempdir (--config), nunca por variavel de ambiente. Linux e Windows.

PCC_SIGLAS <- c("RF", "LGBM", "HW", "LIN_MED", "LIN_MAX", "LIN_MXMN", "RGD_MED", "RGD_MAX")
.na1 <- function(x) is.null(x) || (length(x) == 1L && is.na(x))
.topo <- function(cfg) file.path(dirname(cfg), "topology.yaml")

test_that("[ac-38] 8/8 membros: plugin registra, exogena NA <=> variant_param NA, exogena em allowed; sem plugin orfao", {
  cfg <- pcc_topology_fixture("none")
  r <- pcc_run_child("check_members", list(topology = .topo(cfg)))
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  rows <- r$json$members
  expect_length(rows, 8L)
  expect_setequal(vapply(rows, `[[`, "", "sigla"), PCC_SIGLAS)
  for (m in rows) {
    expect_identical(.na1(m$exogena), .na1(m$variant_param), info = m$sigla)
    if (!.na1(m$exogena)) expect_true(m$exogena %in% unlist(m$allowed), info = m$sigla)
    expect_true(is.numeric(m$delivery_days), info = m$sigla)
  }
  ## variante: so os dois DESSEM declaram variant_param; RF, LGBM e HW nao
  has_vp <- vapply(rows, function(m) !.na1(m$variant_param), NA)
  expect_setequal(vapply(rows[has_vp], `[[`, "", "mdir"), c("dessem-linear", "dessem-regdin"))
  expect_setequal(vapply(rows[!has_vp], `[[`, "", "sigla"), c("RF", "LGBM", "HW"))
  ## o conjunto de mdir da topologia e exatamente o de models/*/run.yaml
  expect_setequal(unique(vapply(rows, `[[`, "", "mdir")), unlist(r$json$run_yaml_dirs))
  expect_setequal(unlist(r$json$run_yaml_dirs), pcc_all_plugins())
  expect_setequal(pcc_all_plugins(), basename(dirname(Sys.glob(file.path(pc_root(), "models", "*", "run.yaml")))))
  expect_length(pcc_all_plugins(), 5L)
  ## horizonte de entrega: O..O+8 nos 7 de horizonte longo, O..O+1 no LGBM
  dd <- vapply(rows, function(m) as.integer(m$delivery_days), 1L); names(dd) <- vapply(rows, `[[`, "", "sigla")
  expect_identical(dd[["LGBM"]], 2L)
  expect_true(all(dd[setdiff(PCC_SIGLAS, "LGBM")] == 9L))
})

test_that("[ac-38][CONTROL] topologia-fixture com (dessem-regdin, MXMN): exit 2 [prevcarga/contract] citando exogena=MXMN", {
  cfg <- pcc_topology_fixture("add_member")
  r <- pcc_run_child("check_members", list(topology = .topo(cfg)))
  expect_identical(r$status, 2L)
  err <- paste(r$stderr, collapse = "\n")
  expect_match(err, "[prevcarga/contract]", fixed = TRUE)
  expect_match(err, "exogena=MXMN", fixed = TRUE)
  expect_match(err, "dessem-regdin", fixed = TRUE)
  expect_null(r$json)
})

test_that("[ac-38][CONTROL] topologia-fixture com `mdir: nao-existe`: exit 2 [prevcarga/contract] citando nao-existe e run.yaml", {
  cfg <- pcc_topology_fixture("bad_mdir")
  r <- pcc_run_child("check_members", list(topology = .topo(cfg)))
  expect_identical(r$status, 2L)
  err <- paste(r$stderr, collapse = "\n")
  expect_match(err, "[prevcarga/contract]", fixed = TRUE)
  expect_match(err, "nao-existe", fixed = TRUE)
  expect_match(err, "run.yaml", fixed = TRUE)
  expect_null(r$json)
})

test_that("[ac-38][CONTROL] a topologia real nao foi tocada pelas fixtures (config/ copiada, nunca editada)", {
  cfg <- pcc_topology_fixture("add_member")
  real <- readLines(file.path(pc_root(), "config", "topology.yaml"), warn = FALSE)
  expect_false(any(grepl("RGD_MXMN", real, fixed = TRUE)))
  expect_false(any(grepl("nao-existe", real, fixed = TRUE)))
  expect_true(any(grepl("RGD_MXMN", readLines(.topo(cfg), warn = FALSE), fixed = TRUE)))
  expect_false(identical(normalizePath(dirname(cfg)), normalizePath(file.path(pc_root(), "config"))))
})

test_that("[ac-38] entrega de cada um dos 8 membros (treino+infer pelas fachadas) == prevcarga_delivery_grid, available_until 09:30", {
  r <- pcc_run_child("delivery")
  expect_identical(r$status, 0L, info = paste(utils::tail(r$stderr, 12L), collapse = "\n"))
  expect_false(is.null(r$json))
  mem <- r$json$members
  expect_length(mem, 8L)
  expect_setequal(vapply(mem, `[[`, "", "sigla"), PCC_SIGLAS)
  for (m in mem) {
    expect_true(isTRUE(m$identical), info = m$sigla)                         # identical(target_datetime, grade)
    expect_identical(m$available_until, "2024-10-15 09:30:00", info = m$sigla)
    expect_identical(m$first, "2024-10-15 10:00:00", info = m$sigla)         # 1o interval_end 10:00 (resto do dia O incluso)
    expect_identical(m$first, m$grid_first, info = m$sigla)
    expect_identical(m$last, m$grid_last, info = m$sigla)
    expect_identical(m$tz, "Etc/GMT+3", info = m$sigla)                      # fuso da carga
    expect_false(isTRUE(m$anyNA), info = m$sigla)
    if (identical(m$sigla, "LGBM")) {
      expect_identical(as.integer(m$delivery_days), 2L)
      expect_identical(m$last, "2024-10-17 00:00:00")                        # O..O+1
      expect_identical(as.integer(m$n), 77L)
    } else {
      expect_identical(as.integer(m$delivery_days), 9L)
      expect_identical(m$last, "2024-10-24 00:00:00")                        # O..O+8
      expect_identical(as.integer(m$n), 413L)
    }
  }
})

test_that("[ac-38][CONTROL] a grade de entrega tem dente: horizonte errado ou corte errado nao e identical", {
  env <- pc_source_lib(c("errors", "features"))
  tz <- "Etc/GMT+3"
  cuts <- function(hm) list(available_until = lubridate::ymd_hm(paste("2024-10-15", hm), tz = tz), tz = tz,
                            origin_day = lubridate::ymd("2024-10-15"))
  g9 <- env$prevcarga_delivery_grid(cuts("09:30"), 9L)
  g2 <- env$prevcarga_delivery_grid(cuts("09:30"), 2L)
  expect_length(g9, 413L); expect_length(g2, 77L)
  expect_false(identical(g9, g2))
  expect_false(identical(g9, env$prevcarga_delivery_grid(cuts("09:00"), 9L)))   # corte diferente muda a grade
  expect_identical(g9[[1L]], lubridate::ymd_hm("2024-10-15 10:00", tz = tz))
})
