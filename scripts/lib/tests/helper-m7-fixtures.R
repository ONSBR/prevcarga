## helper-m7-fixtures.R -- fixtures do M7 por formula (M7.2 cria; M7.x acrescentam). Auto-carregado por testthat.
## Nao e codigo de produto. Sem variaveis de ambiente; tudo em tempdir (nunca data/); datas por lubridate com tz.

PC_M7_TZ <- "Etc/GMT+3"

## ctx de fixture: state_dir/data_dir em tempdir + config.yaml de fixture; limpeza ao fim do frame do chamador.
pc_m7_ctx <- function(.local_envir = parent.frame()) {
  d <- withr::local_tempdir("pc-m7-", .local_envir = .local_envir)
  state <- file.path(d, "state"); data <- file.path(d, "deck-store")
  dir.create(state); dir.create(data)
  cfg <- file.path(d, "config.yaml")
  yaml::write_yaml(list(state_dir = state, data_dir = data), cfg)
  list(state_dir = state, data_dir = data, config_path = cfg, root = pc_root())
}

## spec de run.yaml em memoria (so os campos que o M7 consome)
pc_m7_spec <- function(split = "origin_agnostic", horizon_days = 8L, seed = 42L, granularity_min = 60L,
                       protocol_default = "daily") {
  list(name = "fixture", split = split, horizon_days = as.integer(horizon_days), seed = as.integer(seed),
       granularity_min = as.integer(granularity_min), protocol_default = protocol_default)
}

## siglas do M7 como (mdir, exogena); exogena NA quando o membro nao tem variante
pc_m7_members <- function() {
  data.frame(sigla = c("RF", "LGBM", "HW", "LIN_MED", "LIN_MAX", "LIN_MXMN", "RGD_MED", "RGD_MAX"),
             mdir = c("random-forest", "lgbm", "holt-winters", rep("dessem-linear", 3L), rep("dessem-regdin", 2L)),
             exogena = c(NA, NA, NA, "MED", "MAX", "MXMN", "MED", "MAX"), stringsAsFactors = FALSE)
}

## rotulo de membro (mdir, exogena) a partir da sigla
pc_m7_label <- function(sigla) {
  m <- pc_m7_members()
  m <- m[m$sigla == sigla, , drop = FALSE]
  list(mdir = m$mdir, exogena = m$exogena)
}

## topologia de fixture: 1 sistema e `n_leaves` folhas (nomes por formula)
pc_m7_topology <- function(n_leaves = 2L) {
  leaves <- sprintf("L%02d", seq_len(n_leaves))
  list(system = "SIS", leaves = leaves)
}

## corte intradiario de fixture: instante HH:MM do dia `day` no tz da carga
pc_m7_au <- function(day, hm) lubridate::ymd_hm(paste(day, hm), tz = PC_M7_TZ)

## deck sintetico com manifest.json (fixture_store.R): um dia por origem, ate a maior origem
pc_m7_deck <- function(origins, issues = c("00", "06", "18"), areas = c("A", "B"), seed = 7L,
                       .local_envir = parent.frame()) {
  env <- new.env(parent = globalenv())
  for (m in c("errors")) sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = env)
  sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
  origins <- sort(origins)
  days <- seq(min(origins) - 12L, max(origins), by = "day")
  dir <- file.path(withr::local_tempdir("pc-m7-deck-", .local_envir = .local_envir), "deck")
  env$prevcarga_fixture_deck(dir, seed = seed, areas = areas, days = days)
  dir
}
