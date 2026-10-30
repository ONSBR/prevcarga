## test-parity.R -- paridade do plugin lgbm contra o ORACULO (LGBM.5), Delta = 0 (limiar 1e-5 MW absoluto, mesma plataforma).
## Tiers: `[PLUGINS] paridade:`, `paridade_control:` e `aditividade:` so rodam em --tier parity|parity-full (pc_gate_tier); sem o runner
## (testthat::test_file direto) vale o tier parity (grupo smoke). O golden (.state/golden, nunca versionado) e o `golden.lock`
## (versionado) vem de scripts/tools/plugin_golden.R; golden ausente, digest de dados ou fingerprint de runtime divergente FALHA com
## "golden ausente ou divergente: regere com scripts/tools/plugin_golden.R --model lgbm" (nunca skip, nunca troca de limiar).
## O clean-room roda EM PROCESSO pelo despachante real (leitor -> cortes de prevcarga_resolve_cuts com weather_issue_cutoff =
## origin_midnight_utc -> prevcarga_plugin_train (ingestao, FE, treino, freeze) -> prevcarga_plugin_infer (thaw, infer)) com os
## cortes LITERAIS da unidade (train_origin O-2 / last_data_day O-3, history_start por fase, available_until O 08:30) e e comparado por
## prevcarga_parity_compare (layout lgbm_day_48, day0_source native): `values` 2x48 contra native.rds (linha 1 = completed_D, linha 2 =
## O+1) e `delivery` contra o long.parquet do CLI do oraculo (interval_end = target_datetime + 30 min). As tabelas do artefato e os
## rotulos de calendario do oraculo vem em native.rds$meta$artifact_tables (plugin_golden.R so publica native.rds e long*.parquet).
suppressWarnings(suppressMessages(library(testthat)))

LGP_MODEL <- "lgbm"
LGP_TOL <- 1e-5                                  # unico limiar (ADR-0017): MW absoluto, trilho mesmo-hardware
LGP_PARITY <- file.path(lgbm_root(), "models", LGP_MODEL, "tests", "parity")
LGP_UNITS <- file.path(LGP_PARITY, "units.yaml")
LGP_LOCK <- file.path(LGP_PARITY, "golden.lock")
LGP_TOPOLOGY <- file.path(LGP_PARITY, "topology.yaml")
LGP_GOLDEN <- file.path(lgbm_root(), ".state", "golden")
LGP_DATA <- file.path(lgbm_root(), "data")
LGP_REGEN <- "golden ausente ou divergente: regere com scripts/tools/plugin_golden.R --model lgbm"
LGP_BASE <- "seco-2024-10-02"                    # smoke: base dos CONTROLs
LGP_CTL <- "ctl-aditividade-seco-2024-10-02"     # oracle_env LGBM_TRAIN_CV "0" x clean-room train_cv false
LGP_DEC <- "sp-2024-12-24"                       # dez/24: G-CAL/G-SAZ

## ferramentas de desenvolvimento (P.0f/P.0g) num env filho da fundacao (nunca o global)
lgp_tools <- local({
  e <- new.env(parent = lgbm_lib)
  for (f in c("plugin_golden.R", "plugin_parity.R")) sys.source(file.path(lgbm_root(), "scripts", "tools", f), envir = e)
  e
})

lgp_rscript <- function() file.path(R.home("bin"), "Rscript")
lgp_tier <- function() if (exists(".pc_gates_state", envir = globalenv())) get(".pc_gates_state", envir = globalenv())$tier else "parity"
lgp_gate_tier <- function(min) if (exists("pc_gate_tier", mode = "function")) pc_gate_tier(min) else invisible(TRUE)

## grupos do tier: parity => smoke; parity-full => smoke + foundation + fanout; `--units <grupo>` do runner vence
lgp_groups <- function() {
  g <- if (exists("pc_gate_units", mode = "function")) pc_gate_units(NULL) else NULL
  if (!is.null(g)) return(g)
  if (identical(lgp_tier(), "parity-full")) c("smoke", "foundation", "fanout") else "smoke"
}

lgp_spec <- function(path = LGP_UNITS) lgp_tools$pc_golden_units(path)

lgp_units <- function(groups) {
  spec <- lgp_spec()
  unlist(lapply(groups, function(g) lgp_tools$pc_golden_group_units(spec, g)), recursive = FALSE)
}

lgp_unit <- function(id) {
  us <- lgp_spec()$units
  us[[which(vapply(us, `[[`, "", "id") == id)]]
}

## --- golden.lock: ausente/divergente => mensagem de regerar (nunca skip) --------------------------------------
## Devolve NULL (ok) ou o texto ASCII da falha. Conferencia do P.0g: sha de arquivo, R/plataforma/BLAS, versao de pacote
## (inclui lightgbm), particoes/digest dos dados.
lgp_check_golden <- function(u, root = LGP_GOLDEN, lock = LGP_LOCK, data_dir = LGP_DATA) {
  if (!file.exists(lock)) return(sprintf("%s: golden.lock ausente", LGP_REGEN))
  d <- lgp_tools$.pc_golden_unit_dir(root, LGP_MODEL, u)
  if (!dir.exists(d)) return(sprintf("%s: unidade %s sem golden em %s", LGP_REGEN, u$id, d))
  lk <- jsonlite::read_json(lock, simplifyVector = FALSE)
  r <- lgp_catch(lgp_tools$.pc_parity_check_lock(lk, u, data_dir, d, lgp_rscript()))
  if (is.null(r)) return(NULL)
  sprintf("%s: unidade %s: %s %s", LGP_REGEN, u$id, r$id, paste(unlist(r$details), collapse = " "))
}

## erro classificado da fundacao como lista(id, details); NULL se `expr` passou
lgp_catch <- function(expr) {
  r <- lgbm_lib$prevcarga_with_errors(expr)
  if (is.null(r$cond)) NULL else list(id = r$cond$id, details = r$cond$details)
}

lgp_golden <- function(u, root = LGP_GOLDEN) lgp_tools$.pc_parity_read_golden(u, list(u), root, LGP_MODEL, "Etc/GMT+3")

## --- store real por overlay (nada gravado em data/) ------------------------------------------------------------
.lgp <- new.env(parent = emptyenv())

lgp_store <- function() {
  if (!is.null(.lgp$store)) return(.lgp$store)
  if (!dir.exists(LGP_DATA)) return(NA_character_)
  ov <- tempfile("lgp-store-"); dir.create(ov)
  for (d in c("load", "weather", "auxiliary")) file.symlink(normalizePath(file.path(LGP_DATA, d)), file.path(ov, d))
  file.copy(file.path(lgbm_root(), "config", "deck-manifest.store.json"), file.path(ov, "manifest.json"))
  .lgp$store <- ov
  ov
}

lgp_ctx <- function()
  list(data_dir = lgp_store(), root = lgbm_root(), topology_path = LGP_TOPOLOGY, weather_issue_cutoff = "origin_midnight_utc")

## cortes da fundacao (dates.R, data_lag_days do descriptor); available_until = o da UNIDADE (o store real tem o dia O inteiro)
lgp_cuts <- function(u, ctx = lgp_ctx()) {
  cuts <- suppressWarnings(lgbm_fx_cuts(ctx, u$origin))
  cuts$available_until <- u$available_until
  cuts
}

## Painel pelo leitor unico (M2) com a janela LITERAL da fase. Area = area da unidade; clima da heat_area. Previsto: TODAS as
## emissoes <= weather_issue_limit (a safra e do plugin); observado como `valid_time` (adaptador do P.0i), so ate o corte.
lgp_consts <- function() {
  if (is.null(.lgp$consts)) .lgp$consts <- lgbm_consts(lgbm_source_plugin())
  .lgp$consts
}

lgp_panel <- function(u, cuts, mode, ctx = lgp_ctx(), consts = lgp_consts()) {
  O <- u$origin
  heat <- unname(consts$heat_area[[u$area]])
  from <- lubridate::ymd(u$cuts[[if (mode == "train") "train" else "infer"]]$history_start)
  to <- if (mode == "train") cuts$last_data_day else O
  AU <- cuts$available_until
  rd <- function(...) suppressWarnings(lgbm_lib$prevcarga_read_load(ctx, u$area, from = from, to = to, ...))
  load <- if (mode == "train") rd() else rd(origin = O, truncate_at = AU)
  obs <- lgbm_lib$prevcarga_read_weather(ctx, "observed", heat, from, to)
  obs <- if (mode == "train") obs[lubridate::as_date(timestamp, tz = cuts$tz) <= to] else obs[timestamp <= AU]
  data.table::setnames(obs, "timestamp", "valid_time")
  fc <- lgbm_lib$prevcarga_read_weather(ctx, "forecast", heat, from, O + 2L)
  list(load = load, weather_observed = obs, weather_forecast = fc[issue_time <= cuts$weather_issue_limit],
       holidays = lgbm_lib$prevcarga_read_holidays(ctx, u$area, from, O + 2L),
       attrs = list(origin = O, available_until = AU, weather_issue_cutoff = cuts$weather_issue_cutoff,
                    weather_issue_limit = cuts$weather_issue_limit))
}

## Clean-room da unidade pelo despachante real (treino -> freeze -> thaw -> infer), em processo. `params` e `seed` sobrescrevem o
## run.yaml (a unidade os declara literais; CONTROL: train_cv false). Cache por (area, origem, params, seed): as duas unidades de
## train_cv false (CONTROL e aditividade) compartilham a mesma rodada.
lgp_run <- function(u, params = u$params, seed = u$seed) {
  key <- paste(u$area, format(u$origin), paste(names(params), unlist(params), collapse = ","), seed)
  if (!is.null(.lgp$runs[[key]])) return(.lgp$runs[[key]])
  plugin <- lgbm_fx_plugin(as.list(params))
  if (!is.null(seed)) plugin$spec$seed <- as.integer(seed)
  cuts <- lgp_cuts(u)
  stamp_ctx <- list(load_label = cuts$load_label, timezone = cuts$tz, weather_issue_cutoff = cuts$weather_issue_cutoff)
  art <- suppressWarnings(lgbm_with_cli(lgbm_lib$prevcarga_plugin_train(plugin, lgp_panel(u, cuts, "train"), cuts, plugin$spec, stamp_ctx)))
  ip <- lgp_panel(u, cuts, "infer")
  env <- suppressWarnings(lgbm_with_cli(lgbm_lib$prevcarga_plugin_infer(plugin, art, ip, cuts)))
  out <- list(env = env, art = art, cuts = cuts, infer_panel = ip, plugin = plugin)
  .lgp$runs[[key]] <- out
  out
}

lgp_compare <- function(run, gold, u) lgp_tools$prevcarga_parity_compare(run$env, gold, u, tol = LGP_TOL)

lgp_report <- function(r) sprintf("%s max|D|=%.3e worst=%s", r$unit, r$max_abs_delta, paste(r$worst, collapse = ","))

## --- G-CAL / G-SAZ: rotulos de calendario do quadro de infer e tabela Sazonalidade do artefato ------------------
lgp_calendar <- function(run, u) {
  P <- lgbm_source_plugin()
  K <- lgbm_consts(P)
  frame <- P$lgbm_ingest(run$infer_panel, run$cuts, NULL, "infer", K)
  cal <- data.table::data.table(Data = sort(unique(frame$Data)))
  P$lgbm_add_calendar_cols(cal, attr(frame, "feriados"), run$plugin$spec$params)
  as.data.frame(cal)
}

lgp_cal_delta <- function(mine, theirs) {
  common <- intersect(names(mine), names(theirs))
  days <- intersect(format(mine$Data), format(theirs$Data))
  a <- mine[match(days, format(mine$Data)), common, drop = FALSE]
  b <- theirs[match(days, format(theirs$Data)), common, drop = FALSE]
  bad <- common[!vapply(common, function(n) identical(unname(as.vector(a[[n]])), unname(as.vector(b[[n]]))), NA)]
  list(n_days = length(days), cols = common, bad = bad)
}

## max |D| entre duas tabelas de Sazonalidade: mesmas colunas, chaves (nao double) iguais apos ordenar, valores double por celula
lgp_table_delta <- function(a, b) {
  a <- as.data.frame(a); b <- as.data.frame(b)
  if (!setequal(names(a), names(b)) || nrow(a) != nrow(b)) return(Inf)
  b <- b[, names(a), drop = FALSE]
  keys <- names(a)[!vapply(a, is.double, NA)]
  oa <- do.call(order, c(unname(as.list(a[keys])), list(method = "radix")))
  ob <- do.call(order, c(unname(as.list(b[keys])), list(method = "radix")))
  a <- a[oa, , drop = FALSE]; b <- b[ob, , drop = FALSE]
  if (!all(vapply(keys, function(n) identical(as.vector(a[[n]]), as.vector(b[[n]])), NA))) return(Inf)
  vals <- setdiff(names(a), keys)
  if (!length(vals)) return(0)
  max(vapply(vals, function(n) if (identical(is.na(a[[n]]), is.na(b[[n]]))) max(0, abs(a[[n]] - b[[n]]), na.rm = TRUE) else Inf, 0))
}

lgp_artifact_delta <- function(run, tabs) {
  p <- run$art$payload
  sz <- lgp_table_delta(p$sazonalidade, tabs$sazonalidade)
  fb <- if (is.null(tabs$sazonalidade_fallbacks) && is.null(p$sazonalidade_fallbacks)) 0 else
    if (is.null(tabs$sazonalidade_fallbacks) || is.null(p$sazonalidade_fallbacks) ||
        !setequal(names(p$sazonalidade_fallbacks), names(tabs$sazonalidade_fallbacks))) Inf else
      max(vapply(names(p$sazonalidade_fallbacks), function(n)
        lgp_table_delta(p$sazonalidade_fallbacks[[n]], tabs$sazonalidade_fallbacks[[n]]), 0))
  mm <- if (!setequal(names(p$min_max_BLF), names(tabs$min_max_BLF))) Inf else
    max(vapply(names(p$min_max_BLF), function(n) max(abs(unlist(p$min_max_BLF[[n]]) - unlist(tabs$min_max_BLF[[n]]))), 0))
  list(saz = sz, fallbacks = fb, min_max = mm, cols_ok = identical(p$colunas_entrada_BLF, tabs$colunas_entrada_BLF),
       nao_norm_ok = setequal(p$colunas_nao_normalizar, tabs$colunas_nao_normalizar), best_iter = c(p$best_iter, tabs$best_iter))
}

## regerar o golden: mensagem unica, nunca skip
lgp_need_golden <- function(u) {
  if (is.na(lgp_store())) { testthat::fail(sprintf("%s: store de dados ausente (%s)", LGP_REGEN, LGP_DATA)); return(FALSE) }
  msg <- lgp_check_golden(u)
  if (!is.null(msg)) { testthat::fail(msg); return(FALSE) }
  TRUE
}

## --------------------------------------------------------------------------------------------------------------
test_that("[PLUGINS] units_yaml: esquema P.0f, grupos, cortes por fase, oracle_cut start_label, oracle_env do job.json, data_areas", {
  spec <- lgp_spec()
  expect_identical(spec$groups$smoke, c("seco-2024-10-02", "rs-2024-07-01", "sp-2024-12-24"))
  expect_gte(length(spec$groups$foundation), 3L)
  expect_true(all(startsWith(spec$groups$foundation, "seco-2024-10-")))                # SECO x outubro/2024
  fan <- lgp_tools$pc_golden_group_units(spec, "fanout")
  expect_setequal(vapply(fan, `[[`, "", "area"), c("SP", "RS", "PA", "BAOE"))
  expect_true(all(vapply(fan, function(u) format(u$origin) == "2024-07-15", NA)))
  expect_identical(spec$groups$controls, LGP_CTL)
  consts <- lgp_consts()
  us <- spec$units
  expect_gte(length(unique(vapply(us, function(u) paste(u$area, format(u$origin)), ""))), 3L)
  expect_true(any(vapply(us, function(u) format(u$origin, "%d") == "01", NA)))          # 1o do mes
  expect_true(any(vapply(us, function(u) format(u$origin, "%Y-%m") == "2024-12", NA)))  # dez/24
  for (u in us) {
    O <- u$origin
    expect_identical(u$oracle_cut, "start_label"); expect_identical(u$day0_source, "native")
    expect_identical(u$weather_issue_cutoff, "origin_midnight_utc")
    expect_identical(format(u$cuts$train$train_origin), format(O - 2L))                  # --origin do oraculo = O-2 (= train_origin do clean-room)
    expect_identical(u$cuts$train$history_start, format(O - 732L))                       # O-2-730
    expect_identical(u$cuts$infer$history_start, format(O - 45L))
    expect_identical(format(u$available_until, "%H:%M"), "08:30")
    expect_identical(lgp_tools$.pc_golden_cut_arg(u), paste0(format(O), "T08:00"))        # o oraculo recebe available_until - 30 min
    expect_identical(sort(u$data_areas), sort(unique(c(u$area, unname(consts$heat_area[[u$area]])))))
    expect_identical(u$oracle_env$LGBM_DISABLE_LOESS, "1"); expect_identical(u$oracle_env$LGBM_CALENDAR_YEAREND, "1")
    expect_identical(u$oracle_env$LGBM_TRAIN_CV, if (identical(u$id, LGP_CTL)) "0" else "1")
  }
  ## run.yaml e a fonte dos params do clean-room (os mesmos 3 flags do job.json)
  rp <- yaml::read_yaml(file.path(lgbm_root(), "models", "lgbm", "run.yaml"))$params
  expect_true(isTRUE(rp$disable_loess) && isTRUE(rp$calendar_yearend) && isTRUE(rp$train_cv))
  expect_identical(lgp_unit(LGP_CTL)$params$train_cv, FALSE)
  ## cada unidade tem o proprio diretorio de golden (area x origem x variante): a de aditividade nao sobrescreve a de CV
  dirs <- vapply(us, function(u) lgp_tools$.pc_golden_unit_dir("g", LGP_MODEL, u), "")
  expect_false(anyDuplicated(dirs) > 0L)
  expect_identical(lgp_unit(LGP_CTL)$variant, "aditividade")
  ## o env do oracle_env existe so no filho do oraculo: o pai nunca o ve (nem o plugin)
  expect_identical(Sys.getenv("LGBM_TRAIN_CV"), "")
  expect_false(any(grepl("^LGBM_", names(Sys.getenv()))))
  expect_true(all(grepl("^LGBM_", names(lgp_unit(LGP_CTL)$oracle_env))))
  ## unidade fora do contrato e recusada nomeando o campo (regra de topo `rules`)
  bad <- yaml::read_yaml(LGP_UNITS); bad$units <- bad$units[1:2]; bad$groups <- list(smoke = c("seco-2024-10-02", "rs-2024-07-01"))
  bad$units[[1L]]$cuts$train$train_origin <- "2024-10-01"
  f <- withr::local_tempfile(fileext = ".yaml"); yaml::write_yaml(bad, f)
  r <- lgp_catch(lgp_spec(f))
  expect_identical(r$id, "tools.bad_units")
})

test_that("[PLUGINS] paridade: values 2x48 e delivery O..O+1 vs oraculo, max|d| <= 1e-5 MW; gate02; G-CAL/G-SAZ (Delta=0)", {
  lgp_gate_tier("parity")
  us <- lgp_units(lgp_groups())
  expect_gt(length(us), 0L)
  rep <- list()
  for (u in us) {
    if (!lgp_need_golden(u)) next
    run <- lgp_run(u)
    gold <- lgp_golden(u)
    r <- lgp_compare(run, gold, u)
    rep[[u$id]] <- r
    expect_identical(r$layout, "lgbm_day_48")
    expect_identical(r$day0_source, "native")
    expect_true(isTRUE(r$shape_ok) && isTRUE(r$na_ok), info = u$id)                       # values 2x48: mesma forma e mesmo padrao de NA
    expect_identical(dim(run$env$values), c(2L, 48L))
    expect_true(isTRUE(r$delivery_ok), info = u$id)
    expect_identical(as.integer(r$delivery_missing), 0L)                                  # toda celula da entrega tem par no long do oraculo
    expect_true(isTRUE(r$cut_ok), info = u$id)
    expect_identical(as.integer(r$fora_da_faixa), 0L)
    expect_lte(r$max_abs_delta, LGP_TOL)                                                  # Delta = 0 (limiar fixo)
    expect_true(isTRUE(r$ok), info = lgp_report(r))
    expect_gt(unname(run$art$payload$min_max_BLF$CargaGlobal["max"]), 1000)               # gate02: escala MW do deck real
    ## G-CAL/G-SAZ: rotulos de calendario e tabela Sazonalidade do artefato do clean-room == os do artefato do oraculo
    tabs <- gold$native$meta$artifact_tables
    expect_false(is.null(tabs), info = u$id)
    mycal <- lgp_calendar(run, u)
    cal <- lgp_cal_delta(mycal, tabs$calendario)
    if (identical(u$id, LGP_DEC)) {                                                      # dez/24: vespera/natal presentes e iguais ao oraculo
      expect_true(all(c("Vespera", "Natal_AnoNovo") %in% mycal$DiaEspecialFimAno))
      expect_true(all(c("Vespera", "Natal_AnoNovo") %in% tabs$calendario$DiaEspecialFimAno))
    }
    expect_gte(cal$n_days, 46L); expect_length(cal$bad, 0L)
    ad <- lgp_artifact_delta(run, tabs)
    expect_lte(ad$saz, 1e-9); expect_lte(ad$fallbacks, 1e-9); expect_lte(ad$min_max, 1e-9)
    expect_true(ad$cols_ok, info = u$id); expect_true(ad$nao_norm_ok, info = u$id)
    expect_identical(as.integer(ad$best_iter[[1L]]), as.integer(ad$best_iter[[2L]]))
    ## o pai nunca ganhou variavel LGBM_*
    expect_false(any(grepl("^LGBM_", names(Sys.getenv()))))
  }
  message(sprintf("parity %s unidades=%d max_abs=%.3g", paste(lgp_groups(), collapse = "+"), length(rep),
                  if (length(rep)) max(vapply(rep, function(r) r$max_abs_delta, 0)) else NA_real_))
})

test_that("[PLUGINS] paridade_control: train_cv false contra o golden de CV acusa > 1e-5; golden ausente, digest de dados e fingerprint adulterados falham (regere o golden)", {
  lgp_gate_tier("parity")
  u <- lgp_unit(LGP_BASE)
  if (!lgp_need_golden(u)) return(invisible(NULL))                                       # sem golden valido o controle nao prova nada
  gold <- lgp_golden(u)
  base <- lgp_compare(lgp_run(u), gold, u)
  expect_true(isTRUE(base$ok), info = lgp_report(base))
  ## CONTROL com dente: o clean-room em holdout (train_cv false) contra o golden de CV muda a previsao alem de 1e-5
  cv_off <- lgp_compare(lgp_run(u, params = list(train_cv = FALSE)), gold, u)
  expect_gt(cv_off$max_abs_delta, LGP_TOL); expect_false(isTRUE(cv_off$ok))
  expect_true(isTRUE(cv_off$shape_ok))                                                   # a diferenca e de VALOR, nao de forma
  ## golden ausente (diretorio vazio no lugar do golden) e golden.lock ausente
  m <- lgp_check_golden(u, root = withr::local_tempdir())
  expect_match(m, LGP_REGEN, fixed = TRUE); expect_match(m, u$id, fixed = TRUE)
  expect_match(lgp_check_golden(u, lock = file.path(withr::local_tempdir(), "nao-existe.lock")), LGP_REGEN, fixed = TRUE)
  ## fingerprint do runtime adulterado (versao do lightgbm) e digest de dados adulterado => regere
  lk <- jsonlite::read_json(LGP_LOCK, simplifyVector = FALSE)
  tamper <- function(f) {
    l2 <- f(lk)
    p <- tempfile(fileext = ".lock")
    on.exit(unlink(p), add = TRUE)
    writeLines(jsonlite::toJSON(l2, auto_unbox = TRUE, null = "null", digits = NA), p)
    lgp_check_golden(u, lock = p)
  }
  m1 <- tamper(function(l) { l$units[[u$id]]$fingerprint$packages$lightgbm <- "0.0.0"; l })
  expect_match(m1, LGP_REGEN, fixed = TRUE); expect_match(m1, "lightgbm", fixed = TRUE)
  m2 <- tamper(function(l) { l$units[[u$id]]$data_digest$aggregate <- strrep("0", 64L); l })
  expect_match(m2, LGP_REGEN, fixed = TRUE)
  m3 <- tamper(function(l) { l$units[[u$id]]$files[["native.rds"]] <- strrep("0", 64L); l })
  expect_match(m3, LGP_REGEN, fixed = TRUE)
  ## oraculo intocado: o codigo (tracked e untracked fora dos planos docs/roadmap) do oraculo real nao foi modificado
  orc <- "/opt/ons-source/prevcargadessem3"
  if (dir.exists(file.path(orc, ".git"))) {
    st <- suppressWarnings(system2("git", c("--no-optional-locks", "-C", shQuote(orc), "status", "--porcelain"), stdout = TRUE, stderr = FALSE))
    expect_length(st[!grepl("docs/roadmap/", st, fixed = TRUE)], 0L)
  }
})

test_that("[PLUGINS] aditividade: oracle_env LGBM_TRAIN_CV 0 contra o clean-room train_cv false, max|d| <= 1e-5 (ex-gate_adequacao_add.sh)", {
  lgp_gate_tier("parity")
  u <- lgp_unit(LGP_CTL)
  expect_identical(u$oracle_env$LGBM_TRAIN_CV, "0")
  expect_identical(u$params$train_cv, FALSE)
  if (!lgp_need_golden(u)) return(invisible(NULL))
  r <- lgp_compare(lgp_run(u), lgp_golden(u), u)
  expect_true(isTRUE(r$shape_ok) && isTRUE(r$na_ok) && isTRUE(r$delivery_ok), info = u$id)
  expect_lte(r$max_abs_delta, LGP_TOL)
  expect_true(isTRUE(r$ok), info = lgp_report(r))
  expect_identical(Sys.getenv("LGBM_TRAIN_CV"), "")                                       # o env existe so no filho do oraculo
})
