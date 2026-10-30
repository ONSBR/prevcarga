## test-parity.R -- paridade do plugin dessem-linear contra o ORACULO (DL.9), Delta = 0 (limiar 1e-5 MW absoluto, mesma plataforma).
## Tiers: `[PLUGINS] paridade:` e `paridade_control:` so rodam em --tier parity|parity-full (pc_gate_tier); sem o runner (test_dir direto)
## vale o tier parity (grupo smoke). O golden (.state/golden, nunca versionado) e o `golden.lock` (versionado) vem de
## scripts/tools/plugin_golden.R (driver tests/parity/oracle_driver.R: caminho de INFER do oraculo = run_train + run_infer com o corte
## 08:30 = available_until - 30 min); golden ausente, digest de dados ou fingerprint de runtime divergente FALHA com "regere o golden"
## (nunca skip, nunca troca de limiar). O clean-room roda EM PROCESSO pelo despachante real (leitor -> cortes de prevcarga_resolve_cuts
## com weather_issue_cutoff = origin_midnight_utc -> prevcarga_plugin_train -> prevcarga_plugin_infer), com a mesma montagem de
## scripts/tools/plugin_parity.R (.pc_parity_envelope) e o deck da unidade cortado em available_until; compara por
## prevcarga_parity_compare (layout dessem_semihourly_48, day0_source native): `values` 9x48 (O..O+8) contra native.rds (10x48; o O+9
## do oraculo sai como `fora_da_faixa`) e `delivery` contra o long.parquet do oraculo (interval_end = timestamp + 30 min).
## O runtime do oraculo e R 4.4.3 + biblioteca com WeightSVM/rlist (oracle_env R_LIBS_USER): o fingerprint do lock e conferido com esse
## Rscript e o env gravado no invocation.json do golden (como o motor, DL.4). Tambem mede a regua split x forecast acoplado (ADR-0031)
## e grava tests/parity/foundation_report.json (sha256 por unidade) quando roda o grupo `foundation` inteiro.
suppressWarnings(suppressMessages(library(testthat)))

DLP_MODEL <- "dessem-linear"
DLP_TOL <- 1e-5                                  # unico limiar (ADR-0017): MW absoluto, trilho mesmo-hardware
DLP_PARITY <- function() file.path(dl_root(), "models", DLP_MODEL, "tests", "parity")
DLP_UNITS <- function() file.path(DLP_PARITY(), "units.yaml")
DLP_LOCK <- function() file.path(DLP_PARITY(), "golden.lock")
DLP_REPORT <- function() file.path(DLP_PARITY(), "foundation_report.json")
DLP_GOLDEN <- function() file.path(dl_root(), ".state", "golden")
DLP_ORACLE_RSCRIPT <- "/opt/R/4.4.3/bin/Rscript"  # runtime do oraculo (WeightSVM/rlist na biblioteca do oracle_env)
DLP_ORACLE <- "/opt/ons-source/prevcargadessem3"
DLP_REGEN <- "golden ausente ou divergente: regere com scripts/tools/plugin_golden.R --model dessem-linear"
DLP_BASE <- "seco-2024-10-15"                    # unidade-base dos CONTROLs
DLP_WMAPE_TAIL <- 0.01                           # [TBD: ADR-0031] cauda split x acoplado

dlp_tier <- function() if (exists(".pc_gates_state", envir = globalenv())) get(".pc_gates_state", envir = globalenv())$tier else "parity"
dlp_gate_tier <- function(min) if (exists("pc_gate_tier", mode = "function")) pc_gate_tier(min) else invisible(TRUE)

## ferramentas de desenvolvimento (P.0f/P.0g) num env filho da fundacao (nunca o global)
dlp_tools <- local({
  e <- NULL
  function() {
    if (!is.null(e)) return(e)
    dl_fx_prep()
    e <<- new.env(parent = dl_lib)
    for (f in c("plugin_golden.R", "plugin_parity.R")) sys.source(file.path(dl_root(), "scripts", "tools", f), envir = e)
    e
  }
})

dlp_spec <- function(path = DLP_UNITS()) dlp_tools()$pc_golden_units(path)
dlp_group_units <- function(g) dlp_tools()$pc_golden_group_units(dlp_spec(), g)
dlp_unit <- function(id) { us <- dlp_spec()$units; us[[which(vapply(us, `[[`, "", "id") == id)]] }

## grupos do tier: parity => smoke; parity-full => smoke + foundation (+ fanout se declarado, DL.10); `--units <grupo>` vence
dlp_groups <- function() {
  g <- if (exists("pc_gate_units", mode = "function")) pc_gate_units(NULL) else NULL
  if (!is.null(g)) return(g)
  have <- names(dlp_spec()$groups)
  if (identical(dlp_tier(), "parity-full")) intersect(c("smoke", "foundation", "fanout"), have) else "smoke"
}

dlp_units <- function(groups) {
  us <- unlist(lapply(groups, function(g) dlp_group_units(g)), recursive = FALSE)
  us[!duplicated(vapply(us, `[[`, "", "id"))]
}

## --- golden.lock: ausente/divergente => mensagem de regerar (nunca skip) -----------------------------------------
## Devolve NULL (ok) ou o texto ASCII da falha. Confere sha de arquivo, fingerprint do runtime do ORACULO (Rscript e env do
## invocation.json do golden) e as particoes de dados da unidade.
dlp_check_golden <- function(u, root = DLP_GOLDEN(), lock = DLP_LOCK(), data_dir = dlp_store(), rscript = DLP_ORACLE_RSCRIPT) {
  bad <- function(why) sprintf("%s: unidade %s: %s", DLP_REGEN, u$id, why)
  if (!file.exists(lock)) return(sprintf("%s: golden.lock ausente", DLP_REGEN))
  lk <- jsonlite::read_json(lock, simplifyVector = FALSE)
  e <- lk$units[[u$id]]
  if (is.null(e)) return(bad("ausente do golden.lock"))
  d <- dlp_tools()$.pc_golden_unit_dir(root, DLP_MODEL, u)
  if (!dir.exists(d)) return(bad(sprintf("sem golden em %s", d)))
  for (f in names(e$files)) {
    p <- file.path(d, f)
    if (!file.exists(p)) return(bad(sprintf("%s ausente", f)))
    if (!identical(dlp_tools()$.pc_golden_sha(p), e$files[[f]])) return(bad(sprintf("sha256 de %s difere do golden.lock", f)))
  }
  if (!file.exists(rscript)) return(bad(sprintf("R do oraculo ausente (%s); fingerprint nao conferivel", rscript)))
  inv <- jsonlite::read_json(file.path(d, "invocation.json"), simplifyVector = TRUE)
  fp <- dlp_tools()$pc_runtime_fingerprint(names(e$fingerprint$packages), rscript, as.character(inv$env))
  for (k in c("R", "platform", "blas"))
    if (!identical(fp[[k]], e$fingerprint[[k]])) return(bad(sprintf("fingerprint %s difere do lock", k)))
  for (p in names(e$fingerprint$packages))
    if (!identical(fp$packages[[p]], e$fingerprint$packages[[p]])) return(bad(sprintf("fingerprint do pacote %s difere do lock", p)))
  dg <- dlp_tools()$pc_data_digest(data_dir, u)
  if (!identical(dg$aggregate, e$data_digest$aggregate)) return(bad("particoes de dados diferem do golden.lock"))
  NULL
}

dlp_golden <- function(u, root = DLP_GOLDEN()) dlp_tools()$.pc_parity_read_golden(u, list(u), root, DLP_MODEL, "Etc/GMT+3")

## --- store real por overlay (nada gravado em data/) ----------------------------------------------------------------
.dlp <- new.env(parent = emptyenv())

dlp_store <- function() {
  if (!is.null(.dlp$store)) return(.dlp$store)
  src <- file.path(dl_root(), "data")
  if (!dir.exists(src)) return(NA_character_)
  ov <- tempfile("dlp-store-"); dir.create(ov)
  for (d in c("load", "weather", "auxiliary")) file.symlink(normalizePath(file.path(src, d)), file.path(ov, d))
  file.copy(file.path(dl_root(), "config", "deck-manifest.store.json"), file.path(ov, "manifest.json"))
  .dlp$store <- ov
  ov
}

dlp_quiet <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressWarnings(suppressMessages(dl_with_cli(expr))), type = "output")
  out
}

## Montagem da unidade, a mesma de .pc_parity_envelope (plugin_parity.R): deck cortado em available_until, cortes de
## prevcarga_resolve_cuts, params/seed LITERAIS da unidade e janela da fase (history_days 1825). O artefato fica a mao para o CONTROL.
dlp_setup <- function(u, params = NULL, seed = NULL) {
  tl <- dlp_tools()
  plugin <- dl_lib$prevcarga_plugin_load(DLP_MODEL, dl_models())
  deck <- tl$.pc_parity_deck(u, dlp_store())
  ctx <- list(data_dir = deck, root = dl_root(), weather_issue_cutoff = u$weather_issue_cutoff,
              topology_path = file.path(DLP_PARITY(), "topology.yaml"))
  lag <- stats::setNames(plugin$descriptor$data_lag_days, DLP_MODEL)
  rc <- suppressWarnings(dl_lib$prevcarga_resolve_cuts(ctx, origin = u$origin, model = DLP_MODEL, data_lag_days = lag))
  row <- rc$cuts[rc$cuts$model == DLP_MODEL, ]
  cuts <- list(forecast_origin = rc$forecast_origin, origin_day = rc$forecast_origin, split = row$split, tz = rc$tz,
               load_label = rc$load_label, weather_issue_cutoff = rc$weather_issue_cutoff,
               available_until = rc$available_until, weather_issue_limit = rc$weather_issue_limit,
               train_origin = row$train_origin, last_data_day = row$last_data_day)
  sp <- plugin$spec
  if (length(params)) sp$params <- utils::modifyList(sp$params, params)
  if (!is.null(seed)) sp$seed <- as.integer(seed)
  read <- function(origin, hist) {
    p <- suppressWarnings(dl_lib$prevcarga_read_panel(ctx, origin, u$area, sp$horizon_days, hist))
    lim <- attr(p$weather_forecast, "weather_issue_limit")
    fc <- dl_lib$prevcarga_read_weather(ctx, "forecast", u$area, origin - hist, origin + sp$horizon_days + 1L)
    p$weather_forecast <- fc[issue_time <= lim]       # todas as emissoes ate o limite (safra por origem nativa); painel de 30 min CRU
    p
  }
  list(plugin = plugin, deck = deck, cuts = cuts, sp = sp, read = read, hist = sp$history_days, u = u)
}

## Clean-room da unidade pelo despachante, em processo. mode "split": treino em last_data_day (= O-1) + infer em O (o caminho do
## oraculo); mode "coupled": prevcarga_plugin_forecast sobre o painel de infer (janela +1 dia: o treino do acoplado ve O-1 e o do
## split le ate last_data_day com a MESMA janela de 1825 dias, ver docs/estado/dessem_linear.md DL.8). `params`/`seed` mudam o
## clean-room (CONTROL); a unidade e o golden ficam os mesmos.
dlp_run <- function(u, mode = c("split", "coupled"), params = NULL, seed = NULL) {
  mode <- match.arg(mode)
  key <- paste(u$id, mode, paste(names(params), unlist(params), collapse = ","), if (is.null(seed)) "-" else seed)
  if (!is.null(.dlp$runs[[key]])) return(.dlp$runs[[key]])
  s <- dlp_setup(u, params, seed)
  on.exit(unlink(s$deck, recursive = TRUE), add = TRUE)
  cuts <- c(s$cuts, list(spec = s$sp))
  out <- NULL
  dlp_quiet({
    if (identical(mode, "split")) {
      art <- dl_lib$prevcarga_plugin_train(s$plugin, s$read(s$cuts$last_data_day, s$hist$train), c(cuts, list(phase = "train")), s$sp, list())
      ip <- s$read(u$origin, s$hist$infer)
      env <- dl_lib$prevcarga_plugin_infer(s$plugin, art, ip, c(cuts, list(phase = "infer")), list())
      out <- list(env = env, art = art, infer_panel = ip, setup = s)
    } else {
      ip <- s$read(u$origin, s$hist$infer + 1L)
      env <- dl_lib$prevcarga_plugin_forecast(s$plugin, ip, c(cuts, list(phase = "forecast")), s$sp, list())
      out <- list(env = env, art = NULL, infer_panel = ip, setup = s)
    }
  })
  .dlp$runs[[key]] <- out
  out
}

dlp_compare <- function(run, gold, u) dlp_tools()$prevcarga_parity_compare(run$env, gold, u, tol = DLP_TOL)

dlp_report_line <- function(r) sprintf("%s max|D|=%.3e worst=%s", r$unit, r$max_abs_delta, paste(r$worst, collapse = ","))

## regua split x acoplado (ADR-0031): dia D (O) e cauda O+1..O+8 do infer separado contra o forecast acoplado
dlp_split_ruler <- function(a, b) {
  da <- a$env$values; db <- b$env$values
  d0 <- max(abs(da[1L, ] - db[1L, ]))
  tail_w <- sum(abs(da[-1L, ] - db[-1L, ])) / sum(abs(da[-1L, ]))
  list(day0_delta = d0, tail_wmape = tail_w, max_abs_delta = max(abs(da - db)), same_shape = identical(dim(da), dim(db)))
}

## --- relatorio da fundacao ------------------------------------------------------------------------------------------
## JSON canonico (chaves em ordem radix, numeros em 15 digitos): a MESMA funcao do runner (run_all_gates.R, .dl_report_units_sha)
dlp_canon <- function(x) {
  if (is.list(x)) { x <- lapply(x, dlp_canon); if (!is.null(names(x))) x <- x[order(names(x), method = "radix")] }
  x
}
dlp_sha_text <- function(txt) digest::digest(txt, algo = "sha256", serialize = FALSE)
dlp_sha_obj <- function(x) dlp_sha_text(as.character(jsonlite::toJSON(dlp_canon(x), auto_unbox = TRUE, null = "null", digits = 15, na = "null")))

## entrada por unidade: so escalares (lista), valores em 12 digitos significativos (relido do JSON da o mesmo texto canonico)
dlp_report_entry <- function(u, r, lock, split = NULL) {
  e <- list(unit = u$id, area = u$area, origin = format(u$origin), layout = r$layout, ok = isTRUE(r$ok),
            max_abs_delta = signif(r$max_abs_delta, 12), n_compared = as.integer(r$n_compared),
            fora_da_faixa = as.integer(r$fora_da_faixa), fora_da_faixa_long = as.integer(r$fora_da_faixa_long),
            shape_ok = isTRUE(r$shape_ok), na_ok = isTRUE(r$na_ok), delivery_ok = isTRUE(r$delivery_ok), cut_ok = isTRUE(r$cut_ok),
            day0_source = r$day0_source, golden_native_sha256 = lock$units[[u$id]]$files[["native.rds"]])
  if (!is.null(split)) {
    e$split_day0_delta <- signif(split$day0_delta, 12); e$split_tail_wmape <- signif(split$tail_wmape, 12)
    e$split_max_abs_delta <- signif(split$max_abs_delta, 12)
    e$split_ok <- isTRUE(split$day0_delta <= DLP_TOL && split$tail_wmape <= DLP_WMAPE_TAIL)
  }
  e$sha256 <- dlp_sha_obj(e)
  e
}

## grava o relatorio (sha256 de cada unidade e do conjunto); todas as unidades ok => all_ok
dlp_report_write <- function(entries, path = DLP_REPORT(), groups = "foundation") {
  lock_sha <- digest::digest(DLP_LOCK(), algo = "sha256", file = TRUE)
  units <- unname(entries[order(names(entries), method = "radix")])
  rep <- list(model = DLP_MODEL, groups = as.list(groups), tolerance = DLP_TOL, wmape_tail_limit = DLP_WMAPE_TAIL,
              golden_lock_sha256 = lock_sha, oracle_commit = jsonlite::read_json(DLP_LOCK())$oracle_commit,
              all_ok = all(vapply(units, function(e) isTRUE(e$ok) && isTRUE(e$split_ok %||% TRUE), NA)), units = units)
  rep$sha256 <- dlp_sha_obj(units)
  txt <- jsonlite::toJSON(rep, auto_unbox = TRUE, null = "null", digits = 15, na = "null", pretty = TRUE)
  writeBin(charToRaw(paste0(paste(as.character(txt), collapse = "\n"), "\n")), path)
  invisible(rep)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

## regerar o golden: mensagem unica, nunca skip
dlp_need_golden <- function(u) {
  if (is.na(dlp_store())) { testthat::fail(sprintf("%s: store de dados ausente (%s)", DLP_REGEN, file.path(dl_root(), "data"))); return(FALSE) }
  msg <- dlp_check_golden(u)
  if (!is.null(msg)) { testthat::fail(msg); return(FALSE) }
  TRUE
}

## --------------------------------------------------------------------------------------------------------------
test_that("[PLUGINS] units_yaml: esquema P.0f, grupos smoke/foundation, oracle_cut start_label (08:30), oracle_env EXOGENA, regras de topo", {
  spec <- dlp_spec()
  us <- spec$units
  ids <- vapply(us, `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0L)
  expect_identical(spec$groups$foundation, sprintf("seco-2024-10-%02d", c(1L, 8L, 15L, 22L, 29L)))
  expect_length(intersect(spec$groups$smoke, spec$groups$foundation), 3L)            # 3 origens da fundacao no smoke
  expect_setequal(setdiff(spec$groups$smoke, spec$groups$foundation),
                  c("sp-2024-12-22", "sp-2024-11-15", "go-2024-10-05", "ac-2025-06-30"))   # robustez real
  expect_true(all(c(spec$groups$smoke, spec$groups$foundation) %in% ids))
  for (u in us) {
    O <- u$origin
    expect_identical(u$variant, "MED")                                              # nenhuma unidade MAX/MXMN nesta fatia
    expect_identical(u$oracle_cut, "start_label"); expect_identical(u$day0_source, "native")
    expect_identical(u$weather_issue_cutoff, "origin_midnight_utc")
    expect_identical(u$history_days, 1825L)                                        # history_start = O - 1825 (train == infer)
    expect_identical(format(u$available_until, "%H:%M"), "09:00")                   # hora cheia
    expect_identical(dlp_tools()$.pc_golden_cut_arg(u), paste0(format(O), "T08:30"))   # o oraculo recebe available_until - 30 min
    expect_identical(u$oracle_env$EXOGENA, "MED")
    expect_null(u$cuts)                                                            # janela unica: history_days
  }
  ## so os 5 outubros de SECO na fundacao; cada unidade tem o proprio diretorio de golden (area x origem x variante)
  dirs <- vapply(us, function(u) dlp_tools()$.pc_golden_unit_dir("g", DLP_MODEL, u), "")
  expect_false(anyDuplicated(dirs) > 0L)
  ## o env do oracle_env existe so no filho do oraculo: o pai nunca o ve
  expect_identical(Sys.getenv("EXOGENA"), "")
  ## unidade fora do contrato e recusada nomeando o campo (regras de topo): corte 09:30, variante MAX, history 730
  for (mut in list(function(x) { x$available_until <- "2024-10-01T09:30"; x }, function(x) { x$variant <- "MAX"; x },
                   function(x) { x$history_days <- 730L; x }, function(x) { x$cuts <- list(train = list(train_origin = "2024-09-30")); x })) {
    bad <- yaml::read_yaml(DLP_UNITS()); bad$units <- bad$units[1L]; bad$groups <- list(smoke = bad$units[[1L]]$id)
    bad$units[[1L]] <- mut(bad$units[[1L]])
    f <- withr::local_tempfile(fileext = ".yaml"); yaml::write_yaml(bad, f)
    e <- dl_err(dlp_spec(f))
    expect_identical(e$id, "tools.bad_units")
  }
})

test_that("[PLUGINS] paridade: values O..O+8 e delivery vs oraculo, max|d| <= 1e-5 MW (Delta=0); O+9 do oraculo fora_da_faixa; regua split x acoplado", {
  dlp_gate_tier("parity")
  groups <- dlp_groups()
  us <- dlp_units(groups)
  expect_gt(length(us), 0L)
  lock <- jsonlite::read_json(DLP_LOCK(), simplifyVector = FALSE)
  entries <- list(); rep <- list()
  for (u in us) {
    if (!dlp_need_golden(u)) next
    run <- dlp_run(u)
    gold <- dlp_golden(u)
    r <- dlp_compare(run, gold, u)
    rep[[u$id]] <- r
    expect_identical(r$layout, "dessem_semihourly_48")
    expect_identical(r$day0_source, "native")
    expect_true(isTRUE(r$shape_ok) && isTRUE(r$na_ok), info = u$id)                       # values 9x48: mesma forma e mesmo padrao de NA
    expect_identical(dim(run$env$values), c(9L, 48L))
    expect_identical(dim(gold$native$values), c(10L, 48L))                                # O..O+9 do oraculo
    expect_true(isTRUE(r$delivery_ok), info = u$id)
    expect_identical(as.integer(r$delivery_missing), 0L)                                  # toda celula da entrega tem par no long do oraculo
    expect_true(isTRUE(r$cut_ok), info = u$id)
    expect_identical(as.integer(r$fora_da_faixa), 48L)                                    # O+9 do oraculo (1 dia x 48)
    expect_identical(as.integer(r$fora_da_faixa_long), 48L)
    expect_identical(run$env$available_until, u$available_until)
    expect_lte(r$max_abs_delta, DLP_TOL)                                                  # Delta = 0 (limiar fixo)
    expect_true(isTRUE(r$ok), info = dlp_report_line(r))
    ## delivery$interval_end = timestamp do oraculo + 30 min em toda celula: min(entrega) = available_until + 30 min
    expect_identical(min(run$env$delivery$interval_end), u$available_until + lubridate::minutes(30))
    ## gate02 no deck REAL: finita, positiva e mediana na faixa do SIN (5000-120000 MW)
    expect_true(all(is.finite(run$env$values)) && all(run$env$values > 0), info = u$id)
    expect_gt(stats::median(run$env$delivery$y_pred), 5000); expect_lt(stats::median(run$env$delivery$y_pred), 120000)
    ## regua split x acoplado nas origens da fundacao (area SECO): dia D == separado (<= 1e-5), cauda WMAPE <= 1% [TBD: ADR-0031]
    sp <- NULL
    if (identical(u$area, "SECO")) {
      sp <- dlp_split_ruler(run, dlp_run(u, "coupled"))
      expect_true(sp$same_shape, info = u$id)
      expect_lte(sp$day0_delta, DLP_TOL)
      expect_lte(sp$tail_wmape, DLP_WMAPE_TAIL)
    }
    entries[[u$id]] <- dlp_report_entry(u, r, lock, sp)
  }
  ## o grupo `foundation` inteiro rodou: grava o relatorio da fundacao (pre-condicao do fan-out, DL.10)
  found <- dlp_spec()$groups$foundation
  if (all(found %in% names(entries)) && length(entries)) {
    rpt <- dlp_report_write(entries[found])
    expect_true(file.exists(DLP_REPORT()))
    message(sprintf("foundation_report all_ok=%s sha256=%s unidades=%d max_abs=%.3g", rpt$all_ok, rpt$sha256, length(found),
                    max(vapply(entries[found], function(e) e$max_abs_delta, 0))))
  }
  message(sprintf("parity %s unidades=%d max_abs=%.3g", paste(groups, collapse = "+"), length(rep),
                  if (length(rep)) max(vapply(rep, function(r) r$max_abs_delta, 0)) else NA_real_))
})

test_that("[PLUGINS] paridade_control: exogena MED->MAX e seed 42->43 acusam > 1e-5; artefato de outra origem => contract; golden/lock adulterados falham (regere o golden); oraculo intacto", {
  dlp_gate_tier("parity")
  u <- dlp_unit(DLP_BASE)
  if (!dlp_need_golden(u)) return(invisible(NULL))                                       # sem golden valido o controle nao prova nada
  gold <- dlp_golden(u)
  base <- dlp_compare(dlp_run(u), gold, u)
  expect_true(isTRUE(base$ok), info = dlp_report_line(base))                  # sem a base verde o CONTROL nao prova nada
  ## CONTROL com dente: exogena MED -> MAX (outra variante/model_key) e seed 42 -> 43 mudam a previsao alem de 1e-5, mesma forma
  mx <- dlp_compare(dlp_run(u, params = list(exogena = "MAX")), gold, u)
  expect_gt(mx$max_abs_delta, DLP_TOL); expect_false(isTRUE(mx$ok)); expect_true(isTRUE(mx$shape_ok))
  sd43 <- dlp_compare(dlp_run(u, seed = 43L), gold, u)
  expect_gt(sd43$max_abs_delta, DLP_TOL); expect_false(isTRUE(sd43$ok)); expect_true(isTRUE(sd43$shape_ok))
  message(sprintf("controls exogena_MAX max|D|=%.3g seed_43 max|D|=%.3g", mx$max_abs_delta, sd43$max_abs_delta))
  ## CONTROL (AC3): artefato de outra origem no infer => [prevcarga/contract] (a origem e conferida antes de qualquer ajuste)
  run <- dlp_run(u)
  s <- run$setup
  e <- dl_err(dl_lib$prevcarga_plugin_infer(s$plugin, run$art, run$infer_panel,
                                            c(modifyList(s$cuts, list(origin_day = u$origin - 1L)), list(spec = s$sp, phase = "infer")), list()))
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
  ## golden ausente (diretorio vazio no lugar do golden) e golden.lock ausente
  m <- dlp_check_golden(u, root = withr::local_tempdir())
  expect_match(m, DLP_REGEN, fixed = TRUE); expect_match(m, u$id, fixed = TRUE)
  expect_match(dlp_check_golden(u, lock = file.path(withr::local_tempdir(), "nao-existe.lock")), DLP_REGEN, fixed = TRUE)
  ## fingerprint do runtime (pacote), digest de dados e sha de arquivo adulterados => regere
  lk <- jsonlite::read_json(DLP_LOCK(), simplifyVector = FALSE)
  tamper <- function(f) {
    p <- tempfile(fileext = ".lock")
    on.exit(unlink(p), add = TRUE)
    writeLines(jsonlite::toJSON(f(lk), auto_unbox = TRUE, null = "null", digits = NA), p)
    dlp_check_golden(u, lock = p)
  }
  m1 <- tamper(function(l) { l$units[[u$id]]$fingerprint$packages$WeightSVM <- "0.0.0"; l })
  expect_match(m1, DLP_REGEN, fixed = TRUE); expect_match(m1, "WeightSVM", fixed = TRUE)
  m2 <- tamper(function(l) { l$units[[u$id]]$data_digest$aggregate <- strrep("0", 64L); l })
  expect_match(m2, DLP_REGEN, fixed = TRUE)
  m3 <- tamper(function(l) { l$units[[u$id]]$files[["native.rds"]] <- strrep("0", 64L); l })
  expect_match(m3, DLP_REGEN, fixed = TRUE)
  ## golden de 2 geracoes byte-identicas e corte do oraculo 08:30: o invocation.json grava o argumento passado ao oraculo
  inv <- jsonlite::read_json(file.path(dlp_tools()$.pc_golden_unit_dir(DLP_GOLDEN(), DLP_MODEL, u), "invocation.json"), simplifyVector = TRUE)
  expect_identical(inv$args[which(inv$args == "--available-until") + 1L], paste0(format(u$origin), "T08:30"))
  expect_identical(inv$oracle_commit, lk$units[[u$id]]$oracle_commit)
  ## CONTROL: --out dentro da arvore do oraculo => exit 2 (tools.out_inside_oracle)
  if (dir.exists(file.path(DLP_ORACLE, ".git"))) {
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(file.path(dl_root(), "scripts", "tools", "plugin_golden.R")), "--model", DLP_MODEL, "--units", "smoke",
        "--oracle", DLP_ORACLE, "--out", file.path(DLP_ORACLE, "golden-fora"), "--data", dlp_store()), stdout = TRUE, stderr = TRUE))
    expect_identical(attr(out, "status"), 2L)
    expect_false(dir.exists(file.path(DLP_ORACLE, "golden-fora")))
    ## oraculo intacto: o codigo do oraculo (tracked, e untracked fora dos planos docs/roadmap) nao foi modificado
    st <- suppressWarnings(system2("git", c("--no-optional-locks", "-C", shQuote(DLP_ORACLE), "status", "--porcelain"), stdout = TRUE, stderr = FALSE))
    expect_length(st[!grepl("docs/roadmap/", st, fixed = TRUE)], 0L)
  }
})
