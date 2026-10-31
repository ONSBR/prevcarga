## test-parity.R -- paridade do plugin dessem-regdin contra o ORACULO (RD.6), limiar unico 1e-5 MW absoluto (ADR-0017), mesma plataforma.
## Tiers: `[PLUGINS] paridade_<grupo>:` e `paridade_control:` so rodam em --tier parity|parity-full (pc_gate_tier); sem o runner
## (test_dir direto) vale o tier parity (grupo smoke). O golden (.state/golden, nunca versionado) e o `golden.lock` (versionado) vem
## de scripts/tools/plugin_golden.R (driver tests/parity/oracle_driver.R: caminho de INFER do oraculo = run_train + run_infer com o
## corte start_label = available_until - 30 min, EXOGENA no env do filho); golden ausente, digest de dados ou fingerprint de runtime
## divergente FALHA com "regere o golden" (nunca skip, nunca troca de limiar). O clean-room roda EM PROCESSO pelo despachante real
## (leitor -> cortes de prevcarga_resolve_cuts com weather_issue_cutoff = origin_midnight_utc -> prevcarga_plugin_train ->
## prevcarga_plugin_infer), com a mesma montagem de scripts/tools/plugin_parity.R (.pc_parity_envelope) e o deck da unidade cortado em
## available_until; compara por prevcarga_parity_compare (layout dessem_semihourly_48 = `dessem_matrix_nx48` do ticket, day0_source
## native): `values` 9x48 (O..O+8) contra native.rds (10x48; o O+9 do oraculo sai como `fora_da_faixa`) e `delivery` contra o
## long.parquet do oraculo (interval_end = timestamp + 30 min).
## DIVERGENCIA DELIBERADA (Gabriel 2026-10-30): hora incompleta e DESCARTADA tambem na previsao de clima (to_hourly da fundacao); o
## oraculo tira a media dos slots existentes. Uma emissao 00Z termina em O+9 23:00 (hora 23 com UM slot): o dia O+9 fica curto e e
## persistido, o que alcanca as ULTIMAS horas anteriores (spline monoH.FC de desagregaModeloSH e normalizacao do treino): os
## intervalos nomeados saem de `rdp_partial_from`. A comparacao e EXATA (<= 1e-5) em todo o resto de O..O+8; a divergencia nomeada e
## MEDIDA e listada; o CONTROL emula a media de hora parcial do oraculo numa copia do painel de infer e TUDO tambem fecha em <= 1e-5.
## BRACOS INFORMATIVOS (nunca vermelhos; ausentes => `null` no relatorio): composicao `.rd_fit_pass(mm = NULL)` + `.rd_predict_pass(mm)`
## e o corte 00:00 do oraculo, gerados por `oracle_driver.R --arm` em .state/golden/plugins/dessem-regdin/_arms/<braco>/. Se o plano B
## do RNG estiver ativo (`rng_deviation` nao nulo) o golden do gate passa a ser a composicao e o nativo vira braco informativo.
## Relatorio de paridade: JSON canonico com sha256 em .state/parity/dessem-regdin/parity_report.json (gitignorado).
suppressWarnings(suppressMessages(library(testthat)))

RDP_MODEL <- "dessem-regdin"
RDP_TOL <- 1e-5                                  # unico limiar (ADR-0017): MW absoluto, trilho mesmo-hardware
RDP_PARITY <- function() file.path(rd_root(), "models", RDP_MODEL, "tests", "parity")
RDP_UNITS <- function() file.path(RDP_PARITY(), "units.yaml")
RDP_LOCK <- function() file.path(RDP_PARITY(), "golden.lock")
RDP_GOLDEN <- function() file.path(rd_root(), ".state", "golden")
RDP_REPORT <- function() file.path(rd_root(), ".state", "parity", RDP_MODEL, "parity_report.json")
RDP_ORACLE_RSCRIPT <- function() file.path(R.home("bin"), "Rscript")   # o golden do regdin e gerado em R 4.5.3 (fable/tsibble so la)
RDP_ORACLE <- "/opt/ons-source/prevcargadessem3"
RDP_REGEN <- "golden ausente ou divergente: regere com scripts/tools/plugin_golden.R --model dessem-regdin"
RDP_BASE <- "rgd-med-seco-20241015-0900"         # unidade-base dos CONTROLs
RDP_REACH_HOURS <- 3L
`%||%` <- function(a, b) if (is.null(a)) b else a

rdp_tier <- function() if (exists(".pc_gates_state", envir = globalenv())) get(".pc_gates_state", envir = globalenv())$tier else "parity"
rdp_gate_tier <- function(min) if (exists("pc_gate_tier", mode = "function")) pc_gate_tier(min) else invisible(TRUE)

## ferramentas de desenvolvimento (P.0f/P.0g) num env filho da fundacao (nunca o global)
rdp_tools <- local({
  e <- NULL
  function() {
    if (!is.null(e)) return(e)
    rd5_prep()
    e <<- new.env(parent = rd_lib)
    for (f in c("plugin_golden.R", "plugin_parity.R")) sys.source(file.path(rd_root(), "scripts", "tools", f), envir = e)
    e
  }
})

rdp_spec <- function(path = RDP_UNITS()) rd_with_cli(rdp_tools()$pc_golden_units(path))
rdp_group_units <- function(g) rdp_tools()$pc_golden_group_units(rdp_spec(), g)
rdp_unit <- function(id) { us <- rdp_spec()$units; us[[which(vapply(us, `[[`, "", "id") == id)]] }
rdp_plan_b <- function() !is.null(rd_fx_plugin()$descriptor$rng_deviation)

## grupos do tier: parity => smoke; parity-full => smoke + foundation + fanout; `--units <grupo>` vence
rdp_groups <- function() {
  g <- if (exists("pc_gate_units", mode = "function")) pc_gate_units(NULL) else NULL
  if (!is.null(g)) return(g)
  have <- names(rdp_spec()$groups)
  if (identical(rdp_tier(), "parity-full")) intersect(c("smoke", "foundation", "fanout"), have) else "smoke"
}

## unidades do grupo g SE o grupo esta no escopo do tier/`--units`; senao `[tier]` (N/A no runner)
rdp_units_of <- function(g) {
  if (!(g %in% rdp_groups())) testthat::skip(sprintf("[tier] grupo %s fora do escopo do tier %s", g, rdp_tier()))
  us <- rdp_group_units(g)
  only <- getOption("prevcarga.parity_only")          # opcao de DESENVOLVIMENTO (rodar poucas unidades); nunca usada pelo runner
  if (length(only)) us <- us[vapply(us, `[[`, "", "id") %in% only]
  us
}

## --- store real por overlay (nada gravado em data/) ------------------------------------------------------------------------
.rdp <- new.env(parent = emptyenv())
.rdp$runs <- list(); .rdp$entries <- list()

rdp_store <- function() {
  if (!is.null(.rdp$store)) return(.rdp$store)
  src <- file.path(rd_root(), "data")
  if (!dir.exists(src)) return(NA_character_)
  ov <- tempfile("rdp-store-"); dir.create(ov)
  for (d in c("load", "weather", "auxiliary")) file.symlink(normalizePath(file.path(src, d)), file.path(ov, d))
  file.copy(file.path(rd_root(), "config", "deck-manifest.store.json"), file.path(ov, "manifest.json"))
  .rdp$store <- ov
  ov
}

## --- golden.lock: ausente/divergente => mensagem de regerar (nunca skip) ----------------------------------------------------
## NULL (ok) ou o texto ASCII da falha: sha de arquivo, fingerprint do runtime (Rscript e env do invocation.json) e particoes de dados.
rdp_check_golden <- function(u, root = RDP_GOLDEN(), lock = RDP_LOCK(), data_dir = rdp_store(), rscript = RDP_ORACLE_RSCRIPT()) {
  bad <- function(why) sprintf("%s: unidade %s: %s", RDP_REGEN, u$id, why)
  if (!file.exists(lock)) return(sprintf("%s: golden.lock ausente", RDP_REGEN))
  lk <- jsonlite::read_json(lock, simplifyVector = FALSE)
  e <- lk$units[[u$id]]
  if (is.null(e)) return(bad("ausente do golden.lock"))
  d <- rdp_tools()$.pc_golden_unit_dir(root, RDP_MODEL, u)
  if (!dir.exists(d)) return(bad(sprintf("sem golden em %s", d)))
  for (f in names(e$files)) {
    p <- file.path(d, f)
    if (!file.exists(p)) return(bad(sprintf("%s ausente", f)))
    if (!identical(rdp_tools()$.pc_golden_sha(p), e$files[[f]])) return(bad(sprintf("sha256 de %s difere do golden.lock", f)))
  }
  inv <- jsonlite::read_json(file.path(d, "invocation.json"), simplifyVector = TRUE)
  fp <- rdp_tools()$pc_runtime_fingerprint(names(e$fingerprint$packages), rscript, as.character(inv$env))
  for (k in c("R", "platform", "blas"))
    if (!identical(fp[[k]], e$fingerprint[[k]])) return(bad(sprintf("fingerprint %s difere do lock", k)))
  for (p in names(e$fingerprint$packages))
    if (!identical(fp$packages[[p]], e$fingerprint$packages[[p]])) return(bad(sprintf("fingerprint do pacote %s difere do lock", p)))
  dg <- rdp_tools()$pc_data_digest(data_dir, u)
  if (!identical(dg$aggregate, e$data_digest$aggregate)) return(bad("particoes de dados diferem do golden.lock"))
  NULL
}

rdp_need_golden <- function(u) {
  if (is.na(rdp_store())) { testthat::fail(sprintf("%s: store de dados ausente (%s)", RDP_REGEN, file.path(rd_root(), "data"))); return(FALSE) }
  msg <- rdp_check_golden(u)
  if (!is.null(msg)) { testthat::fail(msg); return(FALSE) }
  TRUE
}

## golden do GATE: o nativo; com o plano B ativo, a composicao (e o nativo vira informativo)
rdp_gold <- function(u) {
  tl <- rdp_tools()
  if (rdp_plan_b()) {
    d <- rdp_arm_dir(u, "composition")
    if (is.null(d)) stop(sprintf("%s: unidade %s: plano B ativo sem o braco composition", RDP_REGEN, u$id), call. = FALSE)
    return(tl$.pc_parity_read_dir(d, "Etc/GMT+3"))
  }
  tl$.pc_parity_read_golden(u, list(u), RDP_GOLDEN(), RDP_MODEL, "Etc/GMT+3")
}
rdp_arm_dir <- function(u, arm) {
  d <- file.path(RDP_GOLDEN(), "plugins", RDP_MODEL, "_arms", arm, if (is.null(u$variant)) "_" else u$variant, u$area, format(u$origin))
  if (file.exists(file.path(d, "native.rds"))) d else NULL
}

## --- requires_report (relatorio da fundacao do dessem-linear, DL.9) -------------------------------------------------------
## NULL (ok) ou o motivo da recusa: o relatorio existe, esta verde e tem o sha256 declarado na unidade
rdp_report_problem <- function(u, root = rd_root()) {
  rr <- u$requires_report
  if (is.null(rr)) return("unidade sem requires_report")
  f <- file.path(root, rr$path)
  if (!file.exists(f)) return(sprintf("relatorio ausente: %s", rr$path))
  rp <- tryCatch(jsonlite::read_json(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(rp) || !is.list(rp$units) || !length(rp$units)) return(sprintf("relatorio ilegivel: %s", rr$path))
  if (!isTRUE(rp$all_ok) || !all(vapply(rp$units, function(x) isTRUE(x$ok), NA))) return(sprintf("relatorio vermelho: %s", rr$path))
  if (!identical(rp$sha256, rr$sha256)) return(sprintf("sha256 do relatorio difere do declarado: %s", rr$path))
  NULL
}

## --- montagem do clean-room da unidade --------------------------------------------------------------------------------------
## A mesma de .pc_parity_envelope (plugin_parity.R): deck cortado em available_until, cortes de prevcarga_resolve_cuts, params/seed
## LITERAIS da unidade e janela da fase (history_days 1825; `hist_delta` = CONTROL history_days.train -7). O painel de infer e lido
## com +1 dia de aquecimento (o oraculo le O - history_days mais 1 dia nas duas fases); o de treino e o MESMO cortado em last_data_day.
rdp_setup <- function(u, params = NULL, hist_delta = 0L) {
  rd5_prep()
  tl <- rdp_tools()
  plugin <- rd_fx_plugin()
  deck <- tl$.pc_parity_deck(u, rdp_store())
  ctx <- list(data_dir = deck, root = rd_root(), weather_issue_cutoff = u$weather_issue_cutoff,
              topology_path = file.path(RDP_PARITY(), "topology.yaml"))
  lag <- stats::setNames(plugin$descriptor$data_lag_days, RDP_MODEL)
  rc <- suppressWarnings(rd_lib$prevcarga_resolve_cuts(ctx, origin = u$origin, model = RDP_MODEL, data_lag_days = lag))
  row <- rc$cuts[rc$cuts$model == RDP_MODEL, ]
  cuts <- list(forecast_origin = rc$forecast_origin, origin_day = rc$forecast_origin, split = row$split, tz = rc$tz,
               load_label = rc$load_label, weather_issue_cutoff = rc$weather_issue_cutoff,
               available_until = rc$available_until, weather_issue_limit = rc$weather_issue_limit,
               train_origin = row$train_origin, last_data_day = row$last_data_day)
  sp <- plugin$spec
  sp$params$exogena <- u$variant
  if (length(params)) sp$params <- utils::modifyList(sp$params, params)
  hist <- list(train = u$history_days - as.integer(hist_delta), infer = u$history_days - as.integer(hist_delta))
  sp$history_days <- hist
  ## Previsto: TODAS as emissoes ate o limite de O (`weather_issue_limit`, origin_midnight_utc) nas DUAS fases: o split e
  ## origem-escopado e o oraculo treina com as emissoes ate O 00Z (o clima previsto entra na normalizacao do treino)
  read <- function() {
    p <- suppressWarnings(rd_lib$prevcarga_read_panel(ctx, u$origin, u$area, sp$horizon_days, hist$infer + 1L))
    fc <- rd_lib$prevcarga_read_weather(ctx, "forecast", u$area, u$origin - hist$infer - 1L, u$origin + sp$horizon_days + 2L)
    p$weather_forecast <- fc[issue_time <= cuts$weather_issue_limit]       # painel de 30 min CRU
    p
  }
  list(plugin = plugin, deck = deck, cuts = cuts, sp = sp, read = read, u = u)
}

rdp_quiet <- function(expr) {
  out <- NULL
  utils::capture.output(out <- suppressWarnings(suppressMessages(rd_with_cli(expr))), type = "output")
  out
}

## Clean-room da unidade pelo despachante, em processo: `train` ate last_data_day + `infer` em O (o caminho do oraculo). `params` e
## `hist_delta` mudam o clean-room (CONTROL); a unidade e o golden ficam os mesmos.
rdp_run <- function(u, params = NULL, hist_delta = 0L) {
  key <- paste(u$id, paste(names(params), unlist(params), collapse = ","), hist_delta)
  if (!is.null(.rdp$runs[[key]])) return(.rdp$runs[[key]])
  s <- rdp_setup(u, params, hist_delta)
  on.exit(unlink(s$deck, recursive = TRUE), add = TRUE)
  cuts <- c(s$cuts, list(spec = s$sp))
  out <- NULL
  rdp_quiet({
    ip <- s$read()
    tp <- rd_lib$.prevcarga_plugin_train_slice(ip, s$cuts)
    art <- rd_lib$prevcarga_plugin_train(s$plugin, tp, c(cuts, list(phase = "train")), s$sp, list())
    env <- rd_lib$prevcarga_plugin_infer(s$plugin, art, ip, c(cuts, list(phase = "infer")), list())
    out <- list(env = env, art = art, infer_panel = ip, setup = s)
  })
  .rdp$runs[[key]] <- out
  out
}

## DIVERGENCIA DELIBERADA, intervalos NOMEADOS: a regra (hora incompleta descartada tambem no clima previsto) afeta o dia cuja
## ultima hora do clima previsto mais fresco tem UM slot (valid_time :00 sem o :30 seguinte; a 00Z termina em O+9 23:00) e, pelo
## spline monoH.FC e pela normalizacao do treino, as horas anteriores (RDP_REACH_HOURS = 3 h). Devolve o interval_end a partir do
## qual (exclusivo) a unidade esta na divergencia nomeada.
rdp_partial_from <- function(run, u) {
  fc <- data.table::copy(run$infer_panel$weather_forecast)
  data.table::setorder(fc, valid_time, -issue_time)
  fc <- fc[, .SD[1L], by = valid_time]
  fc <- fc[valid_time >= lubridate::force_tz(lubridate::as_datetime(u$origin), lubridate::tz(u$available_until))]
  data.table::setorder(fc, valid_time)
  v <- fc$valid_time
  part <- lubridate::minute(v) == 0L & !((v + lubridate::minutes(30L)) %in% v)
  part[length(v)] <- lubridate::minute(v[length(v)]) == 0L
  if (!any(part)) return(NULL)
  d0 <- lubridate::force_tz(lubridate::as_datetime(lubridate::as_date(v[which(part)[1L]], tz = lubridate::tz(v))), lubridate::tz(v))
  d0 - lubridate::hours(RDP_REACH_HOURS)
}

## interval_end de cada celula de `values` (linha r = dia O+r-1, coluna c termina em 00:00 + 30 min c)
rdp_cell_ends <- function(u, nr) {
  t0 <- lubridate::force_tz(lubridate::as_datetime(u$origin), lubridate::tz(u$available_until))
  matrix(as.numeric(t0 + lubridate::days(rep(seq_len(nr) - 1L, 48L)) + lubridate::minutes(30L * rep(1:48, each = nr))), nr, 48L)
}

## Comparacao da fundacao + decomposicao: delta EXATO fora dos intervalos nomeados (values e delivery) e delta MEDIDO dentro deles.
## `ok` = forma, NA, corte e cobertura da entrega ok e max|D| fora dos intervalos nomeados <= 1e-5 (limiar unico).
rdp_compare <- function(run, gold, u) {
  r <- rdp_tools()$prevcarga_parity_compare(run$env, gold, u, tol = RDP_TOL)
  gm <- gold$native$values[seq_len(nrow(run$env$values)), , drop = FALSE]; em <- run$env$values
  d <- if (identical(dim(gm), dim(em))) abs(gm - em) else matrix(Inf, 1L, 1L)
  from <- rdp_partial_from(run, u)
  tail <- if (is.null(from) || any(is.infinite(d))) matrix(FALSE, nrow(d), ncol(d)) else rdp_cell_ends(u, nrow(d)) > as.numeric(from)
  dl <- run$env$delivery
  ref <- gold$long$y_pred[match(as.numeric(dl$interval_end), as.numeric(gold$long$timestamp + lubridate::minutes(30L)))]
  dd <- abs(dl$y_pred - ref)
  in_tail <- if (is.null(from)) rep(FALSE, nrow(dl)) else as.numeric(dl$interval_end) > as.numeric(from)
  mx <- function(x) if (any(!is.na(x))) max(x, na.rm = TRUE) else 0
  r$max_abs_delta_full <- r$max_abs_delta
  r$max_abs_delta <- max(mx(d[!tail]), mx(dd[!in_tail]))
  r$cells_over <- which(d > RDP_TOL & !tail, arr.ind = TRUE)
  r$delivery_over <- format(dl$interval_end[!in_tail & !is.na(dd) & dd > RDP_TOL], "%m-%d %H:%M")
  r$tail_max_abs_delta <- max(mx(d[tail]), mx(dd[in_tail]))
  r$tail_from <- if (is.null(from)) NA_character_ else format(from, "%Y-%m-%d %H:%M")
  r$tail_intervals <- if (is.null(from)) character() else format(range(dl$interval_end[in_tail]), "%Y-%m-%d %H:%M")
  r$by_dplus <- vapply(seq_len(nrow(d)), function(k) if (all(is.finite(d[k, ]))) max(d[k, ]) else NA_real_, 0)
  r$ok <- isTRUE(r$shape_ok) && isTRUE(r$na_ok) && isTRUE(r$cut_ok) && r$delivery_missing == 0L && r$max_abs_delta <= RDP_TOL
  r
}

## CONTROL da causa: infer com a media de hora parcial do oraculo emulada numa COPIA do painel de infer (acrescenta, em cada emissao,
## o slot :30 que falta na ultima hora, com o valor do slot :00 = a media dos slots existentes). Reusa o artefato do split.
rdp_emulated <- function(run, gold, u) {
  key <- paste0("emu-", u$id)
  if (!is.null(.rdp$runs[[key]])) return(.rdp$runs[[key]])
  s <- run$setup
  ip <- run$infer_panel
  fc <- data.table::copy(ip$weather_forecast)
  last <- fc[, .SD[which.max(valid_time)], by = issue_time]
  last <- last[lubridate::minute(valid_time) == 0L]
  add <- data.table::copy(last)[, valid_time := valid_time + lubridate::minutes(30L)]
  ip$weather_forecast <- data.table::rbindlist(list(fc, add), use.names = TRUE, fill = TRUE)
  cuts <- c(s$cuts, list(spec = s$sp, phase = "infer"))
  env <- NULL
  rdp_quiet(env <- rd_lib$prevcarga_plugin_infer(s$plugin, run$art, ip, cuts, list()))
  r <- rdp_tools()$prevcarga_parity_compare(env, gold, u, tol = RDP_TOL)
  out <- list(r = r, added = nrow(add))
  .rdp$runs[[key]] <- out
  out
}

## Delta informativo POR d_plus (O..O+8) do clean-room contra um braco do oraculo (composicao | cut0000); NULL se o braco nao existe
rdp_arm_delta <- function(run, u, arm) {
  d <- rdp_arm_dir(u, arm)
  if (is.null(d)) return(NULL)
  nat <- readRDS(file.path(d, "native.rds"))$values
  em <- run$env$values
  if (nrow(nat) < nrow(em)) return(NULL)
  vapply(seq_len(nrow(em)), function(k) { x <- abs(nat[k, ] - em[k, ]); if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE) }, 0)
}
rdp_dplus_of <- function(delivery, u)
  as.integer(lubridate::as_date(delivery$interval_end - lubridate::minutes(30L), tz = lubridate::tz(delivery$interval_end)) - u$origin)

## --- relatorio de paridade ----------------------------------------------------------------------------------------------------
rdp_canon <- function(x) {
  if (is.list(x)) { x <- lapply(x, rdp_canon); if (!is.null(names(x))) x <- x[order(names(x), method = "radix")] }
  x
}
rdp_sha_obj <- function(x) digest::digest(as.character(jsonlite::toJSON(rdp_canon(x), auto_unbox = TRUE, null = "null", digits = 15, na = "null")),
                                          algo = "sha256", serialize = FALSE)
rdp_num <- function(x) if (is.null(x)) NULL else lapply(signif(x, 12), function(v) if (is.na(v)) NULL else v)

rdp_report_entry <- function(u, r, lock, emu = NULL, comp = NULL, c0 = NULL, group = NA_character_) {
  e <- list(unit = u$id, group = group, area = u$area, variant = u$variant, origin = format(u$origin),
            available_until = format(u$available_until, "%Y-%m-%d %H:%M"), layout = r$layout, ok = isTRUE(r$ok),
            max_abs_delta = signif(r$max_abs_delta, 12), n_compared = as.integer(r$n_compared),
            fora_da_faixa = as.integer(r$fora_da_faixa), fora_da_faixa_long = as.integer(r$fora_da_faixa_long),
            shape_ok = isTRUE(r$shape_ok), na_ok = isTRUE(r$na_ok), delivery_ok = isTRUE(r$delivery_ok), cut_ok = isTRUE(r$cut_ok),
            day0_source = r$day0_source, gate_golden = if (rdp_plan_b()) "composition" else "native",
            golden_native_sha256 = lock$units[[u$id]]$files[["native.rds"]], rng_deviation = rd_fx_plugin()$descriptor$rng_deviation,
            max_abs_delta_full = signif(r$max_abs_delta_full, 12), deliberate_divergence_from = r$tail_from,
            tail_max_abs_delta = signif(r$tail_max_abs_delta, 12), tail_intervals = as.list(r$tail_intervals))
  e$max_abs_delta_by_dplus <- rdp_num(r$by_dplus)
  if (!is.null(emu)) { e$emulated_partial_hour_max_abs_delta <- signif(emu$max_abs_delta, 12); e$emulated_ok <- isTRUE(emu$ok) }
  ## fit(mm = NULL) + predict(mm) e corte 00:00: `null` (nunca omitido) quando o braco nao foi gerado
  e["informative_composition_by_dplus"] <- list(rdp_num(comp))
  e["informative_cut0000_by_dplus"] <- list(rdp_num(c0))
  e$sha256 <- rdp_sha_obj(e)
  e
}

rdp_report_write <- function(entries, path = RDP_REPORT(), groups = character()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lock_sha <- digest::digest(RDP_LOCK(), algo = "sha256", file = TRUE)
  units <- unname(entries[order(names(entries), method = "radix")])
  rep <- list(model = RDP_MODEL, groups = as.list(groups), tolerance = RDP_TOL, golden_lock_sha256 = lock_sha,
              oracle_commit = jsonlite::read_json(RDP_LOCK())$oracle_commit, plan_b = rdp_plan_b(),
              all_ok = all(vapply(units, function(e) isTRUE(e$ok) && isTRUE(e$emulated_ok %||% TRUE), NA)), units = units)
  rep$sha256 <- rdp_sha_obj(units)
  txt <- jsonlite::toJSON(rep, auto_unbox = TRUE, null = "null", digits = 15, na = "null", pretty = TRUE)
  writeBin(charToRaw(paste0(paste(as.character(txt), collapse = "\n"), "\n")), path)
  invisible(rep)
}

rdp_line <- function(r) sprintf("%s max|D|=%.3e (fora da divergencia nomeada; cheio %.3e, nomeada %.3e desde %s) celulas>tol=%d", r$unit,
                                r$max_abs_delta, r$max_abs_delta_full, r$tail_max_abs_delta, r$tail_from, nrow(r$cells_over))

## verifica UMA unidade contra o golden e acumula o relatorio; devolve o resultado da comparacao
rdp_verify_unit <- function(u, group, lock) {
  rp <- rdp_report_problem(u)
  expect_null(rp, info = sprintf("%s: requires_report: %s", u$id, rp %||% ""))      # o relatorio da fundacao LIN_MED x SECO vem ANTES de qualquer RGD
  if (!rdp_need_golden(u)) return(invisible(NULL))
  run <- rdp_run(u)
  gold <- rdp_gold(u)
  r <- rdp_compare(run, gold, u)
  expect_identical(r$layout, "dessem_semihourly_48")
  expect_identical(r$day0_source, "native")
  expect_true(isTRUE(r$shape_ok) && isTRUE(r$na_ok), info = u$id)                       # values 9x48: mesma forma e mesmo padrao de NA
  expect_identical(dim(run$env$values), c(9L, 48L))
  expect_identical(dim(gold$native$values), c(10L, 48L))                                # O..O+9 do oraculo
  expect_identical(as.integer(r$delivery_missing), 0L)                                  # toda celula da entrega tem par no long do oraculo
  expect_true(isTRUE(r$cut_ok), info = u$id)
  expect_identical(as.integer(r$fora_da_faixa), 48L)                                    # O+9 do oraculo (1 dia x 48)
  expect_identical(as.integer(r$fora_da_faixa_long), 48L)
  expect_identical(run$env$available_until, u$available_until)
  expect_lte(r$max_abs_delta, RDP_TOL)                                                  # limiar unico, fora da cauda nomeada
  expect_true(isTRUE(r$ok), info = rdp_line(r))
  ## delivery$interval_end = timestamp do oraculo + 30 min em toda celula: min(entrega) = available_until + 30 min
  expect_identical(min(run$env$delivery$interval_end), u$available_until + lubridate::minutes(30))
  ## escala MW do deck REAL: finita, positiva, mediana ~ carga recente da area
  expect_true(all(is.finite(run$env$values)) && all(run$env$values > 0), info = u$id)
  ref_med <- stats::median(run$infer_panel$load[interval_start >= max(interval_start) - lubridate::days(7L), load_mwh])
  r_med <- stats::median(run$env$delivery$y_pred) / ref_med
  expect_gt(r_med, 0.5); expect_lt(r_med, 2)
  ## CONTROL da causa: a media de hora parcial do oraculo emulada fecha TUDO (inclusive a cauda nomeada) em <= 1e-5
  emu <- rdp_emulated(run, gold, u)
  if (length(r$delivery_over) || nrow(r$cells_over))
    cat(sprintf("OVER %s values=%s delivery=%s\n", u$id, paste(apply(r$cells_over, 1L, paste, collapse = ":"), collapse = " "),
                paste(r$delivery_over, collapse = " ")), file = stderr())
  expect_gt(emu$added, 0L)
  expect_lte(emu$r$max_abs_delta, RDP_TOL)
  expect_true(isTRUE(emu$r$ok), info = rdp_line(r))
  ## informativos por d_plus (nao vermelhos): composicao fit(mm = NULL)+predict(mm) e o corte 00:00 do oraculo
  e <- rdp_report_entry(u, r, lock, emu$r, rdp_arm_delta(run, u, "composition"), rdp_arm_delta(run, u, "cut0000"), group)
  .rdp$entries[[u$id]] <- e
  invisible(r)
}

rdp_group_test <- function(g) {
  us <- rdp_units_of(g)
  expect_gt(length(us), 0L)
  lock <- jsonlite::read_json(RDP_LOCK(), simplifyVector = FALSE)
  for (u in us) rdp_verify_unit(u, g, lock)
  got <- .rdp$entries[vapply(us, `[[`, "", "id")]
  got <- got[!vapply(got, is.null, NA)]
  if (length(got)) {
    rpt <- rdp_report_write(.rdp$entries, groups = unique(vapply(.rdp$entries, function(e) e$group, "")))
    expect_true(file.exists(RDP_REPORT()))
    message(sprintf("parity_report %s all_ok=%s sha256=%s unidades=%d max_abs=%.3g", g, rpt$all_ok, rpt$sha256, length(rpt$units),
                    max(vapply(rpt$units, function(e) e$max_abs_delta, 0))))
  }
}

## --------------------------------------------------------------------------------------------------------------
test_that("[PLUGINS] units_yaml: esquema P.0f, grupos smoke/foundation/fanout, oracle_cut start_label, oracle_env EXOGENA, regras de topo, requires_report", {
  spec <- rdp_spec()
  us <- spec$units
  ids <- vapply(us, `[[`, "", "id")
  expect_false(anyDuplicated(ids) > 0L)
  expect_true(all(unlist(spec$groups) %in% ids))
  by <- function(v) vapply(us[match(spec$groups[[v]], ids)], function(u) paste(u$variant, u$area), "")
  ## foundation: RGD_MED x SECO, outubro/2024 em walk-forward (origens crescentes), cortes 09:00 e 12:00, com 2024-10-07 (feriado em d_plus = 5)
  fu <- us[match(spec$groups$foundation, ids)]
  expect_true(all(vapply(fu, function(u) identical(u$variant, "MED") && identical(u$area, "SECO"), NA)))
  expect_true(all(vapply(fu, function(u) format(u$origin, "%Y-%m") == "2024-10", NA)))
  expect_false(is.unsorted(as.numeric(vapply(fu, function(u) as.numeric(u$origin), 0))))
  expect_setequal(vapply(fu, function(u) format(u$available_until, "%H:%M"), ""), c("09:00", "12:00"))
  expect_true("2024-10-07" %in% vapply(fu, function(u) format(u$origin), ""))
  ## fanout: MED e MAX x SECO, S, NE, N; smoke: <= 3 unidades por variante, so SECO
  expect_setequal(unique(vapply(us[match(spec$groups$fanout, ids)], function(u) paste(u$variant, u$area), "")),
                  c(outer(c("MED", "MAX"), c("SECO", "S", "NE", "N"), paste)))
  sm <- us[match(spec$groups$smoke, ids)]
  expect_true(all(table(vapply(sm, `[[`, "", "variant")) <= 3L))
  expect_true(all(vapply(sm, function(u) identical(u$area, "SECO"), NA)))
  tl <- rdp_tools()
  for (u in us) {
    expect_identical(u$oracle_cut, "start_label"); expect_identical(u$day0_source, "native")
    expect_identical(u$weather_issue_cutoff, "origin_midnight_utc")
    expect_identical(u$history_days, 1825L)                                        # history_start = O - 1825 (train == infer)
    expect_true(format(u$available_until, "%H:%M") %in% c("09:00", "12:00"))        # hora cheia, nunca 00:00
    expect_identical(tl$.pc_golden_cut_arg(u), paste0(format(u$origin), "T", format(u$available_until - lubridate::minutes(30L), "%H:%M")))
    expect_identical(u$oracle_env$EXOGENA, u$variant)                              # a variante so entra pelo env do FILHO do oraculo
    expect_null(u$cuts)                                                            # janela unica: history_days
    expect_identical(u$requires_report$path, "models/dessem-linear/tests/parity/foundation_report.json")
    expect_match(u$requires_report$sha256, "^[0-9a-f]{64}$")
  }
  dirs <- vapply(us, function(u) tl$.pc_golden_unit_dir("g", RDP_MODEL, u), "")
  expect_false(anyDuplicated(dirs) > 0L)                                           # cada unidade tem o proprio diretorio (variante x area x origem)
  expect_identical(Sys.getenv("EXOGENA"), "")                                      # o env do oracle_env existe so no filho: o pai nunca o ve
  ## unidade fora do contrato e recusada nomeando o campo: corte 09:30, history 730, requires_report sem sha, cuts literais
  base <- yaml::read_yaml(RDP_UNITS())
  for (mut in list(function(x) { x$available_until <- "2024-10-15T09:30"; x }, function(x) { x$history_days <- 730L; x },
                   function(x) { x$requires_report <- list(path = "a.json"); x }, function(x) { x$cuts <- list(train = list(train_origin = "2024-10-14")); x })) {
    bad <- base; bad$units <- bad$units[1L]; bad$groups <- list(smoke = bad$units[[1L]]$id)
    bad$units[[1L]] <- mut(bad$units[[1L]])
    f <- withr::local_tempfile(fileext = ".yaml"); yaml::write_yaml(bad, f)
    e <- rd5_err(rd_with_cli(tl$pc_golden_units(f)))
    expect_identical(e$id, "tools.bad_units")
  }
})

## requires_report recusa a unidade ANTES de invocar o oraculo (exit 2): relatorio ausente, vermelho ou com sha divergente
test_that("[PLUGINS] units_yaml: requires_report ausente, vermelho ou com sha divergente recusa a unidade com exit 2 antes de chamar o oraculo", {
  skip_if(.Platform$OS.type == "windows", "[tier] gerador do golden e so Linux")
  tl <- rdp_tools()
  u <- rdp_unit(RDP_BASE)
  repo <- withr::local_tempdir()
  dir.create(file.path(repo, "r"))
  put <- function(ok = TRUE, sha = strrep("a", 64L), unit_ok = ok) {
    rp <- list(all_ok = ok, units = list(list(unit = "x", ok = unit_ok)), sha256 = sha)
    writeLines(jsonlite::toJSON(rp, auto_unbox = TRUE), file.path(repo, "r", "rep.json"))
  }
  mk <- function(sha) { v <- u; v$requires_report <- list(path = "r/rep.json", sha256 = sha); v }
  put()
  expect_true(rd_with_cli(tl$pc_golden_require_report(mk(strrep("a", 64L)), repo)))                      # verde e com o sha declarado: segue
  e0 <- rd5_err(rd_with_cli(tl$pc_golden_require_report(mk(strrep("b", 64L)), repo)))                    # sha divergente
  expect_identical(e0$id, "tools.bad_units"); expect_identical(e0$exit_code, 2L); expect_match(e0$details$why, "sha256", fixed = TRUE)
  put(ok = FALSE)
  e1 <- rd5_err(rd_with_cli(tl$pc_golden_require_report(mk(strrep("a", 64L)), repo)))                    # vermelho
  expect_identical(e1$exit_code, 2L); expect_match(e1$details$why, "vermelho", fixed = TRUE)
  unlink(file.path(repo, "r", "rep.json"))
  e2 <- rd5_err(rd_with_cli(tl$pc_golden_require_report(mk(strrep("a", 64L)), repo)))                    # ausente
  expect_identical(e2$exit_code, 2L); expect_match(e2$details$why, "ausente", fixed = TRUE)
  expect_true(rd_ascii(c(conditionMessage(e0), e0$details$why, e1$details$why, e2$details$why)))
  ## o gerador pela linha de comando: unidade com relatorio ausente => exit 2 sem criar nenhum diretorio de golden (oraculo de mentira,
  ## repositorio git limpo: o driver nunca e chamado)
  fake <- withr::local_tempdir()
  git <- function(...) system2("git", c("-C", shQuote(fake), ...), stdout = FALSE, stderr = FALSE)
  git("init", "-q"); writeLines("x", file.path(fake, "f.txt")); git("add", "f.txt")
  git("-c", "user.name=t", "-c", "user.email=t@t", "commit", "-q", "-m", "x")
  yml <- yaml::read_yaml(RDP_UNITS()); yml$units <- yml$units[1L]; yml$groups <- NULL
  yml$units[[1L]]$requires_report <- list(path = "r/ausente.json", sha256 = strrep("a", 64L))
  yf <- file.path(withr::local_tempdir(), "units.yaml"); yaml::write_yaml(yml, yf)
  out_dir <- withr::local_tempdir()
  res <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(file.path(rd_root(), "scripts", "tools", "plugin_golden.R")), "--model", RDP_MODEL, "--units", shQuote(yf),
      "--oracle", shQuote(fake), "--out", shQuote(out_dir)), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(res, "status"), 2L)
  expect_match(paste(res, collapse = "\n"), "requires_report", fixed = TRUE)
  expect_false(dir.exists(file.path(out_dir, "plugins")))
})

test_that("[PLUGINS] requires_report: o relatorio da fundacao LIN_MED x SECO do dessem-linear (DL.9) existe, esta verde e tem o sha256 declarado em toda unidade", {
  rdp_gate_tier("parity")
  for (u in rdp_spec()$units) {
    why <- rdp_report_problem(u)
    expect_null(why, info = sprintf("%s: %s", u$id, why %||% ""))
  }
})

test_that("[PLUGINS] paridade_smoke: values O..O+8 e delivery vs oraculo, max|d| <= 1e-5 MW; O+9 do oraculo fora_da_faixa; CONTROL da media de hora parcial", {
  rdp_gate_tier("parity")
  rdp_group_test("smoke")
})

test_that("[PLUGINS] paridade_foundation: RGD_MED x SECO, outubro/2024 em walk-forward (cortes 09:00 e 12:00, feriado em d_plus = 5), max|d| <= 1e-5 MW", {
  rdp_gate_tier("parity-full")
  rdp_group_test("foundation")
})

test_that("[PLUGINS] paridade_fanout: MED e MAX x SECO, S, NE, N, max|d| <= 1e-5 MW", {
  rdp_gate_tier("parity-full")
  rdp_group_test("fanout")
})

test_that("[PLUGINS] paridade_control: exogena MED->MAX e history_days.train -7 acusam > 1e-5; golden/lock adulterados falham (regere o golden); oraculo intacto", {
  rdp_gate_tier("parity")
  u <- rdp_unit(RDP_BASE)
  if (!rdp_need_golden(u)) return(invisible(NULL))                                       # sem golden valido o controle nao prova nada
  gold <- rdp_gold(u)
  base <- rdp_compare(rdp_run(u), gold, u)
  expect_true(isTRUE(base$ok), info = rdp_line(base))                         # sem a base verde o CONTROL nao prova nada
  ## CONTROL com dente: exogena MED -> MAX (outra variante/model_key) e history_days.train -7 (janela menor) > 1e-5, mesma forma
  mx <- rdp_compare(rdp_run(u, params = list(exogena = setdiff(c("MED", "MAX"), u$variant))), gold, u)
  expect_gt(mx$max_abs_delta, RDP_TOL); expect_false(isTRUE(mx$ok)); expect_true(isTRUE(mx$shape_ok))
  h7 <- rdp_compare(rdp_run(u, hist_delta = 7L), gold, u)
  expect_gt(h7$max_abs_delta, RDP_TOL); expect_false(isTRUE(h7$ok)); expect_true(isTRUE(h7$shape_ok))
  message(sprintf("controls exogena_MAX max|D|=%.3g history_days_train_-7 max|D|=%.3g", mx$max_abs_delta, h7$max_abs_delta))
  ## golden ausente (diretorio vazio no lugar do golden) e golden.lock ausente: mensagem de regerar, nunca skip
  m <- rdp_check_golden(u, root = withr::local_tempdir())
  expect_match(m, RDP_REGEN, fixed = TRUE); expect_match(m, u$id, fixed = TRUE)
  expect_match(rdp_check_golden(u, lock = file.path(withr::local_tempdir(), "nao-existe.lock")), RDP_REGEN, fixed = TRUE)
  ## fingerprint do runtime (pacote), digest de dados e sha de arquivo adulterados => regere
  lk <- jsonlite::read_json(RDP_LOCK(), simplifyVector = FALSE)
  tamper <- function(f) {
    p <- tempfile(fileext = ".lock")
    on.exit(unlink(p), add = TRUE)
    writeLines(jsonlite::toJSON(f(lk), auto_unbox = TRUE, null = "null", digits = NA), p)
    rdp_check_golden(u, lock = p)
  }
  m1 <- tamper(function(l) { l$units[[u$id]]$fingerprint$packages$fable <- "0.0.0"; l })
  expect_match(m1, RDP_REGEN, fixed = TRUE); expect_match(m1, "fable", fixed = TRUE)
  m2 <- tamper(function(l) { l$units[[u$id]]$data_digest$aggregate <- strrep("0", 64L); l })
  expect_match(m2, RDP_REGEN, fixed = TRUE)
  m3 <- tamper(function(l) { l$units[[u$id]]$files[["native.rds"]] <- strrep("0", 64L); l })
  expect_match(m3, RDP_REGEN, fixed = TRUE)
  ## o golden grava o argumento passado ao oraculo: corte start_label = available_until - 30 min
  inv <- jsonlite::read_json(file.path(rdp_tools()$.pc_golden_unit_dir(RDP_GOLDEN(), RDP_MODEL, u), "invocation.json"), simplifyVector = TRUE)
  expect_identical(inv$args[which(inv$args == "--available-until") + 1L], paste0(format(u$origin), "T08:30"))
  expect_identical(inv$oracle_commit, lk$units[[u$id]]$oracle_commit)
  ## CONTROL: --out dentro da arvore do oraculo => exit 2 (tools.out_inside_oracle); oraculo intacto (so os 3 docs/roadmap + verify-log)
  if (dir.exists(file.path(RDP_ORACLE, ".git"))) {
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(file.path(rd_root(), "scripts", "tools", "plugin_golden.R")), "--model", RDP_MODEL, "--units", "smoke",
        "--oracle", RDP_ORACLE, "--out", file.path(RDP_ORACLE, "golden-fora"), "--data", rdp_store()), stdout = TRUE, stderr = TRUE))
    expect_identical(attr(out, "status"), 2L)
    expect_false(dir.exists(file.path(RDP_ORACLE, "golden-fora")))
    st <- suppressWarnings(system2("git", c("--no-optional-locks", "-C", shQuote(RDP_ORACLE), "status", "--porcelain"), stdout = TRUE, stderr = FALSE))
    expect_length(st[!grepl("docs/roadmap/", st, fixed = TRUE)], 0L)
  }
})
