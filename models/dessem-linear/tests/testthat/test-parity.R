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
## DIVERGENCIA DELIBERADA (Gabriel 2026-10-30): hora incompleta e DESCARTADA tambem na previsao de clima (to_hourly da fundacao); o
## oraculo tira a media dos slots existentes. Uma emissao 00Z termina em O+9 23:00 (hora 23 com UM slot): o dia O+9 fica curto e e
## persistido, o que alcanca as ULTIMAS horas anteriores (spline monoH.FC de desagregaModeloSH e normalizacao do treino): os
## intervalos nomeados saem de `dlp_partial_from` (3 h antes do dia cuja ultima hora do clima previsto tem um slot so). A comparacao
## e EXATA (<= 1e-5) em todo o resto de O..O+8; a divergencia nomeada e MEDIDA e listada; o CONTROL emula a media de hora parcial do
## oraculo numa copia do painel de infer (acrescenta o slot :30 que falta, mesmo valor) e TUDO tambem fecha em <= 1e-5: a causa e so
## essa regra.
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
DLP_WMAPE_TAIL <- 0.01
## EXCECAO NUMERICA DOCUMENTADA (Gabriel 2026-10-30, DL9-GAP-2): sp-2024-11-15 (O e feriado) tem residuo de 0.446 MW (3e-5 rel) por
## 1 ulp em `mm` (oraculo: media de 18 slots; clean-room: media de 9 medias horarias) amplificado pelo SVM da DM(O) (nSV 1683 x 1685).
## So essa unidade tem teto (max_abs, max_rel sobre o maior valor do golden); todas as outras ficam EXATAS (<= 1e-5).
DLP_NUMERIC_EXCEPTION <- list("sp-2024-11-15" = list(max_abs = 0.5, max_rel = 5e-5))
dlp_limit <- function(u) { ex <- DLP_NUMERIC_EXCEPTION[[u$id]]; if (is.null(ex)) DLP_TOL else ex$max_abs }

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

## grupos do tier: parity => smoke; parity-full => smoke + foundation; `--units <grupo>` vence. O grupo `fanout` (DL.10) e de test-fanout.R:
## aqui `--units fanout` nao roda nenhuma unidade (o teste `paridade` fica vazio e so confere isso).
dlp_groups <- function() {
  g <- if (exists("pc_gate_units", mode = "function")) pc_gate_units(NULL) else NULL
  if (!is.null(g)) return(setdiff(g, "fanout"))
  have <- names(dlp_spec()$groups)
  if (identical(dlp_tier(), "parity-full")) intersect(c("smoke", "foundation"), have) else "smoke"
}

dlp_units <- function(groups) {
  us <- unlist(lapply(groups, function(g) dlp_group_units(g)), recursive = FALSE)
  us <- us[!duplicated(vapply(us, `[[`, "", "id"))]
  only <- getOption("prevcarga.parity_only")          # opcao de DESENVOLVIMENTO (rodar poucas unidades); nunca usada pelo runner
  if (length(only)) us <- us[vapply(us, `[[`, "", "id") %in% only]
  us
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
  ## Previsto: TODAS as emissoes ate o limite de O (`cuts$weather_issue_limit`, origin_midnight_utc) nas DUAS fases: o split e
  ## origem-escopado (train_origin = O) e o oraculo treina com as emissoes ate O 00Z (o clima previsto entra na normalizacao do
  ## treino); ler o painel de treino em O-1 daria a emissao de O-1 e normalizacao diferente (medido: SP 2024-11-15).
  read <- function(origin, hist) {
    p <- suppressWarnings(dl_lib$prevcarga_read_panel(ctx, origin, u$area, sp$horizon_days, hist))
    fc <- dl_lib$prevcarga_read_weather(ctx, "forecast", u$area, origin - hist, u$origin + sp$horizon_days + 2L)
    p$weather_forecast <- fc[issue_time <= cuts$weather_issue_limit]       # painel de 30 min CRU
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
      ip <- s$read(u$origin, s$hist$infer + 1L)      # janela do oraculo: history_start + 1 dia de aquecimento (data_adapter.R)
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

## DIVERGENCIA DELIBERADA, intervalos NOMEADOS: a regra (hora incompleta descartada tambem no clima previsto) afeta o dia cuja
## ultima hora do clima previsto mais fresco tem UM slot (valid_time :00 sem o :30 seguinte; a 00Z termina em O+9 23:00; em AC
## 2025-06-30 a emissao 06-29 termina em O+8 23:00) e, pelo spline monoH.FC e pela normalizacao do treino, as horas anteriores
## (DLP_REACH_HOURS = 3 h, medido). Devolve o interval_end a partir do qual (exclusivo) a unidade esta na divergencia nomeada.
DLP_REACH_HOURS <- 3L
dlp_partial_from <- function(run, u) {
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
  d0 - lubridate::hours(DLP_REACH_HOURS)
}

## interval_end de cada celula de `values` (linha r = dia O+r-1, coluna c termina em 00:00 + 30 min c)
dlp_cell_ends <- function(u, nr) {
  t0 <- lubridate::force_tz(lubridate::as_datetime(u$origin), lubridate::tz(u$available_until))
  matrix(as.numeric(t0 + lubridate::days(rep(seq_len(nr) - 1L, 48L)) + lubridate::minutes(30L * rep(1:48, each = nr))), nr, 48L)
}

## Comparacao da fundacao + decomposicao: delta EXATO fora dos intervalos nomeados (values e delivery) e delta MEDIDO dentro deles.
## `ok` = forma, NA, corte e cobertura da entrega ok e max|D| fora dos intervalos nomeados <= 1e-5 (limiar unico).
dlp_compare <- function(run, gold, u) {
  r <- dlp_tools()$prevcarga_parity_compare(run$env, gold, u, tol = DLP_TOL)
  gm <- gold$native$values[seq_len(nrow(run$env$values)), , drop = FALSE]; em <- run$env$values
  d <- if (identical(dim(gm), dim(em))) abs(gm - em) else matrix(Inf, 1L, 1L)
  from <- dlp_partial_from(run, u)
  tail <- if (is.null(from) || any(is.infinite(d))) matrix(FALSE, nrow(d), ncol(d)) else dlp_cell_ends(u, nrow(d)) > as.numeric(from)
  dl <- run$env$delivery
  ref <- gold$long$y_pred[match(as.numeric(dl$interval_end), as.numeric(gold$long$timestamp + lubridate::minutes(30L)))]
  dd <- abs(dl$y_pred - ref)
  in_tail <- if (is.null(from)) rep(FALSE, nrow(dl)) else as.numeric(dl$interval_end) > as.numeric(from)
  mx <- function(x) if (any(!is.na(x))) max(x, na.rm = TRUE) else 0
  r$max_abs_delta_full <- r$max_abs_delta
  r$max_abs_delta <- max(mx(d[!tail]), mx(dd[!in_tail]))
  r$cells_over <- which(d > DLP_TOL & !tail, arr.ind = TRUE)
  r$delivery_over <- format(dl$interval_end[!in_tail & !is.na(dd) & dd > DLP_TOL], "%m-%d %H:%M")
  r$tail_max_abs_delta <- max(mx(d[tail]), mx(dd[in_tail]))
  r$tail_from <- if (is.null(from)) NA_character_ else format(from, "%Y-%m-%d %H:%M")
  r$tail_intervals <- if (is.null(from)) character() else format(range(dl$interval_end[in_tail]), "%Y-%m-%d %H:%M")
  ex <- DLP_NUMERIC_EXCEPTION[[u$id]]
  r$rel_delta <- r$max_abs_delta / max(abs(gold$native$values))
  r$ok <- isTRUE(r$shape_ok) && isTRUE(r$na_ok) && isTRUE(r$cut_ok) && r$delivery_missing == 0L &&
    r$max_abs_delta <= dlp_limit(u) && (is.null(ex) || r$rel_delta <= ex$max_rel)
  r
}

## max|D| do values (O..O+8) do infer reusando o artefato quando a carga do painel e multiplicada por (1 + eps): 1 ulp relativo
dlp_ulp_shift <- function(run, gold, u) {
  s <- run$setup
  ip <- run$infer_panel
  ip$load <- data.table::copy(ip$load)[, load_mwh := load_mwh * (1 + .Machine$double.eps)]
  cuts <- c(s$cuts, list(spec = s$sp, phase = "infer"))
  env <- NULL
  dlp_quiet(env <- dl_lib$prevcarga_plugin_infer(s$plugin, run$art, ip, cuts, list()))
  max(abs(env$values - run$env$values))
}

## CONTROL da causa: infer com a media de hora parcial do oraculo emulada numa COPIA do painel de infer (acrescenta, em cada
## emissao, o slot :30 que falta na ultima hora, com o valor do slot :00 = a media dos slots existentes). Reusa o artefato do split.
## Compara TUDO (inclusive a cauda) com o limiar unico.
dlp_emulated <- function(run, gold, u) {
  key <- paste0("emu-", u$id)
  if (!is.null(.dlp$runs[[key]])) return(.dlp$runs[[key]])
  s <- run$setup
  ip <- run$infer_panel
  fc <- data.table::copy(ip$weather_forecast)
  last <- fc[, .SD[which.max(valid_time)], by = issue_time]
  last <- last[lubridate::minute(valid_time) == 0L]
  add <- data.table::copy(last)[, valid_time := valid_time + lubridate::minutes(30L)]
  ip$weather_forecast <- data.table::rbindlist(list(fc, add), use.names = TRUE, fill = TRUE)
  cuts <- c(s$cuts, list(spec = s$sp, phase = "infer"))
  env <- NULL
  dlp_quiet(env <- dl_lib$prevcarga_plugin_infer(s$plugin, run$art, ip, cuts, list()))
  r <- dlp_tools()$prevcarga_parity_compare(env, gold, u, tol = DLP_TOL)
  out <- list(r = r, added = nrow(add))
  .dlp$runs[[key]] <- out
  out
}

dlp_report_line <- function(r) sprintf("%s max|D|=%.3e (fora da divergencia nomeada; cheio %.3e, nomeada %.3e desde %s) celulas>tol=%d", r$unit,
                                       r$max_abs_delta, r$max_abs_delta_full, r$tail_max_abs_delta, r$tail_from, nrow(r$cells_over))

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
dlp_report_entry <- function(u, r, lock, split = NULL, emu = NULL) {
  e <- list(unit = u$id, area = u$area, origin = format(u$origin), layout = r$layout, ok = isTRUE(r$ok),
            max_abs_delta = signif(r$max_abs_delta, 12), n_compared = as.integer(r$n_compared),
            fora_da_faixa = as.integer(r$fora_da_faixa), fora_da_faixa_long = as.integer(r$fora_da_faixa_long),
            shape_ok = isTRUE(r$shape_ok), na_ok = isTRUE(r$na_ok), delivery_ok = isTRUE(r$delivery_ok), cut_ok = isTRUE(r$cut_ok),
            day0_source = r$day0_source, golden_native_sha256 = lock$units[[u$id]]$files[["native.rds"]])
  e$max_abs_delta_full <- signif(r$max_abs_delta_full, 12); e$max_rel_delta <- signif(r$rel_delta, 12)
  if (!is.null(DLP_NUMERIC_EXCEPTION[[u$id]])) e$numeric_exception <- DLP_NUMERIC_EXCEPTION[[u$id]]
  e$deliberate_divergence_from <- r$tail_from; e$tail_max_abs_delta <- signif(r$tail_max_abs_delta, 12); e$tail_intervals <- as.list(r$tail_intervals)
  if (!is.null(emu)) { e$emulated_partial_hour_max_abs_delta <- signif(emu$max_abs_delta, 12); e$emulated_ok <- isTRUE(emu$ok) }
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
              all_ok = all(vapply(units, function(e) isTRUE(e$ok) && isTRUE(e$split_ok %||% TRUE) && isTRUE(e$emulated_ok %||% TRUE), NA)), units = units)
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
  ## fan-out (DL.10): MAX/MXMN x SECO nas origens da fundacao + MED/MAX/MXMN x RS, CE, PA (2024-10-15); requires_report em cada uma
  fo <- spec$groups$fanout
  expect_length(fo, 19L)
  expect_setequal(fo, c(sprintf("%s-seco-2024-10-%02d", rep(c("max", "mxmn"), each = 5L), c(1L, 8L, 15L, 22L, 29L)),
                        sprintf("%s-%s-2024-10-15", rep(c("med", "max", "mxmn"), 3L), rep(c("rs", "ce", "pa"), each = 3L))))
  expect_length(intersect(fo, c(spec$groups$smoke, spec$groups$foundation)), 0L)
  for (u in us) {
    O <- u$origin
    if (u$id %in% fo) {
      expect_true(u$variant %in% c("MED", "MAX", "MXMN"))
      expect_identical(u$requires_report$path, "models/dessem-linear/tests/parity/foundation_report.json")
      expect_match(u$requires_report$sha256, "^[0-9a-f]{64}$")
      expect_identical(u$id, sprintf("%s-%s-%s", tolower(u$variant), tolower(u$area), format(O)))
    } else {
      expect_identical(u$variant, "MED"); expect_null(u$requires_report)                # fundacao e smoke: so MED
    }
    expect_identical(u$oracle_cut, "start_label"); expect_identical(u$day0_source, "native")
    expect_identical(u$weather_issue_cutoff, "origin_midnight_utc")
    expect_identical(u$history_days, 1825L)                                        # history_start = O - 1825 (train == infer)
    expect_identical(format(u$available_until, "%H:%M"), "09:00")                   # hora cheia
    expect_identical(dlp_tools()$.pc_golden_cut_arg(u), paste0(format(O), "T08:30"))   # o oraculo recebe available_until - 30 min
    expect_identical(u$oracle_env$EXOGENA, u$variant)                              # a variante entra so pelo oracle_env do filho do oraculo
    expect_null(u$cuts)                                                            # janela unica: history_days
  }
  ## cada unidade tem o proprio diretorio de golden (area x origem x variante)
  dirs <- vapply(us, function(u) dlp_tools()$.pc_golden_unit_dir("g", DLP_MODEL, u), "")
  expect_false(anyDuplicated(dirs) > 0L)
  ## o env do oracle_env existe so no filho do oraculo: o pai nunca o ve
  expect_identical(Sys.getenv("EXOGENA"), "")
  ## unidade fora do contrato e recusada nomeando o campo (regras de topo): corte 09:30, history 730, cuts
  for (mut in list(function(x) { x$available_until <- "2024-10-01T09:30"; x },
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
  if (!length(groups)) { expect_identical(pc_gate_units(NULL), "fanout"); return(invisible(NULL)) }     # --units fanout: test-fanout.R
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
    expect_identical(as.integer(r$delivery_missing), 0L)                                  # toda celula da entrega tem par no long do oraculo
    expect_true(isTRUE(r$cut_ok), info = u$id)
    expect_identical(as.integer(r$fora_da_faixa), 48L)                                    # O+9 do oraculo (1 dia x 48)
    expect_identical(as.integer(r$fora_da_faixa_long), 48L)
    expect_identical(run$env$available_until, u$available_until)
    expect_lte(r$max_abs_delta, dlp_limit(u))                                             # Delta = 0 (1e-5) fora da cauda nomeada; teto so na excecao numerica
    expect_true(isTRUE(r$ok), info = dlp_report_line(r))
    ## delivery$interval_end = timestamp do oraculo + 30 min em toda celula: min(entrega) = available_until + 30 min
    expect_identical(min(run$env$delivery$interval_end), u$available_until + lubridate::minutes(30))
    ## gate02 no deck REAL: finita, positiva, mediana ~ carga recente da area (e 5000-120000 MW em SECO)
    expect_true(all(is.finite(run$env$values)) && all(run$env$values > 0), info = u$id)
    ref_med <- stats::median(run$infer_panel$load[interval_start >= max(interval_start) - lubridate::days(7L), load_mwh])
    r_med <- stats::median(run$env$delivery$y_pred) / ref_med
    expect_gt(r_med, 0.5); expect_lt(r_med, 2)                                           # areas pequenas (GO, AC): escala relativa a carga recente
    if (identical(u$area, "SECO")) { expect_gt(stats::median(run$env$delivery$y_pred), 5000); expect_lt(stats::median(run$env$delivery$y_pred), 120000) }
    ## regua split x acoplado nas origens da fundacao (area SECO): dia D == separado (<= 1e-5), cauda WMAPE <= 1% [TBD: ADR-0031]
    sp <- NULL
    if (identical(u$area, "SECO")) {
      sp <- dlp_split_ruler(run, dlp_run(u, "coupled"))
      expect_true(sp$same_shape, info = u$id)
      expect_lte(sp$day0_delta, DLP_TOL)
      expect_lte(sp$tail_wmape, DLP_WMAPE_TAIL)
    }
    emu <- dlp_emulated(run, gold, u)
    if (length(r$delivery_over) || nrow(r$cells_over))
      cat(sprintf("OVER %s values=%s delivery=%s\n", u$id, paste(apply(r$cells_over, 1L, paste, collapse = ":"), collapse = " "),
                  paste(r$delivery_over, collapse = " ")), file = stderr())
    cat(sprintf("EMU %s max=%.3g\n", u$id, emu$r$max_abs_delta), file = stderr())
    expect_gt(emu$added, 0L)                                                              # o painel de teste teve hora parcial acrescentada
    expect_lte(emu$r$max_abs_delta, dlp_limit(u))                                              # CONTROL: com a regra do oraculo a cauda tambem fecha
    expect_true(isTRUE(emu$r$ok) || !is.null(DLP_NUMERIC_EXCEPTION[[u$id]]), info = dlp_report_line(emu$r))   # excecao numerica: so o teto acima
    if (!is.null(DLP_NUMERIC_EXCEPTION[[u$id]])) {
      ## CONTROL: o residuo vem de `mm`/ruido de 1 ulp: perturbar a carga do painel de infer em 1 ulp (x (1 + eps)) move a previsao
      ## desta unidade alem de 1e-5 (e ainda dentro do teto); a perturbacao e relativa, nao muda o desenho
      pert <- dlp_ulp_shift(run, gold, u)
      expect_gt(pert, DLP_TOL)                                                   # 1 ulp de carga move a previsao alem de 1e-5 (medido 1.8 MW)
      cat(sprintf("ULP %s shift=%.3g rel=%.3g\n", u$id, pert, r$rel_delta), file = stderr())
    }
    entries[[u$id]] <- dlp_report_entry(u, r, lock, sp, emu$r)
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
