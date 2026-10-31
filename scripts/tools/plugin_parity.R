#!/usr/bin/env Rscript
## plugin_parity.R -- comparador de paridade clean-room x oraculo por LAYOUT de saida (P.0g). Ferramenta de
## DESENVOLVIMENTO (so Linux, fora do bundle). Constroi sobre plugin_golden.R (P.0f): le as unidades por
## pc_golden_units, confere o golden.lock (mesmas funcoes do P.0f) e compara o envelope do clean-room com o golden.
##
##   Rscript --vanilla scripts/tools/plugin_parity.R --model <m> --units <units.yaml> --golden <dir> --data <store>
##       [--root <models>] [--topology <yaml>] [--out <report.json>] [--rscript <Rscript>] [--format text|json] [--lock <golden.lock>]
##
## Dispatch SEMPRE por `layout` (nunca por nome de modelo). Limiar fixo 1e-5 MW absoluto (trilho mesmo-hardware,
## ADR-0017); nenhuma outra tolerancia, nenhuma excecao de A/B (tests/ab/ab_excecoes.yaml e outro trilho) e nenhuma metrica de acuracia.
## Golden por unidade (<golden>/plugins/<model>/<variant|_>/<area>/<origin>/): native.rds = list(layout, values,
## meta = list(discarded_step?)), long.parquet (timestamp = INICIO do intervalo, y_pred), invocation.json.
## Saida: 0 todas as unidades ok; 1 golden ausente/velho ou paridade reprovada; 2 [contract] args/layout.

.PC_PARITY_TOL <- 1e-5

## Layouts comparaveis. Os nomes sao os de registry.R prevcarga_native_layouts; o ticket cita `dessem_matrix_nx48`, que
## o codigo nao tem (o DESSEM declara `dessem_semihourly_48`): vale o nome do codigo. Minusculas (grep PREVCARGA_[A-Z_]+).
prevcarga_parity_layouts <- list(
  rf_wide_hourly_216   = list(rows = 2L,  cols = 216L, step_min = 60L, rows_are = "origem O-1 | O"),
  lgbm_day_48          = list(rows = 1:2, cols = 48L,  step_min = 30L, rows_are = "dia O completado | O+1"),
  hw_matrix_nx48       = list(rows = NA,  cols = 48L,  step_min = 30L, rows_are = "dias O..O+n-1"),
  dessem_semihourly_48 = list(rows = NA,  cols = 48L,  step_min = 30L, rows_are = "dias O..O+n-1")
)

.pc_parity_layout <- function(layout) {
  if (!is.character(layout) || length(layout) != 1L || is.na(layout) || !(layout %in% names(prevcarga_parity_layouts)))
    prevcarga_abort("tools.bad_layout", where = "plugin_parity.R",
                    details = list(layout = if (is.character(layout) && length(layout)) layout[[1L]] else "?",
                                   known = names(prevcarga_parity_layouts)))
  prevcarga_parity_layouts[[layout]]
}

## --- 1. forma nativa -> matriz + interval_end de cada celula --------------------------------------------------------
## Aceita matriz, data.frame, lista de vetores (uma linha cada) ou vetor (1 linha). A conferencia de LINHAS e da
## comparacao (shape_ok); aqui so as colunas devem ser as do layout.
.pc_parity_as_matrix <- function(values, layout) {
  lay <- .pc_parity_layout(layout)
  m <- tryCatch({
    if (is.data.frame(values)) as.matrix(values)
    else if (is.matrix(values)) values
    else if (is.list(values)) do.call(rbind, lapply(values, as.numeric))
    else matrix(as.numeric(values), nrow = 1L)
  }, error = function(e) NULL)
  if (is.null(m) || !is.numeric(m) || length(dim(m)) != 2L || ncol(m) != lay$cols || nrow(m) < 1L)
    prevcarga_abort("tools.bad_layout", where = layout, details = list(layout = layout, expected_cols = lay$cols,
                    got = if (is.null(m)) "nao numerico" else paste(dim(m), collapse = "x")))
  storage.mode(m) <- "double"
  dimnames(m) <- NULL
  m
}

## interval_end (segundos) por celula. Dia: linha i = dia O+i-1, coluna j termina em 00:00 + 30 min * j. RF horario:
## linha r e a origem T = O-(n-r); a origem nativa T preve do dia T+1 em diante (lead 1 = T+1 00:00-01:00; run_pipeline.R:296-306,
## day_of_horizon >= 1), logo a coluna k termina em 00:00 de T+1 + k horas (nunca em 00:00 de T: erro de 24 h do P.0g).
.pc_parity_cell_ends <- function(layout, dims, origin, tz) {
  lay <- .pc_parity_layout(layout)
  nr <- dims[[1L]]; nc <- dims[[2L]]
  out <- matrix(NA_real_, nr, nc)
  for (r in seq_len(nr)) {
    day <- if (lay$step_min == 60L) origin - (nr - r) + 1L else origin + (r - 1L)
    t0 <- lubridate::force_tz(lubridate::as_datetime(day), tz)
    ends <- if (lay$step_min == 60L) t0 + lubridate::hours(seq_len(nc)) else t0 + lubridate::minutes(30L * seq_len(nc))
    out[r, ] <- as.numeric(ends)
  }
  out
}

## --- 2. faixa e dia O ---------------------------------------------------------------------------------------------
.pc_parity_band_end <- function(cuts, delivery_days) as.numeric(max(prevcarga_delivery_grid(cuts, delivery_days)))

## Mantem as celulas com interval_end ate o fim da grade de entrega (prevcarga_delivery_grid) e conta as demais como
## `fora_da_faixa` (excesso do oraculo, ex. O+9 no RF/dessem). Linha entra se a sua 1a celula esta na faixa; coluna
## entra se a celula da ultima linha mantida esta na faixa. Retangular por construcao.
.pc_parity_band <- function(golden_cells, cuts, delivery_days) {
  be <- .pc_parity_band_end(cuts, delivery_days)
  m <- golden_cells$m; e <- golden_cells$ends
  rows <- which(e[, 1L] <= be)
  if (!length(rows)) return(list(m = m[0L, , drop = FALSE], ends = e[0L, , drop = FALSE], fora = length(m), band_end = be,
                                 rows = rows, cols = integer(), full_dim = dim(m)))
  cols <- which(e[max(rows), ] <= be)
  list(m = m[rows, cols, drop = FALSE], ends = e[rows, cols, drop = FALSE],
       fora = length(m) - length(rows) * length(cols), band_end = be, rows = rows, cols = cols, full_dim = dim(m))
}

## Dia O esperado, pela fonte declarada na unidade. Devolve list(source, m, fill) (fill = celula de entrega que o
## oraculo nao grava: interval_end -> valor).
.pc_parity_day0 <- function(unit, golden, layout, tz) {
  src <- unit$day0_source
  m <- .pc_parity_as_matrix(golden$native$values, layout)
  fill <- NULL
  if (identical(src, "native")) {
    # o proprio golden
  } else if (identical(src, "oracle_origin_minus_1")) {
    if (.pc_parity_layout(layout)$step_min != 60L)
      prevcarga_abort("tools.bad_units", where = paste0(unit$id, ".day0_source"),
                      details = list(unit = unit$id, field = "day0_source", why = "oracle_origin_minus_1 so vale p/ layout horario de 2 origens"))
    ## golden de uma rodada com as 2 origens (long_Om1 presente): a linha O-1 do native.rds ja e a do oraculo e o dia O da
    ## entrega vem do d_plus 1 de long_Om1 (montado em .pc_parity_read_dir). Sem long_Om1, a irma (origem O-1) e a fonte.
    if (is.null(golden$sister) && !isTRUE(golden$has_om1))
      prevcarga_abort("tools.golden_missing", where = unit$id, details = list(unit = unit$id, item = "golden da origem O-1"))
    if (!is.null(golden$sister)) {
      sm <- .pc_parity_as_matrix(golden$sister$values, layout)
      m[1L, ] <- sm[nrow(sm), ]
    }
  } else if (identical(src, "oracle_discarded_step")) {
    ds <- golden$native$meta$discarded_step
    if (!is.numeric(ds) || length(ds) != 1L || is.na(ds))
      prevcarga_abort("tools.golden_missing", where = unit$id, details = list(unit = unit$id, item = "native$meta$discarded_step"))
    t0 <- lubridate::force_tz(lubridate::as_datetime(unit$origin), tz)
    first_end <- unit$available_until + lubridate::minutes(30L)
    col <- as.integer(round(lubridate::time_length(lubridate::interval(t0, first_end), "minute") / 30))
    if (col >= 1L && col <= ncol(m)) m[1L, col] <- ds
    fill <- list(end = as.numeric(first_end), value = ds)
  } else {
    prevcarga_abort("tools.bad_units", where = paste0(unit$id, ".day0_source"),
                    details = list(unit = unit$id, field = "day0_source", why = "fonte desconhecida"))
  }
  list(source = src, m = m, fill = fill)
}

## --- 3. comparacao -----------------------------------------------------------------------------------------------
.pc_parity_cells <- function(a, b, tol = .PC_PARITY_TOL) {
  shape_ok <- identical(dim(a), dim(b))
  na_ok <- shape_ok && identical(is.na(a), is.na(b))
  d <- if (shape_ok) abs(a - b) else NA_real_
  has <- shape_ok && any(!is.na(d))
  m <- if (!shape_ok) Inf else if (has) max(d, na.rm = TRUE) else 0
  worst <- if (has) which(d == m, arr.ind = TRUE)[1L, ] else c(NA_integer_, NA_integer_)
  list(shape_ok = shape_ok, na_ok = na_ok, max_abs_delta = m, worst = as.integer(worst),
       n_compared = if (shape_ok) sum(!is.na(d)) else 0L, ok = shape_ok && na_ok && m <= tol)
}

## delivery (interval_end, y_pred) contra o long do oraculo (timestamp = INICIO): interval_end = timestamp + shift_min.
## So interval_end > available_until entra; toda celula da grade deve existir no envelope e no long (ou em `fill`).
.pc_parity_delivery <- function(delivery, long, available_until, grid, band_end, tol = .PC_PARITY_TOL,
                                shift_min = 30L, fill = NULL) {
  de <- as.numeric(delivery$interval_end)
  keep <- de > as.numeric(available_until)
  de <- de[keep]; dy <- as.numeric(delivery$y_pred)[keep]
  lend <- as.numeric(long$timestamp + lubridate::minutes(shift_min))
  ly <- as.numeric(long$y_pred)
  ref <- ly[match(de, lend)]
  if (!is.null(fill)) ref[is.na(ref) & de == fill$end] <- fill$value
  gnum <- as.numeric(grid)
  miss_grid <- sum(!(gnum %in% de))
  d <- abs(dy - ref)
  n_missing <- sum(is.na(ref)) + miss_grid
  mx <- if (any(!is.na(d))) max(d, na.rm = TRUE) else if (n_missing) Inf else 0
  list(n = sum(!is.na(d)), n_missing = n_missing, max_abs_delta = mx, fora_long = sum(lend > band_end),
       ok = n_missing == 0L && mx <= tol)
}

## Compara o envelope do clean-room (values nativos + delivery) com o golden da unidade. `golden` =
## list(native, long, sister?) (ver .pc_parity_read_golden). `delivery_days` = o do membro (default: o que o envelope entrega).
prevcarga_parity_compare <- function(envelope, golden, unit, tol = .PC_PARITY_TOL, delivery_days = NULL,
                                     join_shift_min = 30L) {
  layout <- envelope$layout
  .pc_parity_layout(layout)
  tz <- lubridate::tz(unit$available_until)
  if (is.null(delivery_days)) {
    t0 <- lubridate::force_tz(lubridate::as_datetime(unit$origin), tz)
    delivery_days <- as.integer(ceiling(lubridate::time_length(lubridate::interval(t0, max(envelope$delivery$interval_end)), "day")))
  }
  cuts <- list(available_until = unit$available_until, tz = tz, origin_day = unit$origin)
  grid <- prevcarga_delivery_grid(cuts, delivery_days)
  d0 <- .pc_parity_day0(unit, golden, layout, tz)
  gm <- .pc_parity_as_matrix(d0$m, layout)
  band <- .pc_parity_band(list(m = gm, ends = .pc_parity_cell_ends(layout, dim(gm), unit$origin, tz)), cuts, delivery_days)
  em <- .pc_parity_as_matrix(envelope$values, layout)
  ## RF: o envelope entrega a matriz nativa inteira (2 x 216, inclui a cauda alem da faixa); as duas matrizes sao recortadas
  ## pela MESMA faixa. Forma diferente da do golden cheio => comparacao direta com o golden recortado (reprova por forma).
  if (identical(dim(em), band$full_dim)) em <- em[band$rows, band$cols, drop = FALSE]
  cl <- .pc_parity_cells(em, band$m, tol)
  dv <- .pc_parity_delivery(envelope$delivery, golden$long, unit$available_until, grid, band$band_end, tol,
                            shift_min = join_shift_min, fill = d0$fill)
  cut_ok <- identical(as.numeric(envelope$available_until), as.numeric(unit$available_until))
  mx <- max(cl$max_abs_delta, dv$max_abs_delta)
  list(unit = unit$id, layout = layout, max_abs_delta = mx, worst = cl$worst,
       n_compared = as.integer(cl$n_compared + dv$n), fora_da_faixa = as.integer(band$fora),
       fora_da_faixa_long = as.integer(dv$fora_long), day0_source = d0$source, shape_ok = cl$shape_ok,
       na_ok = cl$na_ok, delivery_ok = dv$ok, delivery_missing = as.integer(dv$n_missing), cut_ok = cut_ok,
       ok = cl$ok && dv$ok && cut_ok)
}

## --- 4. golden.lock ---------------------------------------------------------------------------------------------
.pc_parity_check_lock <- function(lock, unit, data_dir, golden_dir, rscript = file.path(R.home("bin"), "Rscript")) {
  e <- lock$units[[unit$id]]
  if (is.null(e)) prevcarga_abort("tools.golden_missing", where = unit$id, details = list(unit = unit$id, item = "golden.lock"))
  for (f in names(e$files)) {
    p <- file.path(golden_dir, f)
    if (!file.exists(p)) prevcarga_abort("tools.golden_missing", where = unit$id, details = list(unit = unit$id, item = f))
    if (!identical(.pc_golden_sha(p), e$files[[f]]))
      prevcarga_abort("tools.golden_stale", where = unit$id, details = list(unit = unit$id, item = f))
  }
  fp <- pc_runtime_fingerprint(names(e$fingerprint$packages), rscript)
  for (k in c("R", "platform", "blas"))
    if (!identical(fp[[k]], e$fingerprint[[k]]))
      prevcarga_abort("tools.golden_stale", where = unit$id,
                      details = list(unit = unit$id, item = k, golden = e$fingerprint[[k]], now = fp[[k]]))
  for (p in names(e$fingerprint$packages))
    if (!identical(fp$packages[[p]], e$fingerprint$packages[[p]]))
      prevcarga_abort("tools.golden_stale", where = unit$id,
                      details = list(unit = unit$id, item = paste0("pacote ", p), golden = e$fingerprint$packages[[p]],
                                     now = fp$packages[[p]]))
  dg <- pc_data_digest(data_dir, unit)
  if (!identical(dg$aggregate, e$data_digest$aggregate)) {
    now <- unlist(dg$partitions); was <- unlist(e$data_digest$partitions)
    names_all <- sort(union(names(now), names(was)), method = "radix")
    gone <- names_all[vapply(names_all, function(n) !identical(unname(now[n]), unname(was[n])), NA)]
    prevcarga_abort("tools.golden_stale", where = unit$id,
                    details = list(unit = unit$id, item = paste0("particao ", if (length(gone)) gone[[1L]] else "?")))
  }
  invisible(TRUE)
}

## --- 5. golden em disco e envelope do clean-room -----------------------------------------------------------------
## timestamp do oraculo e naive (rotulo local): le o rotulo em UTC e reancora no tz da unidade.
.pc_parity_read_long <- function(f, tz) {
  lg <- as.data.frame(arrow::read_parquet(f))
  if (!("timestamp" %in% names(lg)) && "target_datetime" %in% names(lg)) names(lg)[names(lg) == "target_datetime"] <- "timestamp"   # long do oraculo
  lg$timestamp <- lubridate::ymd_hms(format(lg$timestamp, "%Y-%m-%d %H:%M:%S", tz = "UTC"), tz = tz)
  lg
}

## long.parquet da unidade; ou, em rodada de 2 origens (RF), long_O.parquet (O+1..) + o d_plus 1 de long_Om1.parquet (dia O).
.pc_parity_read_dir <- function(dir, tz) {
  nat <- readRDS(file.path(dir, "native.rds"))
  fo <- file.path(dir, "long_O.parquet"); fm <- file.path(dir, "long_Om1.parquet")
  if (file.exists(fo) && file.exists(fm)) {
    lo <- .pc_parity_read_long(fo, tz); lm <- .pc_parity_read_long(fm, tz)
    lm <- lm[lm$d_plus == 1L, , drop = FALSE]
    cols <- intersect(names(lo), names(lm))
    lg <- rbind(lm[, cols, drop = FALSE], lo[, cols, drop = FALSE])
    return(list(native = nat, long = lg[order(lg$timestamp), , drop = FALSE], has_om1 = TRUE))
  }
  list(native = nat, long = .pc_parity_read_long(file.path(dir, "long.parquet"), tz), has_om1 = FALSE)
}

.pc_parity_read_golden <- function(unit, units, golden, model, tz) {
  g <- .pc_parity_read_dir(.pc_golden_unit_dir(golden, model, unit), tz)
  if (identical(unit$day0_source, "oracle_origin_minus_1") && !isTRUE(g$has_om1)) {
    sis <- Filter(function(u) identical(u$area, unit$area) && identical(u$variant, unit$variant) &&
                    u$origin == unit$origin - 1L, units)
    d <- if (length(sis)) .pc_golden_unit_dir(golden, model, sis[[1L]]) else NA_character_
    if (is.na(d) || !dir.exists(d))
      prevcarga_abort("tools.golden_missing", where = unit$id, details = list(unit = unit$id, item = "golden da origem O-1"))
    g$sister <- readRDS(file.path(d, "native.rds"))
  }
  g
}

## Deck de leitura da unidade: copia (em tempdir) do store com a carga das `data_areas` cortada em `available_until` da unidade
## (o corte intradiario e DERIVADO do dado: o store real tem o dia O inteiro, o deck da unidade so ate o corte). Clima e
## feriados por symlink (so Linux); o manifest e copiado. Nunca escreve no store.
.pc_parity_deck <- function(unit, data_dir, deck = tempfile("parity-deck-")) {
  man <- jsonlite::read_json(file.path(data_dir, "manifest.json"), simplifyVector = TRUE)
  dir.create(deck, recursive = TRUE)
  file.copy(file.path(data_dir, "manifest.json"), file.path(deck, "manifest.json"))
  for (d in c("weather", "auxiliary")) if (dir.exists(file.path(data_dir, d))) file.symlink(normalizePath(file.path(data_dir, d)), file.path(deck, d))
  au <- lubridate::with_tz(unit$available_until, man$timezone$load)
  au_digits <- lubridate::force_tz(lubridate::ymd_hms(format(au, "%Y-%m-%d %H:%M:%S"), tz = "UTC"), "UTC")
  shift <- if (identical(man$load_label, "end")) 0L else 30L
  for (a in if (length(unit$data_areas)) unit$data_areas else unit$area) {
    base <- file.path(data_dir, "load", paste0("area_code=", a))
    for (f in list.files(base, pattern = "[.]parquet$", recursive = TRUE)) {
      tb <- arrow::read_parquet(file.path(base, f), as_data_frame = FALSE)
      ts <- as.data.frame(tb[, "timestamp"])$timestamp
      keep <- as.numeric(ts) + shift * 60 <= as.numeric(au_digits)
      dst <- file.path(deck, "load", paste0("area_code=", a), f)
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      if (any(keep)) arrow::write_parquet(tb$Filter(arrow::Array$create(keep)), dst)
    }
  }
  deck
}

## Envelope do clean-room pelos despachantes (leitor -> cortes -> train/infer), como em P.0i, com os cortes da unidade.
.pc_parity_envelope <- function(model, unit, models_dir, data_dir, topology = NULL) {
  plugin <- prevcarga_plugin_load(model, models_dir)
  deck <- .pc_parity_deck(unit, data_dir)
  on.exit(unlink(deck, recursive = TRUE), add = TRUE)
  ctx <- list(data_dir = deck, root = dirname(models_dir), weather_issue_cutoff = unit$weather_issue_cutoff)
  if (!is.null(topology)) ctx$topology_path <- topology
  lag <- stats::setNames(plugin$descriptor$data_lag_days, model)
  rc <- suppressWarnings(prevcarga_resolve_cuts(ctx, origin = unit$origin, model = model, data_lag_days = lag))
  row <- rc$cuts[rc$cuts$model == model, ]
  cuts <- list(forecast_origin = rc$forecast_origin, origin_day = rc$forecast_origin, split = row$split, tz = rc$tz,
               load_label = rc$load_label, weather_issue_cutoff = rc$weather_issue_cutoff,
               available_until = rc$available_until, weather_issue_limit = rc$weather_issue_limit,
               train_origin = row$train_origin, last_data_day = row$last_data_day)
  sp <- plugin$spec
  ## cortes e parametros LITERAIS da unidade (nunca os defaults do run.yaml quando a unidade os declara)
  if (length(unit$params)) sp$params <- utils::modifyList(sp$params, unit$params)
  if (!is.null(unit$seed)) sp$seed <- unit$seed
  if (!is.null(unit$cuts$train$train_origin) && !identical(format(row$train_origin), unit$cuts$train$train_origin))
    prevcarga_abort("tools.parity_failed", where = unit$id,
                    details = list(unit = unit$id, field = "train_origin", unit_value = unit$cuts$train$train_origin,
                                   derived = format(row$train_origin)))
  read <- function(origin, hist) {
    p <- suppressWarnings(prevcarga_read_panel(ctx, origin, unit$area, sp$horizon_days, hist))
    ## previsto: o painel do leitor entrega so a emissao mais fresca; o plugin precisa de TODAS as emissoes ate o limite
    ## (safra por origem nativa), como nos testes do plugin (rf_fx_panel)
    lim <- attr(p$weather_forecast, "weather_issue_limit")
    fc <- prevcarga_read_weather(ctx, "forecast", unit$area, origin - hist, origin + sp$horizon_days + 1L)
    p$weather_forecast <- fc[issue_time <= lim]
    ## painel de 30 min CRU: o despachante reamostra (granularity_min + weather_resample do descriptor; FIX 7)
    p
  }
  ## `spec` e a fase viajam em cuts (interino do RF.1: o despachante P.0c ainda nao os repassa ao estagio de ingestao)
  ## janela de leitura = a da unidade (history_start literal da fase); sem `cuts`, a do run.yaml
  hist <- function(phase, origin, default) {
    h <- unit$cuts[[phase]]$history_start
    if (is.null(h)) default else as.integer(origin - lubridate::ymd(h))
  }
  art <- prevcarga_plugin_train(plugin, read(row$last_data_day, hist("train", row$last_data_day, sp$history_days$train)), c(cuts, list(spec = sp, phase = "train")), sp, list())
  prevcarga_plugin_infer(plugin, art, read(unit$origin, hist("infer", unit$origin, sp$history_days$infer)), c(cuts, list(spec = sp, phase = "infer")), list())
}

## --- 6. relatorio e CLI -----------------------------------------------------------------------------------------
.pc_parity_report_json <- function(units) {
  rep <- lapply(units, function(r) .pc_golden_canon(r))
  txt <- jsonlite::toJSON(list(units = unname(rep), tolerance = .PC_PARITY_TOL), auto_unbox = TRUE, null = "null",
                          digits = NA, na = "null", pretty = TRUE)
  paste0(paste(as.character(txt), collapse = "\n"), "\n")
}

pc_parity_parse_args <- function(argv, repo) {
  known <- c("model", "units", "golden", "root", "data", "topology", "out", "rscript", "format", "lock")
  if (length(argv) %% 2L != 0L || any(!grepl("^--", argv[seq(1L, length(argv), by = 2L)])))
    prevcarga_abort("tools.bad_args", where = "argv")
  k <- if (length(argv)) sub("^--", "", argv[seq(1L, length(argv), by = 2L)]) else character()
  a <- stats::setNames(as.list(if (length(argv)) argv[seq(2L, length(argv), by = 2L)] else character()), k)
  if (length(bad <- setdiff(k, known))) prevcarga_abort("tools.bad_args", where = paste0("--", bad[[1L]]))
  for (f in c("model", "units", "golden")) if (is.null(a[[f]])) prevcarga_abort("tools.bad_args", where = paste0("--", f))
  if (is.null(a$root)) a$root <- file.path(repo, "models")
  if (is.null(a$rscript)) a$rscript <- file.path(R.home("bin"), "Rscript")
  if (is.null(a$format)) a$format <- "text"
  if (!(a$format %in% c("text", "json"))) prevcarga_abort("tools.bad_args", where = "--format")
  if (!grepl("^[A-Za-z0-9_][A-Za-z0-9._-]*$", a$model)) prevcarga_abort("tools.bad_args", where = "--model")
  if (is.null(a$out)) a$out <- file.path(a$golden, "parity_report.json")
  ## topologia da paridade do modelo (areas de subsistema como SECO), quando o modelo a declara
  dtop <- file.path(a$root, a$model, "tests", "parity", "topology.yaml")
  if (is.null(a$topology) && file.exists(dtop)) a$topology <- dtop
  a
}

pc_parity_main <- function(argv, repo) {
  args <- pc_parity_parse_args(argv, repo)
  ru <- pc_golden_resolve_units(args$units, args$root, args$model)
  spec <- pc_golden_units(ru$path)
  lockf <- if (is.null(args$lock)) file.path(args$root, args$model, "tests", "parity", "golden.lock") else args$lock
  if (!dir.exists(args$golden) || !file.exists(lockf))
    prevcarga_abort("tools.golden_missing", where = args$golden,
                    details = list(item = if (!dir.exists(args$golden)) "golden" else "golden.lock"))
  lock <- jsonlite::read_json(lockf, simplifyVector = FALSE)
  res <- list()
  for (u in pc_golden_group_units(spec, ru$group)) {
    d <- .pc_golden_unit_dir(args$golden, args$model, u)
    if (!dir.exists(d)) prevcarga_abort("tools.golden_missing", where = u$id, details = list(unit = u$id, item = d))
    .pc_parity_check_lock(lock, u, args$data, d, args$rscript)
    g <- .pc_parity_read_golden(u, spec$units, args$golden, args$model, spec$tz)
    env <- .pc_parity_envelope(args$model, u, args$root, args$data, args$topology)
    res[[u$id]] <- prevcarga_parity_compare(env, g, u, delivery_days = NULL)
  }
  txt <- .pc_parity_report_json(res)
  writeBin(charToRaw(txt), args$out)
  sha <- digest::digest(args$out, algo = "sha256", file = TRUE)
  report <- list(units = unname(res), report = args$out, sha256 = sha, tolerance = .PC_PARITY_TOL)
  bad <- Filter(function(r) !isTRUE(r$ok), res)
  if (length(bad)) {
    w <- bad[[order(-vapply(bad, function(r) r$max_abs_delta, 0))[[1L]]]]
    prevcarga_abort("tools.parity_failed", where = w$unit,
                    details = list(unit = w$unit, layout = w$layout, max_abs_delta = w$max_abs_delta, worst = w$worst,
                                   shape_ok = w$shape_ok, na_ok = w$na_ok, delivery_ok = w$delivery_ok, sha256 = sha),
                    result = report)
  }
  report
}

.pc_parity_text <- function(report) {
  c(vapply(report$units, function(r) sprintf("%s layout=%s max_abs_delta=%.3g n_compared=%d fora_da_faixa=%d day0_source=%s ok=%s",
      r$unit, r$layout, r$max_abs_delta, r$n_compared, r$fora_da_faixa, r$day0_source, if (r$ok) "TRUE" else "FALSE"), ""),
    paste0("sha256: ", report$sha256))
}

pc_parity_cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
  tools_dir <- dirname(normalizePath(f, winslash = "/"))
  lib <- file.path(tools_dir, "..", "lib")
  repo <- normalizePath(file.path(lib, "..", ".."), winslash = "/")
  options(prevcarga.lib = normalizePath(lib, winslash = "/"))
  for (m in c("errors", "runtime")) source(file.path(lib, paste0(m, ".R")))
  invisible(prevcarga_runtime_enforce())
  for (m in c("config", "provenance", "topology", "deck_reader", "validation", "dates", "registry", "features"))
    source(file.path(lib, paste0(m, ".R")))
  ## plugin_golden.R tem guarda propria (so roda no globalenv): carregado num env filho e as funcoes reusadas aqui.
  ge <- new.env(parent = globalenv())
  sys.source(file.path(tools_dir, "plugin_golden.R"), envir = ge)
  for (n in c("pc_golden_units", "pc_golden_resolve_units", "pc_golden_group_units", "pc_runtime_fingerprint", "pc_data_digest", ".pc_golden_sha", ".pc_golden_unit_dir",
              ".pc_golden_canon")) assign(n, get(n, envir = ge), envir = globalenv())
  fmt <- if (length(i <- which(argv == "--format")) && i[[1L]] < length(argv)) argv[[i[[1L]] + 1L]] else "text"
  out <- prevcarga_with_errors(pc_parity_main(argv, repo))
  if (!is.null(out$cond)) {
    if (fmt == "json") {
      writeLines(prevcarga_render_error(out$cond, format = "json", verb = "plugin_parity"), con = stdout())
    } else {
      writeLines(prevcarga_render_error(out$cond, format = "text", verb = "plugin_parity"), con = stderr())
      dt <- out$cond$details
      writeLines(paste0("  id: ", out$cond$id), con = stderr())
      if (length(dt$item)) writeLines(paste0("  item: ", dt$item, " -- regere o golden com scripts/tools/plugin_golden.R"), con = stderr())
      if (length(dt$unit)) writeLines(paste0("  unidade: ", dt$unit), con = stderr())
      if (length(dt$sha256)) writeLines(paste0("  sha256: ", dt$sha256), con = stderr())
    }
  } else if (fmt == "json") {
    writeLines(as.character(jsonlite::toJSON(prevcarga_envelope(verb = "plugin_parity", result = out$value),
                                             auto_unbox = TRUE, null = "null", digits = NA, na = "null")), con = stdout())
  } else {
    writeLines(.pc_parity_text(out$value), con = stdout())
  }
  quit(save = "no", status = out$exit_code)
}

if (identical(environment(), globalenv()) && !interactive()) pc_parity_cli()
