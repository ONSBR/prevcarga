## validation.R -- validacao model-agnostica do deck (M2.3 predicados + M2.4 prevcarga_validate_deck; ADR-0040)
##
## Valida o deck canonico (docs/reference/DECK-CANONICO.md) ANTES de qualquer compute, com veredito ACUMULATIVO:
## nunca aborta no primeiro defeito. Os aborts classificados do leitor/dates viram findings; erro de uso
## (classe config/runtime) sobe. Somente leitura sobre `data_dir`; nao devolve exit (o handler do M1 mapeia
## classe -> exit pela precedencia unica `prevcarga_exit_precedence()` do errors.R, sem lista propria aqui).
##
## Premissas: a referencia NAO preenche com zero (deck com NA/0 reprova); completude flat-48 por dia de
## `interval_start` (dia de 46 e buraco); sem faixa de MW por subsistema e sem monotonicidade; janela de TREINO
## contada ate o `last_data_day` de cada membro (O-1, ou O-3 no LGBM via `data_lag_days`) e janela de
## INFERENCIA ate O-1, como findings distintos; historicos vem do `run.yaml` (`history_days`).
## Classes (ADR-0040): contract, completude, topology, window; avisos (level warning) nao reprovam.
## O salto de temperatura (> 10 C entre horas consecutivas, observado por area e previsto por emissao) e so aviso;
## a faixa fisica (temperature_out_of_range) segue erro contract.
## O outlier de carga e LOCAL (vizinhos +-2 e mesmo horario em +-7 dias, desvio > 30% nos dois), so aviso.
## Requer errors.R, topology.R, config.R, deck_reader.R e dates.R carregados antes. Saida: ASCII puro.

## --- 1. Vocabulario do veredito -----------------------------------------------------------------------
prevcarga_temp_range_c <- c(-10, 50)   # faixa fisica: ERRO contract (exit 2)
prevcarga_temp_jump_c <- 10            # V2: |T(t+1h) - T(t)| > 10 C => aviso temperature_jump
prevcarga_flatline_min_slots <- 6L
## V1 outlier local: +-neigh intervalos vizinhos (sem x) e mesmo interval_start em d+-1..k dias
prevcarga_outlier_neigh_slots <- 2L
prevcarga_outlier_days_k <- 7L
prevcarga_outlier_rel <- 0.30
prevcarga_outlier_min_days <- 3L

.pc_empty_findings <- function() {
  data.table::data.table(area = character(), dataset = character(), what = character(), detail = character(),
                         class = character(), level = character(), where = character())
}

.pc_finding <- function(area, dataset, what, detail = "", class, level = "error", where = NA_character_) {
  data.table::data.table(area = as.character(area), dataset = as.character(dataset), what = as.character(what),
                         detail = as.character(detail), class = as.character(class), level = as.character(level),
                         where = as.character(where))
}

.pc_findings_bind <- function(lst) {
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0L, lst)
  if (!length(parts)) return(.pc_empty_findings())
  data.table::rbindlist(parts, use.names = TRUE)
}

## Coletor de findings (ordem de insercao preservada).
.pc_acc <- function() {
  st <- new.env(parent = emptyenv())
  st$f <- list()
  list(add = function(x) { if (!is.null(x)) st$f[[length(st$f) + 1L]] <- x; invisible(NULL) },
       get = function() .pc_findings_bind(st$f))
}

.prevcarga_verdict <- function(findings) {
  err <- findings[findings$level == "error", ]
  if (!nrow(err)) return(list(ok = TRUE, class = "ok"))
  list(ok = FALSE, class = prevcarga_exit_precedence(err$class))   # unica fonte (ADR-0040)
}

## --- Captura de aborts classificados -------------------------------------------------------------------
.pc_what <- function(id) sub("^[a-z]+\\.", "", id)

.pc_cond_area <- function(where) {
  if (is.null(where) || !length(where) || is.na(where[[1L]])) return("*")
  if (grepl("^[^:]+:[0-9]{2}:[0-9]{2}$", where[[1L]])) sub(":.*$", "", where[[1L]]) else "*"
}

## Executa `expr`; aborts classificados (exceto config/runtime, que sobem) viram `finding`; avisos classificados
## viram findings level warning no coletor `acc` e sao silenciados.
.prevcarga_capture <- function(expr, dataset, acc) {
  withCallingHandlers(
    tryCatch(list(value = expr, finding = NULL),
      prevcarga_error = function(e) {
        if (e$class_name %in% c("config", "runtime")) stop(e)
        list(value = NULL,
             finding = .pc_finding(.pc_cond_area(e$where), dataset, .pc_what(e$id),
                                   detail = sub("^\\[prevcarga/[a-z]+\\] ", "", conditionMessage(e)),
                                   class = e$class_name, where = if (is.null(e$where)) NA_character_ else e$where))
      }),
    prevcarga_warning = function(w) {
      acc$add(.pc_finding(.pc_cond_area(w$where), dataset, .pc_what(w$id), detail = if (is.null(w$where)) "" else w$where,
                          class = w$class_name, level = "warning",
                          where = if (is.null(w$where)) NA_character_ else w$where))
      invokeRestart("muffleWarning")
    })
}

## Leitura defensiva: abort classificado vira finding; qualquer outro erro do arrow vira parquet_unreadable.
.pc_read <- function(expr, dataset, acc) {
  r <- tryCatch(.prevcarga_capture(expr, dataset, acc),
    error = function(e) {
      if (inherits(e, "prevcarga_error")) stop(e)
      list(value = NULL, finding = .pc_finding("*", dataset, "parquet_unreadable", detail = "leitura falhou",
                                               class = "contract", where = dataset))
    })
  acc$add(r$finding)
  r$value
}

## --- 2. Contrato canonico do deck como dado ------------------------------------------------------------
.prevcarga_deck_spec <- function() {
  data.table::rbindlist(list(
    list("load", "timestamp", "timestamp", TRUE), list("load", "load_mwh", "double", TRUE),
    list("weather_forecast", "issue_time", "timestamp", TRUE), list("weather_forecast", "valid_time", "timestamp", TRUE),
    list("weather_forecast", "temperature", "double", TRUE), list("weather_forecast", "heat_index", "double", FALSE),
    list("weather_observed", "timestamp", "timestamp", TRUE), list("weather_observed", "temperature", "double", TRUE),
    list("weather_observed", "heat_index", "double", FALSE), list("weather_observed", "rh", "double", FALSE),
    list("holidays", "date", "date32", TRUE), list("holidays", "holiday_type_id", "int64", TRUE),
    list("holidays", "prevcarga_code", "int64", TRUE), list("holidays", "simplified_code", "int64", TRUE)
  ))[, stats::setNames(.SD, c("dataset", "column", "type", "required"))]
}

.pc_type_ok <- function(actual, expected) {
  switch(expected,
    timestamp = grepl("^timestamp\\[", actual),
    date32 = grepl("^date32", actual),
    identical(actual, expected))
}

## Traduz a sonda do leitor em findings contract sem coletar linhas. Devolve os findings e os datasets reprovados.
.prevcarga_check_contract <- function(probe, spec) {
  out <- list(); bad <- character()
  add <- function(x) out[[length(out) + 1L]] <<- x
  for (ds in unique(spec$dataset)) {
    p <- probe[[ds]]; req <- spec[spec$dataset == ds, ]
    rel <- .PREVCARGA_DECK_DATASETS[[ds]]
    if (!isTRUE(p$present)) {
      add(.pc_finding("*", ds, "dataset_missing", rel, "contract", where = ds)); bad <- c(bad, ds); next
    }
    if (!isTRUE(p$hive_ok)) {
      add(.pc_finding("*", ds, "hive_layout", rel, "contract", where = rel)); bad <- c(bad, ds)
    }
    for (f in p$unreadable) add(.pc_finding("*", ds, "parquet_unreadable", f, "contract", where = f))
    if (length(p$unreadable)) bad <- c(bad, ds)
    if (!length(p$columns)) { bad <- c(bad, ds); next }
    for (cn in setdiff(req$column[req$required], p$columns)) {
      add(.pc_finding("*", ds, "schema_missing", cn, "contract", where = paste0(ds, ".", cn))); bad <- c(bad, ds)
    }
    for (i in which(req$column %in% p$columns)) {
      cn <- req$column[i]; actual <- p$types[[cn]]
      if (isTRUE(p$tz_in_parquet[cn])) {
        add(.pc_finding("*", ds, "tz_in_parquet", actual, "contract", where = paste0(ds, ".", cn))); bad <- c(bad, ds)
      } else if (!.pc_type_ok(actual, req$type[i])) {
        add(.pc_finding("*", ds, "type_mismatch", sprintf("%s: %s, esperado %s", cn, actual, req$type[i]), "contract",
                        where = paste0(ds, ".", cn)))
        if (req$required[i]) bad <- c(bad, ds)
      }
    }
  }
  list(findings = .pc_findings_bind(out), bad = unique(bad))
}

## --- 8. Escopo e topologia -------------------------------------------------------------------------------
.pc_present_areas <- function(probe) {
  rows <- lapply(names(probe), function(ds) {
    f <- if (isTRUE(probe[[ds]]$present)) grep("area_code=[^/]+", probe[[ds]]$files, value = TRUE) else character()
    if (!length(f)) return(NULL)
    data.table::data.table(dataset = ds, area = sub(".*area_code=([^/]+).*", "\\1", f))
  })
  unique(data.table::rbindlist(rows))
}

.prevcarga_check_scope <- function(ctx, probe, scope, acc) {
  tp <- if (!is.null(ctx$topology_path)) ctx$topology_path else prevcarga_topology_path(ctx$root)
  topo <- .prevcarga_capture(prevcarga_topology(path = tp), "scope", acc)
  if (is.null(topo$value)) {
    acc$add(topo$finding)
    return(list(areas = character(), models = character(), models_cuts = NULL, leaves = character()))
  }
  subs <- topo$value$subsystems
  declared <- unique(c(topo$value$leaves, names(subs), vapply(subs, function(s) s$loss, "")))
  pa <- .pc_present_areas(probe)
  present <- if (is.null(pa) || !nrow(pa)) character() else sort(unique(pa$area))
  for (a in setdiff(present, declared)) {
    acc$add(.pc_finding(a, pa$dataset[pa$area == a][[1L]], "area_undeclared", a, "topology", where = a))
  }
  leaves <- topo$value$leaves
  wx <- if (is.null(pa)) pa else pa[pa$dataset %in% c("weather_observed", "weather_forecast") &
                                    pa$area %in% declared & !pa$area %in% leaves]
  if (!is.null(wx) && nrow(wx)) {
    data.table::setorderv(wx, c("area", "dataset"))
    for (i in seq_len(nrow(wx)))
      acc$add(.pc_finding(wx$area[i], wx$dataset[i], "weather_non_leaf",
        sprintf("%s nao e folha (subsistema ou perda)", wx$area[i]), "topology", level = "warning",
        where = sprintf("%s/area_code=%s", .PREVCARGA_DECK_DATASETS[[wx$dataset[i]]], wx$area[i])))
  }
  areas <- if (is.null(scope$area)) intersect(present, declared) else {
    keep <- character()
    for (a in scope$area) {
      if (!a %in% declared) {
        acc$add(.pc_finding(a, "scope", "scope_undeclared", a, "topology", where = a)); next
      }
      keep <- c(keep, if (a %in% names(subs)) c(subs[[a]]$areas, a) else a)
    }
    unique(keep)
  }
  areas <- sort(areas)

  mem <- .prevcarga_capture(prevcarga_members(path = tp, root = ctx$root), "scope", acc)
  acc$add(mem$finding)
  all_m <- if (is.null(mem$value)) character() else unique(mem$value$mdir)
  if (is.null(scope$model)) {
    models <- all_m; mc <- NULL
  } else {
    for (m in setdiff(scope$model, all_m))
      acc$add(.pc_finding("*", "scope", "scope_undeclared", m, "topology", where = m))
    models <- intersect(scope$model, all_m)
    mc <- if (length(models)) models else NULL
  }
  list(areas = areas, models = models, models_cuts = mc, leaves = leaves)
}

## --- 4/7. Intervalo relevante e janelas ---------------------------------------------------------------
## Menor inicio entre a janela de treino (ate o last_data_day de cada membro) e a de inferencia (ate O-1).
.prevcarga_relevant_from <- function(cuts, O, root) {
  from <- vapply(seq_len(nrow(cuts)), function(i) {
    h <- prevcarga_model_spec(cuts$model[i], root)$history_days
    as.numeric(min(cuts$last_data_day[i] - h$train + 1L, O - 1L - h$infer + 1L))
  }, numeric(1))
  lubridate::as_date(min(from))
}

.prevcarga_check_completeness <- function(load, from, O, areas, tz) {
  if (!nrow(load)) return(NULL)
  day <- lubridate::as_date(load$interval_start, tz = tz)
  first <- data.table::data.table(area_code = load$area_code, d = day)[, list(first = min(d)), by = "area_code"]
  dc <- prevcarga_day_complete(load)
  grid <- data.table::rbindlist(lapply(seq_len(nrow(first)), function(i) {
    lo <- max(from, first$first[i]); hi <- O - 1L
    if (lo > hi) return(NULL)
    data.table::data.table(area_code = first$area_code[i], date = seq(lo, hi, by = "day"))
  }))
  if (is.null(grid) || !nrow(grid)) return(NULL)
  m <- merge(grid, dc, by = c("area_code", "date"), all.x = TRUE)
  m[is.na(n_slots), n_slots := 0L]
  bad <- m[n_slots != prevcarga_slots_per_day]
  data.table::setorderv(bad, c("area_code", "date"))
  if (!nrow(bad)) return(NULL)
  .pc_findings_bind(lapply(seq_len(nrow(bad)), function(i)
    .pc_finding(bad$area_code[i], "load", "day_incomplete",
                sprintf("%s: %d/%d", format(bad$date[i]), bad$n_slots[i], prevcarga_slots_per_day),
                "completude", where = "load")))
}

.prevcarga_first_off <- function(dt, area_col = "area_code", stamp_col = "interval_start") {
  sprintf("%d linhas; primeiro: %s", nrow(dt), format(dt[[stamp_col]][1L], "%Y-%m-%d %H:%M"))
}

.prevcarga_check_load_sanity <- function(load) {
  out <- list()
  if (!nrow(load)) return(NULL)
  off <- load[!(lubridate::minute(interval_start) %in% c(0L, 30L)) | lubridate::second(interval_start) != 0]
  for (a in unique(off$area_code))
    out[[length(out) + 1L]] <- .pc_finding(a, "load", "grid_off", .prevcarga_first_off(off[off$area_code == a]),
                                           "contract", where = "load")
  dup <- load[, list(N = .N), by = c("area_code", "interval_start")]
  dup <- dup[dup$N > 1L]
  for (a in unique(dup$area_code))
    out[[length(out) + 1L]] <- .pc_finding(a, "load", "duplicate_interval",
                                           .prevcarga_first_off(dup[dup$area_code == a]), "contract", where = "load")
  np <- load[!is.na(load_mwh) & load_mwh <= 0]
  for (a in unique(np$area_code))
    out[[length(out) + 1L]] <- .pc_finding(a, "load", "load_nonpositive", .prevcarga_first_off(np[np$area_code == a]),
                                           "contract", where = "load")
  .pc_findings_bind(out)
}

.prevcarga_check_temperature <- function(w, dataset) {
  if (is.null(w) || !nrow(w) || !"temperature" %in% names(w)) return(NULL)
  r <- prevcarga_temp_range_c
  bad <- w[!is.na(temperature) & (temperature < r[1L] | temperature > r[2L])]
  .pc_findings_bind(lapply(unique(bad$area_code), function(a) {
    b <- bad[bad$area_code == a]
    .pc_finding(a, dataset, "temperature_out_of_range", sprintf("%d linhas; primeiro: %s C", nrow(b), format(b$temperature[1L])),
                "contract", where = paste0(dataset, ".temperature"))
  }))
}

.prevcarga_check_temperature_jump <- function(w, dataset) {
  if (is.null(w) || !nrow(w) || !"temperature" %in% names(w)) return(NULL)
  by <- if (dataset == "weather_forecast") c("area_code", "issue_time") else "area_code"
  a <- w[!is.na(temperature), c(by, "valid_time", "temperature"), with = FALSE]
  ## pareamento por diferenca de tempo (3600 s), sem deslocar a coluna valid_time (m2_load_shift)
  a[, t_s := as.numeric(valid_time)]
  b <- data.table::copy(a)[, t_s := t_s - 3600]
  p <- a[b, on = c(by, "t_s"), nomatch = NULL][, dT := i.temperature - temperature]
  bad <- p[abs(dT) > prevcarga_temp_jump_c]
  data.table::setorderv(bad, c("area_code", "valid_time"))
  .pc_findings_bind(lapply(unique(bad$area_code), function(ar) {
    x <- bad[bad$area_code == ar]
    .pc_finding(ar, dataset, "temperature_jump", sprintf("%d pares; primeiro: %s (%+.1f C)", nrow(x),
      format(x$valid_time[1L], "%Y-%m-%d %H:%M"), x$dT[1L]), "contract", level = "warning",
      where = paste0(dataset, ".temperature"))
  }))
}

.prevcarga_check_holiday_codes <- function(h) {
  if (is.null(h) || !nrow(h)) return(NULL)
  out <- list()
  for (cn in c("holiday_type_id", "prevcarga_code", "simplified_code")) {
    v <- as.numeric(h[[cn]])
    bad <- which(!is.na(v) & v < 0)
    for (a in unique(h$area_code[bad])) {
      b <- bad[h$area_code[bad] == a]
      out[[length(out) + 1L]] <- .pc_finding(a, "holidays", "code_negative",
        sprintf("%s: %d linhas; primeiro: %s = %s", cn, length(b), format(h$date[b[1L]]), format(v[b[1L]])),
        "contract", where = paste0("holidays.", cn))
    }
  }
  .pc_findings_bind(out)
}

## --- 6. Avisos heuristicos --------------------------------------------------------------------------------
## Outlier LOCAL (so aviso): x desvia mais de `prevcarga_outlier_rel` da mediana dos vizinhos (+-neigh, sem x) E da
## mediana do mesmo interval_start em d+-1..k dias (lookup por igualdade de instante). So avalia x com >= min_days
## referencias de dia. Entrada: data.table de UMA area, ordenada, sem NA/duplicata. Saida: posicoes marcadas.
.prevcarga_outlier_local <- function(s) {
  x <- s$load_mwh; n <- length(x); k <- prevcarga_outlier_neigh_slots
  if (n < 2L) return(integer(0))
  ref_viz <- vapply(seq_len(n), function(i) {
    j <- setdiff(max(1L, i - k):min(n, i + k), i); stats::median(x[j])
  }, numeric(1))
  off <- c(-(prevcarga_outlier_days_k:1L), 1:prevcarga_outlier_days_k)
  refs <- data.table::rbindlist(lapply(off, function(o)
    data.table::data.table(i = seq_len(n), t = s$interval_start + lubridate::days(o))))
  base <- data.table::data.table(t = s$interval_start, ld = x)
  refs <- base[refs, on = "t", nomatch = NULL][, list(ref = stats::median(ld), nd = .N), by = "i"]
  ref_dia <- rep(NA_real_, n); nd <- integer(n)
  ref_dia[refs$i] <- refs$ref; nd[refs$i] <- refs$nd
  rel <- prevcarga_outlier_rel
  which(nd >= prevcarga_outlier_min_days & abs(x / ref_viz - 1) > rel & abs(x / ref_dia - 1) > rel)
}

.prevcarga_flatline <- function(x) {
  r <- rle(x)
  any(r$lengths >= prevcarga_flatline_min_slots, na.rm = TRUE)
}

.prevcarga_check_heuristics <- function(load) {
  out <- list()
  v <- load[!is.na(load_mwh)]
  v <- unique(v, by = c("area_code", "interval_start"))
  data.table::setorderv(v, c("area_code", "interval_start"))
  for (a in unique(v$area_code)) {
    s <- v[v$area_code == a]
    if (nrow(s) < 2L) next
    idx <- .prevcarga_outlier_local(s)
    if (length(idx))
      out[[length(out) + 1L]] <- .pc_finding(a, "load", "outlier", sprintf("%d intervalos; primeiro: %s", length(idx),
        format(s$interval_start[idx[1L]], "%Y-%m-%d %H:%M")), "contract", level = "warning", where = "load")
    if (.prevcarga_flatline(s$load_mwh))
      out[[length(out) + 1L]] <- .pc_finding(a, "load", "flatline", sprintf(">= %d intervalos identicos", prevcarga_flatline_min_slots),
                                             "contract", level = "warning", where = "load")
  }
  .pc_findings_bind(out)
}

.prevcarga_check_climate_cut <- function(raw, limit, O, tz) {
  if (is.null(raw) || !nrow(raw)) return(NULL)
  late <- raw[issue_time > limit & lubridate::as_date(issue_time, tz = tz) == O,
              list(n = data.table::uniqueN(issue_time)), by = "area_code"]
  .pc_findings_bind(lapply(seq_len(nrow(late)), function(i)
    .pc_finding(late$area_code[i], "weather_forecast", "climate_after_cut",
                sprintf("%d emissoes apos o limite", late$n[i]), "contract", level = "warning",
                where = "weather_forecast.issue_time")))
}

## --- 7. Janela minima por membro ------------------------------------------------------------------------------
.prevcarga_check_window <- function(load, cuts, O, areas, root, tz) {
  out <- list()
  v <- load[!is.na(load_mwh)]
  days <- unique(data.table::data.table(area_code = v$area_code, d = lubridate::as_date(v$interval_start, tz = tz)))
  for (i in seq_len(nrow(cuts))) {
    m <- cuts$model[i]; h <- prevcarga_model_spec(m, root)$history_days
    last <- cuts$last_data_day[i]
    for (a in areas) {
      da <- days$d[days$area_code == a]
      have_t <- sum(da >= last - h$train + 1L & da <= last)
      have_i <- sum(da >= O - 1L - h$infer + 1L & da <= O - 1L)
      if (have_t < h$train)
        out[[length(out) + 1L]] <- .pc_finding(a, "load", "window_train_deficit",
          sprintf("%s: train=%d, tem %d dias ate %s", m, h$train, have_t, format(last)), "window",
          where = sprintf("models/%s/run.yaml:history_days.train", m))
      if (have_i < h$infer)
        out[[length(out) + 1L]] <- .pc_finding(a, "load", "window_infer_deficit",
          sprintf("%s: infer=%d, tem %d dias ate %s", m, h$infer, have_i, format(O - 1L)), "window",
          where = sprintf("models/%s/run.yaml:history_days.infer", m))
    }
  }
  .pc_findings_bind(out)
}

## --- 7b. Necessidade AGREGADA dos artefatos (deck kind inference; substitui .prevcarga_check_window) ----------------
## `agg` = prevcarga_inference_needs_aggregate(rows) (inference_needs.R): por dataset e area, os dias exigidos relativos a O.
## load: 48 intervalos em cada dia de [O - days_before, O - 1]; weather_observed: >= 1 leitura em cada dia da janela;
## weather_forecast: por segmento, freshest/min_lead exigem uma emissao valida (issue_time <= weather_issue_limit, lead
## satisfeito) por dia valido em [O+from, O+to] (a regra do proprio leitor, prevcarga_read_forecast_by_rule); per_origin so a
## emissao da origem T = O; holidays: janela inference.windows.holidays do manifest cobre [O+from, O+to]; holiday_annex pedido =>
## anexo presente no manifest e no disco. Falta => finding erro completude.need_gap, detail = dias YYYY-MM-DD em ordem separados
## por virgula. Devolve list(findings, windows) com windows = data.table(dataset, area, from, to, source = "artifacts").
## Nenhum nome de modelo; so leitura.
.prevcarga_need_days <- function(from, to) if (to < from) lubridate::as_date(character()) else seq(from, to, by = "day")

.prevcarga_need_read <- function(expr) tryCatch(expr, prevcarga_error = function(e) {
  if (e$class_name %in% c("config", "runtime")) stop(e)
  NULL
})

.prevcarga_check_inference_needs <- function(ctx, O, cuts, agg, tz, manifest = NULL) {
  O <- lubridate::as_date(O)
  fnd <- list(); win <- list()
  gap <- function(a, ds, miss)
    if (length(miss)) fnd[[length(fnd) + 1L]] <<- .pc_finding(a, ds, "need_gap",
      paste(format(sort(unique(miss)), "%Y-%m-%d"), collapse = ","), "completude", where = ds)
  window <- function(a, ds, from, to) win[[length(win) + 1L]] <<- data.table::data.table(
    dataset = ds, area = a, from = from, to = to, source = "artifacts")
  have_days <- function(x, col, a) {
    x <- x[x$area_code == a]
    lubridate::as_date(x[[col]], tz = tz)
  }

  ld <- agg$load
  if (!is.null(ld) && nrow(ld)) {
    nb <- max(ld$days_before)
    if (nb > 0L) {
      raw <- .prevcarga_need_read(prevcarga_read_load(ctx, ld$area_code, from = O - nb, to = O - 1L))
      dc <- if (!is.null(raw) && nrow(raw)) prevcarga_day_complete(raw) else NULL
      for (i in seq_len(nrow(ld))) {
        a <- ld$area_code[i]; b <- ld$days_before[i]
        if (b < 1L) next
        want <- .prevcarga_need_days(O - b, O - 1L)
        window(a, "load", O - b, O - 1L)
        ok <- if (is.null(dc)) want[0L] else dc$date[dc$area_code == a & dc$complete]
        gap(a, "load", want[!want %in% ok])
      }
    }
  }

  wo <- agg$weather_observed
  if (!is.null(wo) && nrow(wo)) {
    nb <- max(wo$days_before)
    if (nb > 0L) {
      raw <- .prevcarga_need_read(prevcarga_read_weather(ctx, "observed", wo$area_code, O - nb, O - 1L))
      for (i in seq_len(nrow(wo))) {
        a <- wo$area_code[i]; b <- wo$days_before[i]
        if (b < 1L) next
        want <- .prevcarga_need_days(O - b, O - 1L)
        window(a, "weather_observed", O - b, O - 1L)
        have <- if (is.null(raw) || !nrow(raw)) want[0L] else have_days(raw, "valid_time", a)
        gap(a, "weather_observed", want[!want %in% have])
      }
    }
  }

  wf <- agg$weather_forecast
  if (!is.null(wf) && nrow(wf)) {
    lim <- cuts$weather_issue_limit
    for (a in sort(unique(wf$area_code), method = "radix")) {
      s <- wf[wf$area_code == a]
      miss <- lubridate::as_date(character())
      for (i in seq_len(nrow(s))) {
        per <- identical(s$kind[i], "per_origin")
        seg <- list(kind = s$kind[i], from = if (per) 0L else s$from[i], to = if (per) 0L else s$to[i],
                    lead = s$lead[i])
        want <- .prevcarga_need_days(O + seg$from, O + seg$to)
        fc <- .prevcarga_need_read(prevcarga_read_forecast_by_rule(ctx, a, O, list(seg), lim))
        have <- if (is.null(fc) || !nrow(fc)) want[0L] else
          if (per) O else lubridate::as_date(fc$valid_time, tz = lubridate::tz(fc$valid_time))
        miss <- c(miss, want[!want %in% have])
      }
      window(a, "weather_forecast", O + min(wf$from[wf$area_code == a]), O + max(wf$to[wf$area_code == a]))
      gap(a, "weather_forecast", miss)
    }
  }

  ho <- agg$holidays
  inf_win <- if (!is.null(manifest)) manifest$inference$windows$holidays else NULL
  if (!is.null(ho) && nrow(ho)) for (i in seq_len(nrow(ho))) {
    a <- ho$area_code[i]
    want <- .prevcarga_need_days(O + ho$from[i], O + ho$to[i])
    window(a, "holidays", O + ho$from[i], O + ho$to[i])
    if (!is.null(manifest) && identical(manifest$kind, "inference")) {
      have <- if (is.null(inf_win)) want[0L] else want[want >= inf_win$from & want <= inf_win$to]
      gap(a, "holidays", want[!want %in% have])
    }
  }

  if (isTRUE(agg$holiday_annex)) {
    ann <- if (is.null(manifest)) NULL else {
      p <- manifest[["holiday_annex"]]
      if (is.null(p)) p <- manifest$inference[["holiday_annex"]]
      p
    }
    ok <- is.character(ann) && length(ann) == 1L && nzchar(ann) && dir.exists(file.path(ctx$data_dir, ann))
    if (!ok) fnd[[length(fnd) + 1L]] <- .pc_finding("*", "holiday_annex", "need_gap", "", "completude",
                                                    where = "holiday_annex")
  }

  w <- if (length(win)) data.table::rbindlist(win) else
    data.table::data.table(dataset = character(), area = character(), from = lubridate::as_date(character()),
                           to = lubridate::as_date(character()), source = character())
  if (nrow(w)) w <- w[order(w$dataset, w$area, method = "radix")]
  list(findings = .pc_findings_bind(fnd), windows = w)
}

## Cobertura do clima previsto ate O+H (H = maior horizonte do escopo; so folhas). Le com origin = O, como a inferencia
## (weather_issue_limit + emissao mais fresca), e aplica o predicado unico prevcarga_forecast_coverage_end.
.prevcarga_check_forecast_coverage <- function(ctx, cv, leaves, cuts, acc) {
  if (!nrow(cuts) || !length(leaves)) return(NULL)
  O <- cv$forecast_origin; H <- max(cuts$horizon_days); alvo <- O + H
  fc <- .pc_read(prevcarga_read_weather(ctx, "forecast", leaves, from = O, to = alvo, origin = O),
                 "weather_forecast", acc)
  if (is.null(fc)) return(NULL)
  ce <- prevcarga_forecast_coverage_end(fc, O, cv$tz)
  end <- ce$coverage_end[match(leaves, ce$area_code)]
  .pc_findings_bind(lapply(which(is.na(end) | end < alvo), function(i)
    .pc_finding(leaves[i], "weather_forecast", "weather_forecast_coverage",
      if (is.na(end[i])) sprintf("sem clima previsto; alvo %s", format(alvo))
      else sprintf("clima previsto ate %s; alvo %s", format(end[i]), format(alvo)),
      "completude", level = "warning", where = "weather_forecast.valid_time")))
}

## --- Checagens que leem dados (apos cortes) ---------------------------------------------------------------------
.prevcarga_check_data <- function(ctx, cv, areas, models, bad, acc, leaves = character(), inference = FALSE) {
  O <- cv$forecast_origin; tz <- cv$tz
  cuts <- cv$cuts[cv$cuts$model %in% models, ]
  from <- if (nrow(cuts)) .prevcarga_relevant_from(cuts, O, ctx$root) else O - 1L
  if (!length(areas)) return(invisible(NULL))
  if (!"load" %in% bad) {
    load <- .pc_read(prevcarga_read_load(ctx, areas, from = from, to = O - 1L), "load", acc)
    if (!is.null(load)) {
      acc$add(.prevcarga_check_completeness(load, from, O, areas, tz))
      acc$add(.prevcarga_check_load_sanity(load))
      if (nrow(cuts) && !isTRUE(inference)) acc$add(.prevcarga_check_window(load, cuts, O, areas, ctx$root, tz))   # deck inference: check de needs
      acc$add(.prevcarga_check_heuristics(load))
    }
  }
  if (!"weather_observed" %in% bad) {
    wo <- .pc_read(prevcarga_read_weather(ctx, "observed", areas, from, O), "weather_observed", acc)
    acc$add(.prevcarga_check_temperature(wo, "weather_observed"))
    acc$add(.prevcarga_check_temperature_jump(wo, "weather_observed"))
  }
  if (!"weather_forecast" %in% bad) {
    hmax <- if (nrow(cuts)) max(cuts$horizon_days) else 0L
    wf <- .pc_read(prevcarga_read_weather(ctx, "forecast", areas, O, O + hmax), "weather_forecast", acc)
    acc$add(.prevcarga_check_temperature(wf, "weather_forecast"))
    acc$add(.prevcarga_check_temperature_jump(wf, "weather_forecast"))
    raw <- .pc_read(prevcarga_read_weather(ctx, "forecast", areas, O, O), "weather_forecast", acc)
    acc$add(.prevcarga_check_climate_cut(raw, cv$weather_issue_limit, O, tz))
    acc$add(.prevcarga_check_forecast_coverage(ctx, cv, intersect(areas, leaves), cuts, acc))
  }
  invisible(NULL)
}

## --- 9. Orquestrador publico ---------------------------------------------------------------------------------------
## scope = list(model = NULL, area = NULL); origin: Date ou NULL (cai em ctx$origin e, depois, no ultimo dia de carga).
prevcarga_validate_deck <- function(ctx, origin = NULL, scope = NULL, data_lag_days = NULL) {
  prevcarga_require_deck(ctx)
  acc <- .pc_acc()
  scope <- list(model = scope$model, area = scope$area)
  if (is.character(origin)) origin <- prevcarga_parse_origin(origin)
  org <- if (!is.null(origin)) origin else ctx$origin

  probe <- prevcarga_deck_probe(ctx$data_dir)
  ct <- .prevcarga_check_contract(probe, .prevcarga_deck_spec())
  acc$add(ct$findings)

  man <- .prevcarga_capture(prevcarga_deck_manifest(ctx$data_dir), "manifest", acc)
  acc$add(man$finding)
  lab <- NULL
  if (!is.null(man$value)) {
    lab <- .prevcarga_capture(prevcarga_resolve_load_label(ctx, man$value), "manifest", acc)
    acc$add(lab$finding)
  }
  tz_ok <- !is.null(lab) && !is.null(lab$value)

  sc <- .prevcarga_check_scope(ctx, probe, scope, acc)

  cv <- NULL
  if (tz_ok && !"load" %in% ct$bad) {
    cr <- .prevcarga_capture(prevcarga_resolve_cuts(ctx, origin = org, model = sc$models_cuts, data_lag_days = data_lag_days),
                             "load", acc)
    acc$add(cr$finding)
    cv <- cr$value
  }
  inference <- !is.null(man$value) && identical(man$value$kind, "inference")

  ## deck kind inference: a necessidade AGREGADA dos artefatos substitui o check de janela por history_days; roda ANTES das
  ## checagens de dado para o need_gap (a causa) vir antes do day_incomplete do mesmo buraco no primeiro erro da classe
  inf_windows <- NULL
  nl <- intersect(sc$areas, sc$leaves)
  if (inference && !is.null(cv) && length(nl)) {
    rows <- tryCatch(prevcarga_inference_needs_resolve(ctx, cv$forecast_origin, nl, source = "artifacts"),
                     prevcarga_error = function(e) if (grepl("^state\\.", e$id)) NULL else stop(e))   # state.*: o preflight do forecast e dono
    if (!is.null(rows)) {
      chk <- .prevcarga_check_inference_needs(ctx, cv$forecast_origin, cv, prevcarga_inference_needs_aggregate(rows),
                                              cv$tz, man$value)
      acc$add(chk$findings)
      inf_windows <- chk$windows
    }
  }
  if (!is.null(cv)) .prevcarga_check_data(ctx, cv, sc$areas, sc$models, ct$bad, acc, leaves = sc$leaves, inference = inference)

  if (!"holidays" %in% ct$bad && length(sc$areas)) {
    h <- .pc_read(prevcarga_read_holiday_codes(ctx, sc$areas), "holidays", acc)
    acc$add(.prevcarga_check_holiday_codes(h))
  }

  findings <- acc$get()
  c(.prevcarga_verdict(findings),
    list(forecast_origin = if (!is.null(cv)) cv$forecast_origin else if (!is.null(org)) org else NULL,
         origin_source = if (!is.null(cv)) cv$origin_source else if (!is.null(org)) "flag" else NULL,
         available_until = if (!is.null(cv)) cv$available_until else NULL,
         available_until_by_area = if (!is.null(cv)) cv$available_until_by_area else NULL,
         load_label = if (!is.null(cv)) cv$load_label else if (tz_ok) lab$value$value else NULL,
         load_label_source = if (tz_ok) lab$value$source else NULL,
         weather_issue_cutoff = if (!is.null(cv)) cv$weather_issue_cutoff else NULL,
         weather_issue_limit = if (!is.null(cv)) cv$weather_issue_limit else NULL,
         timezone = if (!is.null(man$value)) man$value$timezone else NULL,
         cuts = if (!is.null(cv)) cv$cuts else NULL,
         inference_windows = inf_windows,
         scope = list(areas = sc$areas, models = sc$models),
         findings = findings))
}

## --- Predicado unico de completude do dia (M2.3; o M2.4 estende este arquivo) --------------------------
## O store e flat-48: um dia tem 48 intervalos de 30 min, sem excecao de DST (dia de 46 e buraco).
prevcarga_slots_per_day <- 48L

## `load`: carga do leitor (area_code, interval_start, load_mwh; attr "tz"). Devolve (area_code, date, n_slots,
## complete). Intervalo valido = inicio em :00/:30 com load_mwh nao NA; duplicata conta 1; data pelo
## interval_start no fuso da carga (nunca por interval_end).
prevcarga_day_complete <- function(load) {
  if (!all(c("area_code", "interval_start", "load_mwh") %in% names(load)))
    prevcarga_abort("contract.schema_missing", where = "validation.R")
  tz <- attr(load, "tz")
  if (is.null(tz)) tz <- lubridate::tz(load$interval_start)
  v <- load[lubridate::minute(interval_start) %in% c(0L, 30L) & lubridate::second(interval_start) == 0 &
              !is.na(load_mwh)]
  out <- v[, list(n_slots = data.table::uniqueN(interval_start)),
           by = list(area_code, date = lubridate::as_date(interval_start, tz = tz))]
  out[, complete := n_slots == prevcarga_slots_per_day]
  out[]
}

## --- Predicado unico de cobertura do clima previsto (consumido pelo validate-deck e pelo corte do forecast) ---------
## Ultimo dia civil coberto (os 48 instantes de 30 min, sem tolerancia para o 23:30) em sequencia continua a partir
## de `origin`, por area. `fc`: previsto JA reduzido a emissao usada (prevcarga_read_weather com origin); `tz`: fuso da
## carga (manifest). Puro: nao le disco, nao le env e nao escolhe emissao. NA = o dia `origin` nao esta coberto.
## -> data.table(area_code chr, coverage_end Date | NA)
prevcarga_forecast_coverage_end <- function(fc, origin, tz) {
  origin <- lubridate::as_date(origin)
  if (is.null(fc) || !nrow(fc))
    return(data.table::data.table(area_code = character(), coverage_end = lubridate::as_date(character())))
  v <- fc[lubridate::minute(valid_time) %in% c(0L, 30L) & lubridate::second(valid_time) == 0]
  n <- v[, list(n = data.table::uniqueN(valid_time)), by = list(area_code, d = lubridate::as_date(valid_time, tz = tz))]
  cov <- n[n == prevcarga_slots_per_day]
  areas <- sort(unique(fc$area_code), method = "radix")
  ends <- lapply(areas, function(a) {
    ds <- cov$d[cov$area_code == a]; d <- origin
    if (!d %in% ds) return(lubridate::as_date(NA))
    while ((d + 1L) %in% ds) d <- d + 1L
    d
  })
  data.table::data.table(area_code = areas, coverage_end = do.call(c, ends))
}
