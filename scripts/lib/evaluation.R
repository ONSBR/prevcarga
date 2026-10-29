## evaluation.R -- fonte unica de metrica (M3.1). A UNICA definicao de WMAPE/MAPE/MAE/RMSE/BIAS/pico do repo
## (a referencia tem ~11 WMAPE, ~7 MAPE, 5 BIAS divergentes); consumida por score, reconcile --eval,
## forecast --report (M6) e backtest (M7), para os scorecards nunca discordarem.
##
## Contrato de metrica (WMAPE = 100*sum|e|/sum|y_true| sobre pares finitos; pico = janela fixa 18-22h sobre a
## hora do interval_start) = default de trabalho [TBD: ADR-0028]. ramp/timing/pico-por-regime: DEFERRED (M4/M7).
## Pico difere de proposito do oraculo (carimbo-fim) e fica fora do A/B Delta=0.
##
## Puro: sem relogio, sem env, sem subprocesso. Datas so por lubridate com tz explicito. Requer errors.R e
## schema_long.R carregados (prevcarga_abort, prevcarga_schema_long*); read_actual requer deck_reader.R.
## O deck so e lido por deck_reader.R (leitor unico); LONG em disco so por prevcarga_read_long (leitor unico).

## --- Metricas puras sobre vetores ---------------------------------------------------------------------
## Mascara de pares finitos; comprimentos diferentes sao erro de contrato.
.prevcarga_pairs <- function(y_true, y_pred, what) {
  if (length(y_true) != length(y_pred))
    prevcarga_abort("contract.metric_length", where = what,
                    details = list(n_true = length(y_true), n_pred = length(y_pred)))
  is.finite(y_true) & is.finite(y_pred)
}

.prevcarga_abs_total <- function(x) sum(abs(x))

## weights inerte [DEFERRED]
prevcarga_wmape <- function(y_true, y_pred, weights = NULL) {
  ok <- .prevcarga_pairs(y_true, y_pred, "wmape")
  den <- .prevcarga_abs_total(y_true[ok])
  if (!any(ok) || den == 0) return(NA_real_)
  100 * .prevcarga_abs_total(y_pred[ok] - y_true[ok]) / den
}

## Exclui y_true == 0 (sem floor).
prevcarga_mape <- function(y_true, y_pred) {
  ok <- .prevcarga_pairs(y_true, y_pred, "mape") & y_true != 0
  if (!any(ok)) return(NA_real_)
  100 * mean(abs((y_pred[ok] - y_true[ok]) / y_true[ok]))
}

prevcarga_mae <- function(y_true, y_pred) {
  ok <- .prevcarga_pairs(y_true, y_pred, "mae")
  if (!any(ok)) return(NA_real_)
  mean(abs(y_pred[ok] - y_true[ok]))
}

prevcarga_rmse <- function(y_true, y_pred) {
  ok <- .prevcarga_pairs(y_true, y_pred, "rmse")
  if (!any(ok)) return(NA_real_)
  sqrt(mean((y_pred[ok] - y_true[ok])^2))
}

## bias em MW com sinal (y_pred - y_true); bias_pct = ratio-of-sums.
prevcarga_bias <- function(y_true, y_pred) {
  ok <- .prevcarga_pairs(y_true, y_pred, "bias")
  den <- .prevcarga_abs_total(y_true[ok])
  if (!any(ok)) return(list(bias = NA_real_, bias_pct = NA_real_))
  e <- y_pred[ok] - y_true[ok]
  list(bias = mean(e), bias_pct = if (den == 0) NA_real_ else 100 * sum(e) / den)
}

## --- Ponto unico de agrupamento temporal e pico -------------------------------------------------------
prevcarga_peak_hours <- 18:22  # janela fixa comunicavel [TBD: ADR-0028]

## Todo bucket hora/dia/pico/d_plus usa a coluna `interval_start` anexada pela borda do LONG (schema_long.R).
prevcarga_peak_wmape <- function(interval_start, y_true, y_pred, hours = prevcarga_peak_hours) {
  sel <- lubridate::hour(interval_start) %in% hours
  if (!any(sel)) return(NA_real_)
  prevcarga_wmape(y_true[sel], y_pred[sel])
}

## --- Borda do LONG (ADR-0002/0032) --------------------------------------------------------------------
.prevcarga_long_int <- function(x, col) {
  if (!is.numeric(x) || any(is.finite(x) & x != round(x)) || any(!is.finite(x)))
    prevcarga_abort("contract.long_schema", where = col)
  as.integer(x)
}

## Confere nome+ordem (schema_long.R), tipos, tz e d_plus; devolve o data.frame normalizado
## (forecast_origin continua texto yyyy-MM-dd) com `interval_start` anexado UMA vez pela borda do LONG.
## Idempotente: LONG ja entregue pelo leitor (8 colunas + interval_start no fim) e re-conferido, e a coluna
## tem de bater com a borda.
prevcarga_assert_long <- function(long, ctx) {
  if (!is.data.frame(long)) prevcarga_abort("contract.long_schema", where = "data.frame")
  given <- NULL
  if (identical(names(long), c(names(prevcarga_schema_long), "interval_start"))) {
    given <- long$interval_start
    long <- as.data.frame(long)[names(prevcarga_schema_long)]
  }
  tryCatch(prevcarga_schema_long_check(long),
           prevcarga_error = function(e) {
             if (identical(e$id, "contract.long_schema_constant"))
               prevcarga_abort("contract.long_schema", where = e$where, details = e$details)
             stop(e)
           })
  long <- as.data.frame(long)
  if (!is.character(long$area)) prevcarga_abort("contract.long_schema", where = "area")
  fo <- long$forecast_origin
  if (!is.character(fo) || anyNA(fo) || !all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fo)))
    prevcarga_abort("contract.long_schema", where = "forecast_origin")
  if (!inherits(long$target_datetime, "POSIXct"))
    prevcarga_abort("contract.long_schema", where = "target_datetime")
  for (col in c("y_pred", "y_true")) {
    if (!is.numeric(long[[col]])) prevcarga_abort("contract.long_schema", where = col)
    long[[col]] <- as.double(long[[col]])
  }
  if (anyNA(long$y_pred)) prevcarga_abort("contract.long_schema", where = "y_pred")
  for (col in c("lead", "d_plus", "granularity_min")) long[[col]] <- .prevcarga_long_int(long[[col]], col)
  if (!identical(lubridate::tz(long$target_datetime), ctx$tz))
    prevcarga_abort("contract.long_tz", where = "target_datetime",
                    details = list(found = lubridate::tz(long$target_datetime), expected = ctx$tz))
  o <- lubridate::ymd(fo, tz = ctx$tz, quiet = TRUE)
  if (anyNA(o)) prevcarga_abort("contract.long_schema", where = "forecast_origin")
  long <- prevcarga_long_attach_interval_start(long, ctx$tz)
  if (!is.null(given) && !identical(as.numeric(given), as.numeric(long$interval_start)))
    prevcarga_abort("contract.long_schema", where = "interval_start")
  want <- as.integer(lubridate::as_date(long$interval_start, tz = ctx$tz) - lubridate::as_date(o))
  bad <- which(want != long$d_plus)
  if (length(bad))
    prevcarga_abort("contract.long_dplus_label", where = "d_plus",
                    details = list(row = bad[[1L]], found = long$d_plus[[bad[[1L]]]], expected = want[[bad[[1L]]]]))
  long
}

## LEITOR UNICO de LONG em disco; nao constroi LONG (build_long_output e do M6).
prevcarga_read_long <- function(path, ctx) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path))
    prevcarga_abort("contract.long_schema", where = as.character(path)[1L])
  long <- tryCatch(as.data.frame(arrow::read_parquet(path)),
                   error = function(e) prevcarga_abort("contract.long_schema", where = path))
  if ("forecast_origin" %in% names(long) && !is.character(long$forecast_origin))
    prevcarga_abort("contract.long_schema", where = "forecast_origin")
  if ("forecast_origin" %in% names(long) && anyNA(lubridate::ymd(long$forecast_origin, tz = ctx$tz, quiet = TRUE)))
    prevcarga_abort("contract.long_schema", where = "forecast_origin")
  if ("interval_start" %in% names(long)) prevcarga_abort("contract.long_schema", where = "interval_start")  # so em memoria
  prevcarga_assert_long(long, ctx)
}

## --- Verdade fresca -------------------------------------------------------------------------------------
.prevcarga_eval_tz <- function(ctx) {
  if (!is.null(ctx$tz)) return(ctx$tz)
  prevcarga_deck_manifest(ctx$data_dir)$timezone$load
}

.prevcarga_eval_dt <- function(x, tz) {
  if (inherits(x, "Date")) lubridate::force_tz(lubridate::as_datetime(x), tz) else lubridate::with_tz(x, tz)
}

## Carga realizada ex-post (sem origin/corte: inclusive apos available_until; nunca insumo de modelo) em
## (from, to] por interval_end. Wrapper fino sobre o leitor unico; particao ausente = 0 linhas.
prevcarga_read_actual <- function(ctx, area, from = NULL, to = NULL) {
  tz <- .prevcarga_eval_tz(ctx)
  lo <- if (is.null(from)) NULL else .prevcarga_eval_dt(from, tz)
  hi <- if (is.null(to)) NULL else .prevcarga_eval_dt(to, tz)
  dfrom <- if (is.null(lo)) NULL else lubridate::as_date(lo, tz = tz) - 1
  dto <- if (is.null(hi)) NULL else lubridate::as_date(hi, tz = tz)
  ## o leitor ainda deixa vazar o erro cru do arrow quando falta load_mwh; reclassifica em contrato aqui (so o campo load_mwh ausente; o resto sobe intacto)
  dt <- tryCatch(prevcarga_read_load(ctx, areas = area, from = dfrom, to = dto),
                 error = function(e) {
                   if (inherits(e, "prevcarga_error") ||
                       !grepl("No match for FieldRef.Name(load_mwh)", conditionMessage(e), fixed = TRUE)) stop(e)
                   prevcarga_abort("contract.dataset_missing", where = "load.load_mwh",
                                   details = list(reason = conditionMessage(e)))
                 })
  keep <- rep(TRUE, nrow(dt))
  if (!is.null(lo)) keep <- keep & dt$interval_end > lo
  if (!is.null(hi)) keep <- keep & dt$interval_end <= hi
  dt <- dt[keep]
  out <- data.frame(interval_end = lubridate::with_tz(dt$interval_end, tz), y_true = as.double(dt$load_mwh))
  out <- out[order(out$interval_end), , drop = FALSE]
  rownames(out) <- NULL
  out
}

## --- Regua ---------------------------------------------------------------------------------------------
## Baselines lendo so dias < O (regra = default de trabalho [TBD: ADR-0028]):
##   persistence    = mesmo intervalo de O-1;
##   seasonal_naive = mesmo intervalo e dia da semana do ultimo dia < O (cobre d_plus 0..9).
prevcarga_baseline <- function(ctx, area, origin, kind, long) {
  if (!kind %in% c("persistence", "seasonal_naive"))
    prevcarga_abort("contract.baseline_kind", where = "--baseline", details = list(kind = kind))
  origin <- as.character(origin)
  rows <- long[long$area == area & long$forecast_origin == origin, , drop = FALSE]
  if (!nrow(rows)) return(rows)
  k <- rows$d_plus
  shift <- if (kind == "persistence") k + 1L else 7L * (k %/% 7L + 1L)
  src_end <- rows$target_datetime - lubridate::days(shift)
  o0 <- lubridate::force_tz(lubridate::as_datetime(origin), ctx$tz)  # O 00:00 local
  stopifnot(all(src_end <= o0))
  act <- prevcarga_read_actual(ctx, area, from = min(src_end) - lubridate::days(1), to = o0)
  act <- act[act$interval_end <= o0, , drop = FALSE]  # honestidade e do builder, nao do filtro do leitor
  rows$y_pred <- act$y_true[match(src_end, act$interval_end)]
  rownames(rows) <- NULL
  rows
}

## 1 - WMAPE(modelo)/WMAPE(baseline) nos pares com chave comum e valores finitos. Pura.
prevcarga_skill_score <- function(long, baseline_long) {
  key <- function(d) paste(d$area, d$forecast_origin, as.numeric(d$target_datetime), d$lead, sep = "|")
  i <- match(key(long), key(baseline_long))
  base <- baseline_long$y_pred[i]
  ok <- !is.na(i) & is.finite(long$y_true) & is.finite(long$y_pred) & is.finite(base)
  if (!any(ok)) return(NA_real_)
  wb <- prevcarga_wmape(long$y_true[ok], base[ok])
  if (is.na(wb) || wb == 0) return(NA_real_)
  1 - prevcarga_wmape(long$y_true[ok], long$y_pred[ok]) / wb
}

## Motor unico de score, reconcile --eval, forecast --report e backtest; rollup por subsistema/SIN e do consumidor.
## Exige o `interval_start` do leitor unico (nunca o deriva).
prevcarga_scorecard <- function(long, by = c("d_plus", "area"), baseline_long = NULL) {
  miss <- setdiff(by, names(long))
  if (length(miss)) prevcarga_abort("contract.long_schema", where = paste(miss, collapse = ","))
  if (!"interval_start" %in% names(long)) prevcarga_abort("contract.long_schema", where = "interval_start")
  long <- as.data.frame(long)
  long <- long[is.finite(long$y_true) & is.finite(long$y_pred), , drop = FALSE]
  if (!nrow(long)) {
    out <- data.frame(long[0L, by, drop = FALSE], n = integer(), wmape = double(), mape = double(),
                      mae = double(), rmse = double(), bias = double(), bias_pct = double(), peak = double())
    if (!is.null(baseline_long)) out$skill <- double()
    return(out)
  }
  key <- interaction(long[by], drop = TRUE, lex.order = TRUE)
  rows <- lapply(split(long, key), function(g) {
    b <- prevcarga_bias(g$y_true, g$y_pred)
    out <- data.frame(g[1L, by, drop = FALSE], n = nrow(g),
      wmape = prevcarga_wmape(g$y_true, g$y_pred), mape = prevcarga_mape(g$y_true, g$y_pred),
      mae = prevcarga_mae(g$y_true, g$y_pred), rmse = prevcarga_rmse(g$y_true, g$y_pred),
      bias = b$bias, bias_pct = b$bias_pct,
      peak = prevcarga_peak_wmape(g$interval_start, g$y_true, g$y_pred))
    if (!is.null(baseline_long)) out$skill <- prevcarga_skill_score(g, baseline_long)
    out
  })
  do.call(rbind, c(unname(rows), list(make.row.names = FALSE)))
}

## --- Score (review D+1; M3.3) --------------------------------------------------------------------------
## Orquestra o verbo `score`: escopo de areas (topologia), re-join UNIFORME da verdade fresca por igualdade
## exata target_datetime == interval_end, fan-out do baseline e scorecard pelo motor unico. Nenhuma
## formatacao aqui (render e do verbo). Requer topology.R, deck_reader.R, provenance.R carregados.
.prevcarga_score_topo <- function(ctx) {
  if (!is.null(ctx$topology_path)) prevcarga_topology(path = ctx$topology_path, root = ctx$root)
  else prevcarga_topology(root = ctx$root)
}

## Areas pontuaveis = folhas + subsistemas (sem nos de perda). NULL/"SIN" = todas as presentes no LONG, na
## ordem da topologia; fora da topologia => topology.area_undeclared (antes de qualquer leitura do deck).
.prevcarga_score_areas <- function(long, area, topo) {
  scorable <- unique(c(topo$order, topo$leaves))
  if (is.null(area) || identical(area, "SIN")) return(scorable[scorable %in% long$area])
  if (!(is.character(area) && length(area) == 1L && area %in% scorable))
    prevcarga_abort("topology.area_undeclared", where = "--area")
  area
}

## y_true embutido nunca sobrevive: NA e re-junto por igualdade exata com interval_end (mesmo tzone).
.prevcarga_rejoin_fresh <- function(long, ctx, areas) {
  long <- long[long$area %in% areas, , drop = FALSE]
  long$y_true <- rep(NA_real_, nrow(long))
  for (a in unique(long$area)) {
    i <- which(long$area == a)
    act <- prevcarga_read_actual(ctx, a, from = min(long$target_datetime[i]) - lubridate::days(1),
                                 to = max(long$target_datetime[i]))
    long$y_true[i] <- act$y_true[match(long$target_datetime[i], act$interval_end)]
  }
  rownames(long) <- NULL
  long
}

prevcarga_score <- function(long, ctx, area = NULL, baseline_kind = NULL, origin = NULL) {
  if (!is.null(baseline_kind) && !(is.character(baseline_kind) && length(baseline_kind) == 1L &&
                                   baseline_kind %in% c("persistence", "seasonal_naive")))
    prevcarga_abort("contract.baseline_kind", where = "--baseline", details = list(kind = baseline_kind))
  areas <- .prevcarga_score_areas(long, area, .prevcarga_score_topo(ctx))
  if (!is.null(origin)) long <- long[long$forecast_origin == as.character(origin), , drop = FALSE]
  fresh <- .prevcarga_rejoin_fresh(long, ctx, areas)
  base <- NULL
  if (!is.null(baseline_kind) && nrow(fresh)) {
    parts <- list()
    for (a in unique(fresh$area)) for (o in unique(fresh$forecast_origin[fresh$area == a]))
      parts[[length(parts) + 1L]] <- prevcarga_baseline(ctx, a, o, baseline_kind, fresh)
    base <- do.call(rbind, parts)
  }
  sc <- prevcarga_scorecard(fresh, by = c("area", "d_plus"), baseline_long = base)
  origins <- sort(unique(fresh$forecast_origin))
  tz <- .prevcarga_eval_tz(ctx)
  au <- if (length(origins)) {
    o <- lubridate::ymd(origins[[length(origins)]])
    ## sem nada a pontuar, deck sem carga na origem nao e erro: o aviso nothing_to_score ja diz o que houve
    tryCatch(attr(prevcarga_read_load(ctx, origin = o, from = o, to = o), "available_until"),
             prevcarga_error = function(e) {
               if (!nrow(sc) && identical(e$id, "completude.no_load")) lubridate::ymd_hms(character(), tz = tz) else stop(e)
             })
  } else lubridate::ymd_hms(character(), tz = tz)
  man <- prevcarga_deck_manifest(ctx$data_dir)
  list(scorecard = sc,
       headline = list(d_plus = 1L, rows = sc[sc$d_plus == 1L, , drop = FALSE]),
       origins = lubridate::ymd(origins),
       available_until = au,
       data_hash = prevcarga_data_hash(ctx$data_dir, man, prevcarga_resolve_load_label(ctx, man)$value),
       baseline = if (is.null(baseline_kind)) NULL else list(kind = baseline_kind),
       long = fresh, baseline_long = base,
       warnings = if (nrow(sc)) list() else
         list(list(type = "nothing_to_score", message = "nenhum intervalo com previsao e carga realizada")))
}

## --- Coerencia hierarquica (check-coherence; M3.4) -------------------------------------------------------------
## Gate duro de entrega sobre um LONG reconciliado. PURO: a topologia chega como DADO (prevcarga_topology), nada e
## lido do disco e nenhum nome de area, faixa de k-hat ou lista literal mora aqui (so a tolerancia default, ADR-0008).
## Acumula TODOS os findings por instante (forecast_origin, target_datetime): (e) alinhamento -> (d) escopo ->
## (c) completude -> (a) soma do SIN / (b) k-hat. Classe/exit de cada finding vem do catalogo (errors/evaluation.tsv);
## a classe-dona do veredito e prevcarga_exit_precedence() do M1.4b, nunca reimplementada. [TBD: ADR-0028]
.prevcarga_coh_sin <- "SIN"  # raiz da hierarquia (nao e area de topologia.yaml)

.prevcarga_coh_cols <- c("check", "area", "forecast_origin", "target_datetime", "value", "expected", "class", "id", "detail")

## 1 linha no schema de findings; `class` sai do catalogo pelo `id`. forecast_origin = texto yyyy-MM-dd ou NA.
.prevcarga_coh_finding <- function(check, area, forecast_origin = NA, target_datetime = NA, value = NA_real_,
                                   expected = NA_real_, id, detail = "") {
  data.frame(check = check, area = area,
             forecast_origin = lubridate::ymd(as.character(forecast_origin), tz = "UTC", quiet = TRUE),
             target_datetime = lubridate::as_datetime(as.numeric(target_datetime)),
             value = as.double(value), expected = as.double(expected),
             class = prevcarga_error_lookup(id)$class, id = id, detail = as.character(detail),
             stringsAsFactors = FALSE)
}

.prevcarga_coh_empty <- function(tz = "UTC") {
  out <- .prevcarga_coh_finding("x", "x", id = "coherence.sin_sum")[0L, , drop = FALSE]
  out$target_datetime <- lubridate::with_tz(out$target_datetime, tz)
  rownames(out) <- NULL
  out
}

## Tudo derivado do objeto recebido de prevcarga_topology(): subsistemas na ordem declarada, folhas por subsistema,
## nos de perda (FORA do set emitivel, ADR-0015), declared/emitible, parent_of e a faixa de k-hat efetiva.
.prevcarga_coh_nodes <- function(topo) {
  subs <- as.character(topo$order)
  leaves_of <- lapply(stats::setNames(subs, subs), function(s) as.character(topo$subsystems[[s]]$areas))
  loss <- unname(vapply(subs, function(s) as.character(topo$subsystems[[s]]$loss), ""))
  leaves <- unlist(leaves_of, use.names = FALSE)
  parent_of <- c(stats::setNames(rep(.prevcarga_coh_sin, length(subs)), subs),
                 stats::setNames(rep(subs, lengths(leaves_of)), leaves))
  list(sin = .prevcarga_coh_sin, subsystems = subs, leaves_of = leaves_of, leaves = leaves, loss = loss,
       declared = c(.prevcarga_coh_sin, subs, leaves, loss), emitible = c(.prevcarga_coh_sin, subs, leaves),
       parent_of = parent_of,
       khat_range = lapply(stats::setNames(subs, subs), function(s) as.numeric(topo$subsystems[[s]]$khat_range)))
}

.prevcarga_coh_bind <- function(parts, tz) {
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(.prevcarga_coh_empty(tz))
  out <- do.call(rbind, parts)
  out$target_datetime <- lubridate::with_tz(out$target_datetime, tz)
  rownames(out) <- NULL
  out
}

## (e) Sentinela de alinhamento: grade de target_datetime de um filho deslocada em relacao a do pai (cada lado tem
## instantes que o outro nao tem; ausencia pura NAO e deslocamento). Subsistemas contra o SIN primeiro; folhas
## contra um subsistema ja reprovado nao repetem o finding. Devolve list(findings, origins) das origens quebradas.
.prevcarga_coh_alignment <- function(long, nodes) {
  out <- list(); broken <- character()
  for (o in sort(unique(long$forecast_origin), method = "radix")) {
    lo <- long[long$forecast_origin == o, , drop = FALSE]
    g <- split(as.numeric(lo$target_datetime), lo$area)
    shifted <- function(child) {
      par <- nodes$parent_of[[child]]
      if (is.null(g[[child]]) || is.null(g[[par]])) return(FALSE)
      length(setdiff(g[[child]], g[[par]])) > 0L && length(setdiff(g[[par]], g[[child]])) > 0L
    }
    flagged <- character()
    for (child in c(nodes$subsystems, nodes$leaves)) {
      par <- nodes$parent_of[[child]]
      if (par %in% flagged || !shifted(child)) next
      flagged <- c(flagged, child)
      d <- sort(c(setdiff(g[[child]], g[[par]]), setdiff(g[[par]], g[[child]])))[[1L]]
      out[[length(out) + 1L]] <- .prevcarga_coh_finding("alignment", child, o, NA, id = "coherence.alignment",
        detail = sprintf("grade de %s difere da de %s; 1o instante divergente %s", child, par,
                         format(lubridate::as_datetime(d), "%Y-%m-%d %H:%M:%S", tz = "UTC")))
    }
    if (length(flagged)) broken <- c(broken, o)
  }
  list(findings = out, origins = broken)
}

## (d) Area do LONG fora da topologia: uma linha por area, nao por instante.
.prevcarga_coh_topology <- function(long, nodes) {
  unk <- sort(setdiff(unique(long$area), nodes$declared), method = "radix")
  lapply(unk, function(a) .prevcarga_coh_finding("topology", a, id = "topology.area_undeclared",
                                                 detail = sprintf("%d linhas", sum(long$area == a))))
}

## (c) Nos exigidos e ausentes no instante (cobertura-comum, ADR-0015): SIN => todos os subsistemas; todos os
## subsistemas => SIN; subsistema => todas as suas folhas; todas as folhas => o subsistema. Cobertura parcial
## legitima (folha faltando com subsistema e SIN ausentes) nao dispara regra.
.prevcarga_coh_missing_at <- function(pres, nodes) {
  subs <- nodes$subsystems
  need <- character()
  if (nodes$sin %in% pres) need <- c(need, subs)
  if (all(subs %in% pres)) need <- c(need, nodes$sin)
  for (s in subs) {
    lv <- nodes$leaves_of[[s]]
    if (s %in% pres) need <- c(need, lv)
    if (all(lv %in% pres)) need <- c(need, s)
  }
  setdiff(unique(need), pres)
}

## (a)/(b) por instante, com os nos presentes. v = y_pred nomeado por area.
.prevcarga_coh_identities_at <- function(v, nodes, tol) {
  f <- list()
  subs <- nodes$subsystems
  if (all(c(nodes$sin, subs) %in% names(v))) {
    d <- v[[nodes$sin]] - sum(v[subs])
    if (!is.finite(d) || abs(d) > tol)
      f[[length(f) + 1L]] <- list(check = "sin_sum", area = nodes$sin, id = "coherence.sin_sum",
        value = v[[nodes$sin]], expected = sum(v[subs]),
        detail = if (is.finite(d)) sprintf("|d| = %s > %s", format(abs(d), digits = 7), format(tol)) else "nao finito")
  }
  for (s in intersect(subs, names(v))) {
    lv <- nodes$leaves_of[[s]]
    if (!all(lv %in% names(v))) next
    den <- sum(v[lv]); r <- nodes$khat_range[[s]]
    if (!is.finite(den) || den <= 0 || !is.finite(v[[s]])) {
      f[[length(f) + 1L]] <- list(check = "khat", area = s, id = "coherence.khat", value = NA_real_,
                                  expected = NA_real_, detail = "indefinido")
      next
    }
    k <- v[[s]] / den
    if (k < r[[1L]] - 1e-12 || k > r[[2L]] + 1e-12)
      f[[length(f) + 1L]] <- list(check = "khat", area = s, id = "coherence.khat", value = k,
        expected = if (k < r[[1L]]) r[[1L]] else r[[2L]],
        detail = sprintf("k fora da faixa [%s, %s]", format(r[[1L]]), format(r[[2L]])))
  }
  f
}

## Orquestrador puro. `ctx` so fornece o fuso a conferir (default: o do proprio LONG). Devolve
## list(ok, class, id, findings); `class` = classe-dona pela precedencia unica do M1.4b ("ok" sem findings).
prevcarga_check_coherence <- function(long, topology, tol = 1e-5, ctx = NULL) {
  if (is.data.frame(long) && inherits(long$target_datetime, "POSIXct") && is.null(ctx))
    ctx <- list(tz = lubridate::tz(long$target_datetime))
  long <- prevcarga_assert_long(long, ctx)
  tz <- ctx$tz
  nodes <- .prevcarga_coh_nodes(topology)
  tnum <- as.numeric(long$target_datetime)
  ikey <- paste(long$forecast_origin, tnum, sep = "|")
  if (anyDuplicated(paste(ikey, long$area, sep = "|")))
    prevcarga_abort("contract.long_schema", where = "area,forecast_origin,target_datetime",
                    details = list(reason = "linha duplicada por (instante, area)"))
  al <- .prevcarga_coh_alignment(long, nodes)
  parts <- c(al$findings, .prevcarga_coh_topology(long, nodes))
  keep <- !(long$forecast_origin %in% al$origins) & long$area %in% nodes$emitible
  for (i in split(which(keep), ikey[keep])) {
    o <- long$forecast_origin[[i[[1L]]]]; t <- long$target_datetime[[i[[1L]]]]
    pres <- long$area[i]
    for (a in .prevcarga_coh_missing_at(pres, nodes))
      parts[[length(parts) + 1L]] <- .prevcarga_coh_finding("completude", a, o, t, id = "completude.node_missing",
                                                           detail = "no exigido pela cobertura comum ausente")
    for (x in .prevcarga_coh_identities_at(stats::setNames(long$y_pred[i], pres), nodes, tol))
      parts[[length(parts) + 1L]] <- .prevcarga_coh_finding(x$check, x$area, o, t, x$value, x$expected, x$id, x$detail)
  }
  f <- .prevcarga_coh_bind(parts, tz)
  if (nrow(f)) {
    f <- f[order(f$check, f$forecast_origin, f$target_datetime, f$area, method = "radix"), , drop = FALSE]
    rownames(f) <- NULL
  }
  if (!nrow(f)) return(list(ok = TRUE, class = "ok", id = NA_character_, findings = f))
  owner <- prevcarga_exit_precedence(f$class)
  list(ok = FALSE, class = owner, id = f$id[f$class == owner][[1L]], findings = f)
}
