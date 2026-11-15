## test-rd-state.R -- E4-T3: estado derivado no artefato do dessem-regdin (ADR-045 fase C). Loader real, motor real, despachante real,
## fixture sintetica rd3_panel (14 meses, sem RNG). O artefato MED e o do cache de sessao (rd3_state); os cenarios de feriados
## consecutivos treinam um artefato proprio (~1 min cada, cache de sessao). O deck "curto" e o painel de infer de 15 dias.

rs_O <- function() RD2_O

## espiao de contagem em funcoes do env do motor (restaura no fim)
rs_count_spy <- function(eng, fns) {
  n <- new.env(); n$k <- 0L
  orig <- lapply(stats::setNames(fns, fns), function(f) get(f, envir = eng, inherits = FALSE))
  lk <- vapply(fns, function(f) bindingIsLocked(f, eng), NA)
  for (i in seq_along(fns)) {
    if (lk[[i]]) unlockBinding(fns[[i]], eng)
    local({ f <- fns[[i]]; o <- orig[[f]]; assign(f, function(...) { n$k <- n$k + 1L; o(...) }, envir = eng) })
  }
  list(count = function() n$k,
       restore = function() for (i in seq_along(fns)) {
         assign(fns[[i]], orig[[fns[[i]]]], envir = eng); if (lk[[i]]) lockBinding(fns[[i]], eng)
       })
}

## conta os ajustes lm do perfil de feriado (formula com LAGDS_): 0 = coef congelado; 1 = refit sobre rbind(amostra, horizonte)
rs_lm_calls <- function(code) {
  n <- new.env(); n$k <- 0L
  tr <- bquote(if (any(grepl("LAGDS", deparse(formula)))) assign("k", get("k", envir = .(n)) + 1L, envir = .(n)))
  suppressMessages(trace("lm", tr, where = asNamespace("stats"), print = FALSE))
  on.exit(suppressMessages(untrace("lm", where = asNamespace("stats"))), add = TRUE)
  val <- force(code)
  list(value = val, n = n$k)
}

## painel de feriados proprio: tres semanas passadas (O-36, O-26, O-20), fora dos 15 dias do deck curto, com os codigos 3, 4, 5
## em dias consecutivos (amostra do perfil de feriado) + os feriados do horizonte `horizon` (nomes = dia relativo a O, valor = codigo)
rs_panel <- function(horizon, until = NULL, days = RD2_D) {
  pan <- rd3_panel(until = until, days = days)
  cod <- c(3L, 4L, 5L)
  hist <- data.table::rbindlist(lapply(c(36L, 26L, 20L), function(w) data.table::data.table(off = -w + 0:2, code = cod)))
  h <- rbind(hist, data.table::data.table(off = as.integer(names(horizon)), code = as.integer(horizon)))
  pan$holidays <- data.table::data.table(area_code = "SE", date = RD2_O + h$off, holiday_type_id = 1L,
                                         prevcarga_code = h$code, simplified_code = 3L)
  if (days < RD2_D) pan$holidays <- pan$holidays[date >= RD2_O - days]            # o calendario do deck curto: O-15..O+H
  pan
}
rs_horizon <- list(single = c(`2` = 3L), dbl = c(`2` = 3L, `3` = 3L), trio = c(`2` = 3L, `3` = 4L, `4` = 5L))
rs_art <- function(nm) rd2_cached(paste("rs-art", nm), {
  p <- rd2_plugin()
  rd_with_cli(suppressMessages(rd_lib$prevcarga_plugin_train(p, rs_panel(rs_horizon[[nm]]), rd3_cuts(rd3_au(9L)),
                                                            rd2_spec("MED"), ctx = rd2_ctx())))
})

## ingestao + features de infer ate o estagio do plugin (sem o despachante); `last_day` = ultimo dia historico do deck
rs_stage <- function(p, art, pan, au, last_day = NULL) {
  cu <- rd3_cuts(au)
  if (!is.null(last_day)) {
    cu$last_data_day <- last_day
    pan$load <- pan$load[lubridate::as_date(interval_end - 1) <= last_day | lubridate::as_date(interval_start) >= RD2_O]
    pan$weather_observed <- pan$weather_observed[lubridate::as_date(valid_time) <= last_day | lubridate::as_date(valid_time) >= RD2_O]
  }
  pan$weather_forecast <- pan$weather_forecast[issue_time <= au]
  m <- rd_model_panel(pan, cu)
  frame <- rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  list(frame = frame, cuts = m$cuts,
       inf = rd_with_cli(suppressMessages(p$descriptor$stages$inference_features$fn(frame, art$payload))))
}

test_that("[PLUGINS] inferencia: estado derivado regdin ac-0: o treino grava serie_dm, hol_profile, columns e inference_needs", {
  st <- rd3_state(); p <- st$p; e <- p$env; pl <- st$med$payload
  O <- rs_O(); sp <- format(O + 2L)
  expect_true(all(c("serie_dm", "hol_profile", "columns", "inference_needs") %in% names(pl)))
  s <- pl$serie_dm
  expect_identical(class(s), "data.frame")                                   # puro: sem data.table nem selfref
  expect_null(attr(s, ".internal.selfref")); expect_s3_class(s$Data, "Date")
  expect_true(all(!s$Prev))                                                  # so linhas historicas
  expect_identical(c(min(s$Data), max(s$Data)), c(pl$train_window$start, pl$train_window$end))
  expect_true(all(sprintf("mm_h%02d", 0:23) %in% names(s)))
  expect_false(anyNA(s$Carga))
  ## mm cru por hora de corte == o mm que a inferencia calcularia sobre o deck longo (rd_morning_means), nos dias em comum
  au <- rd3_au(9L)
  f <- rs_stage(p, st$med, rd3_panel(until = au), au)$frame
  mm9 <- e$.rd_mm_core(e$.rd_deck_panel(f$hourly_load), 9L, RD_TZ)
  both <- merge(data.table::as.data.table(s)[, .(Data, mm_h09)], mm9, by = "Data")
  expect_gt(nrow(both), 300L); expect_identical(both$mm_h09, both$mm)
  ## hol_profile: uma entrada por dia especial do horizonte, com os campos do motor
  expect_identical(names(pl$hol_profile), sp)
  hp <- pl$hol_profile[[sp]]
  expect_true(all(c("sd_hist", "sample", "coef", "xlevels", "formula", "diaAno", "HV", "dslag", "diaEsp") %in% names(hp)))
  expect_identical(hp$diaEsp, 3L); expect_true(is.numeric(hp$sd_hist) && hp$sd_hist > 0)
  expect_identical(class(hp$sample), "data.frame"); expect_true(all(hp$sample$Data < O)); expect_false(anyNA(hp$sample$Carga))
  expect_false("DataHora" %in% names(hp$sample))                             # relogio fora do artefato (scan_pure)
  expect_true(is.character(hp$formula) && length(hp$formula) == 1L)
  expect_true(is.numeric(hp$coef) && !is.null(names(hp$coef)))
  ## columns: perfil de cada dia NORMAL x 24 horas; o dia especial nao tem SVM
  expect_identical(names(pl$columns), pl$dias); expect_null(pl$columns[[sp]])
  for (dp in setdiff(pl$dias, sp)) {
    expect_length(pl$columns[[dp]], 24L)
    for (h in 1:24) expect_named(pl$columns[[dp]][[h]], c("vars", "levels"))
  }
  ## inference_needs: 15 dias derivados de lagsPerfil (nunca literal), sem anexo, forma normalizada da fundacao
  n <- ceiling(max(pl$config_mdl$lagsPerfil) / 24) + 1
  expect_identical(pl$inference_needs$load$days_before, as.integer(n)); expect_identical(n, 15)
  expect_identical(pl$inference_needs$weather_observed$days_before, 15L)
  expect_identical(pl$inference_needs$weather_forecast$segments[[1L]]$to, 9L)
  expect_identical(pl$inference_needs$holidays, list(from = -15L, to = 9L))
  expect_false(pl$inference_needs$holiday_annex)
  sys.source(file.path(rd_root(), "scripts", "lib", "inference_needs.R"), envir = rd_lib)
  expect_identical(rd_lib$prevcarga_inference_needs_normalize(pl$inference_needs, "dessem-regdin"), pl$inference_needs)
})

test_that("[PLUGINS] artefato_puro: estado derivado regdin ac-1: scan_pure passa; thaw devolve data.table; digest de serie_dm == dm_digest", {
  st <- rd3_state(); e <- st$p$env; pl <- st$med$payload
  fz <- e$rd_freeze(pl)
  expect_error(rd_with_cli(rd_lib$prevcarga_plugin_scan_pure(fz)), NA)
  expect_identical(class(fz$serie_dm), "data.frame")
  th <- e$rd_thaw(fz)
  expect_true(data.table::is.data.table(th$serie_dm)); expect_s3_class(th$serie_dm$Data, "Date")
  expect_identical(e$.rd_dm_hist_digest(th$serie_dm), pl$arima_fingerprint$dm_digest)
  expect_identical(as.data.frame(th$serie_dm), as.data.frame(pl$serie_dm))
  expect_identical(th$columns, pl$columns)
  expect_identical(as.data.frame(e$rd_thaw(th)$serie_dm), as.data.frame(pl$serie_dm))   # idempotente
  ## CONTROL: forma errada => integrity, exit 9, nomeando o campo
  bad <- fz; bad$serie_dm$mm_h05 <- NULL
  cond <- expect_rd_error(e$rd_thaw(bad), "plugin.regdin_artifact_tampered", "mm_h05")
  expect_identical(cond$exit_code, 9L); expect_identical(cond$class_name, "integrity")
  b2 <- fz; b2$columns[[1L]] <- b2$columns[[1L]][1:23]
  expect_rd_error(e$rd_thaw(b2), "plugin.regdin_artifact_tampered", "columns")
  b3 <- fz; b3$hol_profile[[1L]]$coef <- NULL
  expect_rd_error(e$rd_thaw(b3), "plugin.regdin_artifact_tampered", "coef")
  b4 <- fz; b4$hol_profile <- list()
  expect_rd_error(e$rd_thaw(b4), "plugin.regdin_artifact_tampered", "hol_profile")
})

test_that("[PLUGINS] inferencia: estado derivado regdin ac-2/ac-3: deck de 15 dias == deck longo (09:00 e 00:00) sem bypass; ARIMA refeito bate; 0 getNormaliza*", {
  st <- rd3_state(); p <- st$p; eng <- st$eng
  for (h in c(9L, 0L)) {
    au <- rd3_au(h)
    long <- rd3_infer(p, st$med, au)
    fp <- rd_spy(".rd_check_fingerprint", p$env); norm <- rs_count_spy(eng, c("getNormalizaCarga", "getNormalizaExogena"))
    short <- rd3_infer(p, st$med, au, pan = rd3_panel(until = au, days = 15L))
    n_norm <- norm$count(); norm$restore()
    expect_identical(fp$count(), 1L)                                         # o ARIMA foi refeito e conferido (tolerancia 0)
    expect_identical(n_norm, 0L)                                             # a normalizacao e a do artefato
    expect_identical(short$values, long$values)
    expect_identical(short$delivery$y_pred, long$delivery$y_pred); expect_identical(short$delivery$interval_end, long$delivery$interval_end)
    expect_identical(short$meta$head_used, h == 9L); expect_identical(long$meta$head_used, h == 9L)
    expect_identical(dim(short$values), c(9L, 48L)); expect_false(anyNA(short$values))
  }
  ## CONTROL com dentes: o mesmo estagio de features SEM o norm do artefato chama getNormaliza* (o espiao enxerga)
  au <- rd3_au(9L); sp <- rs_count_spy(eng, c("getNormalizaCarga", "getNormalizaExogena"))
  m <- rd_model_panel(rd3_panel(until = au, days = 15L), rd3_cuts(au))
  deck <- rd_with_cli(suppressMessages(p$env$plugin_ingest(m$panel, m$cuts, NULL)))
  rd_with_cli(suppressMessages(p$env$plugin_build_features(deck, rd2_spec("MED"))))
  n_ctl <- sp$count(); sp$restore()
  expect_gt(n_ctl, 0L)
  ## CONTROL: a serie vem do artefato; alterar a serie_dm gravada muda a saida do deck curto (com a guarda desligada)
  s1 <- rs_stage(p, st$med, rd3_panel(until = au, days = 15L), au)
  alt <- st$med$payload; alt$serie_dm$Carga[nrow(alt$serie_dm)] <- alt$serie_dm$Carga[nrow(alt$serie_dm)] * 1.3
  s2 <- rs_stage(p, list(payload = alt), rd3_panel(until = au, days = 15L), au)
  y1 <- rd_with_cli(suppressMessages(p$env$plugin_infer(st$med$payload, s1$inf, s1$cuts)))
  y2 <- rd_with_cli(suppressMessages(p$env$plugin_infer(alt, s2$inf, s2$cuts, .bypass_window_guard = TRUE)))
  expect_gt(max(abs(y1$values - y2$values)), 1e-5)
})

## deck curto == longo num cenario de feriados no horizonte; devolve os envelopes e a contagem de refit do lm
rs_run <- function(nm, au, ctrl = NULL) {
  p <- rd2_plugin(); art <- rs_art(nm); hz <- rs_horizon[[nm]]
  long <- rd3_infer(p, art, au, pan = rs_panel(hz, au))
  run <- function() rd3_infer(p, art, au, pan = rs_panel(hz, au, 15L))
  r <- rs_lm_calls(if (is.null(ctrl)) run() else rd2_with_plugin_fn("rd_profile_holiday", ctrl, run()))
  list(long = long, short = r$value, lm = r$n)
}

test_that("[PLUGINS] inferencia: estado derivado regdin ac-4: feriado unico no horizonte (O+2, codigo 3, amostras passadas so fora dos 15 dias): deck curto == longo", {
  O <- rs_O(); pan15 <- rs_panel(rs_horizon$single, rd3_au(9L), 15L)
  expect_identical(sum(pan15$holidays$prevcarga_code == 3L & pan15$holidays$date < O), 0L)   # nenhuma amostra do codigo dentro do deck curto
  expect_true((O + 2L) %in% pan15$holidays$date)
  for (h in c(9L, 0L)) {
    r <- rs_run("single", rd3_au(h))
    expect_identical(r$short$values, r$long$values)
    expect_identical(r$short$delivery$y_pred, r$long$delivery$y_pred)
    expect_identical(r$lm, 0L)                                               # coef congelado, sem refit
  }
  expect_identical(nrow(rs_art("single")$payload$hol_profile[[format(O + 2L)]]$sample), 72L)   # 3 dias x 24 h, todos fora do deck curto
})

test_that("[PLUGINS] inferencia: estado derivado regdin ac-4: dois feriados consecutivos do mesmo codigo: 0 lm no primeiro, 1 refit no segundo; deck curto == longo; CONTROL sem refit difere", {
  for (h in c(9L, 0L)) {
    r <- rs_run("dbl", rd3_au(h))
    expect_identical(r$short$values, r$long$values)                          # os dois dias de feriado inclusive
    expect_identical(r$short$delivery$y_pred, r$long$delivery$y_pred)
    expect_identical(r$lm, 1L)                                               # 0 no primeiro feriado (coef congelado), 1 no segundo
  }
  ## CONTROL: o segundo feriado sem o write-back do primeiro na amostra (O no futuro => nenhuma linha elegivel) difere
  noref <- function(real) function(dp, seriePF, cfg, constNorm, hp, O) real(dp, seriePF, cfg, constNorm, hp, lubridate::ymd("2100-01-01"))
  ctl <- rs_run("dbl", rd3_au(9L), noref)
  expect_identical(ctl$lm, 0L)
  expect_false(identical(ctl$short$values, ctl$long$values))
  d2 <- 4L                                                                   # O+3: linha 4 da matriz (O = 1)
  expect_gt(max(abs(ctl$short$values[d2, ] - ctl$long$values[d2, ])), 1e-3)
})

test_that("[PLUGINS] inferencia: estado derivado regdin ac-4: tres dias especiais consecutivos de codigos diferentes (formato Carnaval, O+2..O+4): deck curto == longo", {
  for (h in c(9L, 0L)) {
    r <- rs_run("trio", rd3_au(h))
    expect_identical(r$short$values, r$long$values)
    expect_identical(r$short$delivery$y_pred, r$long$delivery$y_pred)
    expect_identical(r$lm, 0L)                                               # codigos distintos: nenhum feriado anterior entra na amostra
    expect_identical(dim(r$short$values), c(9L, 48L)); expect_false(anyNA(r$short$values))
  }
  art <- rs_art("trio")
  expect_identical(names(art$payload$hol_profile), format(rs_O() + 2:4))
  expect_identical(unname(vapply(art$payload$hol_profile, function(x) x$diaEsp, 1L)), 3:5)
})

test_that("[PLUGINS] inferencia: estado derivado regdin rd_profile_holiday == RL_perfilFeriado do motor, em sequencia, nos dois feriados consecutivos", {
  p <- rd2_plugin(); e <- p$env; eng <- parent.env(e); art <- rs_art("dbl"); pl <- art$payload; au <- rd3_au(9L)
  d1 <- format(rs_O() + 2L); d2 <- format(rs_O() + 3L); cfg <- pl$config_mdl; cn <- pl$norm$pf_cargadm
  base <- function(days) {
    pf <- rs_stage(p, art, rs_panel(rs_horizon$dbl, au, days), au)$inf$features$series$seriePF
    pf[Prev == TRUE, CargaMed := 13000 / cn]                                 # a CargaMed do horizonte e a da DM; valor fixo
    pf
  }
  ref <- base(RD2_D)                                                         # deck longo: o motor recomputa o historico
  eng$RL_perfilFeriado(d1, ref, cfg, cn)
  sh <- base(15L)
  e$rd_profile_holiday(d1, sh, cfg, cn, pl$hol_profile[[d1]], rs_O())
  ref1 <- data.table::copy(ref); sh1 <- data.table::copy(sh)                 # estado ANTES do segundo feriado (so o primeiro escrito de volta)
  eng$RL_perfilFeriado(d2, ref, cfg, cn)
  e$rd_profile_holiday(d2, sh, cfg, cn, pl$hol_profile[[d2]], rs_O())
  for (d in c(d1, d2)) {
    expect_identical(sh[Data == d]$Carga, ref[Data == d]$Carga)
    expect_identical(sh[Data == d]$CargaMed, ref[Data == d]$CargaMed)
  }
  expect_false(anyNA(ref[Data == d2]$Carga))
  ## ac-5: as linhas (e a ordem) que o lm usa == as do deck longo: amostra cronologica + horizonte escrito de volta
  entrada <- function(pf, dp, hp) {
    inp <- e$.rd_fer_input(dp, pf, cfg, cn); s <- inp$serie
    eng$padronizaCargaHorariaRef(s)
    e$.rd_drop_clock(eng$entradaPerfilFeriadoReg(s, dslag = hp$dslag, diaAno = hp$diaAno, hv = hp$HV, tpFer = hp$diaEsp))
  }
  hp2 <- pl$hol_profile[[d2]]
  el <- entrada(ref1, d2, hp2); es <- entrada(sh1, d2, hp2)
  usadas_longo <- el[!is.na(Carga)]
  usadas_curto <- rbind(data.table::as.data.table(hp2$sample), es[Data >= rs_O() & Data != lubridate::as_date(d2) & !is.na(Carga)][, names(hp2$sample), with = FALSE])
  expect_identical(e$.rd_plain_df(usadas_curto), e$.rd_plain_df(usadas_longo[, names(hp2$sample), with = FALSE]))
  expect_gt(nrow(usadas_longo), nrow(hp2$sample))                            # o write-back do primeiro feriado entrou
  expect_true(!is.unsorted(usadas_longo$Data))                               # amostra cronologica
})

test_that("[PLUGINS] inferencia: estado derivado regdin ac-6: serie_dm adulterada => tampered (exit 9) antes do ARIMA; deck que termina em O-2 => window_mismatch nomeando os dois dias", {
  st <- rd3_state(); p <- st$p; au <- rd3_au(9L); O <- rs_O(); short <- rd3_panel(until = au, days = 15L)
  ## CONTROL: o artefato original e o deck valido inferem
  expect_true(is.list(rd3_infer(p, st$med, au, pan = short)))
  bad <- st$med; k <- nrow(bad$payload$serie_dm) - 3L
  bad$payload$serie_dm$Carga[k] <- bad$payload$serie_dm$Carga[k] + 1
  spy_fp <- rd_spy(".rd_check_fingerprint", p$env); spy_pass <- rd_spy(".rd_predict_pass", p$env)
  cond <- tryCatch(rd3_infer(p, bad, au, pan = short), prevcarga_error = function(c) c)
  expect_s3_class(cond, "prevcarga_error")
  expect_identical(cond$id, "plugin.regdin_artifact_tampered")
  expect_identical(cond$exit_code, 9L); expect_identical(cond$class_name, "integrity")
  expect_true(rd_ascii(conditionMessage(cond)))
  expect_identical(spy_fp$count(), 0L); expect_identical(spy_pass$count(), 0L)   # antes do ARIMA
  ## o intervalo de serie_dm fora de train_window tambem e adulteracao
  bad2 <- st$med; bad2$payload$train_window$start <- bad2$payload$train_window$start + 1L
  expect_identical(tryCatch(rd3_infer(p, bad2, au, pan = short), prevcarga_error = function(c) c)$id, "plugin.regdin_artifact_tampered")
  ## deck valido que termina em O-2 (last_data_day = O-2): nenhuma previsao
  s <- rs_stage(p, st$med, short, au, last_day = O - 2L)
  cond2 <- tryCatch(rd_with_cli(p$env$plugin_infer(st$med$payload, s$inf, s$cuts)), prevcarga_error = function(c) c)
  expect_s3_class(cond2, "prevcarga_error")
  expect_identical(cond2$id, "plugin.regdin_window_mismatch")
  expect_identical(cond2$exit_code, 5L); expect_identical(cond2$class_name, "window")
  expect_identical(cond2$details$expected_end, format(O - 1L)); expect_identical(cond2$details$deck_end, format(O - 2L))
  expect_match(conditionMessage(cond2), format(O - 1L), fixed = TRUE); expect_match(conditionMessage(cond2), format(O - 2L), fixed = TRUE)
  expect_true(rd_ascii(conditionMessage(cond2)))
  expect_identical(spy_fp$count(), 0L); expect_identical(spy_pass$count(), 0L)
  spy_fp$restore(); spy_pass$restore()
  ## CONTROL com dente: sem a guarda (so teste) o erro deixa de ser o da janela (o motor recusa a grade com buraco em O-1)
  y <- tryCatch(rd_with_cli(suppressMessages(p$env$plugin_infer(st$med$payload, s$inf, s$cuts, .bypass_window_guard = TRUE))),
                prevcarga_error = function(c) c)
  expect_false(inherits(y, "prevcarga_error") && identical(y$id, "plugin.regdin_window_mismatch"))
  expect_false(".bypass_window_guard" %in% names(formals(p$descriptor$stages$inference$fn)))
  expect_false(".bypass_window_guard" %in% names(p$descriptor))
})

test_that("[PLUGINS] inferencia: estado derivado regdin ac-7: payload v0.1.5 recusado com exit 2 nomeando o primeiro campo ausente, antes de TransformaDeck", {
  st <- rd3_state(); p <- st$p; e <- p$env; eng <- st$eng; pl <- st$med$payload; au <- rd3_au(9L)
  m <- rd_model_panel(rd3_panel(until = au, days = 15L), rd3_cuts(au))
  frame <- rd_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  feat <- p$descriptor$stages$inference_features$fn
  td <- rs_count_spy(eng, "TransformaDeck"); on.exit(td$restore(), add = TRUE)
  v015 <- pl[setdiff(names(pl), c("serie_dm", "hol_profile", "columns", "inference_needs"))]
  cond <- expect_rd_error(feat(frame, v015), "plugin.regdin_artifact_mismatch", "serie_dm")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract"); expect_identical(cond$details$field, "serie_dm")
  expect_identical(expect_rd_error(feat(frame, pl[setdiff(names(pl), c("hol_profile", "columns"))]),
                                   "plugin.regdin_artifact_mismatch")$details$field, "hol_profile")
  expect_identical(expect_rd_error(feat(frame, pl[setdiff(names(pl), "columns")]),
                                   "plugin.regdin_artifact_mismatch")$details$field, "columns")
  ## o thaw tambem recusa o v0.1.5 (exit 2), nao o tampered
  fz <- e$rd_freeze(pl)
  cth <- expect_rd_error(e$rd_thaw(fz[setdiff(names(fz), c("serie_dm", "hol_profile", "columns", "inference_needs"))]),
                         "plugin.regdin_artifact_mismatch", "serie_dm")
  expect_identical(cth$exit_code, 2L)
  ## dia especial sem hol_profile
  nohp <- pl; nohp$hol_profile <- list()
  expect_identical(expect_rd_error(feat(frame, nohp), "plugin.regdin_artifact_mismatch")$details$field, "hol_profile")
  expect_identical(td$count(), 0L)
  ## CONTROL: o payload completo segue (TransformaDeck e chamado)
  ok <- rd_with_cli(suppressMessages(feat(frame, pl)))
  expect_gt(td$count(), 0L)
  expect_true(all(c("features", "mm", "variant", "panel_window", "cuts") %in% names(ok)))
})

test_that("[PLUGINS] inferencia: estado derivado regdin ac-8: descriptor, run.yaml e needs: o deck de treino recebe o anexo; o de inferencia, nao", {
  st <- rd3_state(); p <- st$p; pl <- st$med$payload
  expect_false(p$descriptor$history_infer_equals_train)
  expect_identical(p$spec$history_days$infer, 15L)
  expect_false("infer_history_equals_train" %in% names(p$spec$constraints))
  ## o plugin carrega (o loader recusa history_days.infer != train so com a flag TRUE)
  expect_true(is.function(p$descriptor$data_needs))
  ## deck de treino: o descriptor (ano_civil) pede o anexo de feriados
  area <- "SE"
  dn <- p$descriptor$data_needs(p$spec, area)
  expect_true(dn$infer$holiday_annex)
  ## deck de inferencia: o artefato traz os needs do payload, sem anexo
  for (m in c("inference_needs", "training")) sys.source(file.path(rd_root(), "scripts", "lib", paste0(m, ".R")), envir = rd_lib)
  expect_identical(rd_lib$prevcarga_inference_needs_of(p, p$spec, area)$holiday_annex, TRUE)
  cuts <- list(train_origin = rs_O(), last_data_day = rs_O() - 1L, load_label = "end", tz = RD_TZ,
               weather_issue_cutoff = "available_until", origin = rs_O())
  inp <- list(plugin = p, spec = p$spec, cuts = cuts)
  member <- list(sigla = "RGD_MED", mdir = "dessem-regdin", exogena = "MED")
  a <- rd_with_cli(rd_lib$prevcarga_artifact_build(member, area, inp, st$med))
  expect_identical(a$inference_needs, pl$inference_needs)
  expect_false(a$inference_needs$holiday_annex)
  expect_identical(a$inference_needs$load$days_before, 15L)
  agg <- function(n) rd_lib$prevcarga_inference_needs_aggregate(data.table::data.table(needs = list(n), area = area))
  expect_false(agg(a$inference_needs)$holiday_annex)
  expect_true(agg(rd_lib$prevcarga_inference_needs_normalize(dn$infer, "dessem-regdin"))$holiday_annex)
  ## CONTROL: sem o campo no payload, vale o descriptor (anexo TRUE): o caminho generico nao fixa o plugin
  art2 <- st$med; art2$payload$inference_needs <- NULL
  b <- rd_with_cli(rd_lib$prevcarga_artifact_build(member, area, inp, art2))
  expect_true(b$inference_needs$holiday_annex)
})

test_that("[PLUGINS] inferencia: estado derivado regdin norm vazio cruzado com a flag: so a flag ligada falha alto (ADR-0005)", {
  st <- rd3_state(); e <- st$p$env; eng <- st$eng
  cfg <- eng$getConfigModel("RegDin.Radial.TmpMed")
  norm <- st$med$payload$norm
  expect_length(norm$dm_carga, 0L)                                           # flag da DM desligada: constante vazia e o estado normal
  expect_error(e$.rd_check_norm(norm, cfg), NA)
  bad <- utils::modifyList(norm, list(pf_cargadm = logical(0)))              # perfil$cargadm$flag ligada
  cond <- expect_rd_error(e$.rd_check_norm(bad, cfg), "plugin.regdin_artifact_mismatch", "norm")
  expect_identical(cond$details$field, "norm"); expect_true("pf_cargadm" %in% cond$details$constante)
  ## falha antes de TransformaDeck
  td <- rs_count_spy(eng, "TransformaDeck"); on.exit(td$restore(), add = TRUE)
  au <- rd3_au(9L); m <- rd_model_panel(rd3_panel(until = au, days = 15L), rd3_cuts(au))
  deck <- rd_with_cli(suppressMessages(e$plugin_ingest(m$panel, m$cuts, NULL)))
  expect_rd_error(e$plugin_build_features(deck, rd2_spec("MED"), norm = bad), "plugin.regdin_artifact_mismatch", "norm")
  expect_identical(td$count(), 0L)
  ## CONTROL: com a flag de perfil$cargadm desligada no config, a mesma constante vazia passa; sem config nao ha flag para cruzar
  off <- cfg; off$normalizacao$perfil$cargadm$flag <- FALSE
  expect_error(e$.rd_check_norm(bad, off), NA)
  expect_error(e$.rd_check_norm(bad), NA)
})

test_that("[PLUGINS] inferencia: estado derivado regdin rd_align_row: LAG_FER e indicador (nao o codigo), DiaEspecial com os niveis gravados; resto falha alto", {
  e <- rd2_plugin()$env
  d <- lubridate::ymd("2024-12-26")
  serie <- data.table::data.table(Data = d - 3:0, DiaEspecial = c(0L, 3L, 5L, 0L))
  lv <- paste0("F", sprintf("%02d", 0:12))
  cols <- list(vars = c("Carga", "LAG_FER_1", "LAG_FER_2", "DiaEspecial"), levels = list(DiaEspecial = lv))
  row <- data.table::data.table(Carga = 1)
  al <- rd_with_cli(e$rd_align_row(row, cols, serie, d, "teste"))
  expect_identical(names(al), cols$vars)
  expect_identical(al$LAG_FER_1, 1); expect_identical(al$LAG_FER_2, 1)         # codigos 5 e 3 viram 1 (indicador)
  expect_identical(levels(al$DiaEspecial), lv); expect_identical(as.character(al$DiaEspecial), "F00")
  cond <- expect_rd_error(e$rd_align_row(data.table::data.table(Carga = 1, EXTRA = 2), cols, serie, d, "teste"),
                          "plugin.regdin_artifact_mismatch", "EXTRA")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$details$field, "columns")
  expect_rd_error(e$rd_align_row(row, list(vars = c("Carga", "LAG_1")), serie, d, "teste"), "plugin.regdin_artifact_mismatch", "LAG_1")
  expect_rd_error(e$rd_align_row(row, NULL, serie, d, "teste"), "plugin.regdin_artifact_mismatch", "columns")
})
