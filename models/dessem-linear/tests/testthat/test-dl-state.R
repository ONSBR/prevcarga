## test-dl-state.R -- E4-T2: estado derivado no artefato do dessem-linear (ADR-045 fase B). Fixture sintetica dl_fx_store (MED),
## loader real, motor real; espioes em tune_wsvm (dl_spy_tune) e em getNormaliza*/TransformaDeck no env do motor.
## O artefato e treinado UMA vez por sessao (dl_base_art, memoizado); o deck "curto" e o painel de infer de 15 dias.

ds_O <- function() DL8_O
ds_mmc <- function() sprintf("mm_h%02d", 0:23)

## espiao de contagem em funcoes do env do motor (restaura no fim)
ds_count_spy <- function(eng, fns) {
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

## espiao em tune_wsvm que guarda uma copia dos dados e da formula de cada chamada
ds_data_spy <- function(eng) {
  log <- new.env(); log$n <- 0L; log$data <- list(); log$fml <- character()
  real <- get("tune_wsvm", envir = eng, inherits = FALSE)
  unlock <- bindingIsLocked("tune_wsvm", eng)
  if (unlock) unlockBinding("tune_wsvm", eng)
  assign("tune_wsvm", function(formula, ..., data) {
    log$n <- log$n + 1L
    log$data[[log$n]] <- data.table::copy(data)
    log$fml[[log$n]] <- paste(deparse(formula, width.cutoff = 500L), collapse = "")
    real(formula, ..., data = data)
  }, envir = eng)
  list(log = log, restore = function() { assign("tune_wsvm", real, envir = eng); if (unlock) lockBinding("tune_wsvm", eng) })
}

## deck de inferencia (ingestao do plugin) e features de inferencia sobre `panel`
ds_feat <- function(st, cuts, panel, payload) {
  p <- dl_plugin()
  m <- dl_model_panel(panel, cuts)
  frame <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  list(frame = frame,
       inf = dl_with_cli(suppressMessages(p$descriptor$stages$inference_features$fn(frame, payload))))
}

## decks longo e curto (15 dias) de `au` (hhmm do dia O); o store de `au` corta carga e observado em available_until
ds_decks <- function(au) {
  st <- dl_fx_store(au); cuts <- dl_fx_cuts(st)
  list(st = st, cuts = cuts, long = dl_fx_panel(st, cuts, "infer"),
       short = dl_fx_panel(st, cuts, "infer", history_days = 15L))
}

test_that("[PLUGINS] inferencia: estado derivado ac-0: o treino grava dm_o_sample, dm_o_formula, columns e inference_needs", {
  p <- dl_plugin(); e <- p$env; pl <- dl_base_art()$payload
  O <- format(ds_O())
  expect_true(all(c("dm_o_sample", "dm_o_formula", "columns", "inference_needs", "norm", "head_recipe") %in% names(pl)))
  s <- pl$dm_o_sample
  expect_identical(class(s), "data.frame")                              # puro: sem data.table nem selfref
  expect_null(attr(s, ".internal.selfref"))
  expect_s3_class(s$Data, "Date"); expect_true(all(s$Data < ds_O()))
  expect_true(all(ds_mmc() %in% names(s)))
  expect_false(anyNA(s$Carga))                                          # filtraDadosTreinamento: so Carga nao-NA
  expect_identical(setdiff(names(s), ds_mmc()), pl$columns$dm[[O]]$vars)
  expect_true(is.character(pl$dm_o_formula) && length(pl$dm_o_formula) == 1L)
  expect_false(grepl("mm", pl$dm_o_formula, fixed = TRUE))              # formula do motor, sem mm
  ## columns: dm para os 9 dias e perfil para 9 x 24, com vars e levels
  ## os dias que o motor roda (O..O+9: o spline do ultimo dia entregue ve o dia seguinte), os mesmos de head_recipe$dias
  expect_identical(names(pl$columns$dm), pl$head_recipe$dias); expect_length(pl$columns$dm, 10L)
  expect_identical(names(pl$columns$perfil), pl$head_recipe$dias)
  for (dp in pl$head_recipe$dias) {
    expect_named(pl$columns$dm[[dp]], c("vars", "levels")); expect_true(is.character(pl$columns$dm[[dp]]$vars))
    expect_length(pl$columns$perfil[[dp]], 24L)
    for (h in 1:24) expect_named(pl$columns$perfil[[dp]][[h]], c("vars", "levels"))
  }
  ## inference_needs: 15 dias de lag derivados de lagsPerfil (nunca literal), forma normalizada da fundacao
  n <- ceiling(max(pl$head_recipe$config_mdl$lagsPerfil) / 24) + 1
  expect_identical(pl$inference_needs$load$days_before, as.integer(n))
  expect_identical(pl$inference_needs$load$days_before, 15L)
  expect_identical(pl$inference_needs$weather_observed$days_before, 15L)
  expect_identical(pl$inference_needs$weather_forecast$segments[[1L]]$to, 9L)
  expect_identical(pl$inference_needs$holidays, list(from = -15L, to = 9L))
  sys.source(file.path(dl_root(), "scripts", "lib", "inference_needs.R"), envir = dl_lib)
  expect_identical(dl_lib$prevcarga_inference_needs_normalize(pl$inference_needs, "dessem-linear"), pl$inference_needs)
  ## mm cru por hora de corte = dl_morning_means do deck de inferencia nos dias em comum
  d <- ds_decks("09:00")
  f <- ds_feat(d$st, d$cuts, d$long, pl)$frame
  mm9 <- e$dl_morning_means(f$hourly_load, e$dl_cut_at(ds_O(), DL_TZ, 9L), ds_O())
  both <- merge(data.table::as.data.table(s)[, .(Data, mm_h09)], mm9, by = "Data")
  expect_gt(nrow(both), 3L)
  expect_identical(both$mm_h09, both$mm)
})

test_that("[PLUGINS] inferencia: estado derivado ac-1 nowcast 09:00: deck de 15 dias == deck longo (values, delivery) e treino da cabeca identico", {
  p <- dl_plugin(); e <- p$env; eng <- dl_engine_env(p); art <- dl_base_art(); pl <- art$payload
  d <- ds_decks("09:00")
  spy <- ds_data_spy(eng); on.exit(spy$restore(), add = TRUE)
  long <- dl_infer(art, d$st, d$cuts, panel = d$long)
  short <- dl_infer(art, d$st, d$cuts, panel = d$short)
  expect_identical(short$values, long$values)
  expect_identical(short$delivery, long$delivery)
  expect_identical(short$meta$dm_o_mode, "nowcast"); expect_identical(long$meta$dm_o_mode, "nowcast")
  expect_identical(dim(short$values), c(9L, 48L)); expect_true(all(is.finite(short$values)))
  expect_identical(spy$log$n, 2L)                                       # uma por inferencia
  ## conjunto de treino montado de dm_o_sample (mm_h09) == filtraDadosTreinamento do deck longo com o mm do corte 09:00
  fl <- ds_feat(d$st, d$cuts, d$long, pl)$inf
  inp <- e$dl_dm_head_input(eng, fl$serieDM, fl$config_mdl, fl$mm, ds_O(), TRUE, pl$norm$dm_carga)
  expect_identical(as.data.frame(spy$log$data[[1L]]), as.data.frame(eng$filtraDadosTreinamento(inp$dadosEntrada)))
  expect_identical(as.data.frame(spy$log$data[[2L]]), as.data.frame(spy$log$data[[1L]]))
  expect_match(spy$log$fml[[1L]], "+ mm", fixed = TRUE)
  ## CONTROL: a cabeca vem do artefato; alterar a amostra gravada muda a saida do deck curto
  alt <- art; alt$payload$dm_o_sample$Carga <- alt$payload$dm_o_sample$Carga * 1.4
  ctl <- dl_infer(alt, d$st, d$cuts, panel = d$short)
  expect_gt(max(abs(ctl$values[1L, ] - short$values[1L, ])), 1e-5)
})

test_that("[PLUGINS] inferencia: estado derivado ac-2 no_day_o 00:00: deck de 15 dias == deck longo e formula do motor sem mm", {
  p <- dl_plugin(); eng <- dl_engine_env(p); art <- dl_base_art(); pl <- art$payload
  d <- ds_decks("00:00")
  spy <- ds_data_spy(eng); on.exit(spy$restore(), add = TRUE)
  long <- dl_infer(art, d$st, d$cuts, panel = d$long)
  short <- dl_infer(art, d$st, d$cuts, panel = d$short)
  expect_identical(short$values, long$values)
  expect_identical(short$delivery, long$delivery)
  expect_identical(short$meta$dm_o_mode, "no_day_o"); expect_identical(long$meta$dm_o_mode, "no_day_o")
  expect_identical(spy$log$n, 2L)
  expect_identical(spy$log$fml[[1L]], pl$dm_o_formula); expect_identical(spy$log$fml[[2L]], pl$dm_o_formula)
  expect_false(any(grepl("^mm", names(spy$log$data[[1L]]))))
  expect_identical(as.data.frame(spy$log$data[[1L]]), as.data.frame(spy$log$data[[2L]]))
  expect_identical(nrow(spy$log$data[[1L]]), nrow(pl$dm_o_sample))
})

test_that("[PLUGINS] inferencia: estado derivado ac-3 espioes: tune_wsvm 1 vez, getNormaliza* 0, .Random.seed apos o ajuste identico", {
  p <- dl_plugin(); e <- p$env; eng <- dl_engine_env(p); art <- dl_base_art()
  for (au in c("09:00", "00:00")) {
    d <- ds_decks(au)
    tune <- dl_spy_tune(eng); nrm <- ds_count_spy(eng, c("getNormalizaCarga", "getNormalizaExogena"))
    on.exit({ tune$restore(); nrm$restore() }, add = TRUE)
    seeds <- list(); n_tune <- integer(); n_nrm <- integer()
    for (k in c("long", "short")) {
      tune$reset(); n0 <- nrm$count()
      dl_infer(art, d$st, d$cuts, panel = d[[k]])
      n_tune[[k]] <- tune$log$n; n_nrm[[k]] <- nrm$count() - n0
      ## posicao do RNG logo apos dl_dm_o_fit (set.seed(seed) imediatamente antes, como em plugin_infer)
      inf <- ds_feat(d$st, d$cuts, d[[k]], art$payload)$inf
      a <- e$dl_thaw(art$payload)
      set.seed(as.integer(a$seed)); suppressMessages(e$dl_dm_o_fit(a, inf)); seeds[[k]] <- get(".Random.seed", globalenv())
    }
    expect_identical(n_tune[["long"]], 1L); expect_identical(n_tune[["short"]], 1L)
    expect_identical(n_nrm[["short"]], 0L)                              # deck curto: normalizacao so do artefato
    expect_identical(seeds$short, seeds$long)
    tune$restore(); nrm$restore()
  }
  ## CONTROL: o espiao de getNormaliza* enxerga uma chamada direta
  nrm <- ds_count_spy(eng, c("getNormalizaCarga")); on.exit(nrm$restore(), add = TRUE)
  eng$getNormalizaCarga(list(flag = TRUE, oper = "max", const = 1), data.table::data.table(Carga = c(1, 2)))
  expect_identical(nrm$count(), 1L)
})

test_that("[PLUGINS] inferencia: estado derivado ac-4 dl_align_row: LAG_FER_k e DiaEspecial recompostos; coluna extra ou faltante => artifact_mismatch columns", {
  e <- dl_plugin()$env
  lv <- paste0("F", sprintf("%02d", 0:12))
  cols <- list(vars = c("Data", "LAG_1", "LAG_FER_2", "DiaEspecial"), levels = list(DiaEspecial = lv))
  d <- lubridate::ymd("2024-12-24")
  serie <- data.table::data.table(Data = d + (-4:1), DiaEspecial = c(0L, 0L, 5L, 0L, 2L, 0L))
  row <- data.table::data.table(Data = d, LAG_1 = 0.5)                 # sem LAG_FER_2 e sem DiaEspecial
  r <- dl_with_cli(e$dl_align_row(row, cols, serie, d, "teste"))
  expect_identical(names(r), cols$vars)
  expect_identical(r$LAG_FER_2, 1)                                      # as.numeric(DiaEspecial > 0) de d-2 = 2024-12-22 (tipo 5): indicador, nao o codigo
  expect_true(is.factor(r$DiaEspecial)); expect_identical(levels(r$DiaEspecial), lv)
  expect_identical(as.character(r$DiaEspecial), "F02")
  expect_identical(names(row), c("Data", "LAG_1"))                      # a linha de entrada nao e mutada
  ## so LAG_FER_2 ausente (DiaEspecial presente como fator): recompoe so o lag
  row2 <- data.table::data.table(Data = d, LAG_1 = 0.5, DiaEspecial = factor("F02", levels = lv))
  r2 <- dl_with_cli(e$dl_align_row(row2, cols, serie, d, "teste"))
  expect_identical(r2$LAG_FER_2, 1); expect_identical(r2$DiaEspecial, row2$DiaEspecial)
  ## dummies one-hot da DM de dia especial (ML_DEFAULT): o deck curto nao tem as colunas de tipo/mes que o treino viu
  colsd <- list(vars = c("Data", "LAG_1", "F02", "F05", "D3", "M12"), levels = list())
  sd <- data.table::data.table(Data = d + (-4:1), DiaEspecial = c(0L, 0L, 5L, 0L, 2L, 0L))
  rd <- dl_with_cli(e$dl_align_row(data.table::data.table(Data = d, LAG_1 = 0.5), colsd, sd, d, "DM dummies"))
  expect_identical(names(rd), colsd$vars)
  expect_identical(unlist(rd[, .(F02, F05, D3, M12)], use.names = FALSE), c(1L, 0L, 1L, 1L))   # 2024-12-24: tipo 2, terca, dezembro
  ## dummy desconhecida com 0 some; com 1 (tipo nunca visto no treino) e recusada
  r0 <- data.table::data.table(Data = d, LAG_1 = 0.5, F09 = 0L)
  expect_identical(names(dl_with_cli(e$dl_align_row(r0, colsd, sd, d, "DM dummies"))), colsd$vars)
  expect_dl_error(e$dl_align_row(data.table::data.table(Data = d, LAG_1 = 0.5, F09 = 1L), colsd, sd, d, "DM dummies"),
                  "plugin.dessem_linear_artifact_mismatch", "F09")
  ## coluna extra desconhecida
  row3 <- data.table::data.table(Data = d, LAG_1 = 0.5, EXTRA = 1)
  cond <- expect_dl_error(e$dl_align_row(row3, cols, serie, d, "DM teste"), "plugin.dessem_linear_artifact_mismatch", "EXTRA")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  expect_identical(cond$details$field, "columns"); expect_identical(cond$details$sobrando, "EXTRA")
  ## coluna faltante que nao e uma das duas recomposicoes
  row4 <- data.table::data.table(Data = d, DiaEspecial = factor("F02", levels = lv))
  cond4 <- expect_dl_error(e$dl_align_row(row4, cols, serie, d, "DM teste"), "plugin.dessem_linear_artifact_mismatch", "LAG_1")
  expect_identical(cond4$details$faltando, "LAG_1")
  ## sem colunas gravadas para o dia
  expect_dl_error(e$dl_align_row(row, NULL, serie, d, "DM teste"), "plugin.dessem_linear_artifact_mismatch", "columns")
  ## linha real do treino: alinhar nao muda nada
  pl <- dl_base_art()$payload; O <- format(ds_O())
  real <- data.table::as.data.table(pl$dm_o_sample)[1L, pl$columns$dm[[O]]$vars, with = FALSE]
  expect_identical(dl_with_cli(e$dl_align_row(real, pl$columns$dm[[O]], serie, ds_O(), "real")), real)
})

test_that("[PLUGINS] inferencia: estado derivado ac-5 payload v0.1.5 recusado com exit 2 nomeando o campo, antes de TransformaDeck e de tune_wsvm", {
  p <- dl_plugin(); eng <- dl_engine_env(p); art <- dl_base_art(); pl <- art$payload
  d <- ds_decks("09:00")
  m <- dl_model_panel(d$short, d$cuts)
  frame <- dl_with_cli(suppressMessages(p$descriptor$stages$ingestion$fn(m$panel, m$cuts)))
  feat <- p$descriptor$stages$inference_features$fn
  tune <- dl_spy_tune(eng); td <- ds_count_spy(eng, "TransformaDeck")
  on.exit({ tune$restore(); td$restore() }, add = TRUE)
  v015 <- pl[setdiff(names(pl), c("dm_o_sample", "dm_o_formula", "columns", "inference_needs"))]
  cond <- expect_dl_error(feat(frame, v015), "plugin.dessem_linear_artifact_mismatch", "dm_o_sample")
  expect_identical(cond$exit_code, 2L); expect_identical(cond$class_name, "contract")
  expect_identical(cond$details$field, "dm_o_sample")
  expect_identical(expect_dl_error(feat(frame, pl[setdiff(names(pl), c("dm_o_formula", "columns"))]),
                                   "plugin.dessem_linear_artifact_mismatch")$details$field, "dm_o_formula")
  expect_identical(expect_dl_error(feat(frame, pl[setdiff(names(pl), "columns")]),
                                   "plugin.dessem_linear_artifact_mismatch")$details$field, "columns")
  ## amostra sem uma coluna mm_h
  bad <- pl; bad$dm_o_sample$mm_h05 <- NULL
  expect_identical(expect_dl_error(feat(frame, bad), "plugin.dessem_linear_artifact_mismatch", "mm_h05")$details$field,
                   "dm_o_sample")
  expect_identical(tune$log$n, 0L); expect_identical(td$count(), 0L)
  ## CONTROL: o payload completo segue (TransformaDeck e chamado)
  ok <- dl_with_cli(suppressMessages(feat(frame, pl)))
  expect_gt(td$count(), 0L)
  expect_true(all(c("processado", "serieDM", "seriePF", "mm", "config_mdl", "norm", "cuts", "engine") %in% names(ok)))
})

test_that("[PLUGINS] inferencia: estado derivado ac-6 freeze/thaw: scan_pure passa, thaw devolve data.table com 24 mm; sem mm_h05 => integrity exit 9", {
  e <- dl_plugin()$env; pl <- dl_base_art()$payload
  fz <- e$dl_freeze(pl)
  expect_error(dl_with_cli(dl_lib$prevcarga_plugin_scan_pure(fz)), NA)
  expect_identical(class(fz$dm_o_sample), "data.frame"); expect_null(attr(fz$dm_o_sample, ".internal.selfref"))
  th <- e$dl_thaw(fz)
  expect_true(data.table::is.data.table(th$dm_o_sample))
  expect_true(all(ds_mmc() %in% names(th$dm_o_sample))); expect_s3_class(th$dm_o_sample$Data, "Date")
  expect_identical(th$dm_o_formula, pl$dm_o_formula); expect_identical(th$columns, pl$columns)
  expect_identical(as.data.frame(th$dm_o_sample), as.data.frame(pl$dm_o_sample))
  ## thaw de artefato ja descongelado e idempotente
  expect_identical(as.data.frame(e$dl_thaw(th)$dm_o_sample), as.data.frame(pl$dm_o_sample))
  ## CONTROL: congelado sem mm_h05 => integridade, nomeando a coluna
  bad <- fz; bad$dm_o_sample$mm_h05 <- NULL
  cond <- expect_dl_error(e$dl_thaw(bad), "plugin.dessem_linear_artifact_tampered", "mm_h05")
  expect_identical(cond$exit_code, 9L); expect_identical(cond$class_name, "integrity")
  expect_identical(cond$details$coluna, "mm_h05")
  ## columns incoerente (perfil de um dia com 23 horas) e Data fora de Date
  b2 <- fz; b2$columns$perfil[[1L]] <- b2$columns$perfil[[1L]][1:23]
  expect_dl_error(e$dl_thaw(b2), "plugin.dessem_linear_artifact_tampered", "columns")
  b3 <- fz; b3$dm_o_sample$Data <- as.character(b3$dm_o_sample$Data)
  expect_dl_error(e$dl_thaw(b3), "plugin.dessem_linear_artifact_tampered", "Data")
  ## artefato v0.1.5 (sem os campos): o thaw nao inventa erro; quem recusa e dl_check_payload (exit 2)
  v015 <- fz[setdiff(names(fz), c("dm_o_sample", "dm_o_formula", "columns", "inference_needs"))]
  expect_error(e$dl_thaw(v015), NA)
})

test_that("[PLUGINS] inferencia: estado derivado dl_check_norm: constante vazia so com a flag desligada; flag ligada com vazia falha alto (ADR-0005)", {
  p <- dl_plugin(); e <- p$env; eng <- dl_engine_env(p)
  cfg <- eng$getConfigModel("Linear.TmpMedia")
  norm <- dl_base_art()$payload$norm
  expect_length(norm$pf_exo, 0L)                                        # perfil$exogena: chave sem flag nos model_parameters
  expect_error(e$dl_check_norm(norm, cfg), NA)                          # vazia sem flag ligada: legitima
  ## flag ligada (dm$carga) com constante vazia: recusa nomeando a constante
  bad <- modifyList(norm, list(dm_carga = logical(0)))
  cond <- expect_dl_error(e$dl_check_norm(bad, cfg), "plugin.dessem_linear_artifact_mismatch", "norm")
  expect_identical(cond$details$field, "norm"); expect_true("dm_carga" %in% cond$details$constante)
  expect_dl_error(e$dl_build_series(data.table::data.table(), cfg, norm = bad), "plugin.dessem_linear_artifact_mismatch", "norm")
  ## CONTROL: com a flag de dm$carga desligada no config, a mesma constante vazia passa
  off <- cfg; off$normalizacao$dm$carga$flag <- FALSE
  expect_error(e$dl_check_norm(bad, off), NA)
  ## sem config_mdl a conferencia de vazio fica como antes (nao ha flag para cruzar)
  expect_error(e$dl_check_norm(bad), NA)
})

test_that("[PLUGINS] inferencia: estado derivado needs: o artefato do plugin traz os needs do payload (sem anexo); o data_needs do descriptor, que monta o deck de treino, segue com o anexo", {
  p <- dl_plugin(); art <- dl_base_art(); pl <- art$payload
  for (m in c("inference_needs", "training")) sys.source(file.path(dl_root(), "scripts", "lib", paste0(m, ".R")), envir = dl_lib)
  area <- pl$area
  ## deck de treino: o descriptor (run.yaml efetivo ano_civil) pede o anexo de feriados
  dn <- p$descriptor$data_needs(p$spec, area)
  expect_true(dn$infer$holiday_annex)
  expect_identical(dl_lib$prevcarga_inference_needs_of(p, p$spec, area)$holiday_annex, TRUE)
  ## artefato de treino (facade): a$inference_needs sai do payload do plugin
  cuts <- list(train_origin = ds_O(), last_data_day = ds_O() - 1L, load_label = "end", tz = DL_TZ,
               weather_issue_cutoff = "available_until", origin = ds_O())
  inp <- list(plugin = p, spec = p$spec, cuts = cuts)
  member <- list(sigla = "LIN_MED", mdir = "dessem-linear", exogena = "MED")
  a <- dl_with_cli(dl_lib$prevcarga_artifact_build(member, area, inp, art))
  expect_identical(a$inference_needs, pl$inference_needs)
  expect_false(a$inference_needs$holiday_annex)
  expect_identical(a$inference_needs$load$days_before, 15L)
  ## deck-inference (agregacao dos needs do artefato): sem anexo; o deck de treino (needs do descriptor): com anexo
  agg <- function(n) dl_lib$prevcarga_inference_needs_aggregate(data.table::data.table(needs = list(n), area = area))
  expect_false(agg(a$inference_needs)$holiday_annex)
  expect_true(agg(dn$infer |> dl_lib$prevcarga_inference_needs_normalize("dessem-linear"))$holiday_annex)
  ## CONTROL: sem o campo no payload, vale o descriptor (anexo TRUE): o caminho generico nao fixa o plugin
  art2 <- art; art2$payload$inference_needs <- NULL
  b <- dl_with_cli(dl_lib$prevcarga_artifact_build(member, area, inp, art2))
  expect_true(b$inference_needs$holiday_annex)
})

## painel com os feriados futuros (O..O+H): dl_fx_panel os remove (o fixture comum nao tem amostra historica do tipo)
ds_panel_hol <- function(st, cuts, mode, history_days) {
  p <- dl_fx_panel(st, cuts, mode, history_days = history_days)
  full <- suppressWarnings(dl_lib$prevcarga_read_panel(dl_fx_ctx(st, cuts$weather_issue_cutoff), lubridate::as_date(cuts$origin_day),
                                                       st$areas, horizon_days = cuts$horizon_days, history_days = history_days))
  ## calendario no formato do inference_needs: O-n..O+H (o leitor devolve todos os feriados do store)
  p$holidays <- full$holidays[area_code == DL8_AREA & date >= lubridate::as_date(cuts$origin_day) - as.integer(history_days)]
  p
}

test_that("[PLUGINS] inferencia: estado derivado holiday: origem com feriado no horizonte (O+3, codigo 3, sem outro no deck curto): deck de 15 dias == deck longo com delta 0 em toda a saida", {
  st0 <- dl_fx_store("09:00", days = 100L, hist = 80L); O <- st0$O
  dir <- st0$dir
  for (d in list(O - 36L, O - 26L, O - 20L, O + 3L))   # tres historicos fora dos 15 dias e um no horizonte
    dir <- dl_lib$prevcarga_fixture_set_holiday(dir, DL8_AREA, d, c(holiday_type_id = 1, prevcarga_code = 3, simplified_code = 1))
  st <- dl_fx_with_dir(st0, dir)
  cuts <- dl_fx_cuts(st)
  tr <- ds_panel_hol(st, cuts, "train", st$hist + 1L)
  long_p <- ds_panel_hol(st, cuts, "infer", st$hist + 1L); short_p <- ds_panel_hol(st, cuts, "infer", 15L)
  expect_true((O + 3L) %in% short_p$holidays$date)                  # o feriado do horizonte esta no deck curto
  expect_identical(sum(short_p$holidays$prevcarga_code == 3L & short_p$holidays$date < O), 0L)   # e nao ha outro codigo 3 nele
  art <- dl_train(st, cuts, panel = tr); pl <- art$payload
  ## o treino viu dummies (ML_DEFAULT do dia especial) que o deck curto nao traz: meses de fora dos 15 dias
  dmcol <- pl$columns$dm[[format(O + 3L)]]$vars
  expect_true("F03" %in% dmcol)
  e <- dl_plugin()$env; eng <- dl_engine_env()
  fs <- ds_feat(st, cuts, short_p, pl)$inf
  row <- suppressWarnings(eng$obterEntradaFormulaDM(format(O + 3L), fs$serieDM, fs$config_mdl)$dadosEntrada$serieDiariaModelo[Data == O + 3L])
  falta <- setdiff(dmcol, names(row))
  expect_gt(length(falta), 0L)                                       # preenchimento necessario de verdade (nao e no-op)
  expect_true(all(grepl("^[FDM][0-9]+$", falta)))                    # e so de dummies one-hot
  expect_true("M08" %in% falta)
  long <- dl_infer(art, st, cuts, panel = long_p); short <- dl_infer(art, st, cuts, panel = short_p)
  expect_identical(short$values, long$values)                        # delta 0, dias de feriado inclusive
  expect_identical(short$delivery, long$delivery)
  expect_identical(dim(short$values), c(9L, 48L)); expect_true(all(is.finite(short$values)))
  ## CONTROL: a linha alinhada do deck curto e IGUAL a do deck longo (as dummies recompostas sao as verdadeiras)
  fl <- ds_feat(st, cuts, long_p, pl)$inf
  rl <- suppressWarnings(eng$obterEntradaFormulaDM(format(O + 3L), fl$serieDM, fl$config_mdl)$dadosEntrada$serieDiariaModelo[Data == O + 3L])
  expect_identical(rl$F03, 1L)
  al <- dl_with_cli(e$dl_align_row(row, pl$columns$dm[[format(O + 3L)]], fs$serieDM, O + 3L, "DM holiday"))
  expect_identical(al$F03, 1L)
  expect_identical(as.data.frame(al), as.data.frame(rl[, names(al), with = FALSE]))   # linha alinhada == linha do deck longo
})

test_that("[PLUGINS] inferencia: estado derivado holiday: tres dias especiais consecutivos de codigos diferentes no horizonte (formato do Carnaval, O+2..O+4): deck de 15 dias == deck longo com delta 0", {
  st0 <- dl_fx_store("09:00", days = 120L, hist = 100L); O <- st0$O
  dir <- st0$dir
  cod <- c(3L, 4L, 5L)                                              # segunda, terca e quarta (codigos distintos)
  for (w in c(36L, 26L, 20L))                                       # tres semanas passadas, fora dos 15 dias, com os tres codigos
    for (k in 0:2) dir <- dl_lib$prevcarga_fixture_set_holiday(dir, DL8_AREA, O - w + k,
                                                                c(holiday_type_id = 1, prevcarga_code = cod[[k + 1L]], simplified_code = 1))
  for (k in 0:2) dir <- dl_lib$prevcarga_fixture_set_holiday(dir, DL8_AREA, O + 2L + k,
                                                              c(holiday_type_id = 1, prevcarga_code = cod[[k + 1L]], simplified_code = 1))
  st <- dl_fx_with_dir(st0, dir); cuts <- dl_fx_cuts(st)
  tr <- ds_panel_hol(st, cuts, "train", st$hist + 1L)
  long_p <- ds_panel_hol(st, cuts, "infer", st$hist + 1L); short_p <- ds_panel_hol(st, cuts, "infer", 15L)
  hz <- short_p$holidays[date >= O + 2L & date <= O + 4L]
  expect_identical(hz$prevcarga_code, cod)                          # os 3 dias especiais no deck curto, codigos distintos
  expect_identical(sum(short_p$holidays$date < O & short_p$holidays$prevcarga_code %in% cod), 0L)   # sem amostra passada dentro dos 15 dias
  expect_identical(sum(tr$holidays$date < O - 15L & tr$holidays$prevcarga_code %in% cod), 9L)         # 3 por codigo fora dos 15 dias
  art <- dl_train(st, cuts, panel = tr)
  long <- dl_infer(art, st, cuts, panel = long_p); short <- dl_infer(art, st, cuts, panel = short_p)
  expect_identical(short$values, long$values)                       # delta 0 em toda a saida, os 3 dias inclusive
  expect_identical(short$delivery, long$delivery)
  expect_identical(dim(short$values), c(9L, 48L)); expect_true(all(is.finite(short$values)))
  expect_identical(short$meta$dm_o_mode, "nowcast")
})
