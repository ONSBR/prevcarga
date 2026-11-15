## test-rd-norm-inject.R -- E4-T1: normalizacao injetavel nos builders de serie do dessem-regdin (ADR-045).
## Fixture sintetica (rd_panel de 60 dias), motor real; espioes em getNormaliza*/TransformaDeck no env do motor.

rni_spy <- function(eng, fns) {
  n <- new.env(); n$k <- 0L
  orig <- lapply(fns, function(f) get(f, envir = eng, inherits = FALSE))
  names(orig) <- fns
  lk <- vapply(fns, function(f) bindingIsLocked(f, eng), NA)
  for (i in seq_along(fns)) {
    if (lk[[i]]) unlockBinding(fns[[i]], eng)
    local({ f <- fns[[i]]; o <- orig[[f]]
      assign(f, function(...) { n$k <- n$k + 1L; o(...) }, envir = eng) })
  }
  list(count = function() n$k,
       restore = function() for (i in seq_along(fns)) {
         assign(fns[[i]], orig[[fns[[i]]]], envir = eng)
         if (lk[[i]]) lockBinding(fns[[i]], eng)
       })
}

rni_state <- local({
  s <- NULL
  function() {
    if (!is.null(s)) return(s)
    p <- rd_load()
    pan <- rd_panel()
    ## pico antigo (fora do recorte de 15 dias): as constantes de maximo da serie longa passam a diferir das do recorte
    d_pico <- lubridate::ymd("2024-12-23") - 45L
    pan$load[lubridate::as_date(interval_start) == d_pico, load_mwh := load_mwh + 4000]
    pan$weather_observed[lubridate::as_date(valid_time) == d_pico, temperature := temperature + 8]
        cuts <- rd_cuts(); m <- rd_model_panel(pan, cuts)
    spec <- rd_spec("MED")
    deck <- rd_with_cli(p$env$plugin_ingest(m$panel, m$cuts, spec))
    f <- rd_with_cli(suppressMessages(p$env$plugin_build_features(deck, spec)))
    s <<- list(p = p, eng = parent.env(p$env), deck = deck, spec = spec, f = f,
               S = f$processado$SERIECOMP, cfg = f$config_mdl)
    s
  }
})

rni_ref <- function(eng, S, cfg) {
  nz <- cfg$normalizacao
  base <- if ("Bloco" %in% names(S)) S[Bloco == 0L & Prev == FALSE] else S[Prev == FALSE]
  com <- if ("Bloco" %in% names(S)) S[Bloco == 0L] else S
  dm <- eng$aggregaSerie(serieHist = data.table::copy(com), tipotemp = cfg$exogenaOper,
    normaliza = any(nz$carga$flag, nz$exogena$flag),
    constnorm = eng$getNormalizaCarga(nz$carga, base), constnormtemp = eng$getNormalizaExogena(nz$exogena, base))
  pf <- data.table::copy(S)
  eng$inclui_CargaMediaRef(pf, nz$perfil$cargadm)
  eng$normalizaSerieRef(serieHist = pf, constnorm = nz$perfil$carga$oper,
                        constnormtemp = eng$getNormalizaExogena(nz$perfil$exogena, base))
  list(serieDM = dm, seriePF = pf)
}

test_that("[PLUGINS] inferencia: norm injetavel ac-0: norm NULL identico a chamada direta do motor; plugin devolve o norm de dl_norm_constants", {
  st <- rni_state(); e <- st$p$env
  ref <- rni_ref(st$eng, st$S, st$cfg)
  got <- e$.rd_build_series(st$S, st$cfg)
  expect_identical(got$serieDM, ref$serieDM)
  expect_identical(got$seriePF, ref$seriePF)
  expect_identical(st$f$norm, e$.rd_norm_constants(st$S, st$cfg))
})

test_that("[PLUGINS] inferencia: norm injetavel ac-1: com norm dado getNormaliza* tem 0 chamadas e as series sao identical() as do caminho NULL", {
  st <- rni_state(); e <- st$p$env
  base <- e$.rd_build_series(st$S, st$cfg)
  spy <- rni_spy(st$eng, c("getNormalizaCarga", "getNormalizaExogena")); on.exit(spy$restore(), add = TRUE)
  got <- e$.rd_build_series(st$S, st$cfg, norm = st$f$norm)
  expect_identical(spy$count(), 0L)
  expect_identical(got$serieDM, base$serieDM)
  expect_identical(got$seriePF, base$seriePF)
  ## CONTROL: o caminho NULL chama o motor (o espiao funciona)
  e$.rd_build_series(st$S, st$cfg)
  expect_gt(spy$count(), 0L)
  ## plugin_build_features devolve o proprio norm, sem recalcular
  n0 <- spy$count()
  f2 <- rd_with_cli(suppressMessages(e$plugin_build_features(st$deck, st$spec, norm = st$f$norm)))
  expect_identical(f2$norm, st$f$norm)
  expect_identical(spy$count(), n0)
})

test_that("[PLUGINS] inferencia: norm injetavel ac-2: recorte de 15 dias com o norm da longa = linhas identicas nos dias em comum; CONTROL sem norm difere", {
  st <- rni_state(); e <- st$p$env
  S <- st$S
  last_hist <- max(S[Prev == FALSE]$Data)
  cut <- S[Data >= last_hist - 14L]
  long <- e$.rd_build_series(S, st$cfg)
  inj <- e$.rd_build_series(cut, st$cfg, norm = st$f$norm)
  ctl <- e$.rd_build_series(cut, st$cfg)
  cmp <- function(a, b, cols) {
    d <- intersect(a$Data, b$Data); expect_gt(length(d), 10L)
    cols <- intersect(cols, names(a))
    identical(as.data.frame(a[Data %in% d, ..cols]), as.data.frame(b[Data %in% d, ..cols]))
  }
  pf_cols <- c("Carga", "CargaMed", grep("^Exo", names(long$seriePF), value = TRUE))
  dm_cols <- c("Carga", grep("^Exo", names(long$serieDM), value = TRUE))
  ## chave de comparacao por DataHora (PF) e Data (DM), na ordem original
  key <- function(x, k) x[order(get(k))]
  expect_true(cmp(key(inj$serieDM, "Data"), key(long$serieDM, "Data"), dm_cols))
  pf_l <- key(long$seriePF, "DataHora"); pf_i <- key(inj$seriePF, "DataHora")
  dh <- intersect(pf_l$DataHora, pf_i$DataHora)
  cols <- intersect(pf_cols, names(pf_l))
  expect_identical(as.data.frame(pf_i[DataHora %in% dh, ..cols]), as.data.frame(pf_l[DataHora %in% dh, ..cols]))
  ## CONTROL: sem norm ao menos uma das series difere nos dias em comum
  diff_dm <- !cmp(key(ctl$serieDM, "Data"), key(long$serieDM, "Data"), dm_cols)
  pf_c <- key(ctl$seriePF, "DataHora")
  diff_pf <- !identical(as.data.frame(pf_c[DataHora %in% dh, ..cols]), as.data.frame(pf_l[DataHora %in% dh, ..cols]))
  expect_true(diff_dm || diff_pf)
})

test_that("[PLUGINS] inferencia: norm injetavel ac-3: norm sem pf_cargadm ou com dm_carga NA aborta artifact_mismatch field norm sem chamar TransformaDeck", {
  st <- rni_state(); e <- st$p$env
  spy <- rni_spy(st$eng, "TransformaDeck"); on.exit(spy$restore(), add = TRUE)
  no_pf <- st$f$norm[setdiff(names(st$f$norm), "pf_cargadm")]
  na_dm <- modifyList(st$f$norm, list(dm_carga = NA_real_))
  zero <- modifyList(st$f$norm, list(pf_exo = 0))
  for (c in list(list(no_pf, "pf_cargadm"), list(na_dm, "dm_carga"), list(zero, "pf_exo"))) {
    cond <- expect_rd_error(e$plugin_build_features(st$deck, st$spec, norm = c[[1]]),
                            "plugin.regdin_artifact_mismatch", "norm")
    expect_identical(cond$details$field, "norm")
    expect_true(c[[2]] %in% cond$details$constante)
    expect_error(rd_with_cli(e$.rd_build_series(st$S, st$cfg, norm = c[[1]])), class = "prevcarga_error")
  }
  expect_identical(spy$count(), 0L)
  ## CONTROL: com norm valido o motor e chamado
  rd_with_cli(suppressMessages(e$plugin_build_features(st$deck, st$spec, norm = st$f$norm)))
  expect_gt(spy$count(), 0L)
})
