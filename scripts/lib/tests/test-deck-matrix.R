## test-deck-matrix.R -- matriz FORBIDDEN/CONTROL do validate-deck por `what` (M2.V, AC 12), IN-PROCESS.
## Deck limpo com zero findings (FORBIDDEN) x UMA mutacao isolada por `what` do catalogo do M2 (CONTROL): cada mutacao
## acrescenta exatamente o seu `what`, com a classe e o level esperados, e nada mais. Teste de cobertura: todo `what`
## que validation.R/deck_reader.R emitem (extraido por parse das chamadas a .pc_finding/prevcarga_abort/prevcarga_warn)
## existe em MUTATIONS, salvo a lista curta de excecoes justificadas. Fixtures so em tempdir (nunca data/).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
mv <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader", "dates", "validation", "inference_needs"))
mv$prevcarga_runtime_enforce()

mx_O <- lubridate::ymd("2026-10-01")
mx_at <- function(x) lubridate::ymd_hm(x, tz = "Etc/GMT+3")
mx_stamp <- function(x) lubridate::ymd_hms(x, tz = "UTC")   # digitos do store (modo end: inicio + 30 min)

## ---- infraestrutura ------------------------------------------------------------------------------------------------------------
mx_tmp <- function() { d <- tempfile("mx-"); dir.create(d); d }
mx_root <- function(hw = c(30, 10)) {
  d <- mx_tmp()
  file.copy(file.path(pc_root(), "config"), d, recursive = TRUE)
  file.copy(file.path(pc_root(), "models"), d, recursive = TRUE)
  set <- function(m, tr, inf) {
    f <- file.path(d, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
    y$history_days <- list(train = tr, infer = inf); yaml::write_yaml(y, f)
  }
  set("holt-winters", hw[1L], hw[2L]); set("random-forest", 30, 10); set("lgbm", 20, 5)
  set("dessem-regdin", 20, 20); set("dessem-linear", 20, 20)
  d
}
.mx <- new.env()
MX_A1 <- "RS"; MX_A2 <- "RJ"            # folhas de config/topology.yaml (SECO e S sao subsistemas); ordem radix: RJ < RS
mx_deck <- function() {                                   # limpo de 40 dias (RS, RJ) terminando em O; copia por chamada
  if (is.null(.mx$tpl)) {
    d <- mx_tmp(); h <- pc_deck_holidays_default(c(MX_A1, MX_A2))
    h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
    pc_deck_panel(areas = c(MX_A1, MX_A2), holidays = h, dir = d)
    pc_deck_days(d, 40)
    pc_deck_forecast_extend(d, mx_O, mx_O + 9L)
    .mx$tpl <- d
  }
  dst <- mx_tmp()
  file.copy(list.files(.mx$tpl, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  dst
}
root_std <- mx_root()
root_hw60 <- mx_root(hw = c(60, 10))       # treino de HW maior que o deck de 40 dias
root_hw45 <- mx_root(hw = c(30, 45))       # history_days.infer 45 > train 30: contract.history_infer_gt_train (ADR-045, infer <= train)
root_hw5060 <- mx_root(hw = c(60, 50))     # infer <= train, mas o deck de 40 dias nao cobre nenhum dos dois

mx_run <- function(deck, opts = list()) {
  ctx <- pc_ctx_deck(deck, load_label = opts$flag_label, source = if (is.null(opts$flag_label)) "default" else "flag")
  ctx$root <- if (is.null(opts$root)) root_std else opts$root
  ctx$weather_issue_cutoff <- if (is.null(opts$cutoff)) "available_until" else opts$cutoff
  for (k in names(opts$ctx)) ctx[[k]] <- opts$ctx[[k]]               # papel do deck (data_role) e --origin do ctx
  mv$prevcarga_validate_deck(ctx, origin = if (is.null(opts$origin)) mx_O else opts$origin, scope = opts$scope,
                             data_lag_days = opts$data_lag)
}

## manifest mutado em disco (pc_deck_manifest parte do padrao e aplica `fn(lista)`)
mx_manifest <- function(fn) function(d) pc_deck_manifest(d, mutate = fn)

## ---- MUTATIONS: uma linha por `what` do catalogo do M2 -----------------------------------------------------------------------
mx_row <- function(what, mutate, class, level = "error", opts = list(), also = character())
  list(what = what, mutate = mutate, class = class, level = level, opts = opts, also = also)
MUTATIONS <- list(
  ## contract: dataset / layout / tipos / arquivos
  mx_row("dataset_missing",  function(d) unlink(file.path(d, "auxiliary", "holidays"), recursive = TRUE), "contract"),
  mx_row("hive_layout",      function(d) pc_mut_flatten(d, "weather_observed"), "contract"),
  mx_row("parquet_unreadable", function(d) pc_mut_corrupt(d, sprintf("load/area_code=%s/year=2026/part-0.parquet", MX_A1)), "contract"),
  mx_row("schema_missing",   function(d) pc_mut_drop_col(d, "weather_forecast", "temperature"), "contract"),
  mx_row("tz_in_parquet",    function(d) pc_mut_retype(d, "load", "timestamp", "timestamp_tz"), "contract"),
  mx_row("type_mismatch",    function(d) pc_mut_retype(d, "load", "load_mwh", "string"), "contract"),
  ## contract: manifest
  mx_row("manifest_missing", function(d) unlink(file.path(d, "manifest.json")), "contract"),
  mx_row("manifest_invalid", mx_manifest(function(m) { m$schema_version <- 2L; m }), "contract"),
  mx_row("manifest_unknown_key", mx_manifest(function(m) { m$chave_inventada <- "x"; m }), "contract"),
  mx_row("tz_missing",       mx_manifest(function(m) { m$timezone$load <- NULL; m }), "contract"),
  mx_row("tz_invalid",       mx_manifest(function(m) { m$timezone$load <- "Marte/Olympus"; m }), "contract"),
  mx_row("weather_lag_missing", mx_manifest(function(m) { m$weather_forecast_publication_lag_hours <- NULL; m }), "contract"),
  mx_row("load_label_conflict", function(d) pc_deck_manifest(d, load_label = "start"), "contract", opts = list(flag_label = "end")),
  ## contract: sanidade de carga e clima
  mx_row("grid_off",         function(d) pc_mut_load(d, function(dt) dt[area_code == MX_A1 & timestamp == mx_stamp("2026-09-25 10:30:00"),
                                                                         timestamp := mx_stamp("2026-09-25 10:15:00")]), "contract",
         also = "day_incomplete"),                       # fora da grade: o dia fica com 47 intervalos validos
  mx_row("duplicate_interval", function(d) pc_mut_load(d, function(dt) rbind(dt, dt[area_code == MX_A1 & timestamp == mx_stamp("2026-09-25 10:30:00")])),
         "contract"),
  mx_row("load_nonpositive", function(d) pc_mut_load(d, function(dt) dt[area_code == MX_A1 & timestamp == mx_stamp("2026-09-25 10:30:00"), load_mwh := 0]),
         "contract", also = "outlier"),                  # 0 MW e tambem outlier (aviso)
  mx_row("temperature_out_of_range", function(d) pc_mut_weather(d, "weather_observed", function(dt)
           dt[area_code == MX_A1 & timestamp == mx_stamp("2026-09-25 10:00:00"), temperature := 60]), "contract",
         also = "temperature_jump"),                      # 60 C tambem e salto de temperatura (aviso)
  mx_row("code_negative",    function(d) pc_mut_holiday(d, "simplified_code", -1L), "contract"),
  ## completude
  mx_row("day_incomplete",   function(d) pc_mut_load(d, function(dt) dt[!(area_code == MX_A1 & timestamp == mx_stamp("2026-09-11 10:30:00"))]),
         "completude"),
  mx_row("origin_day_gap",   function(d) pc_deck_drop(d, MX_A1, mx_at("2026-10-01 07:00")), "completude"),
  mx_row("no_load",          function(d) invisible(NULL), "completude", opts = list(origin = mx_O + 60L)),
  ## topology
  mx_row("area_undeclared",  function(d) pc_mut_add_area(d, "XX"), "topology"),
  mx_row("scope_undeclared", function(d) invisible(NULL), "topology", opts = list(scope = list(area = "XX"))),
  ## window
  mx_row("window_train_deficit", function(d) invisible(NULL), "window", opts = list(root = root_hw60, scope = list(model = "holt-winters"))),
  ## ADR-045: infer <= train. A mutacao que antes dava window_infer_deficit (infer 45 > train 30) agora e o contrato de topologia
  mx_row("history_infer_gt_train", function(d) invisible(NULL), "contract", opts = list(root = root_hw45, scope = list(model = "holt-winters"))),
  mx_row("window_infer_deficit", function(d) invisible(NULL), "window", opts = list(root = root_hw5060, scope = list(model = "holt-winters")),
         also = "window_train_deficit"),
  ## avisos (level warning; nao reprovam)
  mx_row("outlier",          function(d) pc_mut_load(d, function(dt) dt[area_code == MX_A1 & timestamp == mx_stamp("2026-09-20 12:00:00"), load_mwh := 28]),
         "contract", "warning"),
  mx_row("flatline",         function(d) pc_mut_load(d, function(dt) dt[area_code == MX_A2 & timestamp >= mx_stamp("2026-09-15 10:30:00") &
                                                                        timestamp < mx_stamp("2026-09-15 13:30:00"), load_mwh := 1024.5]),   # nivel da area S (1000 + slot): sem outlier nas bordas
         "contract", "warning"),
  mx_row("temperature_jump", function(d) pc_mut_weather(d, "weather_observed", function(dt)
           dt[area_code == MX_A1 & timestamp == mx_stamp("2026-09-25 10:00:00"), temperature := temperature + 15]), "contract", "warning"),
  mx_row("weather_forecast_coverage", function(d) { pc_mut_weather(d, "weather_forecast", function(dt)
           dt[valid_time < mx_stamp("2026-10-05 00:00:00")]) }, "completude", "warning"),     # previsto so ate O+3
  mx_row("weather_non_leaf", function(d) pc_mut_add_area(d, "SECO"), "topology", "warning"),
  mx_row("climate_after_cut", function(d) { pc_mut_issue(d, 12L); pc_deck_until(d, mx_at("2026-10-01 08:30")) }, "contract", "warning",
         also = "weather_forecast_coverage"),            # unica emissao (12Z) e posterior ao limite: sem emissao valida
  mx_row("available_until_spread", function(d) { pc_mut_issue(d, 0L); pc_deck_until(d, mx_at("2026-10-01 08:30"))
                                                 pc_deck_drop(d, MX_A2, mx_at("2026-10-01 08:00")) }, "completude", "warning",
         also = "climate_after_cut")                     # a emissao 00Z de O+1 cai no dia O local, depois do limite
)
MX_WHATS <- vapply(MUTATIONS, function(r) r$what, "")

## Roda uma linha e devolve o veredito do braco (ok + mensagem que NOMEIA a linha quando falha).
mx_check_row <- function(row) {
  d <- mx_deck(); row$mutate(d)
  v <- mx_run(d, row$opts)
  got <- sort(unique(v$findings$what)); want <- sort(unique(c(row$what, row$also)))
  fnd <- v$findings[v$findings$what == row$what, ]
  msgs <- c(
    if (!identical(got, want)) sprintf("%s: findings %s != esperado %s", row$what, paste(got, collapse = ","), paste(want, collapse = ",")),
    if (nrow(fnd) && !all(fnd$class == row$class)) sprintf("%s: classe %s != %s", row$what, paste(unique(fnd$class), collapse = ","), row$class),
    if (nrow(fnd) && !all(fnd$level == row$level)) sprintf("%s: level %s != %s", row$what, paste(unique(fnd$level), collapse = ","), row$level),
    if (identical(row$level, "error") != !isTRUE(v$ok) && !length(row$also)) sprintf("%s: veredito ok=%s inconsistente com level %s", row$what, v$ok, row$level))
  list(ok = !length(msgs), msg = paste(msgs, collapse = "; "), v = v)
}

## ---- whats do deck de inferencia (E1-E3, ADR-044): kind/origem abortam; necessidade e schema de data_needs sao checks puros ------
MX_INF <- list(origin = "2026-10-01", available_until = "2026-10-01T08:00:00", train_deck_hash = strrep("a1", 32L),
               windows = list(load = list(from = "2026-08-01", to = "2026-10-01")))
mx_inference_deck <- function(d) pc_deck_manifest(d, mutate = function(m) { m$kind <- "inference"; m$inference <- MX_INF; m })
mx_needs <- function(annex = FALSE) list(
  schema_version = 1L, load = list(days_before = 3L), weather_observed = list(days_before = 3L),
  weather_forecast = list(area = MX_A1, segments = list(list(kind = "freshest", from = 0L, to = 1L))),
  holidays = list(from = -3L, to = 2L), holiday_annex = annex)
mx_abort <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
## Cada linha devolve a condicao/finding que o what produz; `got` = id (aborts) ou what (findings)
MUTATIONS_INF <- list(
  list(what = "deck_kind_mismatch", id = "contract.deck_kind_mismatch", class = "contract",
       run = function() { d <- mx_deck(); mx_abort(mx_run(d, list(ctx = list(data_role = "inference")))) }),
  list(what = "deck_origin_mismatch", id = "contract.deck_origin_mismatch", class = "contract",
       run = function() { d <- mx_deck(); mx_inference_deck(d)
         mx_abort(mx_run(d, list(ctx = list(data_role = "inference", origin = "2026-10-02")))) }),
  list(what = "inference_needs_invalid", id = "contract.inference_needs_invalid", class = "contract",
       run = function() mx_abort(mv$prevcarga_inference_needs_normalize(c(mx_needs(), list(extra = 1)), "mdl"))),
  list(what = "need_gap", id = "completude.need_gap", class = "completude",
       run = function() {            # anexo de feriados pedido e ausente no manifest: finding de erro need_gap
         d <- mx_deck(); man <- mv$prevcarga_deck_manifest(d)
         rows <- data.table::data.table(member = "m", mdir = "m", area = MX_A1, needs = list(mx_needs(annex = TRUE)), source = "artifacts")
         cuts <- list(weather_issue_limit = lubridate::force_tz(lubridate::as_datetime(mx_O), "Etc/GMT+3"))
         chk <- mv$.prevcarga_check_inference_needs(list(data_dir = d, root = pc_root()), mx_O, cuts,
                                                    mv$prevcarga_inference_needs_aggregate(rows), "Etc/GMT+3", man)
         f <- chk$findings; f[f$dataset == "holiday_annex" & f$what == "need_gap", ] }))
MX_WHATS <- c(MX_WHATS, vapply(MUTATIONS_INF, function(r) r$what, ""))

for (row in MUTATIONS_INF) local({
  r <- row
  test_that(paste0("[CONTROL] ", r$what, " (", r$class, "/error): a mutacao do deck de inferencia produz exatamente o seu what"), {
    got <- r$run()
    if (inherits(got, "prevcarga_error")) {
      expect_identical(got$id, r$id); expect_identical(got$class_name, r$class)
    } else {
      expect_identical(nrow(got), 1L)
      expect_identical(got$what, r$what); expect_identical(got$class, r$class); expect_identical(got$level, "error")
    }
  })
})

test_that("[FORBIDDEN] deck de inferencia coerente (papel e origem corretos) nao aborta; anexo ausente so some quando pedido", {
  d <- mx_deck(); mx_inference_deck(d)
  expect_null(mx_abort(mv$prevcarga_require_deck(list(data_dir = d, data_role = "inference", origin = "2026-10-01"))))
  expect_null(mx_abort(mv$prevcarga_require_deck(list(data_dir = mx_deck(), data_role = "train"))))
  expect_null(mx_abort(mv$prevcarga_inference_needs_normalize(mx_needs(), "mdl")))
})

## ---- FORBIDDEN: deck limpo ---------------------------------------------------------------------------------------------------
test_that("[FORBIDDEN] deck limpo: ok, classe ok e zero findings (nenhuma mutacao => nenhum what)", {
  v <- mx_run(mx_deck())
  expect_true(v$ok); expect_identical(v$class, "ok"); expect_identical(nrow(v$findings), 0L)
  expect_identical(names(v$findings), c("area", "dataset", "what", "detail", "class", "level", "where"))
})

## ---- CONTROL: uma mutacao por `what` ----------------------------------------------------------------------------------------------
test_that("MUTATIONS: uma linha por what, sem repeticao, e inclui os whats de cortes e codigos", {
  expect_false(anyDuplicated(MX_WHATS) > 0L)
  expect_true(all(c("code_negative", "origin_day_gap", "climate_after_cut", "available_until_spread") %in% MX_WHATS))
})

for (row in MUTATIONS) local({
  r <- row
  test_that(paste0("[CONTROL] ", r$what, " (", r$class, "/", r$level, "): a mutacao acrescenta exatamente o seu what"), {
    res <- mx_check_row(r)
    expect_true(res$ok, label = res$msg)
    expect_true(r$what %in% res$v$findings$what, label = r$what)
    if (identical(r$level, "warning")) { expect_true(res$v$ok); expect_true(all(res$v$findings$level == "warning")) }
    else expect_false(res$v$ok)
  })
})

test_that("[CONTROL] linha cujo mutador nao produz o seu what falha nomeando-a", {
  bogus <- mx_row("day_incomplete", function(d) invisible(NULL), "completude")      # mutador que nao faz nada
  res <- mx_check_row(bogus)
  expect_false(res$ok)
  expect_match(res$msg, "day_incomplete", fixed = TRUE)
  wrong <- mx_row("grid_off", function(d) pc_mut_holiday(d, "simplified_code", -1L), "contract")   # produz outro what
  r2 <- mx_check_row(wrong)
  expect_false(r2$ok); expect_match(r2$msg, "grid_off", fixed = TRUE)
  badlevel <- MUTATIONS[[which(MX_WHATS == "outlier")]]; badlevel$level <- "error"          # level declarado errado
  r3 <- mx_check_row(badlevel)
  expect_false(r3$ok); expect_match(r3$msg, "outlier", fixed = TRUE)
})

## ---- cobertura do catalogo ------------------------------------------------------------------------------------------------------
## Extrai por parse: 3o argumento literal de .pc_finding(...), e o id literal de prevcarga_abort/prevcarga_warn
## (classes de dado contract/completude/topology/window/warning; config/runtime sobem e nao viram finding).
mx_emitted_whats <- function(files) {
  found <- character()
  walk <- function(e) {
    if (is.call(e)) {
      fn <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
      args <- as.list(e)[-1L]
      if (identical(fn, ".pc_finding")) {
        w <- if ("what" %in% names(args)) args[["what"]] else args[[3L]]
        if (is.character(w) && length(w) == 1L) found <<- c(found, w)
      } else if (fn %in% c("prevcarga_abort", "prevcarga_warn") && length(args) && is.character(args[[1L]])) {
        id <- args[[1L]]
        if (grepl("^(contract|completude|topology|window|warning)[.]", id)) found <<- c(found, sub("^[a-z]+[.]", "", id))
      }
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])     # argumento vazio (x[, i]) nao e no
    } else if (is.expression(e) || is.pairlist(e)) for (i in seq_along(e)) walk(e[[i]])
  }
  for (f in files) walk(parse(f, keep.source = FALSE))
  sort(unique(found))
}
## whats que o validate-deck nunca emite (o leitor os levanta so no painel M2.7, fora do veredito do deck)
MX_EXEMPT <- c(constraint_violated = "prevcarga_pc_panel_attrs: consistencia do painel (M2.7), nao e finding do validate-deck")

test_that("cobertura: todo what emitido por validation.R/deck_reader.R aparece em MUTATIONS (ou na excecao justificada)", {
  lib <- file.path(pc_root(), "scripts", "lib")
  emitted <- mx_emitted_whats(file.path(lib, c("validation.R", "deck_reader.R")))
  expect_gt(length(emitted), 20L)                                    # o extrator enxerga o catalogo (nao esta cego)
  missing <- setdiff(emitted, c(MX_WHATS, names(MX_EXEMPT)))
  expect_identical(missing, character(), info = paste("what sem mutacao:", paste(missing, collapse = ", ")))
  expect_true(all(names(MX_EXEMPT) %in% emitted))                    # a excecao nao e letra morta
  ## o que MUTATIONS declara existe no catalogo de erros do M2 (nenhum what inventado)
  cat <- mv$prevcarga_errors_load()
  ids <- sub("^[a-z]+[.]", "", cat$id)
  expect_true(all(MX_WHATS %in% ids), info = paste(setdiff(MX_WHATS, ids), collapse = ", "))
})

test_that("[CONTROL] cobertura tem dente: tirar uma linha de MUTATIONS acusa o what; o extrator le uma isca", {
  lib <- file.path(pc_root(), "scripts", "lib")
  emitted <- mx_emitted_whats(file.path(lib, c("validation.R", "deck_reader.R")))
  expect_true("code_negative" %in% setdiff(emitted, setdiff(MX_WHATS, "code_negative")))
  bait <- tempfile("mx-bait-", fileext = ".R")
  writeLines(c("f <- function() .pc_finding('*', 'load', 'what_inventado', '', 'contract')",
               "g <- function() prevcarga_abort('completude.outro_inventado', where = 'x')"), bait)
  expect_setequal(mx_emitted_whats(bait), c("what_inventado", "outro_inventado"))
})

## ---- epico validate-deck (F6.5): nivel e classe por what novo; cobertura com dente para os 3 whats novos --------------------------
MX_EPIC_LEVELS <- data.frame(
  what  = c("outlier", "temperature_jump", "weather_non_leaf", "weather_forecast_coverage", "temperature_out_of_range"),
  class = c("contract", "contract", "topology", "completude", "contract"),
  level = c("warning", "warning", "warning", "warning", "error"), stringsAsFactors = FALSE)
test_that("epico validate-deck: cada what novo tem linha CONTROL com classe e nivel certos (temperature_out_of_range = error/contract)", {
  for (i in seq_len(nrow(MX_EPIC_LEVELS))) {
    w <- MX_EPIC_LEVELS$what[i]; k <- match(w, MX_WHATS)
    expect_false(is.na(k), label = paste("linha ausente:", w))
    if (is.na(k)) next
    r <- MUTATIONS[[k]]
    expect_identical(c(r$class, r$level), c(MX_EPIC_LEVELS$class[i], MX_EPIC_LEVELS$level[i]), label = r$what)
  }
})

test_that("[CONTROL] o teste de nivel acusa uma linha divergente pelo nome", {
  bad <- MX_EPIC_LEVELS; bad$level[bad$what == "outlier"] <- "error"
  r <- MUTATIONS[[match("outlier", MX_WHATS)]]
  expect_false(identical(c(r$class, r$level), c(bad$class[1L], bad$level[1L])))
})

test_that("cobertura acusa cada what novo quando a linha some (dente para os 3 whats novos)", {
  lib <- file.path(pc_root(), "scripts", "lib")
  emitted <- mx_emitted_whats(file.path(lib, c("validation.R", "deck_reader.R")))
  for (w in c("temperature_jump", "weather_non_leaf", "weather_forecast_coverage")) {
    expect_true(w %in% emitted, label = w)
    expect_true(w %in% MX_WHATS, label = w)
    ## sem a linha, o what sai de MX_WHATS e entra em `missing` (a mesma conta do teste de cobertura)
    expect_true(w %in% setdiff(emitted, setdiff(MX_WHATS, w)), label = w)
    expect_identical(setdiff(emitted, c(setdiff(MX_WHATS, w), names(MX_EXEMPT))) %in% w, TRUE, label = w)
  }
})
