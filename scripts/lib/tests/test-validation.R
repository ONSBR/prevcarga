## test-validation.R -- prevcarga_validate_deck (M2.4; ADR-0040). Pares limpo/mutacao: cada sub-checagem hard tem braco
## CONTROL com `what` proprio e participa do FORBIDDEN global (deck limpo: zero erro e zero aviso). Assercoes por
## what/class/level (nunca por substring de detail, salvo o formato `<dia>: 47/48`). Somente leitura: o md5 do
## data_dir e conferido antes/depois em TODA chamada (vd_run).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
venv <- pc_source_lib(c("errors", "runtime", "config", "topology", "deck_reader", "dates", "validation"))
venv$prevcarga_runtime_enforce()
for (n in c("prevcarga_validate_deck", "prevcarga_errors_load", "prevcarga_exit_precedence", "prevcarga_day_complete",
            ".prevcarga_verdict", ".pc_finding", ".pc_findings_bind", ".prevcarga_deck_spec"))
  assign(n, venv[[n]])

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2026-10-01")
at <- function(x) lubridate::ymd_hm(x, tz = TZ)
stamp <- function(x) lubridate::ymd_hms(x, tz = "UTC")   # digitos do store (carga em modo end: inicio + 30 min)
lag_lgbm <- c(lgbm = 2L)

## --- fixtures -------------------------------------------------------------------------------------------------
## root de config + models em tempdir; historicos pequenos (editaveis) para manter o deck curto.
vd_tmp <- function(env = NULL) {
  d <- tempfile("vd"); dir.create(d)             # sob o tempdir da sessao (removido no fim do processo)
  if (!is.null(env)) withr::defer(unlink(d, recursive = TRUE), envir = env)
  d
}
vd_root <- function(hw = c(30, 10), rf = c(30, 10), lgbm = c(20, 5), dessem = 20, env = NULL) {
  d <- vd_tmp(env)
  file.copy(file.path(pc_root(), "config"), d, recursive = TRUE)
  file.copy(file.path(pc_root(), "models"), d, recursive = TRUE)
  set <- function(m, tr, inf) {
    f <- file.path(d, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
    y$history_days <- list(train = tr, infer = inf); yaml::write_yaml(y, f)
  }
  set("holt-winters", hw[1L], hw[2L]); set("random-forest", rf[1L], rf[2L]); set("lgbm", lgbm[1L], lgbm[2L])
  set("dessem-regdin", dessem, dessem); set("dessem-linear", dessem, dessem)
  d
}

vd_ctx <- function(dir, root, cutoff = "available_until", ...) {
  c0 <- pc_ctx_deck(dir, ...); c0$root <- root; c0$weather_issue_cutoff <- cutoff; c0
}

.vd_tpl <- new.env()
## deck limpo memoizado por (n, areas): nao tem -1 em feriado; copiado por teste.
vd_template <- function(n = 40, areas = c("SECO", "S")) {
  key <- paste(n, paste(areas, collapse = ","))
  if (is.null(.vd_tpl[[key]])) {
    d <- vd_tmp()
    h <- pc_deck_holidays_default(areas)
    h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
    pc_deck_panel(areas = areas, holidays = h, dir = d)
    if (n != 4) pc_deck_days(d, n)
    .vd_tpl[[key]] <- d
  }
  .vd_tpl[[key]]
}
vd_deck <- function(n = 40, areas = c("SECO", "S"), env = parent.frame()) {
  src <- vd_template(n, areas)
  dst <- vd_tmp(env)
  file.copy(list.files(src, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  dst
}

md5_tree <- function(dir) {
  fs <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  fs <- fs[order(fs, method = "radix")]
  stats::setNames(unname(tools::md5sum(fs)), sub(dir, "", fs, fixed = TRUE))
}
## Roda o validate_deck conferindo que o data_dir ficou intacto (somente leitura) e que o achado e ASCII puro.
vd_run <- function(ctx, ...) {
  before <- md5_tree(ctx$data_dir)
  v <- prevcarga_validate_deck(ctx, ...)
  expect_identical(md5_tree(ctx$data_dir), before)
  expect_false(any(grepl("[^ -~]", unlist(v$findings[, c("area", "dataset", "what", "detail", "where")]))))
  expect_identical(names(v$findings), c("area", "dataset", "what", "detail", "class", "level", "where"))
  v
}
errs <- function(v) v$findings[v$findings$level == "error", ]
warns <- function(v) v$findings[v$findings$level == "warning", ]
whats <- function(v) sort(unique(v$findings$what))
has <- function(v, w, cls = NULL, level = NULL) {
  f <- v$findings[v$findings$what == w, ]
  if (!is.null(cls)) f <- f[f$class == cls, ]
  if (!is.null(level)) f <- f[f$level == level, ]
  nrow(f) > 0L
}
## FORBIDDEN: deck limpo sem nenhum achado
expect_clean <- function(v) {
  expect_true(v$ok); expect_identical(v$class, "ok"); expect_identical(nrow(v$findings), 0L)
}

root0 <- vd_root()
root_hw <- vd_root(hw = c(60, 10))      # HW exige mais historico que o deck de 40 dias

## --- catalogo e higiene do codigo -----------------------------------------------------------------------------
test_that("validation.tsv carrega pelo loader sem id duplicado e cobre os ids proprios", {
  cat <- prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))
  own <- c("contract.hive_layout", "contract.type_mismatch", "contract.parquet_unreadable", "contract.grid_off",
           "contract.duplicate_interval", "contract.load_nonpositive", "contract.temperature_out_of_range",
           "contract.code_negative", "completude.day_incomplete", "topology.scope_undeclared",
           "window.window_train_deficit", "window.window_infer_deficit", "warning.outlier", "warning.flatline",
           "warning.climate_after_cut")
  expect_true(all(own %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  tsv <- utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "validation.tsv"), quote = "", colClasses = "character")
  expect_setequal(tsv$id, own)
  expect_identical(cat$class[match(own, cat$id)], tsv$class[match(own, tsv$id)])
  ## ids de outros TSV nao sao redeclarados aqui
  for (id in c("contract.dataset_missing", "contract.tz_in_parquet", "contract.schema_missing", "completude.origin_day_gap",
               "completude.no_load", "warning.available_until_spread", "topology.area_undeclared"))
    expect_false(id %in% tsv$id)
})

test_that("validation.R: sem vetor de precedencia proprio, sem fill-0 e sem literal de janela; parse limpo", {
  f <- file.path(pc_root(), "scripts", "lib", "validation.R")
  src <- readLines(f, warn = FALSE)
  expect_error(parse(f), NA)
  expect_false(any(grepl("\"contract\".*\"completude\"|\"completude\".*\"topology\"|\"topology\".*\"window\"", src)))
  expect_true(any(grepl("prevcarga_exit_precedence(", src, fixed = TRUE)))
  expect_false(any(grepl("nafill\\([^)]*fill *= *0|fcoalesce\\([^)]*, *0\\)|replace_na\\([^)]*0\\)|\\[is\\.na\\([^]]*\\)\\] *<- *0", src)))
  expect_false(any(grepl("\\b(45|365|730|1825)\\b|DST-aware|>= ?24 slots", src)))
  expect_false(any(grepl("[^ -~]", src)))
})

test_that("veredito: ok sse nao ha erro; classe pela precedencia unica do errors.R", {
  f <- .pc_findings_bind(list(.pc_finding("A", "load", "outlier", "", "contract", level = "warning")))
  expect_identical(.prevcarga_verdict(f)$ok, TRUE)
  expect_identical(.prevcarga_verdict(f)$class, "ok")
  g <- .pc_findings_bind(list(.pc_finding("A", "load", "window_train_deficit", "", "window"),
                              .pc_finding("A", "load", "day_incomplete", "", "completude")))
  expect_identical(.prevcarga_verdict(g)$class, prevcarga_exit_precedence(c("window", "completude")))
  expect_identical(.prevcarga_verdict(g)$class, "completude")
  expect_identical(nrow(.pc_findings_bind(list(NULL))), 0L)
  expect_identical(names(.pc_findings_bind(list())), c("area", "dataset", "what", "detail", "class", "level", "where"))
})

test_that("contrato canonico como dado cobre os 4 datasets", {
  s <- .prevcarga_deck_spec()
  expect_setequal(unique(s$dataset), c("load", "weather_forecast", "weather_observed", "holidays"))
  expect_identical(s$required[s$column == "heat_index"], c(FALSE, FALSE))
  expect_true(all(s$required[s$dataset == "holidays"]))
})

## --- AC 1: FORBIDDEN deck limpo --------------------------------------------------------------------------------
test_that("AC1 FORBIDDEN: deck limpo (escopo todos os membros) nao tem achado e traz o veredito completo", {
  d <- vd_deck(); ctx <- vd_ctx(d, root0)
  v <- vd_run(ctx, origin = O)
  expect_clean(v)
  expect_identical(v$forecast_origin, O); expect_identical(v$origin_source, "flag")
  expect_identical(v$available_until, at("2026-10-02 00:00"))
  expect_identical(nrow(v$available_until_by_area), 2L)
  expect_identical(v$load_label, "end"); expect_identical(v$load_label_source, "default")
  expect_identical(v$weather_issue_cutoff, "available_until")
  expect_identical(v$weather_issue_limit, v$available_until)
  expect_identical(v$timezone$load, TZ)
  expect_identical(nrow(v$cuts), 5L)
  expect_setequal(v$scope$models, v$cuts$model)
  ## origem omitida: cai no ultimo dia de carga do deck (origin_source deck)
  v2 <- vd_run(ctx)
  expect_identical(v2$origin_source, "deck"); expect_identical(v2$forecast_origin, O + 1L)
})

test_that("AC1 deck limpo de 1900 dias com os history_days reais (>= janela dos dessem) passa", {
  d <- vd_deck(1900); real <- vd_tmp(environment())
  file.copy(file.path(pc_root(), "config"), real, recursive = TRUE)
  file.copy(file.path(pc_root(), "models"), real, recursive = TRUE)
  v <- vd_run(vd_ctx(d, real), origin = O, data_lag_days = lag_lgbm)
  expect_clean(v)
})

## --- AC 2/3: completude e nao-positivo -----------------------------------------------------------------------
test_that("AC2 NA / dia de 47 -> day_incomplete (completude); load 0 -> load_nonpositive (contract); CONTROL 48 passa", {
  ctx0 <- vd_ctx(vd_deck(), root0)
  expect_clean(vd_run(ctx0, origin = O))
  s <- stamp("2026-09-25 10:30:00")
  d1 <- vd_deck(); pc_mut_load(d1, function(dt) dt[area_code == "SECO" & timestamp == s, load_mwh := NA_real_])
  v1 <- vd_run(vd_ctx(d1, root0), origin = O)
  expect_false(v1$ok); expect_identical(v1$class, "completude")
  expect_identical(v1$findings$what[v1$findings$level == "error"], "day_incomplete")
  expect_identical(v1$findings$detail[v1$findings$what == "day_incomplete"], "2026-09-25: 47/48")
  d2 <- vd_deck(); pc_mut_load(d2, function(dt) dt[!(area_code == "SECO" & timestamp == s)])
  v2 <- vd_run(vd_ctx(d2, root0), origin = O)
  expect_identical(v2$class, "completude"); expect_true(has(v2, "day_incomplete", "completude", "error"))
  expect_identical(v2$findings$detail[v2$findings$what == "day_incomplete"], "2026-09-25: 47/48")
  d3 <- vd_deck(); pc_mut_load(d3, function(dt) dt[area_code == "SECO" & timestamp == s, load_mwh := 0])
  v3 <- vd_run(vd_ctx(d3, root0), origin = O)
  expect_false(v3$ok); expect_identical(v3$class, "contract")
  expect_identical(unique(errs(v3)$what), "load_nonpositive")   # CONTROL: RF/LGBM preencheriam com 0
  ## buraco inteiro (dia sem linha) tambem e day_incomplete com 0/48
  d4 <- vd_deck(); pc_mut_load(d4, function(dt) dt[!(area_code == "SECO" & as.Date(timestamp - 1) == as.Date("2026-09-25"))])
  v4 <- vd_run(vd_ctx(d4, root0), origin = O)
  expect_identical(v4$findings$detail[v4$findings$what == "day_incomplete"], "2026-09-25: 0/48")
})

test_that("AC3 buraco fora da janela do escopo nao reprova; dentro reprova (fronteira from-1 x from)", {
  root <- vd_root(rf = c(20, 5))
  sc <- list(model = "random-forest")
  ## RF: last_data_day = O-1, from = min(O-1-20+1, O-1-5+1) = O-20 = 2026-09-11
  hole <- function(day) {
    d <- vd_deck()
    pc_mut_load(d, function(dt) dt[!(area_code == "SECO" & timestamp == lubridate::as_datetime(paste(day, "12:30:00"), tz = "UTC"))])
    vd_run(vd_ctx(d, root), origin = O, scope = sc)
  }
  expect_clean(hole("2026-09-05"))       # antes do inicio da janela
  expect_clean(hole("2026-09-10"))       # from - 1
  vin <- hole("2026-09-11")              # from (fronteira, dentro)
  expect_true(has(vin, "day_incomplete", "completude", "error"))
  expect_true(has(hole("2026-09-20"), "day_incomplete"))
  expect_true(has(hole("2026-09-30"), "day_incomplete"))   # cauda: O-1
  ## o mesmo furo de 09-05 reprova num escopo cuja janela o contem (CONTROL: HW train 60)
  d <- vd_deck()
  pc_mut_load(d, function(dt) dt[!(area_code == "SECO" & timestamp == stamp("2026-09-05 12:30:00"))])
  expect_true(has(vd_run(vd_ctx(d, root), origin = O, scope = list(model = "holt-winters")), "day_incomplete"))
})

## --- AC 4: contrato -----------------------------------------------------------------------------------------------
test_that("AC4 oito mutacoes de contract, cada uma com o `what` proprio; CONTROL limpo", {
  one <- function(mut, what, ...) {
    d <- vd_deck(); mut(d)
    v <- expect_no_error(vd_run(vd_ctx(d, root0), origin = O, ...))
    expect_false(v$ok, label = what); expect_identical(v$class, "contract", label = what)
    expect_true(has(v, what, "contract", "error"), label = what)
    v
  }
  one(function(d) pc_mut_drop_col(d, "weather_forecast", "temperature"), "schema_missing")
  one(function(d) pc_mut_retype(d, "load", "load_mwh", "string"), "type_mismatch")
  one(function(d) pc_mut_retype(d, "load", "timestamp", "timestamp_tz"), "tz_in_parquet")
  one(function(d) pc_mut_flatten(d, "weather_observed"), "hive_layout")
  one(function(d) pc_mut_holiday(d, "simplified_code", -1L), "code_negative")
  one(function(d) pc_mut_load(d, function(dt) dt[area_code == "SECO" & timestamp == stamp("2026-09-25 10:30:00"),
                                                   timestamp := stamp("2026-09-25 10:15:00")]), "grid_off")
  one(function(d) pc_mut_load(d, function(dt) rbind(dt, dt[area_code == "SECO" & timestamp == stamp("2026-09-25 10:30:00")])),
      "duplicate_interval")
  one(function(d) pc_mut_weather(d, "weather_observed", function(dt) dt[area_code == "SECO" & timestamp == stamp("2026-09-25 10:00:00"),
                                                                        temperature := 60]), "temperature_out_of_range")
  ## temperatura prevista fora da faixa tambem (dataset proprio)
  v <- one(function(d) pc_mut_weather(d, "weather_forecast", function(dt) { dt[which(dt$area_code == "SECO" & dt$issue_time == stamp("2026-10-01 12:00:00"))[1L], temperature := -30]; dt }),
           "temperature_out_of_range")
  expect_identical(unique(errs(v)$dataset), "weather_forecast")
})

test_that("AC4 heat_index opcional ausente da ok == TRUE; presente com tipo errado nao reprova o dataset", {
  d <- vd_deck()
  pc_mut_drop_col(d, "weather_forecast", "heat_index"); pc_mut_drop_col(d, "weather_observed", "heat_index")
  expect_clean(vd_run(vd_ctx(d, root0), origin = O))
  d2 <- vd_deck(); pc_mut_retype(d2, "weather_observed", "heat_index", "string")
  v2 <- vd_run(vd_ctx(d2, root0), origin = O)
  expect_identical(unique(errs(v2)$what), "type_mismatch")
  expect_identical(v2$findings$where[v2$findings$what == "type_mismatch"], "weather_observed.heat_index")
})

test_that("AC4 parquet corrompido: parquet_unreadable com where relativo ao data_dir, nenhum erro R escapa", {
  d <- vd_deck()
  rel <- "load/area_code=SECO/year=2026/part-0.parquet"
  expect_true(file.exists(file.path(d, rel)))
  pc_mut_corrupt(d, rel)
  v <- expect_no_error(vd_run(vd_ctx(d, root0), origin = O))
  expect_false(v$ok); expect_identical(v$class, "contract")
  f <- v$findings[v$findings$what == "parquet_unreadable", ]
  expect_identical(f$where, rel); expect_identical(f$class, "contract"); expect_identical(f$level, "error")
  ## CONTROL: sem corromper, nada
  expect_clean(vd_run(vd_ctx(vd_deck(), root0), origin = O))
})

test_that("dataset ausente aparece como dataset_missing e nao cascateia findings do dataset", {
  d <- vd_deck(); unlink(file.path(d, "auxiliary", "holidays"), recursive = TRUE)
  v <- vd_run(vd_ctx(d, root0), origin = O)
  expect_identical(whats(v), "dataset_missing"); expect_identical(v$class, "contract")
})

## --- AC 5: avisos -------------------------------------------------------------------------------------------------
vd_warn_deck <- function(issues = 12L, flat = 6L, env = parent.frame()) {
  d <- vd_deck(env = env)
  pc_mut_load(d, function(dt) {
    dt[area_code == "SECO" & timestamp == stamp("2026-09-20 12:00:00"), load_mwh := 1e6]
    hi <- if (flat == 6L) "2026-09-15 13:30:00" else "2026-09-15 13:00:00"
    dt[area_code == "S" & timestamp >= stamp("2026-09-15 10:30:00") & timestamp < stamp(hi), load_mwh := 1024.5]
    dt
  })
  pc_mut_issue(d, issues)
  pc_deck_until(d, at("2026-10-01 08:30"))
  pc_deck_drop(d, "S", at("2026-10-01 08:00"))     # S ate 08:00, SECO ate 08:30 => spread; global 08:00
  d
}

test_that("AC5 outlier, flatline, climate_after_cut (12Z, available_until) e available_until_spread sao warning; ok TRUE", {
  d <- vd_warn_deck()
  v <- vd_run(vd_ctx(d, root0), origin = O)
  expect_true(v$ok); expect_identical(v$class, "ok")
  expect_identical(v$available_until, at("2026-10-01 08:00"))
  expect_setequal(whats(v), c("outlier", "flatline", "climate_after_cut", "available_until_spread"))
  expect_true(all(v$findings$level == "warning")); expect_identical(nrow(errs(v)), 0L)
  expect_identical(v$findings$area[v$findings$what == "outlier"], "SECO")
  expect_identical(v$findings$area[v$findings$what == "flatline"], "S")
  ## origin_midnight_utc: 12Z (09:00) segue tardia
  v2 <- vd_run(vd_ctx(d, root0, cutoff = "origin_midnight_utc"), origin = O)
  expect_true(v2$ok); expect_true(has(v2, "climate_after_cut", level = "warning"))
})

test_that("AC5 flatline: fronteira 5 x 6 intervalos identicos", {
  expect_false(has(vd_run(vd_ctx(vd_warn_deck(flat = 5L), root0), origin = O), "flatline"))
  expect_true(has(vd_run(vd_ctx(vd_warn_deck(flat = 6L), root0), origin = O), "flatline", level = "warning"))
})

test_that("AC5 climate_after_cut: 06Z so e tardia em origin_midnight_utc; 12Z x 18Z com corte 10:00", {
  d <- vd_deck(); pc_mut_issue(d, 6L); pc_deck_until(d, at("2026-10-01 08:00"))
  expect_clean(vd_run(vd_ctx(d, root0), origin = O))     # limite 08:00; 06Z = 03:00 local
  v <- vd_run(vd_ctx(d, root0, cutoff = "origin_midnight_utc"), origin = O)
  expect_true(v$ok); expect_true(has(v, "climate_after_cut", level = "warning"))
  d12 <- vd_deck(); pc_mut_issue(d12, 12L); pc_deck_until(d12, at("2026-10-01 10:00"))
  expect_clean(vd_run(vd_ctx(d12, root0), origin = O))   # 12Z = 09:00 <= 10:00
  d18 <- vd_deck(); pc_mut_issue(d18, 18L); pc_deck_until(d18, at("2026-10-01 10:00"))
  v18 <- vd_run(vd_ctx(d18, root0), origin = O)          # 18Z = 15:00 > 10:00
  expect_true(v18$ok); expect_true(has(v18, "climate_after_cut", level = "warning"))
})

## --- AC 6: janelas ------------------------------------------------------------------------------------------------
test_that("AC6 janelas por membro: treino ate last_data_day, inferencia ate O-1, findings distintos", {
  ## deck de 6 dias (09-27..10-02): HW 60/10 e RF 30/10 deficitam treino e inferencia
  d <- vd_deck(40)                                 # 40 dias: suficiente para RF 30/10, insuficiente para HW 60 (treino)
  v <- vd_run(vd_ctx(d, root_hw), origin = O, scope = list(model = "holt-winters"))
  expect_false(v$ok); expect_identical(v$class, "window")
  expect_true(has(v, "window_train_deficit", "window", "error")); expect_false(has(v, "window_infer_deficit"))
  v_rf <- vd_run(vd_ctx(d, root0), origin = O, scope = list(model = "random-forest"))
  expect_clean(v_rf)                                    # 400d/RF passa (aqui 40 dias >= 30)
  d6 <- vd_deck(6)
  v6 <- vd_run(vd_ctx(d6, root_hw), origin = O, scope = list(model = "holt-winters"))
  expect_identical(v6$class, "window")
  expect_true(has(v6, "window_train_deficit")); expect_true(has(v6, "window_infer_deficit"))
  expect_identical(unique(v6$findings$where[v6$findings$what == "window_infer_deficit"]),
                   "models/holt-winters/run.yaml:history_days.infer")
  v6r <- vd_run(vd_ctx(d6, root0), origin = O, scope = list(model = "random-forest"))
  expect_true(has(v6r, "window_train_deficit")); expect_true(has(v6r, "window_infer_deficit"))
  ## um impl que ignore history_days.infer falharia aqui: HW com infer=10 mas deck de 40 dias => so treino
  expect_identical(sort(unique(v6$findings$what)), c("window_infer_deficit", "window_train_deficit"))
})

test_that("AC6 LGBM: janela de treino termina em O-3 (data_lag_days); contar ate O-1 nao detecta o deficit", {
  ## lgbm train 20 infer 5; deck de 23 dias => primeiro dia O-21: ate O-3 tem 19 (<20); ate O-1 teria 20
  d <- vd_deck(23)
  v <- vd_run(vd_ctx(d, root0), origin = O, scope = list(model = "lgbm"), data_lag_days = lag_lgbm)
  expect_identical(v$class, "window"); expect_true(has(v, "window_train_deficit"))
  expect_false(has(v, "window_infer_deficit"))
  expect_identical(v$cuts$last_data_day, O - 3L)
  ## CONTROL: sem o lag do descriptor o mesmo deck passa; e com 1 dia a mais ate O-3 tambem
  expect_clean(vd_run(vd_ctx(d, root0), origin = O, scope = list(model = "lgbm")))
  expect_clean(vd_run(vd_ctx(vd_deck(24), root0), origin = O, scope = list(model = "lgbm"), data_lag_days = lag_lgbm))
})

## --- AC 7: topologia -----------------------------------------------------------------------------------------------
test_that("AC7 area fora da topologia -> area_undeclared; escopo area XX / modelo yy -> scope_undeclared (topology)", {
  d <- vd_deck(); pc_mut_add_area(d, "XX")
  v <- vd_run(vd_ctx(d, root0), origin = O)
  expect_false(v$ok); expect_identical(v$class, "topology"); expect_true(has(v, "area_undeclared", "topology", "error"))
  expect_identical(v$findings$area[v$findings$what == "area_undeclared"], "XX")
  expect_false("XX" %in% v$scope$areas)                  # a area indeclarada nao entra nas checagens de dados
  ctx <- vd_ctx(vd_deck(), root0)
  va <- vd_run(ctx, origin = O, scope = list(area = "XX"))
  expect_identical(va$class, "topology"); expect_true(has(va, "scope_undeclared", "topology", "error"))
  vm <- vd_run(ctx, origin = O, scope = list(model = "yy"))
  expect_identical(vm$class, "topology"); expect_identical(whats(vm), "scope_undeclared")   # sem model_spec => sem contract
  expect_identical(vm$findings$where, "yy")
  expect_clean(vd_run(vd_ctx(vd_deck(40, c("RS", "PR", "SC", "S")), root0), origin = O,
                      scope = list(area = "S", model = "lgbm")))                             # CONTROL: S expande p/ as folhas
})

test_that("AC7 area = SECO expande para as folhas do subsistema + ele mesmo e nao gera finding de escopo", {
  topo <- venv$prevcarga_topology(path = file.path(root0, "config", "topology.yaml"))
  leaves <- topo$subsystems$SECO$areas
  d <- vd_deck(40, areas = c(leaves, "SECO"))
  v <- vd_run(vd_ctx(d, root0), origin = O, scope = list(area = "SECO"))
  expect_false(has(v, "scope_undeclared"))
  expect_setequal(v$scope$areas, c(leaves, "SECO"))
  expect_clean(v)
})

## --- AC 8: acumulacao e precedencia ---------------------------------------------------------------------------------
test_that("AC8 acumula classes (nao aborta no 1o defeito); dominante contract pela precedencia do errors.R", {
  d <- vd_deck(25)                                   # RF 30/10: deficit de janela
  pc_mut_drop_col(d, "weather_observed", "temperature", area = "S")
  pc_mut_load(d, function(dt) dt[area_code == "SECO" & timestamp == stamp("2026-09-25 10:30:00"), load_mwh := NA_real_])
  v <- vd_run(vd_ctx(d, root0), origin = O)
  cls <- unique(errs(v)$class)
  expect_true(all(c("contract", "completude", "window") %in% cls))
  expect_identical(v$class, prevcarga_exit_precedence(cls)); expect_identical(v$class, "contract")
  expect_true(has(v, "schema_missing")); expect_true(has(v, "day_incomplete")); expect_true(has(v, "window_train_deficit"))
})

test_that("AC8 so NA + area fora da topologia: dominante completude", {
  d <- vd_deck(); pc_mut_add_area(d, "XX")
  pc_mut_load(d, function(dt) dt[area_code == "SECO" & timestamp == stamp("2026-09-25 10:30:00"), load_mwh := NA_real_])
  v <- vd_run(vd_ctx(d, root0), origin = O)
  expect_setequal(unique(errs(v)$class), c("completude", "topology"))
  expect_identical(v$class, "completude")
  expect_identical(v$class, prevcarga_exit_precedence(unique(errs(v)$class)))
})

test_that("AC8 buraco 07:00-07:30 no dia O: abort do leitor vira finding origin_day_gap; available_until nunca vale 07:00", {
  d <- vd_deck(); pc_deck_drop(d, "SECO", at(c("2026-10-01 07:00", "2026-10-01 07:30")))
  v <- expect_no_error(vd_run(vd_ctx(d, root0), origin = O))
  expect_false(v$ok); expect_identical(v$class, "completude")
  f <- v$findings[v$findings$what == "origin_day_gap", ]
  expect_identical(nrow(f), 1L); expect_identical(f$class, "completude"); expect_identical(f$where, "SECO:07:00")
  expect_identical(f$area, "SECO")
  expect_false(isTRUE(v$available_until == at("2026-10-01 07:00")))
  expect_null(v$available_until); expect_null(v$cuts)
  expect_identical(v$forecast_origin, O)
})

test_that("AC8 no_load capturado: origem sem carga no deck", {
  d <- vd_deck()
  v <- expect_no_error(vd_run(vd_ctx(d, root0), origin = O + 60L))
  expect_false(v$ok); expect_identical(v$class, "completude")
  expect_true(has(v, "no_load", "completude", "error")); expect_null(v$available_until)
})

## --- AC 9: manifest e somente leitura -------------------------------------------------------------------------------
test_that("AC9 sem manifest.json: finding contract manifest_missing; layout/schema/tipos/feriados/topologia seguem acumulando", {
  d <- vd_deck(); pc_mut_add_area(d, "XX")
  pc_mut_holiday(d, "simplified_code", -1L); pc_mut_drop_col(d, "weather_forecast", "temperature")
  file.remove(file.path(d, "manifest.json"))
  v <- vd_run(vd_ctx(d, root0), origin = O)
  expect_identical(v$class, "contract")
  expect_true(has(v, "manifest_missing", "contract", "error"))
  expect_true(all(c("code_negative", "schema_missing", "area_undeclared") %in% whats(v)))
  expect_null(v$timezone); expect_null(v$available_until); expect_null(v$load_label_source)   # nenhum fuso default
  expect_clean(vd_run(vd_ctx(vd_deck(), root0), origin = O))                                    # CONTROL
})

test_that("AC9 load_label em conflito com a flag: finding contract load_label_conflict e o resto segue", {
  d <- vd_deck(); pc_deck_manifest(d, load_label = "start")
  pc_mut_holiday(d, "simplified_code", -1L)
  v <- vd_run(vd_ctx(d, root0, load_label = "end", source = "flag"), origin = O)
  expect_identical(v$class, "contract")
  expect_true(has(v, "load_label_conflict", "contract", "error")); expect_true(has(v, "code_negative"))
  expect_null(v$available_until)
  ## CONTROL: flag igual ao manifest nao conflita
  d2 <- vd_deck(); pc_deck_manifest(d2, load_label = "start")
  ## o deck do template tem os digitos em modo end; com manifest start os dados deslocam, mas nao ha conflito
  v2 <- vd_run(vd_ctx(d2, root0, load_label = "start", source = "flag"), origin = O)
  expect_false(has(v2, "load_label_conflict"))
})

test_that("somente leitura: o validate_deck nao cria nem altera arquivos do data_dir (inclui cenarios com erro)", {
  d <- vd_deck(); pc_mut_corrupt(d, "load/area_code=S/year=2026/part-0.parquet"); pc_mut_add_area(d, "XX")
  before <- md5_tree(d)
  invisible(prevcarga_validate_deck(vd_ctx(d, root0), origin = O))
  expect_identical(md5_tree(d), before)
})

test_that("erro de uso (config) nao e capturado: weather_issue_cutoff invalido segue como abort config", {
  d <- vd_deck(); ctx <- vd_ctx(d, root0, cutoff = "nao_existe")
  e <- tryCatch(prevcarga_validate_deck(ctx, origin = O), prevcarga_error = identity)
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$class_name, "config")
  ctx2 <- vd_ctx(d, root0); ctx2$data_dir <- NULL
  e2 <- tryCatch(prevcarga_validate_deck(ctx2, origin = O), prevcarga_error = identity)
  expect_identical(e2$class_name, "config")
})

## --- Bracos dependentes de ADR pendente (skip com motivo) -------------------------------------------------------
test_that("[TBD: ADR-0024] weather_issue_cutoff com regra adicional por modelo", {
  skip("[TBD: ADR-0024] a regra weather_issue_cutoff por modelo depende do ADR pendente; hoje so as duas regras globais")
})
test_that("[TBD: ADR-0027] precedencia entre aviso de spread e corte global por area", {
  skip("[TBD: ADR-0027] politica de available_until por area (spread) aguarda decisao; hoje so o aviso")
})
