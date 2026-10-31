## test-deck-reader.R -- deck_reader.R (M2.1, parte fuso/rotulo). Assercoes por error.id/classe, nunca por mensagem.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "deck_reader"))
for (n in c("prevcarga_deck_manifest", "prevcarga_resolve_load_label", "prevcarga_read_load",
            "prevcarga_read_weather", "prevcarga_deck_probe"))
  assign(n, env[[n]])

D <- lubridate::ymd(c("2026-09-30", "2026-10-01"))
TZ <- "Etc/GMT+3"
cols4 <- c("area_code", "interval_start", "interval_end", "load_mwh")
fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
rd <- function(dir, ctx = pc_ctx_deck(dir), ...) prevcarga_read_load(ctx, from = D[1], to = D[2], ...)

test_that("catalogo do leitor carrega pelo loader sem id duplicado", {
  cat <- env$prevcarga_errors_load(file.path(pc_root(), "scripts", "lib", "errors"))
  ids <- c("contract.manifest_missing", "contract.manifest_invalid", "contract.manifest_unknown_key",
           "contract.tz_missing", "contract.tz_invalid", "contract.tz_in_parquet",
           "contract.load_label_conflict", "contract.dataset_missing", "contract.weather_lag_missing")
  expect_true(all(ids %in% cat$id))
  expect_false(anyDuplicated(cat$id) > 0L)
  expect_true(all(cat$class[cat$id %in% ids] == "contract"))
})

test_that("[FORBIDDEN] sentinela end com flag == start do manifest; [CONTROL] flag errada desloca", {
  d_end <- pc_deck_sentinel("end", days = D); d_sta <- pc_deck_sentinel("start", days = D)
  a <- rd(d_end, pc_ctx_deck(d_end, "end", "flag"))
  b <- rd(d_sta)
  expect_identical(a[, .(area_code, interval_start, interval_end, load_mwh)],
                   b[, .(area_code, interval_start, interval_end, load_mwh)])
  expect_false("timestamp" %in% names(a))
  expect_true(all(a$interval_end - a$interval_start == lubridate::minutes(30)))
  expect_identical(attr(b, "load_label"), "start"); expect_identical(attr(b, "load_label_source"), "manifest")
  expect_identical(attr(a, "load_label_source"), "flag"); expect_identical(attr(a, "tz"), TZ)
  ## valor do slot k no intervalo k
  s <- a[area_code == "A"]
  expect_identical(s$load_mwh, as.double(rep(1:48, 2L) + 1000L))
  expect_identical(format(s$interval_start[1L], "%H:%M"), "00:00")
  pc_deck_manifest(d_sta, load_label = NULL)           # CONTROL: deck de inicio lido como fim
  cc <- rd(d_sta, pc_ctx_deck(d_sta, "end", "flag"))
  expect_false(isTRUE(all.equal(cc$interval_start, b$interval_start)))
  expect_false(identical(cc[area_code == "A"]$load_mwh[1:48], as.double(1:48 + 1000L)) &&
               identical(format(cc[area_code == "A"]$interval_start[1L], "%H:%M"), "00:00"))
})

test_that("agrupamento por hora/dia sobre interval_start; CONTROL por interval_end reprova", {
  d <- pc_deck_sentinel("end", days = D)
  x <- rd(d, pc_ctx_deck(d))[area_code == "A"]
  h <- x[, .(s = sum(load_mwh)), by = .(h = lubridate::floor_date(interval_start, "hour"))]
  h9 <- h[h == lubridate::ymd_hms("2026-09-30 09:00:00", tz = TZ)]$s
  expect_identical(h9, sum(c(19, 20) + 1000))             # slots com inicio 09:00 e 09:30
  dd <- x[, .(n = .N, first = min(interval_start), last = max(interval_start)),
          by = .(day = lubridate::as_date(interval_start, tz = TZ))]
  expect_true(all(dd$n == 48L))
  expect_identical(dd$first[1L], lubridate::ymd_hms("2026-09-30 00:00:00", tz = TZ))
  expect_identical(dd$last[1L], lubridate::ymd_hms("2026-09-30 23:30:00", tz = TZ))
  ## CONTROL: o mesmo agrupamento sobre interval_end junta 09:30+10:00 (slots 20 e 21)
  hc <- x[, .(s = sum(load_mwh)), by = .(h = lubridate::floor_date(interval_end, "hour"))]
  hc9 <- hc[h == lubridate::ymd_hms("2026-09-30 09:00:00", tz = TZ)]$s
  expect_false(identical(hc9, h9))
  expect_identical(hc9, sum(c(18, 19) + 1000))
})

test_that("matriz de precedencia do load_label (manifest > flag > config > end)", {
  d_sta <- pc_deck_sentinel("start", days = D); d_end <- pc_deck_sentinel("end", days = D)
  m_sta <- prevcarga_deck_manifest(d_sta); m_end <- prevcarga_deck_manifest(d_end)
  e <- fail(prevcarga_resolve_load_label(pc_ctx_deck(d_sta, "end", "flag"), m_sta))
  expect_identical(e$id, "contract.load_label_conflict"); expect_identical(e$class_name, "contract")
  expect_identical(e$exit_code, 2L)
  expect_identical(prevcarga_resolve_load_label(pc_ctx_deck(d_sta), m_sta), list(value = "start", source = "manifest"))
  expect_identical(prevcarga_resolve_load_label(pc_ctx_deck(d_sta, "start", "flag"), m_sta),
                   list(value = "start", source = "manifest"))
  expect_identical(prevcarga_resolve_load_label(pc_ctx_deck(d_sta, "end", "config"), m_sta),
                   list(value = "start", source = "manifest"))
  expect_identical(prevcarga_resolve_load_label(pc_ctx_deck(d_end, "start", "config"), m_end),
                   list(value = "start", source = "config"))
  expect_identical(prevcarga_resolve_load_label(pc_ctx_deck(d_end, "start", "flag"), m_end),
                   list(value = "start", source = "flag"))
  expect_identical(prevcarga_resolve_load_label(pc_ctx_deck(d_end), m_end), list(value = "end", source = "default"))
})

test_that("manifests mutados abortam [contract] com error.where do dataset.coluna/chave; nunca fuso default", {
  d <- pc_deck_sentinel("end", days = D)
  ok <- prevcarga_deck_manifest(d)
  expect_identical(ok$timezone$load, TZ); expect_identical(ok$timezone$weather_forecast$issue_time, "UTC")
  chk <- function(mutate, id, where) {
    pc_deck_manifest(d, mutate = mutate)
    e <- fail(prevcarga_deck_manifest(d))
    expect_identical(e$id, id); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
    expect_identical(e$where, where)
  }
  chk(function(m) { m$timezone$load <- NULL; m }, "contract.tz_missing", "load.timestamp")
  chk(function(m) { m$timezone$weather_forecast$issue_time <- NULL; m }, "contract.tz_missing",
      "weather_forecast.issue_time")
  chk(function(m) { m$timezone$load <- "Mars/Base"; m }, "contract.tz_invalid", "load.timestamp")
  chk(function(m) { m$corte <- "06:00"; m }, "contract.manifest_unknown_key", "corte")
  chk(function(m) { m$timezone$extra <- "UTC"; m }, "contract.manifest_unknown_key", "timezone.extra")
  chk(function(m) { m$schema_version <- 2L; m }, "contract.manifest_invalid", "manifest.json:schema_version")
  chk(function(m) { m$load_label <- "meio"; m }, "contract.manifest_invalid", "manifest.json:load_label")
  writeLines("{ nao e json", file.path(d, "manifest.json"))
  e <- fail(prevcarga_deck_manifest(d))
  expect_identical(e$id, "contract.manifest_invalid"); expect_identical(e$where, "manifest.json")
  unlink(file.path(d, "manifest.json"))
  e <- fail(prevcarga_deck_manifest(d))
  expect_identical(e$id, "contract.manifest_missing"); expect_identical(e$where, "manifest.json")
  e <- fail(prevcarga_read_load(pc_ctx_deck(d), from = D[1], to = D[2]))
  expect_identical(e$id, "contract.manifest_missing")
})

test_that("lag de publicacao do clima previsto (ADR-0024): obrigatorio com weather_forecast, inteiro >= 0, sem default", {
  d <- pc_deck_sentinel("end", days = D)
  expect_identical(prevcarga_deck_manifest(d)$weather_forecast_publication_lag_hours, 0L)
  pc_deck_manifest(d, weather_lag_hours = 6L)
  expect_identical(prevcarga_deck_manifest(d)$weather_forecast_publication_lag_hours, 6L)
  W <- "manifest.json:weather_forecast_publication_lag_hours"
  chk <- function(mutate, id, where = W) {
    pc_deck_manifest(d, mutate = mutate)
    e <- fail(prevcarga_deck_manifest(d))
    expect_identical(e$id, id); expect_identical(e$class_name, "contract"); expect_identical(e$exit_code, 2L)
    expect_identical(e$where, where)
  }
  chk(function(m) { m$weather_forecast_publication_lag_hours <- NULL; m }, "contract.weather_lag_missing")
  chk(function(m) { m$weather_forecast_publication_lag_hours <- -1L; m }, "contract.manifest_invalid")
  chk(function(m) { m$weather_forecast_publication_lag_hours <- "6"; m }, "contract.manifest_invalid")
  chk(function(m) { m$weather_forecast_publication_lag_hours <- 5.5; m }, "contract.manifest_invalid")
  chk(function(m) { m$weather_forecast_publication_lag_hours <- c(6L, 6L); m }, "contract.manifest_invalid")
  chk(function(m) { m$weather_forecast_publication_lag_hours <- list(h = 6L); m }, "contract.manifest_invalid")
  chk(function(m) { m$weather_forecast_publication_lag <- 6L; m }, "contract.manifest_unknown_key",
      "weather_forecast_publication_lag")
  ## sem chave => o leitor do previsto aborta (nada de default silencioso)
  pc_deck_manifest(d, mutate = function(m) { m$weather_forecast_publication_lag_hours <- NULL; m })
  expect_identical(fail(prevcarga_read_weather(pc_ctx_deck(d), "forecast", from = D[1], to = D[2]))$id,
                   "contract.weather_lag_missing")
  ## deck sem clima previsto (nem dataset nem fuso): a chave nao e exigida
  pc_deck_manifest(d, mutate = function(m) {
    m$timezone$weather_forecast <- NULL; m$weather_forecast_publication_lag_hours <- NULL; m })
  unlink(file.path(d, "weather", "forecast"), recursive = TRUE)
  m <- prevcarga_deck_manifest(d)
  expect_null(m$weather_forecast_publication_lag_hours); expect_null(m$timezone$weather_forecast)
})

test_that("[FORBIDDEN] issue_time UTC 00Z vira 21:00 do dia anterior; [CONTROL] fuso trocado deixa 00:00", {
  d <- pc_deck_sentinel("end", days = D)
  fc <- prevcarga_read_weather(pc_ctx_deck(d), "forecast", from = D[1], to = D[2])
  expect_identical(attr(fc$issue_time, "tzone"), TZ); expect_identical(attr(fc$valid_time, "tzone"), TZ)
  want <- lubridate::ymd_hms("2026-09-30 21:00:00", tz = TZ)
  expect_true(want %in% fc$issue_time)
  expect_true(all(sort(unique(format(fc$issue_time, "%H"))) == c("03", "09", "15", "21")))
  ## valid_time mantem os digitos do parquet: emissao 00Z (21:00 local) + 1 h => 22:00 local
  one <- fc[area_code == "A" & issue_time == want]
  expect_identical(min(one$valid_time), lubridate::ymd_hms("2026-09-30 22:00:00", tz = TZ))
  pc_deck_manifest(d, issue_tz = TZ)                    # CONTROL: declarar o fuso errado
  fc2 <- prevcarga_read_weather(pc_ctx_deck(d), "forecast", from = D[1], to = D[2])
  expect_false(want %in% fc2$issue_time)
  expect_true(lubridate::ymd_hms("2026-10-01 00:00:00", tz = TZ) %in% fc2$issue_time)
  obs <- prevcarga_read_weather(pc_ctx_deck(d), "observed", areas = "B", from = D[1], to = D[1])
  expect_identical(unique(obs$area_code), "B"); expect_identical(nrow(obs), 24L)
  expect_identical(attr(obs$valid_time, "tzone"), TZ)
  expect_identical(format(obs$valid_time[1L], "%Y-%m-%d %H"), "2026-09-30 00")
  ## F4: o observado sai do leitor com `valid_time`; a coluna crua `timestamp` do store nao atravessa a borda
  expect_false("timestamp" %in% names(obs))
  expect_identical(data.table::key(obs), c("area_code", "valid_time"))
})

test_that("[FORBIDDEN] delta=0 sob TZ/locale de sessao diferentes antes do runtime", {
  d <- pc_deck_sentinel("end", days = D)
  pins <- c(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
  session <- function(tz, lc) {
    withr::with_timezone(tz, withr::with_envvar(pins, {
      withr::with_locale(c(LC_CTYPE = Sys.getlocale("LC_CTYPE"), LC_COLLATE = Sys.getlocale("LC_COLLATE"),
                           LC_NUMERIC = Sys.getlocale("LC_NUMERIC")), {
        suppressWarnings(Sys.setlocale("LC_CTYPE", lc))
        env$prevcarga_runtime_enforce()
        list(load = prevcarga_read_load(pc_ctx_deck(d), from = D[1], to = D[2]),
             obs = prevcarga_read_weather(pc_ctx_deck(d), "observed", from = D[1], to = D[2]),
             fc = prevcarga_read_weather(pc_ctx_deck(d), "forecast", from = D[1], to = D[2]))
      })
    }))
  }
  a <- session("UTC", "C.UTF-8")
  b <- session("America/Sao_Paulo", "pt_BR.UTF-8")
  expect_identical(a, b)
  for (x in list(a$load$interval_start, a$load$interval_end, a$obs$valid_time, a$fc$issue_time, a$fc$valid_time))
    expect_identical(attr(x, "tzone"), TZ)
})

test_that("tz_in_parquet na carga e dataset ausente no probe", {
  d <- pc_deck_sentinel("end", days = D)
  ## regrava um parquet de load com timestamp[ns, tz=UTC]
  f <- list.files(file.path(d, "load"), pattern = "parquet$", recursive = TRUE, full.names = TRUE)
  t <- arrow::read_parquet(f[[1L]], as_data_frame = FALSE)
  t$timestamp <- t$timestamp$cast(arrow::timestamp("ns", timezone = "UTC"))
  arrow::write_parquet(t, f[[1L]])
  unlink(file.path(d, "weather", "observed"), recursive = TRUE)
  pr <- prevcarga_deck_probe(d)
  expect_true(pr$load$present); expect_true(pr$load$hive_ok)
  expect_true(pr$load$tz_in_parquet[["timestamp"]])
  expect_true(all(c("timestamp", "load_mwh") %in% pr$load$columns))
  expect_true(startsWith(pr$load$types[["load_mwh"]], "double"))
  expect_true(all(startsWith(pr$load$files, "load/area_code=")))
  expect_identical(pr$load$unreadable, character())
  expect_false(pr$weather_observed$present)
  expect_true(pr$weather_forecast$present); expect_false(pr$holidays$present)
  e <- fail(prevcarga_read_load(pc_ctx_deck(d), from = D[1], to = D[2]))
  expect_identical(e$id, "contract.tz_in_parquet"); expect_identical(e$where, "load.timestamp")
  e <- fail(prevcarga_read_weather(pc_ctx_deck(d), "observed", from = D[1], to = D[2]))
  expect_identical(e$id, "contract.dataset_missing"); expect_identical(e$where, "weather_observed")
})

test_that("probe marca parquet ilegivel e Hive fora do padrao sem coletar linhas", {
  d <- pc_deck_sentinel("end", days = D)
  writeLines("lixo", file.path(d, "load", "area_code=A", "year=2026", "ruim.parquet"))
  pr <- prevcarga_deck_probe(d)
  expect_true(any(grepl("ruim.parquet", pr$load$unreadable, fixed = TRUE)))
  expect_true(pr$load$hive_ok)
  dir.create(file.path(d, "load", "solto"))
  file.copy(list.files(file.path(d, "load"), "parquet$", recursive = TRUE, full.names = TRUE)[1L],
            file.path(d, "load", "solto", "x.parquet"))
  expect_false(prevcarga_deck_probe(d)$load$hive_ok)
})

test_that("leitor nao le env nem relogio e e o unico a abrir manifest/dataset", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "deck_reader.R"), warn = FALSE)
  code <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("Sys\\.getenv|Sys\\.time|Sys\\.Date|as\\.POSIXct|strptime|as\\.Date", code)))
  ## verify_repro.R (M8.7) le o manifest.json da ENTREGA do forecast, nao o do deck (mesma excecao de test-static-greps-m2.R)
  others <- setdiff(list.files(file.path(pc_root(), "scripts", "lib"), pattern = "\\.R$", full.names = TRUE),
                    file.path(pc_root(), "scripts", "lib", c("deck_reader.R", "verify_repro.R")))
  hits <- vapply(others, function(f) {
    l <- readLines(f, warn = FALSE); any(grepl("open_dataset|^(?=.*(?<![[:alnum:]_.])manifest[.]json)(?:(?!.*((?<![A-Za-z])write|[.]export_write_json))|(?=.*((?<![A-Za-z])(read|fromJSON|parse_json|scan)|\\bfile\\())).*$", l[!grepl("^\\s*#", l)], perl = TRUE))
  }, NA)
  expect_false(any(hits[!grepl("/tests/", others)]))
})
