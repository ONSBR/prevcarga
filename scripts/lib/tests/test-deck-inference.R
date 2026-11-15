## test-deck-inference.R -- verbo deck-inference deterministico (E2-T4). Deck de treino sintetico (helper-deck.R) e artefatos-brinquedo
## com inference_needs em tempdir; o verbo roda em processo via prevcarga_main. Nunca toca data/ nem o store de referencia.
## Anexo de feriados no layout decidido pelo Gabriel (subdeck com manifest proprio; ADR-044; resolucao 01M7FHWSY20K1FXP3RZNT0XNW0).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
for (m in c("errors", "runtime", "config", "banner", "cli"))
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())
lib <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "holiday_islands", "deck_reader", "dates",
                       "validation", "features", "inference_needs", "inference", "deck_inference"))

TZ <- "Etc/GMT+3"
O <- lubridate::ymd("2023-06-01")
O_09h <- lubridate::ymd_hm("2023-06-01 09:00", tz = TZ)
AREAS <- c("RJ", "RS")
TOPO <- c("schema_version: 1", "khat_range: [1.00, 1.15]", "subsystems:", "  S: {areas: [RS, RJ], loss: PS}", "order: [S]",
          "members:", "  - {sigla: RF, mdir: random-forest, exogena: null}", "  - {sigla: HW, mdir: holt-winters, exogena: null}")
ISLAND_DAYS <- c(seq(lubridate::ymd("2018-05-24"), lubridate::ymd("2018-05-31"), by = "day"),
                 seq(lubridate::ymd("2019-06-13"), lubridate::ymd("2019-06-20"), by = "day"))

tmp <- function(env) { d <- tempfile("di"); dir.create(d); withr::defer(unlink(d, recursive = TRUE), envir = env); d }

root_copy <- function(env) {
  d <- tmp(env)
  file.copy(file.path(pc_root(), c("config", "models")), d, recursive = TRUE)
  dir.create(file.path(d, "scripts"))
  file.copy(file.path(pc_root(), "scripts", "lib"), file.path(d, "scripts"), recursive = TRUE)
  unlink(file.path(d, "scripts", "lib", "tests"), recursive = TRUE)
  writeLines(TOPO, file.path(d, "config", "topology.yaml"))
  for (m in c("holt-winters", "random-forest")) {
    f <- file.path(d, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
    y$history_days <- list(train = 30, infer = 10); yaml::write_yaml(y, f)
  }
  d
}

## necessidade-brinquedo (schema v1): carga/observado 9 dias, previsto min_lead lead 1 em O+1 e freshest [0,1], feriados [-3, +2]
needs_toy <- function(area, annex = FALSE) list(
  schema_version = 1L, load = list(days_before = 9L), weather_observed = list(days_before = 9L),
  weather_forecast = list(area = area, segments = list(
    list(kind = "min_lead", from = 1L, to = 1L, lead = 1L), list(kind = "freshest", from = 0L, to = 1L))),
  holidays = list(from = -3L, to = 2L), holiday_annex = annex)

HOL <- data.table::data.table(area_code = "RJ", date = lubridate::ymd(c("2018-05-31", "2019-06-20", "2023-05-30", "2023-06-08")),
                              holiday_type_id = bit64::as.integer64(1L), prevcarga_code = bit64::as.integer64(c(5L, 5L, 9L, 5L)),
                              simplified_code = bit64::as.integer64(1L))

## deck de treino: 40 dias ate O (carga ate O 09:00), 4 emissoes/dia, feriados; `islands` acrescenta dias de 2018/2019
train_deck <- function(env, areas = AREAS, islands = FALSE) {
  d <- tmp(env)
  days <- seq(O - 39L, O, by = "day")
  if (islands) days <- c(ISLAND_DAYS, days)
  pc_deck_sentinel("end", days = days, areas = areas, until = O_09h, dir = d)
  .pc_deck_write(HOL[area_code %in% areas], d, file.path("auxiliary", "holidays"), character(), "date")
  d
}

publish <- function(art, members, areas, needs = needs_toy, skip = function(m, a) FALSE) {
  ctx <- list(artifacts_dir = art)
  for (i in seq_len(nrow(members))) {
    m <- as.list(members[i, , drop = FALSE])
    for (a in areas) {
      key <- lib$prevcarga_registry_key(m, a, ctx)
      v <- paste0("v_", substr(digest::digest(paste(m$sigla, a), algo = "sha256"), 1L, 16L))
      nd <- if (skip(m, a)) NULL else needs(a)
      obj <- list(model = m$mdir)
      if (!is.null(nd)) obj$inference_needs <- nd
      lib$prevcarga_registry_publish(obj, list(origin = format(O)), key, v)
      lib$prevcarga_registry_promote_latest(key, v)
    }
  }
  invisible(art)
}

run_v <- function(root, argv) {
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- prevcarga_main(argv, root = root, runtime = function() invisible(NULL))
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}
jdoc <- function(run) jsonlite::fromJSON(paste(run$out, collapse = "\n"), simplifyVector = FALSE)

setup <- function(env, deck = NULL, skip = function(m, a) FALSE, needs = needs_toy, islands = FALSE) {
  root <- root_copy(env)
  art <- tmp(env)
  publish(art, lib$prevcarga_members(root = root), AREAS, needs = needs, skip = skip)
  list(root = root, art = art, deck = if (is.null(deck)) train_deck(env, islands = islands) else deck)
}
gen <- function(s, out, extra = character(), fmt = "json")
  run_v(s$root, c("deck-inference", paste0("--format=", fmt), paste0("--data-train=", s$deck), paste0("--data-artifacts=", s$art),
                  paste0("--origin=", format(O)), paste0("--out=", out), extra))
files_of <- function(d) sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
sha_of <- function(d, fs = files_of(d)) vapply(file.path(d, fs), digest::digest, "", algo = "sha256", file = TRUE, USE.NAMES = FALSE)
ictx <- function(d) list(data_dir = d, weather_issue_cutoff = "available_until")

test_that("[AC1] duas geracoes dao bytes identicos; manifest inference com origem e hash do treino; carga cobre 9 dias antes de O", {
  s <- setup(environment())
  a <- file.path(tmp(environment()), "A"); b <- file.path(tmp(environment()), "B")
  ra <- gen(s, a); rb <- gen(s, b)
  expect_identical(ra$code, 0L); expect_identical(rb$code, 0L)
  expect_identical(files_of(a), files_of(b))
  expect_identical(sha_of(a), sha_of(b))
  expect_true("manifest.json" %in% files_of(a))
  m <- jsonlite::read_json(file.path(a, "manifest.json"))
  expect_identical(names(m), c("schema_version", "kind", "timezone", "load_label", "weather_forecast_publication_lag_hours", "inference"))
  expect_identical(m$kind, "inference")
  expect_identical(m$inference$origin, "2023-06-01")
  expect_identical(m$inference$available_until, "2023-06-01T09:00:00")
  expect_identical(names(m$inference), c("origin", "available_until", "train_deck_hash", "windows", "inference_needs_source"))
  th <- lib$prevcarga_data_hash(s$deck, lib$prevcarga_deck_manifest(s$deck), "end")
  expect_identical(m$inference$train_deck_hash, th)
  expect_identical(m$inference$inference_needs_source, "artifacts")
  expect_null(m$inference$holiday_annex)
  expect_false(dir.exists(file.path(a, "holiday_annex")))
  expect_false(grepl(tempdir(), paste(readLines(file.path(a, "manifest.json")), collapse = ""), fixed = TRUE))   # sem caminho absoluto
  ## o deck gerado e um deck valido para o leitor, kind inference
  man <- lib$prevcarga_deck_manifest(a)
  expect_identical(man$kind, "inference"); expect_identical(man$inference$origin, O)
  ld <- lib$prevcarga_read_load(ictx(a), AREAS)
  for (ar in AREAS) {
    days <- lubridate::as_date(ld$interval_start[ld$area_code == ar], tz = TZ)
    expect_identical(min(days), O - 9L)
    expect_identical(max(days), O)
  }
  expect_identical(max(ld$interval_end), O_09h)
  ## envelope: result com out, origem, hashes, janelas e arquivos [path, sha256, rows]
  j <- jdoc(ra)
  expect_identical(j$result$data_hash, lib$prevcarga_data_hash(a, man, "end"))
  expect_identical(j$result$train_deck_hash, th)
  expect_identical(j$result$windows$load$RJ$from, "2023-05-23"); expect_identical(j$result$windows$load$RJ$to, "2023-06-01")
  paths <- vapply(j$result$files, function(f) f$path, "")
  expect_identical(paths, files_of(a))
  expect_identical(vapply(j$result$files, function(f) f$sha256, ""), sha_of(a, paths))
  expect_true(all(vapply(j$result$files[paths != "manifest.json"], function(f) is.numeric(f$rows) && f$rows > 0, NA)))
  ## linhas CRUAS: as copiadas sao as mesmas do deck de treino (tipos e digitos do store)
  src <- .pc_deck_read(s$deck, "load"); dst <- .pc_deck_read(a, "load")
  m_key <- function(x) paste(x$area_code, as.numeric(x$timestamp), x$load_mwh)
  expect_true(all(m_key(dst) %in% m_key(src)))
  expect_identical(arrow::read_parquet(file.path(a, "load", "area_code=RJ", "year=2023", "part-0.parquet"))$schema,
                   arrow::read_parquet(file.path(s$deck, "load", "area_code=RJ", "year=2023", "part-0.parquet"))$schema)
  ## texto ASCII
  rt <- gen(s, file.path(tmp(environment()), "T"), fmt = "text")
  expect_identical(rt$code, 0L)
  expect_true(any(grepl("^deck-inference origin=2023-06-01 ", rt$out)))
  expect_false(any(as.integer(unlist(lapply(c(rt$out, rt$err), charToRaw))) > 127L))
})

test_that("[AC2] --out nao vazio: exit 2 deck_out_not_empty antes de ler o deck; nada escrito, sem .partial; --out vazio e aceito", {
  s <- setup(environment())
  base <- tmp(environment()); out <- file.path(base, "out"); dir.create(out)
  writeLines("x", file.path(out, "qualquer.txt"))
  antes <- files_of(base)
  s$deck <- file.path(base, "deck-inexistente")                       # nenhuma leitura: um deck ilegivel nao muda o erro
  r <- gen(s, out)
  expect_identical(r$code, 2L)
  j <- jdoc(r)
  expect_identical(j$error$id, "contract.deck_out_not_empty")
  expect_identical(j$error$where, out)
  expect_identical(files_of(base), antes)
  expect_identical(list.files(base, pattern = "partial"), character())
  ## CONTROL: --out vazio e aceito
  s2 <- setup(environment())
  vazio <- file.path(tmp(environment()), "vazio"); dir.create(vazio)
  expect_identical(gen(s2, vazio)$code, 0L)
  expect_true(file.exists(file.path(vazio, "manifest.json")))
})

test_that("[AC3] artefato ausente do membro x area cai na topologia com aviso; manifest mixed; --from-topology => topology", {
  s <- setup(environment(), skip = function(m, a) identical(m$sigla, "HW") && identical(a, "RJ"))
  out <- file.path(tmp(environment()), "o")
  r <- gen(s, out)
  expect_identical(r$code, 0L)
  j <- jdoc(r)
  expect_identical(j$result$inference_needs_source, "mixed")
  expect_identical(j$result$needs_from_topology[[1L]]$member, "holt-winters")
  expect_identical(j$result$needs_from_topology[[1L]]$area, "RJ")
  expect_identical(vapply(j$warnings, function(w) w$id, ""), "warning.inference_needs_from_topology")
  expect_identical(jsonlite::read_json(file.path(out, "manifest.json"))$inference$inference_needs_source, "mixed")
  ## CONTROL: tudo publicado => sem aviso, fonte artifacts
  s2 <- setup(environment())
  j2 <- jdoc(gen(s2, file.path(tmp(environment()), "o2")))
  expect_length(j2$warnings, 0L)
  expect_identical(j2$result$inference_needs_source, "artifacts")
  ## --from-topology: nenhum artefato lido, fonte topology, sem aviso
  s3 <- setup(environment())
  unlink(s3$art, recursive = TRUE); dir.create(s3$art)                  # raiz existe e vazia: nenhum artefato e lido
  o3 <- file.path(tmp(environment()), "o3")
  r3 <- gen(s3, o3, "--from-topology")
  expect_identical(r3$code, 0L)
  j3 <- jdoc(r3)
  expect_identical(j3$result$inference_needs_source, "topology")
  expect_length(j3$warnings, 0L)
  expect_identical(jsonlite::read_json(file.path(o3, "manifest.json"))$inference$inference_needs_source, "topology")
})

test_that("[AC4] previsto: so as emissoes escolhidas pela regra; ler o deck gerado pela regra da as mesmas linhas do deck de treino", {
  s <- setup(environment())
  out <- file.path(tmp(environment()), "o")
  expect_identical(gen(s, out)$code, 0L)
  tr <- lib$prevcarga_read_weather(ictx(s$deck), "forecast", "RJ")
  expect_equal(length(unique(lubridate::hour(tr$issue_time[lubridate::as_date(tr$issue_time, tz = "UTC") == O]))), 4L)   # 4 emissoes/dia
  segs <- list(list(kind = "min_lead", from = 1L, to = 1L, lead = 1L), list(kind = "freshest", from = 0L, to = 1L, lead = NA_integer_))
  lim <- lib$prevcarga_resolve_cuts(c(ictx(s$deck), list(root = s$root, topology_path = file.path(s$root, "config", "topology.yaml"))),
                                    origin = O)$weather_issue_limit
  want <- lib$prevcarga_read_forecast_by_rule(ictx(s$deck), "RJ", O, segs, lim)
  got <- lib$prevcarga_read_forecast_by_rule(ictx(out), "RJ", O, segs, lim)
  expect_identical(got, want)
  allgen <- lib$prevcarga_read_weather(ictx(out), "forecast", "RJ")
  expect_lt(nrow(allgen), nrow(tr))                                    # o deck gerado nao carrega todas as emissoes
  expect_identical(as.numeric(allgen$issue_time), as.numeric(want$issue_time))
  expect_identical(as.numeric(allgen$valid_time), as.numeric(want$valid_time))
  expect_true(all(allgen$issue_time <= lim))                           # emissao > limite nunca entra (18Z de O fica fora)
  expect_true(nrow(tr[tr$issue_time > lim]) > 0L)
})

test_that("[AC5] anexo de feriados: subdeck com manifest proprio no layout do leitor do E3; nenhuma data >= O; sem need nao existe", {
  s <- setup(environment(), needs = function(a) needs_toy(a, annex = TRUE), islands = TRUE)
  out <- file.path(tmp(environment()), "o")
  r <- gen(s, out, "--area=RJ")
  expect_identical(r$code, 0L)
  expect_true(file.exists(file.path(out, "holiday_annex", "manifest.json")))
  am <- jsonlite::read_json(file.path(out, "holiday_annex", "manifest.json"))
  expect_identical(am, list(schema_version = 1L, kind = "holiday_annex"))
  expect_true(file.exists(file.path(out, "holiday_annex", "islands", "area_code=RJ", "part-0.parquet")))
  expect_true(any(grepl("^holiday_annex/load/area_code=RJ/year=2018/", files_of(out))))
  expect_true(any(grepl("^holiday_annex/weather/observed/area_code=RJ/year=2019/", files_of(out))))
  m <- jsonlite::read_json(file.path(out, "manifest.json"))
  expect_identical(m$inference$holiday_annex, "holiday_annex")
  ## anexo lido pelo leitor do E3 sobre o deck gerado == lido sobre o deck de treino (ramo train)
  W <- lib$prevcarga_dessem_window(O, "ano_civil")
  tr <- lib$prevcarga_read_holiday_annex(ictx(s$deck), O, "RJ", W)
  inf <- lib$prevcarga_read_holiday_annex(ictx(out), O, "RJ", W)
  expect_identical(inf$islands, tr$islands)
  expect_equal(nrow(inf$islands), 2L)
  for (n in names(tr$load)) expect_identical(inf$load[[n]], tr$load[[n]])
  for (n in names(tr$weather_observed)) expect_identical(inf$weather_observed[[n]], tr$weather_observed[[n]])
  expect_true(nrow(inf$load) > 0L)
  expect_true(all(lubridate::as_date(inf$load$interval_start, tz = TZ) < O))
  expect_true(all(inf$islands$to < O))
  ## nenhuma data do anexo gravado e >= O
  raw <- .pc_deck_read(file.path(out, "holiday_annex"), "load")
  expect_true(all(lubridate::as_date(raw$timestamp) < O + 1L) && !any(lubridate::as_date(raw$timestamp - 1800) >= O))
  ## sem need de anexo o dataset nao existe e o campo nao e gravado (o leitor exige o caminho relativo, nunca um booleano)
  s2 <- setup(environment())
  o2 <- file.path(tmp(environment()), "o2")
  expect_identical(gen(s2, o2)$code, 0L)
  expect_false(dir.exists(file.path(o2, "holiday_annex")))
  expect_null(jsonlite::read_json(file.path(o2, "manifest.json"))$inference$holiday_annex)
})

test_that("[AC5] anexo pedido sem nenhuma ilha: islands/ vazio e leitor devolve zero ilhas", {
  s <- setup(environment(), needs = function(a) needs_toy(a, annex = TRUE))     # deck sem dias de 2018/2019: so a ilha de 2023 fora da comum nao existe
  out <- file.path(tmp(environment()), "o")
  r <- gen(s, out, "--area=RS")
  expect_identical(r$code, 0L)
  inf <- lib$prevcarga_read_holiday_annex(ictx(out), O, "RS", lib$prevcarga_dessem_window(O, "ano_civil"))
  expect_equal(nrow(inf$islands), 0L)
  expect_equal(nrow(inf$load), 0L)
})

test_that("[AC6] area sem carga na janela: exit 3 no_load {area, from, to}; area fora da topologia: exit 4; JSON unico ASCII; sem .partial", {
  base <- tmp(environment())
  s <- setup(environment(), deck = train_deck(environment(), areas = "RJ"))      # RS sem carga no deck
  out <- file.path(base, "o")
  r <- gen(s, out, "--area=RS")
  expect_identical(r$code, 3L)
  j <- jdoc(r)
  expect_identical(j$error$id, "completude.no_load")
  expect_identical(j$error$details$area, "RS")
  expect_identical(j$error$details$from, "2023-05-23"); expect_identical(j$error$details$to, "2023-06-01")
  expect_length(r$out, 1L)
  expect_false(any(as.integer(charToRaw(r$out)) > 127L))
  expect_false(dir.exists(out))
  expect_identical(list.files(base, pattern = "partial"), character())
  s2 <- setup(environment())
  r2 <- gen(s2, file.path(base, "o2"), "--area=XX")
  expect_identical(r2$code, 4L)
  j2 <- jdoc(r2)
  expect_identical(j2$error$id, "topology.area_undeclared")
  expect_identical(j2$error$where, "XX")
  expect_length(r2$out, 1L)
  expect_false(dir.exists(file.path(base, "o2")))
  ## subsistema expande para as folhas
  r3 <- gen(s2, file.path(base, "o3"), "--area=S")
  expect_identical(r3$code, 0L)
  expect_setequal(names(jdoc(r3)$result$windows$load), AREAS)
})

test_that("resolve on_missing: abort mantem o contrato; topology avisa por membro x area e nao le plugin quando o artefato basta", {
  s <- setup(environment(), skip = function(m, a) identical(m$sigla, "RF"))
  ctx <- list(root = s$root, topology_path = file.path(s$root, "config", "topology.yaml"), artifacts_dir = s$art)
  e <- tryCatch(lib$prevcarga_inference_needs_resolve(ctx, O, AREAS), prevcarga_error = identity)
  expect_identical(e$id, "contract.inference_needs_missing")
  n <- 0L
  r <- withCallingHandlers(lib$prevcarga_inference_needs_resolve(ctx, O, AREAS, on_missing = "topology"),
                           prevcarga_warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") })
  expect_identical(n, 2L)
  expect_identical(sort(unique(r$source[r$mdir == "random-forest"])), "topology")
  expect_identical(unique(r$source[r$mdir == "holt-winters"]), "artifacts")
})

test_that("deck_inference.R nao cita nome de modelo, literal de janela nem env var", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "deck_inference.R"))
  src <- src[!grepl("^\\s*##", src)]
  expect_false(any(grepl("lgbm|dessem-|holt|random|Sys\\.getenv|Sys\\.time|S3|http", src, ignore.case = TRUE)))
})
