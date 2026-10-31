## test-forecast_shell.R -- `forecast` pela FACHADA real (M6.11, AC "shell estranho"): `Rscript --vanilla scripts/prevcarga forecast
## --origin O --format json` em processo FILHO, com ambiente sujo (TZ=America/Sao_Paulo, LC_ALL=pt_BR.UTF-8, OPENBLAS_NUM_THREADS=4,
## so no teste, por pc_with_env via pc_cli: portavel Linux/Windows) e com ambiente limpo (as tres variaveis REMOVIDAS do filho).
## Entrega byte-identica (sha256 por arquivo + manifest sem gerado_em) entre o shell sujo, o limpo e duas rodadas no mesmo shell.
## A instalacao-copia e a real (scripts/, config/, models/); so o forecast.R da COPIA ganha, ao final, o espiao do infer
## (o membro entrega o sentinela na grade do descriptor e no corte do leitor) para nao treinar 3 modelos reais por rodada.
## Estado (k-hat `latest` pelo reconcile real + pesos da combina) preparado no setup e copiado para cada rodada (o export e imutavel).
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long", "deck_reader",
                       "dates", "evaluation", "validation", "features", "training", "inference", "combination",
                       "build_long_output", "reconcile", "exports", "forecast"))
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = env)
suppressMessages(env$prevcarga_runtime_enforce())
env$prevcarga_registry_reset(); env$prevcarga_plugin_unload_all()

O <- lubridate::ymd("2026-10-15")
work <- withr::local_tempdir("m6shell-")
topo <- m6f_topology_file(work)
deck <- file.path(work, "deck")
env$prevcarga_fixture_deck(deck, seed = 11, areas = c(M6F_LEAVES, M6F_SS), days = seq(O - 39L, O, by = "day"), sentinel = TRUE,
                           load_until = lubridate::ymd_hm("2026-10-15 09:00", tz = M6_TZ), tz = M6_TZ)

## espiao no forecast.R da copia: mesma formula do m6f_delivery (sentinela em minutos desde O + deslocamento por membro e por folha)
SHIM <- c(
  "",
  "## ESPIAO DE TESTE (so nesta copia, em tempdir): o membro entrega o sentinela; k-hat, pesos, gate-0, build_long, combina, reconcile e export sao REAIS.",
  "prevcarga_infer_cell <- function(member, area, origin, ctx, variant = NULL, version = NULL, pin = FALSE, ...) {",
  "  cuts <- prevcarga_resolve_cuts(ctx, origin = origin)",
  "  dd <- as.integer(prevcarga_plugin_descriptor(member$mdir, prevcarga_models_root(ctx$root))$delivery_days)",
  "  t0 <- lubridate::force_tz(lubridate::as_datetime(origin), cuts$tz)",
  "  ie <- seq(cuts$available_until + lubridate::minutes(30L), t0 + lubridate::days(dd), by = '30 min')",
  "  y <- lubridate::time_length((ie - lubridate::minutes(30L)) - t0, 'minute') + 10 * match(as.character(member$sigla), c('RF', 'LGBM', 'HW')) +",
  "       100 * match(area, c('A1', 'A2', 'B1'))",
  "  list(predictions = data.table::data.table(target_datetime = ie, y_pred = y),",
  "       version = if (is.null(version)) paste0('v_', substr(digest::digest(paste(member$sigla, area), algo = 'sha256'), 1L, 16L)) else version,",
  "       available_until_posix = cuts$available_until)",
  "}")
root <- pc_install_copy(plugins = TRUE, mutate = function(r) {
  file.copy(topo, file.path(r, "config", "topology.yaml"), overwrite = TRUE)
  f <- file.path(r, "scripts", "lib", "forecast.R")
  writeLines(c(readLines(f, warn = FALSE), SHIM), f)
})

## estado preparado uma vez: k-hat latest (reconcile_train real), pesos da combina e artefatos do HW (origin_scoped) de O
ctx0 <- m6f_ctx(deck, file.path(work, "state0"), topo)
dir.create(ctx0$state_dir, recursive = TRUE)
env$prevcarga_infer_cell <- m6f_infer_spy(env, new.env())
m6f_prepare_origin(env, ctx0, O)
mem <- env$prevcarga_members(path = topo, root = pc_root())
for (a in M6F_LEAVES) m6f_fake_artifact(env, mem[mem$sigla == "HW", ], a, O, ctx0)

fresh_state <- function(.frame = parent.frame()) {
  st <- withr::local_tempdir("m6shell-st-", .local_envir = .frame)
  file.copy(list.files(ctx0$state_dir, full.names = TRUE), st, recursive = TRUE)
  st
}

CLEAN <- c(TZ = NA_character_, LC_ALL = NA_character_, OPENBLAS_NUM_THREADS = NA_character_)
lc <- pc_dirty_lc_all()
DIRTY <- c(TZ = "America/Sao_Paulo", LC_ALL = if (is.na(lc)) "pt_BR.UTF-8" else lc, OPENBLAS_NUM_THREADS = "4")

run_forecast <- function(shell, .frame = parent.frame()) {
  st <- fresh_state(.frame)
  r <- pc_cli(root, c("forecast", "--origin=2026-10-15", "--skip-validate", "--niveis=tudo", "--incluir-membros",
                      paste0("--data=", deck), paste0("--state=", st), "--format=json"), env = shell)
  j <- if (length(r$stdout) == 1L) jsonlite::fromJSON(r$stdout, simplifyVector = FALSE) else NULL
  list(raw = r, json = j, state = st)
}

## entrega: sha256 por arquivo (ordem radix) + manifest sem o relogio (gerado_em) + o envelope sem o que e do relogio/caminho
delivery <- function(x) {
  d <- x$json$result$saida
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  f <- f[f != "manifest.json"]
  m <- jsonlite::read_json(file.path(d, "manifest.json")); m$gerado_em <- NULL
  list(files = stats::setNames(vapply(f, function(p) digest::digest(file = file.path(d, p), algo = "sha256"), ""), f), manifest = m)
}
envelope <- function(x) {
  j <- x$json
  j$run_id <- NULL; j$result$saida <- NULL
  j$result$arquivos <- lapply(j$result$arquivos, function(a) a[c("path", "area", "linhas", "sha256")])
  j
}

test_that("[AC4] shell sujo (TZ/LC_ALL/OPENBLAS) x shell limpo: entregas byte-identicas; 2 rodadas no mesmo shell idem", {
  clean1 <- run_forecast(CLEAN)
  dirty1 <- run_forecast(DIRTY)
  clean2 <- run_forecast(CLEAN)
  dirty2 <- run_forecast(DIRTY)
  for (x in list(clean1, dirty1, clean2, dirty2)) {
    expect_identical(x$raw$status, 0L, info = paste(c(x$raw$stderr, x$raw$stdout), collapse = "\n"))
    expect_length(x$raw$stderr, 0L)
    expect_true(isTRUE(x$json$ok))
    expect_identical(x$json$result$available_until, "2026-10-15T09:00")
    expect_false(isTRUE(x$json$result$ja_publicado))                      # cada rodada tem state proprio: publicou de verdade
  }
  ref <- delivery(clean1)
  expect_gt(length(ref$files), 12L)                                         # SIN + 2 subsistemas + 3 folhas + 9 membros + run_manifest
  expect_true(all(c("SIN.parquet", "folhas/A1.parquet", "membros/RF/A1.parquet", "run_manifest.json") %in% names(ref$files)))
  for (x in list(dirty1, clean2, dirty2)) expect_identical(delivery(x), ref)
  ## o JSON do envelope e identical exceto o lateral de rodada (run_id e o caminho do state)
  for (x in list(dirty1, clean2, dirty2)) expect_identical(envelope(x), envelope(clean1))
  expect_identical(clean1$json$result$versao, dirty1$json$result$versao)
})

test_that("[AC4] CONTROL: o ambiente sujo e de fato sujo no filho e a rodada nao o le (TZ/BLAS nao entram na entrega)", {
  ## a rodada do shell sujo chega ao mesmo available_until/manifest com o fuso declarado no deck (Etc/GMT+3), nao o do shell
  d <- run_forecast(DIRTY)
  expect_identical(jsonlite::read_json(file.path(d$json$result$saida, "manifest.json"))$timezone, M6_TZ)
  expect_false(identical(DIRTY[["TZ"]], M6_TZ))
  ## CONTROL do teste: um deck com OUTRO available_until muda a entrega (a comparacao acima tem dente)
  deck2 <- file.path(work, "deck2")
  env$prevcarga_fixture_deck(deck2, seed = 11, areas = c(M6F_LEAVES, M6F_SS), days = seq(O - 39L, O, by = "day"), sentinel = TRUE,
                             load_until = lubridate::ymd_hm("2026-10-15 09:30", tz = M6_TZ), tz = M6_TZ)
  st <- fresh_state()
  ctx2 <- utils::modifyList(ctx0, list(data_dir = deck2, state_dir = st))
  m6f_prepare_origin(env, ctx2, O)
  r2 <- pc_cli(root, c("forecast", "--origin=2026-10-15", "--skip-validate", "--niveis=tudo", "--incluir-membros",
                       paste0("--data=", deck2), paste0("--state=", st), "--format=json"), env = CLEAN)
  j2 <- jsonlite::fromJSON(r2$stdout, simplifyVector = FALSE)
  expect_identical(j2$result$available_until, "2026-10-15T09:30")
  expect_false(identical(delivery(list(json = j2))$files, delivery(d)$files))
})
