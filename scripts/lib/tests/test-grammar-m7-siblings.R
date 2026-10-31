## test-grammar-m7-siblings.R -- gates-irmaos do M6 re-escopados pelo M7 (M7.5): a gramatica docopt dos verbos do M6 e do M7 na CLI REAL
## (`Rscript --vanilla scripts/prevcarga`, processo filho) e o gate do M6.5 (`forecast --report` nunca mostra acuracia) re-rodado contra o
## reporting.R estendido pelo M7.4. `--corte`, `--protocol` e `--origin A..B` so existem no backtest; no forecast sao exit 8 config.
## Tudo em tempdir; sem variaveis de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

SIB_TZ <- "Etc/GMT+3"
sib_env <- new.env()
sys.source(file.path(pc_root(), "scripts", "lib", "errors.R"), envir = sib_env)
sys.source(file.path(pc_root(), "scripts", "tools", "fixture_store.R"), envir = sib_env)
SIB_DECK <- withr::local_tempdir("sib-deck-")
sib_env$prevcarga_fixture_deck(file.path(SIB_DECK, "deck"), seed = 3, areas = "SP",
                               days = seq(lubridate::ymd("2024-12-01"), lubridate::ymd("2025-01-07"), by = "day"), tz = SIB_TZ)
SIB_STATE <- withr::local_tempdir("sib-state-")

sib_cli <- function(args, fmt = "json")
  pc_run_cli(c(args, paste0("--data=", file.path(SIB_DECK, "deck")), paste0("--state=", SIB_STATE), paste0("--format=", fmt)))
sib_json <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)

expect_config_usage <- function(args, id = "config.usage") {
  r <- sib_cli(args)
  expect_identical(r$status, 8L, info = paste(args, collapse = " "))
  expect_length(r$stderr, 0L)
  j <- sib_json(r)
  expect_false(j$ok)
  expect_identical(j$error$class, "config")
  expect_identical(j$error$id, id)
  expect_true(all(utf8ToInt(paste(r$stdout, collapse = "")) < 128L))
  ## formato texto: a mesma classe em stderr (ASCII), stdout vazio
  t <- sib_cli(args, fmt = "text")
  expect_identical(t$status, 8L)
  expect_length(t$stdout, 0L)
  expect_match(paste(t$stderr, collapse = "\n"), "[config]", fixed = TRUE)
  expect_true(all(utf8ToInt(paste(t$stderr, collapse = "")) < 128L))
  invisible(j)
}

test_that("[AC4] forecast --corte 08:00 e forecast --protocol daily => exit 8 config.usage, mensagem ASCII", {
  j <- expect_config_usage(c("forecast", "--corte", "08:00"))
  expect_identical(j$error$where, "--corte")
  j2 <- expect_config_usage(c("forecast", "--protocol", "daily"))
  expect_identical(j2$error$where, "--protocol")
})

test_that("[AC4] forecast --origin A..B => exit 8 config (a janela de origens e so do backtest)", {
  j <- expect_config_usage(c("forecast", "--origin", "2025-01-01..2025-01-07"), id = "config.bad_origin")
  expect_identical(j$error$where, "--origin")
})

test_that("[AC4] backtest --corte 08:00 --dry-run e backtest --origin A..B --dry-run => exit 0 (plano, sem treino)", {
  a <- sib_cli(c("backtest", "--corte", "08:00", "--origin", "2025-01-01..2025-01-07", "--dry-run"))
  expect_identical(a$status, 0L)
  expect_length(a$stderr, 0L)
  ja <- sib_json(a)
  expect_true(ja$ok)
  expect_identical(unlist(ja$result$cortes), "08:00")
  b <- sib_cli(c("backtest", "--origin", "2025-01-01..2025-01-07", "--dry-run"))
  expect_identical(b$status, 0L)
  jb <- sib_json(b)
  expect_identical(jb$result$window$origins, 7L)
  expect_identical(jb$result$run_id, ja$result$run_id)              # o corte default e o 08:00 do config: mesma identidade
})

test_that("[AC4] o gate do M6.5 (forecast --report nunca mostra acuracia) re-roda verde contra o reporting.R estendido pelo M7.4", {
  f <- file.path(pc_root(), "scripts", "lib", "tests", "test-reporting.R")
  expect_true(file.exists(f))
  res <- as.data.frame(testthat::test_file(f, reporter = "silent", stop_on_failure = FALSE))
  expect_gt(nrow(res), 0L)
  expect_identical(sum(res$failed), 0L)
  expect_false(any(res$error))
})
