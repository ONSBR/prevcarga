## test-harness-partial.R -- M7.V: entrega parcial do backtest, sem upload (reescrita testthat portavel de jobs/tests/test_partial_upload.sh do
## oraculo; o `test_prevcarga_s3_canary.sh` foi DESCARTADO: nenhum caminho do M7 toca rede/S3). Roda a CLI real (`callr::rscript`, sem shell) em
## tres cenarios de entrega sobre instalacao-copia com plugins-brinquedo e deck por formula em tempdir:
##   (a) PARCIAL: >= 1 celula `done` e >= 1 falha  -> materializa o parcial, exit 14 partial.backtest_delivery, verdict partial;
##   (b) NADA: todas as celulas falham             -> exit 14, verdict failed, nenhum combined/ nem reconciled/ fabricado;
##   (c) OK: tudo ok                                -> exit 0, verdict ok.
## Falha por formula: o membro toy-h (origin_agnostic H = 9) exige historico maior que o do deck curto; os demais cabem.
library(testthat)

PT_ROOT <- pc_m7h_root()
PT_CLI <- file.path(PT_ROOT, "scripts", "prevcarga")
PT_DECK_SHORT <- pc_m7h_deck(lubridate::ymd("2026-09-20"), lubridate::ymd("2026-10-05"))    # toy-h sem historico; toy-s e toy-sc ok
PT_DECK_LONG <- pc_m7h_deck(lubridate::ymd("2026-09-05"), lubridate::ymd("2026-10-05"))     # todos ok em 10-04..10-05; nenhum em 09-06..09-07

## CLI real: sem shell, perfil de usuario/sistema desligados (como `Rscript --vanilla`); stdout/stderr separados
pt_cli <- function(origin, deck, fmt = "json", .frame = parent.frame()) {
  state <- withr::local_tempdir("pt-state-", .local_envir = .frame)
  r <- callr::rscript(PT_CLI, cmdargs = c("backtest", paste0("--origin=", origin), "--corte=08:00", "--keep-going",
                                          paste0("--state=", state), paste0("--data=", deck), paste0("--format=", fmt)),
                      user_profile = FALSE, system_profile = FALSE, show = FALSE, fail_on_status = FALSE, wd = tempdir())
  j <- if (identical(fmt, "json") && length(r$stdout) && nzchar(r$stdout))
    jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE) else NULL
  run_id <- if (is.null(j)) NULL else j$result$run_id
  list(status = as.integer(r$status), json = j, stdout = r$stdout, stderr = r$stderr,
       root = if (is.null(run_id)) NULL else file.path(state, "backtest", run_id))
}
pt_n <- function(root, sub, pat) length(Sys.glob(file.path(root, sub, pat)))

test_that("[AC5] (a) parcial: >= 1 celula done e 1 falha => parcial materializado em backtest/<run_id>/, exit 14, verdict partial", {
  r <- pt_cli("2026-10-02..2026-10-05", PT_DECK_SHORT)
  expect_identical(r$status, 14L)
  expect_false(r$json$ok)
  expect_identical(r$json$error$id, "partial.backtest_delivery")
  expect_identical(r$json$error$class, "partial")
  expect_identical(r$json$result$verdict, "partial")
  s <- r$json$result$summary
  expect_gt(s$ok, 0L); expect_gt(s$failed, 0L)
  expect_identical(s$ok + s$failed + s$skipped, s$total)
  ## materializacao do parcial: as celulas done estao em disco e a falha esta no ledger
  expect_identical(pt_n(r$root, "members", file.path("*", "*", "*", "*", "long.parquet")), s$ok)
  expect_length(readLines(file.path(r$root, "ledger", "failures.jsonl")), s$failed)
  expect_true(file.exists(file.path(r$root, "plan.json")))
  expect_false(dir.exists(file.path(r$root, "run.lock")))
  ## texto: resumo em stderr, tudo ASCII
  t <- pt_cli("2026-10-02..2026-10-05", PT_DECK_SHORT, fmt = "text")
  expect_identical(t$status, 14L)
  expect_false(any(grepl("[^\\x01-\\x7F]", c(t$stdout, t$stderr), perl = TRUE)))
})

test_that("[AC5] (b) nada: todas as celulas falham => exit 14, verdict failed, sem combined/ nem reconciled/ fabricados", {
  r <- pt_cli("2026-09-06..2026-09-07", PT_DECK_LONG)
  expect_identical(r$status, 14L)
  expect_identical(r$json$error$id, "partial.backtest_delivery")
  expect_identical(r$json$result$verdict, "failed")
  s <- r$json$result$summary
  expect_identical(s$ok, 0L); expect_identical(s$failed, s$total)
  expect_identical(pt_n(r$root, "members", file.path("*", "*", "*", "*", "long.parquet")), 0L)
  expect_false(dir.exists(file.path(r$root, "combined")))
  expect_false(dir.exists(file.path(r$root, "reconciled")))
  expect_length(Sys.glob(file.path(r$root, "combined", "*")), 0L)
  expect_length(readLines(file.path(r$root, "ledger", "failures.jsonl")), s$failed)
})

test_that("[AC5] (c) ok: tudo ok => exit 0, verdict ok, entrega completa (CONTROL dos dois cenarios anteriores)", {
  r <- pt_cli("2026-10-04..2026-10-05", PT_DECK_LONG)
  expect_identical(r$status, 0L)
  expect_true(r$json$ok)
  expect_null(r$json$error)
  expect_identical(r$json$result$verdict, "ok")
  s <- r$json$result$summary
  expect_identical(s$ok, s$total); expect_identical(s$failed, 0L)
  expect_gt(pt_n(r$root, "combined", file.path("*", "*", "combined.parquet")), 0L)
  expect_gt(pt_n(r$root, "reconciled", file.path("*", "*", "SIN.parquet")), 0L)
  expect_false(file.exists(file.path(r$root, "ledger", "failures.jsonl")) && length(readLines(file.path(r$root, "ledger", "failures.jsonl"))) > 0L)
})

test_that("[AC5] nenhum caminho do backtest chama rede/S3 (s3_canary descartado): sem paws/s3:///download.file/httr no codigo do verbo", {
  for (f in c("orchestrate.R", "cache.R", "backtest.R", file.path("cli", "backtest.R"), file.path("cli", "plan.R"))) {
    src <- sub("\\s*#.*$", "", readLines(file.path(pc_root(), "scripts", "lib", f), warn = FALSE))
    expect_length(grep("paws|s3://|download\\.file|httr|curl", src), 0L)
  }
  expect_length(grep("paws|s3://", "x <- paws.storage::s3()"), 1L)                      # CONTROL do grep
})
