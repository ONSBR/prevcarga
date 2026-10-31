## test-ab-oracle-intact.R -- o runner do oraculo (ab_oracle_run.sh) deixa o oraculo byte-intocado e nunca chama o aws (M8.8; ADR-0034, ADR-0035).
## O oraculo REAL (/opt/ons-source/prevcargadessem3) nunca e escrito: estes testes rodam o runner sobre um oraculo FALSO (repo git em tempdir, com
## jobs/run_job.sh de brinquedo) e provam o contrato: checksum igual antes/depois, git status limpo, aws.log vazio, `corte` de cada caso =
## available_until - 30 min. Cada CONTROL injeta a violacao (tocar models/*/models/ no oraculo, `aws s3 ls`, arquivo sujo) e o runner acusa (exit 3).
## Sem docker nem R 4.2.3 aqui: --runtime host com o proprio Rscript (a construcao do runtime e do `ci/build_marmita.sh` da copia, via docker).
sh <- function(cmd, args, wd = NULL) {
  out <- suppressWarnings(system2(cmd, args, stdout = TRUE, stderr = TRUE))
  list(out = out, status = as.integer(attr(out, "status") %||% 0L))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

tree_sum <- function(d) {                       # independente do runner: sha256 de todos os arquivos regulares (fora de data/), ordem C
  fs <- sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")
  fs <- fs[!grepl("^data/", fs)]
  digest::digest(paste(fs, vapply(fs, function(f) digest::digest(file = file.path(d, f), algo = "sha256"), ""), collapse = "\n"), algo = "sha256")
}

## oraculo falso: repo git commitado; models/<mdir>/job.json para os mdirs do clean-room, models/*/models/artifact.rds e um run_job.sh que
## registra a chamada (MODEL AREA CUT PROTOCOL FROM TO EXOGENA) e escreve o long.parquet do caso. `mode`: ok | aws | touch | dirty.
fake_oracle <- function(mode = "ok") {
  d <- tempfile("oracle-"); dir.create(d)
  for (m in c("random-forest", "lgbm", "holt-winters", "dessem-regdin", "dessem-linear")) {
    dir.create(file.path(d, "models", m, "models"), recursive = TRUE)
    writeLines(sprintf('{"name": "%s", "protocol": "%s"}', m, if (m == "lgbm") "daily" else "retrain_harness"), file.path(d, "models", m, "job.json"))
    writeLines("artefato", file.path(d, "models", m, "models", "artifact.rds"))
  }
  dir.create(file.path(d, "jobs")); dir.create(file.path(d, "ci")); dir.create(file.path(d, "scripts"))
  writeLines("#!/bin/sh", file.path(d, "ci", "build_marmita.sh")); writeLines("# stub", file.path(d, "scripts", "reconcilia_baseline.R"))
  writeLines(d, file.path(d, "jobs", "orig_path.txt"))
  body <- c("#!/usr/bin/env bash", "set -eu",
            'echo "$MODEL $AREA $CUT $PROTOCOL $FROM $TO ${EXOGENA:--}" >> "$LOCAL_RESULTS_DIR/calls.log"',
            'rid="${FROM}_${TO}_${PROTOCOL}_c${CUT%%:*}${CUT#*:}"; rid="${rid%:*}"',
            'out="$LOCAL_RESULTS_DIR/results/$MODEL/$AREA/$rid"; mkdir -p "$out"; echo long > "$out/long.parquet"',
            switch(mode,
                   aws = 'aws s3 ls s3://bucket/ || true',
                   touch = 'echo adulterado >> "$(cat "$(dirname "$0")/orig_path.txt")/models/random-forest/models/artifact.rds"',
                   dirty = 'echo sujo > "$(cat "$(dirname "$0")/orig_path.txt")/arquivo_novo.txt"',
                   ok = ":"),
            'echo "$out/long.parquet"')
  writeLines(body, file.path(d, "jobs", "run_job.sh"))
  git <- function(...) sh("git", c("-C", d, "-c", "user.email=t@t", "-c", "user.name=t", ...))
  git("init", "-q"); git("add", "-A"); git("commit", "-q", "-m", "fake")
  d
}

cases_file <- function() {
  O <- as.Date("2026-09-10"); deck <- tempfile("deck-")
  AB$prevcarga_fixture_deck(deck, seed = 3, areas = c("SP", "RS"), days = O + (-2:0))
  y <- tempfile(fileext = ".yaml")
  writeLines(c("origens:", "  - {origem: \"2026-09-10\", deck: store, available_until_esperado: \"09:00\"}",
               "  - {origem: \"2026-09-10\", deck: store, available_until_esperado: \"10:00\"}",
               "  - {origem: \"2026-09-10\", deck: store, available_until_esperado: \"00:00\", executavel: false}"), y)
  ctx <- list(root = ab_root, data_dir = deck, weather_issue_cutoff = "origin_midnight_utc")
  ora <- fake_oracle()
  cs <- AB$pc_ab_cases(y, NULL, ctx, oracle = ora, membros = c("RF", "LGBM", "LIN_MAX"), areas = c("SP", "RS"))
  tsv <- tempfile(fileext = ".tsv"); AB$pc_ab_cases_write(cs, tsv)
  list(cases = cs, tsv = tsv, deck = deck)
}

run_runner <- function(tsv, ora, extra = character(), ref = tempfile("ref-")) {
  r <- sh("bash", c(AB_RUNNER, tsv, ref, "--oracle", ora, "--data", tempdir(), "--runtime", "host",
                    "--rscript", file.path(R.home("bin"), "Rscript"), extra))
  c(r, list(ref = ref))
}

test_that("[AB.ORA] matriz (membro, area) vem da topologia do clean-room; corte = available_until - 30 min; 00:00 nao executavel", {
  k <- cases_file(); cs <- k$cases
  expect_setequal(unique(cs$sigla), c("RF", "LGBM", "LIN_MAX")); expect_setequal(unique(cs$area), c("SP", "RS"))
  expect_identical(nrow(cs), 3L * 2L * 3L)
  e <- cs[cs$executavel, ]
  au <- lubridate::ymd_hm(sub("T", " ", e$available_until, fixed = TRUE), tz = AB_TZ)
  co <- lubridate::ymd_hm(sub("T", " ", e$corte_oraculo, fixed = TRUE), tz = AB_TZ)
  expect_true(all(as.numeric(difftime(au, co, units = "mins")) == 30))
  expect_identical(unique(e$available_until), c("2026-09-10T09:00", "2026-09-10T10:00"))
  expect_identical(unique(e$corte_oraculo), c("2026-09-10T08:30", "2026-09-10T09:30"))
  expect_false(any(cs$executavel[grepl("T00:00$", cs$available_until)]))
  expect_match(cs$motivo[grepl("T00:00$", cs$available_until)][[1L]], "executavel:false")
  expect_identical(unique(cs$protocolo[cs$sigla == "LGBM"]), "daily")                  # protocolo do job.json do oraculo, nunca forcado
  expect_identical(unique(cs$delivery_days[cs$sigla == "LGBM"]), 2L)                   # faixa do descriptor do plugin
  rt <- AB$pc_ab_cases_read(k$tsv); expect_identical(rt$caso, cs$caso); expect_identical(rt$exogena, cs$exogena)
  ## deck cujo corte nao e o esperado e recusado
  y <- tempfile(fileext = ".yaml"); writeLines(c("origens:", "  - {origem: \"2026-09-10\", deck: store, available_until_esperado: \"09:30\"}"), y)
  ctx <- list(root = ab_root, data_dir = k$deck, weather_issue_cutoff = "origin_midnight_utc")
  e2 <- tryCatch(AB$pc_ab_cases(y, NULL, ctx, oracle = fake_oracle(), membros = "RF", areas = "SP"), prevcarga_error = function(e) e)
  expect_identical(e2$id, "contract.ab_deck_not_full_hour")
})

test_that("[AB.ORA] FORBIDDEN: checksum igual antes/depois, git status limpo, aws.log vazio e o corte de cada caso = available_until - 30 min", {
  k <- cases_file(); ora <- fake_oracle()
  b <- tree_sum(ora)
  r <- run_runner(k$tsv, ora)
  expect_identical(r$status, 0L, info = paste(r$out, collapse = "\n"))
  expect_identical(tree_sum(ora), b)
  expect_identical(sh("git", c("-C", ora, "status", "--porcelain"))$out, character())
  expect_identical(readLines(file.path(r$ref, "oracle.sha256")), readLines(file.path(r$ref, "oracle.after.sha256")))
  expect_identical(file.size(file.path(r$ref, "aws.log")), 0)
  calls <- readLines(file.path(r$ref, "A", "calls.log"))
  ex <- k$cases[k$cases$executavel, ]
  expect_length(calls, nrow(ex))
  got <- do.call(rbind, strsplit(calls, " ", fixed = TRUE))
  for (i in seq_len(nrow(ex))) {
    hit <- got[got[, 1L] == ex$mdir[[i]] & got[, 2L] == ex$area[[i]] & got[, 3L] == paste0(sub("^.*T", "", ex$corte_oraculo[[i]]), ":00"), , drop = FALSE]
    expect_gte(nrow(hit), 1L)
    expect_identical(unique(hit[, 4L]), ex$protocolo[[i]])           # PROTOCOL do job.json
    expect_identical(unique(hit[, 5L]), "2026-09-09"); expect_identical(unique(hit[, 6L]), "2026-09-10")
  }
  expect_true(all(got[got[, 1L] == "dessem-linear", 7L] == "MAX"))      # a variante chega ao oraculo so como EXOGENA do filho
  idx <- utils::read.delim(file.path(r$ref, "A", "index.tsv"), colClasses = "character")
  expect_true(all(idx$rc == "0")); expect_true(all(file.exists(file.path(r$ref, "A", idx$result))))
  expect_true(file.exists(file.path(r$ref, "A", "manifest.sha256")))
  expect_gt(length(readLines(file.path(r$ref, "A", "manifest.sha256"))), nrow(idx))
  ## ref_out dentro do oraculo e recusado
  e <- sh("bash", c(AB_RUNNER, k$tsv, file.path(ora, "dentro"), "--oracle", ora, "--data", tempdir(), "--runtime", "host", "--rscript", file.path(R.home("bin"), "Rscript")))
  expect_identical(e$status, 2L)
  expect_identical(tree_sum(ora), b)
})

test_that("[AB.ORA] CONTROL: tocar 1 arquivo em models/*/models/ do oraculo faz o checksum acusar (exit 3)", {
  k <- cases_file(); ora <- fake_oracle("touch"); b <- tree_sum(ora)
  r <- run_runner(k$tsv, ora)
  expect_identical(r$status, 3L); expect_match(paste(r$out, collapse = " "), "oraculo alterado")
  expect_false(identical(tree_sum(ora), b))                            # a mudanca e real (o CONTROL tem dentes)
})

test_that("[AB.ORA] CONTROL: `aws s3 ls` injetado no runner faz o log acusar >= 1 chamada (exit 3); o aws falso sai 1", {
  k <- cases_file(); ora <- fake_oracle("aws"); b <- tree_sum(ora)
  r <- run_runner(k$tsv, ora)
  expect_identical(r$status, 3L); expect_match(paste(r$out, collapse = " "), "aws falso foi chamado")
  expect_gte(length(readLines(file.path(r$ref, "aws.log"))), 1L)
  expect_match(readLines(file.path(r$ref, "aws.log"))[[1L]], "^s3 ls ")
  expect_identical(tree_sum(ora), b)                                   # a copia absorveu tudo; o oraculo continua intocado
})

test_that("[AB.ORA] CONTROL: arquivo novo no oraculo (git status sujo) e acusado, salvo a lista permitida explicita", {
  k <- cases_file(); ora <- fake_oracle("dirty")
  r <- run_runner(k$tsv, ora)
  expect_identical(r$status, 3L)                                       # o checksum ja muda (arquivo novo); o git status tambem seria acusado
  ## lista permitida (o oraculo real tem exatamente 3 docs/roadmap/*.md + verify-log/ nao rastreados): so o que esta na lista passa
  ora2 <- fake_oracle(); dir.create(file.path(ora2, "docs", "roadmap"), recursive = TRUE); writeLines("x", file.path(ora2, "docs", "roadmap", "A.md"))
  allow <- tempfile(); writeLines("docs/", allow)
  expect_identical(run_runner(k$tsv, ora2, c("--allow-untracked", allow))$status, 0L)
  expect_identical(run_runner(k$tsv, ora2)$status, 3L)                  # sem a lista, o mesmo untracked reprova
})
