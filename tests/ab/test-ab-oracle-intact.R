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

## oraculo falso: repo git commitado; models/<mdir>/job.json para os mdirs do clean-room, models/*/models/artifact.rds, um run_job.sh que
## registra a chamada (MODEL AREA CUT PROTOCOL FROM TO EXOGENA) e escreve o long.parquet do caso, e um run_pipeline.R (RF/LGBM, modo split)
## que registra mode/origin/history-start/artifact/available-until e o env do filho. O run_job.sh loga o env{} do job.json como o job_lib real
## (`manifest env: export K=V`). `mode`: ok | aws | touch | dirty | jqabsent (loga o WARN do job_lib sem jq) | noenv (nao loga o export).
fake_oracle <- function(mode = "ok") {
  d <- tempfile("oracle-"); dir.create(d)
  for (m in c("random-forest", "lgbm", "holt-winters", "dessem-regdin", "dessem-linear")) {
    dir.create(file.path(d, "models", m, "models"), recursive = TRUE)
    js <- switch(m,
      "random-forest" = '{"name": "random-forest", "protocol": "daily", "horizon_days": 9, "train_args": ["--ntree", "100"], "history_days": {"train": 365, "infer": 45}, "env": {}}',
      "lgbm" = '{"name": "lgbm", "protocol": "daily", "horizon_days": 1, "train_args": [], "history_days": {"train": 730, "infer": 45}, "env": {"FAKE_FLAG": "1"}}',
      "dessem-linear" = '{"name": "dessem-linear", "protocol": "retrain_harness", "env": {"DL_FLAG": "x"}}',
      sprintf('{"name": "%s", "protocol": "retrain_harness"}', m))
    writeLines(js, file.path(d, "models", m, "job.json"))
    writeLines("artefato", file.path(d, "models", m, "models", "artifact.rds"))
  }
  for (m in c("random-forest", "lgbm"))
    writeLines(c('a <- commandArgs(TRUE); g <- function(k) { i <- match(k, a); if (is.na(i)) "-" else a[[i + 1L]] }',
                 'log <- file.path(dirname(Sys.getenv("AB_AWS_LOG")), "A", "split_calls.log")',
                 sprintf('cat(paste("%s", g("--area"), g("--mode"), g("--origin"), g("--history-start"), g("--artifact"), gsub(" ", "_", g("--available-until")),', m),
                 '          Sys.getenv("FAKE_FLAG", "-"), Sys.getenv("PREVCARGA_JOB", "-"), if ("--ntree" %in% a) g("--ntree") else "-"), "\\n", file = log, append = TRUE)',
                 'if (g("--mode") == "train") writeLines("art", g("--artifact")) else { stopifnot(file.exists(g("--artifact"))); writeLines("long", g("--out")) }'),
               file.path(d, "models", m, "run_pipeline.R"))
  dir.create(file.path(d, "jobs")); dir.create(file.path(d, "ci")); dir.create(file.path(d, "scripts"))
  writeLines("#!/bin/sh", file.path(d, "ci", "build_marmita.sh")); writeLines("# stub", file.path(d, "scripts", "reconcilia_baseline.R"))
  writeLines(d, file.path(d, "jobs", "orig_path.txt"))
  env_log <- switch(mode,
    jqabsent = 'echo "[job_lib][WARN] jq absent; skipping manifest env{} export (set env vars manually if needed)" >&2',
    noenv = ":",
    'jq -r \'.env // {} | to_entries[] | "[job_lib] manifest env: export \\(.key)=\\(.value) (default do manifest)"\' "$(dirname "$0")/../models/$MODEL/job.json" >&2')
  body <- c("#!/usr/bin/env bash", "set -eu", env_log,
            'echo "$MODEL $AREA $CUT $PROTOCOL $FROM $TO ${EXOGENA:--}" >> "$LOCAL_RESULTS_DIR/calls.log"',
            'rid="${FROM}_${TO}_${PROTOCOL}_c${CUT%%:*}${CUT#*:}"; rid="${rid%:*}"',
            'out="$LOCAL_RESULTS_DIR/results/$MODEL/$AREA/$rid"; mkdir -p "$out"; echo long > "$out/long.parquet"',
            'if [ -n "${AB_DAY0_DIR:-}" ] && [ -f "$(dirname "$0")/day0_$MODEL" ]; then mkdir -p "$AB_DAY0_DIR"; echo day0 > "$AB_DAY0_DIR/${AREA}_${TO}.parquet"; fi',
            switch(mode,
                   aws = 'aws s3 ls s3://bucket/ || true',
                   touch = 'echo adulterado >> "$(cat "$(dirname "$0")/orig_path.txt")/models/random-forest/models/artifact.rds"',
                   dirty = 'echo sujo > "$(cat "$(dirname "$0")/orig_path.txt")/arquivo_novo.txt"',
                   ":"),
            'echo "$out/long.parquet"')
  writeLines(body, file.path(d, "jobs", "run_job.sh"))
  git <- function(...) sh("git", c("-C", d, "-c", "user.email=t@t", "-c", "user.name=t", ...))
  git("init", "-q"); git("add", "-A"); git("commit", "-q", "-m", "fake")
  d
}

## patch de brinquedo para o oraculo falso: no modo job do `mdir`, liga o sidecar do dia O (jobs/day0_<mdir>) so na COPIA
fake_patch <- function(mdir = "dessem-linear", header = TRUE, aplica = TRUE, file_mdir = mdir) {
  d <- tempfile("patches-"); dir.create(d)
  body <- if (aplica) c("--- /dev/null", sprintf("+++ b/jobs/day0_%s", file_mdir), "@@ -0,0 +1 @@", "+liga")
          else c("--- a/jobs/nao_existe.txt", "+++ b/jobs/nao_existe.txt", "@@ -1 +1 @@", "-a", "+b")
  writeLines(c(if (header) sprintf("# ab-patch: mdir=%s fonte_dia_O=passo1_falso", mdir), body), file.path(d, "fake.patch"))
  d
}

## horizonte (H) do job.json do oraculo falso por mdir e as checagens dos dois modos -- usadas pelo FORBIDDEN e pelos CONTROLs (com dentes)
ab_split_hz <- list("random-forest" = c(h = 9L, tr = 365L, inf = 45L), lgbm = c(h = 1L, tr = 730L, inf = 45L))
job_por_origem_ok <- function(got, jb) {
  all(vapply(seq_len(nrow(jb)), function(i) {
    h <- got[got[, 1L] == jb$mdir[[i]] & got[, 2L] == jb$area[[i]], , drop = FALSE]
    nrow(h) > 0L && all(h[, 5L] == jb$origem[[i]] & h[, 6L] == jb$origem[[i]])
  }, NA))
}
## modo split = como os goldens: por caso, 3 chamadas seguidas -- train em O-1-H (history_start = train_origin - history_days.train), infer em
## O-1 e em O com o MESMO artefato e o mesmo history_start absoluto O - history_days.infer, available_until = <dia do infer> <corte do oraculo>
split_golden_ok <- function(spl, sp, hz) {
  if (nrow(spl) != 3L * nrow(sp)) return(FALSE)
  grp <- split(seq_len(nrow(spl)), (seq_len(nrow(spl)) - 1L) %/% 3L)
  all(vapply(grp, function(ix) {
    g <- spl[ix, , drop = FALSE]; m <- g[1L, 1L]; p <- hz[[m]]
    O <- as.Date(g[3L, 4L]); cut <- sub("^[^_]*_", "", g[3L, 7L]); tor <- O - 1L - p[["h"]]
    ok_case <- any(sp$mdir == m & sp$area == g[1L, 2L] & sp$origem == format(O) & paste0(sub("^.*T", "", sp$corte_oraculo), ":00") == cut)
    ok_case && identical(g[, 3L], c("train", "infer", "infer")) && all(g[, 2L] == g[1L, 2L]) &&
      g[1L, 4L] == format(tor) && g[1L, 5L] == format(tor - p[["tr"]]) &&
      identical(g[2:3, 4L], format(c(O - 1L, O))) && all(g[2:3, 5L] == format(O - p[["inf"]])) &&
      length(unique(g[, 6L])) == 1L && all(g[2:3, 7L] == paste0(g[2:3, 4L], "_", cut))
  }, NA))
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

## sem --patches no `extra`, um diretorio VAZIO (os patches reais sao do oraculo real e nao aplicam no falso)
run_runner <- function(tsv, ora, extra = character(), ref = tempfile("ref-")) {
  if (!"--patches" %in% extra) { pd <- tempfile("nopatch-"); dir.create(pd); extra <- c(extra, "--patches", pd) }
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
  ex <- k$cases[k$cases$executavel, ]
  jb <- ex[ex$split != "origin_agnostic", ]; sp <- ex[ex$split == "origin_agnostic", ]
  expect_gt(nrow(jb), 0L); expect_gt(nrow(sp), 0L)
  calls <- readLines(file.path(r$ref, "A", "calls.log"))
  expect_length(calls, nrow(jb))                                       # run_job.sh so no modo job; RF/LGBM vao pelo modo split
  got <- do.call(rbind, strsplit(calls, " ", fixed = TRUE))
  expect_true(job_por_origem_ok(got, jb))                              # FROM = TO = O: uma origem por chamada
  for (i in seq_len(nrow(jb))) {
    hit <- got[got[, 1L] == jb$mdir[[i]] & got[, 2L] == jb$area[[i]] & got[, 3L] == paste0(sub("^.*T", "", jb$corte_oraculo[[i]]), ":00"), , drop = FALSE]
    expect_gte(nrow(hit), 1L)
    expect_identical(unique(hit[, 4L]), jb$protocolo[[i]])           # PROTOCOL do job.json
  }
  spl <- do.call(rbind, strsplit(readLines(file.path(r$ref, "A", "split_calls.log")), " ", fixed = TRUE))
  expect_identical(nrow(spl), 3L * nrow(sp))                           # 1 train + 2 infer por caso
  expect_true(split_golden_ok(spl, sp, ab_split_hz))
  expect_true(all(spl[spl[, 1L] == "lgbm", 8L] == "1"))                # env{} do job.json chega ao filho (FAKE_FLAG)
  expect_true(all(spl[, 9L] == "1"))                                   # PREVCARGA_JOB=1, como o run_job.sh
  expect_true(all(spl[spl[, 1L] == "random-forest" & spl[, 3L] == "train", 10L] == "100"))   # train_args do job.json
  lg <- readLines(file.path(r$ref, "A", paste0(sp$caso[sp$sigla == "LGBM"][[1L]], ".log")))
  expect_true(any(grepl("manifest env: export FAKE_FLAG=1", lg, fixed = TRUE)))
  expect_true(all(got[got[, 1L] == "dessem-linear", 7L] == "MAX"))      # a variante chega ao oraculo so como EXOGENA do filho
  idx <- utils::read.delim(file.path(r$ref, "A", "index.tsv"), colClasses = "character")
  expect_true(all(idx$rc == "0"))
  expect_true(all(file.exists(file.path(r$ref, "A", unlist(strsplit(idx$result, ";", fixed = TRUE))))))
  expect_identical(sort(unique(idx$modo)), c("job", "split"))
  expect_true(all(grepl("^O-1:mesmo_artefato", idx$fonte_dia_O[idx$modo == "split"])))
  expect_true(all(lengths(strsplit(idx$result[idx$modo == "split"], ";", fixed = TRUE)) == 2L))   # LONG de O-1 e de O
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

test_that("[AB.ORA] CONTROL: log com 'jq absent' (o job_lib pulou o env{} do job.json) reprova o caso (contract.ab_oracle_env, exit 1)", {
  k <- cases_file(); ora <- fake_oracle("jqabsent"); b <- tree_sum(ora)
  r <- run_runner(k$tsv, ora)
  expect_identical(r$status, 1L)
  expect_match(paste(r$out, collapse = " "), "contract.ab_oracle_env", fixed = TRUE)
  expect_match(paste(r$out, collapse = " "), "jq ausente")
  idx <- utils::read.delim(file.path(r$ref, "A", "index.tsv"), colClasses = "character", na.strings = character())
  expect_true(all(idx$rc[idx$modo == "job"] != "0")); expect_true(all(idx$result[idx$modo == "job"] == ""))
  expect_true(all(idx$rc[idx$modo == "split"] == "0"))                 # o modo split exporta o env{} ele mesmo (jq do runner)
  expect_identical(tree_sum(ora), b)
})

test_that("[AB.ORA] CONTROL: env{} do job.json sem o export no log (sem o WARN) tambem reprova; o FORBIDDEN acima (export logado) passa", {
  k <- cases_file(); r <- run_runner(k$tsv, fake_oracle("noenv"))
  expect_identical(r$status, 1L)
  expect_match(paste(r$out, collapse = " "), "nao exportado: DL_FLAG")
})

test_that("[AB.ORA] CONTROL: as checagens por origem e do modo split tem dentes (janela O-1..O e re-treino por origem reprovam)", {
  jb <- data.frame(mdir = "dessem-linear", area = "SP", origem = "2026-09-10", stringsAsFactors = FALSE)
  expect_true(job_por_origem_ok(rbind(c("dessem-linear", "SP", "08:30:00", "retrain_harness", "2026-09-10", "2026-09-10", "MAX")), jb))
  expect_false(job_por_origem_ok(rbind(c("dessem-linear", "SP", "08:30:00", "retrain_harness", "2026-09-09", "2026-09-10", "MAX")), jb))
  sp <- data.frame(mdir = "random-forest", area = "SP", origem = "2026-09-10", corte_oraculo = "2026-09-10T08:30", stringsAsFactors = FALSE)
  ln <- function(mode, o, hs, art, au) c("random-forest", "SP", mode, o, hs, art, au, "-", "1", "-")
  gold <- rbind(ln("train", "2026-08-31", "2025-08-31", "/a", "-"), ln("infer", "2026-09-09", "2026-07-27", "/a", "2026-09-09_08:30:00"),
                ln("infer", "2026-09-10", "2026-07-27", "/a", "2026-09-10_08:30:00"))
  expect_true(split_golden_ok(gold, sp, ab_split_hz))
  daily <- gold; daily[2L, 6L] <- "/b"                                 # o dia O de outro artefato (o `daily` re-treina O-1 em O-11)
  expect_false(split_golden_ok(daily, sp, ab_split_hz))
  shift <- gold; shift[1L, 4L] <- "2026-08-30"                         # treino em O-11 em vez de O-1-H
  expect_false(split_golden_ok(shift, sp, ab_split_hz))
})

test_that("[AB.ORA] patch da COPIA: aplicado so na copia (oraculo byte-identico), listado em patches.tsv, dia O do sidecar no index", {
  k <- cases_file(); ora <- fake_oracle(); b <- tree_sum(ora); pd <- fake_patch("dessem-linear")
  r <- run_runner(k$tsv, ora, c("--patches", pd))
  expect_identical(r$status, 0L, info = paste(r$out, collapse = "\n"))
  expect_identical(tree_sum(ora), b)
  expect_false(file.exists(file.path(ora, "jobs", "day0_dessem-linear")))
  pt <- utils::read.delim(file.path(r$ref, "patches.tsv"), colClasses = "character")
  expect_identical(pt$patch, "fake.patch"); expect_identical(pt$mdir, "dessem-linear")
  expect_identical(pt$sha256, digest::digest(file = file.path(pd, "fake.patch"), algo = "sha256"))
  idx <- utils::read.delim(file.path(r$ref, "A", "index.tsv"), colClasses = "character", na.strings = character())
  dl <- idx[idx$mdir == "dessem-linear", ]; expect_gt(nrow(dl), 0L)
  expect_true(all(dl$fonte_dia_O == "O:fake.patch:passo1_falso"))
  expect_true(all(grepl("day0_2026-09-10[.]parquet$", dl$result)))
  expect_true(all(file.exists(file.path(r$ref, "A", unlist(strsplit(dl$result, ";", fixed = TRUE))))))
  expect_true(all(idx$fonte_dia_O[idx$mdir == "lgbm"] != "O:fake.patch:passo1_falso"))
})

test_that("[AB.ORA] CONTROL: patch sem cabecalho, que nao aplica, ou que nao grava o dia O reprova (contract.ab_oracle_patch)", {
  k <- cases_file(); ora <- fake_oracle(); b <- tree_sum(ora)
  for (pd in list(fake_patch(header = FALSE), fake_patch(aplica = FALSE))) {
    r <- run_runner(k$tsv, ora, c("--patches", pd))
    expect_identical(r$status, 1L); expect_match(paste(r$out, collapse = " "), "contract.ab_oracle_patch", fixed = TRUE)
  }
  r <- run_runner(k$tsv, ora, c("--patches", fake_patch("dessem-linear", file_mdir = "outro")))
  expect_identical(r$status, 1L); expect_match(paste(r$out, collapse = " "), "nao gravou o dia O")
  expect_identical(tree_sum(ora), b)
})
