## helper-ab.R -- helper dos testes do A/B do corte (M8.8). Auto-carregado por test_dir; nao e codigo de produto.
## Carrega as libs do clean-room e as ferramentas ab_* num unico ambiente (`AB`) e monta os fixtures sinteticos (tempdir, nunca data/).
library(testthat)

ab_root <- local({
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "lib", "runtime.R"))) { up <- dirname(d); stopifnot(!identical(up, d)); d <- up }
  d
})

options(prevcarga.lib = file.path(ab_root, "scripts", "lib"))
AB <- new.env(parent = globalenv())
for (m in c("errors", "runtime", "config", "topology", "schema_long", "deck_reader", "dates", "features", "provenance", "registry", "validation",
            "evaluation", "reconcile", "combination", "verify_repro"))
  sys.source(file.path(ab_root, "scripts", "lib", paste0(m, ".R")), envir = AB)
for (m in c("receipts", "fixture_store", "ab_cases", "ab_relayout", "ab_stages", "ab_report"))
  sys.source(file.path(ab_root, "scripts", "tools", paste0(m, ".R")), envir = AB)

AB_TZ <- "Etc/GMT+3"
AB_RUNNER <- file.path(ab_root, "scripts", "tools", "ab_oracle_run.sh")
AB_EXC <- file.path(ab_root, "tests", "ab", "ab_excecoes.yaml")

## Excecoes do enunciado do ticket (AC 21), SO para os testes de injecao: o arquivo real e do Gabriel e nao as contem todas.
ab_fixture_excecoes <- function(dir = tempfile("exc-")) {
  dir.create(dir, recursive = TRUE)
  f <- file.path(dir, "ab_excecoes.yaml")
  writeLines(c("excecoes:",
    "  - {id: reconcile.apply_profile_reuse, membro: reconcile, estagio: B, condicao: \"modo == 'apply' && khat_origin < origem\", justificativa: \"fixture\", adr: \"ADR-0015\"}",
    "  - {id: regdin.mm_horas_completas, membro: dessem-regdin, estagio: A, condicao: \"minute(available_until) == 30\", justificativa: \"fixture\", adr: \"ADR-0035\"}",
    "  - {id: regdin.sem_manha_0000, membro: dessem-regdin, estagio: A, condicao: \"hour(available_until) == 0 && minute(available_until) == 0\", justificativa: \"fixture\", adr: \"ADR-0023\"}",
    "  - {id: regdin.mxmn_recusado, membro: dessem-regdin/RGD_MXMN, estagio: A, condicao: \"exogena == 'MXMN'\", justificativa: \"fixture\", adr: \"ADR-0021\"}"), f)
  f
}

## linha de caso (pc_ab_case_cols) para os testes de comparador
ab_case <- function(sigla = "RF", mdir = "random-forest", area = "SP", origem = "2026-09-10", au = "2026-09-10T09:00", dd = 9L, exogena = NA_character_) {
  data.frame(caso = paste(sigla, area, origem, sep = "."), membro = if (is.na(exogena)) mdir else paste0(mdir, "/", sigla), sigla = sigla, mdir = mdir,
             exogena = exogena, area = area, origem = origem, deck = "synthetic", tz = AB_TZ, available_until = au, corte_oraculo = NA_character_,
             protocolo = "daily", split = "origin_agnostic", delivery_days = as.integer(dd), executavel = TRUE, motivo = "", stringsAsFactors = FALSE)
}

## valor sintetico de y_pred em funcao do INICIO do intervalo (mesmo valor em qualquer origem do oraculo)
ab_f <- function(start) 1000 + as.numeric(difftime(start, as.POSIXct("2026-09-01 00:00:00", tz = AB_TZ), units = "mins")) / 30 * 0.37

## Referencia do oraculo para um caso: LONG com origens O-1 e O (9 dias cada), rotulo de INICIO, digitos civis sem fuso, forecast_origin Date.
ab_oracle_long <- function(case, bump = 0) {
  O <- as.Date(case$origem)
  rows <- lapply(c(O - 1L, O), function(fo) {
    st <- seq(lubridate::force_tz(lubridate::as_datetime(fo + 1L), AB_TZ), by = "30 min", length.out = 48L * 9L)
    d_plus <- as.integer(lubridate::as_date(st, tz = AB_TZ) - fo)
    slot <- (lubridate::hour(st) * 60L + lubridate::minute(st)) %/% 30L + 1L
    data.frame(area = case$area, forecast_origin = fo, target_datetime = lubridate::force_tz(st, "UTC"), lead = as.integer(d_plus * 48L + slot),
               y_pred = ab_f(st) + bump, y_true = NA_real_, d_plus = d_plus, granularity_min = 30L, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

## LONG nativo do clean-room (interval_end no fuso da carga) para o caso: so a faixa [available_until + 30, O + delivery_days]
ab_clean_long <- function(case, extra_days = 0L, bump = 0) {
  O <- as.Date(case$origem)
  au <- lubridate::ymd_hm(sub("T", " ", case$available_until, fixed = TRUE), tz = AB_TZ)
  grid <- AB$prevcarga_delivery_grid(list(available_until = au, tz = AB_TZ, origin_day = O), case$delivery_days + extra_days)
  st <- grid - lubridate::minutes(30L)
  d_plus <- as.integer(lubridate::as_date(st, tz = AB_TZ) - O)
  slot <- (lubridate::hour(st) * 60L + lubridate::minute(st)) %/% 30L + 1L
  data.frame(area = case$area, forecast_origin = case$origem, target_datetime = grid, lead = as.integer(d_plus * 48L + slot),
             y_pred = ab_f(st) + bump, y_true = NA_real_, d_plus = d_plus, granularity_min = 30L, stringsAsFactors = FALSE)
}

## Monta <dir>/<origem>/membros/<sigla>/<area>.parquet + manifest (clean-room) e o relayout do oraculo (index.tsv + long.parquet cru)
ab_build_world <- function(cases, clean_bump = 0, ref_bump = 0, relabel = TRUE, extra_days = 0L) {
  base <- tempfile("abw-"); dir.create(base)
  clean <- file.path(base, "clean"); refraw <- file.path(base, "refraw"); refdir <- file.path(base, "ref")
  idx <- list()
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, ]
    AB$.pc_ab_write_parquet(ab_clean_long(cs, extra_days, clean_bump), file.path(clean, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet")))
    rel <- file.path("results", cs$mdir, cs$area, "run", "long.parquet")
    dir.create(dirname(file.path(refraw, rel)), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(ab_oracle_long(cs, ref_bump), file.path(refraw, rel))
    idx[[i]] <- data.frame(caso = cs$caso, sigla = cs$sigla, mdir = cs$mdir, exogena = ifelse(is.na(cs$exogena), "", cs$exogena), area = cs$area,
                           origem = cs$origem, result = rel, rc = "0", stringsAsFactors = FALSE)
  }
  utils::write.table(do.call(rbind, idx), file.path(refraw, "index.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  AB$pc_ab_relayout(refraw, refdir, list(tz = AB_TZ), relabel = relabel)
  for (o in list.dirs(clean, recursive = FALSE)) AB$pc_ab_manifest_write(o)
  list(base = base, clean = clean, ref = refdir, refraw = refraw)
}

ab_ctx <- function() list(tz = AB_TZ, root = ab_root)
