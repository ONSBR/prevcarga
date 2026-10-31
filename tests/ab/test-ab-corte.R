## test-ab-corte.R -- o A/B do corte REAL: pipeline completo sobre a referencia congelada do oraculo (M8.8; ADR-0034, ADR-0035, ADR-0036).
## So roda com a referencia gerada por `scripts/tools/ab_oracle_run.sh` (R 4.2.3 + renv.lock do oraculo); sem ela, SKIP EXPLICITO com o motivo e o
## comando exato -- nunca verde silencioso e nunca veredito inventado. Entradas (options, nunca variavel de ambiente):
##   prevcarga.ab.ref    = <ref_out> do runner (A/index.tsv, runtime.txt, oracle.sha256, aws.log)           [obrigatoria]
##   prevcarga.ab.clean  = saida de pc_ab_clean_run (clean-room, regra origin_midnight_utc)                  [obrigatoria]
##   prevcarga.ab.cases  = cases.tsv usado no runner                                                          [obrigatoria]
##   prevcarga.ab.stages = list(b = list(ratio, origem, ref_profile, window_days, ss, apply), c = list(saida, referencia),
##                              d = list(panel, origem, area), e = list(long))   -- estagios B-E (ausentes => `estagio_<E>_ausente`, BLOQUEADO)
##   prevcarga.ab.bundles = list(linux = <tarball>, windows = <zip>) -- so entao grava o recibo `ab-corte` (pc_receipt_write)
##   prevcarga.ab.receipts = diretorio dos recibos das pernas (default tests/.receipts)
## O veredito (ab_veredito.json/.md) e gravado em `prevcarga.ab.out` (default tempdir). Comando completo e pendencias: docs/estado/ab.md.
ref <- getOption("prevcarga.ab.ref"); clean <- getOption("prevcarga.ab.clean"); cases_tsv <- getOption("prevcarga.ab.cases")

test_that("[AB.CORTE] pipeline completo sobre a referencia congelada: relayout -> A -> B -> C -> D (propriedade) -> E (reporta) -> veredito", {
  if (is.null(ref) || !file.exists(file.path(ref, "A", "index.tsv")))
    skip("[TBD] referencia do oraculo ausente: gere com `scripts/tools/ab_oracle_run.sh <cases.tsv> <ref_out> --data <store> --runtime docker` (R 4.2.3 + renv.lock do oraculo; docs/estado/ab.md) e passe options(prevcarga.ab.ref=, prevcarga.ab.clean=, prevcarga.ab.cases=)")
  if (is.null(clean) || !dir.exists(clean) || is.null(cases_tsv) || !file.exists(cases_tsv))
    skip("[TBD] saida do clean-room (pc_ab_clean_run) ou cases.tsv ausente: passe options(prevcarga.ab.clean=, prevcarga.ab.cases=)")
  ## o oraculo ficou intocado e a rede nao foi usada (provado pelo runner; reconferido aqui)
  expect_identical(readLines(file.path(ref, "oracle.sha256")), readLines(file.path(ref, "oracle.after.sha256")))
  expect_identical(file.size(file.path(ref, "aws.log")), 0)
  cs <- AB$pc_ab_cases_read(cases_tsv)
  idx <- utils::read.delim(file.path(ref, "A", "index.tsv"), colClasses = "character")
  cs <- cs[cs$caso %in% idx$caso, , drop = FALSE]                   # so os casos que o runner executou
  expect_gt(nrow(cs), 0L)
  tz <- unique(cs$tz); expect_length(tz, 1L)
  ctx <- list(tz = tz, root = ab_root)
  rel <- tempfile("relayout-")
  rl <- AB$pc_ab_relayout(file.path(ref, "A"), rel, ctx)
  expect_length(rl$faltando, 0L)                                    # caso do oraculo sem resultado => reprova aqui, nunca Delta = 0
  exc <- AB$pc_ab_load_excecoes(AB_EXC)
  linhas <- list(AB$pc_ab_stage_a(clean, rel, cs, ctx, exc))
  st <- getOption("prevcarga.ab.stages")
  if (!is.null(st$b)) linhas[[2L]] <- do.call(AB$pc_ab_stage_b, c(st$b, list(excecoes = exc)))
  if (!is.null(st$c)) linhas[[3L]] <- do.call(AB$pc_ab_stage_c, st$c)
  if (!is.null(st$d)) linhas[[4L]] <- do.call(AB$pc_ab_stage_d, c(st$d, list(ctx = ctx)))
  if (!is.null(st$e)) linhas[[5L]] <- AB$pc_ab_stage_e(st$e$long, ctx)
  lin <- AB$pc_ab_bind(linhas)
  bundles <- getOption("prevcarga.ab.bundles")
  rdir <- getOption("prevcarga.ab.receipts", file.path(ab_root, "tests", ".receipts"))
  rec <- if (is.null(bundles)) list() else
    AB$pc_ab_recibos_collect(lapply(bundles, function(b) digest::digest(file = b, algo = "sha256")), rdir)
  v <- AB$pc_ab_verdict(lin, exc, rec)
  out <- getOption("prevcarga.ab.out", tempfile("ab-out-"))
  AB$pc_ab_report_write(v, lin, out,
                        bundles = if (is.null(bundles)) NULL else stats::setNames(bundles, c(linux = "linux-x86_64", windows = "windows-x86_64")[names(bundles)]),
                        receipts_dir = rdir)
  expect_true(file.exists(file.path(out, "ab_veredito.json")))
  expect_true(v$veredito %in% c("CORTE-OK", "CORTE-BLOQUEADO"))
  ## o relato e o motivo, nunca um skip: o veredito (e o que o bloqueia) vai para o log da rodada
  message("A/B do corte: ", v$veredito, " | membros em Delta=0: ", v$n_delta, " | motivos: ", paste(v$motivos, collapse = ", "))
  ## A e a parte determinista que esta sessao mede: sem FAIL fora da lista fechada
  expect_false(any(lin$estagio == "A" & lin$status == "FAIL"), info = paste(lin$detalhe[lin$status == "FAIL"], collapse = " | "))
})
