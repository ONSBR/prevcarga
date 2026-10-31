## test-readme.R -- DL.11: o README do plugin tem as secoes obrigatorias, uma linha por gate da tabela do runner, o mapa do oraculo
## valido e os sha256 dos relatorios de paridade; o estado duravel do epico existe e o README aponta para ele. Gate [PLUGINS] readme.
suppressWarnings(suppressMessages(library(testthat)))

dl_readme_sections <- c(
  "## Mapa para o oraculo", "## Contrato do plugin", "## Layout dessem_matrix_nx48 e faixa O..O+8",
  "## Cabeca de nowcast (mm por horas completas)", "## RNG preservado e plano B", "## Desvios conscientes (A/B do M8)",
  "## Gates gate01..gate18 x dono", "## Relatorios de paridade (sha256)")

## linhas da tabela do runner (objeto `.dl_gates` avaliado sem executar o runner)
dl_gate_table <- local({
  env <- new.env(parent = baseenv())
  for (e in parse(file.path(dl_models(), "dessem-linear", "tests", "run_all_gates.R"), keep.source = FALSE))
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && identical(as.character(e[[2L]])[[1L]], ".dl_gates")) eval(e, env)
  data.frame(gate = vapply(env$.dl_gates, `[[`, "", 1L), oracle = vapply(env$.dl_gates, `[[`, "", 2L), stringsAsFactors = FALSE)
})

dlr_readme_missing <- function(txt) setdiff(dl_readme_sections, intersect(dl_readme_sections, txt[startsWith(txt, "## ")]))

test_that("[PLUGINS] readme: secoes, gates, sha256 dos relatorios, mapa do oraculo e estado duravel; CONTROL sem a secao de desvios", {
  txt <- readLines(file.path(dl_models(), "dessem-linear", "README.md"), encoding = "UTF-8")
  all <- paste(txt, collapse = "\n")
  expect_true(all(utf8ToInt(all) < 128L))
  for (s in dl_readme_sections) expect_true(any(txt == s), label = s)
  for (g in dl_gate_table$gate) expect_true(any(grepl(sprintf("| `%s` |", g), txt, fixed = TRUE)), label = g)
  expect_setequal(dl_gate_table$oracle[grepl("^gate", dl_gate_table$oracle)], sprintf("gate%02d", c(1:11, 13:18)))
  for (r in c("foundation_report.json", "fanout_report.json")) {
    sha <- digest::digest(file = file.path(dl_models(), "dessem-linear", "tests", "parity", r), algo = "sha256")
    expect_true(grepl(sha, all, fixed = TRUE), label = r)
  }
  for (needle in c("origin_scoped", "granularity_min: 60", "horizon_days: 9", "delivery_days: 9", "model_time_offset_min: 60",
                   "dessem_matrix_nx48", "dessem_semihourly_48", "O..O+8", "O+9", "fora_da_faixa", "rng_deviation", "tune_wsvm",
                   "docs/estado/dessem_linear.md", "run_all_gates.R --tier fast", "--tier parity-full", "data_adapter.R:",
                   "dessem_linear_fit.R:", "dessem_linear_predict.R:", "orchestrator.R:"))
    expect_true(grepl(needle, all, fixed = TRUE), label = needle)
  ## desvios: lista numerada 1-6 dentro da secao
  i0 <- which(txt == "## Desvios conscientes (A/B do M8)"); i1 <- grep("^## ", txt); i1 <- c(i1[i1 > i0], length(txt) + 1L)[[1L]]
  sec <- txt[(i0 + 1L):(i1 - 1L)]
  expect_identical(sub("^([0-9]+)[.] .*$", "\\1", grep("^[0-9]+[.] ", sec, value = TRUE)), as.character(1:6))
  ## mapa: cada arquivo do oraculo citado existe e a linha cabe no arquivo
  orc <- "/opt/ons-source/prevcargadessem3/models/dessem-linear"
  if (dir.exists(orc)) {
    refs <- regmatches(all, gregexpr("(data_adapter|dessem_linear_fit|dessem_linear_predict|dessem_linear_inference|orchestrator)[.]R:[0-9]+", all))[[1L]]
    expect_gt(length(refs), 8L)
    for (r in unique(refs)) {
      p <- Sys.glob(file.path(orc, "R", "*", sub(":.*$", "", r)))
      expect_length(p, 1L)
      expect_lte(as.integer(sub("^.*:", "", r)), length(readLines(p[[1L]], warn = FALSE)))
    }
  }
  ## estado duravel do epico: existe, o README aponta para ele e ele registra o fechamento
  est <- file.path(dl_root(), "docs", "estado", "dessem_linear.md")
  expect_true(file.exists(est))
  et <- paste(readLines(est, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("## DL.11", et, fixed = TRUE))
  for (needle in c("LIN_MED", "LIN_MAX", "LIN_MXMN", "engine_manifest()", "history_days.infer == train", "rng_deviation",
                   "foundation_report.json", "fanout_report.json",
                   digest::digest(file = file.path(dl_models(), "dessem-linear", "tests", "parity", "foundation_report.json"), algo = "sha256"),
                   digest::digest(file = file.path(dl_models(), "dessem-linear", "tests", "parity", "fanout_report.json"), algo = "sha256")))
    expect_true(grepl(needle, et, fixed = TRUE), label = needle)
  ## CONTROL: README-fixture sem a secao de desvios falha nomeando-a
  fx <- txt[txt != "## Desvios conscientes (A/B do M8)"]
  expect_identical(dlr_readme_missing(fx), "## Desvios conscientes (A/B do M8)")
  expect_identical(dlr_readme_missing(txt), character())
})
