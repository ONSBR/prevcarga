## test-m7-static.R -- M7.V: gate estatico do M7 (AC22 do epico). Regex por regra (`pc_m7_static_rules`) aplicada por `pc_m7_scan(files, rules)`
## sobre orchestrate.R, cache.R, backtest.R, a secao `.rpt_bt_*` de reporting.R e os registros dos verbos plan e backtest
## (scripts/lib/cli/plan.R e backtest.R; a fachada scripts/prevcarga nao tem bloco por verbo: os verbos sao auto-descobertos por cli.R).
## Comentarios sao removidos (pelo parser do R) antes do scan. CONTROL: cada regra precisa disparar no arquivo-isca fixtures/m7/static-bait.R.
library(testthat)

pc_m7_static_rules <- c(
  env     = "Sys\\.getenv\\(",
  fork    = "mclapply|makeForkCluster",
  shell   = "\\bsystem2?\\(",
  symlink = "file\\.symlink",
  tmp     = "[\"']/tmp",
  clock   = "Sys\\.(time|Date)\\(",
  base_dt = "as\\.POSIXct\\(|strptime\\(|\\bas\\.Date\\(",
  net     = "paws|s3://|download\\.file|httr",
  py      = "python3",
  legacy  = "cut_invariant|train_cutoff|last_label_day",
  counts  = "\\b192\\b|\\b8L\\b|\\b5L\\b|==\\s*24\\b",
  ascii   = "[^\\x01-\\x7F]")

## Excecoes fixadas (linha de codigo exata, nao a regra): `8L` e o numero de DIGITOS da conferencia SIN = soma do relatorio (reporting.R,
## M7.4), nao uma contagem de membros. A isca nao contem esse texto, entao a regra `counts` continua disparando nela.
pc_m7_static_allow <- list(counts = "\\.rpt_bt_value\\(sf\\$sum_check, 8L\\)")

## codigo sem comentarios: os tokens COMMENT do parser sao apagados (linha/coluna), o resto fica intacto
pc_m7_strip_comments <- function(lines) {
  pd <- utils::getParseData(parse(text = lines, keep.source = TRUE, encoding = "UTF-8"), includeText = NA)
  cm <- pd[pd$token == "COMMENT", , drop = FALSE]
  for (k in seq_len(nrow(cm))) {
    l <- cm$line1[[k]]
    lines[[l]] <- substr(lines[[l]], 1L, cm$col1[[k]] - 1L)
  }
  lines
}

## pc_m7_scan(files, rules) -> lista nomeada regra -> data.table(file, line, text); so as regras com >= 1 hit aparecem
pc_m7_scan <- function(files, rules, allow = list()) {
  rows <- lapply(files, function(f) {
    raw <- readLines(f, warn = FALSE, encoding = "UTF-8")
    code <- pc_m7_strip_comments(raw)
    out <- lapply(names(rules), function(r) {
      i <- which(grepl(rules[[r]], code, perl = TRUE))
      if (!is.null(allow[[r]])) i <- i[!grepl(allow[[r]], code[i], perl = TRUE)]
      if (!length(i)) NULL else data.table::data.table(rule = r, file = basename(f), line = i, text = trimws(code[i]))
    })
    data.table::rbindlist(out)
  })
  all <- data.table::rbindlist(rows)
  if (!nrow(all)) return(list())
  lapply(split(all[, c("file", "line", "text")], factor(all$rule, levels = names(rules))[drop = TRUE]), data.table::as.data.table)
}

## secao `.rpt_bt_*` de reporting.R (do marcador do M7.4 ate o fim) num arquivo temporario
pc_m7_rpt_bt_section <- function(.local_envir = parent.frame()) {
  lines <- readLines(file.path(pc_root(), "scripts", "lib", "reporting.R"), warn = FALSE, encoding = "UTF-8")
  at <- grep("^## --- Relatorio do backtest \\(M7\\.4\\)", lines)
  stopifnot(length(at) == 1L)
  f <- file.path(withr::local_tempdir("m7-static-", .local_envir = .local_envir), "reporting-rpt_bt.R")
  writeLines(lines[at:length(lines)], f)
  f
}

m7_files <- function(.frame = parent.frame()) {
  lib <- file.path(pc_root(), "scripts", "lib")
  c(file.path(lib, c("orchestrate.R", "cache.R", "backtest.R")), pc_m7_rpt_bt_section(.frame),
    file.path(lib, "cli", c("plan.R", "backtest.R")))
}

test_that("[AC6] zero ocorrencias das regras proibidas no codigo do M7 (orchestrate, cache, backtest, .rpt_bt_*, verbos plan e backtest)", {
  files <- m7_files()
  expect_true(all(file.exists(files)))
  expect_length(files, 6L)
  hits <- pc_m7_scan(files, pc_m7_static_rules, pc_m7_static_allow)
  if (length(hits)) print(hits)
  expect_length(hits, 0L)
})

test_that("[AC6] CONTROL: toda regra dispara no arquivo-isca fixtures/m7/static-bait.R (o teste falha se alguma regra nao pega)", {
  bait <- test_path("fixtures", "m7", "static-bait.R")
  expect_true(file.exists(bait))
  hits <- pc_m7_scan(bait, pc_m7_static_rules, pc_m7_static_allow)
  expect_setequal(names(hits), names(pc_m7_static_rules))
  ## o comentario da isca cita padroes e nao conta: nenhuma regra dispara nas linhas de comentario (linhas 1 e 2)
  expect_true(all(vapply(hits, function(h) all(h$line > 2L), NA)))
})

test_that("[AC6] CONTROL: a excecao fixada nao mascara a regra (a conferencia 8L do relatorio so passa com o texto exato)", {
  tf <- withr::local_tempfile(fileext = ".R")
  writeLines(c(".rpt_bt_value(sf$sum_check, 8L)", "x <- list(n = 8L)"), tf)
  h <- pc_m7_scan(tf, pc_m7_static_rules, pc_m7_static_allow)
  expect_identical(h$counts$line, 2L)
  expect_length(pc_m7_scan(tf, pc_m7_static_rules), 1L)                                # sem a excecao as duas linhas disparam
  expect_identical(nrow(pc_m7_scan(tf, pc_m7_static_rules)$counts), 2L)
})

test_that("[AC6] bytes > 127: o codigo do M7 e ASCII puro (as mensagens ao usuario vem do catalogo de erros)", {
  for (f in m7_files()) expect_false(any(grepl("[^\\x01-\\x7F]", readLines(f, warn = FALSE, encoding = "bytes"), perl = TRUE)), info = f)
})
