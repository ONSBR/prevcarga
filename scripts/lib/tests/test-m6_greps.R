## test-m6_greps.R -- greps portaveis do M6 (M6.11): `pc_grep_code` (helper da fundacao) sobre os 4 arquivos do epico com a tabela de
## padroes proibidos; cada padrao tem o seu CONTROL (arquivo-fixture em tempdir com o padrao e ACUSADO). Reforca os greps de
## "compor, nao reimplementar" (forecast.R), relogio/ambiente/rede/fork, literal de topologia/contagem, WMAPE local e bytes > 127.
## Fecha tambem o documento de estado do epico (secoes de docs/estado/m6-forecast.md). Sem env vars; tudo em tempdir.
library(testthat)

M6_FILES <- c("forecast.R", "exports.R", "reporting.R", "build_long_output.R")
m6_paths <- file.path(pc_root(), "scripts", "lib", M6_FILES)

## id -> padrao (perl). `forecast_only` so vale para forecast.R: so ele "compoe" o Reconcile e a combina.
M6_FORBIDDEN <- c(
  py_s3_http       = "python|subprocess|paws|aws\\.s3|s3://|https?://",
  env              = "Sys\\.getenv|Sys\\.setenv|Sys\\.setlocale|setwd[(]",
  clock            = "Sys\\.time|Sys\\.Date|\\bnow\\(",
  base_dates       = "as\\.POSIXct|strptime|as\\.Date\\(",
  fork_sys         = "system2?\\(|mclapply|file\\.symlink",
  topology_literal = "'PESE'|\"PESE\"|c\\('SECO'|c\\(\"SECO\"|== *4L?\\b",
  local_wmape      = "wmape *<- *function"
)
M6_FORECAST_ONLY <- c(khat_in_forecast = "khat_fit|reconcile_train|registry_promote|combina_fit")

test_that("[AC5] os 4 arquivos do M6 existem e nao tem nenhum padrao proibido (zero hits)", {
  expect_true(all(file.exists(m6_paths)))
  hits <- pc_grep_code(m6_paths, M6_FORBIDDEN)
  expect_identical(nrow(hits), 0L, info = paste(sprintf("%s:%d [%s]", basename(hits$file), hits$line, hits$pattern), collapse = "; "))
  fh <- pc_grep_code(m6_paths[1L], M6_FORECAST_ONLY)                # forecast.R nao fita k-hat, nao treina o Reconcile, nao promove, nao fita a combina
  expect_identical(nrow(fh), 0L, info = paste(sprintf("%s:%d [%s]", basename(fh$file), fh$line, fh$pattern), collapse = "; "))
})

test_that("[AC5] nenhum byte > 127 em mensagens (ASCII no terminal) nos arquivos do M6", {
  hi <- pc_bytes_over_127(m6_paths)
  expect_identical(nrow(hi), 0L, info = paste(sprintf("%s:%d", basename(hi$file), hi$line), collapse = "; "))
})

## um arquivo-fixture por padrao, com uma linha que o casa (e um comentario com o mesmo texto, que NUNCA conta)
M6_CONTROL_LINES <- c(
  py_s3_http       = "x <- system('python3 job.py')",
  env              = "h <- Sys.getenv('HOME')",
  clock            = "t <- Sys.time()",
  base_dates       = "d <- as.POSIXct('2026-01-01')",
  fork_sys         = "r <- parallel::mclapply(1:3, identity)",
  topology_literal = "areas <- c('SECO', 'S')",
  local_wmape      = "wmape <- function(y, p) mean(abs(y - p) / y)",
  khat_in_forecast = "fit <- prevcarga_khat_fit(tr, o, 30L)"
)

test_that("[AC5] CONTROL: cada padrao proibido acusa um arquivo-fixture que o contem; comentario nao conta", {
  all_pats <- c(M6_FORBIDDEN, M6_FORECAST_ONLY)
  expect_setequal(names(M6_CONTROL_LINES), names(all_pats))
  d <- withr::local_tempdir("m6greps-")
  for (id in names(M6_CONTROL_LINES)) {
    f <- file.path(d, paste0(id, ".R"))
    writeLines(c(paste("#", M6_CONTROL_LINES[[id]]), "ok <- 1L", M6_CONTROL_LINES[[id]]), f)
    h <- pc_grep_code(f, all_pats[id])
    expect_identical(nrow(h), 1L, info = id)                         # so a linha de codigo; o comentario e ignorado
    expect_identical(h$line, 3L, info = id)
    expect_identical(h$pattern, id)
  }
  ## variantes do padrao de contagem/topologia
  for (ln in c("if (length(x) == 4) NULL", "if (n == 4L) 1", "v <- c('PESE')")) {
    f <- file.path(d, "cnt.R"); writeLines(ln, f)
    expect_identical(nrow(pc_grep_code(f, M6_FORBIDDEN["topology_literal"])), 1L, info = ln)
  }
  ## CONTROL: um arquivo limpo nao acusa nada
  clean <- file.path(d, "clean.R"); writeLines(c("f <- function(x) x + 1L", "# Sys.time()"), clean)
  expect_identical(nrow(pc_grep_code(clean, c(M6_FORBIDDEN, M6_FORECAST_ONLY))), 0L)
})

test_that("[AC5] CONTROL: byte > 127 em mensagem e acusado; fora de mensagem e em comentario nao", {
  d <- withr::local_tempdir("m6greps-hi-")
  f <- file.path(d, "msg.R")
  writeBin(c(charToRaw("prevcarga_abort('x', where = 'aten"), as.raw(c(0xc3, 0xa7, 0xc3, 0xa3)), charToRaw("o')\n"),
             charToRaw("# comment with "), as.raw(c(0xc3, 0xa7)), charToRaw("\nx <- 1L\n")), f)
  h <- pc_bytes_over_127(f)
  expect_identical(nrow(h), 1L)
  expect_identical(h$line, 1L)
  g <- file.path(d, "ok.R"); writeLines(c("prevcarga_abort('x', where = 'atencao')", "# sem acento"), g)
  expect_identical(nrow(pc_bytes_over_127(g)), 0L)
})

test_that("[AC5] compor, nao reimplementar: forecast.R chama o Reconcile/combina/exports e nao os reimplementa", {
  txt <- paste(readLines(m6_paths[1L], warn = FALSE), collapse = "\n")
  for (fn in c("prevcarga_reconcile_plan", "prevcarga_reconcile_apply", "prevcarga_combina_apply", "prevcarga_build_long", "prevcarga_export"))
    expect_true(grepl(fn, txt, fixed = TRUE), info = fn)
  for (tok in c("artifacts/reconcile", "prevcarga_scorecard", "wmape"))
    expect_false(grepl(tok, txt, fixed = TRUE), info = tok)
})

test_that("[AC6] docs/estado/m6-forecast.md traz as secoes do fechamento do M6 e nao inventa o numero do bench", {
  f <- file.path(pc_root(), "docs", "estado", "m6-forecast.md")
  expect_true(file.exists(f))
  doc <- readLines(f, warn = FALSE)
  txt <- paste(doc, collapse = "\n")
  for (sec in c("## Modulos vivos", "## Layout da entrega e dos relatorios", "## Composicao com o Reconcile",
                "## Wall-clock do NFR de 30 min", "## Decisoes taticas L2 do epico"))
    expect_true(any(startsWith(doc, sec)), info = sec)
  for (m in c("build_long_output", "exports", "forecast", "reporting")) expect_true(grepl(paste0("`scripts/lib/", m, ".R`"), txt, fixed = TRUE), info = m)
  expect_true(grepl("forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/", txt, fixed = TRUE))
  expect_true(grepl("prevcarga_reconcile_apply", txt, fixed = TRUE)); expect_true(grepl("khat", txt, fixed = TRUE))
  expect_true(grepl("bench_forecast.R", txt, fixed = TRUE))
  ## wall-clock: ou a linha pendente do operador, ou a linha medida com segundos, razao e host (nunca um numero solto)
  row <- doc[grepl("^\\| ", doc) & !grepl("^\\|---", doc) & !grepl("^\\| segundos", doc) & !grepl("^\\| (Modulo|slot)", doc)]
  meas <- row[grepl("pendente|^\\| [0-9]", row)]
  expect_length(meas, 1L)
  expect_true(grepl("pendente", meas) || grepl("^\\| [0-9.]+ \\| [0-9.]+ \\| [0-9.]+ \\| [^|]+ \\|", meas))
  expect_false(any(grepl("[^\\x01-\\x7f]", doc, perl = TRUE, useBytes = TRUE)))        # documento ASCII
  expect_true(grepl("nao e editado por este ticket", txt, fixed = TRUE))                   # o indice e do M1.V
  ## CONTROL: a conferencia de secao acusa um documento sem a secao do wall-clock
  expect_false(any(startsWith(doc[!startsWith(doc, "## Wall-clock")], "## Wall-clock")))
})
