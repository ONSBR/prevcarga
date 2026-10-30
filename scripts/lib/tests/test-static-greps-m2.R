## test-static-greps-m2.R -- grep gates de FONTE do M2 (M2.V, AC 14), em arquivo proprio (nao edita o test-static-greps.R do M1.V).
## Varre CODIGO DE PRODUTO: scripts/prevcarga, scripts/lib/*.R, scripts/lib/cli/*.R (fora de scripts/lib/tests/ e de scripts/tools/).
## Cada gate: FORBIDDEN (zero hit fora das excecoes declaradas) + CONTROL (isca em tempfile() DEVE casar o proprio padrao).
## Comentarios (# ...) sao removidos antes do match. Padroes perl. Nada aqui le variavel de ambiente.
library(testthat)

m2g_root <- pc_root()

m2g_strip <- function(line) {                      # remove o comentario de fim de linha (# fora de aspas)
  vapply(line, function(l) {
    ch <- strsplit(l, "", fixed = TRUE)[[1L]]
    q <- ""
    for (i in seq_along(ch)) {
      c1 <- ch[[i]]
      if (nzchar(q)) { if (c1 == q && (i == 1L || ch[[i - 1L]] != "\\")) q <- "" }
      else if (c1 == "\"" || c1 == "'") q <- c1
      else if (c1 == "#") return(substr(l, 1L, i - 1L))
    }
    l
  }, "", USE.NAMES = FALSE)
}

## arquivos de produto relativos a raiz, na ordem radix
m2g_prod_files <- function(base = m2g_root) {
  fs <- c(file.path(base, "scripts", "prevcarga"), Sys.glob(file.path(base, "scripts", "lib", "*.R")),
          Sys.glob(file.path(base, "scripts", "lib", "cli", "*.R")))
  fs <- sort(unique(normalizePath(fs[file.exists(fs)], winslash = "/")), method = "radix")
  sub("^/", "", sub(normalizePath(base, winslash = "/"), "", fs, fixed = TRUE))
}
m2g_test_files <- function(base = m2g_root)
  sub("^/", "", sub(normalizePath(base, winslash = "/"), "",
                    sort(normalizePath(Sys.glob(file.path(base, "scripts", "lib", "tests", "*.R")), winslash = "/"), method = "radix"),
                    fixed = TRUE))

## linhas de codigo (comentario removido) que casam o padrao: "arquivo:linha"
m2g_hits <- function(pattern, files, base = m2g_root) {
  out <- character()
  for (f in files) {
    p <- if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(base, f)
    ln <- m2g_strip(readLines(p, warn = FALSE, encoding = "UTF-8"))
    k <- which(grepl(pattern, ln, perl = TRUE))
    if (length(k)) out <- c(out, paste0(basename(f), ":", k))
  }
  out
}
m2g_files_hit <- function(pattern, files, base = m2g_root)
  files[vapply(files, function(f) length(m2g_hits(pattern, f, base)) > 0L, NA)]

m2g_bait <- function(text) { f <- tempfile("m2bait-", fileext = ".R"); writeLines(text, f); f }

PROD <- m2g_prod_files()

## ---- tabela de gates: id, padrao, excecoes (arquivos permitidos), isca ------------------------------------------------------
## `except`: conjunto de arquivos de produto onde o padrao e permitido (com o porque ao lado). Ordem das chaves: ver M2.V.
## Referencia ao manifest do DECK: o literal `manifest.json` em qualquer linha de codigo, EXCETO a linha que e demonstravelmente uma
## ESCRITA (chamada write*/.export_write_json) sem chamada de leitura. Escrita do manifest de OUTRO dono (a entrega do export, M6.2) nao e
## leitura do deck; a leitura continua pega mesmo na mesma linha de um write. Sem excecao por nome de arquivo.
M2_READ_CALL <- "(?<![A-Za-z])(read|fromJSON|parse_json|scan)|\\bfile\\("
M2_WRITE_CALL <- "(?<![A-Za-z])write|[.]export_write_json"
M2_MANIFEST_READ <- paste0("^(?=.*(?<![A-Za-z0-9_.])manifest[.]json)(?:(?!.*(", M2_WRITE_CALL, "))|(?=.*(", M2_READ_CALL, "))).*$")
## verify_repro.R (M8.7) le o manifest.json da ENTREGA do forecast (M6.2), nao o do deck; nunca abre dataset nem parquet por conta propria
M2_DELIVERY_READERS <- c("scripts/lib/deck_reader.R", "scripts/lib/verify_repro.R")
GREP_GATES_M2 <- list(
  ## leitor unico: so o deck_reader.R abre o dataset / le o manifest.json do DECK (run_manifest.json e <p>.manifest.json sao de outro dono)
  list(id = "m2_single_reader", pat = paste0("open_dataset|", M2_MANIFEST_READ), except = M2_DELIVERY_READERS,
       bait = "x <- arrow::open_dataset(p)"),
  list(id = "m2_single_reader_manifest", pat = M2_MANIFEST_READ, except = M2_DELIVERY_READERS,
       bait = "m <- jsonlite::read_json(file.path(d, 'manifest.json'))"),
  list(id = "m2_single_reader_dataset", pat = "open_dataset", except = "scripts/lib/deck_reader.R",
       bait = "x <- arrow::open_dataset(p)"),
  ## deslocamento do rotulo de carga (30 min) so no leitor e nas transformacoes (features.R) e na borda do LONG
  list(id = "m2_load_shift", pat = "(?<![A-Za-z0-9_.])d?(minutes|hours)\\(\\s*(30|1)\\s*L?\\)",
       except = c("scripts/lib/deck_reader.R", "scripts/lib/features.R", "scripts/lib/schema_long.R"),
       bait = "iv <- ts - lubridate::minutes(30)"),
  ## sem relogio: o unico relogio permitido e o run_id lateral da config.R (nunca entra em corte nem em calculo)
  list(id = "m2_no_clock", pat = "Sys[.](time|Date)\\(|(?<![A-Za-z0-9_.])now\\(|(?<![A-Za-z0-9_.])today\\(",
       except = "scripts/lib/config.R", bait = "o <- Sys.Date()"),
  ## sem env var fora do runtime.R
  list(id = "m2_no_env", pat = "Sys[.](getenv|setenv|unsetenv)\\(", except = "scripts/lib/runtime.R",
       bait = "tz <- Sys.getenv('TZ')"),
  ## sem conversao de data-hora por base R (a data-hora so por lubridate com tz explicito)
  list(id = "m2_no_base_datetime", pat = "as[.]POSIXct|as[.]POSIXlt|strptime|(?<![A-Za-z0-9_.])as[.]Date\\(", except = character(),
       bait = "d <- as.Date(x)"),
  ## nomes legados
  list(id = "m2_no_legacy_cut_names", pat = "last_label_day|train_end", except = character(), bait = "last_label_day <- o - 1"),
  ## grade flat-48 por interval_start: nenhum rotulo de dia de 24 slots nem DST-aware
  list(id = "m2_no_dst_slots", pat = "DST-aware|24 slots", except = character(), bait = "x <- 'DST-aware: 24 slots'"),
  ## chave `corte` do manifest (legada): nem como chave de lista nem como literal
  ## o `-` no lookbehind deixa passar a FLAG `--corte=<c>` (M7.1); a chave `corte = ` do manifest segue proibida
  list(id = "m2_no_corte_key", pat = "[$]corte(?![A-Za-z0-9_])|[\"']corte[\"']|(?<![A-Za-z0-9_.-])corte\\s*=(?!=)",
       except = character(), bait = "m <- list(corte = 'x')")
)

for (g in GREP_GATES_M2) local({
  g <- g
  test_that(paste("[FORBIDDEN]", g$id), {
    expect_true(all(g$except %in% PROD), info = "excecao declarada tem de existir no produto")
    expect_identical(m2g_hits(g$pat, setdiff(PROD, g$except)), character(), info = g$id)
  })
  test_that(paste("[CONTROL]", g$id), {
    expect_length(m2g_hits(g$pat, m2g_bait(g$bait)), 1L)
  })
})

## as excecoes sao reais: o padrao DEVE casar nelas (excecao morta = gate frouxo)
test_that("[CONTROL] o gate do manifest e do DECK: run_manifest.json e <p>.manifest.json (outros donos) nao casam", {
  pat <- GREP_GATES_M2[[2L]]$pat
  expect_length(m2g_hits(pat, m2g_bait("f <- file.path(d, 'run_manifest.json'); g <- paste0(p, '.manifest.json')")), 0L)
  expect_length(m2g_hits(pat, m2g_bait("f <- readLines(file.path(d, 'manifest.json'))")), 1L)
})

test_that("[CONTROL] o gate nao marca a ESCRITA do manifest da entrega; marca toda LEITURA do manifest do deck fora do leitor", {
  pat <- GREP_GATES_M2[[2L]]$pat
  expect_length(m2g_hits(pat, m2g_bait("write_json(obj, file.path(tmp, 'manifest.json'))")), 0L)
  expect_length(m2g_hits(pat, m2g_bait(".export_write_json(m, file.path(tmp, \"manifest.json\"))")), 0L)
  expect_length(m2g_hits(pat, "scripts/lib/exports.R"), 0L)
  for (b in c("m <- jsonlite::read_json(file.path(d, 'manifest.json'))", "m <- jsonlite::fromJSON(paste0(d, '/manifest.json'))",
              "m <- readLines(file.path(d, \"manifest.json\"))", "x <- scan(file = 'manifest.json')",
              "con <- file(file.path(d, 'manifest.json'), 'rb')", "path <- file.path(d, 'manifest.json')",
              "x <- write_json(read_json(file.path(d, 'manifest.json')), tf)", "m <- fromJSON(\"manifest.json\")"))
    expect_length(m2g_hits(pat, m2g_bait(b)), 1L)
})

test_that("[CONTROL] cada excecao declarada ainda tem o padrao (nenhuma excecao morta)", {
  for (g in GREP_GATES_M2) for (e in g$except)
    expect_gt(length(m2g_hits(g$pat, e)), 0L, label = paste(g$id, e))
})

## ---- --origin como unico flag de data na usage ------------------------------------------------------------------------------
## opcoes longas declaradas nas usages (cli.R: globais; cli/<verbo>.R: do verbo); so --origin pode ser opcao de data.
m2g_long_opts <- function(files) {
  opts <- character()
  for (f in files) {
    ln <- m2g_strip(readLines(if (grepl("^(/|[A-Za-z]:)", f)) f else file.path(m2g_root, f), warn = FALSE))
    m <- regmatches(ln, gregexpr("--[a-z][a-z0-9-]*", ln, perl = TRUE))
    opts <- c(opts, unlist(m))
  }
  sort(unique(sub("^--", "", opts)), method = "radix")
}
M2G_DATE_OPT <- "(^|-)(date|dates|day|days|origin|as-of|asof|until|since|from|to|end|start|ref|reference|cutoff-date)(-|$)"
M2G_CLI_FILES <- c("scripts/lib/cli.R", PROD[grepl("^scripts/lib/cli/", PROD)])

test_that("[FORBIDDEN] --origin e a unica opcao de data nas usages (nenhum --as-of/--date/--until/--end-date)", {
  opts <- m2g_long_opts(M2G_CLI_FILES)
  expect_true("origin" %in% opts)
  expect_identical(grep(M2G_DATE_OPT, opts, value = TRUE, perl = TRUE), "origin")
  ## o valor fixo do docopt da fachada tambem so declara --origin
  expect_false(any(grepl("--as-of|--date=|--until|--end-date|--start-date", readLines(file.path(m2g_root, "scripts", "lib", "cli.R")))))
})

test_that("[CONTROL] o detector de opcao de data pega uma usage com --as-of / --end-date e poupa --load-label / --weather-cutoff", {
  b <- m2g_bait(c("  prevcarga x [--as-of=<d>] [--end-date=<d>]", "  --load-label=<l>  --weather-cutoff=<r>  --history-days=<n>"))
  opts <- m2g_long_opts(b)
  expect_setequal(grep(M2G_DATE_OPT, opts, value = TRUE, perl = TRUE), c("as-of", "end-date", "history-days"))
  expect_false(any(c("load-label", "weather-cutoff") %in% grep(M2G_DATE_OPT, opts, value = TRUE, perl = TRUE)))
})

test_that("[CONTROL] --from (mesmo nomeando um diretorio) e pego: o scratch do export e --scratch", {
  b <- m2g_bait(c("  prevcarga x --from=<scratch> [--valid-from=<d>] [--from-date=<d>] [--scratch=<dir>]"))
  hit <- grep(M2G_DATE_OPT, m2g_long_opts(b), value = TRUE, perl = TRUE)
  expect_setequal(hit, c("from", "valid-from", "from-date"))
})

## ---- nenhum teste toca data/ ------------------------------------------------------------------------------------------------------
## padrao montado em pedacos: o proprio arquivo de gate nao pode casar o seu padrao.
M2G_DATA_PAT <- paste0("[\"'](\\./|\\.\\./)*", "da", "ta/|file[.]path\\([^)]*[\"']", "da", "ta[\"']", "|[\"']/[^\"']*/", "da", "ta/")

test_that("[FORBIDDEN] nenhum teste referencia o diretorio de dados do repositorio (so tempfile()/tempdir())", {
  tests <- m2g_test_files()
  expect_gt(length(tests), 10L)
  hits <- m2g_hits(M2G_DATA_PAT, tests)
  expect_identical(hits, character(), info = paste(hits, collapse = ", "))
})

test_that("[CONTROL] o gate de data/ pega as formas de caminho e poupa o argumento --data", {
  d1 <- paste0("x <- file.path(root, \"", "da", "ta\", \"store\")")
  d2 <- paste0("p <- \"", "da", "ta/load\"")
  ok <- c("r <- c(\"validate-deck\", \"--data\", d)", "ctx <- list(data_dir = d)", "pc_deck_clean(dir = withr::local_tempdir())")
  expect_length(m2g_hits(M2G_DATA_PAT, m2g_bait(c(d1, d2))), 2L)
  expect_identical(m2g_hits(M2G_DATA_PAT, m2g_bait(ok)), character())
})

## ---- cobertura da varredura -----------------------------------------------------------------------------------------------------------
test_that("a varredura cobre fachada, modulos, verbos e deck_reader; exclui testes e ferramentas", {
  expect_true(all(c("scripts/prevcarga", "scripts/lib/deck_reader.R", "scripts/lib/dates.R", "scripts/lib/validation.R",
                    "scripts/lib/cli/validate-deck.R", "scripts/lib/cli.R") %in% PROD))
  expect_false(any(grepl("scripts/lib/tests/|scripts/tools/", PROD)))
})

## ---- write-through (AC 16): documento do M2 e cabecalhos dos modulos sem os restos legados ----------------------------------------
test_that("docs/estado/m2-leitor.md existe e os modulos do M2 nao carregam restos legados (corte do manifest, last_label_day, DST-aware, 24 slots)", {
  doc <- file.path(m2g_root, "docs", "estado", "m2-leitor.md")
  expect_true(file.exists(doc))
  legacy <- "last_label_day|DST-aware|24 slots|train_end"
  expect_length(grep(legacy, readLines(doc, warn = FALSE), value = TRUE), 0L)
  for (m in c("deck_reader.R", "dates.R", "validation.R")) {
    ln <- readLines(file.path(m2g_root, "scripts", "lib", m), warn = FALSE)
    head <- ln[seq_len(min(30L, length(ln)))]
    expect_true(any(grepl("^##", head)), info = m)                                   # cabecalho documentado
    expect_length(grep(legacy, m2g_strip(ln), value = TRUE), 0L)                   # codigo: o cabecalho pode NEGAR o nome legado
  }
  txt <- paste(readLines(doc, warn = FALSE), collapse = "\n")
  for (k in c("deck_reader.R", "dates.R", "validation.R", "interval_start", "load_label", "weather_issue_cutoff",
              "available_until", "train_origin", "data_lag_days", "validate-deck", "data_hash", "deck_reader.tsv",
              "deck_cut.tsv", "dates.tsv", "validation.tsv", "store_manifest.tsv"))
    expect_true(grepl(k, txt, fixed = TRUE), info = k)
})

test_that("[CONTROL] m2_no_corte_key: a flag --corte=<c> nao casa; list(corte = 'x') casa", {
  g <- Filter(function(x) identical(x$id, "m2_no_corte_key"), GREP_GATES_M2)[[1L]]
  expect_false(grepl(g$pat, "\"--corte=<c>   Cortes\"", perl = TRUE))
  expect_true(grepl(g$pat, "m <- list(corte = 'x')", perl = TRUE))
})
