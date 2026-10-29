## test-static-greps.R -- grep gates de FONTE do M1 (M1.V, AC 15). Varre so CODIGO DE PRODUTO:
##   scripts/prevcarga, scripts/lib/*.R, scripts/lib/cli/*.R, scripts/lib/errors/*.tsv, config/*.yaml, models/*/run.yaml
## (fora de scripts/lib/tests/ e do runner dev-only scripts/tools/spike_r453.R, que nao entra na varredura).
## Cada gate: FORBIDDEN (conjunto de arquivos com match == allow) + CONTROL (isca em tempdir() DEVE casar).
## Comentarios (# ...) sao removidos antes do match, exceto no gate de bytes (que le o arquivo cru).

root <- pc_root()

pc_code_files <- function(scope = "code", base = root) {
  g <- function(...) Sys.glob(file.path(base, ...))
  r_code <- c(file.path(base, "scripts", "prevcarga"), g("scripts", "lib", "*.R"), g("scripts", "lib", "cli", "*.R"))
  files <- switch(scope,
    r_code   = r_code,
    code     = c(r_code, g("scripts", "lib", "errors", "*.tsv"), g("config", "*.yaml"), g("models", "*", "run.yaml")),
    run_yaml = g("models", "*", "run.yaml"),
    registry = file.path(base, "scripts", "lib", "registry.R"),
    messages = c(file.path(base, "scripts", "prevcarga"), file.path(base, "scripts", "lib", "errors.R"),
                 file.path(base, "scripts", "lib", "banner.R"), g("scripts", "lib", "errors", "*.tsv"),
                 g("scripts", "lib", "cli", "*.R")),
    stop("escopo desconhecido: ", scope))
  files <- files[file.exists(files)]
  sub("^/", "", sub(base, "", sort(unique(normalizePath(files, winslash = "/"))), fixed = TRUE))
}

## remove o comentario de fim de linha (# fora de aspas); linhas so de comentario viram "".
pc_strip_comment <- function(line) {
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

## arquivos (relativos a base) cujo conteudo casa o padrao (perl). `bytes`: match em bytes crus.
pc_grep <- function(pattern, files, base = root, strip = TRUE, bytes = FALSE) {
  hit <- vapply(files, function(f) {
    p <- if (isAbsolutePath(f)) f else file.path(base, f)
    ln <- readLines(p, warn = FALSE, encoding = if (bytes) "bytes" else "UTF-8")
    if (strip) ln <- pc_strip_comment(ln)
    any(grepl(pattern, ln, perl = TRUE, useBytes = bytes))
  }, NA)
  files[hit]
}
isAbsolutePath <- function(f) grepl("^(/|[A-Za-z]:)", f)

## tokens de topologia montados do config CARREGADO (nunca redigitados): folhas, subsistemas, perdas, siglas.
pc_topo_tokens <- function(base = root) {
  E <- pc_source_lib(c("errors", "topology"))
  t <- E$prevcarga_topology(root = base)
  m <- E$prevcarga_members(root = base)
  unique(c(t$leaves, t$order, vapply(t$subsystems, function(s) s$loss, ""), m$sigla))
}
topo_pat <- function(toks = pc_topo_tokens()) paste0("[\"'](", paste(toks, collapse = "|"), ")[\"']")

## chaves proibidas nos run.yaml: lista unica do loader (topology.R), nunca redigitada aqui
run_forbidden <- pc_source_lib(c("errors", "topology"))$.prevcarga_run_forbidden

## allow = arquivos onde o padrao e permitido (conjunto EXATO). debt: ver discoveries do M1.V.
pc_grep_gates <- data.frame(
  gate = c("env", "env_names", "verbs_central", "schema_long_def", "date_base", "clock", "unix_only", "net",
           "topo_literal", "khat_literal", "dessem_switch", "run_yaml_keys", "registry_lifecycle", "bytes", "minutes30",
           "long_attach"),
  pattern = c(
    "Sys\\.(getenv|setenv|unsetenv)\\(",
    "(?<![.A-Za-z0-9_])PREVCARGA_[A-Z_]+",
    "(?<![A-Za-z0-9_])prevcarga_verbs(?![A-Za-z0-9_])",
    "(?<![A-Za-z0-9_])prevcarga_schema_long\\s*(<-|=)",
    "as\\.POSIXct|as\\.POSIXlt|strptime|strftime|as\\.Date|ISOdatetime|difftime",
    "Sys\\.time|Sys\\.Date|(?<![A-Za-z0-9_.])now\\(|today\\(",
    "(?<![A-Za-z0-9_.])system\\(|mclapply|mcparallel|file\\.symlink|Sys\\.readlink|\"/tmp|\"/opt",
    "download\\.file|curl|httr|paws|s3://",
    NA_character_,
    "(?<![0-9.])1\\.15(?![0-9])|(?<![0-9.])1\\.00(?![0-9])",
    "\"dessem-(linear|regdin)\"",
    paste0("^\\s*(", paste(run_forbidden, collapse = "|"), ")\\s*:"),
    "promote|rollback|(?<![A-Za-z0-9_])gc(?![A-Za-z0-9_])",
    "[\\x80-\\xFF]",
    "(?<![A-Za-z0-9_.])d?(minutes|seconds)\\(\\s*(c\\([^)]*)?-?\\s*(30|1800)(?![0-9])|[-+]\\s*1800(?![0-9])",
    "(?<![A-Za-z0-9_])prevcarga_long_attach_interval_start(?![A-Za-z0-9_])"),
  allow = I(list(
    "scripts/lib/runtime.R", character(), character(), "scripts/lib/schema_long.R",
    character(),                      # divida do combination.R quitada em M4.2b (lubridate::as_datetime): zero hits
    "scripts/lib/config.R",           # lateral run_id (unico relogio permitido)
    character(), character(), character(), "config/topology.yaml", character(), character(),
    character(), character(),
    ## minutes30: o rotulo de hora da carga e operado 1x na entrada e 1x na saida (ESTADO-ATUAL, ADR-0035)
    c("scripts/lib/deck_reader.R",    # borda do deck: rotulo end -> interval_start/interval_end e passo do available_until
      "scripts/lib/features.R",       # reamostragem to_semihourly (2 meias-horas por ancora horaria), nao rotulo
      "scripts/lib/schema_long.R"),   # borda do LONG: unico par target_datetime <-> interval_start (leitor e writer)
    ## long_attach: so o leitor unico do LONG anexa interval_start; consumidores exigem a coluna
    c("scripts/lib/evaluation.R",     # prevcarga_assert_long (leitor unico)
      "scripts/lib/schema_long.R"))), # definicao
  bait = c("x <- Sys.getenv('TZ')", "p <- 'PREVCARGA_DATA'", "prevcarga_verbs <- c('a')",
           "prevcarga_schema_long <- c(a = 'b')", "d <- as.Date('2024-01-01')", "t0 <- Sys.time()",
           "system('ls')", "download.file(u, f)", NA_character_, "k <- c(1.00, 1.15)",
           "f <- 'dessem-regdin'  ; x == \"dessem-regdin\"", "cut_invariant: true", "promote <- function() 1",
           "msg <- 'a\\u00e7ao'", "s <- t - lubridate::minutes(c(-30, 30))",
           "p <- prevcarga_long_attach_interval_start(p)"),
  scope = c("r_code", "code", "r_code", "r_code", "r_code", "r_code", "code", "code", "r_code", "code", "r_code",
            "run_yaml", "registry", "messages", "r_code", "r_code"),
  strip = c(rep(TRUE, 13), FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE)

## o gate de topologia monta o padrao do config carregado (nao ha literal aqui)
pc_grep_gates$pattern[pc_grep_gates$gate == "topo_literal"] <- topo_pat()
pc_grep_gates$bait[pc_grep_gates$gate == "topo_literal"] <- paste0("x <- \"", pc_topo_tokens()[[1L]], "\"")

write_bait <- function(bait, ext = ".R") {
  f <- tempfile("bait-", fileext = ext)
  writeBin(charToRaw(enc2utf8(paste0(gsub("\\u00e7", "ç", bait, fixed = TRUE), "\n"))), f)
  f
}

test_that("pc_code_files cobre o codigo de produto e exclui testes e o runner dev-only", {
  fs <- pc_code_files("code")
  expect_true(all(c("scripts/prevcarga", "scripts/lib/runtime.R", "scripts/lib/errors.R", "scripts/lib/cli/models.R",
                    "scripts/lib/errors/core.tsv", "config/topology.yaml", "models/dessem-regdin/run.yaml") %in% fs))
  expect_false(any(grepl("scripts/lib/tests/|scripts/tools/", fs)))
  expect_false(any(grepl("spike_r453", fs)))
})

for (i in seq_len(nrow(pc_grep_gates))) local({
  g <- pc_grep_gates[i, ]
  files <- pc_code_files(g$scope)
  bytes <- identical(g$gate, "bytes")
  test_that(paste("[FORBIDDEN]", g$gate), {
    hit <- pc_grep(g$pattern, files, strip = g$strip, bytes = bytes)
    expect_setequal(hit, intersect(g$allow[[1L]], files))
    expect_true(all(g$allow[[1L]] %in% files))
  })
  test_that(paste("[CONTROL]", g$gate), {
    bait <- write_bait(g$bait)
    expect_length(pc_grep(g$pattern, bait, strip = g$strip, bytes = bytes), 1L)
  })
})

test_that("[CONTROL] o corte de comentario nao esconde codigo nem inventa match", {
  expect_identical(pc_strip_comment(c("x <- 1  # Sys.time()", "# so comentario", "s <- 'a # b'; Sys.time()")),
                   c("x <- 1  ", "", "s <- 'a # b'; Sys.time()"))
})

test_that("[CONTROL] minutes30 casa as formas de +-30 min e poupa passo de grade e minutos variaveis", {
  pat <- pc_grep_gates$pattern[pc_grep_gates$gate == "minutes30"]
  hit <- function(code) length(pc_grep(pat, write_bait(code))) == 1L
  for (code in c("e <- s + lubridate::minutes(30)", "e <- s + minutes(30L)", "s <- e - lubridate::dminutes(30)",
                 "s <- e - 1800", "e <- s + lubridate::seconds(1800)"))
    expect_true(hit(code), info = code)
  for (code in c("g <- seq(au, end, by = \"30 min\")", "a <- o + lubridate::minutes(mm)", "x <- minutes(300)",
                 "e <- s + lubridate::minutes(60)  # minutes(30)"))
    expect_false(hit(code), info = code)
})

test_that("[CONTROL] a lista de chaves proibidas do loader nao e vazia e inclui cut_invariant", {
  expect_true("cut_invariant" %in% run_forbidden)
})

## ==== GREP_GATES_M3 (M3.V): fonte unica de metrica, honesty-gate e evaluation.R sem relogio/env/alias ======================
## Modos: zero (nenhum casamento), exactly1 (exatamente 1 ocorrencia no conjunto), atleast1. Contagem em ocorrencias
## por linha sem comentario. Cada gate tem isca CONTROL em tempfile() que casa o padrao pelo MESMO predicado.
pc_count <- function(pattern, files, fixed = FALSE, base = root, strip = TRUE) {
  sum(vapply(files, function(f) {
    p <- if (isAbsolutePath(f)) f else file.path(base, f)
    ln <- readLines(p, warn = FALSE, encoding = "UTF-8")
    if (strip) ln <- pc_strip_comment(ln)
    m <- gregexpr(pattern, ln, fixed = fixed, perl = !fixed)
    sum(vapply(m, function(x) sum(x > 0L), 0L))
  }, 0L))
}
pc_gate_ok <- function(mode, n) switch(mode, zero = n == 0L, exactly1 = n == 1L, atleast1 = n >= 1L, stop("modo"))

eval_r <- "scripts/lib/evaluation.R"; hg_r <- "scripts/lib/honesty_gate.R"
grep_gates_m3 <- list(
  list(id = "m3_single_wmape", pattern = "prevcarga_wmape <- function", fixed = TRUE,
       files = pc_code_files("r_code"), mode = "exactly1", bait = "prevcarga_wmape <- function(a, b) 0"),
  list(id = "m3_sum_abs_only_eval", pattern = "sum(abs(", fixed = TRUE,
       files = setdiff(pc_code_files("r_code"), eval_r), mode = "zero", bait = "x <- sum(abs(1))"),
  ## fix/topology-ids-hour (d8086da): o rotulo de hora e convertido 1x na entrada e 1x na saida, nas bordas
  ## (deck_reader.R / schema_long.R / features.R); evaluation.R e honesty_gate.R nao fazem aritmetica de 30 min.
  ## (o plano do M3.V previa exactly1 em evaluation.R; a regra vigente e ZERO fora das bordas.)
  list(id = "m3_no_minutes30", pattern = "minutes\\(\\s*30L?\\s*\\)|[-+]\\s*1800(?![0-9])", fixed = FALSE,
       files = c(eval_r, hg_r), mode = "zero", bait = "a <- s + lubridate::minutes(30)"),
  list(id = "m3_no_clock_env",
       pattern = "Sys\\.(getenv|time|Date)|as\\.POSIXct|strptime|as\\.Date\\(|system2|callr|processx|read_yaml",
       files = c(eval_r, hg_r), mode = "zero", bait = "t <- Sys.time()"),
  list(id = "m3_no_alias_cols", pattern = "carga_mwmed|val_cargacons|ts_r30",
       files = c(eval_r, hg_r), mode = "zero", bait = "x$ts_r30"),
  list(id = "m3_no_khat_range_literal", pattern = "(?<![0-9.])1\\.15(?![0-9])",
       files = Sys.glob(file.path(root, "scripts", "lib", "*.R")), mode = "zero", bait = "k <- c(1.00, 1.15)"))

for (g in grep_gates_m3) local({
  g <- g
  files <- if (all(isAbsolutePath(g$files))) sub(paste0("^", root, "/"), "", g$files) else g$files
  test_that(paste("[FORBIDDEN]", g$id), {
    expect_gt(length(files), 0L)
    expect_true(all(file.exists(file.path(root, files))))
    n <- pc_count(g$pattern, files, fixed = isTRUE(g$fixed))
    expect_true(pc_gate_ok(g$mode, n), info = sprintf("%s: %d ocorrencia(s) em %s", g$id, n, paste(files, collapse = ",")))
  })
  test_that(paste("[CONTROL]", g$id), {
    b <- write_bait(g$bait)
    n <- pc_count(g$pattern, b, fixed = isTRUE(g$fixed))
    expect_gte(n, 1L)                                                  # a isca casa o padrao
    if (g$mode == "zero") expect_false(pc_gate_ok(g$mode, n))            # e reprova o gate de contagem zero
    if (g$mode == "exactly1" && g$id == "m3_single_wmape")               # 2a definicao: a contagem vira 2 e reprova
      expect_false(pc_gate_ok("exactly1", pc_count(g$pattern, c(files, b), fixed = TRUE)))
    if (g$id == "m3_no_minutes30") {                                     # a isca casa as formas de +-30 min
      for (code in c("e <- s + minutes(30L)", "s <- e - 1800")) expect_gte(pc_count(g$pattern, write_bait(code)), 1L)
    }
  })
})

## ---- prova de consumo por AST: a metrica e o corte so existem em um lugar ------------------------------------------
test_that("consumo por AST: score -> scorecard -> wmape; skill/peak -> wmape; baseline -> read_actual; verbos -> modulos", {
  e <- pc_source_lib(c("errors", "config", "topology", "schema_long", "provenance", "deck_reader", "dates", "validation",
                       "evaluation"))
  for (v in c("score", "check-coherence")) sys.source(file.path(root, "scripts", "lib", "cli", paste0(v, ".R")), envir = e)
  calls <- function(f, what) what %in% all.names(body(f))
  reaches <- function(f, what, seen = character()) {                    # fecho transitivo por funcoes do ambiente
    nm <- all.names(body(f))
    if (what %in% nm) return(TRUE)
    for (n in setdiff(unique(nm), seen)) {
      g <- get0(n, envir = e, mode = "function", inherits = FALSE)
      if (!is.null(g) && reaches(g, what, c(seen, n))) return(TRUE)
    }
    FALSE
  }
  expect_true(calls(e$prevcarga_score, "prevcarga_scorecard"))
  for (f in c("prevcarga_scorecard", "prevcarga_skill_score", "prevcarga_peak_wmape"))
    expect_true(calls(e[[f]], "prevcarga_wmape"), info = f)
  expect_true(calls(e$prevcarga_baseline, "prevcarga_read_actual"))
  expect_true(calls(e$cmd_score, "prevcarga_score"))
  expect_true(calls(e$cmd_check_coherence, "prevcarga_check_coherence"))
  expect_true(reaches(e$cmd_check_coherence, "prevcarga_topology"))     # via .prevcarga_score_topo (topologia como dado)
  ## CONTROL: uma funcao-isca com WMAPE inline nao chama prevcarga_wmape e e reprovada pelo mesmo predicado
  iscas <- function(y, p) mean(abs(y - p)) / mean(abs(y))
  expect_false(calls(iscas, "prevcarga_wmape"))
  expect_false(reaches(iscas, "prevcarga_wmape"))
})

## ---- gate de docs m3_docs_live (modo atleast1): o fechamento do M3 declara evaluation.R e honesty_gate.R VIVOS -------
m3_doc <- "docs/estado/m3-avaliacao.md"
m3_docs_live <- list(list(id = "m3_docs_live_evaluation", pattern = "evaluation\\.R"),
                     list(id = "m3_docs_live_honesty_gate", pattern = "honesty_gate\\.R"))
live_ok <- function(pattern, file) pc_gate_ok("atleast1", pc_count(pattern, file, strip = FALSE)) &&
  any(grepl(paste0("(?i)", pattern, "[^\\n]*VIVO|VIVO[^\\n]*", pattern), readLines(
    if (isAbsolutePath(file)) file else file.path(root, file), warn = FALSE), perl = TRUE))
for (g in m3_docs_live) local({
  g <- g
  test_that(paste("[FORBIDDEN]", g$id), {
    expect_true(file.exists(file.path(root, m3_doc)))
    expect_true(live_ok(g$pattern, m3_doc))
  })
  test_that(paste("[CONTROL]", g$id), {                                # copia sem a citacao reprova
    src <- readLines(file.path(root, m3_doc), warn = FALSE)
    cp <- tempfile("m3doc-", fileext = ".md")
    writeLines(src[!grepl(g$pattern, src, perl = TRUE)], cp)
    expect_false(live_ok(g$pattern, cp))
  })
})
