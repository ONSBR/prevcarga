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
           "topo_literal", "khat_literal", "dessem_switch", "run_yaml_keys", "registry_lifecycle", "bytes"),
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
    "[\\x80-\\xFF]"),
  allow = I(list(
    "scripts/lib/runtime.R", character(), character(), "scripts/lib/schema_long.R",
    character(),                      # divida do combination.R quitada em M4.2b (lubridate::as_datetime): zero hits
    "scripts/lib/config.R",           # lateral run_id (unico relogio permitido)
    character(), character(), character(), "config/topology.yaml", character(), character(),
    character(), character())),
  bait = c("x <- Sys.getenv('TZ')", "p <- 'PREVCARGA_DATA'", "prevcarga_verbs <- c('a')",
           "prevcarga_schema_long <- c(a = 'b')", "d <- as.Date('2024-01-01')", "t0 <- Sys.time()",
           "system('ls')", "download.file(u, f)", NA_character_, "k <- c(1.00, 1.15)",
           "f <- 'dessem-regdin'  ; x == \"dessem-regdin\"", "cut_invariant: true", "promote <- function() 1",
           "msg <- 'a\\u00e7ao'"),
  scope = c("r_code", "code", "r_code", "r_code", "r_code", "r_code", "code", "code", "r_code", "code", "r_code",
            "run_yaml", "registry", "messages"),
  strip = c(rep(TRUE, 13), FALSE),
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

test_that("[CONTROL] a lista de chaves proibidas do loader nao e vazia e inclui cut_invariant", {
  expect_true("cut_invariant" %in% run_forbidden)
})
