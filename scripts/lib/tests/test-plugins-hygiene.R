## test-plugins-hygiene.R -- higiene conjunta dos plugins e do motor DESSEM (M5.11; AC39). So VERIFICA a arvore real de models/:
## nao corrige plugin, motor nem golden (achado vira correcao no epico dono). Usa pc_grep_code / pc_bytes_over_127 /
## pc_grep_time_arith / pc_time_arith_allow do helper-plugins.R da fundacao (P.0j) e os helpers de helper-porte-closure.R.
## Comentario (linha so `#`) e *.md nao contam. Cada padrao tem CONTROL plantado numa COPIA em tempdir (acusa arquivo:linha).
## A excecao de hora vem SO da lista versionada pc_time_arith_allow(); este teste nao guarda lista paralela.

PCC_HYGIENE <- data.table::rbindlist(list(
  list("env",         "plugins+engine", "Sys[.](getenv|setenv)",                                    "x <- Sys.getenv('HOME')",                "random-forest/R/ingestion.R"),
  list("clock",       "plugins+engine", "Sys[.](time|Date)|\\bnow\\(|\\btoday\\(",                  "x <- Sys.time()",                        "random-forest/R/ingestion.R"),
  list("base_dates",  "plugins+engine", "as[.]POSIX(ct|lt)|strptime|as[.]Date\\(|difftime",         "x <- as.Date('2026-01-01')",             "lgbm/R/ingestion.R"),
  list("global",      "plugins+engine", "setwd\\(|<<-|globalenv\\(",                                 "g <<- 1",                                "holt-winters/R/training.R"),
  list("io",          "plugins+engine", "saveRDS\\(",                                               "saveRDS(x, 'a.rds')",                    "holt-winters/R/freeze.R"),
  list("shell",       "plugins+engine", "system2?\\(",                                              "system('ls')",                           "lgbm/R/training.R"),
  list("fork",        "plugins+engine", "mclapply",                                                 "mclapply(1:2, identity)",                "lgbm/R/training.R"),
  list("symlink",     "plugins+engine", "file[.]symlink",                                           "file.symlink('a', 'b')",                 "random-forest/R/freeze.R"),
  list("load",        "plugins+engine", "library\\(|source\\(",                                     "library(stats)",                         "random-forest/R/register.R"),
  list("job_env",     "plugins+engine", "PREVCARGA_JOB",                                            "x <- 'PREVCARGA_JOB'",                   "dessem-regdin/R/register.R"),
  list("auto_cut",    "plugins+engine", "max\\(.*timestamp",                                        "m <- max(panel$timestamp)",              "holt-winters/R/ingestion.R"),
  list("ascii",       "plugins+engine", "[^\\x01-\\x7F]",                                           "stop('a\u00e7')",                        "lgbm/R/inference.R"),
  list("hours_arith", "plugins+engine", "%m[-+]%\\s*(lubridate::)?hours\\(",                        "y <- x %m-% hours(1)",                   "dessem-linear/R/training.R"),
  list("sec_const",   "plugins+engine", "3600|1800",                                                "y <- x + 3600",                          "dessem-linear/R/ingestion.R"),
  list("hours_call",  "plugins+engine", "(^|[^_a-zA-Z.])(lubridate::)?hours\\(",                    "h <- hours(2)",                          "dessem-regdin/R/ingestion.R"),
  list("minutes",     "plugins+engine", "(lubridate::)?minutes\\(\\s*(30|60)\\s*\\)",               "m <- lubridate::minutes(30)",            "holt-winters/R/inference.R"),
  list("frames",      "plugins+engine", "to_model_frame|to_display_frame",                          "y <- to_model_frame(x, 60L)",            "random-forest/R/inference.R"),
  list("engine_nowcast", "engine",      "nowcast|[.]rd_nowcast",                                    "cabeca_nowcast <- 1",                    "_dessem-engine/R/funcoes_apoio.R"),
  list("run_yaml_keys", "run_yaml",     "^(resources|vcpu|mem_gb|r_deps|entrypoint|cut_invariant|emits_day_O):", "entrypoint: scripts/foo.R", "lgbm/run.yaml")),
  use.names = FALSE)
data.table::setnames(PCC_HYGIENE, c("id", "scope", "regex", "sample", "plant"))

## Interino conhecido e FIXADO por arquivo (contexto 2026-10-30): busca de funcoes da fundacao no globalenv enquanto o loader nao
## expoe a API (fix definitivo e da fundacao). Nao somar hit novo: o numero por diretorio e exato.
PCC_GLOBAL_PINNED <- c("_dessem-engine" = 1L, "dessem-linear" = 3L, "dessem-regdin" = 3L)

.pcc_fmt <- function(h) if (!nrow(h)) "" else paste0(basename(h$file), ":", h$line, " [", h$pattern, "]", collapse = "\n")

## Hits da regra `id` sobre o escopo, em `models_dir`.
.pcc_row <- function(rid) PCC_HYGIENE[which(PCC_HYGIENE[["id"]] == rid)]   # `id` dentro de [.data.table seria a coluna
.pcc_hits <- function(models_dir, rid) {
  r <- .pcc_row(rid)
  files <- pcc_scope_files(models_dir, r$scope)
  if (identical(rid, "ascii")) return(pc_bytes_over_127(files))
  pc_grep_code(files, stats::setNames(list(r$regex), rid))
}

.pcc_pinned <- function(id) if (identical(id, "global")) PCC_GLOBAL_PINNED else integer()

## contagem de hits por diretorio de modelo (models/<dir>/R/x.R -> <dir>; run.yaml -> <dir>)
.pcc_by_model <- function(h) {
  if (!nrow(h)) return(integer())
  d <- basename(ifelse(grepl("[.]R$", h$file), dirname(dirname(h$file)), dirname(h$file)))
  tb <- table(d)
  out <- stats::setNames(as.integer(tb), names(tb))
  out[order(names(out), method = "radix")]
}

test_that("[ac-39] a tabela de higiene cobre todos os padroes do ticket e os 3 escopos", {
  expect_setequal(PCC_HYGIENE$id, c("env", "clock", "base_dates", "global", "io", "shell", "fork", "symlink", "load", "job_env",
                                     "auto_cut", "ascii", "hours_arith", "sec_const", "hours_call", "minutes", "frames",
                                     "engine_nowcast", "run_yaml_keys"))
  expect_setequal(PCC_HYGIENE$scope, c("plugins+engine", "engine", "run_yaml"))
  ## o escopo varre todos os plugins E o motor, vindos do disco (nunca lista fixa)
  fs <- pcc_scope_files(pcc_models_dir(), "plugins+engine")
  expect_true(any(grepl("/_dessem-engine/R/", fs, fixed = TRUE)))
  for (m in pcc_all_plugins()) expect_true(any(grepl(sprintf("/%s/R/", m), fs, fixed = TRUE)), info = m)
  expect_length(pcc_scope_files(pcc_models_dir(), "run_yaml"), length(pcc_all_plugins()))
  expect_false(any(grepl("[.]md$", c(fs, pcc_scope_files(pcc_models_dir(), "run_yaml")))))
})

for (id in PCC_HYGIENE$id) {
  local({
    id <- id
    test_that(paste0("[ac-39] arvore real limpa: ", id), {
      h <- .pcc_hits(pcc_models_dir(), id)
      expect_identical(.pcc_by_model(h), .pcc_pinned(id), info = .pcc_fmt(h))
      if (identical(id, "global"))
        expect_true(all(grepl("get0\\(", h$text)), info = "o interino pinado e so get0(..., envir = globalenv())")
    })

    test_that(paste0("[ac-39][CONTROL] dente: ", id, " plantado em copia e acusado em arquivo:linha"), {
      r <- .pcc_row(id)
      cp <- pcc_models_copy()
      base <- .pcc_hits(cp, id)
      pl <- pcc_plant(cp, r$plant, r$sample)
      h <- .pcc_hits(cp, id)
      expect_equal(nrow(h), nrow(base) + 1L, info = .pcc_fmt(h))
      expect_true(any(normalizePath(h$file) == normalizePath(pl$file) & h$line == pl$line), info = .pcc_fmt(h))
      ## a mesma linha comentada nao conta
      pcc_plant(cp, r$plant, paste("#", r$sample))
      expect_equal(nrow(.pcc_hits(cp, id)), nrow(h))
      ## nem num README.md (*.md nunca e varrido)
      readme <- file.path(cp, "random-forest", "README.md")
      if (!file.exists(readme)) writeLines("x", readme)
      pcc_plant(cp, "random-forest/README.md", r$sample)
      expect_equal(nrow(.pcc_hits(cp, id)), nrow(h))
    })
  })
}

test_that("[ac-39] aritmetica de hora: zero em todo plugin e no motor; excecao SO da lista versionada da fundacao", {
  mdir <- pcc_models_dir()
  eng <- file.path(mdir, "_dessem-engine")
  for (m in pcc_all_plugins()) {
    md <- file.path(mdir, m)
    h <- pc_grep_time_arith(md, allow = pc_time_arith_allow(), engine_dir = if (identical(m, "dessem-linear") || identical(m, "dessem-regdin")) eng)
    expect_equal(nrow(h), 0L, info = paste(m, .pcc_fmt(h)))
    ## sem a lista tambem zero: as excecoes versionadas (+ hours(6) do HW, hours(lagh) do motor) nao sao usadas hoje
    h0 <- pc_grep_time_arith(md, allow = list(), engine_dir = if (identical(m, "dessem-linear") || identical(m, "dessem-regdin")) eng)
    expect_equal(nrow(h0), 0L, info = paste(m, "(allow = list())", .pcc_fmt(h0)))
  }
  expect_equal(nrow(pc_grep_time_arith(eng, allow = list())), 0L)
})

test_that("[ac-39][CONTROL] excecao de hora so pelo arquivo:padrao de pc_time_arith_allow(); remover a entrada faz o hit aparecer", {
  cp <- pcc_models_copy()
  allow <- pc_time_arith_allow()
  allowed_files <- unique(vapply(strsplit(names(allow), " ", fixed = TRUE), `[`, "", 1L))
  expect_true(length(allowed_files) >= 1L)
  ## o arquivo da excecao (basename declarado na lista da fundacao) com `+ hours(6)`: passa com a lista, acusado sem ela
  f <- file.path(cp, "holt-winters", "R", allowed_files[[1L]])
  writeLines("dst <- x + hours(6)", f)
  expect_equal(nrow(pc_grep_time_arith(file.path(cp, "holt-winters"), allow = allow)), 0L)
  h <- pc_grep_time_arith(file.path(cp, "holt-winters"), allow = list())
  expect_true(nrow(h) >= 1L); expect_identical(unique(basename(h$file)), allowed_files[[1L]]); expect_true(all(h$line == 1L))
  ## remover UMA entrada: o padrao dela reaparece (arquivo:linha)
  h1 <- pc_grep_time_arith(file.path(cp, "holt-winters"), allow = allow[-1L])
  expect_true(nrow(h1) >= 1L)
  expect_true(unlist(strsplit(names(allow)[[1L]], " ", fixed = TRUE))[[2L]] %in% h1$pattern)
  ## o mesmo `+ hours(6)` FORA do arquivo declarado e acusado, e `hours(lagh)` no motor tambem
  p1 <- pcc_plant(cp, "random-forest/R/ingestion.R", "dst <- x + hours(6)")
  h2 <- pc_grep_time_arith(file.path(cp, "random-forest"), allow = allow)
  expect_true(any(normalizePath(h2$file) == normalizePath(p1$file) & h2$line == p1$line))
  p2 <- pcc_plant(cp, "_dessem-engine/R/funcoes_apoio.R", "lag <- t - hours(lagh)")
  h3 <- pc_grep_time_arith(file.path(cp, "dessem-linear"), allow = allow, engine_dir = file.path(cp, "_dessem-engine"))
  expect_true(any(normalizePath(h3$file) == normalizePath(p2$file) & h3$line == p2$line))
  ## to_hourly / to_semihourly sao permitidos; comentario nao conta
  pcc_plant(cp, "lgbm/R/ingestion.R", "x <- to_hourly(p); y <- to_semihourly(p)")
  pcc_plant(cp, "lgbm/R/ingestion.R", "# z <- x + hours(1)")
  expect_equal(nrow(pc_grep_time_arith(file.path(cp, "lgbm"), allow = allow)), 0L)
})

test_that("[ac-39] a lista de excecoes de hora e fechada e vive so na fundacao (sem lista paralela neste arquivo)", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "tests", "test-plugins-hygiene.R"), warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  ## o texto das excecoes (basename:padrao) nao e repetido aqui
  expect_false(any(grepl("holt_winters[.]R period_arith|holt_winters[.]R hours_call", src)))
  expect_true(all(grepl(" ", names(pc_time_arith_allow()), fixed = TRUE)))
})

test_that("[ac-39] write-through: o estado do fechamento do porte esta em docs/estado/", {
  f <- file.path(pc_root(), "docs", "estado", "plugins_porte_fechamento.md")
  expect_true(file.exists(f))
  txt <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (tok in c("M5.11", "test-members-coverage.R", "test-plugins-hygiene.R", "test-plugins-isolation.R", "pcc_snapshot",
                "pc_time_arith_allow", "DEFERRED M8"))
    expect_true(grepl(tok, txt, fixed = TRUE), info = tok)
})

test_that("[DEFERRED M8] braco cross-OS: Windows nativo nao roda aqui; so a parte estatica de portabilidade dos testes do fechamento", {
  ## A execucao em Windows nativo (PowerShell) fica para o M8. Aqui: nenhum teste/helper do fechamento usa o que nao e portavel.
  fs <- file.path(pc_root(), "scripts", "lib", "tests", c("helper-porte-closure.R", "test-members-coverage.R",
                                                         "test-plugins-hygiene.R", "test-plugins-isolation.R"))
  expect_true(all(file.exists(fs)))
  ## (este arquivo traz as linhas-amostra da tabela de higiene como texto; so os outros tres entram no grep de portabilidade)
  h <- pc_grep_code(fs[basename(fs) != "test-plugins-hygiene.R"],
                    list(symlink = "file[.]symlink|Sys[.]readlink", fork = "mclapply|parallel::mc", setenv = "Sys[.](setenv|getenv)",
                         shell = "(^|[^2a-z_.])system[(]|bash|/tmp/"))
  expect_equal(nrow(h), 0L, info = .pcc_fmt(h))
  expect_true(all(vapply(fs, function(f) all(utf8ToInt(paste(readLines(f, warn = FALSE), collapse = "")) < 128L), NA)))
})
