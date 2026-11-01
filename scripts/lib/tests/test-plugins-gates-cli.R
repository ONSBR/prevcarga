## test-plugins-gates-cli.R -- fechamento do porte (2/2) (M5.12; AC42, AC43, AC44). So VERIFICA: nenhum plugin, motor, golden ou
## fachada e alterado aqui; achado vira correcao no epico dono.
## Bloco 1: os runners models/<m>/tests/run_all_gates.R (descobertos por Sys.glob, nunca lista fixa) como PROCESSOS, --tier fast e
##          --tier parity: exit 0, zero [DEFERRED M5], todo [DEFERRED Mx] restante e SKIP com dono.
## Bloco 2: TZ sujo sozinho sai 0 (o runtime sobrescreve o shell); o setter de TZ quebrado (copia da instalacao) sai 7 e stdout vazio.
## Bloco 3: aditividade (manifesto sha256 de models/ + carimbo de tempo) e oraculo intacto; CONTROLs acusam.
## Bloco 4: parity-full (so Linux; ligado por options(prevcarga.closure_tier = "parity-full"), nunca por env; filtro opcional
##          options(prevcarga.closure_models = c("lgbm", ...))): max|Delta y_pred| <= 1e-5 nos 8 membros, sha256 dos relatorios,
##          foundation_report.json == requires_report do fan-out; CONTROL golden.lock com fingerprint alterado => runner sai != 0.
## Bloco 5: write-through (READMEs dos plugins e docs/estado/m5-training.md); CONTROLs com copias incompletas reprovam.
## Sem variavel de ambiente no pacote (so o filho de TESTE recebe TZ sujo, por pc_with_env); tudo em tempdir; datas por lubridate.

PCG_TOL <- 1e-5
PCG_TIERS <- c("fast", "parity")
## sob o Rscript do bundle instalado (tests/run_suite.R, M8.12) so o tier fast: o golden.lock pina o BLAS do ambiente de dev que
## gerou o golden (tools.golden_stale "item: blas" sob outro BLAS); a paridade contra o golden e provada no dev (fechamento do M5.12)
PCG_TIERS <- intersect(PCG_TIERS, getOption("prevcarga.closure_tiers", PCG_TIERS))
PCG_PARITY_SKIP <- "paridade pinada ao BLAS do golden: roda no ambiente de dev (fechamento do M5.12, docs/estado/m5-training.md); sob o bundle so --tier fast (M8.12 AC 1)"
## o oraculo (somente leitura) tem exatamente estas entradas nao rastreadas (docs/roadmap/*.md + verify-log/); nada alem delas
PCG_ORACLE <- "/opt/ons-source/prevcargadessem3"
PCG_ORACLE_TOLERATED <- c("?? docs/roadmap/CLI-MODULAR-OBJETIVO.md", "?? docs/roadmap/CLI-MODULAR-ONDE-ESTAMOS.md",
                          "?? docs/roadmap/MIGRACAO-COMBINA-R.md", "?? docs/roadmap/verify-log/")
## divergencias ACEITAS e NOMEADAS (docs/estado/dessem_linear.md DL.9): fora delas o teto e PCG_TOL
PCG_NUMERIC_EXCEPTIONS <- c("sp-2024-11-15" = 0.5)

## ---- helpers ------------------------------------------------------------------------------------------------------
pcg_runners <- function(root = pc_root()) sort(Sys.glob(file.path(root, "models", "*", "tests", "run_all_gates.R")), method = "radix")
pcg_mdir_of <- function(runner) basename(dirname(dirname(runner)))

## linhas da tabela impressa: `gate | [DONO] | STATUS | detalhe`
pcg_parse_gate_table <- function(lines) {
  m <- regmatches(lines, regexec("^(\\S+)\\s*\\|\\s*\\[([^]]+)\\]\\s*\\|\\s*(PASS|FAIL|SKIP)\\s*\\|?(.*)$", lines))
  m <- m[lengths(m) == 5L]
  if (!length(m)) return(data.frame(id = character(), owner = character(), status = character(), detail = character(),
                                    stringsAsFactors = FALSE))
  data.frame(id = vapply(m, `[[`, "", 2L), owner = vapply(m, `[[`, "", 3L), status = vapply(m, `[[`, "", 4L),
             detail = trimws(vapply(m, `[[`, "", 5L)), stringsAsFactors = FALSE)
}

pcg_run_runner <- function(runner, tier, args = character()) {
  out <- tempfile("pcg-out-"); err <- tempfile("pcg-err-")
  rc <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner), "--tier", tier, args),
                                 stdout = out, stderr = err))
  list(exit = as.integer(rc), stdout = readLines(out, warn = FALSE), stderr = readLines(err, warn = FALSE),
       table = pcg_parse_gate_table(readLines(out, warn = FALSE)))
}

## sha256 por arquivo de models/ (caminho relativo, ordem radix), sem os artefatos models/<m>/models/
pcg_models_manifest <- function(models_dir) {
  f <- list.files(models_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  f <- f[!grepl("^[^/]+/models/", f)]
  f <- sort(f, method = "radix")
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(models_dir, x), algo = "sha256"), ""), f)
}

pcg_additivity <- function(root, manifest0, stamp) {
  md <- file.path(root, "models")
  m1 <- pcg_models_manifest(md)
  novos <- list.files(md, recursive = TRUE, full.names = TRUE)
  novos <- novos[which(file.info(novos)$mtime > stamp)]
  list(manifest_equal = identical(manifest0, m1), newer = novos,
       store_dirs = Sys.glob(file.path(md, "*", "models")))
}

## oraculo: `git status --porcelain` (menos as entradas toleradas) e arquivos mais novos que o carimbo (fora de .git/)
pcg_oracle_state <- function(oracle, stamp_file, tolerated = PCG_ORACLE_TOLERATED) {
  st <- suppressWarnings(system2("git", c("-C", shQuote(oracle), "status", "--porcelain"), stdout = TRUE, stderr = FALSE))
  fs <- list.files(oracle, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  fs <- fs[!grepl("/\\.git/", fs)]
  list(status = setdiff(trimws(st), tolerated), newer = fs[which(file.info(fs)$mtime > file.info(stamp_file)$mtime)])
}

## repositorio-fixture que imita o oraculo (git proprio, 1 commit) em tempdir
pcg_fake_oracle <- function(.local_envir = parent.frame()) {
  d <- withr::local_tempdir(pattern = "pcg-oracle-", .local_envir = .local_envir)
  writeLines("x", file.path(d, "a.R"))
  sh <- function(...) suppressWarnings(system2("git", c("-C", shQuote(d), ...), stdout = FALSE, stderr = FALSE))
  sh("init", "-q"); sh("config", "user.email", "t@t"); sh("config", "user.name", "t"); sh("add", "a.R"); sh("commit", "-q", "-m", "x")
  d
}

## ---- estado compartilhado do arquivo: manifesto e carimbo gravados ANTES de qualquer runner -----------------------------
.pcg <- local({
  st <- NULL
  list(get = function() {
    if (is.null(st)) {
      root <- pc_root()
      stamp_file <- tempfile("pcg-stamp-")
      writeLines("stamp", stamp_file)
      Sys.setFileTime(stamp_file, Sys.time())
      st <<- list(root = root, stamp_file = stamp_file, stamp = file.info(stamp_file)$mtime,
                  manifest0 = pcg_models_manifest(file.path(root, "models")), runs = list())
      Sys.sleep(1.1)                                    # o mtime de qualquer arquivo tocado depois e estritamente maior
    }
    st
  }, run = function(mdir, tier) {
    s <- .pcg$get()
    key <- paste(mdir, tier)
    if (is.null(s$runs[[key]])) {
      r <- pcg_run_runner(file.path(s$root, "models", mdir, "tests", "run_all_gates.R"), tier)
      st$runs[[key]] <<- r
    }
    st$runs[[key]]
  })
})

pcg_tier <- function() getOption("prevcarga.closure_tier", "fast")
pcg_models_sel <- function(all) {
  sel <- getOption("prevcarga.closure_models", NULL)
  if (is.null(sel)) all else intersect(all, sel)
}

## ---- bloco 1: os 5 runners x fast/parity como processos --------------------------------------------------------------
test_that("[ac-42] os runners sao descobertos por glob e cada mdir da topologia tem o seu", {
  s <- .pcg$get()
  mdirs <- vapply(pcg_runners(s$root), pcg_mdir_of, "")
  expect_setequal(mdirs, pcc_all_plugins())
  expect_gte(length(mdirs), 5L)
})

test_that("[ac-42] 5 runners x {fast, parity}: 10 processos saem 0; zero [DEFERRED M5]; [DEFERRED Mx] restante e SKIP com dono", {
  s <- .pcg$get()
  mdirs <- vapply(pcg_runners(s$root), pcg_mdir_of, "")
  n_proc <- 0L
  for (m in mdirs) for (tier in PCG_TIERS) {
    r <- .pcg$run(m, tier)
    n_proc <- n_proc + 1L
    info <- sprintf("%s --tier %s\n%s", m, tier, paste(utils::tail(c(r$stdout, r$stderr), 25L), collapse = "\n"))
    expect_identical(r$exit, 0L, info = info)
    expect_gt(nrow(r$table), 10L)
    expect_false(any(r$table$status == "FAIL"), info = info)
    expect_identical(sum(r$table$owner == "DEFERRED M5"), 0L, info = paste(m, tier, "[DEFERRED M5] sobrando"))
    expect_identical(sum(r$table$owner == "DEFERRED M6"), 0L, info = paste(m, tier, "[DEFERRED M6] sobrando: o pack m6_long.R do M6.6 os substitui"))
    dfd <- r$table[grepl("^DEFERRED", r$table$owner), ]
    expect_true(all(dfd$status == "SKIP"), info = info)
    expect_true(all(grepl("^DEFERRED M[0-9]+$", dfd$owner)), info = info)               # SKIP com dono
    ## os packs M5 e M6 por descoberta estao na tabela e verdes
    expect_true(any(r$table$owner == "M5" & r$table$status == "PASS"), info = paste(m, tier, "pack m5_facades.R nao descoberto"))
    expect_true(any(r$table$owner == "M6" & r$table$status == "PASS"), info = paste(m, tier, "pack m6_long.R nao descoberto"))
  }
  expect_identical(n_proc, length(mdirs) * length(PCG_TIERS))
  expect_identical(n_proc, 10L)
})

test_that("[ac-42] parser da tabela: [DEFERRED M5] sobrando e linha FAIL sao vistos (CONTROL do instrumento)", {
  tab <- pcg_parse_gate_table(c("fachada          | [DEFERRED M5]  | SKIP      | Reason: x", "contrato | [PLUGINS] | FAIL | y",
                                "lixo sem barras"))
  expect_identical(nrow(tab), 2L)
  expect_identical(sum(tab$owner == "DEFERRED M5"), 1L)
  expect_identical(tab$status, c("SKIP", "FAIL"))
})

## ---- bloco 2: runtime ------------------------------------------------------------------------------------------------
test_that("[ac-42] TZ sujo sozinho no ambiente de entrada: o runner sai 0 (o runtime sobrescreve o shell)", {
  s <- .pcg$get()
  runner <- file.path(s$root, "models", "holt-winters", "tests", "run_all_gates.R")
  r <- pc_with_env(list(TZ = "America/Sao_Paulo"), pcg_run_runner(runner, "fast"))
  expect_identical(r$exit, 0L, info = paste(utils::tail(c(r$stdout, r$stderr), 15L), collapse = "\n"))
  expect_false(any(r$table$status == "FAIL"))
})

test_that("[ac-42] setter de TZ quebrado (copia da instalacao): o runner sai 7 e stdout vazio; copia intacta sai 0 ate a tabela", {
  skip_on_os("windows")
  s <- .pcg$get()
  ## copia da instalacao com o setter de TZ trocado por um que so OBSERVA o TZ do shell (nao o fixa): so a copia e alterada
  break_tz <- function(root) cat(c("", "## [M5.12 fixture] setter de TZ quebrado, so nesta copia",
    ".pcg_orig_setters <- prevcarga_runtime_setters",
    "prevcarga_runtime_setters <- function() { s <- .pcg_orig_setters(); s$tz <- function() .prevcarga_tz_observed(); s }"),
    file = file.path(root, "scripts", "lib", "runtime.R"), sep = "\n", append = TRUE)
  bad <- pc_install_copy(mutate = break_tz, plugins = TRUE)
  withr::defer(unlink(dirname(bad), recursive = TRUE))
  tdir <- file.path(bad, "models", "random-forest", "tests")
  dir.create(tdir, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(s$root, "models", "random-forest", "tests", "run_all_gates.R"), tdir)
  r <- pc_with_env(list(TZ = "America/Sao_Paulo"), pcg_run_runner(file.path(tdir, "run_all_gates.R"), "fast"))
  expect_identical(r$exit, 7L, info = paste(r$stderr, collapse = "\n"))
  expect_length(r$stdout, 0L)
  expect_true(any(grepl("runtime", r$stderr)))
})

test_that("[ac-42] tier parity dos 5 runners (golden pinado ao BLAS do dev)", {
  if (!"parity" %in% PCG_TIERS) skip(PCG_PARITY_SKIP)
  expect_true("parity" %in% PCG_TIERS)                       # no dev o laco acima ja rodou o parity dos 5 runners
})

## ---- bloco 3: aditividade e oraculo intacto ------------------------------------------------------------------------
test_that("[ac-42] aditividade: manifesto sha256 de models/ identico, nada mais novo que o carimbo, nenhum models/*/models/", {
  s <- .pcg$get()
  for (m in vapply(pcg_runners(s$root), pcg_mdir_of, "")) for (tier in PCG_TIERS) .pcg$run(m, tier)
  a <- pcg_additivity(s$root, s$manifest0, s$stamp)
  expect_true(a$manifest_equal)
  expect_identical(a$newer, character(), info = paste(a$newer, collapse = "\n"))
  expect_identical(a$store_dirs, character())
})

test_that("[ac-42] oraculo intacto: git status so com as 3 docs/roadmap/*.md + verify-log/ toleradas e nada mais novo que o carimbo", {
  skip_if_not(dir.exists(PCG_ORACLE), "oraculo ausente nesta maquina")
  s <- .pcg$get()
  for (m in vapply(pcg_runners(s$root), pcg_mdir_of, "")) for (tier in PCG_TIERS) .pcg$run(m, tier)
  o <- pcg_oracle_state(PCG_ORACLE, s$stamp_file)
  expect_identical(o$status, character(), info = paste(o$status, collapse = "\n"))
  expect_identical(o$newer, character(), info = paste(utils::head(o$newer, 10L), collapse = "\n"))
})

test_that("[CONTROL] arquivo gravado em models/ por um plugin-fixture e arquivo tocado na copia-fixture do oraculo sao acusados", {
  skip_on_os("windows")
  root <- withr::local_tempdir(pattern = "pcg-root-")
  md <- file.path(root, "models"); dir.create(file.path(md, "toy", "R"), recursive = TRUE)
  writeLines("f <- function() 1", file.path(md, "toy", "R", "a.R"))
  stamp_file <- tempfile("pcg-stamp-"); writeLines("s", stamp_file)
  Sys.setFileTime(stamp_file, Sys.time()); Sys.sleep(1.1)
  m0 <- pcg_models_manifest(md)
  stamp <- file.info(stamp_file)$mtime
  expect_true(pcg_additivity(root, m0, stamp)$manifest_equal)
  expect_length(pcg_additivity(root, m0, stamp)$newer, 0L)
  ## plugin-fixture que grava em models/ (arquivo novo) e um modelo-treinado em models/<m>/models/
  writeLines("vazou", file.path(md, "toy", "R", "efeito.txt"))
  dir.create(file.path(md, "toy", "models")); writeLines("art", file.path(md, "toy", "models", "a.rds"))
  a <- pcg_additivity(root, m0, stamp)
  expect_false(a$manifest_equal)
  expect_true(any(grepl("efeito.txt", a$newer, fixed = TRUE)))
  expect_length(a$store_dirs, 1L)
  ## oraculo-fixture: arquivo tocado depois do carimbo e arquivo nao rastreado
  orc <- pcg_fake_oracle()
  stamp2 <- tempfile("pcg-stamp-"); writeLines("s", stamp2); Sys.setFileTime(stamp2, Sys.time()); Sys.sleep(1.1)
  expect_identical(pcg_oracle_state(orc, stamp2)$status, character())
  expect_identical(pcg_oracle_state(orc, stamp2)$newer, character())
  writeLines("y", file.path(orc, "a.R"))                              # modifica o rastreado
  writeLines("z", file.path(orc, "novo.R"))                           # cria um nao rastreado
  o <- pcg_oracle_state(orc, stamp2)
  expect_true(any(grepl("a.R", o$status, fixed = TRUE)))
  expect_true(any(grepl("novo.R", o$status, fixed = TRUE)))
  expect_true(any(grepl("a.R$", o$newer)) && any(grepl("novo.R$", o$newer)))
})

## ---- bloco 4: parity-full consolidado (so Linux; so no fechamento) ----------------------------------------------------
PCG_REPORTS <- list(                       # onde cada plugin deixa o(s) relatorio(s) de paridade (gitignorados, exceto o da fundacao)
  "random-forest" = c(".state/golden/parity_report_foundation.json"),
  lgbm            = character(),            # LGBM.5 compara em processo e nao grava relatorio: o veredito e a tabela do runner
  "holt-winters"  = c(".state/parity/holt-winters/parity_report.json"),
  "dessem-linear" = c("models/dessem-linear/tests/parity/foundation_report.json"),
  "dessem-regdin" = c(".state/parity/dessem-regdin/parity_report.json"))

## todos os {unit, max_abs_delta} do relatorio (qualquer profundidade); chaves `unit` ou `id`
pcg_collect_deltas <- function(x, acc = list()) {
  if (is.list(x)) {
    if (!is.null(x$max_abs_delta) && is.numeric(x$max_abs_delta)) {
      u <- x$unit; if (is.null(u)) u <- x$id; if (is.null(u)) u <- NA_character_
      acc[[length(acc) + 1L]] <- list(unit = as.character(u), d = as.numeric(x$max_abs_delta))
    }
    for (e in x) acc <- pcg_collect_deltas(e, acc)
  }
  acc
}

pcg_delta_ok <- function(unit, d) {
  cap <- PCG_NUMERIC_EXCEPTIONS[unit]
  d <= if (is.na(cap)) PCG_TOL else cap
}

pcg_full_cache <- local({
  e <- new.env()
  function(mdir) {
    if (is.null(e[[mdir]])) {
      s <- .pcg$get()
      ## options(prevcarga.closure_captured = <dir>): saida (<mdir>.out) e exit (<mdir>.exit) de uma rodada anterior do mesmo runner
      ## (o parity-full leva de minutos a ~1 h por plugin); sem o dir, o teste roda o runner como processo
      cap <- getOption("prevcarga.closure_captured", NULL)
      if (!is.null(cap) && file.exists(file.path(cap, paste0(mdir, ".exit")))) {
        out <- readLines(file.path(cap, paste0(mdir, ".out")), warn = FALSE)
        e[[mdir]] <- list(exit = as.integer(readLines(file.path(cap, paste0(mdir, ".exit")), warn = FALSE)[1L]),
                          stdout = out, stderr = character(), table = pcg_parse_gate_table(out))
      } else {
        e[[mdir]] <- pcg_run_runner(file.path(s$root, "models", mdir, "tests", "run_all_gates.R"), "parity-full")
      }
    }
    e[[mdir]]
  }
})

test_that("[ac-43] parity-full dos 5 runners: 8 membros verdes, max|Delta y_pred| <= 1e-5 MW, sha256 dos relatorios", {
  skip_on_os("windows")                                               # parity-full: so Linux, so no fechamento
  skip_if_not(identical(pcg_tier(), "parity-full"), "parity-full so no fechamento (options(prevcarga.closure_tier = 'parity-full'))")
  s <- .pcg$get()
  for (m in pcg_models_sel(vapply(pcg_runners(s$root), pcg_mdir_of, ""))) {
    r <- pcg_full_cache(m)
    info <- sprintf("%s --tier parity-full\n%s", m, paste(utils::tail(c(r$stdout, r$stderr), 25L), collapse = "\n"))
    expect_identical(r$exit, 0L, info = info)
    expect_false(any(r$table$status == "FAIL"), info = info)
    reps <- file.path(s$root, PCG_REPORTS[[m]])
    reps <- reps[file.exists(reps)]
    if (!length(PCG_REPORTS[[m]])) {                                   # sem relatorio proprio: o gate `paridade` do runner e o veredito
      expect_identical(r$table$status[r$table$id %in% c("paridade", "paridade_control")], c("PASS", "PASS"), info = info)
      message(sprintf("[parity-full] %s sem relatorio de arquivo; sha256 da tabela do runner=%s", m,
                      digest::digest(paste(r$stdout, collapse = "\n"), algo = "sha256", serialize = FALSE)))
    } else expect_gt(length(reps), 0L, label = paste(m, "relatorio de paridade"))
    for (p in reps) {
      j <- jsonlite::read_json(p, simplifyVector = FALSE)
      ds <- pcg_collect_deltas(j$units)                               # so `units`: `controls` (HW) trazem Delta > 1e-5 DE PROPOSITO
      expect_gt(length(ds), 0L)
      for (x in ds) expect_true(pcg_delta_ok(x$unit, x$d), info = sprintf("%s %s max|D|=%g", m, x$unit, x$d))
      message(sprintf("[parity-full] %s %s sha256=%s unidades=%d", m, sub(paste0(s$root, "/"), "", p, fixed = TRUE),
                      digest::digest(file = p, algo = "sha256"), length(ds)))
    }
  }
})

test_that("[ac-43] foundation_report.json do dessem-linear: sha256 igual ao requires_report de toda unidade do fan-out do dessem-regdin", {
  skip_on_os("windows")
  skip_if_not(identical(pcg_tier(), "parity-full"), "parity-full so no fechamento")
  s <- .pcg$get()
  rep <- file.path(s$root, "models", "dessem-linear", "tests", "parity", "foundation_report.json")
  skip_if_not(file.exists(rep))
  j <- jsonlite::read_json(rep, simplifyVector = FALSE)
  sha <- j$sha256                                                    # o `sha256` do relatorio (bloco `units` canonico), o que o fan-out confere
  expect_match(sha, "^[0-9a-f]{64}$")
  expect_true(isTRUE(j$all_ok))
  message(sprintf("[parity-full] foundation_report.json sha256(bloco units)=%s sha256(arquivo)=%s", sha, digest::digest(file = rep, algo = "sha256")))
  for (m in c("dessem-regdin")) {
    u <- yaml::read_yaml(file.path(s$root, "models", m, "tests", "parity", "units.yaml"))$units
    req <- Filter(Negate(is.null), lapply(u, function(x) x$requires_report))
    expect_gt(length(req), 0L)
    for (x in req) {
      expect_identical(x$path, "models/dessem-linear/tests/parity/foundation_report.json")
      expect_identical(x$sha256, sha)
    }
  }
})

test_that("[CONTROL ac-43] golden.lock com fingerprint alterado numa copia: o runner sai != 0 e a ferramenta pede regerar o golden", {
  skip_on_os("windows")
  s <- .pcg$get()
  skip_if_not(dir.exists(file.path(s$root, ".state", "golden", "plugins", "random-forest")), "golden ausente")
  skip_if_not(file.exists(file.path(s$root, ".state", "deck-store", "manifest.json")), "overlay do store ausente")
  skip_if_not(nzchar(Sys.which("git")), "git indisponivel")
  m <- "random-forest"
  ## copia = clone do repositorio (so o versionado) + golden REAL copiado (sem o modo somente-leitura, para poder apagar); sem store na copia
  cp <- file.path(withr::local_tempdir(pattern = "pcg-clone-"), "prevcarga")
  system2("git", c("clone", "-q", "--local", "--no-hardlinks", shQuote(s$root), shQuote(cp)), stdout = FALSE, stderr = FALSE)
  skip_if_not(dir.exists(cp), "clone falhou")
  dir.create(file.path(cp, ".state", "golden", "plugins"), recursive = TRUE)
  file.copy(file.path(s$root, ".state", "golden", "plugins", m), file.path(cp, ".state", "golden", "plugins"), recursive = TRUE,
            copy.mode = FALSE)
  lock <- file.path(cp, "models", m, "tests", "parity", "golden.lock")
  txt <- readLines(lock, warn = FALSE)
  i <- grep('"randomForest": "', txt, fixed = TRUE)
  expect_gt(length(i), 0L)
  txt[i] <- sub('"randomForest": "[^"]*"', '"randomForest": "0.0.0-adulterado"', txt[i])
  writeLines(txt, lock)
  ## a ferramenta de paridade recusa antes de rodar o clean-room e pede regerar o golden
  err <- tempfile("pcg-err-")
  st <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(file.path(s$root, "scripts", "tools", "plugin_parity.R")), "--model", m, "--units",
      shQuote(file.path(cp, "models", m, "tests", "parity", "units.yaml")), "--golden", shQuote(file.path(cp, ".state", "golden")),
      "--data", shQuote(file.path(s$root, ".state", "deck-store")), "--lock", shQuote(lock),
      "--rscript", shQuote(file.path(R.home("bin"), "Rscript"))), stdout = FALSE, stderr = err))
  e <- readLines(err, warn = FALSE)
  expect_false(identical(as.integer(st), 0L))
  expect_true(any(grepl("regere o golden", e, fixed = TRUE)), info = paste(e, collapse = "\n"))
  expect_true(any(grepl("tools.golden_stale", e, fixed = TRUE)))
  ## o runner do plugin na copia (sem store, e com o lock adulterado), no tier parity, reprova `paridade` e sai != 0 (a causa precisa e a da ferramenta, acima)
  r <- pcg_run_runner(file.path(cp, "models", m, "tests", "run_all_gates.R"), "parity")
  expect_false(identical(r$exit, 0L))
  expect_identical(r$table$status[r$table$id == "paridade"], "FAIL", info = paste(utils::tail(r$stdout, 15L), collapse = "\n"))
})

## ---- bloco 5: write-through verificavel -------------------------------------------------------------------------------
PCG_README_SECTIONS <- c(mapa = "^##\\s+.*[Mm]apa", contrato = "^##\\s+.*[Cc]ontrato", layout = "^##\\s+.*[Ll]ayout",
                         gates_dono = "^##\\s+.*[Gg]ate.* x dono")

pcg_check_readme <- function(path) {
  x <- readLines(path, encoding = "UTF-8", warn = FALSE)
  falta <- names(PCG_README_SECTIONS)[!vapply(PCG_README_SECTIONS, function(p) any(grepl(p, x)), NA)]
  list(falta = falta, requires_provides = any(grepl("requires", x, fixed = TRUE)) && any(grepl("provides", x, fixed = TRUE)))
}

pcg_check_estado <- function(path) {
  x <- readLines(path, encoding = "UTF-8", warn = FALSE)
  siglas <- c("RF", "LGBM", "HW", "LIN_MED", "LIN_MAX", "LIN_MXMN", "RGD_MED", "RGD_MAX")
  falta <- siglas[!vapply(siglas, function(s) any(grepl(sprintf("^\\|\\s*%s\\s*\\|", s), x)), NA)]
  linhas <- x[grepl("^\\|\\s*(RF|LGBM|HW|LIN_MED|LIN_MAX|LIN_MXMN|RGD_MED|RGD_MAX)\\s*\\|", x)]
  sha_ok <- length(linhas) == 8L && all(grepl("[0-9a-f]{64}", linhas))
  vered <- c(s3 = any(grepl("ADR-0019 s3 LGBM: (confirma|refuta)", x)), s4 = any(grepl("ADR-0019 s4 RF: (confirma|refuta)", x)))
  mods <- c("training.R", "inference.R", "features.R", "registry.R", "topology.R", "artifacts/", "train-models.R", "infer-model.R", "registry.R",
            "m5_facades.R", "features_facade.tsv", "registry_core.tsv", "training.tsv", "train_models.tsv", "inference.tsv",
            "infer_cut.tsv", "registry_lifecycle.tsv")
  list(falta = falta, sha_ok = sha_ok, vereditos = vered,
       tbd_0031 = any(grepl("[TBD: ADR-0031]", x, fixed = TRUE)), rng = any(grepl("RNG", x, fixed = TRUE)),
       modulos_falta = mods[!vapply(mods, function(m) any(grepl(m, x, fixed = TRUE)), NA)])
}

test_that("[ac-44] cada models/<m>/README.md tem mapa file:line do oraculo, contrato requires/provides, layout nativo e gate x dono", {
  s <- .pcg$get()
  for (m in pcc_all_plugins()) {
    p <- file.path(s$root, "models", m, "README.md")
    expect_true(file.exists(p), info = paste("README ausente:", m))
    chk <- pcg_check_readme(p)
    expect_identical(chk$falta, character(), info = paste(m, "sem secao:", paste(chk$falta, collapse = ", ")))
    expect_true(chk$requires_provides, info = paste(m, "sem requires/provides"))
  }
})

test_that("[ac-44] docs/estado/m5-training.md: 8 membros, [TBD: ADR-0031], RNG do DESSEM, modulos do M5 vivos e os 2 vereditos do spike", {
  s <- .pcg$get()
  p <- file.path(s$root, "docs", "estado", "m5-training.md")
  expect_true(file.exists(p))
  chk <- pcg_check_estado(p)
  expect_identical(chk$falta, character())
  expect_true(chk$sha_ok, info = "as 8 linhas da tabela precisam do sha256 do relatorio")
  expect_true(all(chk$vereditos))
  expect_true(chk$tbd_0031); expect_true(chk$rng)
  expect_identical(chk$modulos_falta, character())
  ## as 8 siglas da tabela == os membros da topologia (nunca lista fixa paralela)
  topo <- .pcc_members(s$root, file.path(s$root, "config", "topology.yaml"))
  expect_setequal(topo$sigla, c("RF", "LGBM", "HW", "LIN_MED", "LIN_MAX", "LIN_MXMN", "RGD_MED", "RGD_MAX"))
  ## os arquivos citados existem
  for (f in c("scripts/lib/training.R", "scripts/lib/inference.R", "scripts/lib/cli/train-models.R", "scripts/lib/cli/infer-model.R",
              "scripts/lib/cli/registry.R", "scripts/tools/gates/m5_facades.R"))
    expect_true(file.exists(file.path(s$root, f)), info = f)
  ## vereditos do estado == vereditos do journal (efemero; so confere se ainda existe)
  jr <- file.path(s$root, "docs", "journal", "spike-adr0019.md")
  if (file.exists(jr)) {
    j <- readLines(jr, warn = FALSE); e <- readLines(p, warn = FALSE, encoding = "UTF-8")
    for (k in c("ADR-0019 s3 LGBM: (confirma|refuta)", "ADR-0019 s4 RF: (confirma|refuta)"))
      expect_identical(regmatches(j, regexpr(k, j))[1L], regmatches(e, regexpr(k, e))[1L])
  }
})

test_that("[ac-44] ESTADO-ATUAL.md, PLANO.md e scripts/lib/README.md nao sao editados por este ticket (git diff --stat vazio)", {
  skip_on_os("windows")
  s <- .pcg$get()
  skip_if_not(dir.exists(file.path(s$root, ".git")) || file.exists(file.path(s$root, ".git")), "fora de um repositorio git")
  d <- suppressWarnings(system2("git", c("-C", shQuote(s$root), "diff", "--stat", "HEAD", "--", "docs/ESTADO-ATUAL.md",
                                         "docs/roadmap/PLANO.md", "scripts/lib/README.md"), stdout = TRUE, stderr = FALSE))
  expect_identical(as.character(d), character())
})

test_that("[CONTROL ac-44] estado sem a linha de um membro, sem um veredito ou README sem a secao gate x dono reprovam", {
  s <- .pcg$get()
  est <- readLines(file.path(s$root, "docs", "estado", "m5-training.md"), encoding = "UTF-8", warn = FALSE)
  w <- function(lines) { f <- withr::local_tempfile(fileext = ".md", .local_envir = parent.frame(2)); writeLines(lines, f, useBytes = TRUE); f }
  expect_identical(pcg_check_estado(w(est))$falta, character())                          # a copia integra passa (o controle tem dente)
  sem_membro <- est[!grepl("^\\|\\s*LIN_MXMN\\s*\\|", est)]
  expect_identical(pcg_check_estado(w(sem_membro))$falta, "LIN_MXMN")
  sem_s4 <- est[!grepl("ADR-0019 s4 RF:", est)]
  expect_false(pcg_check_estado(w(sem_s4))$vereditos[["s4"]])
  sem_tbd <- gsub("[TBD: ADR-0031]", "", est, fixed = TRUE)
  expect_false(pcg_check_estado(w(sem_tbd))$tbd_0031)
  rd <- readLines(file.path(s$root, "models", "random-forest", "README.md"), encoding = "UTF-8", warn = FALSE)
  expect_identical(pcg_check_readme(w(rd))$falta, character())
  expect_identical(pcg_check_readme(w(sub("^##\\s+Gates x dono", "## Outra coisa", rd)))$falta, "gates_dono")
})
