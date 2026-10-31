## test-additivity.R -- fechamento do plugin dessem-regdin (RD.7): aditividade, oraculo intacto, README e estado duravel.
## Gates [PLUGINS] aditividade_epico e readme. Sem variaveis de ambiente; clones e copias em tempdir.
## Aditividade = o epico so escreve em models/dessem-regdin/ (e nos itens sancionados abaixo): medida pelo git (commits "RD.n"
## + working tree), como LGBM.6/RF.5, porque o repo nao carrega o carimbo `.state/manifests` (gitignored). O manifesto sha256 de
## models/ (exceto o plugin e models/*/models/) e comparado entre duas arvores; o CONTROL altera um byte do motor numa copia.
suppressWarnings(suppressMessages(library(testthat)))

rda_oracle <- "/opt/ons-source/prevcargadessem3"
rda_model_dir <- function() file.path(rd_root(), "models", "dessem-regdin")

## itens fora de models/dessem-regdin/ que o epico sancionadamente tocou: catalogo de erros (barrel, ADR-0040), estado duravel e o
## gerador de golden (RD.6 acrescentou o campo `requires_report`, ver docs/estado/dessem_regdin.md)
RDA_SANCTIONED <- c("docs/estado/dessem_regdin.md", "scripts/lib/errors/dessem_regdin.tsv", "scripts/tools/plugin_golden.R")

rda_git <- function(args, dir = rd_root()) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), args), stdout = TRUE, stderr = FALSE))
  st <- attr(out, "status")
  list(out = out, status = if (is.null(st)) 0L else as.integer(st))
}

## manifesto "<caminho relativo a models/>\t<sha256>", ordenado (radix); exclui models/<exclude>/ e todo models/*/models/
rda_manifest <- function(root, exclude = "dessem-regdin") {
  mroot <- file.path(root, "models")
  files <- list.files(mroot, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!grepl("^[^/]+/models/", files)]
  files <- files[!startsWith(files, paste0(exclude, "/"))]
  files <- sort(files, method = "radix")
  paste(files, vapply(file.path(mroot, files), digest::digest, "", algo = "sha256", file = TRUE), sep = "\t")
}

## caminhos sob models/ e scripts/lib/ tocados pelos commits do epico (assunto com "RD.n", sem merges) e pelo working tree,
## fora de models/dessem-regdin/ e dos itens sancionados
rda_foreign_changes <- function(repo) {
  commits <- rda_git(c("log", "--no-merges", "--format=%H", "-E", shQuote("--grep=(^|[^A-Za-z])RD[.][0-9]")), repo)$out
  files <- unlist(lapply(commits, function(cm) rda_git(c("diff-tree", "--no-commit-id", "--name-only", "-r", cm), repo)$out))
  wt <- rda_git(c("status", "--porcelain", "--untracked-files=all", "--", "models", "scripts/lib"), repo)$out
  files <- unique(c(files, sub("^.{3}", "", wt)))
  files <- files[startsWith(files, "models/") | startsWith(files, "scripts/")]
  files <- files[!startsWith(files, "models/dessem-regdin/") & !(files %in% RDA_SANCTIONED)]
  sort(files, method = "radix")
}

## clone temporario com um arquivo plantado em `path` num commit "RD.9: ..." (ou solto)
rda_clone_with_plant <- function(path, commit = TRUE) {
  dir <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "clone")
  system2("git", c("clone", "-q", shQuote(rd_root()), shQuote(dir)), stdout = FALSE, stderr = FALSE)
  full <- file.path(dir, path)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  writeLines("# plantado", full)
  if (commit) {
    rda_git(c("add", "--", shQuote(path)), dir)
    system2("git", c("-C", shQuote(dir), "-c", "user.name=ctl", "-c", "user.email=ctl@example.invalid", "commit", "-q", "-m",
                     shQuote("RD.9: arquivo plantado (CONTROL)")), stdout = FALSE, stderr = FALSE)
  }
  dir
}

test_that("[PLUGINS] aditividade_epico: motor e outros plugins intocados (git + manifesto sha256), oraculo limpo, golden fora do git; CONTROL plantado", {
  skip_if(!nzchar(Sys.which("git")) || rda_git(c("rev-parse", "--git-dir"))$status != 0L, "[tier] git indisponivel")
  expect_identical(rda_foreign_changes(rd_root()), character())
  ## CONTROL: arquivo plantado em outro plugin (commit RD.9), no motor (solto) e em scripts/lib reprova nomeando-o
  c1 <- rda_clone_with_plant("models/holt-winters/plantado.R")
  expect_identical(rda_foreign_changes(c1), "models/holt-winters/plantado.R")
  c2 <- rda_clone_with_plant("models/_dessem-engine/solto.R", commit = FALSE)
  expect_identical(rda_foreign_changes(c2), "models/_dessem-engine/solto.R")
  c3 <- rda_clone_with_plant("scripts/lib/plantado.R")
  expect_identical(rda_foreign_changes(c3), "scripts/lib/plantado.R")
  ## um arquivo plantado DENTRO de models/dessem-regdin/ nao e estranho ao epico
  c4 <- rda_clone_with_plant("models/dessem-regdin/novo.R")
  expect_identical(rda_foreign_changes(c4), character())

  ## manifesto sha256 de models/ (exceto o plugin): a copia da arvore atual e identica; um byte no motor e acusado com o caminho
  now <- rda_manifest(rd_root())
  expect_gt(length(now), 0L)
  expect_true(any(startsWith(now, "_dessem-engine/R/")))
  expect_false(any(startsWith(now, "dessem-regdin/")))
  tmp <- file.path(withr::local_tempdir(), "copia")
  dir.create(file.path(tmp, "models"), recursive = TRUE)
  file.copy(list.files(file.path(rd_root(), "models"), full.names = TRUE), file.path(tmp, "models"), recursive = TRUE)
  expect_identical(rda_manifest(tmp), now)
  alvo <- sort(list.files(file.path(tmp, "models", "_dessem-engine", "R"), pattern = "[.]R$", full.names = TRUE), method = "radix")[[1L]]
  cat("#", file = alvo, append = TRUE)
  diff <- setdiff(rda_manifest(tmp), now)
  expect_length(diff, 1L)
  expect_true(startsWith(diff, paste0("_dessem-engine/R/", basename(alvo), "\t")))
  ## o plugin fica fora do manifesto: editar o proprio dessem-regdin nao o altera
  writeLines("# x", file.path(tmp, "models", "dessem-regdin", "extra.R"))
  expect_identical(length(setdiff(rda_manifest(tmp), now)), 1L)

  ## golden e artefatos nunca versionados
  expect_identical(rda_git(c("check-ignore", "-q", ".state/golden/x"))$status, 0L)
  expect_length(rda_git(c("ls-files", ".state", "models/dessem-regdin/models"))$out, 0L)

  ## oraculo somente leitura: so as 3 notas + verify-log/ de docs/roadmap/ (ja existiam la, nao sao deste epico) sao toleraveis
  if (!dir.exists(file.path(rda_oracle, ".git"))) skip("[tier] oraculo ausente nesta maquina")
  st <- rda_git(c("status", "--porcelain"), rda_oracle)$out
  tolerado <- paste("??", c("docs/roadmap/CLI-MODULAR-OBJETIVO.md", "docs/roadmap/CLI-MODULAR-ONDE-ESTAMOS.md",
                            "docs/roadmap/MIGRACAO-COMBINA-R.md", "docs/roadmap/verify-log/"))
  expect_length(setdiff(st, tolerado), 0L)
  ## CONTROL do criterio: um arquivo a mais no oraculo seria acusado
  expect_gt(length(setdiff(c(st, "?? plantado.R"), tolerado)), 0L)
})

RDA_README_SECTIONS <- c("Mapa funcao -> arquivo:linha do oraculo", "Contrato do descriptor", "Layout `dessem_matrix_nx48` (`dessem_semihourly_48`)",
                         "Faixa O..O+8", "Invariante de janela (`history_days.infer == train`, tres camadas)", "RNG preservado e plano B",
                         "Tabela gate x dono", "Comandos", "Desvios conscientes do oraculo (para o A/B do M8)")

test_that("[PLUGINS] readme: secoes, contrato, desvios do A/B, toda linha do runner, mapa do oraculo e estado duravel", {
  rd <- readLines(file.path(rda_model_dir(), "README.md"), encoding = "UTF-8")
  txt <- paste(rd, collapse = "\n")
  heads <- sub("^##+ +", "", grep("^##+ ", rd, value = TRUE))
  for (s in RDA_README_SECTIONS) expect_true(s %in% heads, info = s)
  expect_true(all(utf8ToInt(txt) < 128L))
  for (needle in c("origin_scoped", "granularity_min: 60", "horizon_days: 9", "delivery_days: 9", "model_time_offset_min: 60",
                   "dessem_matrix_nx48", "O..O+8", "_load_engine.R:300-370", "dessem_regdin_fit.R", "dessem_regdin_predict.R", "data_adapter.R",
                   ".rd_check_window", ".rd_check_fingerprint", "rng_deviation", "MXMN", "available_until", "O+9",
                   "run_all_gates.R --tier fast", "--tier parity-full", "docs/estado/dessem_regdin.md"))
    expect_true(grepl(needle, txt, fixed = TRUE), info = needle)
  ## desvios conscientes: lista numerada 1-6 dentro da secao
  i0 <- grep("^## Desvios conscientes", rd); i1 <- grep("^## ", rd); i1 <- c(i1[i1 > i0], length(rd) + 1L)[[1L]]
  sec <- rd[(i0 + 1L):(i1 - 1L)]
  expect_identical(sub("^([0-9]+)[.] .*$", "\\1", grep("^[0-9]+[.] ", sec, value = TRUE)), as.character(1:6))
  ## toda linha do runner (gate) aparece no README (tabela gate x dono ou nos comandos): os nomes sao lidos do proprio runner
  env <- new.env()
  for (e in parse(file.path(rda_model_dir(), "tests", "run_all_gates.R"), keep.source = FALSE))
    if (is.call(e) && identical(as.character(e[[1L]]), "<-") && identical(as.character(e[[2L]])[[1L]], ".rd_gates")) eval(e, env)
  gates <- vapply(env$.rd_gates, `[[`, "", 1L)
  expect_gt(length(gates), 30L)
  for (g in gates) expect_true(grepl(sprintf("`%s`|`%s_", g, sub("_[a-z]+$", "", g)), txt) || grepl(g, txt, fixed = TRUE), info = g)
  ## mapa: cada arquivo do oraculo citado existe e a linha inicial de cada faixa cabe no arquivo
  if (dir.exists(file.path(rda_oracle, "models", "dessem-regdin"))) {
    refs <- regmatches(txt, gregexpr("(dessem_regdin_fit|dessem_regdin_predict|data_adapter|_load_engine)[.]R:[0-9]+", txt))[[1L]]
    expect_gt(length(refs), 4L)
    for (r in unique(refs)) {
      f <- sub(":.*$", "", r); ln <- as.integer(sub("^.*:", "", r))
      p <- Sys.glob(file.path(rda_oracle, "models", "dessem-regdin", c("R/*/", "vendor/"), f))
      expect_length(p, 1L)
      expect_lte(ln, length(readLines(p[[1L]], warn = FALSE)))
    }
  }
  ## o estado duravel do plugin existe, o README aponta para ele e o registra (membros vivos, invariante, RNG, desvios, hash do relatorio)
  est <- file.path(rd_root(), "docs", "estado", "dessem_regdin.md")
  expect_true(file.exists(est))
  et <- paste(readLines(est, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("## RD.7", et, fixed = TRUE))
  for (needle in c("RGD_MED", "RGD_MAX", "history_days.infer == train", "rng_deviation", "MXMN", "a7b9c157e7d5c3130e49c4879c15e63d6e7cb7570baa85494ebb86c4fdeb1eb3"))
    expect_true(grepl(needle, et, fixed = TRUE), info = needle)
  ## CONTROL: README sem uma secao obrigatoria e acusado
  expect_false(all(RDA_README_SECTIONS %in% sub("^##+ +", "", grep("^##+ ", rd[-grep("^## Tabela gate x dono", rd)], value = TRUE))))
})
