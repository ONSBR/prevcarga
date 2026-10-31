## test-additivity.R -- fechamento do plugin dessem-linear (DL.11): aditividade do epico em models/ e oraculo intacto.
## Gate [PLUGINS] aditividade_epico. Sem variaveis de ambiente; clones em tempdir.
## Aditividade = o epico so escreve em models/dessem-linear/ e models/_dessem-engine/ (o motor unico e do epico): medida pelo git
## (commits "DL.n" sem merges + working tree) e pelo manifesto de blobs de models/ exceto os dois, comparado ao da referencia.
## Referencia: o repo nao tem `origin/main` (nem o carimbo `.state/manifests`, gitignorado) e a `main` e espelho atrasado; por isso a
## base e `HEAD` (blobs commitados contra o working tree: pega edicao nao commitada) e a varredura dos commits "DL.n" pega o historico
## (um epico irmao legitimamente altera o PROPRIO plugin, entao comparar com a `main` acusaria o que nao e deste epico).
suppressWarnings(suppressMessages(library(testthat)))

dla_oracle <- "/opt/ons-source/prevcargadessem3"
DLA_OWN <- "^models/(dessem-linear|_dessem-engine)/"

dla_git <- function(args, dir = dl_root()) {
  out <- suppressWarnings(system2("git", c("-C", shQuote(dir), args), stdout = TRUE, stderr = FALSE))
  st <- attr(out, "status")
  list(out = out, status = if (is.null(st)) 0L else as.integer(st))
}

## data.table path | base_blob (ref) | work_blob (working tree) dos arquivos de models/ fora do plugin e do motor
dl_models_manifest <- function(ref = "HEAD", root = dl_root()) {
  ls <- dla_git(c("ls-tree", "-r", "--name-only", ref, "models/"), root)$out
  ls <- ls[!grepl(DLA_OWN, ls)]
  ls <- sort(ls, method = "radix")
  base <- vapply(ls, function(p) dla_git(c("rev-parse", shQuote(paste0(ref, ":", p))), root)$out[[1L]], "")
  work <- vapply(ls, function(p) {
    f <- file.path(root, p)
    if (!file.exists(f)) NA_character_ else dla_git(c("hash-object", "--", shQuote(f)), root)$out[[1L]]
  }, "")
  data.table::data.table(path = ls, base_blob = unname(base), work_blob = unname(work))
}

## arquivos de models/ fora do plugin e do motor tocados pelos commits "DL.n" (sem merges) ou pelo working tree
dla_foreign_models <- function(repo = dl_root()) {
  commits <- dla_git(c("log", "--no-merges", "--format=%H", "-E", shQuote("--grep=(^|[^A-Za-z])DL[.][0-9]")), repo)$out
  files <- unlist(lapply(commits, function(cm) dla_git(c("diff-tree", "--no-commit-id", "--name-only", "-r", cm), repo)$out))
  wt <- dla_git(c("status", "--porcelain", "--untracked-files=all", "--", "models"), repo)$out
  files <- unique(c(files, sub("^.{3}", "", wt)))
  files <- files[startsWith(files, "models/") & !grepl(DLA_OWN, files)]
  sort(files, method = "radix")
}

## clone temporario com `path` plantado (commit "DL.99: ..." ou solto)
dla_clone_with_plant <- function(path, commit = TRUE, modify = FALSE) {
  dir <- file.path(withr::local_tempdir(.local_envir = parent.frame()), "clone")
  system2("git", c("clone", "-q", shQuote(dl_root()), shQuote(dir)), stdout = FALSE, stderr = FALSE)
  full <- file.path(dir, path)
  dir.create(dirname(full), recursive = TRUE, showWarnings = FALSE)
  cat("# plantado\n", file = full, append = modify)
  if (commit) {
    dla_git(c("add", "--", shQuote(path)), dir)
    system2("git", c("-C", shQuote(dir), "-c", "user.name=ctl", "-c", "user.email=ctl@example.invalid", "commit", "-q", "-m",
                     shQuote("DL.99: arquivo plantado (CONTROL)")), stdout = FALSE, stderr = FALSE)
  }
  dir
}

test_that("[PLUGINS] aditividade_epico: models/ fora de dessem-linear e _dessem-engine intacto (git + blobs), golden fora do git, oraculo limpo; CONTROL plantado", {
  skip_if(!nzchar(Sys.which("git")) || dla_git(c("rev-parse", "--git-dir"))$status != 0L, "[tier] git indisponivel")
  ## historico e working tree: nenhum arquivo de outro plugin tocado pelo epico
  expect_identical(dla_foreign_models(), character())
  ## manifesto de blobs: commitado == working tree para todo models/ fora do plugin e do motor
  m <- dl_models_manifest("HEAD")
  expect_gt(nrow(m), 50L)
  expect_true(any(startsWith(m$path, "models/holt-winters/")))
  expect_false(any(grepl(DLA_OWN, m$path)))
  expect_identical(m[is.na(work_blob) | base_blob != work_blob, path], character(0))

  ## CONTROL: arquivo-fixture alterado em models/holt-winters/ num clone temporario e apontado pelo caminho (manifesto e historico)
  alvo <- sort(grep("^models/holt-winters/.*[.]R$", m$path, value = TRUE), method = "radix")[[1L]]
  c1 <- dla_clone_with_plant(alvo, commit = FALSE, modify = TRUE)
  m1 <- dl_models_manifest("HEAD", c1)
  expect_identical(m1[is.na(work_blob) | base_blob != work_blob, path], alvo)
  expect_identical(dla_foreign_models(c1), alvo)
  c2 <- dla_clone_with_plant("models/holt-winters/plantado.R")
  expect_identical(dla_foreign_models(c2), "models/holt-winters/plantado.R")
  ## o plugin e o motor sao do epico: um arquivo plantado neles nao e estranho
  c3 <- dla_clone_with_plant("models/dessem-linear/novo.R")
  expect_identical(dla_foreign_models(c3), character())
  c4 <- dla_clone_with_plant("models/_dessem-engine/solto.R", commit = FALSE)
  expect_identical(dla_foreign_models(c4), character())

  ## golden e artefatos nunca versionados
  expect_identical(dla_git(c("check-ignore", "-q", ".state/golden/x"))$status, 0L)
  expect_length(dla_git(c("ls-files", ".state", "models/dessem-linear/models"))$out, 0L)

  ## oraculo somente leitura: `git status --porcelain` so com as 3 notas + verify-log/ de docs/roadmap/ (preexistentes, nao deste epico)
  if (!dir.exists(file.path(dla_oracle, ".git"))) skip("[tier] oraculo ausente nesta maquina")
  st <- dla_git(c("status", "--porcelain"), dla_oracle)$out
  tolerado <- paste("??", c("docs/roadmap/CLI-MODULAR-OBJETIVO.md", "docs/roadmap/CLI-MODULAR-ONDE-ESTAMOS.md",
                            "docs/roadmap/MIGRACAO-COMBINA-R.md", "docs/roadmap/verify-log/"))
  expect_length(setdiff(st, tolerado), 0L)
  ## CONTROL do criterio: um arquivo a mais no oraculo seria acusado
  expect_gt(length(setdiff(c(st, "?? plantado.R"), tolerado)), 0L)
})
