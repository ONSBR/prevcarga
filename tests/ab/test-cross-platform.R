## test-cross-platform.R -- Windows x Linux e `--jobs 4` == `--jobs 1` no bundle INSTALADO (M8.9, AC 3; ADR-0036, ADR-0038).
## Roda pelo helper dos e2e do instalador (tests/install/helper-*.R) e pelo LANCADOR da plataforma, nunca pela arvore de dev.
##
## Bracos:
##  [LINUX] `--jobs 4` == `--jobs 1` byte a byte (PSOCK, M7.6). O PSOCK existe no `train-models` (o `forecast` nao tem --jobs: nao ha
##          paralelismo na rodada diaria; discovery M8.9): treina as mesmas celulas com 1 e com 4 workers em dois estados vazios e exige
##          arvores de artefatos identicas. CONTROL: outra semente de dados => arvore diferente (o teste tem dentes).
##  [LINUX] publica a saida de REFERENCIA (entrega do forecast) no diretorio compartilhado `getOption("prevcarga.ab.shared")`, de onde o
##          braco Windows a importa.
##  [WINDOWS] DEFERIDO ao gate windows-vm do M8 (dono: Gabriel, decisao de 2026-10-31): nao ha host Windows nativo nem pwsh nesta maquina.
##          O corpo do braco esta escrito (processx + prevcarga.cmd) mas NUNCA foi executado; aqui vira SKIP explicito com dono e motivo.
##          Ao rodar no Windows, com `options(prevcarga.ab.shared = <dir>)` apontando a saida Linux e `options(prevcarga.ab.bundles =
##          c(linux = <tar.zst>, windows = <zip>))`: `forecast`/`train-models` com --jobs 1 e 4 (Delta = 0), `verificar --trilho
##          entre-plataformas` da saida Windows contra a Linux (PASS, |d| <= 0,001*|ref|), CONTROL +0,2% num y_pred com manifest regerado
##          (exit 12) e, ao passar, o recibo `cross-platform` PASS para o sha dos DOIS bundles (pc_receipt_write, scripts/tools/receipts.R).
library(testthat)

ab_root <- local({
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "lib", "runtime.R"))) { up <- dirname(d); stopifnot(!identical(up, d)); d <- up }
  d
})
for (h in c("helper-install.R", "helper-sync.R", "helper-e2e.R")) source(file.path(ab_root, "tests", "install", h), local = FALSE)

SHARED <- getOption("prevcarga.ab.shared")
WIN <- identical(.Platform$OS.type, "windows")

## --- LINUX -----------------------------------------------------------------------------------------------------------------------
test_that("[LINUX] --jobs 4 == --jobs 1 byte a byte (PSOCK): as mesmas celulas treinadas com 1 e com 4 workers dao arvores identicas", {
  pci_skip_unless_ready()
  W <- pc_e2e_world()
  base <- pci_work_dir()                                      # no frame do teste: os estados vivem ate o fim dele
  train <- function(jobs, deck = W$deck, tag = jobs) {
    st <- file.path(base, paste0("state-", tag)); dir.create(st)
    r <- pce_ok(pc_e2e_cli(W$root, pce_args("train-models", deck, st, "--origin", W$origin, "--model=random-forest",
                                            paste0("--areas=", paste(PCE_LEAVES[1:2], collapse = ",")), "--skip-validate",
                                            paste0("--jobs=", jobs)), timeout = 3600), paste("train-models --jobs", jobs))
    list(state = st, cells = vapply(r$json$result$cells, function(c) paste(c$area, c$version), ""))
  }
  ## `run_id` (carimbo da invocacao da CLI) e o unico campo do run_manifest.json que difere entre duas invocacoes; ele e o relogio da rodada e
  ## entra no SHA256SUMS do manifest. O resto da arvore (artifact.rds, params, fingerprint, versoes) e o conteudo treinado.
  art <- function(state) {
    d <- file.path(state, "artifacts")
    rel <- sort(list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")
    pay <- setdiff(rel, rel[basename(rel) %in% c("run_manifest.json", "SHA256SUMS")])
    mf <- lapply(rel[basename(rel) == "run_manifest.json"], function(p) { m <- jsonlite::read_json(file.path(d, p)); m$run_id <- NULL; m })
    list(files = rel, payload = vapply(pay, function(p) digest::digest(file = file.path(d, p), algo = "sha256"), ""), manifests = mf)
  }
  j1 <- train(1L); j4 <- train(4L)
  expect_length(j1$cells, 2L)
  expect_gte(length(list.files(file.path(j1$state, "artifacts"), recursive = TRUE)), 2L)   # a arvore comparada nao esta vazia
  expect_identical(j4$cells, j1$cells)                                                      # mesmas versoes (v_<hash> do conteudo)
  expect_identical(art(j4$state), art(j1$state))                                            # FORBIDDEN: Delta = 0 (conteudo treinado e manifests sem o run_id)
  ## CONTROL: um deck com outra semente treina outro conteudo => a arvore muda (a comparacao acima acusa diferenca real)
  other <- pc_e2e_deck(file.path(base, "deck-seed"), W$origin, seed = 12)
  jo <- train(1L, other, tag = "seed")
  expect_false(identical(art(jo$state)$payload, art(j1$state)$payload))
})

test_that("[LINUX] a saida de referencia do forecast e publicada no diretorio compartilhado para o braco Windows", {
  pci_skip_unless_ready()
  skip_if(is.null(SHARED), "sem options(prevcarga.ab.shared): nada a publicar para o braco Windows (nao e erro)")
  W <- pc_e2e_world()
  r <- pc_e2e_forecast(W, W$deck)
  expect_identical(r$cli$status, 0L)
  dst <- file.path(SHARED, "linux-x86_64"); unlink(dst, recursive = TRUE); dir.create(dst, recursive = TRUE)
  expect_true(all(file.copy(list.files(r$saida, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)))
  expect_identical(pc_tree_sha(dst), pc_tree_sha(r$saida))
})

## --- WINDOWS (DEFERIDO) ------------------------------------------------------------------------------------------------------------
test_that("[WINDOWS] --jobs 4 == --jobs 1 e verificar --trilho entre-plataformas <= 0,1% contra a saida Linux; CONTROL +0,2% => exit 12; recibo cross-platform", {
  if (!WIN)
    skip("DEFERIDO ao gate windows-vm do M8 (dono: Gabriel, 2026-10-31): sem host Windows nativo nem pwsh nesta maquina; a perna Windows x Linux nao foi executada nem simulada")
  ref <- if (is.null(SHARED)) NA_character_ else file.path(SHARED, "linux-x86_64")
  skip_if(is.na(ref) || !dir.exists(ref), "saida Linux de referencia ausente (options(prevcarga.ab.shared) sem linux-x86_64/)")
  bundles <- getOption("prevcarga.ab.bundles")
  skip_if(is.null(bundles) || !all(c("linux", "windows") %in% names(bundles)), "options(prevcarga.ab.bundles = c(linux=, windows=)) ausente: o recibo precisa do sha dos dois bundles")
  root <- getOption("prevcarga.ab.windows_root")
  skip_if(is.null(root) || !dir.exists(root), "options(prevcarga.ab.windows_root) = raiz do bundle Windows INSTALADO ausente")
  deck <- getOption("prevcarga.ab.deck"); state <- getOption("prevcarga.ab.state")
  skip_if(is.null(deck) || is.null(state), "options(prevcarga.ab.deck / prevcarga.ab.state) ausentes: mesmo deck e artefatos sincronizados da perna Linux")
  O <- jsonlite::read_json(file.path(ref, "run_manifest.json"))$origin
  run <- function(jobs) {
    st <- file.path(tempfile("pc-ab-win-"), "state"); dir.create(dirname(st)); file.copy(list.files(state, full.names = TRUE), st, recursive = TRUE)
    r <- pc_e2e_cli(root, c("forecast", paste0("--data=", deck), paste0("--state=", st), "--origin", O, "--skip-validate"))
    expect_identical(r$status, 0L)
    r$json$result$saida
  }
  s1 <- run(1L); s4 <- run(4L)                     # o forecast nao tem --jobs: duas rodadas independentes iguais; o --jobs vale no train-models (braco Linux)
  jobs_equal <- identical(pc_tree_sha(s4), pc_tree_sha(s1))
  expect_true(jobs_equal)
  v <- pc_e2e_cli(root, c("verificar", paste0("--saida=", s1), paste0("--referencia=", ref), "--trilho=entre-plataformas"))
  pass <- identical(v$status, 0L)
  expect_true(pass)
  ## CONTROL: +0,2% num y_pred do lado Windows, manifest regerado (a integridade passa; o valor fica fora da tolerancia) => exit 12
  bad <- file.path(tempfile("pc-ab-bad-"), "entrega"); dir.create(dirname(bad)); dir.create(bad)
  file.copy(list.files(s1, full.names = TRUE, all.files = TRUE, no.. = TRUE), bad, recursive = TRUE)
  p <- file.path(bad, "SIN.parquet"); x <- arrow::read_parquet(p); x$y_pred[[1L]] <- x$y_pred[[1L]] * 1.002; arrow::write_parquet(x, p)
  mf <- jsonlite::read_json(file.path(bad, "manifest.json"))
  for (i in seq_along(mf$arquivos)) if (identical(mf$arquivos[[i]]$path, "SIN.parquet")) mf$arquivos[[i]]$sha256 <- digest::digest(file = p, algo = "sha256")
  jsonlite::write_json(mf, file.path(bad, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  e <- pc_e2e_cli(root, c("verificar", paste0("--saida=", bad), paste0("--referencia=", ref), "--trilho=entre-plataformas"))
  control <- identical(e$status, 12L)
  expect_true(control)
  ## recibo para o sha dos DOIS bundles (so se tudo acima passou)
  skip_if(!(jobs_equal && pass && control), "recibo nao gravado: algum braco acima falhou")
  source(file.path(ab_root, "scripts", "tools", "receipts.R"), local = TRUE)
  for (p in c(linux = "linux-x86_64", windows = "windows-x86_64")) {
    f <- pc_receipt_write("cross-platform", bundles[[sub("-.*", "", p)]], p, "PASS", dir = file.path(ab_root, "tests", ".receipts"))
    r <- jsonlite::read_json(f)                      # campos do recibo cross-platform alem dos comuns
    r$referencia_platform <- if (identical(p, "windows-x86_64")) "linux-x86_64" else "windows-x86_64"
    r$jobs_equal <- jobs_equal
    jsonlite::write_json(r, f, auto_unbox = TRUE, null = "null", pretty = TRUE)
  }
})

