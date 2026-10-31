## test-nfr-30min.R -- NFR de 30 min: `prevcarga forecast --origin <O> --format json` <= 1800 s nos DOIS sistemas (M8.10, AC 22 do epico M8).
## Roda pelo LANCADOR do bundle INSTALADO (helpers de tests/install/), nunca pela arvore de dev. O tempo e o REGISTRADO pela rodada
## (`result$duracao_s` e `result$passos[]`: validate-deck, infer, combina, reconcile, export, report); o teste nunca cronometra.
## Os membros origin_scoped treinam dentro da rodada (1 re-treino/dia ate O-1, chave = origem, ADR-0023): o tempo cai no passo `infer`.
## `score` fica fora (a rodada nao o executa). O tempo nao entra na entrega nem no run_manifest.json (entrega byte-deterministica).
##
## PROTOCOLO DE MEDICAO REEXECUTAVEL (host de referencia e VM Windows; o mesmo arquivo roda nos dois):
##   1. instalar o bundle da plataforma (install.sh / install.ps1) numa raiz R e sincronizar os artefatos OFICIAIS com `prevcarga-sync`
##      (modelos origin_agnostic, pesos da combina, k-hat `latest`) para o estado local <S>;
##   2. deck REAL de um dia util (com manifest.json) em <D>, origem <O> (o forecast e sempre de UMA origem); topologia cheia (a oficial do bundle);
##   3. `options(prevcarga.nfr.root = <R>, prevcarga.nfr.deck = <D>, prevcarga.nfr.state = <S>, prevcarga.nfr.origin = "<O>",
##               prevcarga.nfr.bundle = <tarball/zip do bundle>, prevcarga.nfr.jobs = <n>)`; depois
##      `Rscript --vanilla -e 'testthat::test_dir("tests/ab", filter = "nfr-30min", stop_on_failure = TRUE)'`;
##   4. o estado <S> e copiado para um diretorio novo (o export e imutavel); `--jobs`: o `forecast` nao tem --jobs (nao ha paralelismo na
##      rodada diaria; M8.9); o valor registrado e o `jobs` da rodada (1) e `prevcarga.nfr.jobs` so documenta o `train-models` do estado;
##   5. PASS grava o recibo `nfr-30min` (tests/.receipts/<sha>.nfr-30min.json; scripts/tools/receipts.R) com duracao_s, passos, runtime_id e jobs,
##      consumido pelo ab_report.R. Recibo FAIL se o orcamento estoura. Sem deck real: SKIP `deck-real-ausente` (nunca verde silencioso).
##   6. tempo do ORACULO na mesma maquina (prevcargadessem3, somente leitura) = `scripts/tools/bench_forecast.R --reference-seconds`; ver
##      docs/estado/nfr-performance.md. O numero real e do operador: nada e inventado aqui.
library(testthat)

nfr_root <- local({
  d <- normalizePath(testthat::test_path(), winslash = "/", mustWork = TRUE)
  while (!file.exists(file.path(d, "scripts", "lib", "runtime.R"))) { up <- dirname(d); stopifnot(!identical(up, d)); d <- up }
  d
})
for (h in c("helper-install.R", "helper-sync.R", "helper-e2e.R")) source(file.path(nfr_root, "tests", "install", h), local = FALSE)
if (!exists("pc_nfr_budget")) source(file.path(nfr_root, "tests", "ab", "helper-nfr.R"), local = FALSE)
if (!exists("pc_receipt_write")) source(file.path(nfr_root, "scripts", "tools", "receipts.R"), local = FALSE)

nfr_opt <- function(k) { v <- getOption(paste0("prevcarga.nfr.", k)); if (is.null(v)) "" else as.character(v) }
nfr_doc <- function(duracoes, platform = "linux-x86_64") {
  list(duracao_s = sum(duracoes), jobs = 1L, platform = platform, runtime_id = "R4.5.3-x86_64-pc-linux-gnu",
       passos = Map(function(p, d) list(passo = p, duracao_s = d), c("validate-deck", "infer", "combina", "reconcile", "export", "report")[seq_along(duracoes)], duracoes))
}

## --- o gate de orcamento (mecanica; roda em qualquer host) -------------------------------------------------------------
test_that("[AC3] pc_nfr_budget: aprova dentro do orcamento, reprova fora e aponta o passo mais lento e a plataforma", {
  d <- nfr_doc(c(12.1, 1105.7, 41, 9.8, 3.2, 140.6))
  ok <- pc_nfr_budget(d, 1800)
  expect_true(ok$ok); expect_identical(ok$mais_lento, "infer"); expect_identical(ok$platform, "linux-x86_64")
  bad <- pc_nfr_budget(d, 1000)
  expect_false(bad$ok)
  expect_match(pc_nfr_verdict(bad), "linux-x86_64.*>.*infer")
  ## le o JSON do envelope (result aninhado) e do proprio result
  f <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(ok = TRUE, result = d), f, auto_unbox = TRUE)
  expect_identical(pc_nfr_budget(f)$duracao_s, ok$duracao_s)
})

test_that("[AC1] a soma dos passos tem de bater com duracao_s (tolerancia 5 s); CONTROL: 6 s de diferenca e erro", {
  d <- nfr_doc(c(1, 2, 3, 4, 5, 6))
  d$duracao_s <- 21 + 4.9
  expect_silent(pc_nfr_budget(d))
  d$duracao_s <- 21 + 6
  expect_error(pc_nfr_budget(d))
  d$duracao_s <- NULL
  expect_error(pc_nfr_budget(d))
})

test_that("[AC3] CONTROL: rodada com passo injetado de 3 s reprova o orcamento de 2 s apontando `infer`; com 1800 s passa", {
  run <- function(sleep_s) nfr_doc(c(0.1, sleep_s, 0.1, 0.1, 0.1))     # forma do result da rodada real (ver scripts/lib/tests/test-forecast_timing.R)
  r <- pc_nfr_injected_round(run, sleep_s = 3)
  tight <- pc_nfr_budget(r, budget_s = 2)
  expect_false(tight$ok); expect_identical(tight$mais_lento, "infer"); expect_gt(tight$duracao_s, 2)
  expect_true(pc_nfr_budget(r, budget_s = 1800)$ok)
})

## --- a rodada real pelo bundle instalado ---------------------------------------------------------------------------------
test_that("[AC1][AC4] forecast <= 1800 s pelo bundle instalado (deck real, topologia cheia, artefatos oficiais sincronizados)", {
  skip_if(!nzchar(nfr_opt("deck")), "deck-real-ausente")
  skip_if(!nzchar(nfr_opt("root")) || !nzchar(nfr_opt("state")) || !nzchar(nfr_opt("origin")) || !nzchar(nfr_opt("bundle")),
          "instalacao-ausente: definir prevcarga.nfr.root/state/origin/bundle (protocolo no cabecalho)")
  st <- pc_e2e_state_copy(nfr_opt("state"))
  r <- pc_e2e_cli(nfr_opt("root"), pce_args("forecast", nfr_opt("deck"), st, "--origin", nfr_opt("origin")), timeout = 7200)
  expect_identical(r$status, 0L)
  g <- pc_nfr_budget(r$json$result, budget_s = 1800)
  expect_true(g$ok, info = pc_nfr_verdict(g))
  expect_true("infer" %in% g$passos$passo)
  expect_false("score" %in% g$passos$passo)
  pc_receipt_write("nfr-30min", nfr_opt("bundle"), g$platform, resultado = if (g$ok) "PASS" else "FAIL", runtime_id = g$runtime_id)
})

test_that("[AC1] mecanica do gate pelo bundle instalado no deck sintetico (nenhum numero de producao)", {
  pci_skip_unless_ready()
  W <- pc_e2e_world()
  f <- pc_e2e_forecast(W, W$deck)
  g <- pc_nfr_budget(f$cli$json$result, budget_s = 1800)
  expect_true(g$ok, info = pc_nfr_verdict(g))
  expect_identical(g$passos$passo[1:2], c("validate-deck", "infer"))
  expect_false("score" %in% g$passos$passo)
  expect_false(pc_nfr_budget(f$cli$json$result, budget_s = 1e-6)$ok)    # o orcamento reduzido reprova
  expect_false(file.exists(file.path(f$saida, "duracao.json")))
  expect_false(any(grepl("duracao_s", readLines(file.path(f$saida, "run_manifest.json")))))   # o tempo nao entra na entrega
})

## --- Windows ----------------------------------------------------------------------------------------------------------------
test_that("[AC2] VM Windows: forecast pelo prevcarga.cmd <= 1800 s e recibo nfr-30min da plataforma", {
  skip("[DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] nao ha host Windows nativo nem pwsh nesta maquina; o mesmo arquivo roda na VM (protocolo no cabecalho) e grava o recibo windows-x86_64")
})
