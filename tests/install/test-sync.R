## test-sync.R -- install/prevcarga-sync.sh (M8.6; ADR-0039, ADR-0040). So Linux (sh + curl); skip explicito no Windows, onde o par e o
## prevcarga-sync.ps1 (Pester em install/tests/prevcarga-sync.Tests.ps1). NUNCA toca bucket real: o "bucket" e um diretorio montado pelo
## release-pack real e servido por um servidor HTTP local de teste (helper-sync.R). Cada braco FORBIDDEN (tamper, corte de rede, local
## divergente, entrada invalida) tem o seu CONTROL (objetos integros => arvore == bucket arquivo a arquivo).
## Tudo em sandbox/ (gitignored), sob `env -i`: nenhuma variavel de ambiente, nenhum perfil, nenhuma credencial.

pcs_skip <- function() {
  testthat::skip_on_os(c("windows", "mac"))
  testthat::skip_if(!nzchar(Sys.which("curl")), "curl ausente no host")
  testthat::skip_if(!requireNamespace("httpuv", quietly = TRUE) || !requireNamespace("callr", quietly = TRUE), "httpuv/callr ausentes")
}

## bucket montado UMA vez por arquivo (read-only para os testes)
BUCKET <- local({ b <- NULL; function() { if (is.null(b)) b <<- pc_fixture_bucket(env = globalenv()); b } })

run_sync <- function(sh, url, state, ..., what = "artifacts", json = TRUE)
  pc_sync_run(sh, c("--what", what, "--release-base", url, "--state", state, ..., if (json) c("--format", "json")))
pj <- function(r) jsonlite::fromJSON(r$stdout, simplifyVector = FALSE)
art_listing <- function(dir) pc_listing(file.path(dir, "artifacts"))

test_that("[CONTROL] objetos integros: exit 0 e <state>/artifacts == bucket arquivo a arquivo (inclusive combina/ e reconcile/)", {
  pcs_skip()
  b <- BUCKET(); sv <- pc_serve_bucket(b$dir); sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  r <- run_sync(sh, sv$url, S)
  expect_identical(r$status, 0L); expect_identical(r$stderr, "")
  j <- pj(r)
  expect_true(j$ok); expect_identical(j$verb, "prevcarga-sync"); expect_identical(j$result$what, "artifacts")
  expect_identical(art_listing(S), pc_listing(file.path(b$dir, "artifacts")))
  cells <- unique(sub("/(v_.*|latest\\.txt)$", "", sub(" .*", "", art_listing(S))))
  expect_true(all(c("combina/A1", "combina/B1", "reconcile/SS1", "reconcile/SS2", "lgbm/exo1/A1", "random-forest/A1", "holt-winters/A1") %in% cells))
  expect_length(j$result$baixados, length(PCS_CELLS))
  expect_length(j$result$pulados, 0L)
  expect_true("artifacts/lgbm/A2" %in% unlist(j$result$ausentes))      # prefixo do conjunto oficial nao publicado: 404 => ausente, nao erro
  expect_length(pc_tmp_left(file.path(S, "artifacts")), 0L)
  for (lt in Sys.glob(file.path(S, "artifacts", "*", "*", "latest.txt")))   # nenhum latest.txt aponta para versao ausente/incompleta
    expect_true(file.exists(file.path(dirname(lt), readLines(lt, n = 1L), "SHA256SUMS")))
})

test_that("idempotente: 2a execucao exit 0 sem escrita (pulados = todos); v_<hash> local divergente => exit 9 sem sobrescrever", {
  pcs_skip()
  b <- BUCKET(); sv <- pc_serve_bucket(b$dir); sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  expect_identical(run_sync(sh, sv$url, S)$status, 0L)
  before <- pc_tree_sha(file.path(S, "artifacts"))
  r2 <- run_sync(sh, sv$url, S); j2 <- pj(r2)
  expect_identical(r2$status, 0L)
  expect_length(j2$result$baixados, 0L); expect_length(j2$result$latest_trocados, 0L)
  expect_length(j2$result$pulados, length(PCS_CELLS))
  expect_identical(pc_tree_sha(file.path(S, "artifacts")), before)
  v <- readLines(file.path(S, "artifacts", "lgbm", "A1", "latest.txt"), n = 1L)
  f <- file.path(S, "artifacts", "lgbm", "A1", v, "artifact.rds")
  writeBin(charToRaw("adulterado localmente"), f)
  mid <- pc_tree_sha(file.path(S, "artifacts"))
  r3 <- run_sync(sh, sv$url, S); j3 <- pj(r3)
  expect_identical(r3$status, 9L); expect_identical(r3$stderr, "")
  expect_false(j3$ok); expect_identical(j3$error$id, "sync.sha_mismatch")
  expect_identical(pc_tree_sha(file.path(S, "artifacts")), mid)                  # o v_<hash> local NAO foi sobrescrito
  expect_identical(rawToChar(readBin(f, "raw", file.size(f))), "adulterado localmente")
  expect_length(pc_tmp_left(file.path(S, "artifacts")), 0L)
})

test_that("[FORBIDDEN] 1 byte adulterado num objeto: exit 9 sync.sha_mismatch, arvore byte-identica, sem .tmp-*, sem latest.txt novo", {
  pcs_skip()
  b <- BUCKET(); sh <- pc_sync_install()
  for (pre in c("vazio", "parcial")) {
    S <- file.path(pci_work_dir(), "S")
    if (pre == "parcial") {                                                       # estado anterior nao vazio: so lgbm, integro
      ok <- pc_serve_bucket(b$dir); expect_identical(run_sync(sh, ok$url, S, "--model", "lgbm", "--area", "A1")$status, 0L); ok$stop()
    }
    before <- pc_tree_sha(file.path(S, "artifacts")); lst <- art_listing(S)
    sv <- pc_serve_bucket(b$dir, "tamper")
    r <- run_sync(sh, sv$url, S); j <- pj(r)
    info <- paste("pre:", pre)
    expect_identical(r$status, 9L, info = info); expect_identical(r$stderr, "", info = info)
    expect_false(j$ok); expect_identical(j$exit_code, 9L); expect_identical(j$error$id, "sync.sha_mismatch")
    expect_identical(j$error$class, "integrity")
    expect_identical(pc_tree_sha(file.path(S, "artifacts")), before, info = info)
    expect_identical(art_listing(S), lst, info = info)
    expect_length(pc_tmp_left(file.path(S, "artifacts")), 0L)
    sv$stop()
  }
})

test_that("[FORBIDDEN] tamper so no 2o prefixo (o 1o ja baixado em staging): nada e promovido, latest.txt nao muda", {
  pcs_skip()
  b <- BUCKET(); sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  sv <- pc_serve_bucket(b$dir, "tamper", target = "artifacts/reconcile/SS2/v_[0-9a-f]+/artifact\\.rds$")
  before <- pc_tree_sha(file.path(S, "artifacts"))
  r <- run_sync(sh, sv$url, S)
  expect_identical(r$status, 9L)
  expect_true(any(grepl("combina/A1", sv$served())))                              # os prefixos anteriores foram de fato baixados
  expect_identical(pc_tree_sha(file.path(S, "artifacts")), before)
  expect_length(Sys.glob(file.path(S, "artifacts", "*", "*", "latest.txt")), 0L)
})

test_that("[FORBIDDEN] conexao cortada no meio de um objeto: exit 10 sync.network, arvore byte-identica, sem .tmp-*, sem latest.txt trocado", {
  pcs_skip()
  b <- BUCKET(); sh <- pc_sync_install()
  for (n in c(2L, 6L)) {                                                           # n=2: dentro do 1o prefixo; n=6: o 1o prefixo ja completo
    S <- file.path(pci_work_dir(), "S")
    before <- pc_tree_sha(file.path(S, "artifacts"))
    sv <- pc_serve_bucket(b$dir, "cut_after", n = n)
    r <- run_sync(sh, sv$url, S); j <- pj(r)
    expect_identical(r$status, 10L, info = n); expect_identical(r$stderr, "")
    expect_false(j$ok); expect_identical(j$error$id, "sync.network"); expect_identical(j$error$class, "network")
    expect_identical(pc_tree_sha(file.path(S, "artifacts")), before, info = n)
    expect_length(pc_tmp_left(file.path(S, "artifacts")), 0L)
    expect_length(Sys.glob(file.path(S, "artifacts", "*", "*", "latest.txt")), 0L)
    sv$stop()
  }
})

test_that("servidor inalcancavel: exit 10 sync.network; release-base so http(s)", {
  pcs_skip()
  sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  r <- run_sync(sh, pc_dead_url(), S)
  expect_identical(r$status, 10L); expect_identical(pj(r)$error$id, "sync.network")
  expect_false(dir.exists(file.path(S, "artifacts")))
  r2 <- run_sync(sh, "ftp://x", S)
  expect_identical(r2$status, 8L); expect_identical(pj(r2)$error$id, "sync.usage")
})

test_that("--model e --area filtram os prefixos (combina/ e reconcile/ por --area; modelos por --model)", {
  pcs_skip()
  b <- BUCKET(); sv <- pc_serve_bucket(b$dir); sh <- pc_sync_install()
  S <- file.path(pci_work_dir(), "S")
  r <- run_sync(sh, sv$url, S, "--model", "lgbm"); expect_identical(r$status, 0L)
  got <- unique(sub("/(v_.*|latest\\.txt)$", "", sub(" .*", "", art_listing(S))))
  expect_setequal(got, c("lgbm/A1", "lgbm/exo1/A1", paste0("combina/", PCS_LEAVES), paste0("reconcile/", PCS_SS)))
  S2 <- file.path(pci_work_dir(), "S2")
  r2 <- run_sync(sh, sv$url, S2, "--area", "A1", "--area", "SS1"); expect_identical(r2$status, 0L)
  got2 <- unique(sub("/(v_.*|latest\\.txt)$", "", sub(" .*", "", art_listing(S2))))
  expect_setequal(got2, c("random-forest/A1", "lgbm/A1", "lgbm/exo1/A1", "holt-winters/A1", "combina/A1", "reconcile/SS1"))
  r3 <- run_sync(sh, sv$url, file.path(pci_work_dir(), "S3"), "--model", "nao-existe")
  expect_identical(r3$status, 8L)
})

test_that("--what forecasts: copia byte a byte da chave publicada, conferida pelo SHA256SUMS; --origin invalido => exit 8", {
  pcs_skip()
  b <- BUCKET(); sv <- pc_serve_bucket(b$dir); sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  r <- run_sync(sh, sv$url, S, "--origin", b$origin, what = "forecasts"); j <- pj(r)
  expect_identical(r$status, 0L); expect_true(j$ok); expect_identical(j$result$what, "forecasts")
  expect_length(j$result$baixados, 2L)
  for (k in b$forecast_keys) {
    src <- file.path(b$dir, "forecasts", b$origin, k); dst <- file.path(S, "forecasts", b$origin, k)
    expect_identical(pc_listing(dst), pc_listing(src), info = k)
    expect_identical(suppressWarnings(system(sprintf("cd %s && sha256sum -c --quiet SHA256SUMS >/dev/null 2>&1", shQuote(dst)))), 0L)
  }
  expect_length(pc_tmp_left(file.path(S, "forecasts")), 0L)
  r2 <- run_sync(sh, sv$url, S, "--origin", b$origin, what = "forecasts")                       # idempotente
  expect_identical(r2$status, 0L); expect_length(pj(r2)$result$baixados, 0L); expect_length(pj(r2)$result$pulados, 2L)
  for (bad in c("2026-1-15", "15/01/2026", "2026-02-30", "x")) {
    rb <- run_sync(sh, sv$url, S, "--origin", bad, what = "forecasts")
    expect_identical(rb$status, 8L, info = bad); expect_identical(pj(rb)$error$id, "sync.usage", info = bad)
  }
  expect_identical(run_sync(sh, sv$url, S, what = "forecasts")$status, 8L)                      # sem --origin
  expect_identical(run_sync(sh, sv$url, S, "--origin", b$origin)$status, 8L)                    # --origin com artifacts
  rn <- run_sync(sh, sv$url, file.path(pci_work_dir(), "S2"), "--origin", "2026-01-16", what = "forecasts")   # origem sem entrega publicada
  expect_identical(rn$status, 10L)
})

test_that("[FORBIDDEN] forecasts adulterado: exit 9, <state>/forecasts byte-identico", {
  pcs_skip()
  b <- BUCKET(); sv <- pc_serve_bucket(b$dir, "tamper"); sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  before <- pc_tree_sha(file.path(S, "forecasts"))
  r <- run_sync(sh, sv$url, S, "--origin", b$origin, what = "forecasts")
  expect_identical(r$status, 9L); expect_identical(pj(r)$error$id, "sync.sha_mismatch")
  expect_identical(pc_tree_sha(file.path(S, "forecasts")), before)
})

test_that("argumentos invalidos: exit 8 sync.usage em JSON (stderr vazio) e em texto (1a linha `prevcarga: erro [config] (exit 8)`, ASCII)", {
  pcs_skip()
  sh <- pc_sync_install(); S <- file.path(pci_work_dir(), "S")
  for (a in list(c("--bogus"), c("--what", "x"), c("--state", S), c("--what", "artifacts", "--format", "yaml"), c("--what", "artifacts", "--model"))) {
    r <- pc_sync_run(sh, c(a, "--format", "json"))
    expect_identical(r$status, 8L, info = paste(a, collapse = " ")); expect_identical(pj(r)$error$id, "sync.usage"); expect_identical(r$stderr, "")
    t <- pc_sync_run(sh, a)
    expect_identical(t$status, 8L)
    expect_identical(strsplit(t$stderr, "\n")[[1L]][1L], "prevcarga: erro [config] (exit 8)")
    expect_false(any(utf8ToInt(paste0(t$stdout, t$stderr)) > 127L))
  }
  expect_identical(pc_sync_run(sh, "--help")$status, 0L)
})

test_that("catalogo ao lado ausente: o fallback embutido tem o MESMO texto de scripts/lib/errors/sync.tsv", {
  pcs_skip()
  sh <- pc_sync_install(catalog = FALSE); S <- file.path(pci_work_dir(), "S")
  tsv <- utils::read.delim(file.path(pci_root(), "scripts", "lib", "errors", "sync.tsv"), sep = "\t", quote = "", colClasses = "character")
  b <- BUCKET()
  got <- list(
    sync.usage = pj(pc_sync_run(sh, c("--bogus", "--format", "json"))),
    sync.network = pj(run_sync(sh, pc_dead_url(), S)),
    sync.sha_mismatch = { sv <- pc_serve_bucket(b$dir, "tamper"); pj(run_sync(sh, sv$url, S)) })
  for (id in names(got)) {
    row <- tsv[tsv$id == id, ]
    expect_identical(got[[id]]$error$id, id)
    expect_identical(got[[id]]$error$message, row$message, info = id)
    expect_identical(got[[id]]$error$hint, row$hint, info = id)
    expect_identical(got[[id]]$error$class, row$class, info = id)
  }
  r <- pc_sync_run(pc_sync_install(catalog = FALSE, script = sh), c("--what", "artifacts", "--release-base", "http://x", "--state", S, "--format", "json"))
  expect_true(r$status %in% c(8L, 10L))
})

test_that("o JSON real do CLI (`prevcarga models|areas --format json`) tem a forma que o utilitario le (membros mdir+exogena; order; areas)", {
  pcs_skip()
  rs <- function(v) {
    o <- system2("env", c("TZ=UTC", "LC_ALL=C.UTF-8", shQuote(pci_rscript()), "--vanilla", shQuote(file.path(pci_root(), "scripts", "prevcarga")), v, "--format", "json"),
                 stdout = TRUE, stderr = FALSE)
    paste(o, collapse = "")
  }
  m <- rs("models"); a <- rs("areas")
  mem <- strsplit(gsub("{\"sigla\"", "\n{\"sigla\"", m, fixed = TRUE), "\n")[[1L]]; mem <- mem[startsWith(mem, "{\"sigla\"")]
  expect_gt(length(mem), 3L)
  expect_true(all(grepl("^\\{\"sigla\":\"[^\"]*\",\"mdir\":\"[^\"]*\",\"exogena\":(null|\"[^\"]*\")", mem)))
  expect_true(grepl("\"order\":\\[\"[A-Z]+\"", a)); expect_true(grepl("\"areas\":\\[\"", a))
})

test_that("equivalencia sh x ps1: mesmas listagens `path + sha256` (so com pwsh; senao skip explicito)", {
  pcs_skip()
  pw <- Sys.which("pwsh")
  testthat::skip_if(!nzchar(pw), "[DEFERRED M8.6 / owner: Gabriel, host com pwsh 7 ou Windows] pwsh ausente nesta maquina de dev: equivalencia sh x ps1 nao executada")
  ps1 <- file.path(pci_root(), "install", "prevcarga-sync.ps1")
  b <- BUCKET(); sv <- pc_serve_bucket(b$dir); sh <- pc_sync_install()
  S1 <- file.path(pci_work_dir(), "S1"); S2 <- file.path(pci_work_dir(), "S2")
  expect_identical(run_sync(sh, sv$url, S1)$status, 0L)
  d <- pci_work_dir(); bin <- file.path(d, "bin"); dir.create(bin); file.copy(ps1, bin)
  file.copy(file.path(dirname(sh), c("prevcarga", "error-catalog.tsv")), bin)
  st <- system2(pw, c("-NoProfile", "-File", shQuote(file.path(bin, "prevcarga-sync.ps1")), "-What", "artifacts", "-ReleaseBase", sv$url, "-State", S2, "-Format", "json"),
                stdout = FALSE, stderr = FALSE)
  expect_identical(as.integer(st), 0L)
  expect_identical(art_listing(S2), art_listing(S1))
})

## --- instalacao do utilitario (M8.6, step 4) -------------------------------------------------------------------------------
test_that("estatico (qualquer SO): sync.sh/.ps1/.cmd em ASCII, sem variavel de ambiente; o shim e CRLF sem atribuicao; os instaladores copiam o utilitario e o catalogo", {
  inst_dir <- file.path(pci_root(), "install")
  for (f in c("prevcarga-sync.sh", "prevcarga-sync.ps1", "prevcarga-sync.cmd")) {
    b <- readBin(file.path(inst_dir, f), "raw", file.size(file.path(inst_dir, f)))
    expect_true(all(b < as.raw(128L)), info = f)
  }
  ps <- paste(readLines(file.path(inst_dir, "prevcarga-sync.ps1"), warn = FALSE), collapse = "\n")
  expect_false(grepl("$env:", ps, fixed = TRUE)); expect_false(grepl("HKLM|HKCU", ps))
  sh <- paste(readLines(file.path(inst_dir, "prevcarga-sync.sh"), warn = FALSE), collapse = "\n")
  expect_false(grepl("AWS_|aws |getenv|printenv", sh))                                # sem credencial nem cliente do servico
  cmd <- readBin(file.path(inst_dir, "prevcarga-sync.cmd"), "raw", 1e5)
  txt <- rawToChar(cmd)
  expect_true(grepl("\r\n", txt, fixed = TRUE)); expect_false(grepl("[^\r]\n", txt))
  expect_false(grepl("(^|\n)\\s*set ", txt, ignore.case = TRUE))
  expect_true(grepl("-NoProfile -ExecutionPolicy Bypass -File \"%~dp0prevcarga-sync.ps1\" %*", txt, fixed = TRUE))
  isl <- paste(readLines(file.path(inst_dir, "install.sh"), warn = FALSE), collapse = "\n")
  expect_true(grepl("bin/prevcarga-sync", isl, fixed = TRUE)); expect_true(grepl("bin/error-catalog.tsv", isl, fixed = TRUE))
  ip <- paste(readLines(file.path(inst_dir, "install.ps1"), warn = FALSE), collapse = "\n")
  expect_true(grepl("prevcarga-sync.ps1", ip, fixed = TRUE)); expect_true(grepl("prevcarga-sync.cmd", ip, fixed = TRUE))
  expect_true(grepl("error-catalog.tsv", ip, fixed = TRUE))
  ## CONTROL: o detector de byte > 127 pega um acento injetado
  expect_false(all(charToRaw("aç") < as.raw(128L)))
})

test_that("install.sh instala <R>/bin/prevcarga-sync e <R>/bin/error-catalog.tsv (== code/install do bundle); o uninstall os remove (precisa do bundle candidato)", {
  pci_skip_unless_ready()
  b <- pc_fixture_bundle("vsync")
  R <- pc_fixture_install_root()
  r <- pc_install(c("--bundle", b$path, "--sha256", b$sha256, "--root", R, "--format", "json"))
  expect_identical(r$status, 0L)
  expect_true(file.exists(file.path(R, "bin", "prevcarga-sync")))
  expect_identical(pcs_sha(file.path(R, "bin", "error-catalog.tsv")), pcs_sha(file.path(R, "versions", "vsync", "code", "install", "error-catalog.tsv")))
  expect_identical(pc_sync_run(file.path(R, "bin", "prevcarga-sync"), "--help")$status, 0L)
  expect_identical(pc_install(c("--uninstall", "--yes", "--root", R, "--format", "json"))$status, 0L)
  expect_false(file.exists(file.path(R, "bin", "prevcarga-sync")))
})
