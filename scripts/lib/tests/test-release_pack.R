## test-release_pack.R -- verbo `release-pack` (M8.5; ADR-0039, ADR-0040). In-process (Linux e Windows): runtime, artifacts, forecasts,
## `<out>` nao-vazio, falha de E/S injetada, latest.txt e SHA256SUMS. Cada braco FORBIDDEN tem o seu CONTROL (a recusa vale; o caso
## limpo passa). Mais 2 testes de PROCESSO (a fachada real, JSON unico em stdout e stderr vazio). Tudo em tempdir; nada de rede,
## de bucket ou de variavel de ambiente.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

E <- pc_source_lib(c("errors", "topology", "provenance", "registry", "exports", "release_pack"))
TZ_LOAD <- "Etc/GMT+3"

## --- fixtures ---------------------------------------------------------------------------------------------------
tmp_dir <- function(tag) { d <- tempfile(paste0("rp-", tag, "-")); dir.create(d, recursive = TRUE); d }
put <- function(path, txt) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeBin(charToRaw(txt), path); path }
sha <- function(f) digest::digest(file = f, algo = "sha256")

fx_root <- tmp_dir("root")
dir.create(file.path(fx_root, "config"))
file.copy(m6f_topology_file(tmp_dir("topo")), file.path(fx_root, "config", "topology.yaml"))   # folhas A1 A2 B1; SS1 SS2
LEAVES <- c("A1", "A2", "B1"); SSES <- c("SS1", "SS2")

new_ctx <- function(state = tmp_dir("state"), root = fx_root, run_id = "rp-test-run")
  list(root = root, state_dir = state, run_id = run_id)

cond_of <- function(expr) tryCatch({ expr; NULL }, prevcarga_error = function(e) e)
pub_file <- function(keys = character()) { f <- tempfile("pub-"); writeLines(keys, f); f }
stage_left <- function(out) Sys.glob(paste0(out, ".stage-*"))

## registry local: <state>/artifacts/<cell>/{latest.txt, v_<hash>/{artifact.rds, run_manifest.json, SHA256SUMS}}
mk_cell <- function(ctx, cell, hex) {
  v <- paste0("v_", hex)
  d <- file.path(ctx$state_dir, "artifacts", cell, v)
  put(file.path(d, "artifact.rds"), paste("rds", cell, hex))
  put(file.path(d, "run_manifest.json"), paste0("{\"cell\":\"", cell, "\"}"))
  fs <- c("artifact.rds", "run_manifest.json")
  put(file.path(d, "SHA256SUMS"), paste0(vapply(file.path(d, fs), sha, ""), "  ", fs, "\n", collapse = ""))
  put(file.path(ctx$state_dir, "artifacts", cell, "latest.txt"), paste0(v, "\n"))
  put(file.path(ctx$state_dir, "artifacts", cell, ".history"), paste0(v, "\n"))      # so local: nunca sai
  dir.create(file.path(ctx$state_dir, "artifacts", cell, ".tmp-orfao"))
  invisible(v)
}
mk_registry <- function(ctx, official = TRUE, omit = character()) {
  cells <- c("random-forest/A1", "lgbm/A1", "lgbm/exo1/A1", "holt-winters/A1",              # 2 classes de split; 1 com variante
             if (official) c(paste0("combina/", LEAVES), paste0("reconcile/", SSES)))
  cells <- setdiff(cells, omit)
  for (i in seq_along(cells)) mk_cell(ctx, cells[[i]], sprintf("%012x", 4096L + i))
  invisible(cells)
}
rp <- function(what, out, ctx, pub = pub_file(), ...) E$prevcarga_release_pack(what, out, pub, ctx, ...)

## bundles + recibos (tests/.receipts/<sha>.<gate>.json)
mk_bundles <- function(v = "1.0.0", plats = c("linux", "windows")) {
  d <- tmp_dir("bundles")
  b <- character()
  if ("linux" %in% plats) {
    f <- put(file.path(d, sprintf("prevcarga-%s-linux-x86_64.tar.zst", v)), "fake linux bundle")
    put(paste0(f, ".RUNTIME.json"), "{\"platform\":\"linux\"}\n")
    b <- c(b, f)
  }
  if ("windows" %in% plats) {
    f <- put(file.path(d, sprintf("prevcarga-%s-windows-x86_64.zip", v)), "fake windows bundle")
    put(paste0(f, ".RUNTIME.json"), "{\"platform\":\"windows\"}\n")
    b <- c(b, f)
  }
  list(dir = d, files = b)
}
mk_receipt <- function(rdir, bundle, gate, resultado = "PASS", veredito = NULL, sha_field = NULL) {
  s <- sha(bundle)
  dir.create(rdir, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(list(gate = gate, sha256 = if (is.null(sha_field)) s else sha_field, resultado = resultado,
                            veredito = veredito), file.path(rdir, sprintf("%s.%s.json", s, gate)), auto_unbox = TRUE, null = "null")
}
mk_receipts <- function(rdir, b, skip = character()) {
  for (f in b$files) {
    gates <- if (grepl("linux", f)) c("install-e2e-linux", "ab-corte") else c("install-e2e-windows", "windows-vm", "ab-corte")
    for (g in setdiff(gates, skip)) mk_receipt(rdir, f, g, veredito = if (g == "ab-corte") "CORTE-OK")
  }
}
tree <- function(d) { fs <- list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE); fs[order(fs, method = "radix")] }

## =======================================================================================================================
## runtime
## =======================================================================================================================
test_that("[FORBIDDEN] runtime: deck, config.yaml e chave publicada -> exit 9 com todos os itens e nada escrito", {
  b <- mk_bundles(); rdir <- tmp_dir("rc"); mk_receipts(rdir, b)
  put(file.path(b$dir, "load_carga_2026.parquet"), "PAR1 dados do deck")
  put(file.path(b$dir, "config.yaml"), "data_dir: /x")
  out <- file.path(tmp_dir("o"), "pack")
  pub <- pub_file(c("releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst", "releases/1.0.0/SHA256SUMS"))
  cond <- cond_of(rp("runtime", out, new_ctx(), pub, bundles = b$dir, receipts_dir = rdir))
  expect_false(is.null(cond))
  expect_identical(cond$id, "integrity.release_forbidden_content")
  expect_identical(cond$exit_code, 9L)
  rec <- cond$result$recusados
  expect_setequal(rec$path[rec$motivo == "fora_da_allowlist"], c("load_carga_2026.parquet", "config.yaml"))
  expect_true("releases/1.0.0/" %in% rec$path[rec$motivo == "chave_publicada"])
  expect_false(dir.exists(out))
  expect_length(stage_left(out), 0L)
})

test_that("[CONTROL] runtime: so os dois bundles + recibos PASS do sha exato -> release completo", {
  b <- mk_bundles(); rdir <- tmp_dir("rc"); mk_receipts(rdir, b)
  out <- file.path(tmp_dir("o"), "pack")
  r <- rp("runtime", out, new_ctx(), bundles = b$dir, receipts_dir = rdir)
  expect_identical(r$what, "runtime")
  expect_identical(as.character(r$chaves), "releases/1.0.0/")
  expect_setequal(tree(out), c("SHA256SUMS", "releases/1.0.0/SHA256SUMS", "releases/1.0.0/RUNTIME-linux-x86_64.json",
                               "releases/1.0.0/RUNTIME-windows-x86_64.json", "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst",
                               "releases/1.0.0/prevcarga-1.0.0-windows-x86_64.zip"))
  expect_identical(sha(file.path(out, "releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst")), sha(b$files[[1L]]))
  expect_identical(readLines(file.path(out, "releases/1.0.0/RUNTIME-linux-x86_64.json"), warn = FALSE), "{\"platform\":\"linux\"}")
  expect_length(stage_left(out), 0L)
})

test_that("runtime: recibo de OUTRO sha, ausente ou reprovado -> integrity.release_receipt (exit 9), nada escrito", {
  b <- mk_bundles()
  chk <- function(setup, motivo) {
    rdir <- tmp_dir("rc"); setup(rdir)
    out <- file.path(tmp_dir("o"), "pack")
    cond <- cond_of(rp("runtime", out, new_ctx(), bundles = b$dir, receipts_dir = rdir))
    expect_identical(cond$id, "integrity.release_receipt")
    expect_identical(cond$exit_code, 9L)
    expect_true(any(startsWith(cond$result$recusados$motivo, motivo)), info = motivo)
    expect_false(dir.exists(out)); expect_length(stage_left(out), 0L)
  }
  chk(function(r) { mk_receipts(r, b, skip = "install-e2e-linux")
                    mk_receipt(r, b$files[[1L]], "install-e2e-linux", sha_field = strrep("0", 64)) }, "recibo_outro_sha")
  chk(function(r) mk_receipts(r, b, skip = "windows-vm"), "recibo_ausente")
  chk(function(r) { mk_receipts(r, b, skip = "install-e2e-windows")
                    mk_receipt(r, b$files[[2L]], "install-e2e-windows", resultado = "FAIL") }, "recibo_fail")
  chk(function(r) { mk_receipts(r, b, skip = "ab-corte")                                   # ab-corte exige CORTE-OK
                    for (f in b$files) mk_receipt(r, f, "ab-corte", veredito = "CORTE-BLOQUEADO") }, "recibo_fail")
  ## recibo do bundle A nao vale para o bundle B (o nome do recibo carrega o sha)
  chk(function(r) { mk_receipts(r, b); unlink(file.path(r, sprintf("%s.windows-vm.json", sha(b$files[[2L]])))) }, "recibo_ausente")
})

test_that("runtime: --latest escreve releases/latest.txt (uma linha <v>); sem a flag nenhum latest.txt existe", {
  b <- mk_bundles(); rdir <- tmp_dir("rc"); mk_receipts(rdir, b)
  o1 <- file.path(tmp_dir("o"), "sem"); rp("runtime", o1, new_ctx(), bundles = b$dir, receipts_dir = rdir)
  expect_false(any(basename(tree(o1)) == "latest.txt"))
  o2 <- file.path(tmp_dir("o"), "com"); rp("runtime", o2, new_ctx(), bundles = b$dir, receipts_dir = rdir, latest = TRUE)
  expect_identical(readBin(file.path(o2, "releases/latest.txt"), "raw", 100L), charToRaw("1.0.0\n"))
})

test_that("runtime: versao divergente entre --release e o nome do bundle, e bundle sem recibo de plataforma desconhecida, sao recusados", {
  b <- mk_bundles(); rdir <- tmp_dir("rc"); mk_receipts(rdir, b)
  out <- file.path(tmp_dir("o"), "pack")
  cond <- cond_of(rp("runtime", out, new_ctx(), bundles = b$dir, receipts_dir = rdir, version = "2.0.0"))
  expect_identical(cond$id, "integrity.release_inconsistent")
  expect_true(all(cond$result$recusados$motivo == "versao_divergente"))
  expect_false(dir.exists(out))
  expect_identical(cond_of(rp("runtime", out, new_ctx(), bundles = b$dir, receipts_dir = rdir, version = "../x"))$id, "config.usage")
})

test_that("runtime: o RUNTIME.json e extraido do proprio tar.zst quando o sistema tem tar e zstd", {
  skip_if(!nzchar(Sys.which("tar")) || !nzchar(Sys.which("zstd")), "tar/zstd ausentes")
  d <- tmp_dir("real"); st <- file.path(d, "prevcarga"); dir.create(st)
  put(file.path(st, "RUNTIME.json"), "{\"runtime\":\"extraido\"}\n"); put(file.path(st, "README"), "x")
  bf <- file.path(d, "prevcarga-3.1.0-linux-x86_64.tar.zst")
  system2("tar", c("--zstd", "-cf", shQuote(bf), "-C", shQuote(d), "prevcarga"))
  skip_if(!file.exists(bf), "tar --zstd indisponivel")
  bd <- tmp_dir("b3"); file.copy(bf, bd)
  rdir <- tmp_dir("rc"); mk_receipts(rdir, list(files = file.path(bd, basename(bf))))
  out <- file.path(tmp_dir("o"), "pack")
  rp("runtime", out, new_ctx(), bundles = bd, receipts_dir = rdir)
  expect_identical(readLines(file.path(out, "releases/3.1.0/RUNTIME-linux-x86_64.json"), warn = FALSE), "{\"runtime\":\"extraido\"}")
})

## =======================================================================================================================
## artifacts
## =======================================================================================================================
test_that("artifacts: layout do bucket com caminho relativo e sha256 de cada arquivo identicos ao registry local", {
  ctx <- new_ctx(); cells <- mk_registry(ctx)
  out <- file.path(tmp_dir("o"), "pack")
  r <- rp("artifacts", out, ctx)
  reg <- file.path(ctx$state_dir, "artifacts")
  want <- unlist(lapply(cells, function(c) {
    v <- readLines(file.path(reg, c, "latest.txt"), warn = FALSE)
    file.path("artifacts", c, v, list.files(file.path(reg, c, v)))
  }))
  expect_setequal(setdiff(tree(out), "SHA256SUMS"), want)
  for (f in want) expect_identical(sha(file.path(out, f)), sha(file.path(reg, sub("^artifacts/", "", f))), info = f)
  expect_true(all(c("artifacts/combina/A1", "artifacts/reconcile/SS2", "artifacts/lgbm/exo1/A1", "artifacts/holt-winters/A1") %in%
                    sub("/v_[0-9a-f]+/.*$", "", want)))
  expect_false(any(grepl("(^|/)(\\.history|\\.pin|\\.tmp-[^/]*)(/|$)", tree(out))))      # estado so local nunca sai
  expect_false(any(basename(tree(out)) == "latest.txt"))                                  # sem --latest nenhum ponteiro
  expect_length(r$chaves, length(cells))
  expect_length(stage_left(out), 0L)
})

test_that("[CONTROL] artifacts: sem combina/ ou sem reconcile/ -> state.official_set_incomplete (exit 13), <out> nao criado", {
  for (omit in list(paste0("combina/", LEAVES), "reconcile/SS2", c(paste0("combina/", LEAVES), paste0("reconcile/", SSES)))) {
    ctx <- new_ctx(); mk_registry(ctx, omit = omit)
    out <- file.path(tmp_dir("o"), "pack")
    cond <- cond_of(rp("artifacts", out, ctx))
    expect_identical(cond$id, "state.official_set_incomplete")
    expect_identical(cond$exit_code, 13L)
    expect_setequal(unlist(cond$details$faltam), omit)
    expect_false(dir.exists(out)); expect_length(stage_left(out), 0L)
  }
})

test_that("artifacts: --latest escreve o latest.txt de cada celula com o mesmo conteudo do registry; --model filtra os modelos", {
  ctx <- new_ctx(); mk_registry(ctx)
  out <- file.path(tmp_dir("o"), "pack")
  rp("artifacts", out, ctx, latest = TRUE, models = "lgbm")
  lt <- grep("latest\\.txt$", tree(out), value = TRUE)
  expect_true(all(c("artifacts/combina/B1/latest.txt", "artifacts/reconcile/SS1/latest.txt", "artifacts/lgbm/A1/latest.txt") %in% lt))
  expect_false(any(grepl("random-forest|holt-winters", tree(out))))
  for (f in lt) expect_identical(sha(file.path(out, f)), sha(file.path(ctx$state_dir, f)), info = f)
  expect_identical(cond_of(rp("artifacts", file.path(tmp_dir("o"), "p2"), ctx, models = "nao-existe"))$id, "config.usage")
})

test_that("artifacts: chave ja publicada e conteudo proibido dentro de v_<hash>/ sao recusados (exit 9) sem escrever", {
  ctx <- new_ctx(); mk_registry(ctx)
  v <- readLines(file.path(ctx$state_dir, "artifacts", "lgbm", "A1", "latest.txt"), warn = FALSE)
  out <- file.path(tmp_dir("o"), "pack")
  cond <- cond_of(rp("artifacts", out, ctx, pub_file(paste0("artifacts/lgbm/A1/", v, "/artifact.rds"))))
  expect_identical(cond$id, "integrity.release_key_published")
  expect_identical(cond$result$recusados$path, paste0("artifacts/lgbm/A1/", v, "/"))
  expect_false(dir.exists(out))
  put(file.path(ctx$state_dir, "artifacts", "lgbm", "A1", v, "config.yaml"), "data_dir: x")
  put(file.path(ctx$state_dir, "artifacts", "lgbm", "A1", v, "load_2026.parquet"), "PAR1")
  cond <- cond_of(rp("artifacts", out, ctx))
  expect_identical(cond$id, "integrity.release_forbidden_content")
  expect_setequal(cond$result$recusados$motivo, "proibido")
  expect_false(dir.exists(out))
})

## =======================================================================================================================
## forecasts
## =======================================================================================================================
VERSAO <- "0123456789ab"
mk_delivery <- function(ctx, day = "2026-01-15", au_utc = "2026-01-15T12:00:00Z", au_dir = "20260115T0900", versao = VERSAO) {
  d <- file.path(ctx$state_dir, "forecasts", day, au_dir, versao)
  put(file.path(d, "manifest.json"), "{\"arquivos\":[]}\n")
  put(file.path(d, "run_manifest.json"), jsonlite::toJSON(list(origin = day, available_until = au_utc, load_tz = TZ_LOAD, versao = versao),
                                                          auto_unbox = TRUE))
  put(file.path(d, "SIN.parquet"), "PAR1 bytes da entrega SIN")
  put(file.path(d, "membros", "RF", "A1.parquet"), "PAR1 bytes do membro")
  d
}

test_that("forecasts: copia byte a byte da entrega local na chave forecasts/<O>/<AAAAMMDDTHHMM>/<versao>/", {
  ctx <- new_ctx(); d <- mk_delivery(ctx)
  out <- file.path(tmp_dir("o"), "pack")
  r <- rp("forecasts", out, ctx, origin = "2026-01-15")
  k <- file.path(out, "forecasts/2026-01-15/20260115T0900", VERSAO)
  expect_identical(as.character(r$chaves), paste0("forecasts/2026-01-15/20260115T0900/", VERSAO, "/"))
  src <- list.files(d, recursive = TRUE)
  expect_setequal(setdiff(list.files(k, recursive = TRUE), "SHA256SUMS"), src)
  for (f in src) expect_identical(sha(file.path(k, f)), sha(file.path(d, f)), info = f)
  expect_true(file.exists(file.path(k, "SHA256SUMS")))                          # prefixo conferivel sem listar o bucket
})

test_that("forecasts: --latest escreve o indice forecasts/<O>/latest.txt (uma <AAAAMMDDTHHMM>/<versao> por linha) que o sync le; sem a flag nao", {
  ctx <- new_ctx(); mk_delivery(ctx)
  o1 <- file.path(tmp_dir("o"), "sem"); rp("forecasts", o1, ctx, origin = "2026-01-15")
  expect_false(file.exists(file.path(o1, "forecasts/2026-01-15/latest.txt")))
  o2 <- file.path(tmp_dir("o"), "com"); rp("forecasts", o2, ctx, origin = "2026-01-15", latest = TRUE)
  idx <- readLines(file.path(o2, "forecasts/2026-01-15/latest.txt"), warn = FALSE)
  expect_identical(idx, paste0("20260115T0900/", VERSAO))
  ## cada linha aponta para um prefixo que existe no pacote (o mesmo formato que install/prevcarga-sync.sh aceita)
  for (k in idx) {
    expect_true(grepl("^[0-9]{8}T[0-9]{4}/[^/]+$", k))
    expect_true(dir.exists(file.path(o2, "forecasts/2026-01-15", k)))
  }
})

test_that("[CONTROL] forecasts: a mesma chave em --published -> integrity.release_key_published (exit 9) e nada escrito", {
  ctx <- new_ctx(); mk_delivery(ctx)
  out <- file.path(tmp_dir("o"), "pack")
  cond <- cond_of(rp("forecasts", out, ctx, pub_file(paste0("forecasts/2026-01-15/20260115T0900/", VERSAO, "/SIN.parquet")),
                     origin = "2026-01-15"))
  expect_identical(cond$id, "integrity.release_key_published")
  expect_identical(cond$exit_code, 9L)
  expect_false(dir.exists(out)); expect_length(stage_left(out), 0L)
})

test_that("forecasts: run_manifest ausente ou de outra chave, e manifest.json de deck, sao recusados; origem sem entrega -> 13", {
  chk <- function(setup, id, motivo) {
    ctx <- new_ctx(); d <- mk_delivery(ctx); setup(d, ctx)
    out <- file.path(tmp_dir("o"), "pack")
    cond <- cond_of(rp("forecasts", out, ctx, origin = "2026-01-15"))
    expect_identical(cond$id, id); expect_true(motivo %in% cond$result$recusados$motivo, info = motivo)
    expect_false(dir.exists(out))
  }
  chk(function(d, ctx) unlink(file.path(d, "run_manifest.json")), "integrity.release_inconsistent", "run_manifest_ausente")
  chk(function(d, ctx) put(file.path(d, "run_manifest.json"), jsonlite::toJSON(
        list(origin = "2026-01-15", available_until = "2026-01-15T13:00:00Z", load_tz = TZ_LOAD, versao = VERSAO), auto_unbox = TRUE)),
      "integrity.release_inconsistent", "chave_divergente")
  chk(function(d, ctx) put(file.path(d, "manifest.json"), "{\"schema_version\":1,\"timezone\":{\"load\":\"Etc/GMT+3\"}}"),
      "integrity.release_forbidden_content", "deck_manifest")
  chk(function(d, ctx) put(file.path(d, "config.yaml"), "x: 1"), "integrity.release_forbidden_content", "proibido")
  cond <- cond_of(rp("forecasts", file.path(tmp_dir("o"), "p"), new_ctx(), origin = "2026-01-15"))
  expect_identical(cond$id, "state.nothing_to_deliver")
  expect_identical(cond_of(rp("forecasts", file.path(tmp_dir("o"), "p"), new_ctx()))$id, "config.usage")
})

## =======================================================================================================================
## <out>, E/S, latest e SHA256SUMS
## =======================================================================================================================
test_that("<out> existente e nao-vazio -> config.release_out_not_empty (exit 8) sem tocar nele; <out> vazio e aceito", {
  ctx <- new_ctx(); mk_registry(ctx)
  out <- tmp_dir("full"); put(file.path(out, "dono.txt"), "nao mexer")
  before <- tree(out)
  cond <- cond_of(rp("artifacts", out, ctx))
  expect_identical(cond$id, "config.release_out_not_empty")
  expect_identical(cond$exit_code, 8L)
  expect_identical(tree(out), before)
  expect_identical(readLines(file.path(out, "dono.txt"), warn = FALSE), "nao mexer")
  expect_length(stage_left(out), 0L)
  empty <- tmp_dir("empty")
  rp("artifacts", empty, ctx)
  expect_true(length(tree(empty)) > 0L)
})

test_that("falha de E/S injetada no meio da copia: o comando falha e nem <out> nem stage <out>.stage-* restam", {
  ctx <- new_ctx(); mk_registry(ctx)
  out <- file.path(tmp_dir("o"), "pack")
  n <- 0L
  real <- E$.pc_rp_copy
  E$.pc_rp_copy <- function(from, to) { n <<- n + 1L; if (n >= 5L) FALSE else real(from, to) }
  on.exit(E$.pc_rp_copy <- real, add = TRUE)
  cond <- cond_of(rp("artifacts", out, ctx))
  expect_identical(cond$id, "integrity.copy_failed")
  expect_gte(n, 5L)
  expect_false(dir.exists(out))
  expect_length(stage_left(out), 0L)
  expect_length(list.files(dirname(out), all.files = TRUE, no.. = TRUE), 0L)
  E$.pc_rp_copy <- real                                         # CONTROL: sem a falha o mesmo comando passa
  expect_silent(rp("artifacts", out, ctx))
  expect_true(dir.exists(out))
})

test_that("SHA256SUMS: linhas '<sha256>  <caminho>' ordenadas (radix), LF, na raiz e dentro de cada v_<hash>/; sha256sum -c passa", {
  ctx <- new_ctx(); mk_registry(ctx)
  out <- file.path(tmp_dir("o"), "pack")
  rp("artifacts", out, ctx, latest = TRUE)
  sha_check <- function(d) if (nzchar(Sys.which("sha256sum"))) {
    old <- setwd(d); on.exit(setwd(old), add = TRUE)
    expect_identical(suppressWarnings(system2("sha256sum", c("-c", "SHA256SUMS"), stdout = FALSE, stderr = FALSE)), 0L)
  }
  chk_dir <- function(d) {
    raw <- readBin(file.path(d, "SHA256SUMS"), "raw", file.size(file.path(d, "SHA256SUMS")))
    expect_false(as.raw(13L) %in% raw)
    ln <- strsplit(rawToChar(raw), "\n", fixed = TRUE)[[1L]]
    expect_identical(rawToChar(raw), paste0(paste(ln, collapse = "\n"), "\n"))
    m <- regmatches(ln, regexec("^([0-9a-f]{64})  (.+)$", ln))
    expect_true(all(lengths(m) == 3L))
    paths <- vapply(m, function(x) x[[3L]], "")
    expect_identical(paths, sort(paths, method = "radix"))
    for (x in m) expect_identical(sha(file.path(d, x[[3L]])), x[[2L]])
    sha_check(d)
    paths
  }
  root_paths <- chk_dir(out)
  expect_setequal(root_paths, setdiff(tree(out), "SHA256SUMS"))
  vd <- file.path(out, "artifacts", "lgbm", "A1", readLines(file.path(out, "artifacts/lgbm/A1/latest.txt"), warn = FALSE))
  expect_setequal(chk_dir(vd), setdiff(list.files(vd), "SHA256SUMS"))
})

test_that("pc_rp_classify: allowlist por --what e conteudo sempre proibido", {
  ok <- function(p, w) E$pc_rp_classify(p, w)$ok
  v <- "v_0123456789ab"
  expect_true(ok(sprintf("artifacts/lgbm/SECO/%s/artifact.rds", v), "artifacts"))
  expect_true(ok(sprintf("artifacts/lgbm/exo1/SECO/%s/artifact.rds", v), "artifacts"))
  expect_true(ok("artifacts/combina/SECO/latest.txt", "artifacts"))
  expect_false(ok("artifacts/lgbm/SECO/v_zz/artifact.rds", "artifacts"))
  expect_false(ok("releases/1.0.0/prevcarga-1.0.0-linux-x86_64.tar.zst", "artifacts"))
  expect_true(ok("releases/1.0.0/prevcarga-1.0.0-windows-x86_64.zip", "runtime"))
  expect_false(ok("releases/1.0.0/prevcarga-1.0.0-macos.tar.gz", "runtime"))
  expect_true(ok("forecasts/2026-01-15/20260115T0900/0123456789ab/SIN.parquet", "forecasts"))
  expect_false(ok("forecasts/2026-01-15/0900/0123456789ab/SIN.parquet", "forecasts"))
  for (p in c("config.yaml", "x/credentials", "a/.aws/config", "chave.pem", "k.key", ".env", "auxiliary/feriados.parquet",
              "load_2026.parquet", "weather_forecast_2026.parquet", "weather_observed.parquet"))
    expect_false(ok(p, "runtime"), info = p)
  forb <- E$pc_rp_classify(sprintf("artifacts/lgbm/SECO/%s/%s", v, c("credentials", "x.pem", "config.yaml", ".env")), "artifacts")
  expect_true(all(forb$motivo == "proibido"))
  expect_error(E$pc_rp_classify("x", "tudo"), class = "prevcarga_error")
})

test_that("o modulo do release-pack nao le env, nao fala rede e nao conhece o servico de upload", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "release_pack.R"), warn = FALSE)
  src <- sub("^\\s*#.*$", "", src)
  expect_false(any(grepl("Sys\\.getenv|Sys\\.setenv|download\\.file|curl|aws s3|AWS_PROFILE|s3://|httr", src)))
})

## =======================================================================================================================
## processo real (fachada): JSON unico em stdout, stderr vazio, exits do catalogo
## =======================================================================================================================
test_that("CLI: runtime com proibidos/chave publicada -> exit 9 JSON unico; CONTROL limpo -> exit 0; <out> cheio -> exit 8", {
  root <- pc_install_copy()
  b <- mk_bundles(); rdir <- file.path(root, "tests", ".receipts"); mk_receipts(rdir, b)
  state <- tmp_dir("cs")
  base <- c("release-pack", "--what=runtime", paste0("--bundles=", b$dir), paste0("--state=", state), "--format=json")
  pj <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
  put(file.path(b$dir, "load_x.parquet"), "PAR1"); put(file.path(b$dir, "config.yaml"), "a: 1")
  out <- file.path(tmp_dir("o"), "pack")
  pub <- pub_file("releases/1.0.0/")
  r <- pc_cli(root, c(base, paste0("--out=", out), paste0("--published=", pub)))
  expect_identical(r$status, 9L); expect_length(r$stderr, 0L); expect_length(r$stdout, 1L)
  j <- pj(r)
  expect_false(j$ok); expect_identical(j$error$class, "integrity")
  expect_true(any(vapply(j$result$recusados, function(x) x$motivo, "") == "chave_publicada"))
  expect_identical(sort(vapply(Filter(function(x) x$motivo == "fora_da_allowlist", j$result$recusados), function(x) x$path, "")),
                   c("config.yaml", "load_x.parquet"))
  expect_false(dir.exists(out))
  unlink(file.path(b$dir, c("load_x.parquet", "config.yaml")))
  r <- pc_cli(root, c(base, paste0("--out=", out), paste0("--published=", pub_file())))
  expect_identical(r$status, 0L); expect_length(r$stderr, 0L)
  j <- pj(r); expect_true(j$ok); expect_identical(j$result$what, "runtime")
  expect_true(dir.exists(file.path(out, "releases", "1.0.0")))
  r <- pc_cli(root, c(base, paste0("--out=", out), paste0("--published=", pub_file())))
  expect_identical(r$status, 8L); expect_identical(pj(r)$error$id, "config.release_out_not_empty")
  expect_length(r$stderr, 0L)
})

test_that("CLI: artifacts sem reconcile/ -> exit 13 state.official_set_incomplete; flags de escopo fora do release-pack -> exit 8", {
  root <- pc_install_copy(mutate = function(r) file.copy(file.path(fx_root, "config", "topology.yaml"),
                                                         file.path(r, "config", "topology.yaml"), overwrite = TRUE))
  state <- tmp_dir("cs"); ctx <- new_ctx(state); mk_registry(ctx, omit = paste0("reconcile/", SSES))
  out <- file.path(tmp_dir("o"), "pack")
  r <- pc_cli(root, c("release-pack", "--what=artifacts", paste0("--out=", out), paste0("--published=", pub_file()),
                      paste0("--state=", state), "--format=json"))
  expect_identical(r$status, 13L); expect_length(r$stderr, 0L); expect_length(r$stdout, 1L)
  expect_identical(jsonlite::fromJSON(r$stdout)$error$id, "state.official_set_incomplete")
  expect_false(dir.exists(out))
  r <- pc_cli(root, c("describe", "--what=runtime", paste0("--state=", state)))
  expect_identical(r$status, 8L)
})
