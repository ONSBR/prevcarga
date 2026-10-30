## test-exports_key.R -- chave imutavel da entrega (M6.7): versao, run_id, layout forecasts/ e no-op/conflito. Cada FORBIDDEN tem o seu CONTROL.
## Tudo em tempdir; sem env vars; scratch de fixture montado aqui (parquet via arrow SO no teste).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
ek <- pc_source_lib(c("errors", "provenance", "schema_long", "exports"))

KSS <- c("SS1", "SS2")
kwrite_leaf <- function(path, area, n, seed) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data.frame(area = area, y_pred = seed * seq_len(n) + 0.5), path, compression = "snappy")
}
kmake_scratch <- function(seed = 1L) {
  s <- withr::local_tempdir("pc-kscratch-", .local_envir = parent.frame())
  rows <- list(); put <- function(rel, area, n, sd) { kwrite_leaf(file.path(s, rel), area, n, sd); rows[[rel]] <<- list(area = area, n = n) }
  put("SIN.parquet", "SIN", 7L, seed)
  for (ss in KSS) put(paste0("subsistemas/", ss, ".parquet"), ss, 6L, 2L)
  put("folhas/Z9.parquet", "Z9", 3L, 4L)
  reg <- lapply(names(rows), function(rel) list(path = rel, area = rows[[rel]]$area, linhas = rows[[rel]]$n,
                                                 sha256 = digest::digest(file = file.path(s, rel), algo = "sha256")))
  run <- list(origem = "2026-01-15", available_until = "2026-01-15T09:00", timezone = "Etc/GMT+3", load_label = "end",
              weather_issue_cutoff = "available_until",
              khat = lapply(KSS, function(ss) list(ss = ss, versao = "v_0123456789abcdef", khat_origin = "2026-01-15")),
              covered = list(), stray = list("Z9"), closed = as.list(KSS), sin_closed = TRUE, members = list(),
              arquivos = reg, versao = "abc123", version = "0.1.0", runtime_id = "rt-1")
  writeLines(as.character(jsonlite::toJSON(run, auto_unbox = TRUE, pretty = TRUE)), file.path(s, "run.json"))
  s
}
kstate <- function(at = "2026-01-15T12:01:00Z") {
  list(state_dir = withr::local_tempdir("pc-kstate-", .local_envir = parent.frame()), generated_at = at)
}
KTZ <- "Etc/GMT+3"
kparts <- function(...) modifyList(list(
  package = "0.1.0", members = c(`random-forest` = "v_aaaaaaaaaaaaaaaa", lgbm = "v_bbbbbbbbbbbbbbbb"),
  combina_weights = "v_cccccccccccccccc", khat = c(SECO = "v_1111111111111111", S = "v_2222222222222222"),
  topology_hash = strrep("d", 64), load_label = "end", weather_issue_cutoff = "available_until"), list(...))
kau <- function(h = 9L, m = 0L) lubridate::make_datetime(2026, 1, 15, h, m, tz = KTZ)
kkey <- function(au = kau(), versao = ek$prevcarga_forecast_versao(kparts()))
  ek$prevcarga_export_key(lubridate::ymd("2026-01-15"), au, versao, KTZ)
kexport <- function(s, ctx, key = kkey()) ek$prevcarga_export(s, key, niveis = "subsistemas", ctx = ctx)
ktree <- function(d) {
  f <- sort(list.files(d, recursive = TRUE, all.files = TRUE), method = "radix")
  stats::setNames(vapply(f, function(x) digest::digest(file = file.path(d, x), algo = "sha256"), ""), f)
}

test_that("[AC4] caminho: regex, ASCII, sem ':' nem barra invertida, string unico", {
  k <- kkey()
  expect_match(k$rel, "^forecasts/[0-9]{4}-[0-9]{2}-[0-9]{2}/[0-9]{8}T[0-9]{4}/[0-9a-f]{12,64}$")
  expect_identical(sub("/[0-9a-f]{12}$", "", k$rel), "forecasts/2026-01-15/20260115T0900")
  expect_true(all(utf8ToInt(k$rel) < 128L))
  expect_false(grepl(":|\\\\", k$rel))
  expect_match(k$run_id, "^[0-9a-f]{12}$")
  ## o string nao depende do fuso da sessao: mesmo instante, mesmo tz de carga
  expect_identical(kkey(lubridate::with_tz(kau(), "UTC"))$rel, k$rel)
  ## CONTROL: outro tz de carga muda a hora do caminho
  expect_identical(ek$prevcarga_export_key(lubridate::ymd("2026-01-15"), kau(), "abcdef012345", "UTC")$rel,
                   "forecasts/2026-01-15/20260115T1200/abcdef012345")
})

test_that("[AC5] versao: CONTROL por k-hat, clima e load_label; FORBIDDEN ordem invariante; run_id estavel", {
  v0 <- ek$prevcarga_forecast_versao(kparts())
  expect_match(v0, "^[0-9a-f]{12}$")
  expect_identical(ek$prevcarga_forecast_versao(kparts()), v0)
  ## CONTROL: so o v_<hash> do perfil de um subsistema / so o clima / so o load_label / membro / pesos / topologia / pacote
  chg <- list(khat = c(SECO = "v_1111111111111111", S = "v_9999999999999999"),
              weather_issue_cutoff = "origin", load_label = "start",
              members = c(`random-forest` = "v_aaaaaaaaaaaaaaaa", lgbm = "v_eeeeeeeeeeeeeeee"),
              combina_weights = "v_ffffffffffffffff", topology_hash = strrep("e", 64), package = "0.2.0")
  for (nm in names(chg)) expect_false(identical(ek$prevcarga_forecast_versao(do.call(kparts, chg[nm])), v0), info = nm)
  ## FORBIDDEN: ordem de membros/subsistemas nao altera versao nem run_id
  p2 <- kparts(members = c(lgbm = "v_bbbbbbbbbbbbbbbb", `random-forest` = "v_aaaaaaaaaaaaaaaa"),
               khat = c(S = "v_2222222222222222", SECO = "v_1111111111111111"))
  expect_identical(ek$prevcarga_forecast_versao(p2), v0)
  expect_identical(kkey(versao = ek$prevcarga_forecast_versao(p2))$run_id, kkey()$run_id)
  expect_identical(kkey()$run_id, kkey()$run_id)
  ## CONTROL: outra chave, outro run_id
  expect_false(identical(kkey(kau(9L, 30L))$run_id, kkey()$run_id))
  expect_error(ek$prevcarga_forecast_versao(kparts(khat = NULL)[setdiff(names(kparts()), "khat")]), class = "prevcarga_error")
})

test_that("[AC1] mesma chave e mesmo conteudo: no-op, bytes e gerado_em originais, sem staging", {
  s <- kmake_scratch(); ctx <- kstate("2026-01-15T12:01:00Z"); k <- kkey()
  r1 <- kexport(s, ctx, k)
  expect_false(r1$ja_publicado)
  expect_identical(r1$saida, file.path(ctx$state_dir, k$rel))
  before <- ktree(r1$saida)
  ctx$generated_at <- "2026-02-01T00:00:00Z"
  r2 <- kexport(s, ctx, k)
  expect_true(r2$ja_publicado)
  expect_identical(ktree(r1$saida), before)
  expect_match(readLines(file.path(r1$saida, "manifest.json")), "2026-01-15T12:01:00Z", all = FALSE)
  expect_false(any(grepl("^[.]tmp-", list.files(dirname(r1$saida), all.files = TRUE))))
})

test_that("[AC2] +1 meia hora: nova chave 0930 e a 0900 permanece byte-identica", {
  s <- kmake_scratch(); ctx <- kstate()
  r1 <- kexport(s, ctx, kkey(kau(9L, 0L)))
  before <- ktree(r1$saida)
  r2 <- kexport(s, ctx, kkey(kau(9L, 30L)))
  expect_false(r2$ja_publicado)
  expect_match(r2$saida, "forecasts/2026-01-15/20260115T0930/[0-9a-f]{12}$")
  expect_true(dir.exists(r2$saida))
  expect_identical(ktree(r1$saida), before)
  expect_false(identical(r1$saida, r2$saida))
})

test_that("[AC3] [FORBIDDEN] conflito: arquivo adulterado ou conteudo novo => integrity.export.key_conflict sem sobrescrever", {
  s <- kmake_scratch(); ctx <- kstate(); k <- kkey()
  r1 <- kexport(s, ctx, k)
  tgt <- file.path(r1$saida, "SIN.parquet")
  b <- readBin(tgt, "raw", 1e6); b[length(b) - 10L] <- as.raw(bitwXor(as.integer(b[length(b) - 10L]), 1L)); writeBin(b, tgt)
  before <- ktree(r1$saida)
  cond <- tryCatch(kexport(s, ctx, k), prevcarga_error = function(e) e)
  expect_identical(cond$id, "export.key_conflict"); expect_identical(cond$class_name, "integrity")
  expect_identical(ek$prevcarga_exit_codes[["integrity"]], 9L)
  expect_identical(cond$details$path, "SIN.parquet")
  expect_identical(ktree(r1$saida), before)
  expect_false(any(grepl("^[.]tmp-", list.files(dirname(r1$saida), all.files = TRUE))))
  ## conteudo novo diferente na mesma chave
  s2 <- kmake_scratch(seed = 9L); ctx2 <- kstate(); r3 <- kexport(s, ctx2, k); before3 <- ktree(r3$saida)
  cond2 <- tryCatch(kexport(s2, ctx2, k), prevcarga_error = function(e) e)
  expect_identical(cond2$id, "export.key_conflict"); expect_identical(cond2$details$path, "SIN.parquet")
  expect_identical(ktree(r3$saida), before3)
  ## arquivo a mais na chave tambem e conflito
  writeLines("x", file.path(r3$saida, "extra.txt"))
  cond3 <- tryCatch(kexport(s, ctx2, k), prevcarga_error = function(e) e)
  expect_identical(cond3$details$path, "extra.txt")
  ## CONTROL: chave intacta e mesma fonte => no-op
  expect_true(kexport(s, kstate(), k)$ja_publicado == FALSE)
  unlink(file.path(r3$saida, "extra.txt"))
  expect_true(kexport(s, ctx2, k)$ja_publicado)
})

test_that("catalogo distribuido: export.key_conflict em errors/export_key.tsv, classe integrity, ASCII", {
  p <- file.path(pc_root(), "scripts", "lib", "errors", "export_key.tsv")
  d <- utils::read.delim(p, stringsAsFactors = FALSE, quote = "")
  expect_identical(names(d), c("id", "class", "message", "hint"))
  expect_identical(d$id, "export.key_conflict"); expect_identical(d$class, "integrity")
  expect_true(all(utf8ToInt(paste(readLines(p, warn = FALSE), collapse = "")) < 128L))
  expect_false(grepl("\r", rawToChar(readBin(p, "raw", 1e5)), fixed = TRUE))
})
