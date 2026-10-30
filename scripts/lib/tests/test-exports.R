## test-exports.R -- exports.R (M6.2): copia de bytes, manifest, niveis-teto, atomicidade e pureza. Cada FORBIDDEN tem o seu CONTROL.
## Tudo em tempdir; sem env vars; scratch de fixture montado aqui (parquet via arrow SO no teste; run.json com arquivos[] como o reconcile grava).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
ex <- pc_source_lib(c("errors", "schema_long", "exports"))

SS <- c("SS1", "SS2"); COVERED <- c("A1", "A2", "B1"); STRAY <- "Z9"
write_leaf <- function(path, area, n = 5L, seed = 1L) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data.frame(area = area, y_pred = seed * seq_len(n) + 0.5), path, compression = "snappy")
  invisible(n)
}
## arquivos[] do run.json: {path, area, linhas, sha256} de cada parquet do scratch (como prevcarga_reconcile_write)
register_files <- function(s, rows) {
  lapply(names(rows), function(rel) list(path = rel, area = rows[[rel]]$area, linhas = rows[[rel]]$n,
                                         sha256 = digest::digest(file = file.path(s, rel), algo = "sha256")))
}
## scratch como o forecast deixa: run.json + SIN + subsistemas + folhas cobertas + 1 folha solta + membros
make_scratch <- function(with_stray = TRUE, with_agg = TRUE, n_sin = 7L, unregistered = character()) {
  s <- withr::local_tempdir("pc-scratch-", .local_envir = parent.frame())
  rows <- list()
  put <- function(rel, area, n, seed) { write_leaf(file.path(s, rel), area, n, seed); rows[[rel]] <<- list(area = area, n = n) }
  if (with_agg) {
    put("SIN.parquet", "SIN", n_sin, 1L)
    for (ss in SS) put(paste0("subsistemas/", ss, ".parquet"), ss, 6L, 2L)
  }
  for (a in COVERED) put(paste0("folhas/", a, ".parquet"), a, 4L, 3L)
  if (with_stray) put(paste0("folhas/", STRAY, ".parquet"), STRAY, 3L, 4L)
  put("membros/M1/A1.parquet", "A1", 2L, 5L)
  rows <- rows[setdiff(names(rows), unregistered)]
  run <- list(origem = "2026-01-15", available_until = "2026-01-15T09:00",
              available_until_por_area = list(A1 = "2026-01-15T09:00"), timezone = "Etc/GMT+3", load_label = "end",
              weather_issue_cutoff = "available_until",
              khat = lapply(SS, function(ss) list(ss = ss, versao = "v_0123456789abcdef", khat_origin = "2026-01-15",
                                                  extra = "ignorado")),
              covered = as.list(COVERED), stray = if (with_stray) list(STRAY) else list(),
              closed = if (with_agg) as.list(SS) else list(), sin_closed = with_agg,
              members = list(M1 = list("A1")), arquivos = register_files(s, rows), versao = "abc123", version = "0.1.0", runtime_id = "rt-1")
  writeLines(as.character(jsonlite::toJSON(run, auto_unbox = TRUE, pretty = TRUE)), file.path(s, "run.json"))
  s
}
state_ctx <- function(at = "2026-01-15T12:01:00Z") {
  d <- withr::local_tempdir("pc-state-", .local_envir = parent.frame())
  list(state_dir = d, generated_at = at)
}
do_export <- function(s, ctx, id = "r1", niveis = "subsistemas", membros = FALSE, fn = ex$prevcarga_export)
  fn(s, list(rel = file.path("exports", id), run_id = id), niveis = niveis, incluir_membros = membros, ctx = ctx)
manifest <- function(r) jsonlite::read_json(file.path(r$saida, "manifest.json"), simplifyVector = FALSE)
paths <- function(r) vapply(r$arquivos, function(a) a$path, "")

test_that("[FORBIDDEN] bytes entregues == bytes da fonte; reescrever muda o sha256 [CONTROL]", {
  s <- make_scratch(); ctx <- state_ctx()
  r <- do_export(s, ctx)
  for (a in r$arquivos) {
    expect_identical(a$sha256, digest::digest(file = file.path(s, a$path), algo = "sha256"))
    expect_identical(a$sha256, digest::digest(file = file.path(r$saida, a$path), algo = "sha256"))
  }
  expect_false(any(nzchar(Sys.readlink(file.path(r$saida, paths(r))))))   # sem symlink
  ## CONTROL: variante que reserializa (arrow::write_parquet) NAO preserva o sha256
  src <- file.path(s, "SIN.parquet"); re <- tempfile(fileext = ".parquet")
  arrow::write_parquet(arrow::read_parquet(src), re, compression = "zstd")
  expect_false(identical(digest::digest(file = re, algo = "sha256"), digest::digest(file = src, algo = "sha256")))
  ## CONTROL: fonte com 1 linha a mais muda sha256 e linhas
  s2 <- make_scratch(n_sin = 8L)
  r2 <- do_export(s2, state_ctx())
  e1 <- r$arquivos[[which(paths(r) == "SIN.parquet")]]; e2 <- r2$arquivos[[which(paths(r2) == "SIN.parquet")]]
  expect_identical(e1$linhas, 7L); expect_identical(e2$linhas, 8L)
  expect_false(identical(e1$sha256, e2$sha256))
})

test_that("manifest: campos copiados do run.json, deterministico exceto gerado_em, sem o proprio manifest", {
  s <- make_scratch()
  c1 <- state_ctx("2026-01-15T12:01:00Z"); c2 <- state_ctx("2026-01-16T08:00:00Z")
  r1 <- do_export(s, c1); r2 <- do_export(s, c2)
  m1 <- manifest(r1); m2 <- manifest(r2)
  expect_named(m1, c("produto", "origem", "available_until", "available_until_por_area", "timezone", "load_label",
                     "weather_issue_cutoff", "khat", "versao", "version", "runtime_id", "niveis", "schema_long",
                     "gerado_em", "arquivos"))
  expect_identical(m1$origem, "2026-01-15"); expect_identical(m1$timezone, "Etc/GMT+3")
  expect_identical(m1$khat[[1L]], list(ss = "SS1", versao = "v_0123456789abcdef", khat_origin = "2026-01-15"))
  expect_identical(unlist(m1$schema_long), ex$prevcarga_schema_long)
  expect_identical(names(m1$arquivos[[1L]]), c("path", "nivel", "area", "linhas", "sha256"))
  expect_false("manifest.json" %in% vapply(m1$arquivos, function(a) a$path, ""))
  expect_identical(m1$arquivos[[1L]]$linhas, r1$arquivos[[1L]]$linhas)
  expect_identical(m1$arquivos[[which(paths(r1) == "subsistemas/SS1.parquet")]]$linhas, 6L)
  d <- setdiff(names(m1), "gerado_em")
  expect_identical(m1[d], m2[d])
  expect_false(identical(m1$gerado_em, m2$gerado_em))          # CONTROL: gerado_em e o unico que muda
  b1 <- readBin(file.path(r1$saida, "manifest.json"), "raw", 1e6)
  expect_false(as.raw(13L) %in% b1)                             # LF, nunca CRLF
  expect_identical(b1[length(b1)], as.raw(10L))
  expect_true(all(b1 < as.raw(128L)))                           # ASCII (UTF-8 sem escape)
  expect_match(rawToChar(b1), "\n  \"produto\"", fixed = TRUE)  # indent 2
  ## so o gerado_em difere nos bytes
  b2 <- readBin(file.path(r2$saida, "manifest.json"), "raw", 1e6)
  expect_identical(length(b1), length(b2))
  expect_identical(sub("\"gerado_em\": \"[^\"]*\"", "", rawToChar(b1)), sub("\"gerado_em\": \"[^\"]*\"", "", rawToChar(b2)))
})

test_that("niveis-teto: sin, subsistemas, tudo e membros; folha solta sempre entregue", {
  s <- make_scratch()
  p <- function(niveis, membros = FALSE) sort(paths(do_export(s, state_ctx(), niveis = niveis, membros = membros)))
  expect_identical(p("sin"), sort(c("SIN.parquet", "folhas/Z9.parquet")))
  expect_identical(p("subsistemas"), sort(c("SIN.parquet", "subsistemas/SS1.parquet", "subsistemas/SS2.parquet", "folhas/Z9.parquet")))
  all <- p("tudo")
  expect_true(all(paste0("folhas/", COVERED, ".parquet") %in% all))
  expect_false(any(grepl("^membros/", all)))                     # CONTROL: membros so com o flag
  expect_true("membros/M1/A1.parquet" %in% p("tudo", membros = TRUE))
  expect_false(any(grepl("^folhas/(A1|A2|B1)", p("subsistemas"))))
  expect_error(do_export(s, state_ctx(), niveis = "x"), class = "prevcarga_error")
})

test_that("nothing_to_deliver: aborta exit 13 sem criar chave nem staging", {
  s <- make_scratch(with_stray = FALSE, with_agg = FALSE)       # nada alem de folhas cobertas
  ctx <- state_ctx()
  cond <- tryCatch(do_export(s, ctx, niveis = "sin"), prevcarga_error = function(e) e)
  expect_identical(cond$id, "state.nothing_to_deliver"); expect_identical(cond$class_name, "state")
  expect_identical(ex$prevcarga_exit_codes[["state"]], 13L)
  expect_length(list.files(ctx$state_dir, all.files = TRUE, no.. = TRUE), 0L)
  ## CONTROL: com niveis = tudo ha o que entregar
  expect_length(do_export(s, ctx, niveis = "tudo")$arquivos, length(COVERED))
})

test_that("[FORBIDDEN] byte adulterado no scratch aborta integrity.copy_failed; CONTROL: scratch integro entrega", {
  s <- make_scratch(); ctx <- state_ctx()
  p <- file.path(s, "SIN.parquet")
  b <- readBin(p, "raw", 1e6); b[length(b) - 10L] <- as.raw(bitwXor(as.integer(b[length(b) - 10L]), 1L))
  writeBin(b, p)
  cond <- tryCatch(do_export(s, ctx), prevcarga_error = function(e) e)
  expect_identical(cond$id, "integrity.copy_failed"); expect_identical(cond$class_name, "integrity")
  expect_false(dir.exists(file.path(ctx$state_dir, "exports", "r1")))
  expect_length(list.files(file.path(ctx$state_dir, "exports"), all.files = TRUE, no.. = TRUE), 0L)
  expect_true(dir.exists(do_export(make_scratch(), state_ctx())$saida))
})

test_that("arquivo nao registrado em arquivos[] gera aviso e e omitido", {
  s <- make_scratch(unregistered = "folhas/Z9.parquet")
  w <- NULL
  r <- withCallingHandlers(do_export(s, state_ctx()), prevcarga_warning = function(e) { w <<- e; invokeRestart("muffleWarning") })
  expect_identical(w$id, "completude.export_file_missing")
  expect_false("folhas/Z9.parquet" %in% paths(r))
})

test_that("arquivo listado e ausente gera aviso (nunca some em silencio)", {
  s <- make_scratch(); unlink(file.path(s, "subsistemas", "SS2.parquet"))
  w <- NULL
  r <- withCallingHandlers(do_export(s, state_ctx()), prevcarga_warning = function(e) { w <<- e; invokeRestart("muffleWarning") })
  expect_identical(w$id, "completude.export_file_missing")
  expect_false("subsistemas/SS2.parquet" %in% paths(r))
})

test_that("atomicidade: falha apos o 1o arquivo nao deixa chave nem staging; sem falha a chave aparece de uma vez", {
  s <- make_scratch(); ctx <- state_ctx()
  mock <- new.env(parent = ex); n <- 0L
  mock$file.copy <- function(...) { n <<- n + 1L; if (n >= 2L) FALSE else base::file.copy(...) }
  for (nm in grep("^(prevcarga_export|[.]export_)", ls(ex, all.names = TRUE), value = TRUE)) {
    f <- get(nm, ex); if (is.function(f)) { environment(f) <- mock; assign(nm, f, mock) }
  }
  cond <- tryCatch(do_export(s, ctx, fn = mock$prevcarga_export), prevcarga_error = function(e) e)
  expect_identical(cond$id, "integrity.copy_failed"); expect_identical(cond$class_name, "integrity")
  expect_gte(n, 2L)
  expect_false(dir.exists(file.path(ctx$state_dir, "exports", "r1")))
  expect_length(list.files(file.path(ctx$state_dir, "exports"), all.files = TRUE, no.. = TRUE), 0L)
  ## CONTROL: sem falha a chave final existe, completa, e o staging sumiu
  r <- do_export(s, ctx)
  expect_true(dir.exists(r$saida))
  expect_true(file.exists(file.path(r$saida, "manifest.json")))
  expect_false(any(grepl("^[.]tmp-", list.files(dirname(r$saida), all.files = TRUE))))
})

test_that("[FORBIDDEN] pureza do exports.R: so copia bytes; CONTROL: a isca e acusada", {
  pat <- "write_parquet|paws|aws\\.s3|s3://|http|curl|Sys\\.time|Sys\\.getenv|file\\.symlink|(^|[^[:alnum:]_.])system2?\\("
  code <- readLines(file.path(pc_root(), "scripts", "lib", "exports.R"), warn = FALSE)
  code <- sub("(^|[ \t])#.*$", "\\1", code)
  expect_length(grep(pat, code, perl = TRUE), 0L)
  expect_length(grep(pat, c("x <- 1", "arrow::write_parquet(d, p)", "t <- Sys.time()"), perl = TRUE), 2L)
  expect_true(all(utf8ToInt(paste(code, collapse = "")) < 128L))
  expect_length(grep("arrow|nanoparquet|read_parquet", code), 0L)   # leitor unico: o export nao abre parquet
})
