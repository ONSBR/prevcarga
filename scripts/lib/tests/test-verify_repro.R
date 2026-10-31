## test-verify_repro.R -- verify_repro.R + verbo `verificar` (M8.7). In-process: entregas nativas montadas em tempdir() (parquet LONG via
## arrow so no teste; manifest.json no formato do M6.2; run_manifest.json escrito direto, com a identidade que cada braco quer).
## Cada FORBIDDEN tem o seu CONTROL. Sem env vars; datas por lubridate com tz explicito.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
E <- pc_source_lib(c("errors", "schema_long", "provenance", "evaluation", "exports", "verify_repro"))
V <- new.env(parent = E)
sys.source(file.path(pc_root(), "scripts", "lib", "cli", "verificar.R"), envir = V)

VR_TZ <- "Etc/GMT+3"
VR_FILES <- c("SIN.parquet", "subsistemas/SS1.parquet", "membros/M1/A1.parquet")
vr_hex <- function(ch, n = 64L) strrep(ch, n)

vr_df <- function(area, k, tz = VR_TZ) {
  t0 <- lubridate::ymd_hm("2026-01-15 00:00", tz = tz)
  data.frame(area = area, forecast_origin = "2026-01-15", target_datetime = t0 + lubridate::minutes(30L * (1:6)),
             lead = 1:6, y_pred = 1000 + 10 * (1:6) + k, y_true = NA_real_, d_plus = 0L, granularity_min = 30L,
             stringsAsFactors = FALSE)
}

## Monta a entrega em `dir`. mutate = list(<path> = function(df) df) altera o parquet ANTES do manifest (entrega coerente).
## id = identidade do run_manifest.json (NULL em um campo = omitido). files = arquivos presentes (a lista do manifest os acompanha).
vr_delivery <- function(dir, files = VR_FILES, mutate = list(), id = list(), versao = "0123456789ab") {
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ents <- lapply(seq_along(files), function(i) {
    p <- files[[i]]
    df <- vr_df(sub("\\.parquet$", "", basename(p)), i)
    if (!is.null(mutate[[p]])) df <- mutate[[p]](df)
    dir.create(dirname(file.path(dir, p)), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(df, file.path(dir, p))
    list(path = p, sha256 = digest::digest(file = file.path(dir, p), algo = "sha256"))
  })
  man <- list(produto = "prevcarga-forecast", origem = "2026-01-15", available_until = "2026-01-15T09:00", timezone = VR_TZ,
              versao = versao, arquivos = ents)
  writeLines(as.character(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE)), file.path(dir, "manifest.json"))
  rm <- list(data_hash = vr_hex("a"), bundle_id = vr_hex("b"), model_versions = list(lgbm = "v_0123456789abcdef"),
             runtime_id = vr_hex("c"), platform = "x86_64-pc-linux-gnu", load_tz = VR_TZ, origin = "2026-01-15",
             available_until = "2026-01-15T09:00", versao = versao)
  for (n in names(id)) rm[n] <- list(id[[n]])
  writeLines(as.character(jsonlite::toJSON(rm, auto_unbox = TRUE, null = "null")), file.path(dir, "run_manifest.json"))
  invisible(dir)
}

vr_dirs <- function() {
  base <- withr::local_tempdir("pc-vr-", .local_envir = parent.frame())
  list(s = file.path(base, "saida"), r = file.path(base, "ref"), base = base)
}
vr_run <- function(d, ..., ctx = list(state_dir = file.path(d$base, "state"))) E$prevcarga_verify_repro(d$s, d$r, ..., ctx = ctx)
vr_err <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
bump <- function(delta, rel = FALSE, row = 1L) function(df) {
  df$y_pred[[row]] <- if (rel) df$y_pred[[row]] * (1 + delta) else df$y_pred[[row]] + delta
  df
}

test_that("[FORBIDDEN] trilho mesma-plataforma: +9e-6 passa, +1,1e-5 reprova (exit 12) [CONTROL: sem delta, delta 0]", {
  d <- vr_dirs(); vr_delivery(d$r)
  vr_delivery(d$s); r0 <- vr_run(d)
  expect_identical(r0$resultado, "PASS"); expect_identical(r0$trilho, "mesma-plataforma"); expect_equal(r0$tol, 1e-5)
  expect_true(all(r0$arquivos$pior_delta == 0)); expect_identical(nrow(r0$arquivos), 3L)
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(9e-6)))
  r1 <- vr_run(d); expect_identical(r1$resultado, "PASS"); expect_true(all(r1$arquivos$n_fora == 0L))
  expect_equal(r1$arquivos$pior_delta[r1$arquivos$path == "SIN.parquet"], 9e-6, tolerance = 1e-6)
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(1.1e-5)))
  e <- vr_err(vr_run(d))
  expect_identical(e$id, "repro.value_out_of_tol"); expect_identical(e$exit_code, 12L)
  a <- e$result$arquivos
  expect_identical(a$n_fora[a$path == "SIN.parquet"], 1L); expect_equal(a$pior_delta[a$path == "SIN.parquet"], 1.1e-5, tolerance = 1e-6)
  expect_true(all(a$n_fora[a$path != "SIN.parquet"] == 0L))
})

test_that("entrega sincronizada do bucket (SHA256SUMS na raiz, do release-pack) passa a integridade; CONTROL: outro arquivo nao listado => exit 9 (M8.9)", {
  d <- vr_dirs(); vr_delivery(d$r); vr_delivery(d$s)
  writeLines("0000  SIN.parquet", file.path(d$r, "SHA256SUMS"))                    # indice de transporte: nao e conteudo da entrega
  r <- vr_run(d)
  expect_identical(r$resultado, "PASS")
  writeLines("x", file.path(d$r, "LEIAME.txt"))                                    # CONTROL: arquivo qualquer fora do manifest continua reprovando
  e <- vr_err(vr_run(d))
  expect_identical(e$id, "integrity.manifest_sha"); expect_identical(e$exit_code, 9L)
  expect_identical(unlist(e$details$nao_listados), "LEIAME.txt")
})

test_that("[FORBIDDEN] trilho entre-plataformas: +0,09% passa, +0,11% reprova [CONTROL: mesma-plataforma reprova o mesmo +0,09%]", {
  d <- vr_dirs()
  vr_delivery(d$r, id = list(platform = "x86_64-w64-mingw32"))
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(0.0009, rel = TRUE)))
  r <- vr_run(d)
  expect_identical(r$trilho, "entre-plataformas"); expect_identical(r$resultado, "PASS"); expect_equal(r$tol, 1e-3)
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(0.0011, rel = TRUE)))
  e <- vr_err(vr_run(d)); expect_identical(e$id, "repro.value_out_of_tol"); expect_identical(e$exit_code, 12L)
  ## CONTROL: o mesmo +0,09% na mesma plataforma (trilho estrito) reprova
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(0.0009, rel = TRUE)))
  vr_delivery(d$r)
  expect_identical(vr_err(vr_run(d))$id, "repro.value_out_of_tol")
})

test_that("ref = 0 no trilho entre-plataformas usa o piso absoluto 1e-5", {
  w <- E$pc_vr_within
  tol <- list(trilho = "entre-plataformas")
  expect_identical(w(c(9e-6, 1.1e-5), c(0, 0), tol), c(TRUE, FALSE))
  expect_identical(w(c(0.9, 1.1), c(1000, 1000), tol), c(TRUE, FALSE))   # CONTROL: ref != 0 e relativo
})

test_that("--tol foi removido: config.verify_tol_removed (exit 8) sem tocar nos arquivos", {
  d <- vr_dirs(); vr_delivery(d$r); vr_delivery(d$s)
  args <- list(`--saida` = d$s, `--referencia` = d$r, `--tol` = "50", `--oficial` = FALSE)
  e <- vr_err(V$cmd_verificar(list(format = "json", state_dir = d$base), args))
  expect_identical(e$id, "config.verify_tol_removed"); expect_identical(e$exit_code, 8L)
  ok <- V$cmd_verificar(list(format = "json", state_dir = d$base), list(`--saida` = d$s, `--referencia` = d$r, `--oficial` = FALSE))
  expect_identical(ok$resultado, "PASS")                                   # CONTROL: sem --tol roda
})

test_that("[FORBIDDEN] integridade quebrada: exit 9 citando o arquivo e o comparador NAO e chamado [CONTROL: integra chama 1x por par]", {
  d <- vr_dirs(); vr_delivery(d$r); vr_delivery(d$s)
  calls <- 0L
  spy <- function(s, r, tol, ctx) { calls <<- calls + 1L; data.frame(path = NA_character_, pior_delta = 0, n_fora = 0L) }
  r <- vr_run(d, compare = spy)
  expect_identical(calls, 3L); expect_identical(r$resultado, "PASS")                    # CONTROL
  calls <- 0L
  f <- file.path(d$r, "SIN.parquet"); b <- readBin(f, "raw", file.size(f)); b[[20L]] <- as.raw(bitwXor(as.integer(b[[20L]]), 1L))
  writeBin(b, f)
  e <- vr_err(vr_run(d, compare = spy))
  expect_identical(e$id, "integrity.manifest_sha"); expect_identical(e$exit_code, 9L); expect_identical(e$where, "SIN.parquet")
  expect_identical(calls, 0L)
  ## a saida adulterada tambem; arquivo nao listado tambem
  vr_delivery(d$r); writeLines("x", file.path(d$s, "lixo.txt"))
  e2 <- vr_err(vr_run(d, compare = spy)); expect_identical(e2$id, "integrity.manifest_sha"); expect_identical(e2$where, "lixo.txt")
  expect_identical(calls, 0L)
})

test_that("identidade: bundle_id, model_versions e data_hash nulo => exit 2 com o campo; runtime_id diferente => PASS com aviso", {
  d <- vr_dirs(); vr_delivery(d$r)
  arms <- list(bundle_id = list(bundle_id = vr_hex("d")),
               model_versions = list(model_versions = list(lgbm = "v_ffffffffffffffff")),
               data_hash = list(data_hash = NULL))
  for (campo in names(arms)) {
    vr_delivery(d$s, id = arms[[campo]])
    e <- vr_err(vr_run(d))
    expect_identical(e$id, "contract.identity_mismatch", info = campo); expect_identical(e$exit_code, 2L)
    expect_identical(e$details$campo, campo)
  }
  vr_delivery(d$s, id = list(runtime_id = vr_hex("e")))
  r <- vr_run(d)
  expect_identical(r$resultado, "PASS"); expect_identical(r$avisos, "repro.runtime_id_differs")
  vr_delivery(d$s); expect_length(vr_run(d)$avisos, 0L)                               # CONTROL: identica, sem aviso
})

test_that("pareamento: arquivo ausente/a mais => exit 2 com o path; linha a mais ou target deslocado => repro.key_mismatch (exit 12)", {
  d <- vr_dirs(); vr_delivery(d$r)
  vr_delivery(d$s, files = VR_FILES[-2L])
  e <- vr_err(vr_run(d)); expect_identical(e$id, "contract.pairing"); expect_identical(e$exit_code, 2L)
  expect_identical(unlist(e$details$ausentes), "subsistemas/SS1.parquet")
  vr_delivery(d$s, files = c(VR_FILES, "folhas/Z9.parquet"))
  e <- vr_err(vr_run(d)); expect_identical(e$id, "contract.pairing"); expect_identical(unlist(e$details$a_mais), "folhas/Z9.parquet")
  ## --sem-membros ignora membros/ nos dois lados
  vr_delivery(d$s, files = VR_FILES[1:2]); r <- vr_run(d, sem_membros = TRUE)
  expect_identical(sort(r$arquivos$path), sort(VR_FILES[1:2]))
  expect_identical(vr_err(vr_run(d))$id, "contract.pairing")                              # CONTROL: sem a flag falha
  ## linha a mais
  more <- function(df) rbind(df, transform(df[6L, ], lead = 7L, target_datetime = target_datetime + lubridate::minutes(30L)))
  vr_delivery(d$s, mutate = list(`SIN.parquet` = more))
  e <- vr_err(vr_run(d)); expect_identical(e$id, "repro.key_mismatch"); expect_identical(e$exit_code, 12L)
  ## o join usa target_datetime (= interval_end): deslocar 30 min com os mesmos valores quebra a chave
  shift <- function(df) { df$target_datetime <- df$target_datetime + lubridate::minutes(30L); df }
  vr_delivery(d$s, mutate = list(`SIN.parquet` = shift))
  expect_identical(vr_err(vr_run(d))$id, "repro.key_mismatch")
  vr_delivery(d$s); expect_identical(vr_run(d)$resultado, "PASS")                          # CONTROL
})

test_that("--oficial: referencia resolvida pela chave; ausente => exit 13 com o caminho; trilho forcado => exit 8", {
  d <- vr_dirs()
  vr_delivery(d$s)
  st <- file.path(d$base, "state"); ctx <- list(state_dir = st, format = "json")
  want <- file.path(st, "forecasts", "2026-01-15", "20260115T0900", "0123456789ab")
  e <- vr_err(V$cmd_verificar(ctx, list(`--saida` = d$s, `--oficial` = TRUE)))
  expect_identical(e$id, "state.official_forecast_missing"); expect_identical(e$exit_code, 13L); expect_identical(e$where, want)
  vr_delivery(want)
  expect_identical(E$pc_vr_resolve_oficial(d$s, ctx), want)
  r <- V$cmd_verificar(ctx, list(`--saida` = d$s, `--oficial` = TRUE))
  expect_identical(r$resultado, "PASS"); expect_identical(r$trilho, "mesma-plataforma"); expect_true(r$oficial)
  e2 <- vr_err(V$cmd_verificar(ctx, list(`--saida` = d$s, `--oficial` = TRUE, `--trilho` = "entre-plataformas")))
  expect_identical(e2$id, "config.verify_trilho_forced"); expect_identical(e2$exit_code, 8L)
  both <- vr_err(V$cmd_verificar(ctx, list(`--saida` = d$s, `--oficial` = TRUE, `--referencia` = want)))
  expect_identical(both$id, "config.usage")
  ## CONTROL: entre-plataformas valido quando as plataformas diferem
  vr_delivery(want, id = list(platform = "x86_64-w64-mingw32"))
  expect_identical(V$cmd_verificar(ctx, list(`--saida` = d$s, `--oficial` = TRUE))$trilho, "entre-plataformas")
})

vr_limiares <- function(path, lim = 5.0, tamper = FALSE) {
  l <- list(list(alvo = "SECO", limiar_mw = lim), list(alvo = "SIN", limiar_mw = 10.0))
  y <- list(limiares = l, assinado_por = "Gabriel", data = "2026-10-23")
  y$sha256 <- digest::digest(yaml::as.yaml(yaml::yaml.load(yaml::as.yaml(l))), algo = "sha256", serialize = FALSE)
  if (tamper) y$limiares[[1L]]$limiar_mw <- lim + 100
  yaml::write_yaml(y, path)
  path
}

test_that("--alvo/--limiares assinado: abaixo do limiar passa, acima reprova (12), adulterado => exit 9", {
  d <- vr_dirs(); vr_delivery(d$r)
  lf <- vr_limiares(file.path(d$base, "lim.yaml"))
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(5.0 - 0.01)))
  r <- vr_run(d, alvo = "SECO", limiares = lf)
  expect_identical(r$resultado, "PASS"); expect_identical(r$origem_tol, "alvo:SECO"); expect_equal(r$tol, 5)
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(5.0 + 0.01)))
  e <- vr_err(vr_run(d, alvo = "SECO", limiares = lf)); expect_identical(e$id, "repro.value_out_of_tol"); expect_identical(e$exit_code, 12L)
  tf <- vr_limiares(file.path(d$base, "lim-adulterado.yaml"), tamper = TRUE)
  e <- vr_err(vr_run(d, alvo = "SECO", limiares = tf))
  expect_identical(e$id, "integrity.limiares_signature"); expect_identical(e$exit_code, 9L)
  expect_identical(vr_err(vr_run(d, alvo = "INEXISTENTE", limiares = lf))$id, "config.usage")
  expect_identical(vr_err(vr_run(d, alvo = "SECO"))$id, "config.usage")
})

test_that("trilho invalido e pureza: verify_repro.R nao le env, relogio nem rede", {
  d <- vr_dirs(); vr_delivery(d$r); vr_delivery(d$s)
  expect_identical(vr_err(vr_run(d, trilho = "tanto-faz"))$id, "config.usage")
  src <- paste(readLines(file.path(pc_root(), "scripts", "lib", "verify_repro.R")), collapse = "\n")
  src <- gsub("(?m)^\\s*##.*$", "", src, perl = TRUE)
  expect_false(grepl("Sys\\.getenv|Sys\\.time|Sys\\.Date|download|curl|httr|system2?\\(", src))
})

test_that("catalogo distribuido: ids do verbo, classes e exits do ADR-0040", {
  cat <- E$prevcarga_errors_load()
  ids <- c("config.verify_tol_removed" = 8L, "config.verify_trilho_forced" = 8L, "contract.identity_mismatch" = 2L,
           "contract.pairing" = 2L, "integrity.artifact_sha" = 9L, "integrity.limiares_signature" = 9L,
           "integrity.manifest_sha" = 9L, "repro.key_mismatch" = 12L, "repro.runtime_id_differs" = 12L,
           "repro.value_out_of_tol" = 12L, "state.artifact_missing" = 13L, "state.official_forecast_missing" = 13L)
  for (i in names(ids)) expect_identical(E$prevcarga_error_lookup(i)$class, names(E$prevcarga_exit_codes)[E$prevcarga_exit_codes == ids[[i]]], info = i)
  expect_true(all(names(ids) %in% cat$id))
})

test_that("[PROCESSO] verbo descoberto pela fachada: --tol => exit 8; PASS e FAIL com envelope JSON; --corte => config.usage", {
  root <- pc_install_copy()
  d <- vr_dirs(); vr_delivery(d$r); vr_delivery(d$s)
  j <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
  r <- pc_cli(root, c("verificar", paste0("--saida=", d$s), paste0("--referencia=", d$r), "--tol", "50", "--format=json"))
  expect_identical(r$status, 8L); expect_identical(j(r)$error$id, "config.verify_tol_removed")
  r <- pc_cli(root, c("verificar", paste0("--saida=", d$s), paste0("--referencia=", d$r), "--format=json"))
  expect_identical(r$status, 0L); expect_identical(j(r)$result$resultado, "PASS")
  expect_identical(length(j(r)$result$arquivos), 3L)
  vr_delivery(d$s, mutate = list(`SIN.parquet` = bump(1.1e-5)))
  r <- pc_cli(root, c("verificar", paste0("--saida=", d$s), paste0("--referencia=", d$r), "--format=json"))
  expect_identical(r$status, 12L); expect_identical(j(r)$error$id, "repro.value_out_of_tol")
  expect_identical(j(r)$result$arquivos[[1L]]$n_fora, 1L)
  r <- pc_cli(root, c("verificar", paste0("--saida=", d$s), paste0("--referencia=", d$r), "--corte=2026-01-15"))
  expect_identical(r$status, 8L)
  txt <- pc_cli(root, c("verificar", paste0("--saida=", d$s), paste0("--referencia=", d$r)))
  expect_identical(txt$status, 12L); pc_assert_ascii(txt$raw)
})
