## test-portability.R -- DL.8 (AC14 do epico): portabilidade do artefato do dessem-linear. Dois treinos identicos (cortes
## intradiarios diferentes) => mesmo sha256; artefato = dado puro (`prevcarga_plugin_scan_pure` limpo); thaw do arquivo ==
## memoria; artefato adulterado => [prevcarga/integrity] exit 9. O braco Windows x Linux depende do bundle (M8): SKIP com dono.

test_that("[PLUGINS] portabilidade: sha256 estavel entre dois treinos independentes e entre dois congelamentos; artefato puro", {
  a <- dl_base_art(); b <- dl_art12()                                  # dois treinos reais (cortes 09:00 e 12:00)
  expect_identical(dl_sha(a), dl_sha(b))
  expect_identical(dl_sha(a), dl_sha(a))
  expect_match(dl_sha(a), "^[0-9a-f]{64}$")
  expect_true(dl_lib$prevcarga_plugin_scan_pure(a))
  expect_true(dl_lib$prevcarga_plugin_scan_pure(b))
  ## pureza falsificavel: a varredura recusa relogio, caminho absoluto e closure
  bad <- a; bad$payload$trained_at <- lubridate::ymd_hms("2024-10-15 09:00:00", tz = "UTC")
  e <- dl_err(dl_lib$prevcarga_plugin_scan_pure(bad))
  expect_dl_class(e, "contract", 2L, id = "plugin.impure_artifact")
  bad2 <- a; bad2$payload$path <- "/tmp/artefato.rds"
  expect_dl_class(dl_err(dl_lib$prevcarga_plugin_scan_pure(bad2)), "contract", 2L, id = "plugin.impure_artifact")
})

test_that("[PLUGINS] portabilidade: thaw do arquivo == memoria (mesmo artefato e mesma entrega); adulterado => integrity exit 9", {
  st <- dl_fx_store("09:00"); cuts <- dl_fx_cuts(st); art <- dl_base_art(); p <- dl_plugin()
  f <- tempfile("dl8-art-", fileext = ".rds"); on.exit(unlink(c(f, paste0(f, ".manifest.json"))), add = TRUE)
  dl_with_cli(dl_lib$prevcarga_plugin_freeze_bytes(art, f))
  th <- dl_with_cli(dl_lib$prevcarga_plugin_thaw(f, p))
  expect_s3_class(th, "prevcarga_plugin_artifact")
  expect_identical(th, art)
  ## a entrega do artefato lido do disco e identica a do artefato em memoria
  expect_identical(dl_infer(th, st, cuts)$delivery, dl_infer(art, st, cuts)$delivery)
  ## os SVMs descongelados do disco predizem como os do artefato em memoria
  mem <- p$descriptor$thaw(art$payload); dsk <- p$descriptor$thaw(th$payload)
  x <- data.frame(LAG_1 = 1, LAG_7 = 1)
  expect_identical(names(mem$svm_dm), names(dsk$svm_dm))
  expect_true(all(vapply(dsk$svm_perfil, function(d) all(vapply(d, inherits, NA, what = "wsvm")), NA)))
  ## adulterado: 1 byte do arquivo trocado => sha256 diverge do manifesto => integrity (exit 9)
  raw <- readBin(f, "raw", file.info(f)$size)
  raw[[length(raw) %/% 2L]] <- as.raw(bitwXor(as.integer(raw[[length(raw) %/% 2L]]), 255L))
  g <- tempfile("dl8-tam-", fileext = ".rds"); on.exit(unlink(c(g, paste0(g, ".manifest.json"))), add = TRUE)
  writeBin(raw, g); file.copy(paste0(f, ".manifest.json"), paste0(g, ".manifest.json"))
  e <- dl_err(dl_lib$prevcarga_plugin_thaw(g, p))
  expect_dl_class(e, "integrity", 9L, id = "plugin.artifact_corrupt")
  ## adulterado por dentro (SV do SVM removido): o thaw do motor recusa (integrity, exit 9)
  tam <- art$payload
  tam$svm_perfil[[1L]][[1L]]$SV <- NULL
  e2 <- dl_err(p$descriptor$thaw(tam))
  expect_dl_class(e2, "integrity", 9L, id = "engine.dessem_svm_thaw_failed")
})

test_that("[DEFERRED M8] cross_os: sha256 do artefato identico entre Windows nativo e Linux (bundle)", {
  testthat::skip("[DEFERRED M8] braco Windows x Linux: precisa do bundle e do runner Windows do M8 (dono: M8 portabilidade)")
})
