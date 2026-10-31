## test-portability.R -- RD.5 (AC12 do epico, `artefato_puro`): portabilidade do artefato do dessem-regdin. Dois treinos
## independentes de entradas identicas (cortes intradiarios 09:00 e 12:00: o corte nao entra no artefato) => mesmo sha256;
## artefato = dado puro (`prevcarga_plugin_scan_pure` limpo: sem externalptr, environment, closure, caminho absoluto, mable
## nem campo de relogio) com os carimbos de proveniencia; freeze -> arquivo -> thaw == memoria (mesma entrega); artefato
## adulterado => [prevcarga/integrity] exit 9. CONTROL: um campo de relogio muda o sha256 e a varredura o recusa.
## O braco Windows x Linux depende do bundle (M8): SKIP com dono.

rd5_has_class <- function(x, cls) {
  if (inherits(x, cls)) return(TRUE)
  if (is.list(x) && !is.data.frame(x)) return(any(vapply(x, rd5_has_class, NA, cls = cls)))
  FALSE
}

RD5_MABLE <- c("mdl_df", "mdl_ts", "lst_mdl")

test_that("[PLUGINS] artefato_puro: sha256 estavel entre dois treinos independentes; artefato puro com carimbos e sem mable", {
  a <- rd5_art("MED"); b <- rd5_art("MED", hhmm = "12:00")            # dois treinos reais (cortes 09:00 e 12:00)
  expect_identical(rd5_sha(a), rd5_sha(b))
  expect_identical(rd5_sha(a), rd5_sha(a))                              # e entre dois congelamentos do mesmo objeto
  expect_match(rd5_sha(a), "^[0-9a-f]{64}$")
  expect_identical(a, b)
  expect_true(rd_lib$prevcarga_plugin_scan_pure(a)); expect_true(rd_lib$prevcarga_plugin_scan_pure(b))
  expect_false(rd5_has_class(a, RD5_MABLE))                             # o ARIMA nao e congelado (re-fit + fingerprint no infer)
  ## carimbos de proveniencia (do despachante e do estagio de treino)
  need <- c("train_window", "arima_fingerprint", "train_origin", "origin_day", "load_label", "timezone",
            "weather_issue_cutoff", "seed")
  have <- c(names(a$stamp), names(a$payload))
  expect_true(all(need %in% have), info = paste(setdiff(need, have), collapse = ","))
  expect_identical(a$stamp$train_origin, RD5_O); expect_identical(a$stamp$origin_day, RD5_O)
  expect_identical(a$stamp$load_label, "start"); expect_identical(a$stamp$timezone, RD_TZ)
  expect_identical(a$stamp$weather_issue_cutoff, "available_until")
  expect_identical(a$payload$train_window$end, RD5_O - 1L)
  expect_false(any(c("available_until", "trained_at", "cut", "weather_issue_limit", "mm_used") %in% c(names(a$stamp), names(a$payload))))
  ## CONTROL: a varredura de pureza apanha cada defeito (e o mable, pelo localizador de classe deste teste)
  imp <- function(mut) rd5_err(rd_lib$prevcarga_plugin_scan_pure(mut))
  bad <- a; bad$payload$trained_at <- lubridate::ymd_hms("2024-10-07 09:00:00", tz = "UTC")
  expect_rd5_class(imp(bad), "contract", 2L, id = "plugin.impure_artifact")
  bad2 <- a; bad2$payload$path <- "/tmp/artefato.rds"
  expect_rd5_class(imp(bad2), "contract", 2L, id = "plugin.impure_artifact")
  bad3 <- a; bad3$payload$fn <- local(function(x) x + hold, envir = list2env(list(hold = 1)))
  expect_rd5_class(imp(bad3), "contract", 2L, id = "plugin.impure_artifact")
  bad4 <- a; bad4$payload$env <- new.env()
  expect_rd5_class(imp(bad4), "contract", 2L, id = "plugin.impure_artifact")
  bad5 <- a; bad5$payload$ptr <- methods::new("externalptr")
  expect_rd5_class(imp(bad5), "contract", 2L, id = "plugin.impure_artifact")
  bad6 <- a; bad6$payload$arima_dm <- structure(list(model = 1), class = c("mdl_df", "tbl_df"))
  expect_true(rd5_has_class(bad6, RD5_MABLE))
})

test_that("[PLUGINS] artefato_puro: CONTROL campo de relogio muda o sha256", {
  a <- rd5_art("MED")
  clk1 <- a; clk1$payload$trained_at <- lubridate::ymd_hms("2024-10-07 09:00:00", tz = "UTC")
  clk2 <- a; clk2$payload$trained_at <- lubridate::ymd_hms("2024-10-07 09:00:01", tz = "UTC")
  expect_false(identical(rd5_sha(clk1), rd5_sha(a)))
  expect_false(identical(rd5_sha(clk1), rd5_sha(clk2)))
  f <- rd5_freeze_path()
  expect_rd5_class(rd5_err(rd_lib$prevcarga_plugin_freeze_bytes(clk1, f)), "contract", 2L, id = "plugin.impure_artifact")
  expect_false(file.exists(f))                                         # nada gravado quando a varredura recusa
})

test_that("[PLUGINS] artefato_puro: thaw do arquivo == memoria (mesmo artefato e mesma entrega); adulterado => integrity exit 9", {
  st <- rd5_store(); cu <- rd5_cuts(st, "09:00"); art <- rd5_art("MED"); p <- rd_fx_plugin()
  f <- rd5_freeze_path()
  rd_with_cli(rd_lib$prevcarga_plugin_freeze_bytes(art, f))
  expect_identical(jsonlite::read_json(paste0(f, ".manifest.json"), simplifyVector = TRUE)$sha256, rd5_sha(art))
  th <- rd_with_cli(rd_lib$prevcarga_plugin_thaw(f, p))
  expect_s3_class(th, "prevcarga_plugin_artifact")
  expect_identical(th, art)
  ## a entrega do artefato lido do disco e identica a do artefato em memoria
  expect_identical(rd5_infer(th, st, cu)$delivery, rd5_infer(art, st, cu)$delivery)
  ## os SVMs descongelados do disco sao os da memoria (plugin_thaw idempotente)
  mem <- rd_with_cli(p$descriptor$thaw(art$payload)); dsk <- rd_with_cli(p$descriptor$thaw(th$payload))
  expect_identical(names(mem$svm_perfil), names(dsk$svm_perfil))
  expect_identical(mem$svm_perfil[[1L]][[1L]]$coefs, dsk$svm_perfil[[1L]][[1L]]$coefs)
  ## adulterado: 1 byte do arquivo trocado => sha256 diverge do manifesto => integrity (exit 9)
  raw <- readBin(f, "raw", file.info(f)$size)
  k <- length(raw) %/% 2L
  raw[[k]] <- as.raw(bitwXor(as.integer(raw[[k]]), 255L))
  g <- rd5_freeze_path()
  writeBin(raw, g); file.copy(paste0(f, ".manifest.json"), paste0(g, ".manifest.json"))
  e <- rd5_err(rd_with_cli(rd_lib$prevcarga_plugin_thaw(g, p)))
  expect_rd5_class(e, "integrity", 9L, id = "plugin.artifact_corrupt")
  ## manifesto ausente tambem e integridade
  h <- rd5_freeze_path(); file.copy(f, h)
  expect_rd5_class(rd5_err(rd_with_cli(rd_lib$prevcarga_plugin_thaw(h, p))), "integrity", 9L, id = "plugin.artifact_corrupt")
  ## adulterado por dentro (SVM de perfil removido de um dia normal): o thaw do plugin recusa (integrity, exit 9)
  tam <- art$payload
  dn <- names(tam$svm_perfil)[!vapply(tam$svm_perfil, is.null, NA)][[1L]]
  tam$svm_perfil[[dn]] <- tam$svm_perfil[[dn]][1:5]
  e2 <- rd5_err(rd_with_cli(p$descriptor$thaw(tam)))
  expect_rd5_class(e2, "integrity", 9L, id = "plugin.regdin_artifact_tampered")
  expect_true(rd_ascii(conditionMessage(e2)))
})

test_that("[DEFERRED M8] cross_os: sha256 do artefato identico entre Windows nativo e Linux (bundle)", {
  testthat::skip("[DEFERRED M8] braco Windows x Linux: precisa do bundle e do runner Windows do M8 (dono: M8 portabilidade)")
})
