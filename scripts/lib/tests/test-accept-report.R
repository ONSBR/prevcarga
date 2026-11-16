## test-accept-report.R -- accept_report.R e estagios medicao/relatorio do accept_docker.sh (E8-T3).
## Fixture sintetica pequena (store de fixture_store.R, LONGs sinteticos de 2 origens x 2 areas) e docker FALSO (helper-accept.R);
## nenhum docker real, nenhuma chamada AWS, nenhuma medicao grande (as 12 origens x 8 areas sao do E8-T4).

.acr_e <- acc_report_env()
.acr_origens <- c("2023-12-31", "2024-02-11")        # 2023-12-31: o D+1 cai em 2024-01-01 (virada de ano)
.acr_areas <- c("RJ", "SP")
.acr_days <- c(lubridate::ymd("2023-12-30") + 0:3, lubridate::ymd("2024-02-10") + 0:3)
.acr_store <- acc_report_store(.acr_e, .acr_areas, .acr_days)
.acr_tz <- .acr_e$prevcarga_deck_manifest(.acr_store)$timezone$load
.acr_eps <- c(LIN_MED = 0.010, LIN_MAX = 0.012, LIN_MXMN = 0.014, LIN_UNI = 0.020, RGD_MED = 0.030, RGD_MAX = 0.032)
.acr_epsf <- function(v, m) if (v == "ref") 1.5 * .acr_eps[[m]] else .acr_eps[[m]]

## chave so na nova (alvo O+2 00:30, com verdade no store) e chave sem verdade (2030): nenhuma pode entrar na metrica
.acr_extra_new <- function(u) {
  o0 <- lubridate::force_tz(lubridate::ymd(u$origem), .acr_tz)
  data.frame(area = u$area, forecast_origin = u$origem, target_datetime = o0 + lubridate::days(2L) + lubridate::minutes(30L), lead = 999L,
             y_pred = 1e6, y_true = 999, d_plus = 1L, granularity_min = 30L, stringsAsFactors = FALSE)
}
.acr_extra_all <- function(u) {
  data.frame(area = u$area, forecast_origin = u$origem, target_datetime = lubridate::ymd_hms("2030-01-01 00:30:00", tz = .acr_tz), lead = 998L,
             y_pred = 5e5, y_true = 999, d_plus = 1L, granularity_min = 30L, stringsAsFactors = FALSE)
}

.acr_run <- function(out, store = .acr_store) {
  se <- tempfile("acr-err-")
  so <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", acc_report_script(), "--out-dir", out, "--data", store),
                                 stdout = TRUE, stderr = se))
  st <- attr(so, "status"); if (is.null(st)) st <- 0L
  txt <- paste(so, collapse = "\n")
  list(status = as.integer(st), stderr = readLines(se, warn = FALSE),
       json = if (startsWith(txt, "{")) jsonlite::fromJSON(txt, simplifyVector = FALSE) else NULL)
}
.acr_read <- function(f) { x <- data.table::fread(f, sep = "\t", colClasses = "character", na.strings = NULL); x }
.acr_num <- function(x) suppressWarnings(as.numeric(x))
.acr_nonascii <- function(f) any(as.integer(readBin(f, "raw", file.size(f))) > 127L)

## expectativa "a mao": so as chaves pareadas ref x nova (LIN_UNI so na nova), verdade fresca e prevcarga_scorecard
.acr_hand <- function(refused = character()) {
  e <- .acr_e; parts <- list()
  for (a in .acr_areas) for (o in .acr_origens) {
    act <- acc_med_long(e, .acr_store, a, o, 0); act <- act[act$d_plus == 1L, ]
    for (v in c("ref", "new")) for (md in c("dessem-linear", "dessem-regdin")) {
      unit <- sprintf("medicao-%s-backtest-%s-%s-%s", v, a, o, md)
      if (unit %in% refused) next
      for (m in acc_members_of(v, md))
        parts[[length(parts) + 1L]] <- data.frame(versao = v, membro = m, area = a, forecast_origin = o, target_datetime = act$target_datetime,
                                                  y_true = act$y_true, y_pred = act$y_true * (1 + .acr_epsf(v, m)), granularity_min = 30L,
                                                  stringsAsFactors = FALSE)
    }
  }
  d <- do.call(rbind, parts)
  k <- paste(d$membro, d$area, d$forecast_origin, as.numeric(d$target_datetime))
  both <- intersect(k[d$versao == "ref"], k[d$versao == "new"])
  d <- d[k %in% both | d$membro == "LIN_UNI", ]
  d$interval_start <- .acr_e$prevcarga_long_interval_start(d$target_datetime, 30L, .acr_tz)
  .acr_e$prevcarga_scorecard(d, by = c("versao", "membro", "area"))
}

test_that("report.tsv = prevcarga_scorecard sobre as chaves pareadas com d_plus 1 e verdade fresca; LIN_UNI com '-' na ref", {
  fx <- acc_report_out(.acr_e, .acr_store, .acr_origens, .acr_areas, .acr_epsf, extra_new = .acr_extra_new, extra_all = .acr_extra_all)
  r <- .acr_run(fx$out)
  expect_equal(r$status, 0L)
  rep <- .acr_read(file.path(fx$out, "report.tsv"))
  expect_identical(names(rep), c("membro", "area", "n_ref", "n_new", "wmape_ref", "wmape_new", "wmape_diff", "mape_ref", "mape_new", "mape_diff"))
  expect_identical(nrow(rep), 6L * length(.acr_areas))
  expect_identical(rep$membro, sort(rep$membro, method = "radix")[order(rep$membro, rep$area, method = "radix")])   # ordem radix
  hand <- .acr_hand()
  for (i in seq_len(nrow(rep))) {
    m <- rep$membro[[i]]; a <- rep$area[[i]]
    nw <- hand[hand$versao == "new" & hand$membro == m & hand$area == a, ]
    expect_equal(.acr_num(rep$wmape_new[[i]]), nw$wmape, tolerance = 1e-9, info = paste(m, a))
    expect_equal(.acr_num(rep$mape_new[[i]]), nw$mape, tolerance = 1e-9, info = paste(m, a))
    expect_identical(as.integer(rep$n_new[[i]]), as.integer(nw$n), info = paste(m, a))
    if (m == "LIN_UNI") {
      expect_identical(c(rep$wmape_ref[[i]], rep$mape_ref[[i]], rep$wmape_diff[[i]], rep$mape_diff[[i]]), rep("-", 4L), info = a)
      expect_identical(rep$n_ref[[i]], "0")
    } else {
      rf <- hand[hand$versao == "ref" & hand$membro == m & hand$area == a, ]
      expect_equal(.acr_num(rep$wmape_ref[[i]]), rf$wmape, tolerance = 1e-9, info = paste(m, a))
      expect_equal(.acr_num(rep$mape_ref[[i]]), rf$mape, tolerance = 1e-9, info = paste(m, a))
      expect_equal(.acr_num(rep$wmape_diff[[i]]), nw$wmape - rf$wmape, tolerance = 1e-9)
      expect_equal(.acr_num(rep$mape_diff[[i]]), nw$mape - rf$mape, tolerance = 1e-9)
      expect_identical(rep$n_ref[[i]], rep$n_new[[i]])
    }
  }
  ## so d_plus 1 (48 intervalos por origem e area) e a virada de ano 2023-12-31 -> 2024-01-01 entra
  expect_true(all(as.integer(rep$n_new) == 48L * length(.acr_origens)))
  ## CONTROL: a verdade gravada no LONG (999) e as chaves fora do par NAO entram: o erro relativo medido e o do y_pred, nao o do y_true gravado
  expect_equal(.acr_num(rep$wmape_new[rep$membro == "LIN_MED" & rep$area == "RJ"]), 1.0, tolerance = 1e-9)   # wmape em %
  expect_equal(.acr_num(rep$wmape_ref[rep$membro == "LIN_MED" & rep$area == "RJ"]), 1.5, tolerance = 1e-9)
})

test_that("unidade recusada (rc 5, window.x) vira ausencia em report.md e report.json, a chave sem par fica fora e o exit e 0", {
  ref_unit <- "medicao-ref-backtest-RJ-2023-12-31-dessem-regdin"
  fx <- acc_report_out(.acr_e, .acr_store, .acr_origens, .acr_areas, .acr_epsf, refuse = ref_unit, extra_new = .acr_extra_new,
                       extra_all = .acr_extra_all)
  r <- .acr_run(fx$out)
  expect_equal(r$status, 0L)
  md <- readLines(file.path(fx$out, "report.md"))
  expect_true(any(grepl("^## Ausencias", md)))
  expect_true(any(grepl("| ref | 2023-12-31 | RJ | RGD_MAX | window.x | 5 |", md, fixed = TRUE)))
  expect_true(any(grepl("| ref | 2023-12-31 | RJ | RGD_MED | window.x | 5 |", md, fixed = TRUE)))
  expect_true(any(grepl("RUN_ID: RUNFIX-1", md, fixed = TRUE)))
  js <- jsonlite::read_json(file.path(fx$out, "report.json"), simplifyVector = TRUE)
  ab <- js$ausencias
  expect_identical(names(ab), c("versao", "origem", "area", "membro", "error_id", "rc"))
  expect_setequal(paste(ab$versao, ab$origem, ab$area, ab$membro, ab$error_id), c("ref 2023-12-31 RJ RGD_MED window.x", "ref 2023-12-31 RJ RGD_MAX window.x"))
  expect_identical(js$d_plus, 1L); expect_identical(js$run_id, "RUNFIX-1"); expect_true(js$store_unchanged)
  expect_identical(js$unidades$total, 16L); expect_identical(js$unidades$recusa, 1L); expect_identical(js$unidades$failed, 0L)
  expect_identical(js$sha256_report_tsv, digest::digest(file = file.path(fx$out, "report.tsv"), algo = "sha256"))
  ## a celula recusada tira a origem do par: RGD_* so mede a outra origem (48 intervalos), nas duas versoes; as demais areas seguem inteiras
  rep <- .acr_read(file.path(fx$out, "report.tsv"))
  g <- function(m, a, col) rep[[col]][rep$membro == m & rep$area == a]
  expect_identical(c(g("RGD_MED", "RJ", "n_ref"), g("RGD_MED", "RJ", "n_new")), c("48", "48"))
  expect_identical(g("RGD_MED", "SP", "n_ref"), "96")
  hand <- .acr_hand(refused = ref_unit)
  expect_equal(.acr_num(g("RGD_MED", "RJ", "wmape_new")), hand$wmape[hand$versao == "new" & hand$membro == "RGD_MED" & hand$area == "RJ"], tolerance = 1e-9)
  ## a chave so da nova (LIN_MED, O+2 00:30, y_pred 1e6) nao entra no wmape de nenhuma das versoes
  expect_lt(.acr_num(g("LIN_MED", "RJ", "wmape_new")), 5)
  expect_lt(.acr_num(g("LIN_MED", "RJ", "wmape_ref")), 5)
})

test_that("saidas sem byte > 0x7F, store intacto e store alterado no meio da execucao => exit 9 integrity.accept_store_changed", {
  fx <- acc_report_out(.acr_e, .acr_store, .acr_origens, .acr_areas, .acr_epsf)
  before <- .acr_e$.bench_data_listing(.acr_store)
  expect_equal(.acr_run(fx$out)$status, 0L)
  for (f in c("report.tsv", "report.md", "report.json")) expect_false(.acr_nonascii(file.path(fx$out, f)), info = f)
  expect_identical(.acr_e$.bench_data_listing(.acr_store), before)
  expect_false(any(grepl("^\\.report-", list.files(fx$out, all.files = TRUE))))   # tmp + rename: nenhum temporario sobra
  ## CONTROL: um arquivo novo no store durante a execucao (hook de teste) => 9, e nenhuma saida nova e gravada
  st2 <- acc_report_store(.acr_e, .acr_areas, .acr_days)
  fx2 <- acc_report_out(.acr_e, st2, .acr_origens, .acr_areas, .acr_epsf)
  res <- .acr_e$prevcarga_with_errors(.acr_e$pc_accept_report_run(fx2$out, st2, pc_root(),
                                                                   between = function() writeLines("x", file.path(st2, "novo.txt"))))
  expect_identical(res$cond$id, "integrity.accept_store_changed")
  expect_equal(as.integer(res$cond$exit_code), 9L)
  expect_false(any(file.exists(file.path(fx2$out, c("report.tsv", "report.md", "report.json")))))
  ## alterar um arquivo existente tambem e detectado
  st3 <- acc_report_store(.acr_e, .acr_areas, .acr_days)
  fx3 <- acc_report_out(.acr_e, st3, .acr_origens, .acr_areas, .acr_epsf)
  f1 <- list.files(file.path(st3, "load"), recursive = TRUE, full.names = TRUE)[[1L]]
  res3 <- .acr_e$prevcarga_with_errors(.acr_e$pc_accept_report_run(fx3$out, st3, pc_root(), between = function() cat("x", file = f1, append = TRUE)))
  expect_identical(res3$cond$id, "integrity.accept_store_changed")
})

test_that("index.tsv, units.medicao.tsv ou LONG listado ausente/ilegivel => exit 2 contract.accept_report_input", {
  mk <- function() acc_report_out(.acr_e, .acr_store, .acr_origens[[1L]], .acr_areas[[1L]], .acr_epsf)
  fx <- mk(); unlink(file.path(fx$out, "index.tsv"))
  r <- .acr_run(fx$out); expect_equal(r$status, 2L); expect_identical(r$json$error_id, "contract.accept_report_input")
  expect_false(any(grepl("[^\\x20-\\x7E]", r$stderr, perl = TRUE)))
  fx <- mk(); unlink(file.path(fx$out, "units.medicao.tsv"))
  expect_equal(.acr_run(fx$out)$status, 2L)
  fx <- mk(); unlink(list.files(file.path(fx$out, "units"), pattern = "long.parquet", recursive = TRUE, full.names = TRUE)[[1L]])
  r <- .acr_run(fx$out); expect_equal(r$status, 2L); expect_identical(r$json$error_id, "contract.accept_report_input")
  fx <- mk(); writeLines("nao e parquet", list.files(file.path(fx$out, "units"), pattern = "long.parquet", recursive = TRUE, full.names = TRUE)[[1L]])
  expect_equal(.acr_run(fx$out)$status, 2L)
  expect_equal(.acr_run(file.path(tempdir(), "nao-existe-out"))$status, 2L)
  expect_equal(.acr_run(fx$out, store = file.path(tempdir(), "nao-existe-store"))$status, 2L)
  ## o erro esta no catalogo com classe contract (exit 2)
  expect_identical(.acr_e$prevcarga_with_errors(.acr_e$prevcarga_abort("contract.accept_report_input", where = "x"))$cond$exit_code, 2L)
})

## ---- estagio medicao e relatorio com o docker falso ---------------------------------------------------------------------------------
.acr_cfg <- function(origens = .acr_origens, areas = .acr_areas) {
  y <- yaml::read_yaml(file.path(pc_root(), "scripts", "tools", "accept_docker.yaml"))
  y$medicao <- list(origens = as.list(origens), areas = as.list(areas), mdirs = list("dessem-linear", "dessem-regdin"))
  f <- tempfile("acr-cfg-", fileext = ".yaml")
  yaml::write_yaml(y, f, handlers = list(logical = yaml::verbatim_logical))
  f
}
.acr_med_args <- function(stage, out, store, b, cfg, receipts = tempfile("acr-rcpt-"), extra = character())
  c("--stage", stage, "--out", out, "--data", store, "--ref-bundle", b$ref, "--new-bundle", b$new, "--config", cfg,
    "--min-free-gb-docker", "0", "--min-free-gb-out", "0", "--receipts-dir", receipts, "--format", "json", extra)
.acr_runs_of <- function(calls, u) sum(grepl(paste0("-", u$versao, " backtest --origin=", u$origem, " --area=", u$area, " --model=", u$mdir, " "),
                                              calls, fixed = TRUE) & startsWith(calls, "run "))

test_that("--stage medicao --resume: unidades conferidas nao rodam, divergente e de outro runtime_id rodam, rc 3 nao para a fila e rc 137 => exit 14", {
  med <- acc_med_fixture(.acr_e, .acr_store, .acr_areas, .acr_origens)
  un <- acc_units_medicao(.acr_origens, .acr_areas)
  fx <- acc_report_out(.acr_e, .acr_store, .acr_origens, .acr_areas, .acr_epsf)
  idx <- readLines(file.path(fx$out, "index.tsv"))
  ok8 <- un$unit[1:8]; diverg <- un$unit[[9L]]; rc3 <- un[10L, ]; rc137 <- un[11L, ]; oldrt <- un$unit[[12L]]
  rows <- c(idx[[1L]], idx[-1L][match(c(ok8, diverg, oldrt), un$unit)])
  rows[[10L]] <- sub("\t[0-9a-f]{64}$", "\t0000000000000000000000000000000000000000000000000000000000000000", rows[[10L]])
  rows[[11L]] <- sub("\trt-(ref|new)\t", "\trt-antigo\t", rows[[11L]])
  writeLines(rows, file.path(fx$out, "index.tsv"))
  ctrl <- list(c(rc3$versao, rc3$origem, rc3$area, rc3$mdir, 3L, "contract.fake_refusal"), c(rc137$versao, rc137$origem, rc137$area, rc137$mdir, 137L, "-"))
  fk <- acc_fake_docker(mode = "medicao", med = med, ctrl = ctrl); b <- acc_bundle_fixture()
  r <- acc_run(.acr_med_args("medicao", fx$out, .acr_store, b, .acr_cfg(), extra = "--resume"), fk)
  expect_equal(r$status, 14L)
  expect_identical(r$json$error_id, "partial.accept_units_failed")
  for (i in 1:8) expect_equal(.acr_runs_of(r$calls, un[i, ]), 0L, info = un$unit[[i]])             # conferidas: nenhum docker run
  for (u in list(un[9L, ], un[12L, ], rc3, rc137)) expect_equal(.acr_runs_of(r$calls, u), 1L, info = u$unit)
  for (i in 13:16) expect_equal(.acr_runs_of(r$calls, un[i, ]), 1L, info = un$unit[[i]])
  expect_identical(c(r$json$units$total, r$json$units$ok, r$json$units$refused, r$json$units$failed), c(16L, 14L, 1L, 1L))
  ix <- .acr_read(file.path(fx$out, "index.tsv")); last <- ix[, .SD[.N], by = unit]
  expect_identical(last$rc[match(rc3$unit, last$unit)], "3"); expect_identical(last$error_id[match(rc3$unit, last$unit)], "contract.fake_refusal")
  expect_identical(last$rc[match(rc137$unit, last$unit)], "137"); expect_identical(last$error_id[match(rc137$unit, last$unit)], "runtime.accept_oom_killed")
  expect_identical(last$rc[match(diverg, last$unit)], "0")
  expect_equal(acc_residue(fx$out), list(containers = 0L, images = 0L, cache = 0L))
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
  expect_true(r$json$store_unchanged)
})

test_that("sem --resume a medicao roda as 16 unidades; recusa classificada nao e failed (exit 0) e --resume repetido nao roda nada", {
  med <- acc_med_fixture(.acr_e, .acr_store, .acr_areas, .acr_origens)
  un <- acc_units_medicao(.acr_origens, .acr_areas)
  fk <- acc_fake_docker(mode = "medicao", med = med, ctrl = list(c("ref", "2024-02-11", "SP", "dessem-linear", 5L, "window.x")))
  b <- acc_bundle_fixture(); out <- tempfile("acr-out-"); cfg <- .acr_cfg()
  r <- acc_run(.acr_med_args("medicao", out, .acr_store, b, cfg), fk)
  expect_equal(r$status, 0L)
  expect_identical(c(r$json$units$total, r$json$units$ok, r$json$units$refused, r$json$units$failed), c(16L, 15L, 1L, 0L))
  expect_equal(sum(vapply(seq_len(nrow(un)), function(i) .acr_runs_of(r$calls, un[i, ]), 0L)), 16L)
  ## 2a execucao com --resume: as 15 unidades rc 0 ficam; so a recusada roda de novo
  fk2 <- acc_fake_docker(mode = "medicao", med = med, ctrl = list(c("ref", "2024-02-11", "SP", "dessem-linear", 5L, "window.x")))
  r2 <- acc_run(.acr_med_args("medicao", out, .acr_store, b, cfg, extra = "--resume"), fk2)
  expect_equal(r2$status, 0L)
  expect_equal(sum(vapply(seq_len(nrow(un)), function(i) .acr_runs_of(r2$calls, un[i, ]), 0L)), 1L)
})

test_that("--stage all verde grava o recibo docker-accept PASS com o runtime_id da nova; com unidade failed o recibo e FAIL e o relatorio sai mesmo assim", {
  med <- acc_med_fixture(.acr_e, .acr_store, .acr_areas, .acr_origens)
  b <- acc_bundle_fixture(); cfg <- .acr_cfg()
  sha_new <- digest::digest(file = b$new, algo = "sha256")
  ## (a) tudo verde
  fk <- acc_fake_docker(mode = "medicao", med = med); out <- tempfile("acr-out-"); rc <- tempfile("acr-rcpt-")
  r <- acc_run(.acr_med_args("all", out, .acr_store, b, cfg, receipts = rc), fk)
  expect_equal(r$status, 0L)
  expect_identical(r$json$verdict, "PASS")
  expect_identical(r$json$report, file.path(out, "report.tsv")); expect_true(file.exists(r$json$report))
  expect_identical(r$json$receipt, file.path(rc, paste0(sha_new, ".docker-accept.json")))
  rec <- jsonlite::read_json(r$json$receipt)
  expect_identical(rec$resultado, "PASS"); expect_identical(rec$runtime_id, "rt-new"); expect_identical(rec$gate, "docker-accept")
  expect_identical(rec$platform, "linux-x86_64"); expect_identical(rec$sha256, sha_new)
  expect_equal(acc_residue(out), list(containers = 0L, images = 0L, cache = 0L))
  st <- .acr_read(file.path(out, "stages.tsv")); expect_identical(st$V2[match(c("smoke", "aceite", "medicao", "relatorio"), st$V1)], rep("0", 4L))
  expect_true(all(c("report.tsv", "report.md", "report.json") %in% list.files(out)))
  ## relatorio isolado reaproveita run.json e stages.tsv do --out e regrava o recibo PASS
  fk1 <- acc_fake_docker(mode = "medicao", med = med); rc1 <- tempfile("acr-rcpt-")
  r1 <- acc_run(.acr_med_args("relatorio", out, .acr_store, b, cfg, receipts = rc1), fk1)
  expect_equal(r1$status, 0L); expect_identical(jsonlite::read_json(r1$json$receipt)$resultado, "PASS")
  expect_false(any(grepl("^(build|run|create) ", r1$calls)))
  ## (b) uma unidade da medicao rc 137: exit 14, recibo FAIL, relatorio gravado
  fk2 <- acc_fake_docker(mode = "medicao", med = med, ctrl = list(c("new", "2024-02-11", "RJ", "dessem-regdin", 137L, "-")))
  out2 <- tempfile("acr-out-"); rc2 <- tempfile("acr-rcpt-")
  r2 <- acc_run(.acr_med_args("all", out2, .acr_store, b, cfg, receipts = rc2), fk2)
  expect_equal(r2$status, 14L); expect_identical(r2$json$error_id, "partial.accept_units_failed")
  expect_identical(jsonlite::read_json(r2$json$receipt)$resultado, "FAIL")
  expect_true(file.exists(file.path(out2, "report.tsv")))
  expect_equal(acc_residue(out2), list(containers = 0L, images = 0L, cache = 0L))
  ## (c) smoke falho: aceite e medicao sao pulados, o relatorio registra FAIL
  fk3 <- acc_fake_docker(mode = "smoke_tz"); out3 <- tempfile("acr-out-"); rc3 <- tempfile("acr-rcpt-")
  r3 <- acc_run(.acr_med_args("all", out3, .acr_store, b, cfg, receipts = rc3), fk3)
  expect_equal(r3$status, 6L); expect_identical(r3$json$error_id, "bundle.accept_image_smoke")
  expect_identical(jsonlite::read_json(r3$json$receipt)$resultado, "FAIL")
  expect_false(any(grepl("backtest", r3$calls)))
})

test_that("script sem variavel de ambiente e sem chamada a AWS ou docker real no relatorio", {
  src <- grep("^\\s*#", readLines(acc_report_script()), value = TRUE, invert = TRUE)
  expect_false(any(grepl("Sys.getenv|Sys.setenv|system2?\\(|\\baws\\b|docker", src)))
  expect_false(.acr_nonascii(acc_report_script()))
})
