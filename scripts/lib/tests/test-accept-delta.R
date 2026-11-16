## test-accept-delta.R -- accept_delta.R e estagio aceite do accept_docker.sh (E8-T2).
## LONGs sinteticos de 8 colunas (arrow) em tempdir e docker FALSO (helper-accept.R); nenhum docker real, nenhuma chamada AWS.
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
pc_source_lib(c("errors", "topology"), env = environment())
source(file.path(pc_root(), "scripts", "tools", "accept_cases.R"), local = environment())
source(file.path(pc_root(), "scripts", "tools", "accept_delta.R"), local = environment())

acc_delta_script <- function() file.path(pc_root(), "scripts", "tools", "accept_delta.R")
acc_delta_run <- function(args) {
  se <- tempfile("acc-delta-err-")
  so <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", acc_delta_script(), args), stdout = TRUE, stderr = se))
  st <- attr(so, "status"); if (is.null(st)) st <- 0L
  txt <- paste(so, collapse = "\n")
  list(status = as.integer(st), stderr = readLines(se, warn = FALSE),
       json = if (startsWith(txt, "{")) jsonlite::fromJSON(txt, simplifyVector = FALSE) else NULL)
}
acc_read_delta <- function(f) data.table::fread(f, sep = "\t", na.strings = NULL)

## par de diretorios com os tres tipos de artefato (members/*, combined/*, reconciled/subsistemas/S)
acc_delta_dirs <- function() {
  mk <- function() {
    d <- tempfile("acc-delta-"); r <- file.path(d, "state", "backtest", "run1")
    for (p in c("members/LIN_MED/RS/2023-06-01/0800/long.parquet", "combined/EQW.parquet", "reconciled/subsistemas/S.parquet"))
      acc_long_fixture(file.path(r, p))
    list(dir = d, run = r)
  }
  list(ref = mk(), new = mk())
}
acc_delta_out <- function() tempfile("delta-", fileext = ".tsv")

test_that("fixtures identicas => exit 0, uma linha por artefato e area, max_abs 0 e nenhuma chave exclusiva", {
  d <- acc_delta_dirs(); out <- acc_delta_out()
  r <- acc_delta_run(c("--ref", d$ref$dir, "--new", d$new$dir, "--out", out))
  expect_equal(r$status, 0L)
  expect_true(r$json$ok); expect_identical(r$json$result$delta$keys_only, 0L); expect_equal(r$json$result$delta$max_abs, 0)
  x <- acc_read_delta(out)
  expect_identical(names(x), c("artefato", "area", "n_ref", "n_new", "keys_only_ref", "keys_only_new", "max_abs"))
  expect_identical(sort(x$artefato), sort(c("members/LIN_MED/RS/2023-06-01/0800/long.parquet", "combined/EQW.parquet",
                                            "reconciled/subsistemas/S.parquet")))
  expect_true(all(x$area == "RS")); expect_true(all(x$n_ref == x$n_new & x$n_ref == 6L))
  expect_true(all(x$keys_only_ref == 0L & x$keys_only_new == 0L & x$max_abs == 0))
  ## o proprio run (sem state/backtest) tambem e aceito como diretorio
  r2 <- acc_delta_run(c("--ref", d$ref$run, "--new", d$new$run, "--out", acc_delta_out()))
  expect_equal(r2$status, 0L)
})

test_that("y_pred +1e-9, linha a mais so no new e membro LIN_UNI so no new => exit 12 com as colunas certas", {
  d <- acc_delta_dirs(); out <- acc_delta_out()
  acc_long_fixture(file.path(d$new$run, "members/LIN_MED/RS/2023-06-01/0800/long.parquet"), bump = 1e-9)
  acc_long_fixture(file.path(d$new$run, "combined/EQW.parquet"), n = 7L)
  acc_long_fixture(file.path(d$new$run, "members/LIN_UNI/RS/2023-06-01/0800/long.parquet"))
  r <- acc_delta_run(c("--ref", d$ref$dir, "--new", d$new$dir, "--out", out))
  expect_equal(r$status, 12L)
  expect_identical(r$json$error_id, "repro.accept_delta_nonzero"); expect_identical(r$json$error$id, "repro.accept_delta_nonzero")
  expect_true(any(grepl("repro", r$stderr)))
  x <- acc_read_delta(out); setkey <- function(a) x[artefato == a]
  alt <- setkey("members/LIN_MED/RS/2023-06-01/0800/long.parquet")
  expect_equal(alt$max_abs, 1e-9, tolerance = 1e-3); expect_identical(alt$keys_only_new, 0L)
  ext <- setkey("combined/EQW.parquet")
  expect_identical(ext$keys_only_new, 1L); expect_identical(ext$keys_only_ref, 0L); expect_identical(ext$n_new, 7L)
  uni <- setkey("members/LIN_UNI/RS/2023-06-01/0800/long.parquet")
  expect_identical(uni$n_ref, 0L); expect_identical(uni$keys_only_new, 6L)
  expect_equal(setkey("reconciled/subsistemas/S.parquet")$max_abs, 0)
  expect_equal(r$json$error$details$keys_only, 7L)
  ## so um artefato exclusivo ja basta para o 12 (e a ref tambem pode ter o exclusivo)
  d2 <- acc_delta_dirs(); acc_long_fixture(file.path(d2$ref$run, "members/LIN_UNI/RS/2023-06-01/0800/long.parquet"))
  r2 <- acc_delta_run(c("--ref", d2$ref$dir, "--new", d2$new$dir, "--out", acc_delta_out()))
  expect_equal(r2$status, 12L)
})

test_that("target_datetime com tzone diferente e o mesmo instante nao conta como diferenca; instante diferente conta", {
  d <- acc_delta_dirs(); out <- acc_delta_out()
  for (p in c("members/LIN_MED/RS/2023-06-01/0800/long.parquet", "combined/EQW.parquet", "reconciled/subsistemas/S.parquet"))
    acc_long_fixture(file.path(d$new$run, p), tz = "America/Sao_Paulo")
  expect_equal(acc_delta_run(c("--ref", d$ref$dir, "--new", d$new$dir, "--out", out))$status, 0L)
  ## unitario: mesmo instante em tz diferentes
  a <- data.frame(area = "RS", forecast_origin = "2023-06-01", lead = 1L, y_pred = 1)
  a$target_datetime <- as.POSIXct("2023-06-01 03:00:00", tz = "UTC")
  b <- a; b$target_datetime <- as.POSIXct("2023-06-01 00:00:00", tz = "America/Sao_Paulo")
  x <- pc_accept_delta_one(a, b, "x")
  expect_identical(c(x$keys_only_ref, x$keys_only_new), c(0L, 0L)); expect_equal(x$max_abs, 0)
  ## [CONTROL] instante diferente => chave exclusiva dos dois lados
  b$target_datetime <- as.POSIXct("2023-06-01 04:00:00", tz = "UTC")
  y <- pc_accept_delta_one(a, b, "x")
  expect_identical(c(y$keys_only_ref, y$keys_only_new), c(1L, 1L))
  ## NA so de um lado e Inf; NA nos dois lados e igual
  c1 <- a; c1$y_pred <- NA_real_
  expect_equal(pc_accept_delta_one(a, c1, "x")$max_abs, Inf)
  expect_equal(pc_accept_delta_one(c1, c1, "x")$max_abs, 0)
})

test_that("infer-model: LONG em long.parquet da unidade, forecast_origin vem de origin e lead do posto do alvo", {
  mk <- function(bump = 0) {
    d <- tempfile("acc-inf-"); dir.create(d)
    t <- as.POSIXct("2023-06-01 01:00:00", tz = "UTC") + 3600 * (0:3)
    arrow::write_parquet(data.frame(model = "dessem-linear", variant = "MED", area = "RJ", origin = as.Date("2023-06-01"),
                                    available_until = t[[1L]], target_datetime = t, y_pred = c(1, 2, 3, 4) + c(bump, 0, 0, 0),
                                    stringsAsFactors = FALSE), file.path(d, "long.parquet"))
    d
  }
  expect_equal(acc_delta_run(c("--ref", mk(), "--new", mk(), "--out", acc_delta_out()))$status, 0L)
  expect_equal(acc_delta_run(c("--ref", mk(), "--new", mk(1e-9), "--out", acc_delta_out()))$status, 12L)
  ## nenhum LONG em nenhum lado e erro de uso, nao delta zero
  e <- tempfile("acc-vazio-"); dir.create(e)
  r <- acc_delta_run(c("--ref", e, "--new", e, "--out", acc_delta_out()))
  expect_equal(r$status, 2L); expect_identical(r$json$error_id, "contract.accept_bad_args")
})

## index.tsv e units.tsv de fixture para o veredito
acc_verdict_fixture <- function(uni_error = "-", secs_ref = 8L, secs_new = 3L, hol_error = "generic.unexpected", hol_area = "AM") {
  cfg <- pc_accept_config_load(file.path(pc_root(), "scripts", "tools", "accept_docker.yaml"), pc_root())
  u <- pc_accept_units(cfg, "aceite")
  ix <- data.frame(unit = u$unit, versao = u$versao, image = "img", runtime_id = "rt", origem = u$origem, area = u$area, mdir = u$mdir,
                   rc = 0L, error_id = "-", segundos = 1L, long_paths = "-", sha256 = "-", stringsAsFactors = FALSE)
  tm <- u$verbo == "infer-model" & u$data_flag != "--data-inference"
  ix$segundos[tm & u$versao == "ref"] <- secs_ref; ix$segundos[tm & u$versao == "new"] <- secs_new
  h <- u$verbo == "backtest" & u$mdir == "dessem-linear" & u$area == hol_area
  ix$error_id[h] <- uni_error; ix$rc[h] <- if (uni_error == "-") 0L else 1L
  d <- tempfile("acc-verdict-"); dir.create(d)
  ui <- file.path(d, "units.tsv"); ii <- file.path(d, "index.tsv")
  data.table::fwrite(u, ui, sep = "\t", quote = FALSE); data.table::fwrite(ix, ii, sep = "\t", quote = FALSE)
  list(dir = d, units = ui, index = ii, aceite = file.path(d, "aceite.json"), unit_hol = u$unit[h])
}

test_that("--verdict: index limpo => 0; generic.unexpected de feriado e tempo nao menor => 12 com o id certo e culpados", {
  f <- acc_verdict_fixture()
  r <- acc_delta_run(c("--verdict", "--index", f$index, "--units", f$units, "--out", f$aceite))
  expect_equal(r$status, 0L)
  j <- jsonlite::read_json(f$aceite)
  expect_identical(j$verdict, "PASS"); expect_length(j$culprits, 0L); expect_equal(j$infer_seconds$ref, 8); expect_equal(j$infer_seconds$new, 3)
  ## feriado
  f <- acc_verdict_fixture(uni_error = "generic.unexpected")
  r <- acc_delta_run(c("--verdict", "--index", f$index, "--units", f$units, "--out", f$aceite))
  expect_equal(r$status, 12L); expect_identical(r$json$error_id, "repro.accept_unexpected_error")
  j <- jsonlite::read_json(f$aceite)
  expect_identical(j$verdict, "FAIL"); expect_length(j$culprits, 1L)
  expect_identical(j$culprits[[1L]]$unit, f$unit_hol); expect_identical(j$culprits[[1L]]$error_id, "repro.accept_unexpected_error")
  ## tempo: igual e maior reprovam; menor passa
  for (s in list(c(8L, 8L), c(8L, 9L))) {
    f <- acc_verdict_fixture(secs_ref = s[[1L]], secs_new = s[[2L]])
    r <- acc_delta_run(c("--verdict", "--index", f$index, "--units", f$units, "--out", f$aceite))
    expect_equal(r$status, 12L, info = paste(s, collapse = ">=")); expect_identical(r$json$error_id, "repro.accept_infer_not_faster")
    expect_identical(jsonlite::read_json(f$aceite)$culprits[[1L]]$error_id, "repro.accept_infer_not_faster")
  }
  ## os dois juntos: ambos listados
  f <- acc_verdict_fixture(uni_error = "generic.unexpected", secs_ref = 3L, secs_new = 3L)
  r <- acc_delta_run(c("--verdict", "--index", f$index, "--units", f$units, "--out", f$aceite))
  expect_equal(r$status, 12L); expect_length(jsonlite::read_json(f$aceite)$culprits, 2L)
  ## generic.unexpected na unidade de backtest S da ref (nao e feriado do dessem-linear da nova) nao reprova
  f <- acc_verdict_fixture()
  ix <- utils::read.delim(f$index, stringsAsFactors = FALSE, na.strings = character(), quote = "")
  ix$error_id[grepl("^aceite-ref-backtest-S", ix$unit)] <- "generic.unexpected"; ix$rc[grepl("^aceite-ref-backtest-S", ix$unit)] <- 1L
  utils::write.table(ix, f$index, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_equal(acc_delta_run(c("--verdict", "--index", f$index, "--units", f$units, "--out", f$aceite))$status, 0L)
})

## estagio aceite ponta a ponta com o docker falso (long_fixture/diverge/holiday_unexpected)
acc_aceite_args <- function(out, store, bundles, extra = character()) {
  c("--stage", "aceite", "--out", out, "--data", store, "--ref-bundle", bundles$ref, "--new-bundle", bundles$new,
    "--min-free-gb-docker", "0", "--min-free-gb-out", "0", "--format", "json", extra)
}
acc_run_lines <- function(calls) grep("^run .*--user", calls, value = TRUE)
acc_vols <- function(line) sub("^-v ", "", regmatches(line, gregexpr("-v [^ ]+", line))[[1L]])

test_that("estagio aceite com docker falso: ondas em ordem, montagens extras so :ro, delta 0, aceite.json e residuo zero", {
  fk <- acc_fake_docker(mode = "long_fixture"); st <- acc_store(); b <- acc_bundle_fixture(); out <- tempfile("acc-out-")
  r <- acc_run(acc_aceite_args(out, st, b), fk)
  expect_equal(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  expect_identical(r$json$verdict, "PASS"); expect_null(r$json$error_id)
  expect_equal(r$json$delta$max_abs, 0); expect_identical(r$json$delta$keys_only, 0L)
  expect_identical(r$json$units$total, 16L); expect_identical(r$json$units$failed, 0L)
  expect_equal(acc_residue(out), list(containers = 0L, images = 0L, cache = 0L))
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
  for (f in c("delta.tsv", "delta.inference.tsv", "aceite.json", "units.aceite.tsv", "index.tsv")) expect_true(file.exists(file.path(out, f)), info = f)
  expect_identical(jsonlite::read_json(file.path(out, "aceite.json"))$verdict, "PASS")
  expect_true(all(acc_read_delta(file.path(out, "delta.tsv"))$max_abs == 0))
  expect_equal(sort(unique(acc_read_delta(file.path(out, "delta.tsv"))$artefato)),
               sort(c("members/LIN_MED/RS/2023-06-01/0800/long.parquet", "members/LIN_MAX/RS/2023-06-01/0800/long.parquet",
                      "combined/EQW.parquet", "reconciled/subsistemas/S.parquet")))
  ## ondas: nenhuma unidade com /artifacts antes do ultimo treino; o deck antes do infer com /deckinf
  runs <- acc_run_lines(r$calls)
  expect_length(runs, 16L)
  is_train <- grepl(" train-models ", runs); art <- grepl("/artifacts:ro", runs, fixed = TRUE); deck <- grepl("/deckinf:ro", runs, fixed = TRUE)
  expect_equal(sum(is_train), 2L); expect_equal(sum(art), 4L); expect_equal(sum(deck), 1L)
  expect_lt(max(which(!art & !deck)), min(which(art))); expect_lt(max(which(art & !deck)), which(deck))
  expect_true(all(which(is_train) < min(which(art))))
  ## montagens: so /data:ro, /out e /tmp (rw da propria unidade) e extras todas :ro
  allv <- unlist(lapply(runs, acc_vols))
  extra <- allv[!grepl(":/data:ro$|:/out$|:/tmp$", allv)]
  expect_gt(length(extra), 0L); expect_true(all(grepl(":ro$", extra)))
  expect_true(any(grepl(":/opt/prevcarga/config/topology.yaml:ro$", extra)))
  expect_equal(sum(grepl(":/opt/prevcarga/models/dessem-(linear|regdin)/run.yaml:ro$", extra)), 2L)
  expect_equal(sum(grepl("/state/artifacts:/artifacts:ro$", extra)), 4L)
  ## --data-artifacts na nova e --artifacts-dir na ref; --data-train/--data-inference/--data
  inf <- grep(" infer-model ", runs, value = TRUE)
  expect_equal(sum(grepl("--data-inference=/deckinf", inf, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("--artifacts-dir=/artifacts", inf, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("--data-artifacts=/artifacts", inf, fixed = TRUE)), 2L)
  expect_false(any(grepl("(^| )-e | --env", runs)))
  ## o S da nova roda com a topologia sem LIN_UNI e a janela referencia; o overlay existe so no lado nova
  ov <- yaml::read_yaml(file.path(out, "units", "aceite-new-backtest-S-2023-06-01", "overlay", "topology.yaml"))
  expect_false("LIN_UNI" %in% vapply(ov$members, function(m) m$sigla, ""))
  expect_false(dir.exists(file.path(out, "units", "aceite-ref-backtest-S-2023-06-01", "overlay")) &&
               length(list.files(file.path(out, "units", "aceite-ref-backtest-S-2023-06-01", "overlay")) ) > 0L)
  jr <- yaml::read_yaml(file.path(out, "units", "aceite-new-backtest-S-2023-06-01", "overlay", "dessem-linear.run.yaml"))
  expect_identical(jr$params$janela_dessem, "referencia")
  expect_identical(readLines(file.path(out, "units", "aceite-new-backtest-S-2023-06-01", "config.yaml")), c("schema_version: 1", "format: json"))
  ## as extracoes da imagem sao containers rotulados com --network none
  ex <- grep("--entrypoint cat", r$calls, value = TRUE)
  expect_gt(length(ex), 0L); expect_true(all(grepl("--network none", ex, fixed = TRUE) & grepl("--label prevcarga.accept.run=", ex, fixed = TRUE)))
})

test_that("estagio aceite com divergencia injetada => exit 12 repro.accept_delta_nonzero, delta.tsv mostra o y_pred, residuo zero", {
  fk <- acc_fake_docker(mode = "diverge"); st <- acc_store(); b <- acc_bundle_fixture(); out <- tempfile("acc-out-")
  r <- acc_run(acc_aceite_args(out, st, b), fk)
  expect_equal(r$status, 12L)
  expect_identical(r$json$error_id, "repro.accept_delta_nonzero"); expect_identical(r$json$verdict, "FAIL")
  expect_equal(r$json$delta$max_abs, 1e-9, tolerance = 1e-3)
  x <- acc_read_delta(file.path(out, "delta.tsv"))
  expect_equal(x[artefato == "reconciled/subsistemas/S.parquet"]$max_abs, 1e-9, tolerance = 1e-3)
  expect_true(all(x[artefato != "reconciled/subsistemas/S.parquet"]$max_abs == 0))
  expect_true(file.exists(file.path(out, "aceite.json")))
  expect_equal(acc_residue(out), list(containers = 0L, images = 0L, cache = 0L))
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
})

test_that("estagio aceite: feriado com generic.unexpected na nova => exit 12 repro.accept_unexpected_error e aceite.json lista a unidade", {
  fk <- acc_fake_docker(mode = "holiday_unexpected"); st <- acc_store(); b <- acc_bundle_fixture(); out <- tempfile("acc-out-")
  r <- acc_run(acc_aceite_args(out, st, b), fk)
  expect_equal(r$status, 12L)
  expect_identical(r$json$error_id, "repro.accept_unexpected_error")
  j <- jsonlite::read_json(file.path(out, "aceite.json"))
  expect_identical(j$verdict, "FAIL"); expect_identical(j$culprits[[1L]]$unit, "aceite-new-backtest-AM-2023-06-01-dessem-linear")
  expect_equal(acc_residue(out), list(containers = 0L, images = 0L, cache = 0L))
})

test_that("config invalida no estagio aceite => exit 2 contract.accept_bad_config antes de qualquer objeto Docker; flag vence o YAML", {
  fk <- acc_fake_docker(mode = "long_fixture"); st <- acc_store(); b <- acc_bundle_fixture(); fx <- acc_cfg_fixture()
  r <- acc_run(acc_aceite_args(tempfile("acc-out-"), st, b, c("--config", fx$area_zz)), fk)
  expect_equal(r$status, 2L); expect_identical(r$json$error_id, "contract.accept_bad_config")
  expect_false(any(grepl("^(build|run|create|images|rmi) ", r$calls)))
  ## precedencia: --jobs vence o jobs do YAML; sem flag vale o YAML
  sh <- sprintf("source %s; CONFIG=%s; STAGE=aceite; JOBS=%s; pc_accept_load_settings; echo $JOBS,$MEMORY,$CPUS,$MIN_FREE_DOCKER,$MIN_FREE_OUT,$PC_ACCEPT_REF_REV,$OVERLAY_MDIRS",
                shQuote(acc_script()), shQuote(file.path(pc_root(), "scripts", "tools", "accept_docker.yaml")), "%s")
  run <- function(j) suppressWarnings(system2("bash", c("-c", shQuote(sprintf(sh, if (is.null(j)) "''" else j))), stdout = TRUE,
                                              env = paste0("PATH=", acc_path(fk))))
  expect_identical(run(NULL), "13,8g,1,10,50,de24021,dessem-linear,dessem-regdin")
  expect_identical(run("14"), "14,8g,1,10,50,de24021,dessem-linear,dessem-regdin")
})
