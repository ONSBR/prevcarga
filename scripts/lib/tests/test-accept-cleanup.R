## test-accept-cleanup.R -- accept_docker.sh (E8-T1): preflight, limpeza com residuo zero, executor de unidade e pins do Dockerfile.
## Docker FALSO em tempdir (helper-accept.R); nenhum docker real, nenhuma chamada AWS.

acc_zero <- list(containers = 0L, images = 0L, cache = 0L)
acc_no_object_calls <- function(calls) !any(grepl("^(build|run|create|pull|images|rmi) ", calls))

test_that("--out dentro do store, do repo ou do DockerRootDir => exit 2 contract.accept_out_forbidden sem objeto Docker", {
  fk <- acc_fake_docker(); b <- acc_bundle_fixture(); st <- acc_store()
  for (out in c(file.path(st, "acc-out"), file.path(pc_root(), "acc-out-nunca-criado"), file.path(fk$root, "acc-out"))) {
    r <- acc_run(acc_args(out, st, b), fk)
    expect_equal(r$status, 2L, info = out)
    expect_equal(r$json$error_id, "contract.accept_out_forbidden", info = out)
    expect_false(dir.exists(out), info = out)
    expect_true(acc_no_object_calls(r$calls), info = out)
  }
})

test_that("espaco livre abaixo do minimo => exit 2 contract.accept_disk_low antes do primeiro build, residuo zero", {
  fk <- acc_fake_docker(); b <- acc_bundle_fixture(); st <- acc_store()
  for (flag in c("--min-free-gb-docker", "--min-free-gb-out")) {
    out <- tempfile("acc-out-")
    r <- acc_run(c(acc_args(out, st, b), flag, "99999999"), fk)
    expect_equal(r$status, 2L, info = flag)
    expect_equal(r$json$error_id, "contract.accept_disk_low", info = flag)
    expect_equal(acc_residue(out), acc_zero, info = flag)
    expect_false(any(grepl("^build ", r$calls)), info = flag)
  }
})

test_that("estagio invalido e opcao desconhecida => contract.accept_bad_args sem objeto Docker", {
  fk <- acc_fake_docker(); b <- acc_bundle_fixture(); st <- acc_store()
  r1 <- acc_run(c("--stage", "nao-existe", "--out", tempfile("acc-out-"), "--data", st), fk)
  expect_equal(r1$status, 2L); expect_equal(r1$json$error_id, "contract.accept_bad_args")
  r2 <- acc_run(c("--stage", "smoke", "--nao-existe", "x", "--out", tempfile("acc-out-"), "--data", st), fk)
  expect_equal(r2$status, 2L); expect_equal(r2$json$error_id, "contract.accept_bad_args")
  expect_true(acc_no_object_calls(c(r1$calls, r2$calls)))
})

test_that("residuo zero em sucesso, falha e TERM", {
  st <- acc_store(); b <- acc_bundle_fixture()
  ## (a) tudo passa
  fk <- acc_fake_docker(mode = "ok"); out <- tempfile("acc-out-")
  r <- acc_run(acc_args(out, st, b), fk)
  expect_equal(r$status, 0L); expect_equal(r$json$verdict, "PASS"); expect_null(r$json$error_id)
  expect_equal(acc_residue(out), acc_zero)
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
  expect_true(r$json$store_unchanged)
  expect_equal(r$json$runtime_id$ref, "rt-ref")
  ## (b) zoneinfo ausente na imagem nova
  fk <- acc_fake_docker(mode = "smoke_tz"); out <- tempfile("acc-out-")
  r <- acc_run(acc_args(out, st, b), fk)
  expect_equal(r$status, 6L); expect_equal(r$json$error_id, "bundle.accept_image_smoke")
  expect_equal(acc_residue(out), acc_zero)
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
  ## falha no docker build
  fk <- acc_fake_docker(mode = "build_fail"); out <- tempfile("acc-out-")
  r <- acc_run(acc_args(out, st, b), fk)
  expect_equal(r$status, 6L); expect_equal(r$json$error_id, "bundle.accept_image_build")
  expect_equal(acc_residue(out), acc_zero)
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
  ## (c) TERM com um container vivo
  fk <- acc_fake_docker(mode = "hang"); out <- tempfile("acc-out-")
  p <- processx::process$new("bash", c(acc_script(), acc_args(out, st, b)), env = c("current", PATH = acc_path(fk)),
                             stdout = "|", stderr = "|")
  acc_wait_for(function() file.exists(fk$log) && any(grepl("--entrypoint", readLines(fk$log, warn = FALSE))) && length(acc_state_files(fk, "containers")) > 0L)
  p$signal(tools::SIGTERM); p$wait(30000)
  expect_equal(p$get_exit_status(), 143L)
  for (w in c("containers", "images", "cache")) expect_length(acc_state_files(fk, w), 0L)
  expect_equal(acc_residue(out), acc_zero)
})

test_that("container que sobra apos rm -f => exit 9 integrity.accept_residue, prevalecendo sobre o rc original", {
  fk <- acc_fake_docker(mode = "sticky"); st <- acc_store(); b <- acc_bundle_fixture(); out <- tempfile("acc-out-")
  r <- acc_run(acc_args(out, st, b), fk)
  expect_equal(r$status, 9L); expect_equal(r$json$error_id, "integrity.accept_residue")
  expect_gte(acc_residue(out)$containers, 1L)
})

test_that("store alterado durante o run => exit 9 integrity.accept_store_changed", {
  st <- acc_store(); fk <- acc_fake_docker(mode = "store_touch", store = st); b <- acc_bundle_fixture(); out <- tempfile("acc-out-")
  r <- acc_run(acc_args(out, st, b), fk)
  expect_equal(r$status, 9L); expect_equal(r$json$error_id, "integrity.accept_store_changed")
  expect_false(r$json$store_unchanged)
})

test_that("base preexistente e cache preexistente ficam; so ids novos saem por prune com --filter; nunca system prune", {
  st <- acc_store(); b <- acc_bundle_fixture()
  fk <- acc_fake_docker(mode = "base_present"); out <- tempfile("acc-out-")
  r <- acc_run(acc_args(out, st, b), fk)
  expect_equal(r$status, 0L)
  expect_true(file.exists(file.path(fk$state, "images", "base")))
  expect_equal(jsonlite::read_json(file.path(out, "run.json"))$base_preexistente, TRUE)
  fk2 <- acc_fake_docker(mode = "cache_pre"); out2 <- tempfile("acc-out-")
  r2 <- acc_run(acc_args(out2, st, b), fk2)
  expect_equal(r2$status, 0L)
  expect_equal(acc_state_files(fk2, "cache"), "pre0001")
  expect_equal(jsonlite::read_json(file.path(out2, "run.json"))$base_preexistente, FALSE)
  for (calls in list(r$calls, r2$calls)) {
    pr <- grep("prune", calls, value = TRUE)
    expect_false(any(grepl("system", calls)))
    expect_true(all(grepl("^builder prune -f --filter id=", pr)))
  }
  expect_true(any(grepl("^builder prune -f --filter id=build-", r2$calls)))
})

test_that("pc_accept_run_unit monta o docker run isolado, sem -e, e acrescenta a linha de index.tsv; a fila devolve 14 se falhar", {
  for (mode in c("ok", "oom", "unit_refuse", "unit_fail")) {
    fk <- acc_fake_docker(mode = mode); st <- acc_store(); out <- tempfile("acc-out-"); dir.create(out)
    u <- file.path(out, "u1"); mf <- file.path(out, "mounts.txt"); { tp <- file.path(out, "topology.yaml"); writeLines("x", tp); writeLines(paste0(tp, ":/cfg/topology.yaml:ro"), mf) }
    tsv <- file.path(out, "units.tsv")
    writeLines(c(paste("1", u, "ref", "7", mf, "2026-01-01", "SE", "m1", "forecast", sep = "\t"),
                 paste("1", file.path(out, "u2"), "ref", "8", "", "2026-01-02", "SE", "m1", "forecast", sep = "\t")), tsv)
    script <- sprintf(paste0("source %s; RUN_ID=RID1; DATA=%s; OUT=%s; RUNTIME_ID[ref]=rt-ref; qrc=0; pc_accept_queue %s 2 || qrc=$?; echo QRC=$qrc; ",
                             "echo U=$UNITS_TOTAL,$UNITS_OK,$UNITS_REFUSED,$UNITS_FAILED"),
                      shQuote(acc_script()), shQuote(st), shQuote(out), shQuote(tsv))
    res <- suppressWarnings(system2("bash", c("-c", shQuote(script)), stdout = TRUE, stderr = FALSE, env = paste0("PATH=", acc_path(fk))))
    calls <- readLines(fk$log); run <- grep("^run ", calls, value = TRUE)
    expect_length(run, 2L)
    one <- grep("pc-accept-RID1-7", run, value = TRUE)
    for (pat in c("--rm", "--name pc-accept-RID1-7 ", "--label prevcarga.accept.run=RID1", "--network none", "--memory 8g", "--cpus 1",
                  sprintf("--user %s:%s", system("id -u", intern = TRUE), system("id -g", intern = TRUE)),
                  sprintf("-v %s:/data:ro", st), sprintf("-v %s:/out", u), sprintf("-v %s/tmp:/tmp", u),
                  sprintf("-v %s/topology.yaml:/cfg/topology.yaml:ro", out)))
      expect_true(grepl(pat, one, fixed = TRUE), info = pat)
    expect_false(grepl(" -e | --env", one))
    idx <- utils::read.delim(file.path(out, "index.tsv"), stringsAsFactors = FALSE)
    expect_equal(nrow(idx), 2L)
    row <- idx[idx$unit == "u1", ]
    expect_equal(row$rc, c(ok = 0L, oom = 137L, unit_refuse = 2L, unit_fail = 1L)[[mode]], info = mode)
    expect_equal(row$error_id, c(ok = "-", oom = "runtime.accept_oom_killed", unit_refuse = "contract.fake_refusal", unit_fail = "-")[[mode]], info = mode)
    expect_true(is.numeric(row$segundos))
    expect_equal(row$runtime_id, "rt-ref"); expect_equal(row$origem, "2026-01-01")
    expect_match(res[grepl("^QRC=", res)], if (mode %in% c("oom", "unit_fail")) "QRC=14" else "QRC=0", info = mode)
  }
})

test_that("pins do Dockerfile iguais aos do build_bundle_linux.sh, com tzdata, zstd, TZ=UTC e sem aws", {
  df <- readLines(acc_dockerfile()); bb <- readLines(file.path(pc_root(), "scripts", "tools", "build_bundle_linux.sh"))
  image <- sub("^IMAGE='(.*)'$", "\\1", grep("^IMAGE='", bb, value = TRUE))
  snap <- sub("^APT_SNAPSHOT='(.*)'$", "\\1", grep("^APT_SNAPSHOT='", bb, value = TRUE))
  expect_true(any(df == paste0("ARG BASE=", image)))
  expect_true(any(df == paste0("ARG APT_SNAPSHOT=", snap)))
  expect_true(any(grepl("^FROM \\$\\{BASE\\}$", df)))
  expect_true(any(grepl("--no-install-recommends ca-certificates tzdata zstd", df, fixed = TRUE)))
  expect_true(any(grepl("ENV TZ=UTC LC_ALL=C.UTF-8 PATH=/opt/prevcarga/bin", df, fixed = TRUE)))
  expect_false(any(grepl("aws", df, ignore.case = TRUE)))
  expect_true(any(grepl("sha256sum -c", df, fixed = TRUE)))
})

test_that("todo error_id do script existe em accept.tsv com o exit da classe; tudo ASCII", {
  sh <- readLines(acc_script())
  cat_ <- utils::read.delim(file.path(pc_root(), "scripts", "lib", "errors", "accept.tsv"), stringsAsFactors = FALSE, quote = "")
  exits <- pc_source_lib("errors")$prevcarga_exit_codes
  ids <- unique(unlist(regmatches(sh, gregexpr("(contract|bundle|integrity|partial|runtime|repro)\\.accept_[a-z_]+", sh))))
  expect_gt(length(ids), 5L)
  expect_true(all(ids %in% cat_$id))
  expect_true(all(cat_$class %in% names(exits)))
  expect_equal(sort(cat_$id), sort(ids))
  ## exit usado no script: tabela pc_accept_class_exit
  tab <- c(contract = 2L, bundle = 6L, runtime = 7L, integrity = 9L, repro = 12L, partial = 14L)
  for (cl in names(tab)) expect_equal(unname(exits[[cl]]), tab[[cl]])
  for (cl in names(tab))
    expect_true(any(grepl(sprintf("%s) echo %d ;;", cl, tab[[cl]]), paste(sh, collapse = " "), fixed = TRUE)), info = cl)
  asc <- function(x) !any(as.integer(unlist(lapply(x, function(s) as.integer(charToRaw(s))))) > 127L)
  expect_true(asc(sh)); expect_true(asc(readLines(file.path(pc_root(), "scripts", "lib", "errors", "accept.tsv"))))
  expect_true(asc(readLines(acc_dockerfile())))
  expect_false(any(grepl("system prune|builder prune -f( |$)", sh[!grepl("^#", sh)]) & !grepl("--filter", sh[!grepl("^#", sh)])))
})
