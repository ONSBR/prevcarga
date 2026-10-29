## test-plugin-tools-gates.R -- runner comum de gates + agregador (P.0h). Tudo por processo real (Rscript --vanilla),
## com TZ sujo (America/Sao_Paulo) no ambiente do filho: o runner deve sobrescreve-lo in-process.

.pg_run <- function(runner, args = character()) {
  pc_with_env(c(TZ = "America/Sao_Paulo"), pc_rscript(c(runner, args)))
}
.pg_tool <- function() file.path(pc_root(), "scripts", "tools", "plugin_gates.R")
.pg_ascii <- function(x) all(utf8ToInt(paste(x, collapse = "\n")) < 128L)
.pg_line <- function(out, gate) grep(paste0("^", gate, " +\\|"), out$stdout, value = TRUE)

test_that("[ac-0] tier fast: PLUGINS verdes, paridade N/A(tier), DEFERRED como SKIP com dono; TZ sujo sobrescrito", {
  root <- withr::local_tempdir()
  r <- pc_toy_gates_model(root, "toy-30", tests = c("ok", "parity"))
  out <- .pg_run(r, c("--tier", "fast"))
  expect_identical(out$status, 0L)
  expect_match(out$stdout[[1L]], "^gate \\| dono \\| status \\| detalhe$")
  expect_match(.pg_line(out, "contrato"), "\\[PLUGINS\\] +\\| PASS")
  expect_match(.pg_line(out, "corte"), "\\[PLUGINS\\] +\\| PASS")
  expect_match(.pg_line(out, "paridade"), "\\| N/A\\(tier\\)")
  expect_match(.pg_line(out, "schema_long"), "\\[DEFERRED M6\\] +\\| SKIP")
  expect_true(.pg_ascii(out$stdout))
  ## o teste `contrato` so passa se TZ/Sys.timezone eram UTC dentro do runner (o shell tinha Sao_Paulo)
})

test_that("[ac-1][CONTROL] PLUGINS falhando 1, sem dono 1, parity sem golden 1, tier invalido 2, runtime nao fixavel 7", {
  root <- withr::local_tempdir()
  rf <- pc_toy_gates_model(root, "toy-fail", tests = c("ok", "fail"))
  o <- .pg_run(rf)
  expect_identical(o$status, 1L)
  expect_match(.pg_line(o, "determinismo"), "FAIL")
  expect_match(paste(o$stderr, collapse = "\n"), "tools.gate_failed|Gate reprovado")
  expect_match(paste(o$stderr, collapse = "\n"), "determinismo")

  ru <- pc_toy_gates_model(root, "toy-unowned", tests = c("ok", "unowned"))
  o <- .pg_run(ru)
  expect_identical(o$status, 1L)
  expect_match(paste(o$stderr, collapse = "\n"), "sem dono|teste sem rotulo")

  rp <- pc_toy_gates_model(root, "toy-parity", tests = c("ok", "parity"))
  o <- .pg_run(rp, c("--tier", "parity"))
  expect_identical(o$status, 1L)
  expect_match(.pg_line(o, "paridade"), "\\| FAIL +\\| golden ausente")   # FAIL, nunca skip

  o <- .pg_run(rp, c("--tier", "xyz"))
  expect_identical(o$status, 2L)
  expect_length(o$stdout, 0L)
  expect_match(paste(o$stderr, collapse = "\n"), "Tier de gate desconhecido")

  rr <- file.path(root, "toy-rt.R")
  writeLines(sub("tier = \"fast\"", "tier = \"fast\", runtime_enforce = function() prevcarga_abort(\"runtime.tz_unfixable\", where = \"TZ\")",
                 readLines(rp), fixed = TRUE), rr)
  o <- .pg_run(rr)
  expect_identical(o$status, 7L)
  expect_length(o$stdout, 0L)
})

test_that("[ac-2] --format json: um unico documento JSON no envelope ADR-0040, result.gates, ASCII", {
  root <- withr::local_tempdir()
  r <- pc_toy_gates_model(root, "toy-30", tests = c("ok", "parity"))
  o <- .pg_run(r, c("--format", "json"))
  expect_identical(o$status, 0L)
  doc <- jsonlite::fromJSON(paste(o$stdout, collapse = "\n"), simplifyVector = FALSE)
  expect_true(all(c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version") %in% names(doc)))
  expect_true(doc$ok)
  g <- doc$result$gates
  expect_true(all(c("gate", "owner", "tier", "status", "detail") %in% names(g[[1L]])))
  st <- stats::setNames(vapply(g, `[[`, "", "status"), vapply(g, `[[`, "", "gate"))
  expect_identical(st[["contrato"]], "PASS"); expect_identical(st[["schema_long"]], "SKIP")
  expect_true(.pg_ascii(o$stdout))
  ## falha em json: ainda um unico documento, ok=false, exit_code 1
  rf <- pc_toy_gates_model(root, "toy-fail", tests = c("ok", "fail"))
  o <- .pg_run(rf, c("--format", "json"))
  doc <- jsonlite::fromJSON(paste(o$stdout, collapse = "\n"), simplifyVector = FALSE)
  expect_identical(o$status, 1L); expect_false(doc$ok); expect_identical(doc$error$id, "tools.gate_failed")
})

test_that("[ac-3] agregador: descobre pela topologia, pior codigo, runner ausente = FAIL tools.gate_failed", {
  base <- withr::local_tempdir()
  mroot <- pc_toy_gates_topology(base)
  pc_toy_gates_model(mroot, "toy-30", tests = "ok")
  pc_toy_gates_model(mroot, "toy-60", tests = c("ok", "fail"))
  o <- .pg_run(.pg_tool(), c("--models", "all", "--root", mroot))
  expect_identical(o$status, 1L)
  expect_match(grep("^toy-30 ", o$stdout, value = TRUE), "PASS \\(exit 0\\)")
  expect_match(grep("^toy-60 ", o$stdout, value = TRUE), "FAIL \\(exit 1\\)")
  expect_match(grep("^toy-none ", o$stdout, value = TRUE), "FAIL tools.gate_failed: runner ausente .*toy-none/tests/run_all_gates.R")
  o <- .pg_run(.pg_tool(), c("--models", "toy-30", "--root", mroot))
  expect_identical(o$status, 0L)
  expect_length(o$stdout, 1L)
  o <- .pg_run(.pg_tool(), c("--models", "nao-existe", "--root", mroot))
  expect_identical(o$status, 2L)
  o <- .pg_run(.pg_tool(), c("--models", "toy-30", "--root", mroot, "--tier", "xyz"))
  expect_identical(o$status, 2L)
})

test_that("[ac-3][CONTROL] plugin_gates.R: sem system(, bash, mclapply nem switch por nome", {
  src <- readLines(.pg_tool(), warn = FALSE)
  expect_false(any(grepl("system\\(|bash|mclapply|switch\\(", src)))
  expect_true(any(grepl("system2\\(", src)))
  expect_true(.pg_ascii(src))
})

test_that("[ac-4] packs descobertos: filtro applies_to, ordem radix, duplicado e pack sem funcao => tools.gate_unowned", {
  root <- withr::local_tempdir(); gd <- file.path(root, "gates")
  pc_toy_gate_packs(gd, list(
    zz_other = list(list(gate = "zz_only", applies_to = "toy-60", pass = TRUE)),
    mm_mid = list(list(gate = "mm_ok", applies_to = "all", pass = TRUE)),
    aa_toy = list(list(gate = "aa_pass", applies_to = "toy-30", pass = TRUE),
                  list(gate = "aa_fail", applies_to = "toy-30", pass = FALSE))))
  r <- pc_toy_gates_model(root, "toy-30", tests = "ok", gates_dir = gd)
  expect_length(readLines(r), 3L)
  o <- .pg_run(r)
  expect_identical(o$status, 1L)                                    # aa_fail e bloqueante
  gates <- sub(" .*", "", o$stdout[-1L])
  expect_identical(gates, c("contrato", "corte", "schema_long", "aa_pass", "aa_fail", "mm_ok"))
  expect_match(.pg_line(o, "aa_pass"), "PASS"); expect_match(.pg_line(o, "aa_fail"), "FAIL")
  expect_length(.pg_line(o, "zz_only"), 0L)
  ## CONTROL: renomear aa_toy -> zz_toy move seus gates para depois dos demais, sem mudar status
  pc_toy_gate_packs(gd, list(zz_toy = list(list(gate = "aa_pass", applies_to = "toy-30", pass = TRUE),
                                           list(gate = "aa_fail", applies_to = "toy-30", pass = FALSE))))
  unlink(file.path(gd, "aa_toy.R"))
  o <- .pg_run(r)
  expect_identical(sub(" .*", "", o$stdout[-1L]), c("contrato", "corte", "schema_long", "mm_ok", "aa_pass", "aa_fail"))
  expect_match(.pg_line(o, "aa_pass"), "PASS"); expect_match(.pg_line(o, "aa_fail"), "FAIL")
  ## gate duplicado entre dois packs
  pc_toy_gate_packs(gd, list(mm_mid = list(list(gate = "aa_pass", applies_to = "all", pass = TRUE))))
  o <- .pg_run(r)
  expect_identical(o$status, 1L)
  err <- paste(o$stderr, collapse = "\n")
  expect_match(err, "duplicado: mm_mid.R x zz_toy.R")
  ## pack sem a funcao (bad.R)
  gd2 <- file.path(root, "gates2")
  pc_toy_gate_packs(gd2, list(bad = NULL, aa_toy = list(list(gate = "aa_pass", applies_to = "toy-30", pass = TRUE))))
  r2 <- pc_toy_gates_model(root, "toy-31", tests = "ok", gates_dir = gd2)
  o <- .pg_run(r2)
  expect_identical(o$status, 1L)
  expect_match(paste(o$stderr, collapse = "\n"), "bad\\.R")
  expect_match(paste(o$stderr, collapse = "\n"), "Gate sem dono")
})

test_that("[ac-4] scripts standalone da pasta real de gates (quit()) nao sao carregados; runners nao carregam gates", {
  root <- withr::local_tempdir()
  r <- pc_toy_gates_model(root, "toy-30", tests = "ok")           # gates_dir padrao = scripts/tools/gates (m1..m4.R)
  o <- .pg_run(r)
  expect_identical(o$status, 0L)
  expect_length(grep("source\\(", readLines(r)), 1L)
  expect_match(grep("source\\(", readLines(r), value = TRUE), "plugin_gates\\.R")
  mine <- list.files(file.path(pc_root(), "models"), pattern = "^run_all_gates\\.R$", recursive = TRUE, full.names = TRUE)
  for (f in mine) expect_false(any(grepl("source\\(.*tools/gates", readLines(f))))
})
