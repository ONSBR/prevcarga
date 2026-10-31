#!/usr/bin/env Rscript
## run_all_gates.R -- runner dos gates do plugin dessem-linear: casca fina sobre prevcarga_gates_run (P.0h). Uso:
##   Rscript --vanilla models/dessem-linear/tests/run_all_gates.R [--tier fast|parity|parity-full] [--units foundation|fanout|<grupo>] [--format text|json]
## O runner chama SO prevcarga_gates_run, que fixa o runtime in-process antes de tudo, roda models/dessem-linear/tests/testthat e anexa
## os sub-gates descobertos em scripts/tools/gates/*.R (sort radix; applies_to inclui dessem-linear): M5/M6/M7 nunca editam este arquivo.
## Cada teste de testthat/ carrega o rotulo `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` com uma linha desta tabela (senao
## tools.gate_unowned). `oracle` = gate do oraculo (prevcargadessem3/models/dessem-linear/tests/run_all_gates.R; documentacao: a
## fundacao le gate, owner e tier). DEFERRED vira SKIP com dono ate o pack existir. O motor (models/_dessem-engine/tests/testthat) entra
## por inclusao no gate `motor_incluso`. Tiers: parity = grupo smoke do units.yaml; parity-full = smoke + foundation (+ fanout, DL.10).
## PRE-CONDICAO do fan-out (`--units fanout`): o relatorio da fundacao (tests/parity/foundation_report.json) tem de existir, estar verde
## (todas as unidades ok), com o sha256 registrado igual ao recalculado e com o golden de cada unidade igual ao do golden.lock vigente; senao exit 1 com mensagem ASCII.
.dl_runner <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]), winslash = "/")
.dl_repo <- dirname(dirname(dirname(dirname(.dl_runner))))
source(file.path(.dl_repo, "scripts", "tools", "plugin_gates.R"))

## gate, oracle (gate do oraculo), owner, tier. gate01/gate06 (LONG/y_true) = M6; gate03 (varias origens pela CLI) = M7.
.dl_gates <- list(
  list("00h", "-", "PLUGINS", "fast"), list("descritor", "-", "PLUGINS", "fast"), list("ingestao", "-", "PLUGINS", "fast"),
  list("nowcast_mm", "-", "PLUGINS", "fast"), list("treino_freeze", "-", "PLUGINS", "fast"),
  list("inferencia", "-", "PLUGINS", "fast"),
  list("gate02_escala_mw", "gate02", "PLUGINS", "fast"), list("gate04_exogena_params", "gate04", "PLUGINS", "fast"),
  list("gate05_clima_honesto", "gate05", "PLUGINS", "fast"), list("gate07_dataprev_o", "gate07", "PLUGINS", "fast"),
  list("gate08_contrato", "gate08", "PLUGINS", "fast"), list("gate09_dado_faltante", "gate09", "PLUGINS", "fast"),
  list("gate10_modelo_vazio", "gate10", "PLUGINS", "fast"), list("gate11_clamp_cross", "gate11", "PLUGINS", "fast"),
  list("gate13_flag12", "gate13", "PLUGINS", "fast"), list("gate14_corte_intradiario", "gate14", "PLUGINS", "fast"),
  list("gate15_sem_crash_13h", "gate15", "PLUGINS", "fast"), list("gate16_cauda_curta", "gate16", "PLUGINS", "fast"),
  list("gate17_split", "gate17", "PLUGINS", "fast"), list("gate18_sem_treino_circular", "gate18", "PLUGINS", "fast"),
  list("corte_treino", "-", "PLUGINS", "fast"), list("mm_head", "-", "PLUGINS", "fast"),
  list("robustez", "-", "PLUGINS", "fast"), list("sentinela", "-", "PLUGINS", "fast"),
  list("grep", "-", "PLUGINS", "fast"), list("no_env", "-", "PLUGINS", "fast"),
  list("portabilidade", "-", "PLUGINS", "fast"), list("motor_incluso", "-", "PLUGINS", "fast"),
  list("units_yaml", "-", "PLUGINS", "fast"), list("runner_controle", "-", "PLUGINS", "fast"),
  list("paridade", "-", "PLUGINS", "parity"), list("paridade_control", "-", "PLUGINS", "parity"),
  list("fanout_parity", "-", "PLUGINS", "parity-full"), list("exogena_oper", "-", "PLUGINS", "fast"),
  list("aditividade_epico", "-", "PLUGINS", "fast"), list("readme", "-", "PLUGINS", "fast"),
  list("gate01_schema_long", "gate01", "DEFERRED M6", "fast"), list("gate06_ytrue_real", "gate06", "DEFERRED M6", "fast"),
  list("gate03_sem_crash", "gate03", "DEFERRED M7", "fast"), list("fachada", "-", "DEFERRED M5", "fast"),
  list("cross_os", "-", "DEFERRED M8", "fast"))
gate_table <- data.frame(
  gate = vapply(.dl_gates, `[[`, "", 1L), oracle = vapply(.dl_gates, `[[`, "", 2L),
  owner = vapply(.dl_gates, `[[`, "", 3L), tier = vapply(.dl_gates, `[[`, "", 4L), stringsAsFactors = FALSE)

## --- pre-condicao do fan-out: foundation_report.json verde e integro ------------------------------------------------------
.dl_sha <- function(x) digest::digest(x, algo = "sha256", serialize = FALSE)

## sha256 do bloco `units` do relatorio: JSON canonico (chaves em ordem radix, numeros em 15 digitos), o mesmo que test-parity.R grava
.dl_report_units_sha <- function(units) {
  canon <- function(x) {
    if (is.list(x)) { x <- lapply(x, canon); if (!is.null(names(x))) x <- x[order(names(x), method = "radix")] }
    x
  }
  .dl_sha(as.character(jsonlite::toJSON(canon(units), auto_unbox = TRUE, null = "null", digits = 15, na = "null")))
}

## NULL (ok) ou o motivo ASCII da recusa. O vinculo com o golden e por unidade (o `golden_native_sha256` do relatorio = sha do native.rds no
## golden.lock vigente): o lock cresce com o fan-out sem invalidar o relatorio da fundacao; golden regerado de uma unidade o invalida.
.dl_report_problem <- function(path, lock) {
  if (!file.exists(path)) return(sprintf("relatorio ausente: %s (rode --tier parity-full --units foundation)", path))
  rp <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(rp) || !is.list(rp$units) || !length(rp$units)) return(sprintf("relatorio ilegivel ou sem unidades: %s", path))
  if (!isTRUE(rp$all_ok) || !all(vapply(rp$units, function(u) isTRUE(u$ok), NA)))
    return(sprintf("relatorio vermelho (unidade nao ok): %s", path))
  if (!identical(rp$sha256, .dl_report_units_sha(rp$units)))
    return(sprintf("sha256 do relatorio difere do registrado: %s", path))
  lk <- if (file.exists(lock)) tryCatch(jsonlite::read_json(lock, simplifyVector = FALSE), error = function(e) NULL) else NULL
  if (is.list(lk$units))
    for (u in rp$units)
      if (!identical(u$golden_native_sha256, lk$units[[u$unit]]$files[["native.rds"]]))
        return(sprintf("relatorio de outro golden (unidade %s; regere o relatorio): %s", u$unit, path))
  NULL
}

.dl_precondition <- function(argv) {
  i <- which(argv == "--units")
  units <- if (length(i) && i[[1L]] < length(argv)) argv[[i[[1L]] + 1L]] else sub("^--units=", "", grep("^--units=", argv, value = TRUE)[1L])
  if (!identical(units, "fanout")) return(invisible(NULL))
  if (!is.null(prevcarga_with_errors(prevcarga_runtime_enforce())$cond)) return(invisible(NULL))   # runtime primeiro; exit 7 e do prevcarga_gates_run
  pdir <- file.path(.dl_repo, "models", "dessem-linear", "tests", "parity")
  why <- .dl_report_problem(file.path(pdir, "foundation_report.json"), file.path(pdir, "golden.lock"))
  if (is.null(why)) return(invisible(NULL))
  fmt <- if (length(j <- which(argv == "--format")) && j[[1L]] < length(argv)) argv[[j[[1L]] + 1L]] else "text"
  out <- prevcarga_with_errors(prevcarga_abort("tools.gate_failed", where = "foundation_report.json",
                                               details = list(gate = "requires_report", reason = why)))
  if (identical(fmt, "json")) writeLines(.pc_gates_ascii(prevcarga_render_error(out$cond, "json", verb = "plugin-gates")), con = stdout())
  else writeLines(.pc_gates_ascii(c(prevcarga_render_error(out$cond, "text", verb = "plugin-gates"), paste0("motivo: ", why))), con = stderr())
  quit(save = "no", status = out$exit_code)
}
.dl_precondition(commandArgs(TRUE))

prevcarga_gates_run(gate_table, tier = NULL, model = "dessem-linear")
