## test-fanout.R -- fan-out do dessem-linear contra o ORACULO (DL.10): exogena MAX e MXMN x SECO (origens da fundacao) e MED/MAX/MXMN x uma
## area-folha por subsistema (S = RS, NE = CE, N = PA) em 2024-10-15. Mesmo harness, mesmo driver (tests/parity/oracle_driver.R) e mesmas
## regras da fundacao (DL.9): a comparacao e EXATA (<= 1e-5 MW) em O..O+8 salvo o intervalo NOMEADO da divergencia deliberada (hora incompleta
## descartada tambem no clima previsto), medido por unidade e fechado pelo CONTROL de hora parcial emulada. Reusa os ajudantes `dlp_*` de
## test-parity.R (avaliados aqui sem rodar os testes dele). PRE-CONDICAO: foundation_report.json verde e com o sha256 declarado em
## `requires_report` de cada unidade (`dl_require_foundation`). Roda so em --tier parity-full (grupo `fanout`, default de `--units`).
## Golden ausente/divergente FALHA pedindo regerar (nunca skip). Grava tests/parity/fanout_report.json (sha256) quando todo o grupo roda.
suppressWarnings(suppressMessages(library(testthat)))

## ajudantes de test-parity.R (todo objeto de topo exceto os test_that); nada e executado alem de definicoes
local({
  ex <- parse(file.path(testthat::test_path(), "test-parity.R"), keep.source = FALSE)
  for (e in ex) if (!(is.call(e) && identical(e[[1L]], quote(test_that)))) eval(e, envir = parent.env(environment()))
})

DLF_GROUP <- "fanout"
DLF_REPORT <- function() file.path(DLP_PARITY(), "fanout_report.json")
DLF_AREAS <- c(S = "RS", NE = "CE", "N" = "PA")           # uma area-folha por subsistema (alem de SECO)
DLF_SECO_ORIGINS <- sprintf("2024-10-%02d", c(1L, 8L, 15L, 22L, 29L))
DLF_VARIANTS <- c("MED", "MAX", "MXMN")
DLF_MODEL_KEY <- c(MED = "Linear.TmpMedia", MAX = "Linear.TmpMaxima", MXMN = "Linear.TmpMaxMin")
DLF_CAUSE <- paste("hora incompleta descartada tambem na previsao de clima (divergencia deliberada, Gabriel 2026-10-30); emissao 00Z termina",
                   "em O+9 23:00 com UM slot na hora 23: o dia fica curto e e persistido, efeito so nas ultimas ~3 h do ultimo dia entregue")

## Pre-condicao do fan-out: relatorio da fundacao presente, verde (todas as unidades ok), integro (sha256 do bloco `units` recomputado) e com
## o sha256 registrado. Aborta com mensagem ASCII (o runner converte em exit 1); devolve o relatorio.
dl_require_foundation <- function(report_path, expected_sha) {
  if (!file.exists(report_path)) stop("fanout: foundation_report.json ausente; rode --tier parity-full --units foundation", call. = FALSE)
  rp <- jsonlite::read_json(report_path, simplifyVector = FALSE)
  oks <- vapply(rp$units, function(x) isTRUE(x$ok), NA)
  if (!isTRUE(rp$all_ok) || !length(oks) || !all(oks))
    stop(sprintf("fanout: relatorio da fundacao vermelho (%d unidade(s) nao-ok)", sum(!oks)), call. = FALSE)
  got <- dlp_sha_obj(rp$units)
  if (!identical(got, rp$sha256) || !identical(rp$sha256, expected_sha))
    stop(sprintf("fanout: relatorio da fundacao invalido (sha %s, registrado %s)", substr(got, 1L, 12L), substr(as.character(expected_sha), 1L, 12L)),
         call. = FALSE)
  invisible(rp)
}
dl_foundation_sha <- function(units = dlp_group_units(DLF_GROUP)) {
  s <- unique(vapply(units, function(u) u$requires_report$sha256, ""))
  if (length(s) != 1L) stop("fanout: as unidades declaram requires_report.sha256 diferentes", call. = FALSE)
  s
}

dlf_units <- function() {
  us <- dlp_units(DLF_GROUP)
  us
}
dlf_run <- function(u, params = list(exogena = u$variant)) dlp_run(u, "split", params = params)

## o grupo roda so no tier parity-full e so se `--units` nao pediu OUTRO grupo
dlf_gate <- function() {
  dlp_gate_tier("parity-full")
  g <- if (exists("pc_gate_units", mode = "function")) pc_gate_units(NULL) else NULL
  if (!is.null(g) && !identical(g, DLF_GROUP)) testthat::skip(sprintf("[units] fan-out so roda com --units %s (pedido: %s)", DLF_GROUP, paste(g, collapse = ",")))
  invisible(TRUE)
}

dlf_entry <- function(u, r, lock, emu) {
  e <- dlp_report_entry(u, r, lock, NULL, emu)
  e$sha256 <- NULL
  e$variant <- u$variant
  e$sha256 <- dlp_sha_obj(e)
  e
}

dlf_divergences <- function(entries) {
  out <- list()
  for (e in entries) {
    if (!is.null(e$tail_max_abs_delta) && e$tail_max_abs_delta > DLP_TOL)
      out[[length(out) + 1L]] <- list(id = e$unit, causa = DLF_CAUSE, isolavel = isTRUE(e$emulated_ok),
        justificativa = sprintf("max|D| %.3g MW so em %s..%s (fora: %.3g); CONTROL de hora parcial emulada fecha tudo em %.3g (<= 1e-5)",
                                e$tail_max_abs_delta, e$tail_intervals[[1L]], e$tail_intervals[[2L]], e$max_abs_delta, e$emulated_partial_hour_max_abs_delta))
  }
  out
}

dlf_report_write <- function(entries, found_sha, path = DLF_REPORT()) {
  units <- unname(entries[order(names(entries), method = "radix")])
  div <- dlf_divergences(units)
  rep <- list(model = DLP_MODEL, group = DLF_GROUP, tolerance = DLP_TOL, foundation_sha256 = found_sha,
              oracle_commit = jsonlite::read_json(DLP_LOCK())$oracle_commit,
              all_ok = all(vapply(units, function(e) isTRUE(e$ok) && isTRUE(e$emulated_ok), NA)) && all(vapply(div, function(d) isTRUE(d$isolavel), NA)),
              units = units, divergences = div)
  rep$sha256 <- dlp_sha_obj(units)
  txt <- jsonlite::toJSON(rep, auto_unbox = TRUE, null = "null", digits = 15, na = "null", pretty = TRUE)
  writeBin(charToRaw(paste0(paste(as.character(txt), collapse = "\n"), "\n")), path)
  invisible(rep)
}

## --------------------------------------------------------------------------------------------------------------
test_that("[PLUGINS] fanout_parity: MAX/MXMN x SECO e MED/MAX/MXMN x areas reproduzem o oraculo (max|D| <= 1e-5; divergencia nomeada medida e fechada pelo CONTROL)", {
  dlf_gate()
  us <- dlf_units()
  expect_gt(length(us), 0L)
  ## PRE-CONDICAO: relatorio da fundacao verde e com o sha registrado, antes de qualquer unidade
  dl_require_foundation(DLP_REPORT(), dl_foundation_sha(dlp_group_units(DLF_GROUP)))
  lock <- jsonlite::read_json(DLP_LOCK(), simplifyVector = FALSE)
  entries <- list()
  for (u in us) {
    if (!dlp_need_golden(u)) next
    run <- dlf_run(u)
    gold <- dlp_golden(u)
    r <- dlp_compare(run, gold, u)
    expect_identical(r$layout, "dessem_semihourly_48")
    expect_identical(r$day0_source, "native")
    expect_true(isTRUE(r$shape_ok) && isTRUE(r$na_ok), info = u$id)
    expect_identical(dim(run$env$values), c(9L, 48L))
    expect_identical(dim(gold$native$values), c(10L, 48L))
    expect_identical(as.integer(r$delivery_missing), 0L)
    expect_true(isTRUE(r$cut_ok), info = u$id)
    expect_identical(as.integer(r$fora_da_faixa), 48L)
    expect_identical(as.integer(r$fora_da_faixa_long), 48L)
    expect_identical(run$env$available_until, u$available_until)
    expect_identical(gold$native$meta$exogena, u$variant)                              # o golden e da variante da unidade
    expect_lte(r$max_abs_delta, DLP_TOL)                                               # Delta = 0 (1e-5) fora do intervalo nomeado
    expect_true(isTRUE(r$ok), info = dlp_report_line(r))
    expect_true(all(is.finite(run$env$values)) && all(run$env$values > 0), info = u$id)
    emu <- dlp_emulated(run, gold, u)                                                  # CONTROL: com a regra do oraculo a cauda tambem fecha
    expect_gt(emu$added, 0L)
    expect_lte(emu$r$max_abs_delta, DLP_TOL)
    cat(sprintf("FANOUT %s max|D|=%.3e nomeada=%.3e emu=%.3e\n", u$id, r$max_abs_delta, r$tail_max_abs_delta, emu$r$max_abs_delta), file = stderr())
    entries[[u$id]] <- dlf_entry(u, r, lock, emu$r)
  }
  if (all(vapply(dlp_group_units(DLF_GROUP), `[[`, "", "id") %in% names(entries)) && !length(getOption("prevcarga.parity_only"))) {
    rpt <- dlf_report_write(entries, dl_foundation_sha())
    expect_true(file.exists(DLF_REPORT()))
    expect_true(isTRUE(rpt$all_ok))
    message(sprintf("fanout_report all_ok=%s sha256=%s unidades=%d max_abs=%.3g divergencias=%d", rpt$all_ok, rpt$sha256, length(entries),
                    max(vapply(entries, function(e) e$max_abs_delta, 0)), length(rpt$divergences)))
  }
})

test_that("[PLUGINS] fanout_parity: LIN_MED, LIN_MAX e LIN_MXMN na mesma origem e area: envelopes 9x48 na grade de entrega, y_pred distintos dois a dois, artefatos com variante e model_key", {
  dlf_gate()
  skip_if(length(getOption("prevcarga.parity_only")) > 0L, "opcao de desenvolvimento prevcarga.parity_only ativa")
  for (a in unname(DLF_AREAS)) {
    runs <- lapply(stats::setNames(DLF_VARIANTS, DLF_VARIANTS), function(v) dlf_run(dlp_unit(sprintf("%s-%s-2024-10-15", tolower(v), tolower(a)))))
    for (v in DLF_VARIANTS) {
      env <- runs[[v]]$env
      expect_identical(dim(env$values), c(9L, 48L), info = paste(a, v))
      expect_identical(runs[[v]]$art$variant %||% runs[[v]]$art$payload$variant, v, info = paste(a, v))
      expect_identical(runs[[v]]$art$model_key %||% runs[[v]]$art$payload$model_key, DLF_MODEL_KEY[[v]], info = paste(a, v))
      s <- runs[[v]]$setup
      grid <- dl_lib$prevcarga_delivery_grid(s$cuts, 9L)
      expect_identical(env$delivery$interval_end, grid, info = paste(a, v))             # mesma grade prevcarga_delivery_grid(cuts, 9)
    }
    for (p in utils::combn(DLF_VARIANTS, 2L, simplify = FALSE))
      expect_gt(max(abs(runs[[p[[1L]]]]$env$delivery$y_pred - runs[[p[[2L]]]]$env$delivery$y_pred)), DLP_TOL, label = paste(a, p[[1L]], p[[2L]]))
  }
})

test_that("[PLUGINS] fanout_parity: CONTROLs -- pre-condicao do relatorio, golden ausente/adulterado => regere, variante trocada acusa > 1e-5", {
  dlf_gate()
  us <- dlp_group_units(DLF_GROUP)
  sha <- dl_foundation_sha(us)
  ## pre-condicao (AC1): relatorio ausente, vermelho ou com sha diferente recusam; o real segue
  expect_error(dl_require_foundation(file.path(withr::local_tempdir(), "nao-existe.json"), sha), "ausente", fixed = TRUE)
  expect_error(dl_require_foundation(DLP_REPORT(), strrep("0", 64L)), "invalido", fixed = TRUE)
  red <- jsonlite::read_json(DLP_REPORT(), simplifyVector = FALSE)
  red$units[[1L]]$ok <- FALSE; red$all_ok <- FALSE
  f <- withr::local_tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(red, auto_unbox = TRUE, null = "null", digits = 15), f)
  expect_error(dl_require_foundation(f, sha), "vermelho", fixed = TRUE)
  expect_error(dl_require_foundation(f, sha), "^[ -~]+$")                                 # mensagem ASCII
  expect_identical(dl_require_foundation(DLP_REPORT(), sha)$sha256, sha)
  ## golden ausente / adulterado de uma unidade do fan-out => mensagem de regerar (AC3), nunca skip
  u <- us[[1L]]
  if (dlp_need_golden(u)) {
    expect_match(dlp_check_golden(u, root = withr::local_tempdir()), DLP_REGEN, fixed = TRUE)
    lk <- jsonlite::read_json(DLP_LOCK(), simplifyVector = FALSE)
    tamper <- function(f) {
      p <- tempfile(fileext = ".lock")
      on.exit(unlink(p), add = TRUE)
      writeLines(jsonlite::toJSON(f(lk), auto_unbox = TRUE, null = "null", digits = NA), p)
      dlp_check_golden(u, lock = p)
    }
    m1 <- tamper(function(l) { l$units[[u$id]]$fingerprint$packages$WeightSVM <- "0.0.0"; l })
    expect_match(m1, DLP_REGEN, fixed = TRUE); expect_match(m1, u$id, fixed = TRUE)
    m2 <- tamper(function(l) { l$units[[u$id]]$files[["native.rds"]] <- strrep("0", 64L); l })
    expect_match(m2, DLP_REGEN, fixed = TRUE)
    m3 <- tamper(function(l) { l$units[[u$id]] <- NULL; l })
    expect_match(m3, DLP_REGEN, fixed = TRUE)
    ## o golden e da variante da unidade: o MED de uma area contra o golden MAX da mesma area acusa > 1e-5 (CONTROL com dente)
    skip_if(length(getOption("prevcarga.parity_only")) > 0L, "opcao de desenvolvimento prevcarga.parity_only ativa")
    umed <- dlp_unit("med-rs-2024-10-15"); umax <- dlp_unit("max-rs-2024-10-15")
    if (dlp_need_golden(umax)) {
      bad <- dlp_compare(dlf_run(umed), dlp_golden(umax), umax)
      expect_gt(bad$max_abs_delta, DLP_TOL); expect_false(isTRUE(bad$ok)); expect_true(isTRUE(bad$shape_ok))
      message(sprintf("fanout control MED x golden MAX (RS) max|D|=%.3g", bad$max_abs_delta))
    }
  }
})
