## test-engine-golden.R -- DL.1: equivalencia numerica com o motor do oraculo (AC11, AC12) e formula sem `mm` (AC9).
## O oraculo (/opt/ons-source/prevcargadessem3, so leitura) roda em processo filho; sem ele ou sem R 4.4.3 o golden e pulado.

ORACLE_VENDOR <- "/opt/ons-source/prevcargadessem3/models/dessem-linear/vendor"
ORACLE_RSCRIPT <- "/opt/R/4.4.3/bin/Rscript"
MODEL_KEY <- "Linear.TmpMedia"
ORIGIN <- "2019-02-28"

skip_if_no_oracle <- function() {
  skip_if_not(file.exists(file.path(ORACLE_VENDOR, "_load_engine.R")) && file.exists(ORACLE_RSCRIPT),
              "oraculo (prevcargadessem3 + R 4.4.3) ausente")
}

run_oracle <- function(seed, deck) {
  dk <- tempfile(fileext = ".rds"); out <- tempfile(fileext = ".rds")
  saveRDS(deck, dk)
  child <- file.path(testthat::test_path(), "fixtures", "oracle_child.R")
  args <- c(ORACLE_VENDOR, dk, out, seed, ORIGIN, MODEL_KEY)
  expr <- sprintf(".oracle_args <- %s; source(%s)", paste(deparse(args), collapse = ""),
                  deparse(normalizePath(child, winslash = "/")))
  rc <- suppressWarnings(system2(ORACLE_RSCRIPT, c("--vanilla", "-e", shQuote(expr)), stdout = FALSE, stderr = FALSE))
  expect_identical(rc, 0L)
  readRDS(out)
}

## motor clean-room (env do loader) sobre o mesmo deck
run_clean <- function(seed, deck, eng, offset_min = 60L) {
  dk <- suppressMessages(eng$TransformaDeck(eng_model_deck(deck, offset_min), ORIGIN, "10DIAS", tz = "Etc/GMT+3"))
  cfg <- eng$getConfigModel(MODEL_KEY)
  if (!identical(seed, "none")) set.seed(as.integer(seed))
  hor <- suppressMessages(eng$processaModelosDMPerfil(dk, cfg))
  list(seriecomp = dk$SERIECOMP, horaria = hor, diasprev = dk$DIASPREV,
       semi = eng$desagregaModeloSH("M", data.table::copy(hor)[, Modelo := "M"]), eng = eng, dk = dk, cfg = cfg)
}

maxdiff <- function(a, b) max(abs(a$Carga - b$Carga))

test_that("ac-11/12 processaModelosDMPerfil e desagregaModeloSH == oraculo (<= 1e-5 MW) no deck fixo", {
  skip_if_no_oracle()
  deck <- eng_deck()
  eng <- eng_env(eng_load(eng_plugin_root()))
  ref <- run_oracle("42", deck)
  mine <- run_clean("42", deck, eng)
  expect_equal(mine$diasprev, ref$diasprev)
  expect_identical(nrow(mine$horaria), length(ref$diasprev) * 24L)
  expect_equal(mine$seriecomp, ref$seriecomp, tolerance = 0)
  expect_identical(lubridate::tz(mine$horaria$DataHora), "Etc/GMT+3")
  ## o motor devolve o frame de modelagem; o despachante devolve ao frame de exibicao (+60 min) e o oraculo
  ## ja entrega a hora de inicio: mesmos instantes
  expect_identical(mine$horaria$DataHora, ref$horaria$DataHora)
  expect_true(all(is.finite(mine$horaria$Carga)))
  expect_lte(maxdiff(mine$horaria, ref$horaria), 1e-5)
  ## AC12: desagregacao semi-horaria
  expect_identical(nrow(mine$semi), 2L * nrow(mine$horaria))
  expect_identical(mine$semi$Passo, seq_len(2L * nrow(mine$horaria)))
  expect_identical(eng_semi_stamp(mine$semi), ref$semi$DataHora)
  expect_lte(maxdiff(mine$semi, ref$semi), 1e-5)
  ## CONTROL (AC12): splinefun fmm no lugar de monoH.FC afasta a saida
  fmm <- eval(parse(text = gsub("monoH.FC", "fmm", paste(deparse(eng$desagregaModeloSH), collapse = "\n"), fixed = TRUE)),
              envir = eng)
  alt <- fmm("M", data.table::copy(mine$horaria)[, Modelo := "M"])
  expect_gt(maxdiff(alt, ref$semi), 1e-5)
  ## CONTROL (AC2 DL.3): motor alimentado sem o to_model_frame do despachante (offset 0) completa a montagem da
  ## serie, mas a carga fica 1 h deslocada em relacao ao oraculo (series diferem muito alem de 1e-5)
  dk0 <- suppressMessages(eng$TransformaDeck(eng_model_deck(deck, 0L), ORIGIN, "10DIAS", tz = "Etc/GMT+3"))
  expect_s3_class(dk0$SERIECOMP, "data.table")
  a <- dk0$SERIECOMP[Prev == FALSE][order(DataHora)]
  b <- ref$seriecomp[Prev == FALSE][order(DataHora)]
  expect_false(identical(a$DataHora, b$DataHora) && isTRUE(all.equal(a$Carga, b$Carga, tolerance = 0)))
  m <- merge(a[, .(DataHora, Carga)], b[, .(DataHora, Carga)], by = "DataHora")
  expect_gt(max(abs(m$Carga.x - m$Carga.y), na.rm = TRUE), 1e-5)
})

test_that("ac-11 CONTROL: outra semente no clean-room afasta a saida do oraculo (> 1e-5 MW)", {
  skip_if_no_oracle()
  deck <- eng_deck()
  eng <- eng_env(eng_load(eng_plugin_root()))
  ref <- run_oracle("42", deck)
  other <- run_clean("7", deck, eng)
  expect_gt(maxdiff(other$horaria, ref$horaria), 1e-5)
})

test_that("ac-9 obterEntradaFormulaDM do motor e a do oraculo: formula sem o termo mm", {
  deck <- eng_deck()
  eng <- eng_env(eng_load(eng_plugin_root()))
  dk <- suppressMessages(eng$TransformaDeck(eng_model_deck(deck), ORIGIN, "10DIAS", tz = "Etc/GMT+3"))
  cfg <- eng$getConfigModel(MODEL_KEY)
  serieDM <- eng$aggregaSerie(serieHist = data.table::copy(dk$SERIECOMP), tipotemp = cfg$exogenaOper, normaliza = TRUE,
                              constnorm = eng$getNormalizaCarga(cfg$normalizacao$dm$carga, dk$SERIECOMP[Prev == FALSE]),
                              constnormtemp = eng$getNormalizaExogena(cfg$normalizacao$dm$exogena, dk$SERIECOMP[Prev == FALSE]))
  inp <- eng$obterEntradaFormulaDM(dk$DIASPREV[[1L]], serieDM, cfg)
  expect_false("mm" %in% all.vars(inp$formulaMDL))
  expect_false("mm" %in% names(inp$dadosEntrada$serieDiariaModelo))
  expect_true(all(c("Exo_Temperatura", "LAGEXO_Temperatura_1") %in% all.vars(inp$formulaMDL)) ||
              any(grepl("LAGEXO", all.vars(inp$formulaMDL))))
})

## ---- DL.4: golden ISOLADO do motor (plugin_golden.R --model _dessem-engine; .state/golden + golden.lock) ----------------
## tier fast: units.yaml valido pelo leitor do P.0f e a conferencia do golden.lock com dente (sem rodar o motor).
## tier parity: o motor clean-room roda sobre o MESMO deck cru do golden (frame de modelagem) e e comparado ao oraculo.

eng_tools <- local({
  e <- new.env(parent = eng_lib)
  sys.source(file.path(eng_root(), "scripts", "tools", "plugin_golden.R"), envir = e)
  e
})

test_that("ac-1 units.yaml: MED/MAX/MXMN SECO + robustez GO/TON/flag-12, history_start = O - 1825, valido pelo plugin_golden", {
  spec <- eng_lib$prevcarga_with_errors(eng_tools$pc_golden_units(file.path(eng_parity_dir(), "units.yaml")))
  expect_null(spec$cond)
  us <- spec$value$units
  expect_identical(vapply(us, `[[`, "", "id"), c(vapply(eng_units("engine"), `[[`, "", "id"), vapply(eng_units("robustez"), `[[`, "", "id")))
  expect_identical(names(spec$value$groups), c("engine", "robustez"))
  eng <- eng_units("engine"); rb <- eng_units("robustez")
  expect_identical(vapply(eng, `[[`, "", "variant"), c("MED", "MAX", "MXMN"))
  expect_true(all(vapply(eng, function(u) identical(u$area, "SECO") && identical(u$origin, "2024-10-15"), NA)))
  expect_identical(vapply(eng, `[[`, "", "model_key"), c("Linear.TmpMedia", "Linear.TmpMaxima", "Linear.TmpMaxMin"))
  expect_identical(vapply(rb, function(u) paste(u$area, u$origin), ""), c("GO 2024-10-05", "SP 2024-11-19", "SP 2024-12-22"))
  expect_true(all(vapply(rb, function(u) identical(u$variant, "MED"), NA)))
  for (u in c(eng, rb)) {
    ## 1825 = O - 1825 (jobs de producao); so o caso TON/cross usa 730 (com 1825 a amostra do dia especial nao e pequena)
    expect_identical(as.integer(u$history_days), if (grepl("^robustez_ton", u$id)) 730L else 1825L, info = u$id)
    expect_identical(u$oracle_env$EXOGENA, u$variant, info = u$id)
    expect_true(grepl("T23:30$", u$available_until), info = u$id)
  }
  for (x in spec$value$units)
    expect_identical(format(x$origin - lubridate::days(x$history_days)),
                     format(as.Date(x$origin) - if (grepl("^robustez_ton", x$id)) 730L else 1825L))
})

test_that("ac-1 CONTROL: units.yaml com campo desconhecido (`calls`) ou variante fora do conjunto => recusado pelo leitor", {
  doc <- yaml::read_yaml(file.path(eng_parity_dir(), "units.yaml"))
  doc$units[[1L]]$calls <- c("processaModelosDMPerfil", "desagregaModeloSH")
  f <- tempfile(fileext = ".yaml"); yaml::write_yaml(doc, f)
  cond <- eng_lib$prevcarga_with_errors(eng_tools$pc_golden_units(f))$cond
  expect_identical(cond$id, "tools.bad_units")
  expect_identical(cond$where, "engine_med_seco_20241015.calls")
})

## golden sintetico em tempdir: lock + arquivos + invocation.json (o runtime do filho e o do proprio Rscript do teste)
eng_fake_golden <- function(variant = "MED") {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  u <- list(id = "u1", variant = variant, area = "SECO", origin = "2024-10-15")
  d <- eng_unit_dir(u, root); dir.create(d, recursive = TRUE)
  saveRDS(list(x = 1), file.path(d, "native.rds")); writeLines("{}", file.path(d, "long.parquet"))
  env <- c("TZ=UTC", "LC_ALL=C.UTF-8")
  writeLines(jsonlite::toJSON(list(env = env), auto_unbox = TRUE), file.path(d, "invocation.json"))
  rs <- file.path(R.home("bin"), "Rscript")
  fp <- eng_tools$pc_runtime_fingerprint(c("data.table"), rs, env)
  files <- vapply(c("native.rds", "long.parquet", "invocation.json"), function(f) digest::digest(file.path(d, f), algo = "sha256", file = TRUE), "")
  lock <- file.path(root, "golden.lock")
  put <- function(fp2 = fp, files2 = files) writeLines(jsonlite::toJSON(list(units = list(u1 = list(files = as.list(files2), fingerprint = fp2))),
                                                                       auto_unbox = TRUE, null = "null"), lock)
  put()
  list(u = u, root = root, lock = lock, rs = rs, d = d, fp = fp, files = files, put = put)
}

test_that("ac-1 CONTROL (fast): golden.lock ausente, unidade ausente, arquivo ausente, sha256 ou fingerprint divergente => regere o golden", {
  fg <- eng_fake_golden()
  expect_null(eng_check_golden(fg$u, fg$root, fg$lock, fg$rs))
  expect_match(eng_check_golden(fg$u, fg$root, file.path(fg$root, "nao-existe.lock"), fg$rs), "golden.lock ausente.*regere o golden")
  expect_match(eng_check_golden(list(id = "u2", variant = "MED", area = "SECO", origin = "2024-10-15"), fg$root, fg$lock, fg$rs),
               "ausente do golden.lock.*regere o golden")
  ## sha256 do arquivo diferente do lock
  saveRDS(list(x = 2), file.path(fg$d, "native.rds"))
  expect_match(eng_check_golden(fg$u, fg$root, fg$lock, fg$rs), "sha256 de native.rds difere do golden.lock.*regere o golden")
  saveRDS(list(x = 1), file.path(fg$d, "native.rds"))
  expect_null(eng_check_golden(fg$u, fg$root, fg$lock, fg$rs))
  ## arquivo do golden ausente
  file.rename(file.path(fg$d, "long.parquet"), file.path(fg$d, "long.bak"))
  expect_match(eng_check_golden(fg$u, fg$root, fg$lock, fg$rs), "long.parquet ausente.*regere o golden")
  file.rename(file.path(fg$d, "long.bak"), file.path(fg$d, "long.parquet"))
  ## fingerprint: versao do R e pacote diferentes do gravado
  bad <- fg$fp; bad$R <- "0.0.1"
  fg$put(bad)
  expect_match(eng_check_golden(fg$u, fg$root, fg$lock, fg$rs), "fingerprint R difere do lock.*regere o golden")
  bad <- fg$fp; bad$packages[["data.table"]] <- "0.0.1"
  fg$put(bad)
  expect_match(eng_check_golden(fg$u, fg$root, fg$lock, fg$rs), "fingerprint do pacote data.table difere do lock")
})

test_that("ac-1 golden.lock versionado cobre as 6 unidades do units.yaml (arquivos, commit do oraculo, fingerprint, digest dos dados)", {
  lk <- jsonlite::read_json(file.path(eng_parity_dir(), "golden.lock"), simplifyVector = FALSE)
  expect_identical(lk$model, "_dessem-engine")
  ids <- vapply(eng_units("all"), `[[`, "", "id")
  expect_setequal(names(lk$units), ids)
  for (id in ids) {
    e <- lk$units[[id]]
    expect_setequal(names(e$files), c("native.rds", "long.parquet", "invocation.json"))
    expect_true(all(grepl("^[0-9a-f]{64}$", unlist(e$files))), info = id)
    expect_true(grepl("^[0-9a-f]{40}$", e$oracle_commit), info = id)
    expect_true(all(c("R", "platform", "blas", "packages") %in% names(e$fingerprint)), info = id)
    expect_true(all(c("WeightSVM", "e1071") %in% names(e$fingerprint$packages)), info = id)
    expect_true(nzchar(e$data_digest$aggregate), info = id)
  }
})

eng_compare <- function(r, g, info) {
  expect_identical(nrow(r$hourly), nrow(g$hourly), info = info)
  expect_identical(nrow(r$semihourly), nrow(g$semihourly), info = info)
  expect_identical(eng_wall(r$hourly$DataHora), eng_wall(g$hourly$DataHora), info = info)
  expect_identical(eng_wall(r$semihourly$DataHora), eng_wall(g$semihourly$DataHora), info = info)
  expect_identical(r$semihourly$Passo, g$semihourly$Passo, info = info)
  expect_true(all(is.finite(r$hourly$Carga)), info = info)
  list(hourly = eng_maxdiff(r$hourly, g$hourly), semihourly = eng_maxdiff(r$semihourly, g$semihourly))
}

test_that("ac-1 [parity] motor isolado reproduz o oraculo (<= 1e-5 MW) em processaModelosDMPerfil e desagregaModeloSH nas 3 variantes SECO", {
  eng_require_parity()
  eng <- eng_env(eng_load(eng_plugin_root()))
  for (u in eng_units("engine")) {
    g <- eng_golden(u)
    if (is.null(g)) next
    expect_identical(g$model_key, u$model_key)
    expect_identical(g$variant, u$variant)
    r <- eng_run_cached(eng, g, u)
    expect_identical(r$guards, character(), info = u$id)
    d <- eng_compare(r, g, u$id)
    expect_identical(nrow(r$hourly), 9L * 24L)
    expect_identical(nrow(r$semihourly), 9L * 48L)
    expect_lte(d$hourly, ENG_TOL); expect_lte(d$semihourly, ENG_TOL)
  }
})

test_that("ac-1 [parity] CONTROL: deslocamento do despachante trocado por offset 0, ou set.seed(42L) removido => > 1e-5 MW", {
  eng_require_parity()
  eng <- eng_env(eng_load(eng_plugin_root()))
  u <- eng_units("engine")[[1L]]
  g <- eng_golden(u)
  if (is.null(g)) return(invisible(NULL))
  base <- eng_run_cached(eng, g, u)
  expect_lte(eng_maxdiff(base$semihourly, g$semihourly), ENG_TOL)
  ## offset 0 = motor alimentado sem o to_model_frame do despachante: a serie de carga fica 1 h defasada (diferenca grande ja na
  ## montagem do deck) e a corrida COMPLETA ou aborta na grade de lags, ou entrega saida > 1e-5 MW distante do oraculo
  expect_lte(eng_series_diff(eng, g, 60L)$max, ENG_TOL)
  sd0 <- eng_series_diff(eng, g, 0L)
  expect_gt(sd0$n, 1000L); expect_gt(sd0$max, ENG_TOL)
  id0 <- tryCatch(eng_run_cached(eng, g, u, offset_min = 0L), error = function(e) e)
  if (!inherits(id0, "error")) {
    expect_gt(eng_maxdiff(id0$hourly, g$hourly), ENG_TOL)
    expect_gt(eng_maxdiff(id0$semihourly, g$semihourly), ENG_TOL)
  }
  nos <- eng_run_cached(eng, g, u, seed = NULL)                     # CV do tune_wsvm com outra posicao do RNG
  expect_gt(eng_maxdiff(nos$hourly, g$hourly), ENG_TOL)
  expect_gt(eng_maxdiff(nos$semihourly, g$semihourly), ENG_TOL)
})

test_that("ac-1 [parity] casos de robustez completam (9x24 horario finito), emitem a guarda esperada e batem o oraculo (<= 1e-5 MW)", {
  eng_require_parity()
  eng <- eng_env(eng_load(eng_plugin_root()))
  want <- c(robustez_go_20241005 = "warning.engine_empty_model_guard", robustez_ton_sp_20241119 = "warning.engine_cross_clamped",
            robustez_flag12_sp_20241222 = NA_character_)
  for (u in eng_units("robustez")) {
    g <- eng_golden(u)
    if (is.null(g)) next
    r <- eng_run_cached(eng, g, u)
    d <- eng_compare(r, g, u$id)
    expect_identical(nrow(r$hourly), 216L); expect_identical(nrow(r$semihourly), 432L)
    expect_true(all(is.finite(r$semihourly$Carga)), info = u$id)
    if (!is.na(want[[u$id]])) expect_true(want[[u$id]] %in% r$guards, info = u$id)
    expect_lte(d$hourly, ENG_TOL, label = u$id); expect_lte(d$semihourly, ENG_TOL, label = u$id)
  }
})
