## test-validate-deck.R -- verbo validate-deck (M2.5) pela fachada in-process (prevcarga_main + handler de topo do M1).
## Exit por classe (0/2/3/4/5/8) nos dois formatos; envelope unico no JSON com stderr vazio; ASCII; sem traceback.
library(testthat)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
for (m in c("errors", "runtime", "config", "banner", "cli"))   # equivale ao source do shim (modulos do verbo vao a globalenv)
  sys.source(file.path(pc_root(), "scripts", "lib", paste0(m, ".R")), envir = globalenv())

TZ <- "Etc/GMT+3"
vd_tmp <- function(env = NULL) {
  d <- tempfile("vd"); dir.create(d)
  if (!is.null(env)) withr::defer(unlink(d, recursive = TRUE), envir = env)
  d
}
## instalacao-copia (config + models + scripts/lib sem tests) com historicos pequenos para o deck curto
vd_root <- function(hw = c(30, 10), env = NULL) {
  d <- vd_tmp(env)
  file.copy(file.path(pc_root(), c("config", "models")), d, recursive = TRUE)
  dir.create(file.path(d, "scripts"))
  file.copy(file.path(pc_root(), "scripts", "lib"), file.path(d, "scripts"), recursive = TRUE)
  unlink(file.path(d, "scripts", "lib", "tests"), recursive = TRUE)
  set <- function(m, tr, inf) {
    f <- file.path(d, "models", m, "run.yaml"); y <- yaml::read_yaml(f)
    y$history_days <- list(train = tr, infer = inf); yaml::write_yaml(y, f)
  }
  set("holt-winters", hw[1L], hw[2L]); set("random-forest", 30, 10); set("lgbm", 20, 5)
  set("dessem-regdin", 20, 20); set("dessem-linear", 20, 20)
  d
}
.vd_h <- function(areas) {
  h <- pc_deck_holidays_default(areas)
  h[which(as.numeric(simplified_code) < 0), simplified_code := bit64::as.integer64(1L)]
  h
}
## deck de `n` dias terminando em 2026-10-01 (O); `until` corta a carga; label start grava o manifest start
vd_build <- function(n = 40, areas = c("SECO", "S"), label = "end", until = NULL, env = parent.frame()) {
  d <- vd_tmp(env)
  days <- seq(lubridate::ymd("2026-10-01") - (n - 1L), lubridate::ymd("2026-10-01"), by = "day")
  pc_deck_sentinel(label, days = days, areas = areas, until = until, dir = d)
  .pc_deck_write(.vd_h(areas), d, file.path("auxiliary", "holidays"), character(), "date")
  d
}
.vd_tpl <- new.env()
vd_clean <- function(env = parent.frame()) {
  if (is.null(.vd_tpl$d)) .vd_tpl$d <- vd_build(env = globalenv())
  dst <- vd_tmp(env)
  file.copy(list.files(.vd_tpl$d, full.names = TRUE, all.files = TRUE, no.. = TRUE), dst, recursive = TRUE)
  dst
}

run_v <- function(root, argv) {
  err <- character(); code <- NA_integer_
  out <- utils::capture.output({
    err <- utils::capture.output({
      code <- prevcarga_main(argv, root = root, runtime = function() invisible(NULL))
    }, type = "message")
  }, type = "output")
  list(code = code, out = out, err = err)
}
non_ascii <- function(x) any(unlist(lapply(x, function(l) any(as.integer(charToRaw(l)) > 127L))))
jdoc <- function(run) jsonlite::fromJSON(paste(run$out, collapse = "\n"), simplifyVector = FALSE)
root0 <- vd_root()
root_hw <- vd_root(hw = c(60, 10))
at <- function(x) lubridate::ymd_hm(x, tz = TZ)

## --- fixtures por classe: exit esperado, argv extra e raiz -----------------------------------------------------------
vd_cases <- function(env = parent.frame()) {
  d_ok <- vd_clean(env)
  d_miss <- vd_clean(env); pc_mut_drop_col(d_miss, "load", "load_mwh")
  d_47 <- vd_clean(env); pc_mut_load(d_47, function(dt) dt[-(.N - 240L)])
  d_xx <- vd_clean(env); pc_mut_add_area(d_xx, "XX")
  list(ok = list(0L, d_ok, character(), root0, NA), contract = list(2L, d_miss, character(), root0, "contract"),
       completude = list(3L, d_47, character(), root0, "completude"),
       topology = list(4L, d_xx, character(), root0, "topology"),
       window = list(5L, d_ok, c("--model", "holt-winters"), root_hw, "window"),
       config = list(8L, d_ok, c("--load-label", "xyz"), root0, "config"))
}

test_that("exit 0/2/3/4/5/8 por classe nos dois formatos; envelope unico, stderr vazio no JSON, ASCII, sem traceback (ac-9)", {
  cs <- vd_cases()
  for (nm in names(cs)) {
    k <- cs[[nm]]; argv <- c("validate-deck", "--data", k[[2L]], k[[3L]])
    rt <- run_v(k[[4L]], argv)
    rj <- run_v(k[[4L]], c(argv, "--format", "json"))
    expect_identical(rt$code, k[[1L]], info = nm); expect_identical(rj$code, k[[1L]], info = nm)
    doc <- jdoc(rj)
    expect_setequal(names(doc), c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version"))
    expect_identical(doc$verb, "validate-deck"); expect_identical(doc$exit_code, k[[1L]], info = nm)
    expect_length(Filter(nzchar, rj$out), 1L)                    # um unico documento
    expect_identical(rj$err, character(), info = nm)
    expect_false(non_ascii(c(rt$out, rt$err, rj$out)), info = nm)
    expect_false(any(grepl("Traceback|stop\\(|Error in", c(rt$out, rt$err, rj$out))), info = nm)
    if (k[[1L]] == 0L) {
      expect_true(doc$ok); expect_null(doc$error)
      expect_true(any(grepl("^OK$", rt$out)))
      expect_true(any(grepl("^validate-deck origin=", rt$out)))
    } else {
      expect_false(doc$ok)
      expect_identical(doc$error$class, k[[5L]], info = nm)
      expect_true(all(c("class", "id", "message", "where", "hint", "details") %in% names(doc$error)))
      expect_identical(doc$exit_code, as.integer(unname(prevcarga_exit_codes[[doc$error$class]])))
      expect_true(any(grepl(sprintf("^prevcarga: erro [%s] (exit %d)", k[[5L]], k[[1L]]), rt$err, fixed = FALSE) |
                      startsWith(rt$err, sprintf("prevcarga: erro [%s] (exit %d)", k[[5L]], k[[1L]]))), info = nm)
    }
  }
  ## erro de classe de dado: o resultado em texto tambem vai a stdout (FALHA <classe>)
  rt <- run_v(root0, c("validate-deck", "--data", cs$completude[[2L]]))
  expect_true(any(grepl("^FALHA completude$", rt$out)))
  expect_true(any(grepl("day_incomplete", rt$out)))
  ## ids do primeiro erro da classe dominante
  expect_identical(jdoc(run_v(root0, c("validate-deck", "--data", cs$completude[[2L]], "--format", "json")))$error$id,
                   "completude.day_incomplete")
  expect_identical(jdoc(run_v(root0, c("validate-deck", "--data", cs$topology[[2L]], "--format", "json")))$error$class, "topology")
  expect_identical(jdoc(run_v(root_hw, c("validate-deck", "--data", cs$ok[[2L]], "--model", "holt-winters",
                                         "--format", "json")))$error$class, "window")
})

test_that("--debug so afeta traceback de erro inesperado; verbo sem --data: config.data_dir_missing exit 8", {
  r <- run_v(root0, c("validate-deck"))
  expect_identical(r$code, 8L)
  expect_true(any(grepl("prevcarga: erro [config] (exit 8)", r$err, fixed = TRUE)))
  j <- jdoc(run_v(root0, c("validate-deck", "--format", "json")))
  expect_identical(j$error$id, "config.data_dir_missing")
  for (flag in c("--origin=2026-13-45", "--weather-cutoff=xyz", "--format=xml", "--color=xyz")) {
    rr <- run_v(root0, c("validate-deck", "--data", vd_clean(), flag))
    expect_identical(rr$code, 8L, info = flag)
  }
})

test_that("conflito de load_label: manifest start x --load-label end = exit 2; sem flag = 0 com fonte manifest (ac-10)", {
  d <- vd_build(label = "start")
  r <- run_v(root0, c("validate-deck", "--data", d, "--load-label", "end", "--format", "json"))
  j <- jdoc(r)
  expect_identical(r$code, 2L); expect_identical(j$exit_code, 2L)
  expect_identical(j$error$id, "contract.load_label_conflict")
  expect_identical(j$error$where, "manifest.json:load_label")
  r0 <- run_v(root0, c("validate-deck", "--data", d, "--format", "json"))
  j0 <- jdoc(r0)
  expect_identical(r0$code, 0L); expect_true(j0$ok)
  expect_identical(j0$result$load_label, "start"); expect_identical(j0$result$load_label_source, "manifest")
  expect_identical(jdoc(run_v(root0, c("validate-deck", "--data", d, "--load-label", "start", "--format", "json")))$result$load_label_source,
                   "manifest")
})

test_that("origem default pelo deck x flag; result carrega os campos do ADR-0040 e cuts por membro (ac-11)", {
  d <- vd_build(until = at("2026-10-01 09:30"))      # a carga termina no intervalo com inicio 09:00 de O
  r <- run_v(root0, c("validate-deck", "--data", d, "--format", "json"))
  j <- jdoc(r)$result
  expect_identical(r$code, 0L)
  expect_identical(j$forecast_origin, "2026-10-01"); expect_identical(j$origin_source, "deck")
  expect_identical(j$available_until, "2026-10-01 09:30")
  for (k in c("available_until_by_area", "weather_issue_cutoff", "weather_issue_limit", "timezone", "data_hash", "findings",
              "cuts", "load_label", "load_label_source"))
    expect_true(k %in% names(j), info = k)
  expect_match(j$data_hash, "^[0-9a-f]{64}$")
  expect_identical(j$timezone$load, TZ)
  expect_identical(names(j$available_until_by_area), c("S", "SECO"))
  expect_length(j$cuts, 5L)
  for (row in j$cuts)
    expect_true(all(c("model", "train_origin", "last_data_day", "data_lag_days", "data_lag_source") %in% names(row)))
  lg <- Filter(function(x) x$model == "lgbm", j$cuts)[[1L]]
  expect_identical(lg$train_origin, "2026-09-29"); expect_identical(lg$last_data_day, "2026-09-30")
  expect_identical(lg$data_lag_source, "default")
  j2 <- jdoc(run_v(root0, c("validate-deck", "--data", d, "--origin", "2026-09-28", "--format", "json")))$result
  expect_identical(j2$forecast_origin, "2026-09-28"); expect_identical(j2$origin_source, "flag")
  expect_identical(Filter(function(x) x$model == "lgbm", j2$cuts)[[1L]]$train_origin, "2026-09-26")
  ## data_hash model-agnostico: --model lgbm x random-forest dao o mesmo hash do deck inteiro
  h <- function(m) jdoc(run_v(root0, c("validate-deck", "--data", d, "--model", m, "--format", "json")))$result$data_hash
  expect_identical(h("lgbm"), h("random-forest")); expect_identical(h("lgbm"), j$data_hash)
  expect_length(jdoc(run_v(root0, c("validate-deck", "--data", d, "--model", "lgbm", "--format", "json")))$result$cuts, 1L)
  ## aviso do deck truncado: stderr no texto, `warnings` no JSON (stderr vazio), exit 0
  rt <- run_v(root0, c("validate-deck", "--data", d))
  rw <- run_v(root0, c("validate-deck", "--data", d, "--format", "json"))
  expect_identical(rt$code, 0L)
  expect_true(any(grepl("prevcarga: aviso [warning.climate_after_cut]", rt$err, fixed = TRUE)))
  expect_true("warning.climate_after_cut" %in% vapply(jdoc(rw)$warnings, function(w) w$id, ""))
  expect_identical(rw$err, character())
  ## --area fora da topologia: topology.scope_undeclared (exit 4)
  ja <- run_v(root0, c("validate-deck", "--data", d, "--area", "ZZ", "--format", "json"))
  expect_identical(ja$code, 4L); expect_identical(jdoc(ja)$error$id, "topology.scope_undeclared")
  ## somente leitura: o deck nao muda
  before <- unname(tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE)))
  invisible(run_v(root0, c("validate-deck", "--data", d)))
  expect_identical(unname(tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE))), before)
})

test_that("verbo auto-registrado: sai da lista de stubs e o usage traz --origin como unico flag humano de data (ac-13)", {
  tab <- prevcarga_verbs_discover()
  r <- tab[tab$verb == "validate-deck", ]
  expect_identical(nrow(r), 1L); expect_false(is.null(r$handler[[1L]]))
  expect_identical(r$owner, "M2")
  usage <- prevcarga_usage(tab)
  expect_true(grepl("--origin=<data>", usage, fixed = TRUE))
  ## `--from=<dir>` e do reconcile (diretorio de folhas, nao data; R.5): fora o do reconcile, nenhum flag de data
  expect_false(grepl("--(start|end|since|until|date|from|to)=", gsub("--from=<dir>", "", usage, fixed = TRUE)))
  expect_true(file.exists(file.path(pc_root(), "scripts", "lib", "cli", "validate-deck.R")))
  env <- new.env(parent = globalenv())
  sys.source(file.path(pc_root(), "scripts", "lib", "cli", "validate-deck.R"), envir = env)
  expect_true("prevcarga_verb_validate_deck" %in% ls(env))
})
