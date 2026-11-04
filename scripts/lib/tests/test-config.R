## test-config.R -- config.R (M1.4a): config invalido exit 8, precedencia flag > YAML (com CONTROL), defaults, flags.
library(testthat)
local({
  e <- pc_source_lib(c("errors", "runtime", "config"))
  for (n in ls(e, all.names = TRUE)) assign(n, get(n, e), envir = globalenv())
})
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))

yaml_file <- function(txt) {
  f <- tempfile("cfg", fileext = ".yaml"); writeLines(txt, f); f
}
fake_root <- function(yaml = NULL) {
  r <- tempfile("root"); dir.create(file.path(r, "scripts", "lib"), recursive = TRUE)
  file.copy(file.path(pc_root(), "scripts", "lib", "runtime.R"), file.path(r, "scripts", "lib", "runtime.R"))
  dir.create(file.path(r, "config"))
  file.copy(file.path(pc_root(), "config", "VERSION"), file.path(r, "config", "VERSION"))
  if (!is.null(yaml)) writeLines(yaml, file.path(r, "config.yaml"))
  normalizePath(r, winslash = "/")
}
abort_of <- function(expr) {
  tryCatch({ force(expr); NULL }, prevcarga_error = identity)
}
expect_abort <- function(expr, id, where = NULL) {
  cond <- abort_of(expr)
  expect_false(is.null(cond))
  expect_identical(cond$id, id)
  expect_identical(cond$class_name, "config")
  expect_identical(cond$exit_code, 8L)
  if (!is.null(where)) expect_identical(cond$where, where)
}

test_that("[AC1] tres config invalidos abortam classe config sem lista parcial", {
  r1 <- NULL
  expect_abort(r1 <- prevcarga_config_load(yaml_file(c("schema_version: 1", "dados_dir: x"))),
               "config.unknown_key", "dados_dir")
  expect_abort(r1 <- prevcarga_config_load(yaml_file(c("schema_version: 1", "color: 3"))),
               "config.bad_type", "color")
  expect_abort(r1 <- prevcarga_config_load(yaml_file(c("schema_version: 1", "weather_issue_cutoff: meia_noite"))),
               "config.bad_weather_cutoff", "weather_issue_cutoff")
  expect_null(r1)
  expect_abort(prevcarga_config_load(yaml_file("schema_version: 2")), "config.bad_type", "schema_version")
  expect_abort(prevcarga_config_load(yaml_file("color: auto")), "config.bad_type", "schema_version")
  expect_abort(prevcarga_config_load(yaml_file(c("schema_version: 1", "backtest:", "  corte_default: 25:99"))),
               "config.bad_type", "backtest.corte_default")
  expect_abort(prevcarga_config_load(yaml_file(c("schema_version: 1", "backtest:", "  x: 1"))),
               "config.unknown_key", "backtest.x")
  expect_abort(prevcarga_config_load(yaml_file(c("schema_version: 1", "format: xml"))),
               "config.bad_format", "format")
  # CONTROL: yaml valido carrega
  ok <- prevcarga_config_load(yaml_file(c("schema_version: 1", "color: never", "load_label: end")))
  expect_identical(ok$color, "never"); expect_identical(ok$load_label, "end")
  expect_identical(ok$weather_issue_cutoff, "available_until")
})

test_that("[AC2] flag vence YAML; CONTROL: sem flag o valor do YAML volta", {
  root <- fake_root()
  f <- yaml_file(c("schema_version: 1", "data_dir: A", "weather_issue_cutoff: available_until"))
  c1 <- prevcarga_ctx(list("--config" = f), root)
  expect_identical(c1$data_dir, normalizePath("A", winslash = "/", mustWork = FALSE))
  expect_identical(c1$weather_issue_cutoff, "available_until")
  c2 <- prevcarga_ctx(list("--config" = f, "--data" = "B", "--weather-cutoff" = "origin_midnight_utc"), root)
  expect_identical(c2$data_dir, normalizePath("B", winslash = "/", mustWork = FALSE))
  expect_identical(c2$weather_issue_cutoff, "origin_midnight_utc")
  c3 <- prevcarga_ctx(list("--config" = f), root)
  expect_identical(c3$data_dir, c1$data_dir)
  expect_identical(c3$weather_issue_cutoff, "available_until")
})

test_that("[AC3] defaults sem --config e config da raiz; guarda de data_dir", {
  root <- fake_root()
  ctx <- prevcarga_ctx(list(), root)
  expect_identical(ctx$state_dir, file.path(root, ".state"))
  expect_false(dir.exists(ctx$state_dir))
  expect_null(ctx$data_dir); expect_null(ctx$origin); expect_null(ctx$load_label_flag)
  expect_identical(ctx$weather_issue_cutoff, "available_until")
  expect_identical(ctx$color, "auto"); expect_identical(ctx$format, "text")
  expect_false(ctx$debug)
  expect_identical(ctx$version, readLines(file.path(pc_root(), "config", "VERSION"), warn = FALSE)[1])
  expect_match(ctx$run_id, "^[0-9]{8}T[0-9]{6}Z-[0-9]+$")
  expect_abort(prevcarga_require_data_dir(ctx), "config.data_dir_missing")
  root2 <- fake_root(c("schema_version: 1", "format: json"))
  expect_identical(prevcarga_ctx(list(), root2)$format, "json")
  expect_identical(prevcarga_ctx(list("--format" = "text"), root2)$format, "text")
  expect_identical(prevcarga_require_data_dir(prevcarga_ctx(list("--data" = "X"), root)) $data_dir,
                   normalizePath("X", winslash = "/", mustWork = FALSE))
})

test_that("[AC4] flags invalidas e faixa de origem", {
  root <- fake_root()
  expect_abort(prevcarga_ctx(list("--origin" = "2024-13-01"), root, "models"), "config.bad_origin")
  expect_abort(prevcarga_ctx(list("--load-label" = "meio"), root, "models"), "config.bad_load_label")
  expect_abort(prevcarga_ctx(list("--weather-cutoff" = "agora"), root, "models"), "config.bad_weather_cutoff")
  expect_abort(prevcarga_ctx(list("--color" = "rosa"), root, "models"), "config.bad_color")
  expect_abort(prevcarga_ctx(list("--format" = "xml"), root, "models"), "config.bad_format")
  expect_abort(prevcarga_ctx(list("--origin" = "2024-06-01..2024-06-30"), root, "models"), "config.bad_origin")
  expect_abort(prevcarga_ctx(list("--origin" = "2024-06-30..2024-06-01"), root, "backtest"), "config.bad_origin")
  expect_identical(prevcarga_ctx(list("--origin" = "2024-06-01"), root, "models")$origin, as.Date("2024-06-01"))
  r <- prevcarga_ctx(list("--origin" = "2024-06-01..2024-06-30"), root, "backtest")$origin
  expect_identical(r, list(from = as.Date("2024-06-01"), to = as.Date("2024-06-30")))
  expect_identical(prevcarga_ctx(list("--load-label" = "start"), root)$load_label_flag, "start")
  expect_true(prevcarga_ctx(list("--debug" = TRUE), root)$debug)
})

test_that("[AC5] template, VERSION, gitignore; sem leitura de env", {
  tpl <- file.path(pc_root(), "config", "config.yaml")
  cfg <- prevcarga_config_load(tpl)
  expect_identical(cfg$schema_version, 1L)
  lines <- readLines(tpl, warn = FALSE)
  expect_false(any(as.integer(unlist(lapply(lines, function(s) charToRaw(s)))) > 127L))
  for (k in c(names(.prevcarga_config_schema), "corte_default"))
    expect_true(any(grepl(paste0("^#? ?\\s*", k, ":"), lines)), info = k)
  expect_identical(prevcarga_ctx(list(), pc_root())$version, readLines(file.path(pc_root(), "config", "VERSION"), warn = FALSE)[1])
  skip_if(!nzchar(Sys.which("git")), "git ausente")
  chk <- function(p) suppressWarnings(system2("git", c("-C", shQuote(pc_root()), "check-ignore", "-q", p)))
  expect_identical(chk("config.yaml"), 0L)
  expect_identical(chk(".state/x"), 0L)
  expect_identical(chk("config/config.yaml"), 1L)
  src <- readLines(file.path(pc_root(), "scripts", "lib", "config.R"), warn = FALSE)
  expect_false(any(grepl("Sys.getenv|Sys.setenv", src)))
})
