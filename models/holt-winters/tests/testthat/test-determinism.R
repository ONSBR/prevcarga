## test-determinism.R -- gates [PLUGINS] determinismo / seed_medicao / sem_seed_literal do Holt-Winters (HW.7; AC 11; gate06 do
## oraculo). Fixture com dias atipicos (carga x0,5 em 6 dias uteis) que forcam a imputacao por cluster do MO1 (kmeans); tudo
## pelo DESPACHANTE da fundacao (helpers hv_* de helper-hwverif.R). O relogio e o RNG do chamador so existem no teste.

## deck de helper-hw.R com `n` dias uteis do historico a 0,5x (cada copia vem de prevcarga_fixture_perturb, nunca da base)
hx_atypical_days <- lubridate::ymd(c("2024-04-02", "2024-04-03", "2024-04-09", "2024-04-10", "2024-04-16", "2024-04-17"))

hx_atypical_store <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      st <- hv_store("09:00")
      for (d in hx_atypical_days) {
        s0 <- hv_day_start(d)
        out <- hw_lib$prevcarga_fixture_perturb(st$dir, "load", after = s0 - lubridate::minutes(30L),
                                                at_or_before = s0 + lubridate::minutes(30L * 47L),
                                                perturb = function(v) v * 0.5)
        st <- hv_with_dir(st, out)
      }
      cache <<- st
    }
    cache
  }
})

hx_base <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      st <- hx_atypical_store(); cuts <- hv_cuts(st)
      cache <<- list(st = st, cuts = cuts, train = hv_panel(st, cuts, "train"), infer = hv_panel(st, cuts, "infer"))
    }
    cache
  }
})

hx_get_seed <- function() get(".Random.seed", envir = globalenv())

test_that("[PLUGINS] determinismo: 2 treinos = mesmo sha256 e .Random.seed do chamador intacto; kmeans_seed 123 -> 7 muda o sha256 (CONTROL)", {
  b <- hx_base()
  ## a fixture atipica de fato exercita o MO1: os 6 dias a 0,5x estao no historico de treino
  ld <- b$train$load
  dia <- lubridate::as_date(ld$interval_start, tz = b$cuts$tz)
  m_at <- mean(ld$load_mwh[dia %in% hx_atypical_days]); m_ok <- mean(ld$load_mwh[!dia %in% hx_atypical_days])
  expect_lt(m_at, 0.7 * m_ok)
  set.seed(99); caller <- hx_get_seed()
  a1 <- hv_train(b$train, b$cuts); a2 <- hv_train(b$train, b$cuts)
  expect_identical(hx_get_seed(), caller)                                  # o despachante restaura o RNG do chamador
  expect_identical(hv_sha(a1), hv_sha(a2))
  expect_identical(hv_sha(a1), hv_sha(hv_train(b$train, b$cuts)))
  ## CONTROL: a semente do kmeans tem dente (a imputacao por cluster consome o RNG)
  s7 <- hv_spec(); s7$params$kmeans_seed <- 7L
  a7 <- hv_train(b$train, b$cuts, spec = s7)
  expect_identical(a7$stamp$params$kmeans_seed, 7L)
  expect_identical(a1$stamp$params$kmeans_seed, 123L)
  expect_false(identical(hv_sha(a7), hv_sha(a1)))
  expect_identical(hx_get_seed(), caller)
})

test_that("[PLUGINS] seed_medicao: seed 1234 -> 1235 medido e gravado em .state/parity/holt-winters/seed_measure.json, sem asserção de Delta", {
  b <- hx_base()
  a0 <- hv_train(b$train, b$cuts)
  s1 <- hv_spec(); s1$seed <- 1235L
  a1 <- hv_train(b$train, b$cuts, spec = s1)
  expect_identical(a0$stamp$seed, 1234L); expect_identical(a1$stamp$seed, 1235L)
  v0 <- hv_infer(a0, b$infer, b$cuts)$values; v1 <- hv_infer(a1, b$infer, b$cuts)$values
  expect_identical(dim(v0), dim(v1))
  d <- max(abs(as.matrix(v0) - as.matrix(v1)))
  expect_true(is.finite(d))                                                # medida, nao limite: hw() e MLE e o kmeans re-semeia
  dir <- file.path(hw_root(), ".state", "parity", "holt-winters")             # fora de models/ (aditividade)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  {
    jsonlite::write_json(list(seed_from = 1234L, seed_to = 1235L, max_abs_delta_values = d, n_values = length(v0)),
                         file.path(dir, "seed_measure.json"), auto_unbox = TRUE, digits = NA)
    expect_true(file.exists(file.path(dir, "seed_measure.json")))
  }
})

## grep de codigo (pc_grep_code, P.0j): carregado so por este arquivo e so se ainda nao estiver no ambiente
hx_load_grep <- function() {
  if (!exists("pc_grep_code", envir = hw_lib, inherits = FALSE))
    sys.source(file.path(hw_root(), "scripts", "lib", "tests", "helper-plugins.R"), envir = hw_lib)
  get("pc_grep_code", envir = hw_lib)
}

test_that("[PLUGINS] sem_seed_literal: nenhum set.seed com literal no codigo do plugin; CONTROL plantado numa copia e acusado", {
  grep_code <- hx_load_grep()
  fs <- list.files(file.path(hw_root(), "models", "holt-winters", "R"), pattern = "[.]R$", full.names = TRUE)
  pat <- list(seed_literal = "set[.]seed\\s*\\(\\s*[0-9]")
  h <- grep_code(fs, pat)
  expect_identical(nrow(h), 0L, info = paste(basename(h$file), h$line, collapse = "; "))
  ## as 2 chamadas existentes recebem a semente do run.yaml (variavel), nunca literal
  expect_gt(length(unlist(lapply(fs, function(f) grep("set[.]seed\\s*\\(", readLines(f, warn = FALSE))))), 0L)
  cp <- file.path(withr::local_tempdir(), "R"); dir.create(cp)
  writeLines(c("f <- function() set.seed(123)", "# set.seed(7)", "g <- function(s) set.seed(s)"), file.path(cp, "plant.R"))
  hc <- grep_code(file.path(cp, "plant.R"), pat)
  expect_identical(hc$line, 1L)
})
