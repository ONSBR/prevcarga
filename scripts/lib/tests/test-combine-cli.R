## test-combine-cli.R -- M4.V: o verbo `combine` POR PROCESSO (Rscript --vanilla scripts/prevcarga via pc_run_cli, M1.1).
## Fixtures em tempdir; o ambiente sujo dos filhos (TZ/LC_ALL/BLAS) so existe dentro do teste (pc_with_env), nunca no pacote.
## Todo delta e sobre y_pred, nunca sobre w. Unico skip permitido: docopt ausente.
skip_if_not_installed("docopt")
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "registry", "combination"))
env$prevcarga_runtime_enforce()

fx <- pc_cmb_cli_fixture(dir = withr::local_tempdir())
tmp <- withr::local_tempdir()
ascii <- function(x) !any(as.integer(unlist(lapply(x, charToRaw))) > 127L)
j1 <- function(r) jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE)
rd <- function(p) env$prevcarga_read_long(p, list(tz = pc_cmb_tz))
base_args <- function(mode, fmt, state, area = "SECO", extra = character())
  c("combine", paste0("--mode=", mode), paste0("--area=", area), paste0("--root=", fx$root),
    paste0("--data=", fx$data), paste0("--state=", state), paste0("--format=", fmt), extra)
state_empty <- file.path(tmp, "state-vazio"); dir.create(state_empty)

## tabela de invocacoes: 1 linha por caso, cada uma roda em text e em json
m4_cli_cases <- data.table::data.table(
  id    = c("train", "apply", "full", "bad_mode", "bad_area", "not_trained"),
  mode  = c("train", "apply", "full", "xyz", "full", "apply"),
  area  = c("SECO", "SECO", "SECO", "SECO", "XX", "SECO"),
  out   = c(FALSE, TRUE, TRUE, FALSE, FALSE, TRUE),
  exit  = c(0L, 0L, 0L, 8L, 4L, 13L),
  error_id = c(NA, NA, NA, "config.bad_mode", "topology.area_undeclared", "state.combina_not_trained"))
run_case <- function(i, fmt, state) {
  cs <- m4_cli_cases[i]
  extra <- if (cs$out) paste0("--out=", file.path(tmp, sprintf("%s-%s.parquet", cs$id, fmt))) else character()
  pc_run_cli(base_args(cs$mode, fmt, if (cs$id == "not_trained") state_empty else state, cs$area, extra))
}

## treina primeiro (json), depois cada caso nos dois formatos; o train de text re-treina o mesmo conteudo (reaponta o latest)
res <- list()
for (fmt in c("json", "text")) for (i in seq_len(nrow(m4_cli_cases)))
  res[[paste(m4_cli_cases$id[i], fmt)]] <- run_case(i, fmt, fx$state)

test_that("M4.V - CLI por processo: exit esperado, JSON unico, erro so em stderr no text, ASCII, sem traceback", {
  for (i in seq_len(nrow(m4_cli_cases))) {
    cs <- m4_cli_cases[i]
    rj <- res[[paste(cs$id, "json")]]; rt <- res[[paste(cs$id, "text")]]
    expect_identical(rj$status, cs$exit, info = cs$id)
    expect_identical(rt$status, cs$exit, info = cs$id)                    # exit identico entre formatos
    expect_true(ascii(c(rj$stdout, rj$stderr, rt$stdout, rt$stderr)), info = cs$id)
    expect_false(any(grepl("Error in|Traceback", c(rj$stdout, rj$stderr, rt$stdout, rt$stderr))), info = cs$id)
    expect_length(rj$stdout, 1L)                                           # JSON unico em stdout
    expect_identical(rj$stderr, character(0), info = cs$id)               # stderr vazio no json
    j <- j1(rj)
    expect_true(all(c("ok", "verb", "exit_code", "error", "warnings", "result", "run_id", "version") %in% names(j)))
    expect_identical(j$verb, "combine")
    expect_identical(j$exit_code, cs$exit)
    if (cs$exit == 0L) {
      expect_true(j$ok); expect_null(j$error)
      expect_identical(rt$stderr, character(0), info = cs$id)
      expect_identical(rt$stdout[[length(rt$stdout)]], "OK")
    } else {
      expect_false(j$ok)
      expect_identical(j$error$id, cs$error_id, info = cs$id)
      expect_identical(rt$stdout, character(0), info = cs$id)             # erro so em stderr no text
      expect_gt(length(rt$stderr), 0L)
    }
  }
})

test_that("M4.V - CLI por processo: train/apply/full gravam artefato, parquets 8 colunas e y_pred identico", {
  r <- j1(res[["train json"]])$result
  expect_match(r$version_hash, "^v_[0-9a-f]{16}$")
  adir <- file.path(fx$state, "artifacts", "combina", "SECO")
  expect_true(file.exists(file.path(adir, "latest.txt")))
  expect_true(file.exists(file.path(adir, r$version_hash, "weights.rds")))
  expect_identical(readLines(file.path(adir, "latest.txt")), r$version_hash)
  a <- rd(file.path(tmp, "apply-json.parquet")); f <- rd(file.path(tmp, "full-json.parquet"))
  expect_identical(setdiff(names(a), "interval_start"), c("area", "forecast_origin", "target_datetime", "lead", "y_pred",
                                                          "y_true", "d_plus", "granularity_min"))
  expect_gt(nrow(a), 0L)
  expect_identical(a$y_pred, f$y_pred)                                    # full == train + apply
  expect_identical(rd(file.path(tmp, "apply-text.parquet"))$y_pred, a$y_pred)
  expect_identical(rd(file.path(tmp, "full-text.parquet"))$y_pred, f$y_pred)
  expect_identical(j1(res[["bad_mode json"]])$error$id, "config.bad_mode")
  expect_identical(j1(res[["not_trained json"]])$error$id, "state.combina_not_trained")
})

test_that("M4.V - CLI por processo: `prevcarga combine` no Usage de --help", {
  h <- pc_run_cli("--help")
  expect_identical(h$status, 0L)
  expect_true(any(grepl("Usage:", h$stdout, fixed = TRUE)))
  expect_true(any(grepl("prevcarga combine", h$stdout, fixed = TRUE)))
  expect_true(ascii(c(h$stdout, h$stderr)))
})

## ---- determinismo entre processos (shell limpo vs shell sujo) -----------------------------------------------------------------
test_that("M4.V - determinismo por processo: shell sujo (TZ/LC_ALL/BLAS) => exit 0 e identical(y_pred); in-process idem", {
  out_clean <- file.path(tmp, "det-clean.parquet"); out_dirty <- file.path(tmp, "det-dirty.parquet")
  rc <- pc_with_env(c(TZ = NA, LC_ALL = NA, OPENBLAS_NUM_THREADS = NA),
                    pc_run_cli(base_args("full", "json", fx$state, extra = paste0("--out=", out_clean))))
  rd_ <- pc_with_env(c(TZ = "America/Sao_Paulo", LC_ALL = "pt_BR.UTF-8", OPENBLAS_NUM_THREADS = "4"),
                     pc_run_cli(base_args("full", "json", fx$state, extra = paste0("--out=", out_dirty))))
  expect_identical(rc$status, 0L)
  expect_identical(rd_$status, 0L)                                        # o runtime fixa TZ/locale/BLAS in-process
  yc <- rd(out_clean)$y_pred; yd <- rd(out_dirty)$y_pred
  expect_gt(length(yc), 0L)
  expect_identical(yc, yd)
  ctx <- c(pc_ctx_deck(fx$data), list(tz = pc_cmb_tz, root = pc_root(), load_label = "start",
                                      weather_issue_cutoff = "available_until"))
  ip <- suppressWarnings(env$prevcarga_combina_full(env$prevcarga_combina_load(fx$root, "SECO", ctx, fx$members), ctx))
  expect_identical(yc, ip$y_pred)
})

## ---- members_by_lead e troca do membro curto pelo run-manifest ---------------------------------------------------------------------
test_that("M4.V - members_by_lead no JSON: 8 em O..O+1, 7 em O+2..O+8; trocar o curto no root muda sem mudar codigo", {
  one <- function(short) {
    d <- withr::local_tempdir(.local_envir = parent.frame())
    root <- pc_cmb_root_aligned8(fx$members, dir = file.path(d, "root"), short_member = short)
    deck <- pc_deck_sentinel("start", days = lubridate::ymd("2024-02-29") + 0:11, areas = "SECO", dir = file.path(d, "deck"))
    st <- file.path(d, "state"); dir.create(st)
    out <- file.path(d, "o.parquet")
    r <- pc_run_cli(c("combine", "--mode=full", "--area=SECO", paste0("--root=", root), paste0("--data=", deck),
                      paste0("--state=", st), "--format=json", paste0("--out=", out)))
    list(r = r, out = out)
  }
  s1 <- one(fx$members$sigla[[1L]]); s2 <- one(fx$members$sigla[[2L]])
  expect_identical(s1$r$status, 0L); expect_identical(s2$r$status, 0L)
  mbl <- function(s) j1(s$r)$result$members_by_lead
  n_of <- function(m, days) unique(vapply(Filter(function(x) pc_cmb_lead_day(x$lead) %in% days, m), function(x) x$n_members, 0L))
  for (s in list(s1, s2)) {
    expect_identical(n_of(mbl(s), 0:1), 8L)
    expect_identical(n_of(mbl(s), 2:8), 7L)
  }
  absent <- function(m) unique(unlist(lapply(Filter(function(x) pc_cmb_lead_day(x$lead) >= 2L, m),
                                              function(x) setdiff(fx$members$sigla, unlist(x$members)))))
  expect_identical(absent(mbl(s1)), fx$members$sigla[[1L]])
  expect_identical(absent(mbl(s2)), fx$members$sigla[[2L]])               # so o root mudou, nao o codigo
  ## a faixa de saida do LONG: 1a entrega available_until + 30, ultima O + 9 dias
  o <- rd(s1$out)
  expect_identical(min(o$target_datetime), lubridate::ymd_hm("2024-03-01 09:30", tz = pc_cmb_tz))
  expect_identical(max(o$target_datetime), lubridate::ymd("2024-03-01", tz = pc_cmb_tz) + lubridate::days(9))
  ## y_pred identico ao in-process
  ctx <- c(pc_ctx_deck(NULL), list(tz = pc_cmb_tz, root = pc_root(), load_label = "start", weather_issue_cutoff = "available_until"))
  d <- withr::local_tempdir()
  root <- pc_cmb_root_aligned8(fx$members, dir = file.path(d, "root"))
  ctx$data_dir <- pc_deck_sentinel("start", days = lubridate::ymd("2024-02-29") + 0:11, areas = "SECO", dir = file.path(d, "deck"))
  ip <- suppressWarnings(env$prevcarga_combina_full(env$prevcarga_combina_load(root, "SECO", ctx, fx$members), ctx))
  expect_identical(o$y_pred, ip$y_pred)
})
