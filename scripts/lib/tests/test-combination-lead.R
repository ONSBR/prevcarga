## test-combination-lead.R -- bloco lead_coverage: combinacao por lead com delivery_days (M4.2c)
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                       "evaluation", "features", "combination"))
env$prevcarga_runtime_enforce()

fn <- function(name) get(name, envir = env)
members <- env$prevcarga_members(root = pc_root())
short_m <- members$sigla[[1L]]
long_ms <- setdiff(members$sigla, short_m)

fail <- function(expr) tryCatch({ force(expr); NULL }, prevcarga_error = identity)
expect_contract_id <- function(expr, id, where) {
  e <- fail(expr)
  expect_false(is.null(e))
  expect_identical(e$id, id)
  expect_match(e$where, where, fixed = TRUE)
  expect_false(any(as.integer(charToRaw(conditionMessage(e))) > 127L))
}

## cenario base (i), variantes (ii) e (iii) e as saidas de fit+produce, calculados uma vez
long0   <- pc_cmb_long8_lead(members)
panel0  <- pc_cmb_panel_from_long(env, long0)
run0    <- pc_cmb_run_combina(env, panel0)
long_t  <- pc_cmb_fill_tail(long0, short_m, members)                                    # (ii) cauda cheia, dd = 2
panel_t <- pc_cmb_panel_from_long(env, long_t)
run_t   <- pc_cmb_run_combina(env, panel_t)
long_9  <- pc_cmb_with_prov(long_t, pc_cmb_set_days(attr(long_t, "provenance"), short_m, 9L))   # (iii)
panel_9 <- pc_cmb_panel_from_long(env, long_9)
run_9   <- pc_cmb_run_combina(env, panel_9)

test_that("M4.2c - AC1 pesos ausentes (nao zero) do membro curto em O+2..O+8, soma 1, y_pred fechado", {
  w <- run0$weights
  day <- pc_cmb_lead_day(w$lead)
  nm <- lapply(w$w, function(x) sub("^f_", "", names(x)))
  expect_true(all(vapply(nm[day >= 2L], function(x) !short_m %in% x && length(x) == 7L, logical(1))))
  expect_true(all(vapply(nm[day < 2L], function(x) setequal(x, members$sigla), logical(1))))
  expect_true(all(abs(vapply(w$w, sum, numeric(1)) - 1) < 1e-9))
  ## celulas ajustadas (nao so fallback) existem nos dois grupos
  expect_true(any(is.na(w$fallback) & day >= 2L)); expect_true(any(is.na(w$fallback) & day < 2L))
  ## fallback EQW: 1/7 nos leads longos e 1/8 em O..O+1
  fb <- !is.na(w$fallback)
  expect_true(all(vapply(w$w[fb & day >= 2L], function(x) all(abs(x - 1 / 7) < 1e-12), logical(1))))
  expect_true(all(vapply(w$w[fb & day < 2L], function(x) all(abs(x - 1 / 8) < 1e-12), logical(1))))
  ## y_pred = a + M_pres %*% w_pres recalculado fora do modulo
  df <- as.data.frame(panel0)
  idx <- match(paste(df$lead, df$cut, df$ym), paste(w$lead, w$cut, w$ym))
  ref <- vapply(seq_len(nrow(df)), function(i) {
    ww <- w$w[[idx[i]]]
    w$a[idx[i]] + sum(unlist(df[i, names(ww)]) * ww)
  }, numeric(1))
  expect_equal(run0$y_pred, ref, tolerance = 1e-12)
  expect_false(anyNA(run0$y_pred))
  expect_true(all(is.na(df[[paste0("f_", short_m)]][pc_cmb_lead_day(df$lead) >= 2L])))
  ## CONTROL: com o membro curto forcado a zero o y_pred dos leads longos mudaria (peso ausente != zero)
  expect_true(all(is.finite(df[[paste0("f_", short_m)]][pc_cmb_lead_day(df$lead) < 2L])))
})

test_that("M4.2c - AC2 FORBIDDEN cauda mascarada delta=0; CONTROL delivery_days=9 so em O+2..O+8", {
  day <- pc_cmb_lead_day(panel0$lead)
  expect_identical(panel0$lead, panel_t$lead)
  expect_lt(max(abs(run_t$y_pred - run0$y_pred)), 1e-12)                       # FORBIDDEN
  dlt <- abs(run_9$y_pred - run0$y_pred)
  expect_gt(max(dlt[day >= 2L]), 1e-5)                                         # CONTROL
  expect_lt(max(dlt[day < 2L]), 1e-12)
  ## caminho normal: a cauda fora da faixa e recusada pelo M4.2b no campo delivery_days (sem delta)
  expect_true(any(!is.na(as.data.frame(panel_9)[[paste0("f_", short_m)]][day >= 2L])))
})

test_that("M4.2c - AC3 faixa de saida e members_by_lead", {
  out <- fn(".prevcarga_cmb_trim_output")(panel0, run0$y_pred)
  tz <- pc_cmb_tz
  o <- lubridate::ymd(out$forecast_origin, tz = tz)
  expect_false(anyNA(out$y_pred))
  expect_true(all(out[, min(target_datetime), by = "forecast_origin"]$V1 ==
                    lubridate::ymd(unique(out$forecast_origin), tz = tz) + lubridate::hours(9) + lubridate::minutes(30)))
  expect_true(all(out[, max(target_datetime), by = "forecast_origin"]$V1 ==
                    lubridate::ymd(unique(out$forecast_origin), tz = tz) + lubridate::days(9)))
  expect_identical(nrow(out), nrow(panel0))
  mbl <- fn("prevcarga_combina_members_by_lead")(run0$weights)
  expect_identical(nrow(mbl), length(unique(panel0$lead)))
  expect_true(all(mbl$n_members[pc_cmb_lead_day(mbl$lead) >= 2L] == 7L))
  expect_true(all(mbl$n_members[pc_cmb_lead_day(mbl$lead) < 2L] == 8L))
  expect_true(all(vapply(mbl$members[pc_cmb_lead_day(mbl$lead) >= 2L], function(x) !short_m %in% x, logical(1))))
  expect_identical(mbl$members[[1L]], sort(members$sigla, method = "radix"))

  ## 8 membros com delivery_days = 2 e cauda cheia declarada ate 9: sem membro cobrindo, a linha nao e emitida
  l2 <- long0 <- pc_cmb_long8_lead(members, short_days = 2L, long_days = 2L)
  for (s in members$sigla) l2 <- pc_cmb_fill_tail(l2, s, members)
  p2 <- pc_cmb_panel_from_long(env, l2)
  expect_gt(nrow(p2), nrow(pc_cmb_panel_from_long(env, long0)))
  r2 <- pc_cmb_run_combina(env, p2)
  out2 <- fn(".prevcarga_cmb_trim_output")(p2, r2$y_pred)
  expect_false(anyNA(out2$y_pred))
  expect_true(all(out2[, max(target_datetime), by = "forecast_origin"]$V1 ==
                    lubridate::ymd(unique(out2$forecast_origin), tz = tz) + lubridate::days(2)))
  expect_true(all(out2[, min(target_datetime), by = "forecast_origin"]$V1 ==
                    lubridate::ymd(unique(out2$forecast_origin), tz = tz) + lubridate::hours(9) + lubridate::minutes(30)))
  expect_true(all(fn("prevcarga_combina_members_by_lead")(r2$weights)$n_members == 8L))

  ## CONTROL: buraco dentro da faixa (y_pred NA) e linha faltando na grade abortam, nunca sao preenchidos
  yp <- run0$y_pred; yp[5L] <- NA_real_
  expect_contract_id(fn(".prevcarga_cmb_trim_output")(panel0, yp), "contract.combina_input", "trim_output:y_pred NA")
  expect_contract_id(fn(".prevcarga_cmb_trim_output")(panel0[-5L], run0$y_pred[-5L]), "contract.combina_input",
                     "faixa de saida")
  expect_contract_id(fn("prevcarga_combina_members_by_lead")(data.frame(lead = 1L)), "contract.combina_input",
                     "members_by_lead")
})

test_that("M4.2c - AC4 sem literais de faixa/sigla, troca de membro curto pelo run-manifest, dd inconsistente", {
  src <- readLines(file.path(pc_root(), "scripts", "lib", "combination.R"), warn = FALSE)
  code <- sub("#.*$", "", src)
  for (s in members$sigla) {
    expect_false(any(grepl(sprintf("[\"']%s[\"']", s), code)), info = s)
  }
  expect_false(any(grepl("O\\s*\\+\\s*[18]\\b", code)))
  expect_false(any(grepl("days\\(\\s*[29]\\s*\\)", code)))
  ## trocar qual membro e curto no run-manifest muda a cobertura sem tocar no codigo
  other <- long_ms[[1L]]
  provA <- attr(long0, "provenance")
  provB <- pc_cmb_set_days(pc_cmb_set_days(provA, short_m, 9L), other, 2L)
  covA <- fn("prevcarga_member_coverage")(provA, panel0)
  covB <- fn("prevcarga_member_coverage")(provB, panel0)
  last <- function(cv, s) unique(cv$last_lead[cv$member == s])
  expect_identical(last(covA, short_m), 95L)
  expect_identical(last(covB, other), 95L)
  expect_identical(last(covB, short_m), last(covA, other))
  expect_identical(nrow(covA), nrow(panel0) * nrow(members))
  ## delivery_days inconsistente no mesmo (member, cut)
  dup <- pc_cmb_set_days(provA, short_m, 3L)[provA$member == short_m]
  dup$run_id <- "run2"
  bad <- data.table::rbindlist(list(provA, dup))
  expect_contract_id(fn("prevcarga_member_coverage")(bad, panel0), "contract.member_misaligned",
                     sprintf("%s:delivery_days inconsistente no corte 0900", short_m))
  nas <- pc_cmb_set_days(provA, short_m, NA_integer_)
  expect_contract_id(fn("prevcarga_member_coverage")(nas, panel0), "contract.member_misaligned", "delivery_days")
  expect_contract_id(fn("prevcarga_build_panel")(pc_cmb_with_prov(long0, bad)), "contract.member_misaligned",
                     "delivery_days")
})
