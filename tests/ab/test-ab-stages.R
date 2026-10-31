## test-ab-stages.R -- comparadores dos estagios A-E do A/B do corte (M8.8; ADR-0034, ADR-0036, ADR-0025, ADR-0032 c).
## Fixtures sinteticos em tempdir (nunca data/): o oraculo e simulado no layout dele (LONG de 8 colunas, rotulo de INICIO, digitos civis sem
## fuso, forecast_origin Date, origens O-1 e O) e passa pelo relayout real; o clean-room entra no LONG nativo (interval_end). Cada FORBIDDEN
## (Delta <= 1e-5) tem o seu CONTROL (Delta != 0). Rodada real contra o oraculo em R 4.2.3: test-ab-corte.R (skip explicito sem referencia).
EXC <- AB$pc_ab_load_excecoes(AB_EXC)
TOL <- AB$pc_ab_load_limiares(file.path(ab_root, "tests", "ab", "limiares_ab.yaml"))$A

test_that("[AB.A] FORBIDDEN: clean-room == oraculo (|D| <= 1e-5) em RF e LGBM, nas duas classes de dia O", {
  cs <- rbind(ab_case("RF", "random-forest"), ab_case("LGBM", "lgbm", dd = 2L))
  w <- ab_build_world(cs)
  r <- AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  expect_identical(r$status, c("PASS", "PASS"))
  expect_true(all(r$delta_max <= TOL))
  expect_match(r$detalhe[[1L]], "fonte_dia_O=O\\+O-1")   # RF: dia O vem do passo da origem O-1 (o que o oraculo calcula e descarta)
  expect_match(r$detalhe[[2L]], "fonte_dia_O=O\\+O-1")   # LGBM (2 dias): dia O da origem O-1, dia O+1 da origem O
  expect_identical(AB$pc_ab_load_limiares(file.path(ab_root, "tests", "ab", "limiares_ab.yaml"))$B, 1e-5)
})

test_that("[AB.A] CONTROL: +0,2 MW numa folha da referencia com manifest regerado => FAIL de valor; sem regerar o manifest => integridade", {
  cs <- ab_case("RF", "random-forest")
  w <- ab_build_world(cs, ref_bump = 0.2)               # o relayout regera o manifest.json da referencia adulterada
  r <- AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  expect_identical(r$status, "FAIL")
  expect_equal(r$delta_max, 0.2, tolerance = 1e-9)
  expect_match(r$detalhe, "fora de ab_excecoes.yaml")
  ## adulteracao sem regerar o manifest: a fase de integridade do verificar acusa antes de qualquer valor (exit 9)
  w0 <- ab_build_world(cs)
  f <- file.path(w0$ref, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet"))
  x <- arrow::read_parquet(f); x$y_pred <- x$y_pred + 0.2; arrow::write_parquet(x, f)
  e <- tryCatch(AB$pc_ab_stage_a(w0$clean, w0$ref, cs, ab_ctx(), EXC), prevcarga_error = function(e) e)
  expect_s3_class(e, "prevcarga_error")
  expect_identical(e$id, "integrity.manifest_sha")
})

## referencia do runner com `result` em 2 arquivos (modo split: LONG da origem O-1 e da origem O) e a coluna fonte_dia_O do index
ab_world_split <- function(cs, dup = FALSE) {
  w <- ab_build_world(cs)
  long <- ab_oracle_long(cs); O <- as.Date(cs$origem)
  d <- file.path(w$refraw, "results", cs$mdir, cs$area, "split")
  dir.create(d, recursive = TRUE)
  arrow::write_parquet(long[long$forecast_origin == O - 1L, ], file.path(d, "long_Om1.parquet"))
  arrow::write_parquet(if (dup) long else long[long$forecast_origin == O, ], file.path(d, "long_O.parquet"))
  idx <- data.frame(caso = cs$caso, sigla = cs$sigla, mdir = cs$mdir, exogena = "", area = cs$area, origem = cs$origem,
                    result = paste(file.path("results", cs$mdir, cs$area, "split", c("long_Om1.parquet", "long_O.parquet")), collapse = ";"),
                    rc = "0", modo = "split", fonte_dia_O = "O-1:mesmo_artefato(train_origin=2026-08-31)", stringsAsFactors = FALSE)
  utils::write.table(idx, file.path(w$refraw, "index.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  ref2 <- file.path(w$base, "ref2")
  list(w = w, ref = ref2, run = function() AB$pc_ab_relayout(w$refraw, ref2, list(tz = AB_TZ)))
}

test_that("[AB.A] relayout de varios LONG por caso (modo split) + fonte do dia O do runner no detalhe; CONTROL: instante repetido entre eles reprova", {
  cs <- ab_case("RF", "random-forest")
  k <- ab_world_split(cs); k$run()
  expect_true(file.exists(file.path(k$ref, "fonte_dia_O.tsv")))
  r <- AB$pc_ab_stage_a(k$w$clean, k$ref, cs, ab_ctx(), EXC)
  expect_identical(r$status, "PASS")
  expect_match(r$detalhe, "fonte_dia_O=O+O-1 oraculo_dia_O=O-1:mesmo_artefato(train_origin=2026-08-31)", fixed = TRUE)
  k2 <- ab_world_split(cs, dup = TRUE)
  e <- tryCatch(k2$run(), prevcarga_error = function(e) e)
  expect_s3_class(e, "prevcarga_error"); expect_identical(e$id, "contract.ab_stage_input")
})

test_that("[AB.A] CONTROL: relayout sem a re-rotulagem para interval_end => as chaves divergem (FAIL), nunca Delta = 0", {
  cs <- ab_case("RF", "random-forest")
  w <- ab_build_world(cs, relabel = FALSE)
  r <- AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  expect_identical(r$status, "FAIL")
  expect_gt(r$delta_max, TOL)
  ## relayout: +30 min nos semi-horarios, +60 nos horarios; o rotulo de inicio vira interval_end no fuso da carga
  raw <- data.frame(area = "SP", forecast_origin = as.Date("2026-09-10"), target_datetime = as.POSIXct("2026-09-10 08:30:00", tz = "UTC"),
                    lead = 18L, y_pred = 1, y_true = NA_real_, d_plus = 0L, granularity_min = 30L)
  expect_identical(format(AB$pc_ab_relabel(raw, AB_TZ)$target_datetime, "%H:%M"), "09:00")
  raw$granularity_min <- 60L
  expect_identical(format(AB$pc_ab_relabel(raw, AB_TZ)$target_datetime, "%H:%M"), "09:30")
  expect_identical(AB$pc_ab_relabel(raw, AB_TZ)$forecast_origin, "2026-09-10")
  expect_identical(names(AB$pc_ab_relabel(raw, AB_TZ, relabel = FALSE)), names(AB$prevcarga_schema_long))
})

test_that("[AB.A] faixa delivery_days: LGBM O..O+1 (delivery_days 2) e RF O..O+8 (9); o resto vira fora_da_faixa; pedir LGBM em O+2 e erro do comparador", {
  cs <- rbind(ab_case("RF", "random-forest"), ab_case("LGBM", "lgbm", dd = 2L))
  w <- ab_build_world(cs, extra_days = 1L)                       # o clean-room entrega 1 dia a mais que a faixa em cada membro
  r <- AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  expect_identical(r$status, c("PASS", "PASS"))
  expect_equal(r$n, c(48L * 9L - 18L, 48L * 2L - 18L))          # 09:30 do dia O ate 00:00 de O+9 (RF, delivery_days 9) / de O+2 (LGBM, 2)
  expect_match(r$detalhe, "fora_da_faixa=48", all = TRUE)       # o dia extra e contado, nunca comparado
  e <- tryCatch(AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC, request_end = list(LGBM = "2026-09-12")),
                prevcarga_error = function(e) e)
  expect_identical(e$id, "contract.ab_fora_da_faixa")
  expect_error(AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC, request_end = list(LGBM = "2026-09-11")), NA)   # O+1 e o ultimo dia da faixa do LGBM
})

test_that("[AB.A] entrada ausente nunca e Delta = 0 silencioso", {
  cs <- ab_case("RF", "random-forest")
  w <- ab_build_world(cs)
  unlink(file.path(w$clean, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet")))
  AB$pc_ab_manifest_write(file.path(w$clean, cs$origem))
  r <- AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  expect_identical(r$status, "FAIL")
  expect_match(r$detalhe, "entrada ausente")
})

test_that("[AB.A] excecoes semeadas so casam por membro + estagio + condicao + teto (a lista e fechada)", {
  at <- function(cs, bump) {
    w <- ab_build_world(cs, ref_bump = bump)
    AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  }
  ## regdin (ADR-0036): ate max_rel 1,3e-5; os y_pred sinteticos sobem a ~1.2e3 => 0,010 = 0,8e-5 dentro, 0,05 = 4e-5 fora
  rgd <- ab_case("RGD_MED", "dessem-regdin", exogena = "MED")
  r <- at(rgd, 0.010); expect_identical(r$status, "EXCECAO"); expect_identical(r$excecao_id, "regdin.versao_pacotes")
  r <- at(rgd, 0.050); expect_identical(r$status, "FAIL"); expect_match(r$detalhe, "teto max_rel")
  ## o mesmo desvio num membro fora da lista (RF) e FAIL: so o regdin tem a excecao
  expect_identical(at(ab_case("RF", "random-forest"), 0.010)$status, "FAIL")
  ## dessem-linear sp-2024-11-15 (DL9-GAP-2): teto 0,5 MW e 5e-5 rel, so SP em 2024-11-15
  sp <- ab_case("LIN_MED", "dessem-linear", "SP", "2024-11-15", "2024-11-15T09:00", exogena = "MED")
  r <- at(sp, 0.04); expect_identical(r$status, "EXCECAO"); expect_identical(r$excecao_id, "dessem_linear.sp_2024_11_15_residuo_numerico")
  expect_identical(at(sp, 0.6)$status, "FAIL")                                          # acima do teto max_abs
  other <- ab_case("LIN_MED", "dessem-linear", "SP", "2026-09-10", "2026-09-10T09:00", exogena = "MED")
  expect_identical(at(other, 0.04)$status, "FAIL")                                       # outra origem: a condicao nao casa
  expect_identical(at(ab_case("LIN_MED", "dessem-linear", "RS", "2024-11-15", "2024-11-15T09:00", exogena = "MED"), 0.04)$status, "FAIL")
})

test_that("[AB.A] hora incompleta do clima previsto (dessem-linear): so as ultimas 3 h da faixa; divergencia no meio da faixa e FAIL", {
  cs <- ab_case("LIN_MED", "dessem-linear", exogena = "MED")
  w <- ab_build_world(cs)
  f <- file.path(w$ref, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet"))
  bump_at <- function(pick) {
    x <- arrow::read_parquet(f); i <- pick(x); x$y_pred[i] <- x$y_pred[i] + 50; arrow::write_parquet(x, f); AB$pc_ab_manifest_write(file.path(w$ref, cs$origem))
    AB$pc_ab_stage_a(w$clean, w$ref, cs, ab_ctx(), EXC)
  }
  end <- as.numeric(lubridate::force_tz(lubridate::as_datetime(as.Date("2026-09-10") + 9L), AB_TZ))   # fim da faixa do RF/dessem (O+9 00:00)
  last <- function(x) { t <- as.numeric(x$target_datetime); x$forecast_origin == "2026-09-10" & t > end - 2 * 3600 & t <= end }
  r <- bump_at(last)
  expect_identical(r$status, "EXCECAO"); expect_identical(r$excecao_id, "dessem_linear.hora_incompleta_clima_previsto")
  w2 <- ab_build_world(cs); f <- file.path(w2$ref, cs$origem, "membros", cs$sigla, paste0(cs$area, ".parquet"))
  x <- arrow::read_parquet(f); x$y_pred[x$forecast_origin == "2026-09-10" & x$d_plus == 3L][5L] <- 9999; arrow::write_parquet(x, f)
  AB$pc_ab_manifest_write(file.path(w2$ref, cs$origem))
  r2 <- AB$pc_ab_stage_a(w2$clean, w2$ref, cs, ab_ctx(), EXC)
  expect_identical(r2$status, "FAIL"); expect_match(r2$detalhe, "so cobre as ultimas 3 h")
})

test_that("[AB.A] CONTROL de clima: deck sintetico com emissao 06Z -- available_until no lugar de origin_midnight_utc tira o membro do limiar", {
  deck <- tempfile("deck06-"); O <- as.Date("2026-09-10")
  AB$prevcarga_fixture_deck(deck, seed = 7, areas = "SP", days = O + (-3:3), weather_lag_hours = 0L,
                            load_until = lubridate::ymd_hm("2026-09-10 09:00", tz = AB_TZ))
  au <- lubridate::ymd_hm("2026-09-10 09:00", tz = AB_TZ)
  member <- function(rule) {                      # membro de brinquedo que so depende do clima previsto (a regra e do leitor do clean-room)
    ctx <- list(root = ab_root, data_dir = deck, weather_issue_cutoff = rule)
    fc <- AB$prevcarga_read_weather(ctx, "forecast", "SP", O, O + 1L, origin = O, truncate_at = au)
    fc <- fc[order(fc$valid_time)]
    list(y = 1000 + 10 * fc$temperature, issue = max(fc$issue_time))
  }
  oracle <- member("origin_midnight_utc")         # regra do oraculo (ADR-0024)
  ok <- member("origin_midnight_utc"); bad <- member("available_until")
  expect_lt(max(abs(ok$y - oracle$y)), TOL)       # FORBIDDEN: mesma regra => Delta = 0
  expect_gt(max(abs(bad$y - oracle$y)), TOL)      # CONTROL: a emissao 06Z entra => Delta != 0
  expect_gt(as.numeric(bad$issue), as.numeric(oracle$issue))
  ## o gerador de casos recusa deck cujo corte derivado nao e hora cheia / difere do esperado
  y <- tempfile(fileext = ".yaml"); writeLines(c("origens:", "  - {origem: \"2026-09-10\", deck: store, available_until_esperado: \"10:00\"}"), y)
  ctx <- list(root = ab_root, data_dir = deck, weather_issue_cutoff = "origin_midnight_utc")
  ora <- tempfile("ora-")
  for (m in c("random-forest", "lgbm", "holt-winters", "dessem-regdin", "dessem-linear")) {
    dir.create(file.path(ora, "models", m), recursive = TRUE); writeLines('{"protocol": "daily"}', file.path(ora, "models", m, "job.json"))
  }
  e <- tryCatch(AB$pc_ab_cases(y, NULL, ctx, oracle = ora, membros = "RF", areas = "SP"), prevcarga_error = function(e) e)
  expect_identical(e$id, "contract.ab_deck_not_full_hour")
})

## --- B ----------------------------------------------------------------------------------------------------------------------------
ab_ratio <- function(O, days = 40L, tz = AB_TZ) {
  st <- seq(lubridate::force_tz(lubridate::as_datetime(O - days), tz), by = "30 min", length.out = 48L * days)
  data.frame(interval_start = st, interval_end = prevcarga_end(st), ratio = 1.04 + 0.01 * sin(as.numeric(st) / 7e4))
}
prevcarga_end <- function(st) AB$prevcarga_long_target_datetime(st, 30L, AB_TZ)
ab_ref_profile <- function(ratio, O, win) {                        # perfil "do oraculo": calculado independentemente, por bucket hora x mes x dow
  d <- lubridate::as_date(ratio$interval_start, tz = AB_TZ)
  w <- ratio[d >= O - win & d <= O - 1L, ]
  b <- data.frame(hour = lubridate::hour(w$interval_start), month = lubridate::month(w$interval_start),
                  dow = lubridate::wday(w$interval_start, week_start = 7L), ratio = w$ratio)
  a <- stats::aggregate(ratio ~ hour + month + dow, b, mean); names(a)[4L] <- "k"; a
}

test_that("[AB.B] FORBIDDEN: k-hat por origem == oraculo (|D| <= 1e-5); CONTROL: ratio perturbado em 1 dia da janela tira do limiar", {
  O <- as.Date("2026-09-10"); ratio <- ab_ratio(O)
  ref <- ab_ref_profile(ratio, O, 30L)
  r <- AB$pc_ab_stage_b(ratio, O, ref, 30L, "S", EXC)
  expect_identical(r$status, "PASS"); expect_lte(r$delta_max, TOL); expect_identical(r$modo, "por_origem")
  pert <- ratio; hit <- lubridate::as_date(pert$interval_start, tz = AB_TZ) == O - 5L
  pert$ratio[hit] <- pert$ratio[hit] + 0.05
  rp <- AB$pc_ab_stage_b(pert, O, ref, 30L, "S", EXC)             # a referencia ficou com o ratio original
  expect_identical(rp$status, "FAIL"); expect_gt(rp$delta_max, TOL)
  r2 <- AB$pc_ab_stage_b(ratio, O, ref[-1L, ], 30L, "S", EXC)       # bucket sem par nunca e Delta = 0
  expect_identical(r2$status, "FAIL"); expect_match(r2$detalhe, "buckets sem par")
})

test_that("[AB.B] apply de producao: khat_origin = O compara (PASS); perfil reutilizado (khat_origin = O-5) e EXCECAO so se a lista a conhece", {
  O <- as.Date("2026-09-10"); ratio <- ab_ratio(O); ref <- ab_ref_profile(ratio, O, 30L)
  st <- seq(lubridate::force_tz(lubridate::as_datetime(O), AB_TZ), by = "30 min", length.out = 48L)
  lg <- data.frame(target_datetime = prevcarga_end(st), y_pred = 100 + seq_along(st))
  ap <- function(ko) list(khat_origin = ko, khat_age_days = as.integer(O - ko), n_fallback = 3L, long = lg, ref_long = lg)
  r0 <- AB$pc_ab_stage_b(ratio, O, ref, 30L, "S", EXC, apply = ap(O))
  expect_identical(r0$status, c("PASS", "PASS"))
  fx <- AB$pc_ab_load_excecoes(ab_fixture_excecoes())
  r5 <- AB$pc_ab_stage_b(ratio, O, ref, 30L, "S", fx, apply = ap(O - 5L))
  expect_identical(r5$status, c("PASS", "EXCECAO"))
  expect_identical(r5$excecao_id[[2L]], "reconcile.apply_profile_reuse")
  expect_match(r5$detalhe[[2L]], "khat_origin=2026-09-05 khat_age_days=5 n_fallback=3")
  r5x <- AB$pc_ab_stage_b(ratio, O, ref, 30L, "S", EXC, apply = ap(O - 5L))    # a lista real nao a tem: FAIL, nunca entrada nova
  expect_identical(r5x$status[[2L]], "FAIL"); expect_match(r5x$detalhe[[2L]], "fora de ab_excecoes.yaml")
})

## --- C ----------------------------------------------------------------------------------------------------------------------------
ab_delivery <- function(codec = "snappy", skip = NULL) {
  d <- tempfile("deliv-"); O <- as.Date("2026-09-10")
  st <- seq(lubridate::force_tz(lubridate::as_datetime(O), AB_TZ), by = "30 min", length.out = 48L)
  for (p in c("SIN.parquet", "subsistemas/SECO.parquet", "folhas/SP.parquet", "folhas/RS.parquet")) {
    if (p %in% skip) next
    dir.create(dirname(file.path(d, p)), recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(data.frame(t = st, y = seq_along(st) * 1.5), file.path(d, p), compression = codec, version = "2.6", use_dictionary = FALSE)
  }
  AB$pc_ab_manifest_write(d)
  d
}

ab_copy_tree <- function(a) { b <- tempfile("deliv-"); dir.create(b); file.copy(list.files(a, full.names = TRUE), b, recursive = TRUE); b }

test_that("[AB.C] FORBIDDEN: conjunto de arquivos e sha256 identicos; CONTROL: arquivo removido => FAIL de conjunto; outro codec => FAIL de sha", {
  a <- ab_delivery(); b <- ab_copy_tree(a)          # copia de bytes (o M6.2 copia, nunca reserializa)
  r <- AB$pc_ab_stage_c(a, b, origem = "2026-09-10")
  expect_true(all(r$status == "PASS")); expect_identical(nrow(r), 4L)
  miss <- ab_delivery(skip = "folhas/RS.parquet")
  rm <- AB$pc_ab_stage_c(miss, b)
  expect_identical(rm$status[rm$area == "RS"], "FAIL"); expect_match(rm$detalhe[rm$area == "RS"], "conjunto de arquivos difere")
  codec <- ab_delivery(codec = "zstd")                              # mesmo conteudo logico, outros bytes (manifest coerente)
  rc <- AB$pc_ab_stage_c(codec, b)
  expect_true(all(rc$status == "FAIL")); expect_match(rc$detalhe[[1L]], "sha256 difere")
  ## inverter 1 byte do arquivo tambem acusa (o sha vem do disco, nao do manifest)
  flip <- ab_copy_tree(a); f <- file.path(flip, "SIN.parquet"); x <- readBin(f, "raw", file.size(f)); x[length(x) - 9L] <- as.raw(bitwXor(as.integer(x[length(x) - 9L]), 1L))
  writeBin(x, f)
  expect_identical(AB$pc_ab_stage_c(flip, b)$status[[1L]], "FAIL")
})

## --- D ----------------------------------------------------------------------------------------------------------------------------
for (h in c("helper-combination.R", "helper-combination-lead.R", "helper-combination-cli.R"))
  sys.source(file.path(ab_root, "scripts", "lib", "tests", h), envir = AB)

test_that("[AB.D] so os gates de PROPRIEDADE da combina (sem eps, sem dominancia, sem paridade de solver); degenerado reprova", {
  expect_false("eps" %in% names(formals(AB$pc_ab_stage_d)))
  qp <- AB$pc_cmb_quality_panel()
  r <- AB$pc_ab_stage_d(qp, ab_ctx(), origem = "2024-01-01", area = "X")
  expect_identical(r$status, "PASS"); expect_match(r$detalhe, "simplex_ok=TRUE")
  ## a combina perdendo para o EQW em WMAPE (qualidade) NAO reprova o estagio D: a qualidade so e reportada (ADR-0025)
  expect_identical(AB$pc_ab_stage_d(AB$pc_cmb_loses_panel(), ab_ctx())$status, "PASS")
  expect_false(any(grepl("prevcarga_combina_gate\\(.*eps", deparse(AB$pc_ab_stage_d))))
})

## --- E ----------------------------------------------------------------------------------------------------------------------------
test_that("[AB.E] qualidade so REPORTADA e estavel em 10 execucoes seguidas; nunca vira FAIL nem entra no veredito", {
  cs <- ab_case("RF", "random-forest")
  lg <- ab_clean_long(cs); lg$y_true <- lg$y_pred * 1.01
  runs <- lapply(1:10, function(i) AB$pc_ab_stage_e(lg, ab_ctx(), origem = cs$origem))
  expect_true(all(vapply(runs, function(r) identical(r, runs[[1L]]), NA)))
  expect_true(all(runs[[1L]]$status == "REPORT")); expect_match(runs[[1L]]$detalhe[[1L]], "^wmape=")
  expect_gt(nrow(runs[[1L]]), 0L)
})
