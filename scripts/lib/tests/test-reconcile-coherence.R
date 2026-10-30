## test-reconcile-coherence.R -- R.V: integracao Reconcile -> check-coherence (M3.4). Compoe folhas + subsistemas + SIN
## de uma rodada apply com perfil sintetico (um bucket k = 1,20 fora da faixa [1,00; 1,15]) e roda o modulo
## prevcarga_check_coherence e o verbo `check-coherence --long` por processo. Braco FORBIDDEN (k fora) pareado com CONTROL
## (todo k na faixa). Topologia sintetica SS1..SS3 em tempdir; nada de literal da topologia real.
skip_if_not_installed("docopt")
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
env <- pc_source_lib(c("errors", "runtime", "config", "topology", "provenance", "registry", "schema_long",
                       "deck_reader", "dates", "evaluation", "reconcile"))
O <- "2026-01-15"
Od <- lubridate::ymd(O)
tmp <- withr::local_tempdir()

## topologia de fixture com faixa de k-hat [1,00; 1,15]
topo_yaml <- pc_topology_fixture(3L, dir = file.path(tmp, "topo"))
writeLines(sub("khat_range: [1.0, 1.2]", "khat_range: [1.0, 1.15]", readLines(topo_yaml), fixed = TRUE), topo_yaml)
topo <- env$prevcarga_topology(path = topo_yaml, root = tempdir())
ctx0 <- list(tz = pc_rc_tz)
sctx <- function() {
  d <- file.path(tmp, paste0("state-", length(list.files(tmp)))); dir.create(file.path(d, "artifacts"), recursive = TRUE)
  list(state_dir = d, tz = pc_rc_tz, load_label = "start", topology_path = topo_yaml, root = tempdir())
}

## congela + promove um perfil para `ss` (k = base em todo bucket; `bump` = k de UM bucket hora 18 / jan / quinta)
freeze_profile <- function(ctx, ss, base = 1.05, bump = NULL) {
  prof <- pc_khat_profile_sentinel(glob = base)
  prof$profile$k <- base
  if (!is.null(bump)) prof$profile[hour == 18L & month == 1L & dow == 5L, k := bump]
  fit <- list(profile = prof$profile, glob = base, n_ratio = 30L,
              window_days = 30L, window_from = Od - 30L, window_to = Od - 1L)
  ratio <- pc_truth_ratio(seq(Od - 30L, Od - 1L, by = "day"), function(d) base, tz = pc_rc_tz)
  meta <- list(ss = ss, khat_origin = Od, window_days = 30L, window_from = fit$window_from, window_to = fit$window_to,
               profile_keys = env$prevcarga_khat_keys, week_start = 7L, areas = topo$subsystems[[ss]]$areas,
               loss = topo$subsystems[[ss]]$loss, n_ratio = fit$n_ratio, load_label = "start", timezone = pc_rc_tz,
               data_hash = env$prevcarga_khat_data_hash(ratio), code_commit = "abc", renv_lock_hash = strrep("a", 64))
  fz <- env$prevcarga_khat_freeze(fit, meta, ctx)
  suppressWarnings(env$prevcarga_khat_advance_latest(ss, fz$version, Od, ctx))
  fz$version
}

## roda o apply com perfis (bump so em SS1) e devolve o LONG composto folhas + subsistemas + SIN
run_apply <- function(bump) {
  ctx <- sctx()
  for (ss in topo$order) freeze_profile(ctx, ss, bump = if (identical(ss, topo$order[[1L]])) bump else NULL)
  leaves <- pc_leaves_long(topo, O, ctx0)
  res <- suppressWarnings(env$prevcarga_reconcile_apply(leaves, ctx, subsystems = topo$order))
  list(res = res, leaves = leaves,
       all = as.data.frame(data.table::rbindlist(c(list(leaves), lapply(res$long, as.data.frame)), use.names = TRUE)))
}
bu_of <- function(hit, leaves, ss) {
  lv <- leaves[leaves$area %in% topo$subsystems[[ss]]$areas, ]
  tapply(lv$y_pred, lv$target_datetime, sum)[as.character(hit$target_datetime)]
}

S1 <- topo$order[[1L]]
bad <- run_apply(bump = 1.20)
ok <- run_apply(bump = NULL)

test_that("AC1 k = 1,20 usado sem clamp: y_pred = BU_pred x 1,20 no bucket; os demais usam 1,05", {
  l <- bad$res$long[[S1]]
  st <- env$prevcarga_long_interval_start(l$target_datetime, l$granularity_min, pc_rc_tz)
  h18 <- lubridate::hour(st) == 18L
  expect_true(any(h18))
  r <- l$y_pred / as.numeric(bu_of(l, bad$leaves, S1))
  expect_true(all(abs(r[h18] - 1.20) < 1e-12))
  expect_true(all(abs(r[!h18] - 1.05) < 1e-12))
  ## CONTROL: as outras origens de perfil (sem bump) nao tem 1,20 em lugar nenhum
  lo <- ok$res$long[[S1]]
  expect_true(all(abs(lo$y_pred / as.numeric(bu_of(lo, ok$leaves, S1)) - 1.05) < 1e-12))
})

test_that("AC1 modulo: k fora da faixa => exit 11 coherence.khat no subsistema e instante; sin_sum nao dispara", {
  fx <- env$prevcarga_check_coherence(bad$all, topo, tol = 1e-5)
  expect_false(fx$ok)
  expect_identical(unname(env$prevcarga_exit_codes[fx$class]), 11L)
  expect_identical(fx$id, "coherence.khat")
  f <- fx$findings
  expect_true("coherence.khat" %in% f$id)
  expect_false("coherence.sin_sum" %in% f$id)
  expect_true(all(f$area[f$id == "coherence.khat"] == S1))
  st <- env$prevcarga_long_interval_start(f$target_datetime[f$id == "coherence.khat"], 30L, pc_rc_tz)
  expect_true(all(lubridate::hour(st) == 18L))
  expect_true(all(abs(f$value[f$id == "coherence.khat"] - 1.20) < 1e-9))
})

test_that("AC1 modulo CONTROL: k na faixa => ok, findings vazio, sem sin_sum (|SIN - soma| <= 1e-5)", {
  fx <- env$prevcarga_check_coherence(ok$all, topo, tol = 1e-5)
  expect_true(fx$ok)
  expect_identical(nrow(fx$findings), 0L)
  expect_false("coherence.sin_sum" %in% fx$findings$id)
  sub <- data.table::as.data.table(ok$all)
  s <- sub[area %in% topo$order, list(v = sum(y_pred)), by = target_datetime]
  sin <- sub[area == "SIN"]
  expect_lt(max(abs(sin$y_pred - s[match(sin$target_datetime, s$target_datetime)]$v)), 1e-5)
})

## ---- verbo por processo ----------------------------------------------------------------------------------------------
root <- pc_install_copy(mutate = function(r) file.copy(topo_yaml, file.path(r, "config", "topology.yaml"), overwrite = TRUE))
deck <- pc_deck_sentinel("start", days = Od + (-45:0), areas = c(topo$order, topo$leaves), issues = 0L,
                         dir = file.path(tmp, "deck"))
cc <- function(long) {
  p <- file.path(tmp, paste0("long-", length(list.files(tmp)), ".parquet"))
  arrow::write_parquet(long, p)
  r <- pc_cli(root, c("check-coherence", "--long", p, paste0("--data=", deck), paste0("--origin=", O), "--format=json"))
  list(status = r$status, j = jsonlite::fromJSON(paste(r$stdout, collapse = "\n"), simplifyVector = FALSE), r = r)
}

test_that("AC1 verbo check-coherence --long: k = 1,20 => exit 11 coherence.khat; k na faixa => exit 0", {
  b <- cc(bad$all)
  expect_identical(b$status, 11L, info = paste(b$r$stderr, collapse = "\n"))
  expect_identical(b$j$error$id, "coherence.khat")
  fl <- b$j$error$result$findings %||% b$j$result$findings
  ids <- vapply(fl, function(f) f$id, "")
  expect_true("coherence.khat" %in% ids); expect_false("coherence.sin_sum" %in% ids)
  expect_true(all(vapply(fl[ids == "coherence.khat"], function(f) f$area, "") == S1))
  expect_true(all(vapply(fl[ids == "coherence.khat"], function(f) !is.null(f$target_datetime), TRUE)))
  g <- cc(ok$all)
  expect_identical(g$status, 0L, info = paste(g$r$stderr, collapse = "\n"))
  expect_true(g$j$ok)
  expect_length(g$j$result$findings, 0L)
})
