## test-harness-determinism.R -- M7.V: gate de determinismo do harness (reescrita testthat portavel de jobs/tests/determinism_gate.sh do
## oraculo, sem bash nem Docker). Duas rodadas --jobs 1 e uma --jobs 4 (PSOCK), cada uma num state_dir novo, comparam por identical() todos
## os long.parquet (membros, combined/, reconciled/). CONTROL: worker sem a semente do run.yaml diverge. Plugins-brinquedo que consomem o
## RNG (runif sem set.seed proprio) + deck por formula em tempdir; sem variaveis de ambiente.
library(testthat)

DET_O1 <- lubridate::ymd("2026-10-05")
DET_ORIGINS <- DET_O1 + 0:3
DET_ROOT <- pc_m7h_root(noise = TRUE)
DET_DECK <- pc_m7h_deck(DET_O1 - 30L, max(DET_ORIGINS))

## uma rodada completa (MAP + REDUCE) num state_dir novo, em R filho (lib no globalenv, como a CLI); devolve o LONG coletado
pc_m7_det_run <- function(jobs, hooks = "NULL") {
  state <- tempfile("m7-det-state-"); dir.create(state)
  withr::defer(unlink(state, recursive = TRUE), envir = parent.frame())
  body <- c(sprintf("res <- prevcarga_backtest(ctx, origins = lubridate::ymd(%s), cortes = '08:00', jobs = %dL, keep_going = TRUE, baseline = 'none')",
                    paste(deparse(as.character(DET_ORIGINS)), collapse = ""), as.integer(jobs)),
            "saveRDS(list(root = res$paths$root, verdict = res$summary$verdict, ok = res$summary$ok, total = res$summary$total), OUT)")
  r <- pc_m7h_child(body, DET_ROOT, DET_DECK, state, hooks = hooks)
  expect_identical(r$status, 0L, info = paste(r$stderr, collapse = "\n"))
  expect_identical(r$out$verdict, "ok")
  list(long = pc_m7_collect_long(r$out$root), summary = r$out)
}

## copia sem o indice auxiliar que o `[` do data.table cria (identical() compara atributos)
plain <- function(x) { x <- data.table::copy(x); data.table::setindex(x, NULL); x }

noseed_hook <- "list(post_runtime = function() assign('.m7_seed_apply', function(seed) NULL, envir = globalenv()))"

test_that("[AC1] FORBIDDEN: membros, combinado e reconciliado identicos (delta 0) entre 2 rodadas --jobs 1 e 1 rodada --jobs 4", {
  a <- pc_m7_det_run(1L); b <- pc_m7_det_run(1L); c4 <- pc_m7_det_run(4L)
  la <- a$long
  expect_setequal(unique(la$estagio), c("member", "combined", "reconciled"))        # o teste cobre os tres estagios
  expect_gt(nrow(la[estagio == "member"]), 0L); expect_gt(nrow(la[estagio == "combined"]), 0L); expect_gt(nrow(la[estagio == "reconciled"]), 0L)
  expect_true("SIN" %in% la[estagio == "reconciled"]$area)
  expect_identical(plain(b$long), plain(la))                                                      # identical(), nao all.equal: y_pred, y_true e chaves
  expect_identical(plain(c4$long), plain(la))
  expect_identical(c4$summary$ok, a$summary$ok)
  ## o toy realmente consome o RNG: sem a semente do run.yaml as linhas dos membros divergiriam (o teste tem dentes)
  expect_false(isTRUE(all.equal(la[estagio == "member"]$y_pred, rev(la[estagio == "member"]$y_pred))))
})

test_that("[AC1] CONTROL: worker sem a semente do run.yaml (sabotagem no fixture) diverge de --jobs 1", {
  a <- pc_m7_det_run(1L)
  s <- pc_m7_det_run(4L, hooks = noseed_hook)
  expect_false(identical(plain(s$long), plain(a$long)))
  expect_gt(max(abs(s$long[estagio == "member"]$y_pred - a$long[estagio == "member"]$y_pred)), 0)
})

test_that("[AC1] pc_m7_collect_long ordena por chave e cobre so os long.parquet da rodada (isca CONTROL de ordem)", {
  a <- pc_m7_det_run(1L)
  l <- a$long
  shuffled <- l[rev(seq_len(nrow(l)))]
  expect_false(identical(plain(shuffled), plain(l)))                                              # embaralhar quebra identical()
  data.table::setorderv(shuffled, c("estagio", "membro", "area", "forecast_origin", "corte", "target_datetime"), na.last = TRUE)
  expect_identical(plain(shuffled), plain(l))
})
