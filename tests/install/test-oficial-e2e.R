## test-oficial-e2e.R -- `verificar --oficial` ponta a ponta no bundle INSTALADO (M8.9, AC 2; ADR-0036, ADR-0039, ADR-0040).
## O mundo (helper-e2e.R): estado produtor treinado pelo lancador, rodada oficial, `release-pack` REAL -> bucket LOCAL servido por httpuv.
## Aqui: o `prevcarga-sync` instalado baixa os artefatos oficiais (modelos, combina, k-hat) para o estado LOCAL e a inferencia oficial
## da chave (origem, available_until, versao) para um estado separado; `forecast --origin O` roda no estado local e `verificar --saida <s>
## --oficial` o compara com a inferencia oficial: PASS no trilho `mesma-plataforma`. Nunca fala com bucket real nem com a rede.
library(testthat)
pci_skip_unless_ready()
W <- pc_e2e_world()
S <- pc_e2e_synced(W)
O <- lubridate::ymd(W$origin)

verificar <- function(saida, ...) pc_e2e_cli(W$root, c("verificar", paste0("--saida=", saida), "--oficial", paste0("--state=", S$oficial), ...))

LOCAL <- pc_e2e_forecast(W, W$deck)

test_that("prevcarga-sync instalou os artefatos oficiais e a inferencia oficial da mesma chave", {
  art <- list.files(file.path(S$local, "artifacts"), pattern = "^latest\\.txt$", recursive = TRUE)
  expect_true(all(c(paste0("combina/", PCE_LEAVES, "/latest.txt"), paste0("reconcile/", PCE_SS, "/latest.txt"),
                    paste0("random-forest/", PCE_LEAVES, "/latest.txt"), paste0("lgbm/", PCE_LEAVES, "/latest.txt")) %in% art))
  of <- list.files(file.path(S$oficial, "forecasts"), pattern = "^manifest\\.json$", recursive = TRUE)
  expect_length(of, 1L)
  expect_match(of, paste0("^", W$origin, "/20261015T0900/[0-9a-f]{12}/manifest\\.json$"))
  expect_identical(LOCAL$cli$status, 0L)
  expect_false(dir.exists(file.path(S$local, "forecasts", "..", "forecasts-oficial")))   # o estado local nao recebeu a entrega oficial
})

test_that("[FORBIDDEN] forecast local x inferencia oficial da mesma chave: verificar --oficial PASS mesma-plataforma, exit 0, delta 0", {
  v <- verificar(LOCAL$saida)
  expect_identical(v$status, 0L, info = paste(v$stdout, v$stderr))
  expect_true(v$json$ok); expect_identical(v$json$result$resultado, "PASS"); expect_identical(v$json$result$trilho, "mesma-plataforma")
  expect_gte(nrow(do.call(rbind, lapply(v$json$result$arquivos, as.data.frame))), 3L)
  expect_true(all(vapply(v$json$result$arquivos, function(a) a$pior_delta, 0) == 0))
  expect_identical(normalizePath(v$json$result$referencia), normalizePath(list.dirs(file.path(S$oficial, "forecasts"), recursive = TRUE)[
    grepl("/[0-9a-f]{12}$", list.dirs(file.path(S$oficial, "forecasts"), recursive = TRUE))]))
  ## a entrega local e a oficial sao a mesma chave e os mesmos bytes (o estado oficial veio do bucket, nao do local)
  oficial <- v$json$result$referencia
  ## SHA256SUMS = indice de transporte do bucket; `gerado_em` e o unico campo do manifest.json que e relogio da rodada
  sem_relogio <- function(d) { m <- jsonlite::read_json(file.path(d, "manifest.json")); m$gerado_em <- NULL; m }
  expect_identical(pc_e2e_delivery_shas(LOCAL$saida, skip = "manifest.json"), pc_e2e_delivery_shas(oficial, skip = c("SHA256SUMS", "manifest.json")))
  expect_identical(sem_relogio(LOCAL$saida), sem_relogio(oficial))
})

test_that("[CONTROL] load perturbado em O-1 (interval_end <= available_until): a rodada local nao e a oficial e o verificar NAO passa", {
  deck <- pc_perturb_load(W$deck, O - 1L, delta = 50)
  p <- pc_e2e_forecast(W, deck)
  expect_identical(p$cli$status, 0L)
  expect_false(identical(pc_tree_sha(deck), pc_tree_sha(W$deck)))
  rm_loc <- jsonlite::read_json(file.path(LOCAL$saida, "run_manifest.json")); rm_per <- jsonlite::read_json(file.path(p$saida, "run_manifest.json"))
  expect_false(identical(rm_per$data_hash, rm_loc$data_hash))        # a identidade da rodada mudou (data_hash cobre os parquets do deck)
  expect_false(identical(rm_per$versao, rm_loc$versao))              # e a chave (origem, available_until, versao) deixou de ser a oficial
  v <- verificar(p$saida)
  expect_false(identical(v$status, 0L))                              # FORBIDDEN dual: a divergencia nunca vira PASS
  expect_false(isTRUE(v$json$ok)); expect_identical(v$stderr, "")
  ## o desvio real do texto do AC (exit 12): a chave da rodada perturbada nao existe no estado oficial => exit 13 (ver o discovery do M8.9)
  expect_identical(v$status, 13L); expect_identical(v$json$error$id, "state.official_forecast_missing")
})

test_that("[CONTROL] valor da inferencia oficial adulterado (manifest regerado, mesma chave): verificar --oficial => FAIL exit 12 [prevcarga/repro]", {
  off <- pc_e2e_state_copy(S$oficial)
  key <- list.dirs(file.path(off, "forecasts"), recursive = TRUE)
  key <- key[grepl("/[0-9a-f]{12}$", key)]
  expect_length(key, 1L)
  p <- file.path(key, "SIN.parquet")
  x <- arrow::read_parquet(p); x$y_pred[[1L]] <- x$y_pred[[1L]] + 1; arrow::write_parquet(x, p)
  mf <- jsonlite::read_json(file.path(key, "manifest.json"))
  for (i in seq_along(mf$arquivos)) if (identical(mf$arquivos[[i]]$path, "SIN.parquet")) mf$arquivos[[i]]$sha256 <- digest::digest(file = p, algo = "sha256")
  jsonlite::write_json(mf, file.path(key, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
  v <- pc_e2e_cli(W$root, c("verificar", paste0("--saida=", LOCAL$saida), "--oficial", paste0("--state=", off)))
  expect_identical(v$status, 12L)
  expect_false(v$json$ok); expect_identical(v$json$error$id, "repro.value_out_of_tol"); expect_identical(v$json$error$class, "repro")
  expect_identical(v$stderr, "")
  ## FORBIDDEN dual: a mesma chamada contra o estado oficial INTACTO passa (o CONTROL tem dentes e o PASS nao e acidental)
  expect_identical(verificar(LOCAL$saida)$status, 0L)
})
