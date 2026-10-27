## provenance.R — proveniência + freeze (M1, CONTRATO)
##
## Responsabilidade: definir CEDO o contrato que combina (M4) e training (M5) vão usar para congelar
## artefatos e carimbar saídas — para nenhum módulo nascer com local descartável (saveRDS ad-hoc,
## version=timestamp) que uma fase posterior re-centraliza.
##
## Contrato alvo:
##   - version CONTENT-ADDRESSED = hash(model_id + data_fingerprint + cutoff + feature_config + code_commit +
##     renv_lock_hash). model_id (mdir/name do run.yaml) + feature_config (config efetiva, incl. exogena) são
##     OBRIGATÓRIOS p/ os 5 membros DESSEM (2 modelos × exogena) não colidirem (ADR-0021).
##     trained_at/package_version são METADADO LATERAL (NÃO entram no hash — senão
##     reintroduzem não-determinismo; alinhado ao GATE 10 da referência).
##   - run-manifest: {data_hash, as_of, member_set_hash, model_versions, code_commit, renv_lock_hash,
##     seed, platform, BLAS_threads, TZ, LC, bundle_id} + laterais {trained_at, package_version}, carimbado em
##     todo artefato e saída. as_of = forecast-origin ÚNICO da rodada; NÃO um corte de treino run-level único
##     (seria o anti-padrão de leak do ADR-0006 — o corte de treino é por-modelo, no fingerprint de cada membro).
##   - freeze: saveRDS BINÁRIO (IEEE-754), não JSON.
##
## Gate falsificável (M1): round-trip binário saveRDS Δ=0; dois freezes de inputs idênticos → mesmo id.

prevcarga_version_hash <- function(fingerprint) {
  stop("provenance.R: prevcarga_version_hash() não implementado — TODO M1")
}

prevcarga_run_manifest <- function(...) {
  stop("provenance.R: prevcarga_run_manifest() não implementado — TODO M1")
}

prevcarga_freeze <- function(obj, path) {
  stop("provenance.R: prevcarga_freeze() não implementado — TODO M1")
}

prevcarga_thaw <- function(path) {
  stop("provenance.R: prevcarga_thaw() não implementado — TODO M1")
}
