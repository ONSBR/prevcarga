# Estado: runtime (M1.1; ADR-0022, ADR-0037)

- `scripts/lib/runtime.R` roda primeiro em todo comando. `prevcarga_runtime_enforce(setters)` registra `found`
  (ambiente cru, so diagnostico), FIXA in-process TZ=UTC, locale (Linux `C.UTF-8`; LC_COLLATE/LC_NUMERIC=C),
  os 4 pins de threads = 1 e BLAS 1-thread; so aborta (exit 7) com `runtime.tz_unfixable`,
  `runtime.locale_unavailable` ou `runtime.blas_unfixable` se NAO conseguir fixar. Shell sujo nunca aborta.
- BLAS: com `RhpcBLASctl` instalado fixa 1 thread; sem ele so o BLAS de referencia do R e aceito (senao aborta).
  `RhpcBLASctl` deve entrar no renv lock/bundle (o BLAS do sistema aqui e OpenBLAS pthread).
- `prevcarga_runtime_check()` (nao muda nada) e `prevcarga_runtime_apply(strict = FALSE)` (nunca aborta) servem
  doctor/status; `prevcarga_runtime_stamp()` entrega tz/lc/blas_threads ao provenance (erro interno antes de enforce/apply).
- `prevcarga_install_root(args)` resolve a raiz via `--file=` (aceita barra invertida), nunca env.
- Unico `Sys.getenv` do pacote: `.prevcarga_runtime_found()`. Requer `errors.R` carregado antes.
- Testes: `scripts/lib/tests/test-runtime.R` + helper `helper-prevcarga.R` (pc_root, pc_source_lib, pc_with_env,
  pc_rscript, pc_run_cli, pc_probe_script). Pacote adicionado no R 4.5.3: RhpcBLASctl.
- `scripts/tools/export_error_catalog.R` ainda guarda `prevcarga_runtime_fix()` com `exists()`; este ticket nao o define.
