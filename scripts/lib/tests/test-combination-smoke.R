## test-combination-smoke.R -- M4.V: smoke NAO-gating sobre um root real de runs de membro. Ativado pela OPCAO R
## `prevcarga.m4_smoke_root` (definida pelo desenvolvedor na chamada; nunca variavel de ambiente). Somente leitura: o checksum
## recursivo do root e identico antes e depois e nada e escrito em /opt/ons-source/prevcargadessem3 nem em data/.
## Afirma so PROPRIEDADE (simplex, n_fitted_converged > 0); a qualidade sai por message(), nunca reprova (ADR-0025).
options(prevcarga.lib = file.path(pc_root(), "scripts", "lib"))
root <- getOption("prevcarga.m4_smoke_root")

test_that("M4.V - smoke nao-gating: root real, somente leitura, so propriedade", {
  if (is.null(root))
    skip("[TBD] smoke nao-gating: runs de membro com proveniencia indisponiveis localmente")
  env <- pc_source_lib(c("runtime", "errors", "config", "schema_long", "topology", "provenance", "deck_reader",
                         "evaluation", "features", "registry", "combination"))
  env$prevcarga_runtime_enforce()
  md5 <- function(r) tools::md5sum(list.files(r, recursive = TRUE, full.names = TRUE))
  before <- md5(root)
  ctx <- getOption("prevcarga.m4_smoke_ctx")
  if (is.null(ctx)) skip("[TBD] smoke nao-gating: defina options(prevcarga.m4_smoke_ctx = <ctx com data_dir e tz>)")
  panel <- env$prevcarga_combina_load(root, getOption("prevcarga.m4_smoke_area", "SECO"), ctx)
  g <- env$prevcarga_combina_gate(panel, ctx)
  expect_true(g$property$simplex_ok)
  expect_gt(g$property$n_fitted_converged, 0L)
  message("smoke combina: max_grad_norm=", format(g$property$max_grad_norm, digits = 3),
          " pass=", g$pass, " (qualidade so reportada)")
  print(g$quality)
  expect_identical(md5(root), before)
})
