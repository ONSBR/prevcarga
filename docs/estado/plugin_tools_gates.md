# plugin_tools_gates -- runner comum de gates + agregador (P.0h)

Arquivo: `scripts/tools/plugin_gates.R` (ferramenta de desenvolvimento, fora do bundle). Testes: `scripts/lib/tests/test-plugin-tools-gates.R`
(brinquedos `pc_toy_gates_model`, `pc_toy_gate_packs`, `pc_toy_gates_topology` em `helper-plugins.R`).

## Runner (`prevcarga_gates_run(gate_table, tier, model, argv, runtime_enforce, gates_dir, tests_dir, exit)`)
- 1a instrucao: `runtime_enforce()`; falha => exit 7, stdout vazio (erro so em stderr, texto). Depois `--tier` (fast|parity|parity-full; invalido => `tools.bad_tier`, exit 2).
- Roda `testthat::test_dir(<runner>/testthat)` com ListReporter; cada teste e rotulado `[PLUGINS] <gate>:` ou `[DEFERRED Mx] <gate>:` e cruzado com a gate_table (colunas gate, owner, tier|tier_min).
- Status: PASS / FAIL / SKIP (so DEFERRED) / N/A(tier) (tier_min acima do tier, ou skip `[tier]`). Skip de gate aplicavel que nao e DEFERRED conta como FAIL.
- Exit 0 sse nenhum gate bloqueante (dono nao-DEFERRED) falhou; falha => `tools.gate_failed` (exit 1); teste sem rotulo, gate sem teste, gate duplicado ou pack invalido => `tools.gate_unowned` (exit 1).
- `--format json`: envelope ADR-0040 em stdout, `result.gates` = {gate, owner, tier, status, detail}; tudo ASCII.
- Helpers para os testes dos plugins: `pc_gate_deferred(owner, reason)`, `pc_gate_tier(min)`, `pc_gate_require_golden(model, unit, root)` (usa `fail()`; golden = `<root>/<model>/tests/parity/golden.lock`).
- A raiz do repo vem do caminho de `plugin_gates.R` capturado no `source()` (nao de `prevcarga_install_root()`, que mediria a partir do runner do modelo).

## Packs por descoberta (`scripts/tools/gates/<dono>.R`, ADR-0040 item 8)
- `list.files` + `sort(method = "radix")`; cada pack define `prevcarga_gate_pack_<dono>(model, ctx, tier)` -> linhas (gate, owner, tier_min, applies_to, test); `test` e uma funcao executada pelo runner (erro = FAIL, `testthat::skip` = SKIP).
- Filtra por `applies_to` (model_ids ou "all"). Arquivo sem a funcao => `tools.gate_unowned`.
- Scripts autonomos (m1..m4.R: chamam `quit()`) nunca sao carregados in-process: o arquivo e analisado por `parse()`; sem a funcao do pack e com `quit`/`q` no topo => ignorado.

## Agregador
`Rscript --vanilla scripts/tools/plugin_gates.R --models <m[,m2]|all> [--tier T] [--root <models>] [--topology <yaml>]`. `all` = `unique(prevcarga_members()$mdir)`;
um processo `Rscript --vanilla` por runner (`system2`), uma linha por modelo, sai com o maior codigo. Runner ausente => `FAIL tools.gate_failed` com o caminho esperado.
O hint de `tools.bad_tier` no catalogo (plugin.tsv, P.0a) cita "fast, full ou deploy" e esta desatualizado em relacao aos tiers reais (follow-up de P.0a).
