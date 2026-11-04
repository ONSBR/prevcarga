# Estado: M1 Fundacao (fechamento do epico; M1.V)

M1 esta implementado no clean-room: fachada, runtime, config, contrato de erros, topologia, proveniencia, registry Tier A,
schema long, introspeccao e spike R 4.5.3. Este arquivo e o fechamento do epico; o detalhe de cada modulo mora em
`docs/estado/<modulo>.md` (runtime, errors, config, banner, cli, introspect, topology, provenance, schema_long, m1-spike-r453).

## Contrato (o que os epicos seguintes herdam)
- **Exit-codes 0-15 e catalogo distribuido (ADR-0040).** `prevcarga_exit_codes` (errors.R) e a tabela unica
  (ok 0, generic 1, contract 2, completude 3, topology 4, window 5, bundle 6, runtime 7, config 8, integrity 9, network 10,
  coherence 11, repro 12, state 13, partial 14, honesty 15); precedencia runtime > config > contract > completude > topology >
  window > demais por menor codigo. Os `error.id` vivem em `scripts/lib/errors/<modulo>.tsv` (um por modulo; `core.tsv` e o do M1),
  lidos por `prevcarga_errors_load()` (radix, id unico, classe da tabela, ASCII). `install/error-catalog.tsv` e GERADO por
  `scripts/tools/export_error_catalog.R` (nao versionado, `.gitignore`); o gate de processo compara o gerado com a uniao dos TSV.
- **ctx e config (ADR-0037).** `prevcarga_ctx()` monta flag > `config.yaml` (`--config` ou `<raiz>/config.yaml`) > default;
  o pacote NAO le variavel de ambiente. Unico `Sys.getenv`/`Sys.setenv`: `runtime.R`. Unico relogio: `run_id` em `config.R`.
- **Runtime fixado in-process (ADR-0022).** `runtime.R` roda primeiro; sobrescreve TZ/locale/pins/BLAS do shell sujo e so
  aborta (exit 7) se nao conseguir fixar. Isencao fechada: `doctor` (imprime tudo, depois sai 7 se FAIL) e `status` (sai 0).
- **Topologia.** `config/topology.yaml` e a fonte unica (areas, perdas, membros, `khat_range` default `[1.00, 1.15]` com
  override por subsistema); nenhum literal de topologia ou da faixa do k-hat em codigo (grep gate).
- **`prevcarga_schema_long`** (schema_long.R): 8 colunas, `forecast_origin` texto `yyyy-MM-dd`; definido so la.
- **run.yaml novo:** `name, split, granularity_min, horizon_days, history_days, seed, modes, backtest.protocol_default,
  constraints, params`; chaves de orquestracao (`resources`, `vcpu`, `mem_gb`, `r_deps`, `entrypoint`, `train_args`,
  `cut_invariant`) sao proibidas.
- **Verbos auto-registrados.** Cada verbo e `scripts/lib/cli/<verbo>.R` (objeto `prevcarga_verb_<verbo>`), descoberto por
  `prevcarga_verbs_discover()`; o shim `scripts/prevcarga` so despacha. Verbos planejados sem arquivo viram stub
  `todo.not_implemented` nomeando o dono (`prevcarga_verbs_planned`). Hoje com arquivo: doctor, status, models, areas, describe.
- **Sub-gates por descoberta:** `scripts/tools/gates/<dono>.R`; o do M1 e `scripts/tools/gates/m1.R` (roda os tres arquivos abaixo).

## Gates do M1.V (processo e fonte)
- `scripts/lib/tests/test-cli-process.R`: `Rscript --vanilla <copia>/scripts/prevcarga` sobre instalacao-copia em `tempdir()`
  (`helper-install-copy.R`: `pc_install_copy`, `pc_cli`). Cobre sujo x limpo (FORBIDDEN delta=0 + CONTROL sem CLI hora 21),
  setter de TZ quebrado (models 7 com stdout vazio; doctor imprime tudo e sai 7; status 0), config/flags invalidos (exit 8),
  verbo/flag fora da gramatica, stubs derivados da descoberta, matriz de classes 1-15 por verbos-fixture
  (`fixtures/cli_fixture_verbs.R`, um arquivo `cli/fixture-<x>.R` por fixture) + 0/1/2/7/8 pela CLI real, 6 conjuntos de
  precedencia (normal e invertida), `--debug`, sincronia do catalogo, copia quebrada => exit 2 com CONTROL intacta.
- `scripts/lib/tests/test-ascii-bytes.R`: nenhum byte > 127 em --help/--version/sem verbo/verbos/stubs/ids do catalogo/TSV;
  banner (blueprint, `docs/estado/banner.md`) nunca com stdout redirecionado, em `models` nem em JSON; sob pty (CONTROL) nos 4 pontos,
  com cor SGR so em `always`/`auto`; CONTROL `fixture-accent` reprova.
- `scripts/lib/tests/test-static-greps.R`: 14 grep gates sobre o codigo de produto (comentarios removidos, exceto no gate de
  bytes), cada um com CONTROL de isca. Escopos: chamadas de codigo (env, datas, relogio, topologia, k-hat, dessem-switch)
  so em `.R`; `PREVCARGA_*`, unix/rede em `.R`+tsv+yaml. Permitidos: `Sys.setenv/getenv` em `runtime.R`; `now(` em `config.R`;
  `1.00/1.15` em `topology.yaml`; `prevcarga_schema_long <-` em `schema_long.R`.
- Divida registrada como discovery (gate a deixa passar de forma explicita, nunca silenciosa): `as.POSIXct(NA)` em
  `combination.R` (gate de datas base); constantes internas `.PREVCARGA_*` em `deck_reader.R` (colidem com o nome de variavel
  de ambiente; o gate exige que `PREVCARGA_*` nao esteja precedido de `.`/letra).
- Fixture do setter quebrado: `runtime_setter_tz_broken` entra so na COPIA, anexando uma linha a `runtime.R` da copia
  (a guarda roda antes de qualquer modulo de verbo, entao nao da para injeta-lo por `modules`).

## Spike R 4.5.3 (M1.7, ADR-0036)
Veredito e tabela completa em `docs/estado/m1-spike-r453.md`: RF, LGBM e HW com delta 0; dessem-linear max_rel 2.2e-13
(pendente do Gabriel); dessem-regdin max_rel 1.3e-5 (excecao ao delta=0, ADR-0036 confirmada com renv.lock novo);
braco estrito (renv.lock antigo em 4.5.3) nao compila.

## Rodadas
- **Linux (R 4.5.3, `export PATH=/opt/R/4.5.3/bin:$PATH`), 2026-10-29:** `testthat::test_dir("scripts/lib/tests")` verde:
  FAIL 0, PASS 4097, SKIP 3 (os 3 skips sao `[TBD: ADR-...]` de dates/validation, de outros tickets; nenhum CONTROL skipado).
- **Windows nativo (R 4.5.3): NAO EXECUTADA.** Nao ha maquina Windows disponivel (pendencia ja registrada para o M8, ADR-0038).
  Nenhum braco foi mascarado como passe: os gates sao portaveis (sem bash, sem env de shell, `system2`+`shQuote`,
  `pc_with_env`), mas o resultado no Windows fica em aberto ate haver a maquina; o AC 16 do M1.V so fecha integralmente depois.
