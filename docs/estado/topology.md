# Estado: topologia e contrato por-modelo (M1.2; ADR-0001/0009/0021)

- `config/topology.yaml` e a fonte unica: schema_version 1, `khat_range` default `[1.00, 1.15]` (unico lugar da faixa;
  override opcional por subsistema), 4 subsistemas/24 areas/perdas, `order`, 8 `members` (sigla, mdir, exogena).
  Area nova = editar o YAML. `"N"` precisa de aspas (YAML 1.1 le `N` como FALSE; vale para chave e para `order`).
- `scripts/lib/topology.R` so carrega e valida: `prevcarga_topology(path, root)` (khat_range efetivo por subsistema),
  `prevcarga_members(path, root)` (exige `models/<mdir>/run.yaml`), `prevcarga_model_spec(model, root)`.
  Caminho so por argumento; qualquer violacao e `prevcarga_abort` classe contract, sem objeto parcial.
- `models/<m>/run.yaml` (5): name, split, granularity_min, horizon_days, history_days{train,infer}, seed, modes,
  backtest.protocol_default, constraints (tabela `.prevcarga_constraints`), params. Sem `protocol` como atributo,
  sem `cut_invariant` nem chaves de orquestracao (recusadas com `contract.run_yaml_forbidden_key`).
- Ids de erro do modulo: core.tsv (`contract.topology_*`, `run_yaml_*`, `khat_range_invalid`) + `errors/topology.tsv`
  (`contract.topology_bad_value`, `topology.area_undeclared`). `topology.area_undeclared` (exit 4) e o UNICO id de "area
  fora da topologia": deck_reader, combination, validation (`what`) e score/evaluation (o antigo `area_unknown` foi fundido).
- Testes: `scripts/lib/tests/test-topology.R` + fixtures `topology_*.yaml`. Desvios de run.yaml/members sao gerados em tempdir.
- [TBD ADR-0028] valor final da faixa k-hat; mudar so o YAML.
