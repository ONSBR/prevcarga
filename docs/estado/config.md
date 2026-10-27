# Estado: config (M1.4a; ADR-0037, ADR-0024)

- `scripts/lib/config.R` e o unico canal de configuracao: precedencia flag > `--config`/`<root>/config.yaml` > default. Nenhuma leitura de env.
- `prevcarga_config_load(path)` valida o schema v1 (`.prevcarga_config_schema`): chave desconhecida => `config.unknown_key`
  (`where` = chave, `backtest.<x>` no aninhado); tipo errado, `schema_version != 1`/ausente, `corte_default` fora de HH:MM,
  arquivo ilegivel => `config.bad_type`; enum invalido => `config.bad_<load_label|weather_cutoff|color|format>`. Tudo exit 8.
- `prevcarga_ctx(args, root, verb)` monta o ctx (campos em `config.R`); `--origin` aceita `YYYY-MM-DD` estrito
  (via `lubridate::ymd`) e `A..B` so em `backtest`/`plan`; `state_dir` default `<root>/.state` e so resolvido (nunca criado).
  `load_label_flag`/`load_label_config` ficam separados e `--load-label`/`--weather-cutoff` so sao validados no M1 (aplicar e do M2).
- `prevcarga_require_data_dir(ctx)` => `config.data_dir_missing` (exportada; usada pelos verbos do M2+).
- `config/config.yaml` e o modelo versionado (o instalador do M8 copia para `<root>/config.yaml`); `config/VERSION` = `0.0.0-dev`;
  `/config.yaml` e `/.state/` no `.gitignore`.
- Testes: `scripts/lib/tests/test-config.R`.
