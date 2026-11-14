# Estado: config (M1.4a; ADR-0037, ADR-0024)

- `scripts/lib/config.R` e o unico canal de configuracao: precedencia flag > `--config`/`<root>/config.yaml` > default. Nenhuma leitura de env.
- `prevcarga_config_load(path)` valida o schema v1 (`.prevcarga_config_schema`): chave desconhecida => `config.unknown_key`
  (`where` = chave, `backtest.<x>` no aninhado); tipo errado, `schema_version != 1`/ausente, `corte_default` fora de HH:MM,
  arquivo ilegivel => `config.bad_type`; enum invalido => `config.bad_<load_label|weather_cutoff|color|format>`. Tudo exit 8.
- `prevcarga_ctx(args, root, verb)` monta o ctx (campos em `config.R`); `--origin` aceita `YYYY-MM-DD` estrito
  (via `lubridate::ymd`) e `A..B` so em `backtest`/`plan`; `state_dir` default `<root>/.state` e so resolvido (nunca criado).
  `load_label_flag`/`load_label_config` ficam separados e `--load-label`/`--weather-cutoff` so sao validados no M1 (aplicar e do M2).
- `prevcarga_require_data_dir(ctx)` => `config.data_dir_missing` (exportada; usada pelos verbos do M2+); a dica cita `--data-train`/`--data-inference`.
- **Dados por papel (E1-T2):** `prevcarga_ctx` traz `data_train_dir` (`--data-train` | `--data` | `data_train_dir` | `data_dir`),
  `data_inference_dir` (`--data-inference` | `data_inference_dir`) e `artifacts_dir` (`--data-artifacts` | `--artifacts-dir` | `artifacts_dir` | NULL).
  `ctx$data_dir` = deck ATIVO = `data_train_dir` ate o E1-T3 escolher por papel. Apelidos (`--data`, `--artifacts-dir`, `data_dir`) emitem
  `warning.option_alias {alias, novo}` (`.prevcarga_pick_alias`); apelido x nome novo diferentes => `config.usage` (exit 8); no config.yaml
  `data_dir` x `data_train_dir` diferentes => vence `data_train_dir` com aviso. Testes: `test-data-options.R`, `test-config.R`.
- `config/config.yaml` e o modelo versionado (o instalador do M8 copia para `<root>/config.yaml`); `config/VERSION` = `0.0.0-dev`;
  `/config.yaml` e `/.state/` no `.gitignore`.
- Testes: `scripts/lib/tests/test-config.R`.

## set-data-dir (2026-11-10)
- `prevcarga_config_set_data_dir(ctx, dir)` (config.R) e o unico escritor do `config.yaml` dentro do pacote; o verbo
  `set-data-dir <dir>` (`scripts/lib/cli/set-data-dir.R`) so chama e formata. Alvo: `ctx$config_path` (`--config`, o da raiz do
  codigo ou o da instalacao); sem nenhum, `<R>/config.yaml` da instalacao, criado com `schema_version: 1`. Fora do layout instalado
  e sem config.yaml: `config.usage` (hint: `--config`).
- O diretorio tem de existir (`config.data_dir_missing`); o valor vai por `normalizePath(winslash = "/")` e entre aspas simples
  com `''` (a forma do `pc_yaml_q`/`ConvertTo-PcYamlQuoted` do instalador, cujo leitor o preserva no upgrade). As outras linhas do
  arquivo ficam como estavam (so as `data_dir:` saem). Escrita atomica: temporario no mesmo diretorio, fechado e validado por
  `prevcarga_config_load` antes do `file.rename`; falha de escrita ou rename = `config.write_failed` (core.tsv, exit 8).
- Testes: `scripts/lib/tests/test-set-data-dir.R` (criacao, troca com preservacao, aspas, `--config`, recusas sem escrita).

## F3.6 -- artifacts_dir (raiz de artefatos apontada)
- Chave `artifacts_dir` (chr, default NULL) no schema; `prevcarga_ctx` resolve `ctx$artifacts_dir` por flag `--artifacts-dir` > config.yaml > NULL
  (sem env var), normalizado. Presente e inexistente => `config.artifacts_dir_missing` (core.tsv, exit 8, `where` = caminho), antes de qualquer infer.
- Testes: `test-config.R` (chave nova, template) e `test-forecast_artifacts_dir.R` (precedencia e diretorio inexistente).
