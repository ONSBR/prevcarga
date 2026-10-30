# M5 -- gate de pureza e contrato de saida (M5.9)

Sub-gate `scripts/tools/gates/m5.R` (barrel por descoberta; roda `test-m5-purity.R` e `test-m5-envelope.R`).

## Pureza (`scripts/lib/tests/test-m5-purity.R`)
- Escopo: so a fachada de `features.R` (`prevcarga_build_features`, `prevcarga_assert_stage_requires`), `training.R`, `inference.R`
  inteiros e as funcoes do nucleo/ciclo de vida de `registry.R` (verbos do registry; o loader de plugins e do P.0). O escopo e
  resolvido por `parse()`/srcref; comentarios (inteiros ou de fim de linha) saem pelo parser.
- Regras (`M5_PURITY_RULES`): sem feature (`loess(`, wavelet, haar, m_sql), sem switch por nome, sem literal de janela
  (1825/730/45) nem `horizon_days = N`/`granularity_min == N` (comparacao com `spec$` e aceita), sem `Sys.getenv`, `Sys.time/Date`
  (isenta so a linha de `trained_at`), `as.POSIXct/strptime/as.Date(`, `mclapply`, `system(`, `file.symlink`, `/tmp`, python,
  rede. Cada regra tem isca plantada em copia (tempdir) em 5 funcoes e acusada com `arquivo:linha`.
- Isencao do `%in% c("`: a linha com `"latest.txt"` em `registry.R` (`.prevcarga_reg_walk`) checa arquivos do registry, nao nomes de modelo.
- Pacotes usados (`::`, `requireNamespace`, `library`) devem estar no `Packages` do `renv.lock`. **Estrito desde o M8.1** (o repo tem
  `renv.lock`, R 4.5.3): sem o lock o teste e o gate `m5.R` FALHAM; nao ha mais o conjunto das bibliotecas instaladas como substituto.
  O CONTROL cobre a leitura do JSON, o pacote fora do lock e o lock ausente.
- `granularity_min == 60L` em `training.R:72`/`inference.R:171` e interino do despachante (branch `fix/foundation-w1-f4-lag` o remove).

## Contrato de saida (`scripts/lib/tests/test-m5-envelope.R`)
- Processo real (`Rscript --vanilla scripts/prevcarga <verbo> ... --format json`), instalacao-copia + deck sintetico + plugin-brinquedo.
  Exits cobertos: 0, 2, 4, 8, 9, 13 nos 3 verbos; um unico JSON com as 8 chaves do ADR-0040, `exit_code` = exit do processo = exit do
  modo texto, stderr vazio em json, stdout vazio no erro em texto, bytes <= 127.
- Os 7 TSV do M5 sao ASCII puro; todo id citado por `prevcarga_abort/condition` no codigo do M5 existe no catalogo; nenhum id repetido.
- `--horizon`/`--exogena` agora sao `scoped = c(...)` no registro do `train-models` (mecanismo do M7.1, `.prevcarga_check_scoped` em cli.R): o `infer-model` os recusa com `config.usage` (exit 8). `test-infer-model.R` AC1 usa deck valido + CONTROL (sem a flag passa do guard), logo nao e mais vacuo.
- Correcao minima feita aqui: `scripts/lib/cli/infer-model.R` nao carregava `validation.R` (o verbo falhava por processo com
  `plugin.contract` em `deck_spec`); o `modules` agora o inclui.
