# M5 -- Treino, inferencia, FE-fachada e registry: estado do epico (fechamento do porte, M5.12)

Estado duravel do epico M5 (write-through do M5.12, ex-P.6 fatia 2/2). `docs/ESTADO-ATUAL.md` e do M1.V (indice + decisoes transversais) e
nao foi editado aqui; `docs/roadmap/PLANO.md` e `scripts/lib/README.md` tambem nao. O fechamento so VERIFICA: achado vira correcao no epico dono.
Parte 1/2 (cobertura, higiene, isolamento): `docs/estado/plugins_porte_fechamento.md` (M5.11).

## Os 8 membros (parity-full, M5.12)

Rodada: `export PATH=/opt/R/4.5.3/bin:$PATH && TZ=UTC LC_ALL=C.UTF-8 Rscript --vanilla models/<mdir>/tests/run_all_gates.R --tier parity-full`
em R 4.5.3, Linux, oraculo `c4af737` (so leitura; `golden.lock` de cada plugin conferido no runner). `max|D|` = maximo de `|y_pred|` do clean-room
menos o golden do oraculo sobre a faixa de entrega (`weather_issue_cutoff: origin_midnight_utc`; DESSEM contra o caminho de INFER do oraculo; dia O
pela `day0_source` da unidade). Tolerancia unica 1e-5 MW (ADR-0017), exceto as divergencias NOMEADAS abaixo.

| sigla | mdir | variante | layout | caminho(s) | faixa de entrega | paridade | sha256 do relatorio |
|---|---|---|---|---|---|---|---|
| RF | random-forest | - | rf_wide_hourly_216 | train, infer | O..O+8 | @RF_STATUS@ | @RF_SHA@ |
| LGBM | lgbm | - | lgbm_day_48 | train, infer | O..O+1 | @LGBM_STATUS@ | @LGBM_SHA@ |
| HW | holt-winters | - | hw_matrix_nx48 | train, infer | O..O+8 | @HW_STATUS@ | @HW_SHA@ |
| LIN_MED | dessem-linear | MED | dessem_semihourly_48 | train, infer, forecast | O..O+8 | @DL_STATUS@ | @DL_SHA@ |
| LIN_MAX | dessem-linear | MAX | dessem_semihourly_48 | train, infer, forecast | O..O+8 | @DL_STATUS_FAN@ | @DL_SHA@ |
| LIN_MXMN | dessem-linear | MXMN | dessem_semihourly_48 | train, infer, forecast | O..O+8 | @DL_STATUS_FAN@ | @DL_SHA@ |
| RGD_MED | dessem-regdin | MED | dessem_semihourly_48 | train, infer, forecast | O..O+8 | @RD_STATUS@ | @RD_SHA@ |
| RGD_MAX | dessem-regdin | MAX | dessem_semihourly_48 | train, infer, forecast | O..O+8 | @RD_STATUS@ | @RD_SHA@ |

Notas da tabela:
- Entrega dos 8 pelas fachadas `identical()` a `prevcarga_delivery_grid` (M5.11): 413 `interval_end` nos 7 de horizonte longo, 77 no LGBM.
- O `foundation_report.json` do dessem-linear (caso-fundacao LIN_MED x SECO, versionado em `models/dessem-linear/tests/parity/`) e o
  `requires_report` de toda unidade do fan-out do dessem-regdin e do proprio dessem-linear; o sha256 e o mesmo em todos (conferido por
  `test-plugins-gates-cli.R`). O motor `_dessem-engine` tem golden isolado (`.state/golden/plugins/_dessem-engine`, MED/MAX/MXMN) e nao e membro.
- `@FOOT@`

## Divergencias aceitas e NOMEADAS (nenhuma escondida; nenhuma entrada em `tests/ab/ab_excecoes.yaml`, que so o Gabriel cria)
1. **Hora incompleta descartada tambem na previsao de clima** (decisao Gabriel 2026-10-30; divergencia deliberada do oraculo, documentada por plugin):
   afeta so as ultimas ~3 h do ultimo dia entregue (O+8) dos DESSEM, em intervalos nomeados por unidade em `docs/estado/dessem_linear.md` (max 122,5 MW na
   fundacao MED x SECO de outubro/2024; fora deles `<= 1e-9`). O CONTROL (emular a media de hora parcial do oraculo) fecha tudo em `<= 1e-9`.
2. **dessem-linear `sp-2024-11-15`** (O = feriado): residuo numerico documentado de 0,446 MW (3e-5 rel; teto 0,5 MW / 5e-5), causado por 1 ulp em `mm`
   amplificado pelo SVM da DM(O). As outras 8 unidades da fundacao ficam exatas.
3. **dessem-regdin: versoes de pacote** (R 4.5.3, fable 0.5.0 x fable 0.3.3 do oraculo): tolerancia do ADR-0036 (max_rel 1,3e-5), medida pelo A/B do M8.8, nao aqui.

## Pendencia `[TBD: ADR-0031]`
Caminho de infer DESSEM e tolerancia da cauda: a regua split x acoplado (`prevcarga_plugin_forecast`) vale hoje dia D `<= 1e-5` e cauda WMAPE `<= 1%`
(`wmape_tail_limit` do `foundation_report.json`). O limiar da cauda continua `[TBD: ADR-0031]` ate o ADR ser aceito; nada aqui o endurece ou afrouxa.

## Posicao do RNG do DESSEM
PRESERVADA (sem plano B ativo): `rng_deviation = NULL` nos dois descriptors; `dl_rng_mode`/`rd_rng_mode` = `preserve`. Treino: `set.seed(seed)`, cabeca de
referencia descartada (so consome o gerador) e 9 x 25 chamadas de `tune_wsvm` (dessem-linear) / 49 ajustes por dia-alvo (dessem-regdin), como o oraculo.
O plano B (`reseed`, pula a cabeca e emite `warning.dessem_linear_rng_deviation`) existe em codigo e nao foi necessario. Desvio a listar para o A/B do M8:
nenhum de RNG; so os itens de "Divergencias aceitas" acima.

## Modulos do M5 vivos
- **Fachadas** em `scripts/lib/`: `features.R` (`prevcarga_build_features`, fachada de FE; acrescimos do M5: estagios e `requires`), `training.R`
  (`prevcarga_train`: treino pela classe de split, ADR-0023), `inference.R` (`prevcarga_infer_cell`, `prevcarga_infer_read_panel` = unico construtor do painel
  de infer; corte intradiario). `topology.R` ganhou os membros de topologia (`prevcarga_members`, `mdir`, `exogena`).
- **Registry** (`registry.R`, no layout `artifacts/` do bucket publico): `<state_dir>/artifacts/<mdir>/[<exogena>/]<area>/{latest.txt, v_<hash>/...}`;
  nucleo (M5.2), ciclo de vida (M5.7: pin, gc, promote). Estado em `docs/estado/registry_core.md` e `registry_lifecycle.md`.
- **Verbos auto-registrados** (barrel por descoberta): `scripts/lib/cli/train-models.R`, `scripts/lib/cli/infer-model.R`, `scripts/lib/cli/registry.R`.
- **Catalogos de erro** (um TSV por modulo, ADR-0040 7-8): `scripts/lib/errors/features_facade.tsv`, `registry_core.tsv`, `training.tsv`, `train_models.tsv`, `inference.tsv`, `infer_cut.tsv`, `registry_lifecycle.tsv`.
- **Pack de gates** `scripts/tools/gates/m5_facades.R` (M5.8: `m5_determinism`, `m5_split_honesty`, `m5_intraday`, `m5_weather_reach`, `m5_day0_infer`,
  `m5_transparency`; substitui os slots `[DEFERRED M5]` dos 5 runners) e `scripts/tools/gates/m5.R` (M5.9: pureza e contrato de saida). Estado:
  `docs/estado/m5_facades_gates.md`, `docs/estado/m5-purity.md`. O pack `scripts/tools/gates/m6_long.R` (M6.6) substituiu os `[DEFERRED M6]`:
  sobram so `[DEFERRED M7]` (harness) e `[DEFERRED M8]` (cross-OS), SKIP com dono.
- Estado por modulo: `docs/estado/{features_facade,training,inference,plugin_dispatch,plugins_porte_fechamento}.md`; por plugin:
  `docs/estado/{random_forest,lgbm,holt_winters,dessem_linear,dessem_regdin,dessem_engine}.md`.

## Vereditos do spike ADR-0019 (M5.10; copiados de `docs/journal/spike-adr0019.md`, efemero e gitignorado, com a evidencia)
Medicao, nao correcao (2026-10-29): R 4.5.3 (clean-room) x R 4.2.3 (oraculo, processo filho `env -i ... TZ=UTC`, deck COPIADO); deck sentinela de
`scripts/tools/fixture_store.R` (`sentinel = TRUE`), tz Etc/GMT+3; rodado com o rotulo da carga gravado `end` (caso real do ADR-0035: o leitor desloca -30 min) e `start`.
- ADR-0019 s3 LGBM: confirma. Caminho: leitor unico -> despachante (offset 0) -> ingestao -> `prevcarga_build_features(stage = "train")`, `disable_loess: true`.
  Clean-room (end e start): temperatura - carga = 0 em todas as linhas, `offset_min` = 0 em todas (2736 linhas / 2561 de features). Oraculo: com store `start`
  alinhado (0), com store `end` +30 min em todas (2831 linhas), porque o oraculo nao le o rotulo. Delta=0 preservado no store `start` (o do oraculo e da producao);
  deck sha256 (SP, 60 dias, O=2026-10-01): end d7cbfc51...28b7, start a5c41fe7...479e4.
- ADR-0019 s4 RF: confirma. Ingestao RF (`to_hourly` por `interval_start`): 0 de 2400 intervalos com dia diferente do de `interval_start` (100 nas bordas 23:30/00:00), media
  diaria RF - ref = 0 em 50 dias completos, para end e start. Oraculo com store `end`: 60 intervalos (1 por dia, 23:30-00:00) caem em D+1, hora -30 em 1440 horas; com `start`: 0.
  deck sha256 (ZZ1, 80 dias): end b7ec17e9...159f9, start 94498 0ac...0c (ver journal ate o /consolidate).
- Nao-interferencia: sha256 de `models/` antes == depois (52d0fd42...109b); oraculo com as mesmas 4 entradas `??` pre-existentes.
- Achados laterais (sem dono ainda): `prevcarga_assert_stage_requires` trata `requires = "frame"` do LGBM como coluna do frame (o spike usou `frame.raw`);
  plugins acham a fundacao via `.GlobalEnv` fora da CLI. Nenhum vies confirmado => nao ha correcao pos-M5 a nomear para s3/s4.

## Gates do fechamento (M5.12)
`scripts/lib/tests/test-plugins-gates-cli.R`: (1) 5 runners x `fast`/`parity` como processos (10 saem 0; zero `[DEFERRED M5]`/`[DEFERRED M6]`; pack `[M5]` e `[M6]`
descobertos e verdes); (2) TZ sujo sozinho sai 0 e setter de TZ quebrado sai 7 com stdout vazio; (3) aditividade (manifesto sha256 de `models/` + carimbo) e oraculo
intacto, com CONTROLs; (4) parity-full (`options(prevcarga.closure_tier = "parity-full")`, so Linux) com sha256 dos relatorios e CONTROL de `golden.lock` adulterado;
(5) write-through (READMEs dos 5 plugins, este arquivo) com CONTROLs de copia incompleta.
