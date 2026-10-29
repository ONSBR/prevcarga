# Estado: M4 (combina convexa com leak corrigido) - fechamento do epico (M4.V)

Documento de estado do epico M4, indexado por `docs/ESTADO-ATUAL.md` (so o M1.V edita o indice e `scripts/lib/README.md`). O detalhe por
sub-ticket (M4.1..M4.4) mora em `docs/estado/combination.md`; este arquivo e a visao do epico e o registro dos gates de verificacao do M4.V.

## O que o M4 entrega (VIVO, fora da lista de stubs)
- **Combina R in-process** em `scripts/lib/combination.R` -- VIVO (dono M4; consumidores M6/M7/M8). Nucleo numerico (simplex de Duchi, FISTA no
  sistema normal centrado, parada unica por gradient mapping do objetivo BRUTO), IO/descoberta de membros por identidade, painel por corte,
  join fresco da verdade, recusa de membro desalinhado, combinacao por lead, freeze/apply e `prevcarga_combina_gate`. Verbo `combine` em
  `scripts/lib/cli/combine.R` (modos train, apply, full; exits 0/2/4/8/9/13). Catalogo: `scripts/lib/errors/combination.tsv` e `errors/combine.tsv`.
- **Leak #1 corrigido**: o peso de um mes-de-ORIGEM m so treina com alvos cujo `interval_start` e anterior a inicio(m) no fuso da carga
  (`prevcarga_combina_train_mask`, UMA funcao; eixo de agrupamento = mes-de-origem). O corte do PESO NAO e `origem-1-horizonte`: esse e o corte
  dos MEMBROS (M2/M5). A saida diverge do legado de proposito; o A/B da combina e por propriedade/qualidade, sem paridade de solver (ADR-0014).
- **G2 via honesty_gate** (M3.2, sem mock): `prevcarga_honesty_gate` com `cut = inicio(m)` e `run_fn` = `combina_full` filtrado a
  `forecast_origin == origin` na 1a origem de um mes FITADO. O filtro por origem e necessario porque os pesos de meses posteriores veem
  legitimamente a verdade [TBD: ADR-0029].
- **Recusa de membro desalinhado** (`contract.member_misaligned`, exit 2): inicio e grade comuns; o FIM e por membro (`delivery_days`).
- **Combinacao por lead**: so membros que cobrem o lead entram (k variavel); a faixa vem do `delivery_days` do run-manifest, nunca do nome do modelo.
- **Pesos como artefato oficial**: content-addressed em `<state>/artifacts/combina/<area>/v_<16hex>/weights.rds` + `latest.txt`, o layout do
  prefixo `artifacts/combina/` do bucket; o artefato oficial e publicado pelo M8 (esta combina so escreve local).
- **Determinismo**: medido por `max|delta y_pred| < 1e-12` (in-process) e `identical(y_pred)` entre processos; nunca por `delta w`
  (pesos de problema mal-condicionado nao sao unicos).

## Re-baseline e achados
- Os numeros de backtest da combina VAO mudar (leak corrigido) e re-baselinam no M7; nao ha delta=0 contra o legado.
- **Escala do `grad_tol`**: o criterio e ABSOLUTO; em carga real (~1000 MW) bate o cap de iteracoes. Normalizar por `||b||` evita a falsa parada
  [TBD: ADR-0026]. Os fixtures do M4 ficam em escala O(1).
- Qualidade (WMAPE <= melhor membro e <= EQW) so e ASSERTADA nos fixtures desenhados, sem epsilon e sem parametro `eps` (ADR-0025); no
  holdout real e so reportada (M7/M8).

## Verificacao (M4.V)
- `tests/test-combination-gates.R`: G2 (honesto + dentes: mascara leaky => VAZA, apply como run_fn => HARNESS QUEBRADO, full sem filtro =>
  VAZA, origem em mes EQW => HARNESS QUEBRADO), otimalidade bruta por caminho independente (`pc_cmb_gate_grad`), determinismo in-process
  e dente, sentinela de alinhamento, qualidade nos fixtures desenhados. Helper proprio: `helper-combination-gates.R`.
- `tests/test-combination-invariants.R`: `freeze_parity`, `member_discovery`, `ytrue_join`, `panel_append`, `refit_per_cut`,
  `member_alignment`, `lead_coverage`.
- `tests/test-combine-cli.R` (por processo, `skip_if_not_installed('docopt')`): matriz de exits x {text, json}, JSON unico, ASCII, shell sujo
  TZ/LC_ALL/OPENBLAS == limpo, `members_by_lead`. `tests/test-combination-greps.R`: grep gates de fonte com isca, consumo por AST e `m4_docs_live`.
- `tests/test-combination-smoke.R` (nao-gating): ativado por `options(prevcarga.m4_smoke_root = <root>)`; sem a opcao faz `skip`.
- Sub-gate: `scripts/tools/gates/m4.R` (descoberto por barrel).

## Pendencias (TBD)
- ADR-0025 (qualidade so reportada), ADR-0026 (escala do `grad_tol`), ADR-0029 (escopo do molde do honesty-gate), ADR-0030 (staging do artefato),
  `model_versions` do run-manifest (lista nomeada vazia provisoria) e o layout de run dos membros (provisorio, ADR novo no M6).
- `docs/roadmap/` foi arquivado em `docs/_archive/roadmap/` (somente leitura): os docs tecnicos do roadmap nao foram editados.
