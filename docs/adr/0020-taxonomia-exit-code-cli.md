# ADR-0020 — Taxonomia de exit-code da CLI por classe de erro

- **Status:** Aceito
- **Data:** 2026-10-11
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** changed-in-rebuild  <!-- contrato novo do clean-room; a referência não tem taxonomia estável -->
- **Relacionados:** [ADR-0005](0005-contrato-entrada-fail-por-origem.md), [ADR-0008](0008-honestidade-determinismo-falsificavel.md), [CLI-MODULAR-OBJETIVO.md §4.2](../_archive/roadmap/CLI-MODULAR-OBJETIVO.md)

## Contexto
A CLI `prevcarga` serve de **gate de CI e de automação do ONS**, não só de uso interativo: `validate-deck`,
`doctor`, `score`, `check-coherence` precisam de exit-code **estável por classe de erro** para encadear
(`forecast` depois de `validate-deck` como gate-0) e para scripts externos ramificarem. `CLI-MODULAR-OBJETIVO.md
§4.2` já esboçou a ideia (`0/2/3/4/5/6`), mas **nenhum ADR a congelou** — e o M1 descobriu que a classe
`runtime` (abort do `runtime-guard`) não tinha inteiro atribuído (referência circular M1.1↔M1.4). Como M2–M8
herdam esse contrato, renumerar depois quebraria todo consumidor de CI.

## Decisão
A CLI mapeia **classe de erro → exit-code fixo**, por um handler de topo que lê o **prefixo do erro**
(`[prevcarga/<classe>]`) e traduz ao código:

| code | classe | prefixo | quem emite |
|---|---|---|---|
| `0` | ok | — | qualquer comando bem-sucedido |
| `2` | contrato/formato | `[prevcarga/contract]` (schema/tipo/tz; config `run.yaml`/`topology.yaml` inválido; registry wire-time) | `validate-deck` (M2), `model_spec`/`topology`/`registry` (M1) |
| `3` | completude | `[prevcarga/completude]` (datas/slots/DST) | `validate-deck` (M2) |
| `4` | escopo-área | `[prevcarga/topology]` | `validate-deck`/`train-models` |
| `5` | déficit-de-janela | `[prevcarga/window]` | `validate-deck`/`train-models` (M2/M5) |
| `6` | bundle | `[prevcarga/bundle]` | install/bootstrap (M8) |
| `7` | **runtime** | `[prevcarga/runtime]` | `runtime-guard` (M1): TZ≠UTC / thread>1 |

`1` fica reservado para erro genérico não-classificado (traceback R não-mapeado). O número de classes é
**imutável** uma vez aqui: adicionar classe nova = novo código no fim, **nunca renumerar** (quebra CI).

## Consequências
Torna `validate-deck`/`doctor` utilizáveis como gate-0 encadeável (`--skip-validate` desarma). O M1 atribui
`7=runtime` e fecha a referência circular; M2 passa a emitir 2/3/4/5; M8, 6. Todo comando ganha `--format
{text,json}` (ortogonal: o exit-code é o mesmo nas duas saídas). Um teste por classe asserta
`exit==<code>` para o prefixo correspondente.

## Alternativas consideradas
Exit-code binário (0/1) — insuficiente p/ um gate de CI ramificar por classe; códigos derivados de `errno`/
sinais — colidem com semântica de shell; mapear por string no chamador em vez de código — frágil, não-estável.

## Evidência
- `docs/_archive/roadmap/CLI-MODULAR-OBJETIVO.md` §4.2 (esboço `0/2/3/4/5/6`, sem `runtime`).
- `docs/_archive/roadmap/M1-FUNDACAO.md` M1.4 (handler prefixo→código) e M1.1 (classe `runtime`).
- Referência (`/opt/ons-source/prevcargadessem3`): a CLI Python `scripts/prevcarga` não tem taxonomia
  estável (argparse + `subprocess check=True` propaga `1`), prefixo `[prevcarga/contract]` em `_validate_contract:152`.
