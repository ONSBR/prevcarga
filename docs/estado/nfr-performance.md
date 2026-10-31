# Estado: NFR de 30 min da rodada `forecast` (M8.10; AC 22 do epico M8; ADR-0023, ADR-0038)

## O que e
Gate de performance: `prevcarga forecast --origin <O> --format json` pelo bundle INSTALADO deve registrar `duracao_s` <= 1800 nos dois sistemas
(host Linux de referencia e VM Windows). Medicao pela duracao REGISTRADA pela rodada, nunca por relogio do teste.

## Peca de produto (aditiva)
- `scripts/lib/forecast.R` `.forecast_stopwatch`: cronometro por passo com relogio INJETADO (`ctx$step_clock`, segundos; forecast.R nao le relogio:
  o grep `clock` do M6 segue em zero hits). A fachada (`scripts/lib/cli/forecast.R`) injeta `proc.time()[["elapsed"]]`.
- O `result` do envelope ganha `duracao_s`, `passos[]` (`validate-deck`, `infer`, `combina`, `reconcile`, `export`, `report` so com `--report`), `jobs`
  (1: o `forecast` nao tem --jobs; M8.9), `platform`, `runtime_id`. Sem relogio injetado (testes de biblioteca) nao ha campos de tempo.
- O re-treino dos origin_scoped acontece dentro do `infer` (ADR-0023) e por isso entra no orcamento. `score` nao e executado nem cronometrado.
- Desvio do texto do ticket: o tempo NAO esta no `run_manifest.json`. A entrega e byte-deterministica (chave imutavel; shell sujo x limpo byte a byte),
  entao relogio de parede nao pode entrar nela. O ticket assumia campos que a M6.4 nunca emitiu; o contrato de tempo vive so no `result`.

## Testes
- `tests/ab/helper-nfr.R`: `pc_nfr_budget`, `pc_nfr_verdict`, `pc_nfr_injected_round`.
- `tests/ab/test-nfr-30min.R`: mecanica do gate; CONTROL (3 s vs orcamento de 2 s reprova apontando `infer`; 1800 s passa); rodada real pelo bundle
  instalado (SKIP `deck-real-ausente` sem deck real; protocolo reexecutavel no cabecalho do arquivo); mecanica no deck sintetico pelo bundle (SKIP sem
  `dist/`); recibo `nfr-30min` (`pc_receipt_write`) so em PASS real.
- `scripts/lib/tests/test-forecast_timing.R`: o `result` real da rodada com relogio de teste, soma dos passos == total, entrega byte-identica com e sem relogio,
  sem tempo no run_manifest.json, CONTROL de espera injetada no `infer`.

## Pendencias
- [PENDENTE, dono: Gabriel/operador] O numero REAL (<= 1800 s) exige o deck real com `manifest.json` (o `data/` local nao tem), artefatos oficiais
  sincronizados e o tempo do oraculo na mesma maquina. Nenhum numero e inventado: o procedimento esta no cabecalho de `tests/ab/test-nfr-30min.R` e em
  `docs/estado/m6-forecast.md` (bench M6.6).

| plataforma | duracao_s | passo mais lento | jobs | host | data |
|---|---|---|---|---|---|
| linux-x86_64 | (pendente: operador) | | | | |
| windows-x86_64 | [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] | | | | |
