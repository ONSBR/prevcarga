# ADR-0008 — Honestidade + determinismo como invariante de módulo falsificável por MEDIÇÃO (FORBIDDEN Δ=0 / CONTROL Δ≠0) + determinismo container×container (TZ/LC/BLAS 1-thread)

- **Status:** Aceito
- **Data:** 2026-10-10
- **Decisores:** Gabriel (ONS) + Claude
- **Tipo:** carry-over  <!-- carry-over = invariante a preservar · changed-in-rebuild = mudança consciente · open-question = decisão pendente -->
- **Relacionados:** [ADR-0003](0003-tempo-naive-brt-runtime-env.md), [ADR-0006](0006-backtest-honesto-corte-treino.md), [ADR-0007](0007-contrato-corte-intradiario.md), [ADR-0017](0017-empacotamento-bundle-s3-entrega.md)

## Contexto
Um Δ=0 só vale se o harness FOSSE CAPAZ de detectar vazamento — senão uma árvore-sombra não lida daria Δ=0 em todo modelo e declararia todos honestos (falso-negativo silencioso). E o Δ=0 só é reproduzível se o host sai da equação e o ponto-flutuante não é reordenado.

## Decisão
Honestidade é provada empiricamente por 4 braços falsificáveis: **CLEAN** (baseline B), **CLEAN2** (=B → determinismo), **FORBIDDEN** (corrompe só a região > corte → exige y_pred=B E y_true≠B, controle positivo de graça), **CONTROL** (corrompe dia O-1 legal → exige y_pred≠B, sensibilidade), + MORNING opt-in (exige Δ≠0: a manhã DEVE ser usada). Determinismo é invariante de 1ª classe provado container×container (dois `docker run` na imagem pinada) com `TZ=UTC` + `LC=C` + `OMP/OPENBLAS/MKL/VECLIB=1` (o ARIMA-MLE do regdin só é Δ=0 exato mono-thread). Otimização de performance que altere o input só entra se um gate Δ=0 bit-a-bit a provar (flag OFF por default até a prova).

## Consequências
É o núcleo do invariante de módulo falsificável do rebuild. Sem o braço CONTROL/y_true uma árvore-sombra não lida daria falso-verde. BLAS single-thread é a diferença entre o regdin passar ou falhar o Δ=0. É o mesmo rigor aplicado à performance (`--infer-history-days` só porque provado Δ=0 ~20× em lgbm). Tensão com o `verificar` do produto, que usa tolerância (ver [ADR-0017](0017-empacotamento-bundle-s3-entrega.md)).

## Alternativas consideradas
Gate por grep/revisão de código (frágil); Δ=0 de braço único (vulnerável a falso-verde); comparação host×container (confunde drift de pin com não-determinismo); BLAS multi-thread (Δ≠0 por reordenação FP); tolerância ε>0 (esconde não-determinismo).

## Evidência no código de referência (`/opt/ons-source/prevcargadessem3`)
- scripts/tools/honesty_gate.R:11-52
- scripts/tools/honesty_gate.R:460-566
- jobs/tests/determinism_gate.sh:74-116
- scripts/tools/honesty_gate.R:347-350
- scripts/walk_forward.R:199-201

## Limiar no rebuild (2026-10-10, Gabriel — ver [ADR-0017](0017-empacotamento-bundle-s3-entrega.md))
O "Δ=0 bit-a-bit" da referência passa a ser medido por limiar explícito no clean-room:
- **Determinismo (duas rodadas, mesmo hardware)** e **braço FORBIDDEN do honesty-gate:** `|Δ| ≤ 1e-5`.
  (Um vazamento de horizonte real move a previsão **ordens de grandeza** acima de 1e-5, então 1e-5 é piso
  falsificável seguro; o braço CONTROL precisa dar `|Δ| ≫ 1e-5`.)
- **Cross-machine (produto `verificar`):** `|Δ| ≤ 0,1 MW` absoluto (0017), nunca aplicado aos gates internos.
Não é afrouxamento do invariante: 1e-5 mesmo-hardware é quase-estrito e mantém o gate falsificável; a folga
cross-machine mora no `verificar` do produto, separada dos gates.
