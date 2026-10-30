# Golden do oraculo do Reconcile (R.6)

Nada de dado e versionado aqui: so o lock hash-only `golden.lock`, escrito por `scripts/tools/reconcile_golden.R`
(dev-only, Linux). Nao editar o lock a mao: regenere com a receita abaixo.

## Onde mora cada coisa

- Entrada (verdade S = RS + PR + SC e folhas da combina-A de O): sintetica deterministica (sem RNG), funcao pura de
  (origem, dias, semente) em `rg_build_inputs()`. O teste a regera em memoria e confere `inputs.sha256` do lock.
- Saidas do oraculo: `.state/golden/reconcile/golden_profile.parquet` (k por hour x month x dow, rotulo de inicio) e
  `golden_long.parquet` (y_pred/BU_pred por intervalo, `target_datetime` = interval_end, fuso `Etc/GMT+3`).
  `.state/` e gitignored. `test-reconcile-parity.R`: golden ausente = SKIP com a receita; sha256 divergente do lock = FALHA.

## Receita

```
Rscript --vanilla scripts/tools/reconcile_golden.R --origin 2026-02-20 --rscript /opt/R/4.2.3/bin/Rscript \
  --oracle-env R_HOME=/opt/R/4.2.3/lib/R --oracle-env R_LIBS_SITE=/opt/R/4.2.3/lib/R/library \
  --oracle-env R_LIBS_USER=<biblioteca 4.2 do oraculo>
```

O lock registra commit e R do oraculo, comando, origem, janela (730), dias (760), semente (42), digest da entrada e
sha256/linhas das saidas. Janela real do oraculo: `seq(O - W, O - 1)` = `[O-730, O-1]` (730 dias); o comentario
`(O-W, O-1]` do oraculo esta errado, o clean-room segue o codigo.

## Excecoes conscientes do A/B (insumo do write-through do R.V / M8)

- glob: o oraculo usa a media de TODO o historico (inclusive o futuro) como fallback; o clean-room usa a media da JANELA. O golden exige `n_fallback = 0` dos dois lados (o oraculo e abortado se imprimir `fallback glob_ratio`).
- fallback silencioso removido: bucket sem observacao na janela gera aviso `reconcile.khat_fallback` e `n_fallback`, nunca silencio.
- perfil reutilizado: no apply de producao, uma origem O >= khat_origin reutiliza o perfil ja ajustado (o oraculo reajusta por origem); este golden cobre o ajuste por origem (fit em O).
- bucket por `interval_start`: o oraculo bucketiza pelo rotulo de inicio da carga; o clean-room deriva o inicio de `interval_end - 30 min` na borda do LONG, mesmo resultado (Delta = 0).
- dow: domingo = 1 (`week_start = 7`) nos dois lados; CONTROL com `week_start = 1`, janela de 729 dias e bucket por `interval_end` quebram a paridade.
