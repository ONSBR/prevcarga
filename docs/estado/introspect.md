# Estado: comandos de introspeccao (M1.5)

- `scripts/lib/introspect.R` concentra os 5 verbos: construtor puro `prevcarga_build_<verbo>(ctx)` + renderizador texto
  ASCII `prevcarga_render_<verbo>(result)` + `cmd_<verbo>(ctx, args)`. O `cmd_*` SO devolve o resultado: quem imprime
  (texto/JSON, banner, exit) e `prevcarga_emit()` chamado por `prevcarga_main` (chamar emit no handler imprimiria 2x).
- Registro por auto-descoberta: `scripts/lib/cli/{doctor,status,models,areas,describe}.R`, owner `M1`; `doctor`/`status`
  `exempt = TRUE` (ADR-0022, sem enforce; config quebrado vira `ctx$config_error`). O handler e um wrapper
  `function(ctx, args) cmd_<verbo>(ctx, args)` (resolvido em tempo de chamada, pois `modules` so e carregado depois da
  descoberta). `scripts/prevcarga` e `cli.R` nao foram tocados.
- `doctor`: 18 linhas fixas (TZ, locale, 4 pins, pool BLAS, versao do R [WARN != 4.5.3], 6 pacotes obrigatorios [FAIL],
  arrow/withr [WARN], config.yaml, data_dir [WARN ausente]); found x effective vem de `prevcarga_runtime_apply()` (shell
  sujo nao e erro). Exit calculado DEPOIS de montar todas as linhas: `prevcarga_exit_precedence` entre runtime (7),
  config (8) e bundle (6); atributo `exit_code`. Vocabulario PASS/WARN/FAIL/N/A aberto para o M8 acrescentar linhas.
- `status`: resumo do doctor + deck (best-effort; manifest lido pelo leitor unico `prevcarga_deck_manifest`, nunca por
  caminho literal aqui: `test-deck-reader.R` proibe) + `bundle: N/A` (M8); sempre exit 0.
- `models`: membros x `prevcarga_model_spec()` (nada literal; copia com run.yaml editado muda a saida).
- `areas`: SIN -> subsistemas em `$order` -> areas, com perda e `khat_range` efetivo de `prevcarga_topology()`.
- `describe [--origin] [--model]`: topologia + specs + `member_set_hash`; cortes (`train_origin`, `last_data_day`,
  `available_until`) sao NULL com a nota "derivado no M2": nunca calcula data de corte (o M2 re-liga em
  `scripts/lib/cli/describe.R` com dates.R). `--model` aceita o `mdir`; desconhecido => `config.unknown_model` (exit 8).
- Erros de topology/run.yaml chegam como `contract` (exit 2) pela fachada.
- Testes: `scripts/lib/tests/test-introspection.R` (o main roda com a pilha da CLI em globalenv, como o shim).
