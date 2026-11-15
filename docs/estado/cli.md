# Estado: fachada da CLI (M1.4)

- `scripts/prevcarga` e um shim so de despacho (unico ticket que o edita): acha-se pelo `--file=`, define
  `options(prevcarga.lib)`, carrega `errors.R`+`runtime.R`, resolve `prevcarga_install_root()`, carrega
  `config.R`/`banner.R`/`cli.R` e faz `quit(status = prevcarga_main(...))`. Protegido por
  `identical(environment(), globalenv())` (carregavel por `sys.source` nos testes). Nao conhece verbo algum.
- `scripts/lib/cli.R`: `prevcarga_verbs_discover(dir, planned)` (barrel por descoberta, `scripts/lib/cli/*.R` em
  ordem `sort(method="radix")`; cada arquivo define `prevcarga_verb_<verbo>` = list(name, usage, options, handler,
  owner, exempt, modules); arquivo sem objeto/nome divergente/handler nao-funcao = `stop` interno => `generic.unexpected`).
  Tabela `verb/owner/exempt/usage` + list-cols `modules/options/handler`. Verbos de `prevcarga_verbs_planned` sem
  arquivo viram stubs (handler NULL, usage `prevcarga <v> [<args>...]`) -> `todo.not_implemented` exit 1,
  `where = "<verbo> -> <dono>"`. Cada epico entra criando `scripts/lib/cli/<verbo>.R`; nunca edita cli.R nem o shim.
- `prevcarga_usage(verbs)` gera o docopt (cada linha ganha `[options]`; flags globais no bloco Options, sem
  `[default:]`: default e do config.R). Verbo/flag/posicional fora da gramatica = exit 8 `config.usage`, `where` = token.
- `prevcarga_main(argv, verbs, root, runtime, handlers)`: `-h/--help`, `--version`, sem verbo => banner (so com stdout em terminal)+usage/versao,
  exit 0, sem runtime. Verbo guardado: `runtime()` e a 1a acao do corpo (ADR-0022), depois `prevcarga_ctx`, `modules`
  (`sys.source` relativo a root) e handler. Isencao fechada: `exempt = TRUE` (doctor/status); config.yaml quebrado
  nesses verbos nao aborta: ctx de defaults+flags com `ctx$config_error`. Handler pode devolver
  `attr(result, "exit_code")` (impresso e usado como exit). Avisos `prevcarga_warning` viram `warnings` do envelope.
- `deck_roles` (campo opcional do registro `prevcarga_verb_<v>`; ausente = `train`): subconjunto de `c("train","inference")`
  ou `function(args)` que o devolve; validado em `.prevcarga_load_verb_file` (invalido = `stop` de bug do pacote) e exposto na
  list-col `deck_roles` de `prevcarga_verbs_discover()`. Ordem em `.prevcarga_run_verb` (verbo nao exempt): `runtime()` ->
  `prevcarga_ctx` -> `prevcarga_resolve_deck_role(ctx, roles, args)` -> `modules` -> handler. Verbos exempt (doctor, describe,
  models, status, areas) nao resolvem papel. Resolucao: flag (`--data-inference` => inference; `--data-train`/`--data` => train)
  > `data_train_dir` > `data_inference_dir` > train com `data_dir` NULL (a guarda do verbo segue abortando
  `config.data_dir_missing`). Flag de treino junto com `--data-inference` = `config.usage` (exit 8, `where` =
  `--data-train x --data-inference`). Papel fora de `roles` = `contract.deck_role_refused` (exit 2, `where` = opcao,
  `details` role/verb/accepted) antes de qualquer leitura. Define `ctx$data_role`, `ctx$data_role_option` e `ctx$data_dir`
  (deck ativo, lido por `prevcarga_require_deck`).

  | Verbo | Papeis aceitos |
  |---|---|
  | forecast, infer-model, validate-deck | train, inference |
  | reconcile | train com `--mode=train` ou `--eval`; senao train e inference (apply) |
  | combine | train com `--mode=train` ou `full`; senao train e inference (apply) |
  | demais verbos de deck (train-models, backtest, plan, score, features) | train (campo ausente) |
- Handler de topo = `prevcarga_with_errors` (errors.R): classificado => exit da tabela; resto => `generic.unexpected`
  exit 1; traceback so com `--debug` (stderr). `--format json` = um unico envelope em stdout (erro tambem), stderr vazio.
- Contrato do handler: `function(ctx, args)` (ou `function(ctx)`) -> list(schema_version = 1L, ...); texto via
  `prevcarga_render_<verbo>(result)` se existir, senao print ASCII generico.
- Testes: `scripts/lib/tests/test-facade.R` (verbos-fixture em dir temporario; lista de verbos/stubs derivada da descoberta).

- **Opcoes de dados (E1-T2):** `.prevcarga_global_options` ganha `--data-train`, `--data-inference` e `--data-artifacts`; `--data` e
  `--artifacts-dir` NAO existem (ADR-044, clean break): sem apelidos, a opcao antiga e opcao desconhecida (erro de uso, exit 8). Nenhuma opcao nova e prefixo de outra (docopt so casa prefixo sem casamento exato).
  Ver `docs/estado/config.md`; teste `test-data-options.R`.

- **set-data-dir (2026-11-10):** 21o verbo, `prevcarga set-data-dir [<dir>] [--train=<dir>] [--inference=<dir>] [--artifacts=<dir>]` (E1-T5; interativo sem chave), dono M8, nao isento. A escrita mora em config.R
  (ver `docs/estado/config.md`). O instalador sugere o verbo nos proximos passos quando o `data_dir` falta.

- **Ajuda por verbo (F2.3):** `prevcarga <verbo> -h|--help` imprime `prevcarga_usage_verb(verbs, verb)` (stdout, ASCII, rc 0, sem JSON
  e sem runtime do verbo): titulo, `Usage:` com a usage do verbo, e `Options:` com as comuns (globais menos `--origin`/`--model`)
  mais as linhas de opcao (de qualquer verbo ou global) cujo nome a usage do verbo cita. Regra da opcao compartilhada: a linha de
  opcao mora so no verbo dono (o docopt exige uma so) e os demais verbos a citam na usage (e em `scoped`); a ajuda a encontra
  por nome. `prevcarga --help` sem verbo segue sendo `prevcarga_usage(verbs)` (todas as opcoes). A gramatica do docopt nao muda:
  o filtro e so de apresentacao. `.prevcarga_opt_lines(verbs)` e o parser unico das linhas de opcao (`.prevcarga_opt_table` deriva
  dele). Teste: `scripts/lib/tests/test-cli-help-verb.R`.
