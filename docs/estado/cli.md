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
- Handler de topo = `prevcarga_with_errors` (errors.R): classificado => exit da tabela; resto => `generic.unexpected`
  exit 1; traceback so com `--debug` (stderr). `--format json` = um unico envelope em stdout (erro tambem), stderr vazio.
- Contrato do handler: `function(ctx, args)` (ou `function(ctx)`) -> list(schema_version = 1L, ...); texto via
  `prevcarga_render_<verbo>(result)` se existir, senao print ASCII generico.
- Testes: `scripts/lib/tests/test-facade.R` (verbos-fixture em dir temporario; lista de verbos/stubs derivada da descoberta).
