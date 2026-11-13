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

- **set-data-dir (2026-11-10):** 21o verbo, `prevcarga set-data-dir <dir>`, dono M8, nao isento. A escrita mora em config.R
  (ver `docs/estado/config.md`). O instalador sugere o verbo nos proximos passos quando o `data_dir` falta.

- **Ajuda por verbo (F2.3):** `prevcarga <verbo> -h|--help` imprime `prevcarga_usage_verb(verbs, verb)` (stdout, ASCII, rc 0, sem JSON
  e sem runtime do verbo): titulo, `Usage:` com a usage do verbo, e `Options:` com as comuns (globais menos `--origin`/`--model`)
  mais as linhas de opcao (de qualquer verbo ou global) cujo nome a usage do verbo cita. Regra da opcao compartilhada: a linha de
  opcao mora so no verbo dono (o docopt exige uma so) e os demais verbos a citam na usage (e em `scoped`); a ajuda a encontra
  por nome. `prevcarga --help` sem verbo segue sendo `prevcarga_usage(verbs)` (todas as opcoes). A gramatica do docopt nao muda:
  o filtro e so de apresentacao. `.prevcarga_opt_lines(verbs)` e o parser unico das linhas de opcao (`.prevcarga_opt_table` deriva
  dele). Teste: `scripts/lib/tests/test-cli-help-verb.R`.
