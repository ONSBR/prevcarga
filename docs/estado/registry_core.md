# registry_core (M5.2) -- nucleo do registry local no layout do bucket

Codigo: `scripts/lib/registry.R` (fim do arquivo), erros em `scripts/lib/errors/registry_core.tsv`, testes em
`scripts/lib/tests/test-registry-core.R`. Requer errors.R + provenance.R (freeze/thaw/run_manifest).

Layout (ADR-0039/0030), identico ao prefixo `artifacts/` do bucket publico:
`<state_dir>/artifacts/<mdir>/[<exogena>/]<area>/{latest.txt, v_<hash>/{artifact.rds, artifact.rds.manifest.json,
run_manifest.json, SHA256SUMS}}`; so locais: `.pin`, `.history`, `.tmp-*`. Texto UTF-8 com LF. O pacote nao fala
rede: oficiais gravados pelo `prevcarga-sync` (M8) sao tratados igual aos proprios, sem conversao.

API: `prevcarga_registry_root/contain/key` (segmento de variante sse `member$exogena` nao vazia; sem decisao por nome
de modelo; `..` colapsado lexicamente porque normalizePath nao o faz em caminho inexistente),
`prevcarga_resolve_version(key, mode, version, ctx = NULL)`, `prevcarga_registry_publish` (staging `.tmp-<rand>/`
irmao + file.rename; `force` move o antigo para `.tmp-old-*` e o restaura se o rename do novo falha),
`prevcarga_load_artifact` (confere SHA256SUMS antes do thaw; exit 9 `integrity.artifact_hash`),
`prevcarga_registry_promote_latest` (tempfile `.tmp-latest-*` + rename; 1 linha em `.history`).

Decisoes: o sidecar `artifact.rds.manifest.json` do `prevcarga_freeze` e parte da versao e entra no SHA256SUMS;
falha de publicar/promover = `state.promote_failed`; `.tmp-*` orfaos ficam para o `registry gc` (M5.7). `ctx`
opcional nas funcoes de leitura/escrita verifica o contido em `<state_dir>/artifacts`.

## F3.6 -- raiz efetiva
- `prevcarga_registry_root(ctx)` devolve `ctx$artifacts_dir` normalizado quando presente (raiz apontada, so leitura) e senao
  `<state_dir>/artifacts`; contain/key valem sobre a raiz efetiva (membros, `combina/<folha>`, `reconcile/<ss>` do k-hat). A raiz default segue inalterada.
- `prevcarga_registry_mode(ctx)`: `"apontado"` | `"registry"`. A identidade e a integridade (sha) das versoes valem iguais fora do dir default.

## key_params e chave por parametro (E5-T4)
- `descriptor$key_params`: lista nomeada param -> default `character(1)` (nome em `params_schema`, senao `plugin.bad_descriptor` com `key_params$<param>`); ausente = `list()`. Param de `key_params` e OPCIONAL no run.yaml (`prevcarga_plugin_check_params(..., optional =)`; presente ainda e conferido pelo tipo) e o default nunca e injetado em `spec$params`.
- `prevcarga_registry_key(member, area, ctx, spec = NULL)`: `<root>/<mdir>[/<exogena>][/<param>-<valor>...]/<area>`. Com `spec`, carrega o descriptor do mdir (cache do loader) e, em ordem declarada, acrescenta o segmento so se `spec$params[[p]]` existe, difere do default e `p` nao esta em `variants[[exogena]]$ignores_params`. `spec = NULL` ou valores default => caminho anterior byte a byte. O reconcile (k-hat) continua sem `spec`.
- Call sites com o spec efetivo: `prevcarga_train` e `prevcarga_train_plan` (`inp$spec`), `prevcarga_infer_resolve` e `.forecast_scoped_version` / preflight do forecast (`.prevcarga_tr_key_spec`: run.yaml + overrides do membro; NULL se o descriptor nao tem `key_params`).
- `prevcarga_registry_cell_key` (e `registry <model>`) aceita `mdir/exogena/<param>-<valor>` (segmentos separados por `/`) alem da sigla.
- Testes: `scripts/lib/tests/test-registry-key-params.R` (descriptors sinteticos).
