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
