# registry_lifecycle (M5.7) -- ciclo de vida do registry + verbo `registry`

Codigo: `scripts/lib/registry.R` (fim do arquivo; unico dono do ciclo de vida, gate `registry_lifecycle`), verbo
`scripts/lib/cli/registry.R` (fachada fina; so le de registry.R a lista de subcomandos e o usage), erros em
`scripts/lib/errors/registry_lifecycle.tsv`, testes em `scripts/lib/tests/test-registry-lifecycle.R`. Constroi sobre
`registry_core.md` (M5.2).

Verbo: `prevcarga registry (ls|show|promote|rollback|pin|unpin|gc) [<model>] [<area>] [<version>] [--dry-run]`.
`<model>` = sigla do membro (resolve a variante), mdir ou pasta do registry (`combina`, `reconcile`). Despacho unico em
`prevcarga_registry_run`. Celula inexistente => `state.cell_missing` (13).

- `promote`: valida a versao e delega a `prevcarga_registry_promote_latest` (tempfile `.tmp-latest-*` + rename + `.history`).
  Falha de rename => `state.promote_failed`; o `.tmp-*` fica (como numa interrupcao real) e o `gc` o coleta; latest nunca
  fica vazio. `rollback`: penultima versao distinta da `.history` que ainda existe em disco; sem anterior =>
  `state.registry_no_previous`. `pin`/`unpin`: `.pin` (varias linhas) reescrito por tempfile + rename; inexistente/nao
  pinada => `state.pin_missing`.
- Invariante k-hat: o latest do perfil do Reconcile (manifest `produto == "reconcile"`) so anda para frente; promote/rollback
  para `khat_origin` menor => `state.registry_khat_backward` (13). Pin nao move o latest. O consumo do k-hat continua
  exigindo `khat_origin == O` (decisao 2026-10-29); promote/rollback manual nao pode contorna-lo.
- `gc [--dry-run]`: percorre `<state>/artifacts` sem seguir links (link = caminho resolvido != pai resolvido + nome; sem
  `Sys.readlink`, gate unix_only), inclusive `combina/` e `reconcile/`. Remove `^v_[0-9a-f]+$` orfaos (nem latest nem
  pinada) e qualquer entrada `^[.]tmp-` (staging de qualquer escritor). Celula sem `latest.txt` nem `.pin` nao tem `v_*`
  podada (reason `no_pointer`; nada a preservar por referencia). Todo alvo passa por `prevcarga_registry_contain`.
  Retorno `list(removed, kept = data.table(key, version, reason), dry_run)`. Sem lock entre processos; higiene local, nunca S3.
- `ls`/`show` sao read-only; `show` le o `run_manifest.json` (model_id/cuts tambem de `fingerprint` no manifest de treino;
  timezone = `load_tz`). Celulas oficiais sincronizadas (sem `.pin`/`.history`) aparecem igual.
