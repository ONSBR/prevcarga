# Estado: verbo release-pack e scripts/tools/upload (M8.5; ADR-0039, ADR-0040, ADR-0030)

## O que e
`prevcarga release-pack --what=<runtime|artifacts|forecasts> --out=<dir> --published=<arq> [--release=<v>] [--bundles=<dir>] [--model=<m,..>] [--origin=<O>] [--latest]`
monta LOCALMENTE (sem rede, sem variavel de ambiente, Linux e Windows) um diretorio no layout do bucket publico. **Nada e publicado aqui**:
o envio e do `scripts/tools/upload` (dev, so Linux, fora do bundle), unico arquivo do repo de produto que conhece o servico de
armazenamento. O nome do bucket segue `[TBD: ADR-0039]` e so existe como `--bucket` do upload. Codigo: `scripts/lib/release_pack.R`
(`prevcarga_release_pack` + `pc_rp_*`), fachada `scripts/lib/cli/release-pack.R` (descoberta por cli.R; `scripts/prevcarga` nao foi editado),
ids em `scripts/lib/errors/release_pack.tsv`.

## Layout produzido
- `runtime`: `releases/<v>/{prevcarga-<v>-linux-x86_64.tar.zst, prevcarga-<v>-windows-x86_64.zip, RUNTIME-<plataforma>.json, SHA256SUMS}`.
  O RUNTIME.json e extraido do proprio bundle (zip por `utils::unzip`, tar.zst pelo `tar --zstd` do sistema); se nao der, usa o
  `<bundle>.RUNTIME.json` ao lado (companheiro do build do M8.1, nunca copiado nem recusado, como o `.sha256`).
- `artifacts`: `artifacts/<modelo>/[<variante>/]<area>/v_<hash>/`, `artifacts/combina/<area>/v_<hash>/`, `artifacts/reconcile/<ss>/v_<hash>/`:
  so a versao apontada pelo `latest.txt` de cada celula do registry local (leitor `prevcarga_registry_ls`, o do M5). Bytes copiados
  (sha256 identico ao registry); `.history`, `.pin` e `.tmp-*` so sao locais e nunca saem. O `SHA256SUMS` do registry dentro de `v_<hash>/` e mantido.
- `forecasts`: `forecasts/<AAAA-MM-DD>/<AAAAMMDDTHHMM>/<versao>/` de cada entrega local da origem `--origin` (`<state_dir>/forecasts/...`),
  copia byte a byte; a chave do caminho tem de coincidir com a recomputada do `run_manifest.json` por `prevcarga_export_key` (M6.7), senao `chave_divergente`.
- `SHA256SUMS` (`<sha256>  <caminho relativo com />`, radix, LF; `sha256sum -c` passa) na raiz de `<out>`, dentro de cada `v_<hash>/`, de cada chave de forecasts e de `releases/<v>/`.
- `latest.txt` (`releases/latest.txt` com `<v>`; `artifacts/.../<area>/latest.txt` com `v_<hash>`) so com `--latest`; e a unica chave mutavel.

## Invariantes
1. Stage irmao `<out>.stage-<run_id>` (mesmo volume); so com ZERO recusas o stage vira `<out>` por `file.rename`; qualquer erro remove o stage
   (`on.exit`). `<out>` existente e nao-vazio => `config.release_out_not_empty` (8) sem tocar nada. Toda E/S de escrita passa por `.pc_rp_copy` (o teste injeta falha ali).
2. Recusas acumulam em `result.recusados` (`path`, `motivo`) e saem num UNICO erro `integrity` (9): `fora_da_allowlist` (fora da allowlist do `--what`,
   inclui deck, dados e `config.yaml` soltos), `proibido` (config.yaml, credencial, `*.pem`/`*.key`, `.aws/`, `.env`, parquet de entrada do deck
   `load*`/`weather_*`/`auxiliary/` DENTRO de uma arvore permitida), `deck_manifest` (manifest.json de deck dentro de uma chave de forecast),
   `chave_publicada` (chave presente em `--published`, que e a saida de `upload --list`; `latest.txt` nunca conta), `recibo_ausente|recibo_outro_sha|recibo_fail:<gate>`,
   `versao_divergente`, `runtime_json_ausente`, `run_manifest_ausente`, `run_manifest_invalido`, `chave_divergente`. Id do erro por precedencia:
   `integrity.release_forbidden_content` (do core.tsv) > `integrity.release_key_published` > `integrity.release_receipt` > `integrity.release_inconsistent`.
   Depois de montar, uma autoconferencia reclassifica TUDO que vai sair (defesa em profundidade).
3. Conjunto oficial de `artifacts`: toda folha de `prevcarga_topology()` precisa de `combina/<folha>` e todo subsistema de `reconcile/<ss>` com `latest.txt`;
   faltou => `state.official_set_incomplete` (13) com `details.faltam`, nada escrito. Recusas de conteudo (9) vem antes do 13.
4. Recibos (`tests/.receipts/<sha>.<gate>.json`, escritos por `scripts/tools/receipts.R`, lidos aqui com jsonlite): Linux `install-e2e-linux` + `ab-corte`;
   Windows `install-e2e-windows` + `windows-vm` + `ab-corte`; sha256 do arquivo recalculado, `sha256` do recibo igual, `resultado == PASS`, `ab-corte` com `veredito == CORTE-OK`.
   `--receipts-dir` nao e flag: o default e `<root>/tests/.receipts` (o argumento existe so na funcao, para teste).

## Desvios do texto do ticket (decididos aqui; motivo)
- Flag **`--latest`** no lugar de `--promote`, e **`--release=<v>`** no lugar de `--version=<v>`: o gate `registry_lifecycle` (test-static-greps.R) proibe a palavra
  `promote` em qualquer codigo de `scripts/lib` fora de `registry.R` (strings inclusas); `--version` e a flag global (sem argumento) e o docopt a tornaria ambigua.
- `--model` aceita lista separada por virgula (a flag global nao repete); `--out` ja consta no bloco Options (combine), nao foi redeclarada.
- Reuso do id `integrity.release_forbidden_content` (core.tsv, M1.4b) em vez de criar `integrity.release_forbidden`; `integrity.release_inconsistent` e novo.
- Motivo de um deck/`config.yaml` solto no diretorio de bundles e `fora_da_allowlist` (como diz o AC 1); `proibido` fica para o sensivel dentro de arvore permitida.
- Um `.sha256`/`.RUNTIME.json` companheiro do build nao e recusado nem copiado.
- `export_error_catalog.R` recebe `--out <arq>` (nao argumento posicional).
- O modulo le o manifest.json da entrega so para SONDAR e recusar um manifest de deck; o nome do arquivo vem de constante para nao acionar o gate
  de leitor unico (`m2_single_reader*`): o deck como insumo continua lido so por `deck_reader.R`.

## scripts/tools/upload
`upload --bucket <b> --list [--profile <p>] [--format text|json]` imprime as chaves versionadas (sem `latest.txt`); `upload --bucket <b> <dir>` confere cada chave com
`head-object` (404 = ausente; qualquer outra falha = rede, exit 10) e recusa sobrescrever chave versionada (`upload.key_exists`, 9) ANTES de enviar qualquer objeto;
envia as versionadas com `--object-lock-mode COMPLIANCE` e os `latest.txt` por ultimo, sem trava. O `SHA256SUMS` da RAIZ de `<dir>` e conferencia local do pacote e nao sobe
(senao colidiria no segundo release). Nao calcula hash nem decide conteudo. Contrato ADR-0040: o catalogo (`id, class, exit_code, message, hint`) e GERADO sob demanda em
`mktemp -d` por `export_error_catalog.R --out` (ou `--catalog <tsv>`); nunca lido do repo; fallback embutido com o MESMO texto (o teste compara). JSON unico em stdout e stderr vazio.
Exits: 0, 8 `upload.usage`, 9 `upload.key_exists`, 10 `upload.network`.

## Testes
`scripts/lib/tests/test-release_pack.R` (20 testes, in-process + 2 de processo; roda no Windows): runtime/artifacts/forecasts com FORBIDDEN e CONTROL, `<out>` nao-vazio, falha de E/S injetada, SHA256SUMS, allowlist.
`tests/install/test-upload.R` (so Linux; `aws` falso no PATH do filho, nunca rede/credencial): `--list`, chave existente sem envio, so `latest.txt` regravado, rede 10, uso 8, catalogo gerado, grep.
O grep "so upload conhece o servico" cobre o codigo de PRODUTO (scripts/prevcarga, scripts/lib, install, config, models/*/R); dev tools antigos (`scripts/tools/bundle_gates.R`) e docs citam o termo.

## Pendencias
- `prevcarga-sync` (M8.6, `docs/estado/sync.md`) consome este layout (`artifacts/.../latest.txt` + `SHA256SUMS` por `v_<hash>`). Pendente no release-pack: `--what forecasts` deveria escrever `forecasts/<O>/latest.txt` (uma `<available_until>/<versao>` por linha), o indice que o sync le.
- Retencao do Object Lock (`--object-lock-retain-until-date`) e a aceitacao da flag por `aws s3 cp` dependem da politica do bucket ([TBD: ADR-0039]); ver discovery do M8.5.
- O `install.sh` (M8.3) le o catalogo gerado cortando as colunas 3/4/5 como se fossem `class/message/hint` (o gerado tem `exit_code` na coluna 3); ver discovery.
