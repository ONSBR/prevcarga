# Estado: instalador Linux e lancador (M8.3; ADR-0038, ADR-0040)

## O que e
`install/install.sh` (POSIX sh, so Linux x86_64; o Windows e o `install.ps1` do M8.4, WSL2 nao e requisito) instala o bundle do M8.1
(`docs/estado/m8-bundle.md`) **sem admin** em `<R>` = `~/.local/share/prevcarga` (`--root` troca; so o DEFAULT le `$HOME`). **Nada e
publicado nem baixado nos testes**: o upload e de outro ticket; os testes usam `--bundle` local e raizes temporarias em `sandbox/`.
`install/prevcarga` e o lancador fino copiado para `<R>/bin/prevcarga`.

## Uso (so por argumentos; sem variavel de ambiente)
`install.sh [--version <v>|latest] [--release-base <url>] [--bundle <arq> [--sha256 <h>]] [--root <dir>] [--data <dir>] [--state <dir>]
[--skip-platform-check] [--add-to-path] [--profile <arq>] [--format text|json]`; `--rollback`; `--uninstall --yes [--purge]`.
Sem `--bundle` baixa `<release-base>/releases/<v>/prevcarga-<v>-linux-x86_64.tar.zst` e `SHA256SUMS` (HTTPS; falha => exit 10);
`latest` le `<release-base>/releases/latest.txt` (1 linha) [DEFERRED M8.5: o release-pack publica esse arquivo; o nome e premissa deste ticket].
Com `--bundle` a versao vem de `--version` ou do nome `prevcarga-<v>-linux-x86_64.tar.zst`.

## Layout de `<R>` (gravado pelo instalador)
`versions/<v>/{runtime,syslib,code,RUNTIME.json}` (imutavel), `current` (1 linha `<v>`; trocado por tempfile + `mv -f`, nunca symlink),
`previous` (1 linha; a versao que estava em `current` antes da ultima instalacao; apagado pelo `--rollback`), `config.yaml`, `state/`, `bin/prevcarga`.
O runtime do bundle e `runtime/bin/Rscript` (wrapper do M8.1, ja relocavel: o instalador NAO reescreve `R_HOME`; o texto do ticket citava
`runtime/R/bin`, o layout real e o do M8.1).

## Ordem e invariantes
1. Argumentos (desconhecido => `install.usage`, exit 8) e `--state` dentro de `<R>/versions` => `install.state_inside_versions` (8): antes de qualquer escrita.
2. Preflight: recusa musl, glibc < 2.35, arquitetura != x86_64, falta de zstd/tar/sha256sum/awk e de curl|wget (`install.preflight_failed`, exit 6;
   `--skip-platform-check` vira aviso).
3. Aquisicao: download ou `--bundle` num `mktemp -d` FORA de `<R>`; sha256 obrigatorio (`--sha256`, ou `SHA256SUMS` ao lado; senao `install.sha_missing`;
   errado `install.sha_mismatch`; ambos exit 9) conferido ANTES de extrair. Ate aqui `<R>` fica byte-identica.
4. Extracao em `<R>/versions/.stage-<v>/` (trap remove o stage em qualquer saida), validacao de TODO `files[]` do `RUNTIME.json` com o `Rscript --vanilla`
   do proprio stage (`prevcarga_runtime_validate(strict=TRUE)`; falha => `install.manifest_invalid`, exit 6, `current` intacto), rename do stage para
   `versions/<v>/`, copia do lancador, troca atomica de `current`.
5. Retencao N=2: ficam `current` e `previous`; o resto de `versions/` sai. Reinstalar a mesma versao preserva a anterior.
6. `config.yaml` (schema v1 do M1.4a) com `data_dir`/`state_dir` (default `<R>/state`); upgrade preserva chaves existentes; `state/` e criado e NUNCA tocado
   por upgrade/rollback/uninstall (so `--uninstall --yes --purge` o remove).
7. `--add-to-path` grava um bloco `# >>> prevcarga >>>` ... `# <<< prevcarga <<<` no perfil (`--profile`, default `~/.profile`), idempotente; o uninstall o remove.
8. Fim: `<R>/bin/prevcarga doctor --format json` e devolvido em `result.doctor` (JSON unico em `--format json`).
- `--rollback` aponta `current` para `previous`; sem ele (ou depois de um rollback) => `install.no_previous_version` (8), `current` intacto.
- Erros: `scripts/lib/errors/install.tsv` (catalogo distribuido, ADR-0040 item 8); `pc_die` le o `error-catalog.tsv` GERADO ao lado do script (no
  stage: `code/install/`) e tem fallback embutido com o MESMO texto (o teste compara). JSON unico em stdout (stderr vazio) ou texto ASCII em stderr com
  a 1a linha `prevcarga: erro [<classe>] (exit <n>)`.
- **Ids sem digito**: o loader (`errors.R`, `^[a-z]+\.[a-z_]+$`) rejeita digitos, entao os ids sao `install.sha_missing` / `install.sha_mismatch`
  (o texto do ticket dizia `sha256_*`); `errors.R` nao foi editado.

## Lancador (`install/prevcarga`)
Resolve `<R>` por `readlink -f "$0"`, le a 1a linha de `current`, confere `versions/<v>/runtime/bin/Rscript` e faz `exec env -u R_HOME ... Rscript --vanilla
versions/<v>/code/scripts/prevcarga "$@"`. Nao altera variavel de ambiente (a palavra proibida nao aparece no arquivo; o teste conta 0). Sem `current` ou com
versao inexistente: `prevcarga: erro [bundle] (exit 6)` em ASCII. O bundle carrega o lancador em `code/install/prevcarga` (copiado por
`build_bundle_linux.sh`); bundles antigos usam o que esta ao lado do `install.sh`.

## Fachada: config e doctor da instalacao
O codigo instalado mora em `<R>/versions/<v>/code`, entao `prevcarga_ctx` (config.R) tambem procura `<R>/config.yaml` (`prevcarga_install_dir(root)`,
so no layout `.../versions/<v>/code`; arvore de dev nao muda). `ctx$install_root` e os campos `install_root`/`state_dir` do `result` do doctor
(JSON; linhas de `checks` inalteradas) mostram onde esta a instalacao.

## Testes (`tests/install/`, sob R 4.5.3; precisam de um bundle candidato em `dist/`, senao fazem skip com motivo)
`test-install-e2e-linux.R` (AC 1-4, 6, 7; grava o recibo `install-e2e-linux` por `scripts/tools/receipts.R`, em `tests/.receipts/`, gitignored) e
`test-shell-sujo-linux.R` (AC 5, bundle COMPLETO). Helper: `helper-install.R` (bundles reempacotados a partir do candidato com o `code/` do worktree
sobreposto; variante `slim` sem as libs pesadas; `pc_tree_sha`; variante de teste do instalador que troca `current` antes de validar = CONTROL do AC 2).
- Raizes e bundles em `sandbox/` (mesmo disco; ~500 MB por bundle completo), apagados ao fim. Nunca `~/.local`, `~/.profile`, rede ou upload.
- [DEFERRED M6.9] o verbo `forecast` ainda nao existe na CLI: o teste do shell sujo usa `describe` e `validate-deck` (texto e JSON) + `--version`.
  Unica diferenca permitida entre execucoes: `run_id` (relogio UTC + pid) do envelope JSON. Se o host nao tem `pt_BR.UTF-8`, o shell sujo usa o 1o
  locale UTF-8 instalado (o bash do `bin/R` avisa por conta propria); se o fuso do host ja e `America/Sao_Paulo`, o TZ sujo vira `Asia/Tokyo` (a sonda do CONTROL precisa distinguir).
- [DEFERRED M8.4/M8.7] braco Windows (`install.ps1`) e `doctor` validando o RUNTIME.json: outros tickets.
