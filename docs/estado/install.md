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
`latest` le `<release-base>/releases/latest.txt` (1 linha) (publicado pelo `release-pack --what runtime --latest`, M8.5).
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
versao inexistente: `prevcarga: erro [bundle] (exit 6)` em ASCII.
**Hermetico a library herdada (M8.9, decisao do Gabriel)**: o `exec` e `env -u R_HOME -u R_LIBS -u R_LIBS_USER -u R_LIBS_SITE -u R_PROFILE_USER -u R_ENVIRON_USER`.
`--vanilla` ignora os ARQUIVOS de perfil/Renviron mas NAO as variaveis de ambiente herdadas (todo processo R que lanca filhos as propaga, e muitos
usuarios as fixam no perfil do shell): sem isso um `R_LIBS_USER` poe a library do host na frente da do bundle no `.libPaths()` (visto no e2e do M8.9: o `arrow` do
host, compilado contra outro libstdc++, foi carregado no lugar do do bundle). `env -u` nao define nada (o teste continua contando 0 `export`). Teste:
`tests/install/test-shell-sujo-linux.R` (library sombra com um `jsonlite` falso que aborta ao carregar; saida byte a byte igual a do shell limpo; CONTROL: o lancador antigo,
so `-u R_HOME`, carrega a sombra e cai). O e2e do M8.9 NAO remove essas variaveis do filho: prova o lancador. `runtime-libpaths` (forcar `.libPaths()` em processo) e uma
linha de WARN no doctor nao foram necessarias. Fora do escopo: o `install.sh` (validacao do stage, linha `env -u R_HOME ... Rscript`, fixada pelo helper dos testes) e o
`install.ps1` ainda herdam o ambiente ao validar o bundle; o `prevcarga-sync` so chama o lancador. O bundle carrega o lancador em `code/install/prevcarga` (copiado por
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
- (fechado no M8.9) `forecast --origin O` roda no teste do shell sujo pelo lancador instalado do mundo do e2e (`docs/estado/m8-e2e-bundle.md`): entrega identica byte a byte entre o shell sujo e `env -i` (so o `gerado_em` do manifest, relogio da rodada, difere), CONTROL com outro deck. `describe`/`validate-deck`/`--version` ficam como braco barato.
  Unica diferenca permitida entre execucoes: `run_id` (relogio UTC + pid) do envelope JSON. Se o host nao tem `pt_BR.UTF-8`, o shell sujo usa o 1o
  locale UTF-8 instalado (o bash do `bin/R` avisa por conta propria); se o fuso do host ja e `America/Sao_Paulo`, o TZ sujo vira `Asia/Tokyo` (a sonda do CONTROL precisa distinguir).
- [DEFERRED M8.7] `doctor` validando o RUNTIME.json: outro ticket. O braco Windows e a secao abaixo (M8.4).

# Instalador Windows nativo e lancador `prevcarga.cmd` (M8.4; ADR-0038, ADR-0040)

## O que e
`install/install.ps1` (Windows PowerShell 5.1 e pwsh 7, `-ExecutionPolicy Bypass`, **sem admin**) espelha o `install.sh`: mesmo layout `<R>\current` +
`<R>\versions\<v>` (+ `previous`, `config.yaml`, `state\`, `bin\prevcarga.cmd`), mesma ordem de invariantes, mesmos `error.id` e exits
(`install.sha_missing`/`install.sha_mismatch` 9, `install.manifest_invalid`/`install.preflight_failed`/`install.io_failed` 6, `install.usage`/
`install.no_previous_version`/`install.state_inside_versions` 8, `install.download_failed` 10). **Nada e baixado nem publicado nos testes.**
Raiz default `%LOCALAPPDATA%\prevcarga` lida por `[Environment]::GetFolderPath('LocalApplicationData')` (nenhum `$env:` no instalador).

## Uso
`install.ps1 [-Version <v>|latest] [-ReleaseBase <url>] [-Bundle <zip> [-Sha256 <h>]] [-Root <dir>] [-Data <dir>] [-State <dir>] [-SkipPlatformCheck] [-Format text|json]`;
`-Rollback`; `-Uninstall -Yes [-Purge]`. A grafia `--bundle`/`--sha256`/... dos docs tambem e aceita (captura em `ValueFromRemainingArguments`).
Argumento desconhecido/valor ausente/`-Format` invalido => `install.usage` (exit 8) pelo contrato, nao pelo erro nativo do PowerShell.

## Diferencas deliberadas em relacao ao `install.sh`
- Zip em vez de tar.zst: `releases/<v>/prevcarga-<v>-windows-x86_64.zip` (+ `SHA256SUMS`; TLS 1.2 forcado; `Invoke-WebRequest -UseBasicParsing`).
  O zip do M8.2 tem a pasta `prevcarga/` no topo: o stage `versions\.stage-<v>` e extraido e o diretorio unico e movido para `versions\<v>`.
- Runtime = `runtime\R\bin\Rscript.exe` (layout do M8.2; no Linux e `runtime/bin/Rscript`). Validacao: `Rscript.exe --vanilla -e ...` com
  `prevcarga_runtime_validate(strict = TRUE)` (so aspas simples no codigo R: o PS 5.1 estraga aspas duplas em argumentos nativos).
- Troca de `current`/`previous`/`config.yaml`: tempfile no mesmo diretorio + `[IO.File]::Replace` (ou `Move` se nao existe). Nunca symlink/junction.
- PATH: `HKCU\Environment\Path` (REG_EXPAND_SZ preservado, `DoNotExpandEnvironmentNames`), `<R>\bin` acrescentado uma vez (comparacao sem caixa e sem `\` final),
  `WM_SETTINGCHANGE` avisa a sessao (falha vira aviso). Nunca HKLM. O `-Uninstall` remove a entrada. Em `sh` isso e opt-in (`--add-to-path`); aqui e sempre (AC 6).
- Preflight: SO 64-bit, PowerShell >= 5.1, >= 2 GiB livres no volume de `-Root` (unidade com letra; UNC nao e checada). `-SkipPlatformCheck` vira aviso.
- Saida: bytes ASCII escritos direto no stream (`OpenStandardOutput/Error`), independentes de code page; JSON escapa tudo fora de 32..126 como `\uXXXX`.
- Sucesso em `--format json` acrescenta `install_root`, `retained` e `path_updated` ao `result` (alem de `version/current/previous/state_dir/doctor` do `install.sh`).

## Lancador (`install/prevcarga.cmd`, CRLF, copiado para `<R>\bin`)
Resolve `<R>` por `%~dp0..`, le a 1a linha de `current` por `for /f "usebackq delims="`, confere `versions\<v>\runtime\R\bin\Rscript.exe` e executa
`Rscript.exe --vanilla ...\code\scripts\prevcarga %*`; `& exit /b` sem argumento propaga o `ERRORLEVEL` do R. Sem `current` ou com versao inexistente:
`prevcarga: erro [bundle] (exit 6)` (ASCII, stderr) e `exit /b 6`. Unicas atribuicoes: `setlocal` + 6 limpezas `set "X="` (R_HOME, R_LIBS, R_LIBS_USER, R_LIBS_SITE, R_PROFILE_USER, R_ENVIRON_USER; so no processo; equivalem ao `env -u ...` do Linux, M8.9); qualquer outro `set ` e proibido (teste estatico com CONTROL; a execucao no Windows e do gate windows-vm). **Sem blocos `( ... )`**: um caminho com `(x86)` quebraria o
parser do cmd dentro deles (por isso `goto` em vez do `if (...)` do snippet do ticket). O bundle o carrega em `code\install\prevcarga.cmd` (git archive, M8.2);
bundles antigos usam o que esta ao lado do `install.ps1`.

## Testes
- `install/tests/install.Tests.ps1` (Pester **5.5.0** fixo) + `PSScriptAnalyzerSettings.psd1` (PSScriptAnalyzer **1.22.0** fixo; `PSUseCompatibleSyntax` 5.1/7.0): AC 1-7, com os
  CONTROLs (variante que troca `current` antes de validar; checksum de `state\` acusa 1 arquivo). Roda instalador em processo filho sob `chcp 850`, em `powershell.exe` e `pwsh`;
  salva/restaura o Path de HKCU.
- `tests/install/test-install-e2e-windows.R`: ponte (Pester nos dois shells + analyzer; recibo `install-e2e-windows`); fora do Windows `skip` `nao-windows`.
- `tests/install/test-install-windows-static.R`: checagens estaticas que rodam no Linux (ASCII, sem `$env:`/HKLM, lancador sem `set `, ids no catalogo, ordem hash < `Expand-Archive`, argumentos espelhados).
- **[DEFERRED M8.6 -> Gabriel, host Windows]** (ainda pendente apos o M8.6; ver `docs/estado/sync.md`) TUDO que executa o `.ps1`/`.cmd` (Pester, analyzer, registro HKCU, `Expand-Archive`, `cmd`, MAX_PATH ao extrair `runtime\R\library` sob `%LOCALAPPDATA%`,
  `doctor` no Windows): nao ha host Windows nem `pwsh` nesta maquina de dev; nada foi executado nem simulado.
- Catalogo: os hints de `install.tsv` (usage, no_previous_version, preflight_failed) agora sao neutros de plataforma; `install.sh` e `install.ps1` embutem o MESMO texto (fallback pre-extracao). `install.ps1 -Help` imprime o uso.
- **[DEFERRED M8.6]** tambem: aspas do `-e` no PS 5.1, MAX_PATH e falsos positivos do PSScriptAnalyzer (`PSUseDeclaredVarsMoreThanAssignments` nao habilitada).

## prevcarga-sync (M8.6)
O instalador (sh e ps1) copia `prevcarga-sync` e o `error-catalog.tsv` GERADO para `<R>/bin/`; o uninstall remove `bin/`. Detalhe e protocolo: `docs/estado/sync.md`.

- Decisao (Gabriel, 2026-10-31): os [DEFERRED M8.6] acima ficam para o gate windows-vm do M8 (dono Gabriel); ver `docs/estado/sync.md`.
