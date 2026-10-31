# Estado: prevcarga-sync (.sh/.ps1) e gate do contrato de erros dos utilitarios (M8.6; ADR-0039, ADR-0040, ADR-0030)

## O que e
`install/prevcarga-sync.sh` (POSIX sh, Linux) e `install/prevcarga-sync.ps1` + shim `install/prevcarga-sync.cmd` (Windows nativo, PS 5.1 e 7) sincronizam os
artefatos e as inferencias OFICIAIS do bucket publico para o registry local. **Fora do pacote R**, so LEITURA HTTP(S), sem credencial, sem variavel de ambiente
(o pacote R nunca fala rede). Sao, com `install.sh`/`install.ps1`, os utilitarios que falam rede (o `scripts/tools/upload` e o unico que conhece o servico de storage).
O `install.sh` copia o utilitario para `<R>/bin/prevcarga-sync` e o catalogo GERADO para `<R>/bin/error-catalog.tsv` (origem: `versions/<v>/code/install/`;
bundles antigos: ao lado do `install.sh`); o `install.ps1` faz o mesmo para `<R>\bin\prevcarga-sync.{ps1,cmd}`. O uninstall remove `bin/` inteiro.
`scripts/tools/build_bundle_linux.sh` passou a copiar `install/prevcarga-sync.sh` para `code/install/` (1 linha). Windows: o build do M8.2 nao copia o launcher nem o
sync para `code\install\`; o `install.ps1` cai no arquivo ao lado dele (mesmo fallback do `prevcarga.cmd`).

## Uso
`prevcarga-sync --what artifacts|forecasts [--release-base <url>] [--state <dir>] [--model <m>]... [--area <a>]... [--origin <AAAA-MM-DD>] [--format text|json]`
(ps1: `-What -ReleaseBase -State -Model -Area -Origin -Format`, aceita tambem `--x`). `--state` ausente: `state_dir` de `<R>/config.yaml`, senao `<R>/state`.
`--release-base` default = o do instalador (`https://prevcarga.s3.amazonaws.com`, [TBD: ADR-0039]); aceita http(s) (os testes usam `http://127.0.0.1`).
- **artifacts**: o conjunto oficial vem do CLI instalado ao lado (`prevcarga models --format json` e `areas --format json`; o bucket NUNCA e listado):
  `artifacts/<mdir>/[<exogena>/]<folha>/`, `artifacts/combina/<folha>/`, `artifacts/reconcile/<ss>/`. `--model` filtra os modelos (combina/reconcile sempre entram, como no
  release-pack); `--area` filtra pelo ULTIMO segmento do prefixo (folha ou subsistema). Filtro que nao casa com nada oficial => `sync.usage`. Um prefixo sem `latest.txt` (404)
  nao e erro: entra em `result.ausentes`; nenhum prefixo publicado => `sync.network`.
- **forecasts**: `forecasts/<O>/<available_until>/<versao>/` das chaves listadas em `forecasts/<O>/latest.txt` (uma `<AAAAMMDDTHHMM>/<versao>` por linha). Ver discovery abaixo.

## Protocolo (identico em sh e ps1)
1. Fase 1 (nada publico): por prefixo baixa `latest.txt` (`v_<hash>`), o `SHA256SUMS` da versao e cada arquivo listado para UM staging `.tmp-<rand>` do registry
   (`<state>/artifacts/.tmp-*` ou `<state>/forecasts/.tmp-*`; o `registry gc` do M5 remove `.tmp-*` orfaos), conferindo o sha256 de cada um ANTES do proximo.
   Se `<state>/<chave>/` ja existe: confere cada arquivo e o `SHA256SUMS` local contra o remoto (igual => `pulados`; diferente => `sync.sha_mismatch`, exit 9, sem tocar).
2. Fase 2 (so com TUDO conferido): `rename` de cada `v_<hash>` do staging para o destino e, por ultimo, regrava o `latest.txt` local (tempfile `.tmp-latest-*` + `mv -f`)
   so se mudou. Resultado: qualquer falha da fase 1 (hash, rede) deixa `<state>` byte-identico ao de antes, sem `.tmp-*` e sem `latest.txt` novo; a 2a execucao nao escreve nada.
   (O texto do ticket descrevia rename por prefixo; o AC pede arvore identica mesmo quando o 3o prefixo falha, por isso o commit e em duas fases.)
3. Trap/`finally` remove o staging e os diretorios que esta execucao criou e ficaram vazios.
4. Transporte: `curl -s --proto =https,http -w %{http_code}` (wget como reserva; sem curl nem wget => rede). 200 ok; 404 = ausente; qualquer outra coisa (5xx, conexao cortada, DNS,
   recusada) => `sync.network` (exit 10). Caminho absoluto/`..` num `SHA256SUMS` publicado e recusado (`sync.sha_mismatch`).
5. Envelope (`--format json`, UM documento em stdout, stderr vazio): `{ok, exit_code, verb:"prevcarga-sync", error, warnings, result:{what, prefixos, baixados, pulados, latest_trocados, ausentes}, run_id:null, version}`;
   falha: `{ok:false, exit_code, verb, error:{id, class, message, hint, where, details}}`. Texto: ASCII puro; erro em stderr com 1a linha `prevcarga: erro [<classe>] (exit <n>)`.

## Erros (`scripts/lib/errors/sync.tsv`, catalogo distribuido ADR-0040 item 8; `errors.R` nao foi editado)
`sync.usage` (config, 8), `sync.sha_mismatch` (integrity, 9), `sync.network` (network, 10), `sync.io_failed` (bundle, 6). **Ids sem digito**: o loader rejeita digitos,
entao e `sync.sha_mismatch` (o ticket dizia `sync.sha256_mismatch`; mesma regra do `install.sha_mismatch`). Os utilitarios leem o `error-catalog.tsv` GERADO ao lado pelo NOME das
colunas (`id, class, exit_code, message, hint`) e tem fallback embutido com o MESMO texto do TSV (o teste compara).

## Gate transversal do contrato de erros (AC 6 e 7)
`tests/install/test-utilities-error-contract.R`: matriz 5 utilitarios x {8 argumento, 9 hash, 10 rede} x {json, texto} (install.sh, prevcarga-sync.sh e upload executados aqui;
os dois .ps1 sob pwsh/powershell quando existirem, senao skip explicito); JSON unico com `ok=false`, `exit_code` == saida, `error.id` e classe no catalogo carregado por
`prevcarga_errors_load()`, stderr vazio; texto: 1a linha de stderr e zero byte > 127. Sincronia: catalogo gerado por `export_error_catalog.R --out` == uniao ordenada por id dos
`scripts/lib/errors/*.tsv` (reconstruida de forma independente) == `code/install/error-catalog.tsv` do bundle candidato (skip explicito sem candidato em `dist/`); todo id literal
(`pc_die`/`Write-PcError`) dos 5 utilitarios consta nele; CONTROL: id injetado numa copia de cada utilitario e acusado pelo nome. (O texto do ticket passava o destino como
argumento posicional; o tool recebe `--out <arq>`.)

## Testes
`tests/install/test-sync.R` (so Linux; `helper-sync.R`: bucket montado pelo `prevcarga_release_pack` REAL sobre um registry de fixture com as duas classes + combina + reconcile, servido por
`httpuv` num processo filho de `callr`; modos ok/cut_after/tamper; o CLI ao lado e um stub com o JSON canonico; `env -i`): CONTROL integro (arvore == bucket arquivo a arquivo, inclusive
combina/reconcile), idempotencia, local divergente (9 sem sobrescrever), tamper (9, arvore identica, estado vazio e parcial), tamper so no 2o prefixo, corte (10, n=2 e n=6),
servidor inalcancavel, filtros, forecasts (copia byte a byte, SHA256SUMS, origem invalida, tamper), usage JSON/texto, fallback do catalogo == TSV, forma do JSON REAL do CLI.
`install/tests/prevcarga-sync.Tests.ps1` (Pester 5.5.0; HttpListener local) espelha os bracos.
Pacote instalado so para os testes (R 4.5.3, P3M binario): `httpuv` 1.6.17 (e dependencias ja presentes).

## Pendencias
- [DEFERRED M8.6 -> Gabriel, host Windows (recibo windows-vm)] TUDO que executa os `.ps1`/`.cmd`: Pester (`install.Tests.ps1`, `prevcarga-sync.Tests.ps1`), aspas do `-e` no PS 5.1,
  MAX_PATH ao extrair `runtime\R\library` sob `%LOCALAPPDATA%`, PSScriptAnalyzer 1.22.0, matriz dos .ps1 do contrato de erros, equivalencia sh x ps1 (AC 5; tambem roda no Linux com
  `pwsh` 7), `doctor` no Windows, `cross_os`. Nada foi executado nem simulado: nao ha host Windows nem `pwsh` nesta maquina.
- [DEFERRED M8.6 -> Gabriel, apos rebuild do bundle] o braco "catalogo gerado == `code/install/error-catalog.tsv` do bundle candidato" e o e2e "install.sh instala o sync" precisam de um
  candidato em `dist/` que traga `sync.tsv` e `prevcarga-sync.sh` (o build jammy nao roda aqui).
- **Indice de forecasts**: o `release-pack --what forecasts` ainda nao publica o ponteiro `forecasts/<O>/latest.txt`; o sync o exige (sem listar o bucket nao ha como descobrir
  `available_until`/`versao`). Ver discovery do M8.6. O fixture do teste o escreve a mao.

## Decisoes de fechamento (Gabriel, 2026-10-31)
- Tudo que executa os `.ps1`/`.cmd` (Pester, equivalencia sh x ps1, matriz dos .ps1, e2e do instalador, catalogo do bundle) fica DEFERIDO para o gate **windows-vm do M8** (dono: Gabriel). Os SKIPs explicitos permanecem; nada foi instalado nesta maquina.
- Indice de forecasts: o `release-pack` passara a escrever `forecasts/<O>/latest.txt` como fix de integracao apos o merge do M8.6; o lado do sync ja le esse arquivo.
