# Estado: bundle Linux reprodutivel (M8.1; ADR-0036, ADR-0038)

## O que e
`scripts/tools/build_bundle_linux.sh <v> [--out DIR] [--src REPO] [--rev REV]` (dev, so Linux, fora do bundle) gera
`prevcarga-<v>-linux-x86_64.tar.zst` (+ `.sha256` + `.RUNTIME.json`) em `dist/` (gitignored). **Nada e publicado**: o upload ao
bucket publico e de outro ticket (M8.x). O build roda num container `ubuntu:22.04@sha256:5ec03bb3...` (digest pinado no script),
sem python3, com apt de `https://snapshot.ubuntu.com/ubuntu/20261027T000000Z` (so apos instalar `ca-certificates` do archive padrao:
a imagem base nao traz certificados e o snapshot so atende por https; esse pacote nao entra no bundle), `MAKEFLAGS=-j1`.
R 4.5.3 = `.deb` do Posit (`cdn.posit.co/r/ubuntu-2204`, sha256 pinado no script). O code/ vem de `git archive <rev>` (arvore limpa;
`--src` aponta outro repo/clone, o que serve ao CONTROL "1 linha muda o id"). Arvore suja => recusa.

## Layout do tarball (`prevcarga/`)
`runtime/` (R_HOME de /opt/R/4.5.3 com a library do renv restaurada DIRETO nela, sem `R_LIBS_*`), `syslib/` (fecho `ldd` menos glibc
core), `code/` (`scripts/` sem `tools/`/`data-prep/`/tests/*.md, `models/` sem `models/*/models/`, `config/`, `renv.lock`,
`LICENSE` e `NOTICE` (Apache-2.0, copyright do ONS; obrigatorios: sem eles o `pc_code_tree` aborta),
`install/error-catalog.tsv` GERADO por `export_error_catalog.R --out` e `install/prevcarga` = lancador do M8.3, ver `docs/estado/install.md`), `RUNTIME.json`.
- **Relocavel**: o `bin/R` do Posit tem R_HOME fixo; o build o reescreve para calcular o R_HOME do proprio caminho e troca
  `runtime/bin/R` e `runtime/bin/Rscript` por wrappers sh (o Rscript recebe `RHOME` para nao usar o prefixo compilado).
  Quem chama o Rscript do bundle deve limpar `R_HOME` do ambiente (senao o `bin/R` avisa `ignoring environment value of R_HOME`).
- **Isolado**: `etc/Renviron` do bundle fixa `R_LIBS_USER=` e `R_LIBS_SITE=` vazios (a library do usuario do host vazaria no
  `.libPaths()` e quebraria o determinismo); `.libPaths()` = so `runtime/lib/R/library`.
- **syslib**: acrescentada ao `etc/ldpaths` por caminho relativo (`${R_HOME}/../../../syslib`) logo apos `${R_HOME}/lib` e ANTES do sistema
  do host (as libs empacotadas vem do jammy; exigem glibc >= 2.35 do host). Exclusao glibc core = libc, libm, libpthread, libdl, librt
  (ticket) + libutil, libresolv, libmvec, libanl, libnsl, libBrokenLocale, libnss_*, ld-linux (mesma familia; misturar versoes e perigoso).
- **Python-free**: tres pacotes R trazem artefatos que casam com o gate (`BH/include/boost/python*`, `arrow/demo_flight_server.py`,
  `docopt/spec/docopt.py`, `colorspace/hclwizard/html/python.html`); `prune_python_artifacts` os remove do `runtime/` (log `[prune]` no build).
  Nenhum e usado em execucao.

## RUNTIME.json e runtime_id (fonte unica: `scripts/lib/runtime.R`)
`prevcarga_runtime_json_canonical` (chaves ordenadas recursivamente, `files` por path em radix, sem `built_at`, LF),
`prevcarga_runtime_id` (sha256 da forma canonica; `NULL` sem arquivo = arvore de dev), `prevcarga_runtime_validate(root, strict)`
(recalcula o sha256 de todo `files[]`; `strict` aborta com `bundle.manifest_mismatch`, exit 6), `prevcarga_bundle_require(pkgs)`
(aborta `bundle.package_missing` no primeiro ausente). O sha de um link e o do alvo (o gate `unix_only` de test-static-greps proibe `Sys.readlink` no codigo de produto); o stage nao tem links e a gravacao aborta num link pendente. **Sem relogio**:
`built_at` e lateral (fora do id) e vem do commit (`SOURCE_DATE_EPOCH`), logo dois builds do mesmo commit tem RUNTIME.json IGUAL byte a byte.
Escrita por `scripts/tools/bundle_manifest.R write` (compartilhado com o futuro build Windows); a gravacao aborta se o R do build difere de
`R.Version` do `renv.lock`. `compilados` lista pacotes que o renv compilou do fonte (esperado `[]`: todos vem de binario P3M jammy; um
compilado leva a data de compilacao e quebra a reprodutibilidade, por isso e declarado).
Erros: `scripts/lib/errors/bundle.tsv` (`bundle.manifest_mismatch`, `bundle.package_missing`, `bundle.platform_mismatch`). O id
`bundle.package_missing` morava em `core.tsv` (M1.4b); foi MOVIDO para `bundle.tsv` (o loader rejeita id duplicado).

## renv.lock (R 4.5.3, snapshot P3M 2026-10-27)
75 pacotes = fecho (Depends/Imports/LinkingTo) das raizes de runtime: data.table, lubridate, arrow, jsonlite, digest, yaml, docopt,
RhpcBLASctl, randomForest, forecast, lightgbm, wavelets, zoo, WeightSVM, e1071, rlist, tsibble, fabletools, fable, feasts, urca.
Pacotes recomendados (MASS, Matrix, lattice, nlme, nnet, class...) NAO entram: vem com o R 4.5.3 do bundle. Pacotes so de teste (testthat etc.) e o
proprio renv ficam fora (withr entra, por dependencia de runtime). As versoes sao as do conjunto relock do spike M1.7 (todas conferem
com o indice P3M 2026-10-27). Receita de regeneracao: `installed.packages()` do R 4.5.3 -> fecho -> JSON no formato do lock da referencia
(Package/Version/Source=Repository/Repository=RSPM/Requirements), repo `https://packagemanager.posit.co/cran/2026-10-27`.
`tests/bundle/lock_desvios.yaml` e a lista FECHADA dos 60 pacotes comuns com a referencia (R 4.2.3, 2023-04-21) cuja versao difere, com motivo
(spike M1.7; ADR-0036 cobre WeightSVM/e1071/fable do dessem-regdin). Os engines mantem Delta=0 (RF/LGBM/HW) ou tolerancia ADR-0036 (regdin).
O gate `m5` ficou ESTRITO: sem `renv.lock` ele reprova (o fallback nas bibliotecas instaladas foi removido; CONTROL cobre o lock ausente).

## Gates (tests/bundle/, rodar com `testthat::test_dir("tests/bundle")` sob R 4.5.3)
`test-runtime-id.R` (reprodutibilidade, manifesto, CONTROLs), `test-python-free.R` (+ `pc_bundle_net_grep`: linhas inteiras de comentario nao contam,
pois `deck_reader.R` e `helpers_loess.R` citam `Sys.getenv` para dizer que nao o usam), `test-bundle-linux.R` (conteudo, lock x referencia,
catalogo, `bundle.package_missing`, tarball). Os bracos que dependem de `dist/` ou de dois builds fazem `skip` com motivo (nunca verde silencioso):
`dist/prevcarga-<v>-linux-x86_64.tar.zst` e `dist/repro-1`, `dist/repro-2` (`--out dist/repro-N`, mesmo commit).

## Lock x host (R 4.5.3 da maquina de dev, onde os testes e a paridade rodaram) e validacao DENTRO do bundle
- **Diferencas de versao lock x host: nenhuma** (75/75 pacotes iguais; conferido por programa contra o host e contra a library do bundle extraido).
  Pacotes que importam para a paridade, todos iguais: lightgbm 4.7.0, WeightSVM 1.7-16, e1071 1.7-17, randomForest 4.7-1.2, forecast 9.0.2,
  fable 0.5.0, fabletools 0.8.0, data.table 1.18.6.1, arrow 26.0.0. Os recomendados (MASS, Matrix, lattice, nlme...) tambem coincidem.
  A unica diferenca e o BUILD binario: jammy (glibc 2.35, libstdc++ do jammy no `syslib/`) no bundle x noble no host.
- **Validacao no bundle extraido** (so o R e a library do bundle, `R_HOME` limpo, `.libPaths()` = `runtime/lib/R/library`; testthat e deps
  instalados a parte, binarios jammy, em diretorio de scratch via `R_LIBS`, fora do lock): `prevcarga doctor` 18 PASS + 1 WARN (`data_dir`
  ausente); `scripts/lib/tests` 83 arquivos / 11035 expectativas, 0 falhas (os filhos R dos testes de plugin precisam de `R_LIBS` e `PATH`
  do bundle); `run_all_gates.R --tier fast`: random-forest 25 PASS, holt-winters 44 PASS, lgbm 39 PASS, 0 FAIL. Delta vs host: nenhum.
- Os pacotes de teste do host (compilados em noble) NAO carregam contra o libstdc++ do jammy do bundle; so afeta o harness de teste.

## Release no CI (`.github/workflows/release.yml`)
Uma tag `v*` dispara: `versao` (a tag tem de bater com `config/VERSION`), `linux` (este `build_bundle_linux.sh` num runner `ubuntu-22.04`, que ja traz
Docker; repete ate 3 vezes so quando o log mostra o snapshot.ubuntu.com fora do ar, `50x`/`Unable to locate package`), `windows`
(`docs/estado/m8-bundle-windows.md`) e `publicar`. Cada build instala o proprio bundle numa raiz temporaria e roda `--version` e `doctor`.
`publicar` (so em ref de tag, unico job com `contents: write`) confere os `.sha256` do build, monta o layout de `releases/<v>/` do `release-pack`
(bundles, `RUNTIME-<plataforma>.json`, `SHA256SUMS` em ordem radix) mais `latest.txt` e os dois instaladores, e cria a release `v<v>`
(`--prerelease` se a versao tem `-`) ou substitui os assets de uma release que ja existe. Rodar o workflow a mao numa tag republica; numa branch, so builda e testa.
O CI nao passa pelo `prevcarga release-pack`: os recibos que ele exige (`install-e2e-*`, `windows-vm`, `ab-corte`) nao sao gerados no CI.

## Pendencias
- [DEFERRED M8.8] tiers de paridade (`--tier parity|parity-full`) e A/B completo Delta=0 contra o oraculo DENTRO do bundle: dono M8.8 ("A/B contra prevcargadessem3").
- [DEFERRED M8.6] gate `cross_os` (artefato Linux -> Windows) e braco Windows: o verbo `verificar` (M8.7, `docs/estado/verificar.md`) ja compara entre plataformas; falta a execucao no Windows.
- [DEFERRED M8.x] braco Windows (build, `windows-x86_64`, `bundle.platform_mismatch` no doctor): dono M8.6/M8.7; nao executavel nesta maquina.
- `prevcarga doctor` agora valida o bundle instalado (`prevcarga_runtime_validate`, plataforma, R, artefatos): M8.7, `docs/estado/verificar.md`.
- dessem-linear (max_rel 2.2e-13) segue pendente de decisao do Gabriel (m1-spike-r453.md); nao tratado aqui.

- **Gate Windows (M8.11)**: os itens Windows deste modulo (braco Windows, cross_os) sao EXECUTADOS pelo gate da VM Windows (`tests/windows/Invoke-WindowsGate.ps1`; roteiro `docs/reference/windows-gate.md`; estado `docs/estado/windows-gate.md`, itens W02, W14). Pendente: Gabriel.

## Fechamento do M8 (M8.12; AC 23 e 3 do epico)

### O que o M8 entrega (resumo duravel; detalhe nos estados citados)
- **Bundles R 4.5.3**: `linux-x86_64` (`.tar.zst`, build em container jammy, este arquivo) e `windows-x86_64` (`.zip`, `docs/estado/m8-bundle-windows.md`); `RUNTIME.json` com `runtime_id`.
- **Instaladores** `install.sh` e `install.ps1` (layout `<R>/versions/<v>` + ponteiro `current`, rollback, uninstall; `docs/estado/install.md`); WSL2 nao e requisito.
- **Distribuicao**: `prevcarga release-pack` (monta o layout das tres arvores do bucket publico: `releases/`, `artifacts/`, `forecasts/`; `docs/estado/release-pack.md`) e
  `scripts/tools/upload` (o unico que envia); `prevcarga-sync` (.sh/.ps1/.cmd) leitura do bucket (`docs/estado/sync.md`).
- **Rodada operacional**: `forecast` (uma origem; `--origin` unico flag de data) e `verificar` em dois trilhos + `--oficial` (`docs/estado/verificar.md`).
- **Classes de erro 6/8/9/10/12/13** dos utilitarios e dos verbos: 6 (bundle), 8 (config), 9 (integrity), 10 (network), 12 (repro), 13 (state); catalogo gerado `install/error-catalog.tsv`.
- **NFR de 30 min**: mecanismo e CONTROL no Linux; numeros reais pendentes (`docs/estado/nfr-performance.md`).
- **Oraculo**: `prevcargadessem3` e validado uma vez no A/B do corte e descartado depois (`docs/estado/ab.md`).

### Excecoes conscientes do A/B (lista fechada `tests/ab/ab_excecoes.yaml`, dono Gabriel; ADR-0034 D23)
`regdin.versao_pacotes` (ADR-0036), `dessem_linear.hora_incompleta_clima_previsto` (ADR-0035) e `dessem_linear.sp_2024_11_15_residuo_numerico` (ADR-0035).
Nenhuma entrada nova entra por quem implementa.

### Gates do fechamento (tests/)
- `tests/bundle/test-emendas.R` + `tests/bundle/emendas_m8.txt`: tag git `m8-base` (commit 4989a34, em que a branch do M8 nasceu; tag LOCAL, o merger/Gabriel a publica);
  `git diff --name-status m8-base` sobre `scripts/lib/tests`, `models/*/tests` e `tests` sem D/R e com todo M listado (23 emendas conscientes de testes de outros epicos,
  cada uma com motivo); CONTROL num clone temporario (M fora da lista, D e R acusam o caminho; sem a tag o gate FALHA com motivo).
- `tests/bundle/test-python-free-release.R`: extrai o bundle `linux-x86_64` de `dist/` e roda `pc_bundle_python_free`, `pc_bundle_net_grep`, `pc_bundle_portability_grep`,
  valida o `RUNTIME.json` (`r_version` 4.5.3); CONTROL de injecao (`x.py` e `download.file(`) num stage copiado. Braco `windows-x86_64` = SKIP `nao-windows` com dono (abaixo).
- `tests/bundle/test-docs-writethrough.R`: este arquivo, `README.md` e `docs/reference/MANUAL-USUARIO.md` (verbos citados na lista descoberta de `scripts/lib/cli/*.R`,
  so `--origin` como flag de data, zero variavel de ambiente do produto, WSL2 nunca requisito, todo `id` de `ab_excecoes.yaml`, topicos obrigatorios); CONTROLs plantados.
- `tests/run_suite.R`: a suite inteira pelo `Rscript` do bundle instalado (`scripts/lib/tests`, `tests/bundle`, `tests/install`, `tests/ab`, `models/*/tests/run_all_gates.R --tier fast`
  por membro de `prevcarga_members()`), sai != 0 com falha ou skip fora da lista permitida.
  - Uso: `env -u R_HOME TZ=UTC LC_ALL=C.UTF-8 <R>/versions/<v>/runtime/bin/Rscript --vanilla tests/run_suite.R --lib=<dir com testthat e deps> --list-skips`. Chame o
    WRAPPER `runtime/bin/Rscript` (o binario `runtime/lib/R/bin/Rscript` sozinho cai no R_HOME compilado, o R do sistema).
  - O bundle e zero-dep: o `--lib` vai tambem para o `R_LIBS` do processo de QA, para os filhos Rscript que os testes lancam acharem o testthat (sem isso o
    `m5-facades-gates` caia em `erro [generic]`). Ferramenta de dev, fora do bundle.
  - Sob o bundle, `test-plugins-gates-cli.R` roda so `--tier fast` (`options(prevcarga.closure_tiers = "fast")`): o `golden.lock` pina o BLAS do ambiente de dev
    que gerou o golden e o `plugin_parity.R` recusa outro BLAS (`tools.golden_stale`, item: blas). O tier parity vira skip com dono; a paridade e provada no dev
    (fechamento do M5.12, `docs/estado/m5-training.md`).
  - O runner chama 1 `run_all_gates.R` por plugin (`mdir` unico; as variantes exogenas compartilham o runner).
  - Medido (2026-11-01, bundle jammy `0.12.0` instalado, R 4.5.3, ponta `0f9a3e2`): `tests/bundle` 72/0, `tests/install` 82/0, `tests/ab` 41/0,
    `scripts/lib/tests` 1206 testes, 1 falha (a contagem fixa `n_proc == 10L` do fechamento dos plugins, corrigida no `0f9a3e2`; o arquivo re-rodado pelo bundle deu
    114 ok / 0 falha / 3 skips com dono), `run_all_gates.R --tier fast` dos 5 plugins verdes, 36 skips todos na lista com motivo e dono.

### Pendencias do fechamento (honestas; nada foi simulado)
- **[PENDENTE, dono: Gabriel] Windows nativo**: a suite inteira no Windows com o R do bundle instalado (item W16) e o Python-free/net/portabilidade/RUNTIME.json sobre o
  bundle `windows-x86_64` (item W17) estao PREPARADOS no gate da VM Windows (`tests/windows/Invoke-WindowsGate.ps1`; `docs/reference/windows-gate.md`; `docs/estado/windows-gate.md`),
  nao executados: nao ha host Windows nem `pwsh` aqui (decisao de 2026-10-31).
- **[PENDENTE, dono: Gabriel] Corte do A/B**: o veredito e `CORTE-BLOQUEADO` (matriz completa nao rodada, estagios B-D sem dado real, recibos `ab-corte` ausentes). Sem `CORTE-OK`
  assinado pelo Gabriel nos dois bundles, o roadmap `M8-BUNDLE.md` NAO e removido (AC 3): a copia vive em `docs/_archive/roadmap/M8-BUNDLE.md` (o `docs/roadmap/` ja foi arquivado
  no M1.V); o teste de write-through exige que ela exista enquanto nao houver recibos `ab-corte` `CORTE-OK` de `linux-x86_64` e `windows-x86_64`.
- **[PENDENTE, dono: Gabriel] NFR de 30 min** (numero real Linux e Windows) e o deck real: `docs/estado/nfr-performance.md`.
- Os bracos que dependem de `dist/` (tarball candidato) fazem SKIP com motivo sem ele; `dist/` nunca e versionado.
