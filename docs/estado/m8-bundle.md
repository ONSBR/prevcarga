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
`install/error-catalog.tsv` GERADO por `export_error_catalog.R --out`), `RUNTIME.json`.
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

## Pendencias
- [DEFERRED M8.8] tiers de paridade (`--tier parity|parity-full`) e A/B completo Delta=0 contra o oraculo DENTRO do bundle: dono M8.8 ("A/B contra prevcargadessem3").
- [DEFERRED M8.6/M8.7] gate `cross_os` (artefato Linux -> Windows) e braco Windows.
- [DEFERRED M8.x] braco Windows (build, `windows-x86_64`, `bundle.platform_mismatch` no doctor): dono M8.6/M8.7; nao executavel nesta maquina.
- `prevcarga doctor` ainda nao chama `prevcarga_bundle_require`/`prevcarga_runtime_validate` (introspect.R nao e deste ticket): dono M8.7.
- dessem-linear (max_rel 2.2e-13) segue pendente de decisao do Gabriel (m1-spike-r453.md); nao tratado aqui.
