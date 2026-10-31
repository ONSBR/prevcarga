# Estado: gate da VM Windows (windows-vm) (M8.11; ADR-0036, ADR-0038)

Reune, num so lugar, toda pendencia que ficou **DEFERIDA** por falta de host Windows (decisao do Gabriel, 2026-10-31: nao ha host Windows aqui; o ticket fica
**PRONTO PARA RODAR no Windows**, com roteiro). **Nenhum resultado Windows foi obtido, simulado ou inventado.** Roteiro operacional: `docs/reference/windows-gate.md`.

## O que existe
- `tests/windows/Invoke-WindowsGate.ps1` -- ponto de entrada unico (PS 5.1 e pwsh 7; sem admin; sem R pre-instalado, o R e o do bundle). Parametros: `-Bundle`/`-Sha256` ou
  `-ReleaseBase`/`-Version`, `-RefDir` (artefatos Linux), `-Work`, `-Root`, `-ReceiptsDir`, `-ModulesDir`/`-InstallModules`, `-PwshExe`, `-Build`, `-NfrDeck/-NfrState/-NfrOrigin`, `-AllowSkip`.
  Roda W01..W15 em ordem, loga em `<Work>\logs`, grava os recibos pelo `Rscript.exe` do bundle instalado e sai 0/1/2/3 (PASS / FAIL / INCOMPLETO por SKIP / uso).
- `install/tests/windows-vm.Tests.ps1` (Pester 5.5.0 fixo): W03 (instalacao sem admin, PATH so em HKCU), W04 (doctor x3 shells, zero bytes > 127, CONTROL do acento), W08 (aspas do `-e`),
  W09 (MAX_PATH), W10 (hermeticidade do `prevcarga.cmd`), W11 (matriz dos `.ps1`), W12 (catalogo), W13 (shell sujo: lancador so com as 6 limpezas `set "X="`, sonda sem guard difere,
  setter sem efeito => exit 7 com stdout vazio, `forecast` sujo x limpo byte a byte, CONTROL outro deck). Cada `It` e um passo do recibo (`passo_falho`).
  `install/tests/windows-vm.Helpers.ps1` = funcoes compartilhadas; `install/tests/windows-vm-cross.Tests.ps1` = W14 (cross_os) e W15 (NFR 30 min).
- `tests/windows/write-receipt.R` (roda pelo Rscript.exe do bundle; usa `scripts/tools/receipts.R` + `runtime.R` do code\ instalado; extras: shells, bytes_gt_127, passo_falho...) e
  `tests/windows/control-perturb.R` (CONTROL +0,2% com manifest regerado).
- `scripts/tools/prepare_windows_gate.sh` (Linux): empacota gate + bundle + referencias Linux + catalogo GERADO numa pasta (e `.zip` com `--zip`), com `SHA256SUMS`/`PACOTE.json`; **nao envia nada**.
- Pontes testthat para um R de dev no Windows: `tests/install/test-windows-vm.R` (Pester + recibo `windows-vm`) e `tests/install/test-shell-sujo-windows.R`; fora do Windows `skip` `nao-windows`.
- Testes que rodam NO LINUX: `tests/install/test-windows-gate-static.R` (ASCII, sem variavel de ambiente, cobertura gate x tabela do roteiro com CONTROL, tags dos `It`, parse com `pwsh` so se existir)
  e `tests/install/test-prepare-windows-gate.R` (pacote completo, SHA256SUMS, ASCII, CONTROLs de entrada invalida).

## Itens W01..W15 (origem das pendencias)
W01 preflight (M8.11) · W02 build x2 e runtime_id (M8.2/M8.4/M8.6; `m8-bundle-windows.md`) · W03 install.ps1 em VM limpa (M8.4; `install.md`) · W04 doctor no Windows (M8.7; `verificar.md`) ·
W05 Pester do instalador (M8.4) · W06 Pester do sync e equivalencia sh x ps1 (M8.6; `sync.md`) · W07 PSScriptAnalyzer (M8.4) · W08 aspas do `-e` no PS 5.1 (M8.4/M8.6) · W09 MAX_PATH ·
W10 hermeticidade do `prevcarga.cmd` (M8.9) · W11 matriz dos `.ps1` (M8.6) · W12 catalogo do bundle (M8.6) · W13 shell sujo (M8.3/M8.9/M8.11) ·
W14 cross_os + recibos `cross-platform` (M8.7/M8.9; `m8-e2e-bundle.md`, `verificar.md`) · W15 NFR de 30 min no Windows (M8.10; `nfr-performance.md`).
Os estados `install.md`, `sync.md`, `m8-bundle-windows.md`, `m8-bundle.md`, `m8-e2e-bundle.md`, `nfr-performance.md` e `verificar.md` apontam para este arquivo: **os itens Windows deles sao executados por este gate**.

## Decisoes e desvios (honestos)
- **Lancador sem `set`**: o texto do ticket diz "`prevcarga.cmd` sem `set `", mas o lancador do M8.9 tem 6 limpezas `set "X="` (R_HOME, R_LIBS, R_LIBS_USER, R_LIBS_SITE, R_PROFILE_USER,
  R_ENVIRON_USER; decisao do Gabriel no M8.9, `install.md`). O gate confere a regra vigente: nenhuma outra atribuicao (CONTROL: uma a mais e acusada).
- **CONTROL do acento (AC 1)**: o ticket pede que um acento injetado no catalogo faca o grep de bytes > 127 reprovar. Mas o loader (`errors.R`, `prevcarga_catalog_assert_ascii`) RECUSA um catalogo
  nao-ASCII (`contract.error_catalog_schema`, exit 2), entao a saida do CLI continua ASCII. O CONTROL foi desenhado em tres pernas: o catalogo injetado TEM byte > 127 (o mesmo contador acusa),
  o contador reprova um fluxo com acento, e o CLI recusa o catalogo injetado com saida ainda ASCII (defesa em profundidade). O catalogo real nunca e alterado (copia em stage).
- **Catalogo usado no CONTROL**: o ticket cita `<R>\bin\error-catalog.tsv`; esse arquivo e do `install.ps1`/sync. O CLI le `scripts\lib\errors\*.tsv` do `code\`; o stage copia o `code\` e injeta em `core.tsv`.
- **Receitas sem testthat na VM**: o bundle e zero-dep (sem testthat/processx); por isso o gate e PowerShell + Pester e so chama o `Rscript.exe` do bundle para os recibos e o CONTROL +0,2%.
  `test-cross-platform.R` e `test-nfr-30min.R` ficam como o desenho de referencia (SKIP com dono); a execucao na VM e W14/W15.
- **`forecast` nao tem `--jobs`** (M8.9): "jobs 1 == 4" no Windows = duas rodadas independentes identicas; o `--jobs` do `train-models` fica no braco Linux.
- **Equivalencia sh x ps1 (W06)**: no Windows nao ha `sh`; vale por transitividade (arvore sincronizada == bucket nos dois lados, CONTROL integro). A comparacao direta com `pwsh` no Linux e do
  `test-sync.R` quando houver `pwsh` no host.
- **Recibos**: `windows-vm` so em PASS integral ou FAIL (skip por insumo ausente nao grava); o do lado Linux do `cross-platform` e gerado no Linux (roteiro, secao 6). `ab-corte` e do A/B.
- **Dirty shell no R de dev**: `test-shell-sujo-windows.R` usa `processx` (fora do bundle); a execucao na VM e o `[W13]` do Pester.

## Pendencias
- **[PENDENTE, dono: Gabriel] Executar o roteiro na VM Windows limpa** (`docs/reference/windows-gate.md`) e trazer os recibos para `tests/.receipts/`. Ate la, todo AC que depende de execucao
  Windows (ACs 1, 2 e 3 do ticket, na parte executada) esta PENDENTE por decisao de 2026-10-31; nada foi simulado nesta maquina Linux.
- `pwsh` nao existe aqui e nunca foi instalado: os `.ps1` novos **nao passaram por parse** nem por Pester/PSScriptAnalyzer; so pelos testes estaticos acima. Primeiro erro de sintaxe/Pester na VM
  deve ser corrigido no proprio gate (falha do gate, nao do produto) antes de julgar o produto.
- Linha `windows-x86_64` de `docs/estado/nfr-performance.md` preenchida apos o W15.

## Resultado da execucao (preencher apos a VM)
| data | host/VM | bundle (versao, sha256) | itens PASS | itens FAIL/SKIP | observacoes |
|---|---|---|---|---|---|
| (pendente: Gabriel) | | | | | |
