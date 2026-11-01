# Roteiro de testes: gate da VM Windows (windows-vm)

Referencia operacional (M8.11). Reune, num unico ponto de entrada, tudo que ficou **DEFERIDO** por falta de host Windows
(decisao do Gabriel, 2026-10-31). **Nada deste roteiro foi executado ainda**: nenhum resultado Windows foi simulado nem inventado.
O dono da execucao e o Gabriel (ou o operador que ele designar). Estado e pendencias: `docs/estado/windows-gate.md`.

O gate roda em Windows x86_64 **limpo** (sem R, sem administrador), em Windows PowerShell 5.1 e pwsh 7, e grava os recibos
(`windows-vm`, `install-e2e-windows`, `cross-platform`, `nfr-30min`) no mesmo formato de `scripts/tools/receipts.R`.

## 1. Pre-requisitos

| Pre-requisito | Detalhe |
|---|---|
| VM | Windows 10/11 ou Server x86_64, **restaurada de um snapshot limpo** (reexecutar = restaurar o snapshot). |
| Usuario | Comum (sem o grupo Administrators ativo no token). Nenhum passo pede UAC. |
| R | **Nenhum** instalado: `Get-Command Rscript` e `Get-Command R.exe` devem falhar. O R usado e o do bundle instalado. |
| Windows PowerShell 5.1 | Ja vem no Windows. Execution policy default (Restricted/RemoteSigned): o gate usa `-ExecutionPolicy Bypass` so por processo. |
| pwsh 7 | Zip portatil do PowerShell 7 extraido em pasta do usuario (sem instalador, sem admin); informe `-PwshExe <...\pwsh.exe>` se nao estiver no PATH. |
| Pester 5.5.0 e PSScriptAnalyzer 1.22.0 | Versoes FIXAS. Com internet: `-InstallModules` (instala em `CurrentUser`, sem admin). Offline: copie as pastas `Pester\5.5.0` e `PSScriptAnalyzer\1.22.0` (de um `Save-Module`) para uma pasta e passe `-ModulesDir <pasta>`. |
| Rede | Opcional. Com o pacote offline (secao 2) o gate nao usa a rede; `-InstallModules`, `-ReleaseBase` e `-Build` usam. Nada e enviado a lugar nenhum. |
| Disco | ~2 GB livres (bundle extraido tem centenas de MB; o instalador exige 2 GiB livres). |

## 2. Preparar o pacote (no Linux)

```bash
export PATH=/opt/R/4.5.3/bin:$PATH
scripts/tools/prepare_windows_gate.sh --out /caminho/pacote-windows-gate \
  --bundle dist/prevcarga-<V>-windows-x86_64.zip \
  --deck <deck sintetico ou real> --state <estado sincronizado> --linux-output <entrega do forecast Linux para esse deck/estado/origem> \
  --nfr-deck <deck real com manifest.json> --nfr-state <estado oficial sincronizado> \
  --linux-bundle dist/prevcarga-<V>-linux-x86_64.tar.zst --suite-tree --test-lib <pasta testthat+deps win> --zip
```

- **Nao envia nada**: so copia para a pasta (e gera `<pasta>.zip` com `--zip`). Copie o zip para a VM por pasta compartilhada, pendrive ou `scp`; nao use o bucket publico.
- **Artefatos de referencia Linux** (`ref\`): `--linux-output` e a entrega (`manifest.json` + `run_manifest.json` + parquets) de `prevcarga forecast --origin <O>` rodado no Linux com o
  **mesmo** `--deck`/`--state` (copiados para `ref\deck` e `ref\state`). A origem sai do `run_manifest.json`. Sem eles os itens W13 (braco do forecast) e W14 ficam SKIP.
- `--nfr-deck/--nfr-state` (deck REAL de um dia util com `manifest.json` + artefatos oficiais sincronizados pelo `prevcarga-sync`) alimentam o W15. Sem eles: SKIP `deck-real-ausente`.
- `--suite-tree` (W16) copia para `repo\` a arvore versionada do HEAD (`git archive`), e `--test-lib <dir>` copia para `testlib\` a pasta com testthat e dependencias (binarios Windows do R 4.5.3 baixados do P3M; o pacote nao os traz). Sem eles o W16 fica SKIP (INCOMPLETO).
- `--linux-bundle` so grava o sha256 do bundle Linux (o recibo `cross-platform` do lado Linux e gerado no Linux; secao 6).
- O pacote traz `SHA256SUMS` e `PACOTE.json`. Na VM, descompacte e confira (opcional): `Get-Content SHA256SUMS | ForEach-Object { ... }` ou `Get-FileHash -Algorithm SHA256 <arquivo>`.

## 3. Rodar o gate (na VM)

```powershell
cd <pasta do pacote>
powershell -NoProfile -ExecutionPolicy Bypass -File gate\tests\windows\Invoke-WindowsGate.ps1 `
  -Bundle bundle\prevcarga-<V>-windows-x86_64.zip -Sha256 <sha256 do zip> -RefDir ref `
  -NfrDeck nfr\deck -NfrState nfr\state -NfrOrigin <AAAA-MM-DD> -SuiteRepo repo -TestLib testlib `
  -ModulesDir C:\modulos -PwshExe C:\pwsh7\pwsh.exe -Work C:\gate-work -ReceiptsDir C:\gate-work\receipts
```

Parametros: `-Bundle`/`-Sha256` (o hash e **obrigatorio** e conferido antes de qualquer instalacao) **ou** `-ReleaseBase <url> [-Version <v>|latest]` (baixa o zip e confere contra o `SHA256SUMS` do release);
`-RefDir` (referencias Linux); `-Work` (logs e temporarios); `-Root` (raiz da instalacao; default `%LOCALAPPDATA%\prevcarga`, o caso real do W09);
`-SuiteRepo` (arvore completa do repo, pasta `repo\` do pacote) e `-TestLib` (pasta com testthat e dependencias, binarios Windows do R 4.5.3; o bundle e zero-dep) para o W16; `-ReceiptsDir` (recibos); `-ModulesDir`/`-InstallModules`; `-PwshExe`; `-Build -BuildVersion <v>` (W02, so em host de build com checkout git e internet); `-AllowSkip`.

Cada item roda em ordem, com log em `<Work>\logs\`. O sumario (ASCII) lista `PASS|FAIL|SKIP` por item e o resultado fica em `<Work>\gate-result.json`.
**Exit**: `0` todos PASS; `1` algum FAIL; `2` sem FAIL mas com SKIP (INCOMPLETO, a menos que `-AllowSkip`); `3` erro de uso ou pre-condicao (por exemplo sha256 divergente).

Reexecucao (idempotente): restaure o snapshot limpo e rode o mesmo comando. Reinstalar sobre a instalacao anterior tambem funciona (o instalador e idempotente), mas so o snapshot garante "VM limpa".

Execucao parcial (so para diagnostico; um recibo so e gravado com PASS integral da suite ou FAIL): rode a suite sozinha, por exemplo
`New-PesterContainer -Path gate\install\tests\windows-vm.Tests.ps1 -Data @{ Bundle=...; Sha256=...; Root=...; Work=... }` + `Invoke-Pester -Container $c` (Pester 5.5.0 importado).

## 4. Tabela de conferencia (um item por pendencia diferida)

Marque cada linha apos a execucao. "Origem" e o ticket que deferiu o item.

| Item | Pendencia | Origem | Como (dentro do gate) | Resultado esperado |
|---|---|---|---|---|
| W01 | VM limpa: sem R, sem admin, PS 5.1 e pwsh 7, policy | M8.11 | preflight do `Invoke-WindowsGate.ps1` | PASS: token sem `S-1-5-32-544`, sem `Rscript`/`R.exe`, pwsh presente |
| W02 | Build do bundle windows (`build_bundle_windows.ps1`) e `runtime_id` igual em 2 builds | M8.2, M8.4/M8.6 | `-Build -BuildVersion <v>` (2 builds, `-SourceDateEpoch` fixo) | PASS: `.RUNTIME.json` e zip com o mesmo sha256 nos dois builds |
| W03 | `install.ps1` em VM limpa, sem admin, `-ExecutionPolicy Bypass`; PATH so em HKCU | M8.4, M8.11 | `windows-vm.Tests.ps1` `[W03]` | PASS: exit 0; `<R>\bin` em `HKCU:\Environment\Path`; ausente do PATH de maquina (HKLM); sem elevacao |
| W04 | `prevcarga doctor` pelo cmd, PS 5.1 e pwsh 7: exit 0, zero bytes > 127; CONTROL do acento | M8.4, M8.7, M8.11 | `windows-vm.Tests.ps1` `[W04]` | PASS: tres exit 0; `bytes_gt_127 = 0`; CONTROL: o contador acusa o acento injetado e o CLI recusa o catalogo nao-ASCII (exit 2) |
| W05 | Pester do instalador/lancador (`install.Tests.ps1`) | M8.4/M8.6 | suite existente, em `powershell.exe` e `pwsh` | PASS nos dois shells (0 falhas, 0 skip) |
| W06 | `prevcarga-sync.ps1/.cmd` (`prevcarga-sync.Tests.ps1`) e equivalencia sh x ps1 | M8.6 | suite existente, nos dois shells | PASS; a equivalencia vale por transitividade: arvore sincronizada == bucket (CONTROL integro) nos dois lados |
| W07 | PSScriptAnalyzer 1.22.0 (falsos positivos) | M8.4/M8.6 | `Invoke-ScriptAnalyzer` com `install\tests\PSScriptAnalyzerSettings.psd1` | PASS: nenhum Error/Warning em `install.ps1` |
| W08 | Aspas do `-e` do R no PS 5.1 | M8.4/M8.6 | `windows-vm.Tests.ps1` `[W08]` | PASS: `ab` nos dois shells e via `ProcessStartInfo` com `\"` |
| W09 | MAX_PATH em `%LOCALAPPDATA%` (`runtime\R\library`) | M8.4/M8.6 | `windows-vm.Tests.ps1` `[W09]` | PASS: maior caminho < 260 (o valor sai em `max_path` dos fatos) |
| W10 | Hermeticidade do `prevcarga.cmd` (R_LIBS*, R_PROFILE_USER, R_ENVIRON_USER) | M8.9 | `windows-vm.Tests.ps1` `[W10]` | PASS: com library sombra no ambiente a saida == shell limpo; CONTROL: sem as limpezas carrega a sombra e cai |
| W11 | Matriz dos `.ps1` do contrato de erros | M8.6 | `windows-vm.Tests.ps1` `[W11]` | PASS: `--bogus` => exit 8, JSON com `error.id`, texto `prevcarga: erro [`, ASCII, nos dois shells |
| W12 | Catalogo do bundle == `<R>\bin\error-catalog.tsv` | M8.6 | `windows-vm.Tests.ps1` `[W12]` | PASS: bytes iguais e ASCII |
| W13 | Shell sujo no Windows (TZ, locale, `OPENBLAS_NUM_THREADS=4`, `.Rprofile` que muda TZ) | M8.3/M8.9, M8.11 | `windows-vm.Tests.ps1` `[W13]` | PASS: `prevcarga.cmd` so com as 6 limpezas `set "X="`; `forecast` byte-identico sujo x limpo; CONTROLs: sonda sem guard difere, setter sem efeito => exit 7 com stdout vazio, outro deck => outra entrega |
| W14 | `cross_os`: entrega Linux -> Windows, `verificar` entre plataformas, recibos | M8.7, M8.9 | `windows-vm-cross.Tests.ps1` `[W14]` | PASS: 2 rodadas identicas; `verificar --trilho=entre-plataformas` exit 0 (delta <= 0,001 do valor de referencia); CONTROL +0,2% => exit 12; recibo `cross-platform` |
| W15 | Braco Windows do NFR de 30 min | M8.10 | `windows-vm-cross.Tests.ps1` `[W15]` | PASS: `duracao_s <= 1800` registrada pela rodada, sem passo `score`; recibo `nfr-30min` |
| W16 | Suite inteira no Windows nativo com o R do bundle instalado (`tests/run_suite.R`: testthat de `scripts/lib/tests`, `tests/bundle`, `tests/install`, `tests/ab` + `run_all_gates.R --tier fast` de cada membro) | M8.12 | `-SuiteRepo repo -TestLib testlib` (a arvore completa do repo e uma pasta com testthat e dependencias, binarios Windows do R 4.5.3, via `prepare_windows_gate.sh --suite-tree --test-lib <dir>`); `Rscript.exe` do bundle instalado roda `run_suite.R --list-skips` | PASS: exit 0, zero falha e zero skip fora da lista (`nao-windows`, `nao-linux`, `deck-real-ausente`, `[TBD: ADR-0036]`, `[DEFERRED windows-vm do M8`); o skip de git ausente do gate de emendas e esperado (o gate de emendas e do host de dev) |
| W17 | Python-free, grep de rede/env e portabilidade e `RUNTIME.json` sobre o bundle `windows-x86_64` (a prova do `test-python-free-release.R`, braco Windows) | M8.12 | `tests\windows\release-check.R` pelo `Rscript.exe` do bundle instalado (so R base + jsonlite do bundle) | PASS: zero arquivos Python, zero hits de `Sys.getenv`/rede/portabilidade no `code\`, `r_version` 4.5.3, `platform` windows-x86_64, todo `files[]` do `RUNTIME.json` confere; o `code_commit` deve ser o do bundle Linux (`RUNTIME-<plataforma>.json` do release) |

Cobertura: o gate acusa **INCOMPLETO** (exit 2) enquanto houver SKIP; um item so conta como cumprido com PASS.

## 5. Ler os recibos

Cada recibo e `tests\.receipts\<sha256 do zip>.<gate>.json` (ou `-ReceiptsDir`), no formato de `scripts/tools/receipts.R`:

| Recibo | Gravado quando | Campos extras |
|---|---|---|
| `windows-vm` | PASS integral da suite `windows-vm.Tests.ps1`, ou FAIL em qualquer passo | `shells {cmd, ps51, ps7}` (exit codes do doctor), `bytes_gt_127`, `passo_falho` (nome do `It` que falhou ou `null`) |
| `install-e2e-windows` | W05 e W07 definitivos (PASS ou FAIL) | `pester_install`, `analyzer` |
| `cross-platform` (lado Windows) | W14 PASS (ou FAIL) | `referencia_platform`, `jobs_equal`, `verificar_exit`, `control_exit` |
| `nfr-30min` | W15 executado | `duracao_s`, `passos[]`, `jobs`, `passo_mais_lento` |

Campos comuns: `gate`, `sha256`, `platform: windows-x86_64`, `runtime_id`, `resultado` (PASS|FAIL), `veredito` (null), `data` (UTC). Skip (insumo ausente) **nao** grava recibo.
O `ab-corte` e produzido pelo A/B no Linux (`ab_report.R`) e nao faz parte deste gate.

## 6. Trazer os recibos para o repo Linux

1. Copie `<ReceiptsDir>\*.json` e `<Work>\gate-result.json` + `<Work>\logs\` da VM para o Linux (pendrive/pasta compartilhada).
2. Coloque os recibos em `tests/.receipts/` do repo (gitignored; consumidos pelo `release-pack --what runtime` e pelo `ab_report.R`). Confira que o nome e `<sha256>.<gate>.json` e que o `sha256` interno bate com o zip: `sha256sum dist/prevcarga-<V>-windows-x86_64.zip`.
3. O recibo `cross-platform` exige um por bundle: o do Linux sai no Linux, a partir do resultado Windows PASS, com o mesmo extras:
   ```bash
   export PATH=/opt/R/4.5.3/bin:$PATH
   Rscript --vanilla tests/windows/write-receipt.R --repo=$PWD --gate=cross-platform --bundle=dist/prevcarga-<V>-linux-x86_64.tar.zst \
     --platform=linux-x86_64 --resultado=PASS --runtime-json=<RUNTIME.json do bundle Linux extraido> --dir=tests/.receipts
   ```
   (so depois de ler `resultado: PASS` do recibo Windows). O `ab-corte` do Windows e gerado pelo A/B.
4. Atualize `docs/estado/windows-gate.md` (secao "Resultado da execucao": data, host, versao do bundle, itens PASS/FAIL, valores de `max_path` e `duracao_s`; para o NFR, a linha `windows-x86_64` de `docs/estado/nfr-performance.md`).

## 7. O que fazer em cada falha

| Item | Falhou | Faca |
|---|---|---|
| W01 | admin/R/pwsh | Use usuario comum, VM do snapshot limpo, instale o pwsh portatil; nao instale R. |
| W02 | build/`runtime_id` | Veja `logs\build-N.log`; diferenca entre builds = nao-reprodutibilidade (ordem do zip, timestamps, versao de pacote): abra discovery. |
| W03 | instalador | `logs\pester-windows-vm.log`; confira o sha256, a rede/AV (Defender pode bloquear a extracao), espaco em disco; `install.ps1 -Format json` mostra o `error.id`. |
| W04 | doctor | Exit != 0: leia o `error.id` no log (catalogo ADR-0040); byte > 127: bug do catalogo ou de saida (stdout/stderr devem ser ASCII). |
| W05/W06 | Pester | O nome do teste e o log (`pester-install-*.log`, `pester-sync-*.log`) apontam o braco. Porta ocupada no sync: feche o que usa 127.0.0.1 e repita. |
| W07 | analyzer | Regra e linha no log. Falso positivo conhecido: ajuste o settings (`PSScriptAnalyzerSettings.psd1`) e registre em `docs/estado/install.md`. |
| W08 | aspas | O 5.1 remove aspas duplas de argumento nativo: o produto deve usar aspas simples dentro do codigo R ou `\"` via `ProcessStartInfo`. |
| W09 | MAX_PATH | `max_path` e o pior caminho no log; mova `-Root` para uma pasta mais curta e abra discovery (encurtar `runtime\R\library` ou habilitar caminhos longos e documentar). |
| W10 | hermeticidade | Se a library sombra nao instalar (`R CMD INSTALL`), o item vira SKIP: instale a sombra a mao e repita. FAIL = o `prevcarga.cmd` deixou passar `R_LIBS*`. |
| W11/W12 | contrato de erros/catalogo | Gere de novo o catalogo no Linux (`export_error_catalog.R`) e o pacote; compare `<R>\bin\error-catalog.tsv` com `code\install\`. |
| W13 | shell sujo | Diferenca de byte: compare `state-sujo`/`state-limpo` em `<Work>\vm`; exit 7 = o runtime nao conseguiu fixar TZ/locale/BLAS (e o comportamento correto do CONTROL). |
| W14 | cross_os | exit 12 do `verificar` fora da tolerancia = divergencia numerica real entre plataformas: NAO relaxe a tolerancia (ADR-0036); abra discovery com o `result.arquivos[]`. exit 9/2 = entrega/identidade (confira que `ref\` e o mesmo deck/estado/origem). |
| W15 | NFR | Estourou 1800 s: o passo mais lento esta no recibo; abra discovery de desempenho (nao mude o orcamento). |
| W16 | suite | `logs\W16.log` traz as falhas e os skips fora da lista: corrija o produto ou o teste; nao amplie a lista de skips permitidos sem decisao do Gabriel. Falha so no Windows vira discovery. |
| W17 | python-free | `logs\W17.log`: hit de Python, rede ou portabilidade no `code\` do bundle, ou `RUNTIME.json` divergente; refaca o build (W02). |

Qualquer FAIL deve virar uma **discovery** no SpecForge (tipo `ticket`), com a observacao (log, passo) e a avaliacao; nao ajuste o gate para passar.

## 8. Itens que este gate NAO cobre

- Publicacao/upload (outro ticket; nunca feito aqui).
- A/B Delta=0 completo dentro do bundle (M8.8) e `ab-corte`.
- Equivalencia direta sh x ps1 com `pwsh` no Linux: `tests/install/test-sync.R` ja a executa sozinho quando `pwsh` existir no host.
