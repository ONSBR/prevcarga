# Estado: bundle windows-x86_64 (M8.2; ADR-0036, ADR-0038)

## O que e
`scripts/tools/build_bundle_windows.ps1 -Version <v> [-Out dist] [-Rev HEAD] [-SourceDateEpoch <n>]` (dev, host Windows de build, PS 5.1/7,
sem admin, fora do bundle) gera `prevcarga-<v>-windows-x86_64.zip` + `.sha256` (minusculo, formato `sha256sum`) + `.RUNTIME.json` em `dist/`.
**Nada e publicado**. A maquina de dev e Linux: o script **nunca foi executado** [DEFERRED M8.4/M8.6: execucao num host Windows]. O que foi
verificado aqui e tudo o que e portavel: as funcoes de `scripts/tools/bundle_manifest.R` e `bundle_gates.R`, os pinos e os testes `tests/bundle/test-bundle-windows*.R`.

## Entradas pinadas (todas conferidas por sha256 antes de uso; downloads so de cloud.r-project.org e packagemanager.posit.co)
- R 4.5.3: `https://cloud.r-project.org/bin/windows/base/old/4.5.3/R-4.5.3-win.exe` (a URL `base/R-4.5.3-win.exe` e 404: a pagina base so serve a
  versao corrente), sha256 `768ae31bb0b6056def5b1a9789a7dc49306bd037d69b0a99cdd90183aa0c1a31` (90 616 928 bytes; conferido em 2026-10-30).
- Binarios CRAN Windows: snapshot P3M `https://packagemanager.posit.co/cran/2026-10-27` (o MESMO do `renv.lock`), `bin/windows/contrib/4.5/<pkg>_<ver>.zip`.
  **Os 75 pacotes do lock tem binario Windows na versao EXATA do lock** (conferido contra o PACKAGES do snapshot) => `compilados[]` esperado `[]`,
  sem Rtools. `scripts/tools/windows_binaries.tsv` (pacote, versao, grupo, sha256) pina o sha256 de cada zip baixado (75 `lock` + `zip` 3.0.2 `ferramenta`);
  teste `test-bundle-windows.R` exige que cubra exatamente o lock. Pacote sem binario cairia no fonte (so com `pkgbuild::has_build_tools()`) e entraria em `compilados[]`.
- `zip` (3.0.2) e ferramenta do BUILD (empacota o zip sem zip.exe), nao esta no lock nem no bundle; `jsonlite`/`digest`/`zip` vao para um `tools-lib` fora do stage
  (bootstrap antes de restaurar o lock), passado como `--tools-lib` ao `bundle_manifest.R`.

## Fluxo do .ps1
Arvore limpa -> commit/epoch -> R 4.5.3 silencioso em `<stage>\prevcarga\runtime\R` (`/VERYSILENT /CURRENTUSER /NOICONS /MERGETASKS="!recordversion,!associate"`;
remove `unins*`, que carrega caminho/data) -> `etc\Renviron` com `R_LIBS_USER=`/`R_LIBS_SITE=` vazios (por arquivo; nunca `SetEnvironmentVariable`/`setx`/`$env:`) ->
`git archive` (LF) -> `bundle_manifest.R restore-windows` (binarios pinados, confere o `Version` instalado de cada pacote contra o lock) -> `code-tree` ->
`export_error_catalog.R --out code\install\error-catalog.tsv` -> `write` (`platform = windows-x86_64`, `apt_snapshot = null`, `compilados`) -> `zip` reprodutivel
(`pc_zip_reproducible`: paths `/` ordenados, mtime = SOURCE_DATE_EPOCH do commit, `zip::zip(mode = "mirror")`) -> extrai e `validate` (todo `files[]`).
Layout: `prevcarga/{runtime/R (R_HOME), code/, RUNTIME.json}`; library em `runtime/R/library`.

## Funcoes (scripts/tools/bundle_manifest.R, compartilhado com o Linux)
`pc_lock_install_windows(lock, lib, repos_bin, repos_src, hashes, fetch, installer)` (fetch/installer injetaveis: testes no Linux), `pc_zip_reproducible(stage, out, epoch)`,
`pc_compilados_as_list` (data.frame -> lista de objetos; `pc_runtime_json_write` aceita os dois). CLI nova: `restore-windows`, `zip`, `validate`, `--compilados-tsv`, `--tools-lib`.
A versao instalada e lida de `DESCRIPTION` (`packageDescription`), nao de `packageVersion`: este normaliza `2.1-3` para `2.1.3` e mascararia uma troca.
`scripts/tools/bundle_gates.R`: `pc_bundle_portability_grep(code_dir)` (regex do ticket; mesma convencao do net_grep: linha inteira de comentario nao conta) varre
`scripts/prevcarga`, `scripts/**/*.R`, `models/**/*.R`. O `code/` real tem 0 hits.

## Testes (tests/bundle/, sob R 4.5.3; Linux roda o que e portavel, o resto faz skip com motivo)
`test-bundle-windows.R` (restauracao simulada + CONTROLs de versao/sha, pino x lock, esquema Windows x Linux, `bundle.manifest_mismatch` exit 6, runtime_id, zip reprodutivel [precisa do pacote `zip`], arms do zip real de `dist/`),
`test-bundle-windows-hygiene.R` (python_free, net_grep, portability_grep + CONTROL de injecao; higiene do .ps1), `test-bundle-windows-engines.R` (fixture LGBM em texto -> `dist/win-engines-fixture/`, CONTROL do RDS binario = `plugin.artifact_corrupt` integrity exit 9).
Validacao unica fora do repo (nao versionada): stage montado no Linux com os 75 binarios Windows reais + `code/` + RUNTIME.json + zip (sem R.exe real: `Rscript.exe` de brinquedo) passou os arms do zip real (library == lock, validate, sem CR) e a higiene.

## Pendencias / diferencas do texto do ticket
- [DEFERRED M8.4/M8.6] executar o .ps1 num host Windows (flags do instalador `/MERGETASKS`, `etc\Renviron` lido com `--vanilla`, ausencia de DLL fora do bundle), os arms `nao-windows`
  (`lgb.load` pelo `Rscript.exe` do bundle, `forecast --report` com PATH minimo e o CONTROL `bundle.package_missing`), o launcher Windows e o `doctor` no Windows.
- O AC5 cita `prevcarga_plugin_check_artifact` com classe 9; a recusa de um RDS e do `prevcarga_plugin_thaw` (`plugin.artifact_corrupt`, integrity, exit 9): `check_artifact` so confere carimbos (contract, exit 2).
- O AC6 cita `bundle.package_missing` por motor removido: `prevcarga_bundle_require` nao e chamado pelo verbo `forecast` hoje; o PDF e `grDevices::pdf` (base R), sem pacote de motor separado.
- `export_error_catalog.R` aceita `--out <arq>` (o texto do ticket o passava posicional).
- Comparacao de `runtime_id` entre dois builds exige o mesmo host, instalador e snapshot; um build real ainda nao foi comparado.

- **Gate Windows (M8.11)**: os itens Windows deste modulo (build x2/runtime_id, `.ps1` no host Windows, doctor) sao EXECUTADOS pelo gate da VM Windows (`tests/windows/Invoke-WindowsGate.ps1`; roteiro `docs/reference/windows-gate.md`; estado `docs/estado/windows-gate.md`, itens W02-W04). Pendente: Gabriel.

## Build no CI (GitHub Actions)
`.github/workflows/bundle-windows.yml` roda o `build_bundle_windows.ps1` num runner `windows-2022` hospedado pelo GitHub, em toda tag `v*`
ou por `workflow_dispatch` (input `version`, que tem de bater com `config/VERSION`). Depois do build instala o zip com `install/install.ps1 -Bundle`
numa raiz temporaria e roda `prevcarga.cmd --version` e `doctor`. O zip, o `.sha256` e o `.RUNTIME.json` ficam como artefato do run; anexar a
release e passo de quem publica (o token do job e so leitura).
