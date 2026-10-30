# build_bundle_windows.ps1 -- build do bundle windows-x86_64 (M8.2; ADR-0036, ADR-0038). Ferramenta de DEV (fora do bundle).
# Roda no host Windows de build (PowerShell 5.1 ou 7), SEM admin e sem tocar o registro de maquina. Nada e publicado:
# o upload ao bucket publico e de outro ticket. ASCII puro (o PS 5.1 le arquivo sem BOM como ANSI).
#
#   build_bundle_windows.ps1 -Version <v> [-Out dist] [-Rev HEAD] [-SourceDateEpoch <n>]
#
# Saida: <Out>\prevcarga-<v>-windows-x86_64.zip + .sha256 (minusculo, formato sha256sum) + .RUNTIME.json.
# Entradas pinadas abaixo: instalador do R (URL + sha256), snapshot P3M de binarios (o MESMO do renv.lock; sha256 de cada zip em
# scripts\tools\windows_binaries.tsv). O build NUNCA usa [Environment]::SetEnvironmentVariable nem setx.
# [DEFERRED M8.4/M8.6] a execucao deste script so e possivel num host Windows: nao foi executado na maquina Linux de dev.
param(
  [Parameter(Mandatory = $true)][string]$Version,
  [string]$Out = 'dist',
  [string]$Rev = 'HEAD',
  [string]$SourceDateEpoch = ''
)
$ErrorActionPreference = 'Stop'

# --- pinos ------------------------------------------------------------------------------------------------------
$RUrl      = 'https://cloud.r-project.org/bin/windows/base/old/4.5.3/R-4.5.3-win.exe'
$RSha256   = '768ae31bb0b6056def5b1a9789a7dc49306bd037d69b0a99cdd90183aa0c1a31'
$ReposBin  = 'https://packagemanager.posit.co/cran/2026-10-27'
$ReposSrc  = 'https://packagemanager.posit.co/cran/2026-10-27'
$Platform  = 'windows-x86_64'

function Die([string]$cls, [int]$code, [string]$msg) {
  [Console]::Error.WriteLine("prevcarga: erro [$cls] (exit $code)")
  [Console]::Error.WriteLine($msg)
  exit $code
}
function Sha256Of([string]$p) { (Get-FileHash -Algorithm SHA256 -LiteralPath $p).Hash.ToLowerInvariant() }
function Run([string]$exe, [string[]]$argv) {
  & $exe @argv
  if ($LASTEXITCODE -ne 0) { Die 'bundle' 6 "falhou ($LASTEXITCODE): $exe $($argv -join ' ')" }
}

$Src = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$OutDir = if ([System.IO.Path]::IsPathRooted($Out)) { $Out } else { Join-Path (Get-Location).Path $Out }
New-Item -ItemType Directory -Force -Path $OutDir | Out-Null

# --- 1. arvore limpa e commit ------------------------------------------------------------------------------------
$dirty = git -C $Src status --porcelain
if ($dirty) { Die 'bundle' 6 'arvore suja: commite antes de empacotar' }
$Commit = (git -C $Src rev-parse $Rev).Trim()
if (-not $SourceDateEpoch) { $SourceDateEpoch = (git -C $Src log -1 --format=%ct $Rev).Trim() }
$BuiltAt = [DateTimeOffset]::FromUnixTimeSeconds([long]$SourceDateEpoch).UtcDateTime.ToString('yyyy-MM-ddTHH:mm:ssZ')

# --- 2. stage (fora de $Out; o que entra no zip e so $Stage\prevcarga) ----------------------------------------------
$Work  = Join-Path $OutDir ("work-$Version-$Platform")
if (Test-Path -LiteralPath $Work) { Remove-Item -Recurse -Force -LiteralPath $Work }
$Stage = Join-Path $Work 'stage'
$Root  = Join-Path $Stage 'prevcarga'
$Tools = Join-Path $Work 'tools-lib'     # jsonlite, digest, zip: pacotes do BUILD, nunca entram no bundle
$Tmp   = Join-Path $Work 'tmp'
New-Item -ItemType Directory -Force -Path $Root, $Tools, $Tmp | Out-Null

# --- 3. R 4.5.3 (instalador oficial CRAN, sha256 conferido ANTES de executar) -----------------------------------------
$RInstaller = Join-Path $Tmp 'R-4.5.3-win.exe'
Invoke-WebRequest -UseBasicParsing -Uri $RUrl -OutFile $RInstaller
$got = Sha256Of $RInstaller
if ($got -ne $RSha256) { Die 'integrity' 9 "sha256 do instalador do R difere do pinado: $got" }
$RHome = Join-Path $Root 'runtime\R'
$p = Start-Process -FilePath $RInstaller -Wait -PassThru -ArgumentList @(
  '/VERYSILENT', '/SUPPRESSMSGBOXES', '/CURRENTUSER', '/NOICONS', '/MERGETASKS="!recordversion,!associate"',
  "/DIR=$RHome")
if ($p.ExitCode -ne 0) { Die 'bundle' 6 "instalador do R saiu com $($p.ExitCode)" }
# o desinstalador grava o caminho e a data da instalacao: nao e reprodutivel e nao pertence ao bundle
Get-ChildItem -LiteralPath $RHome -Filter 'unins*' -File | Remove-Item -Force
$Rscript = Join-Path $RHome 'bin\Rscript.exe'
if (-not (Test-Path -LiteralPath $Rscript)) { Die 'bundle' 6 'Rscript.exe ausente apos a instalacao do R' }
$v = (& $Rscript --vanilla -e 'cat(as.character(getRversion()))')
if ($v -ne '4.5.3') { Die 'bundle' 6 "R instalado e $v, esperado 4.5.3" }

# Isolamento: a library do usuario do host nao pode vazar para .libPaths() (determinismo). O Renviron do R_HOME e lido mesmo
# com --vanilla; escreve-se por arquivo, nunca por variavel de ambiente da maquina/usuario.
$Renviron = Join-Path $RHome 'etc\Renviron'
$utf8 = New-Object System.Text.UTF8Encoding($false)
$prev = if (Test-Path -LiteralPath $Renviron) { [System.IO.File]::ReadAllText($Renviron) } else { '' }
[System.IO.File]::WriteAllText($Renviron, ($prev.TrimEnd("`r", "`n") + "`nR_LIBS_USER=`nR_LIBS_SITE=`n").TrimStart("`n"), $utf8)

# --- 4. codigo do commit (git archive, LF) -------------------------------------------------------------------------
$ArchSrc = Join-Path $Tmp 'src'
New-Item -ItemType Directory -Force -Path $ArchSrc | Out-Null
$tarball = Join-Path $Tmp 'src.tar'
git -C $Src -c core.autocrlf=false -c core.eol=lf archive --format=tar -o $tarball $Commit
if ($LASTEXITCODE -ne 0) { Die 'bundle' 6 'git archive falhou' }
tar -xf $tarball -C $ArchSrc
if ($LASTEXITCODE -ne 0) { Die 'bundle' 6 'tar -xf do codigo falhou' }

# --- 5. pacotes do build (binarios pinados por sha256) --------------------------------------------------------------
$BM = Join-Path $ArchSrc 'scripts\tools\bundle_manifest.R'
$Hashes = Join-Path $ArchSrc 'scripts\tools\windows_binaries.tsv'
$rows = Import-Csv -Delimiter "`t" -LiteralPath $Hashes
$rv = '4.5'
foreach ($name in @('jsonlite', 'digest', 'zip')) {
  $r = @($rows | Where-Object { $_.pacote -eq $name })
  if ($r.Count -ne 1) { Die 'bundle' 6 "sem linha unica em windows_binaries.tsv para $name" }
  $zf = Join-Path $Tmp ("{0}_{1}.zip" -f $name, $r[0].versao)
  Invoke-WebRequest -UseBasicParsing -Uri ("{0}/bin/windows/contrib/{1}/{2}_{3}.zip" -f $ReposBin, $rv, $name, $r[0].versao) -OutFile $zf
  if ((Sha256Of $zf) -ne $r[0].sha256) { Die 'integrity' 9 "sha256 do binario $name difere do pinado" }
  Add-Type -AssemblyName System.IO.Compression.FileSystem
  [System.IO.Compression.ZipFile]::ExtractToDirectory($zf, $Tools)
}

# --- 6. restaura o renv.lock (binarios CRAN; fonte so com Rtools45, e entra em compilados[]) -----------------------------
$Lib = Join-Path $RHome 'library'
$Comp = Join-Path $Tmp 'compilados.tsv'
Run $Rscript @('--vanilla', $BM, 'restore-windows', '--tools-lib', $Tools, '--lock', (Join-Path $ArchSrc 'renv.lock'),
               '--lib', $Lib, '--repos-bin', $ReposBin, '--repos-src', $ReposSrc, '--hashes', $Hashes, '--out-compilados', $Comp)

# --- 7. code/ + catalogo de erros consolidado + RUNTIME.json --------------------------------------------------------------
$CodeDir = Join-Path $Root 'code'
Run $Rscript @('--vanilla', $BM, 'code-tree', '--src', $ArchSrc, '--dest', $CodeDir)
New-Item -ItemType Directory -Force -Path (Join-Path $CodeDir 'install') | Out-Null
Run $Rscript @('--vanilla', (Join-Path $ArchSrc 'scripts\tools\export_error_catalog.R'), '--out', (Join-Path $CodeDir 'install\error-catalog.tsv'))
Run $Rscript @('--vanilla', $BM, 'write', '--root', $Root, '--version', $Version, '--platform', $Platform,
               '--code-commit', $Commit, '--built-at', $BuiltAt, '--compilados-tsv', $Comp)

# --- 8. zip reprodutivel + sha256 ---------------------------------------------------------------------------------------
$ZipName = "prevcarga-$Version-$Platform.zip"
$ZipPath = Join-Path $OutDir $ZipName
Run $Rscript @('--vanilla', $BM, 'zip', '--tools-lib', $Tools, '--stage', $Stage, '--out', $ZipPath, '--epoch', $SourceDateEpoch)
$sha = Sha256Of $ZipPath
[System.IO.File]::WriteAllText("$ZipPath.sha256", "$sha  $ZipName`n", $utf8)
Copy-Item -LiteralPath (Join-Path $Root 'RUNTIME.json') -Destination (Join-Path $OutDir "prevcarga-$Version-$Platform.RUNTIME.json") -Force

# --- 9. validacao do zip extraido (todo files[] confere) ---------------------------------------------------------------
$Chk = Join-Path $Work 'extraido'
New-Item -ItemType Directory -Force -Path $Chk | Out-Null
[System.IO.Compression.ZipFile]::ExtractToDirectory($ZipPath, $Chk)
Run $Rscript @('--vanilla', $BM, 'validate', '--tools-lib', $Tools, '--root', (Join-Path $Chk 'prevcarga'))

Remove-Item -Recurse -Force -LiteralPath $Work
Write-Output "prevcarga: bundle $ZipPath"
Write-Output "sha256 $sha"
