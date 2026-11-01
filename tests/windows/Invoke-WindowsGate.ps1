# Invoke-WindowsGate.ps1 -- ponto de entrada UNICO do gate da VM Windows (M8.11; ADR-0036, ADR-0038, ADR-0039, ADR-0040).
# Roteiro em docs/reference/windows-gate.md (tabela de itens W01..W15). Sem administrador, sem R pre-instalado (o R e o do bundle),
# Windows PowerShell 5.1 e pwsh 7. Nao faz upload e nao escreve fora de -Work, -Root e -ReceiptsDir. ASCII puro; nenhuma variavel de ambiente.
# [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] NAO foi executado nesta maquina de dev (sem host Windows nem pwsh).
#
#   powershell -NoProfile -ExecutionPolicy Bypass -File tests\windows\Invoke-WindowsGate.ps1 `
#       -Bundle <zip> -Sha256 <h> -RefDir <pasta ref\ do pacote> [-Work <dir>] [-ReceiptsDir <dir>]
#   ... -ReleaseBase <url> [-Version <v>|latest]      (baixa o zip e confere o sha256 contra o SHA256SUMS do release)
#
# Saida: tabela ASCII (item, status, detalhe), logs em <Work>\logs\, recibos em <ReceiptsDir>, resultado em <Work>\gate-result.json.
# Exit: 0 = todos PASS (SKIP so com -AllowSkip); 1 = algum FAIL; 2 = nenhum FAIL mas ha SKIP (INCOMPLETO); 3 = erro de uso/pre-condicao.
param(
  [string]$Bundle = '', [string]$Sha256 = '', [string]$ReleaseBase = '', [string]$Version = '',
  [string]$RefDir = '', [string]$Origin = '',
  [string]$Work = '', [string]$Root = '', [string]$ReceiptsDir = '',
  [string]$ModulesDir = '', [switch]$InstallModules, [string]$PwshExe = '',
  [switch]$Build, [string]$BuildVersion = '',
  [string]$NfrDeck = '', [string]$NfrState = '', [string]$NfrOrigin = '',
  [string]$SuiteRepo = '', [string]$TestLib = '',
  [switch]$AllowSkip
)
Set-StrictMode -Version 3
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$script:Repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$script:Results = New-Object System.Collections.ArrayList
$script:PesterVersion = '5.5.0'
$script:AnalyzerVersion = '1.22.0'

# --- catalogo dos itens (a tabela do runbook tem uma linha por id) ---------------------------------------------------
$script:Items = [ordered]@{
  'W01' = 'preflight: VM limpa (sem R), sem admin, PS 5.1 e pwsh 7, execution policy'
  'W02' = 'build do bundle windows (build_bundle_windows.ps1) x2: mesmo runtime_id'
  'W03' = 'install.ps1 na VM limpa (sem R, sem admin; -ExecutionPolicy Bypass; PATH so em HKCU)'
  'W04' = 'prevcarga doctor exit 0 e ASCII no cmd, PS 5.1 e pwsh 7; CONTROL do acento'
  'W05' = 'Pester install.Tests.ps1 (instalador e lancador) em PS 5.1 e pwsh 7'
  'W06' = 'Pester prevcarga-sync.Tests.ps1 (prevcarga-sync.ps1/.cmd; equivalencia sh x ps1 por igualdade com o bucket)'
  'W07' = 'PSScriptAnalyzer 1.22.0 sobre install.ps1 (settings fixo)'
  'W08' = 'aspas do -e do R no PS 5.1 e no pwsh 7'
  'W09' = 'MAX_PATH: instalacao sob %LOCALAPPDATA% (runtime\R\library)'
  'W10' = 'hermeticidade do prevcarga.cmd (R_LIBS*, R_PROFILE_USER, R_ENVIRON_USER)'
  'W11' = 'matriz dos .ps1 do contrato de erros (exit 8, json e texto, ASCII)'
  'W12' = 'catalogo do bundle: <R>\bin\error-catalog.tsv == code\install\error-catalog.tsv'
  'W13' = 'shell sujo no Windows (TZ, locale, OPENBLAS, .Rprofile): forecast byte-identico e prevcarga.cmd sem set'
  'W14' = 'cross_os: entrega Linux x Windows (verificar entre-plataformas), CONTROL +0,2% => exit 12, recibo cross-platform'
  'W15' = 'NFR de 30 min no Windows (forecast <= 1800 s) e recibo nfr-30min'
  'W16' = 'suite inteira (tests/run_suite.R) pelo Rscript.exe do bundle instalado: testthat + run_all_gates --tier fast, sem skip fora da lista'
  'W17' = 'Python-free, grep de rede/env e portabilidade e RUNTIME.json sobre o bundle windows-x86_64 instalado (release-check.R)'
}

function Add-Result([string]$Id, [string]$Status, [string]$Detail) {
  [void]$script:Results.Add([pscustomobject]@{ Id = $Id; Item = $script:Items[$Id]; Status = $Status; Detail = $Detail })
  Write-Host ('[' + $Status + '] ' + $Id + ' ' + $Detail)
}
function Write-Log([string]$Name, [string]$Text) {
  [IO.File]::AppendAllText((Join-Path $script:Logs ($Name + '.log')), ($Text + "`r`n"), [Text.Encoding]::ASCII)
}
function ConvertTo-AsciiText([string]$Text) {
  $sb = New-Object System.Text.StringBuilder
  foreach ($c in $Text.ToCharArray()) { $n = [int]$c; if ($n -eq 10 -or $n -eq 13 -or ($n -ge 32 -and $n -le 126)) { [void]$sb.Append($c) } else { [void]$sb.Append('?') } }
  return $sb.ToString()
}
function Invoke-Proc([string]$Exe, [string]$ArgLine, [string]$LogName) {
  $psi = New-Object Diagnostics.ProcessStartInfo
  $psi.FileName = $Exe; $psi.Arguments = $ArgLine
  $psi.RedirectStandardOutput = $true; $psi.RedirectStandardError = $true; $psi.UseShellExecute = $false; $psi.CreateNoWindow = $true
  $p = New-Object Diagnostics.Process
  $p.StartInfo = $psi
  [void]$p.Start()
  $t1 = $p.StandardOutput.ReadToEndAsync(); $t2 = $p.StandardError.ReadToEndAsync()
  $p.WaitForExit()
  $o = $t1.Result; $e = $t2.Result
  $code = [int]$p.ExitCode
  $p.Dispose()
  if ($LogName) { Write-Log $LogName ('$ ' + $Exe + ' ' + $ArgLine + "`r`n--- stdout`r`n" + (ConvertTo-AsciiText $o) + "`r`n--- stderr`r`n" + (ConvertTo-AsciiText $e) + "`r`n--- exit " + $code) }
  return @{ Exit = $code; Out = $o; Err = $e }
}
function Get-Sha([string]$Path) { return (Get-FileHash -Algorithm SHA256 -LiteralPath $Path).Hash.ToLowerInvariant() }
function Write-Json([string]$Path, $Obj) { [IO.File]::WriteAllText($Path, (ConvertTo-Json -InputObject $Obj -Depth 8), (New-Object Text.UTF8Encoding $false)) }
function Read-Json([string]$Path) { return ([IO.File]::ReadAllText($Path) | ConvertFrom-Json) }

# --- Pester num processo filho (shell escolhido); devolve a lista de testes e contagens ------------------------------
function Write-PesterRunner([string]$Path) {
  $lines = @(
    'param([string]$Tests, [string]$DataFile, [string]$OutFile, [string]$ModulesDir, [switch]$InstallModules)',
    '$ErrorActionPreference = ''Stop''',
    'function Import-Pinned([string]$Name, [string]$Ver) {',
    '  $cand = $null',
    '  if ($ModulesDir) { $cand = Join-Path $ModulesDir ($Name + ''\'' + $Ver + ''\'' + $Name + ''.psd1''); if (-not (Test-Path -LiteralPath $cand)) { $cand = $null } }',
    '  if ($cand) { Import-Module $cand -Force; return }',
    '  if (-not (Get-Module -ListAvailable -Name $Name | Where-Object { $_.Version -eq [version]$Ver })) {',
    '    if (-not $InstallModules) { throw ($Name + '' '' + $Ver + '' ausente: use -ModulesDir ou -InstallModules'') }',
    '    Install-Module -Name $Name -RequiredVersion $Ver -Scope CurrentUser -Force -SkipPublisherCheck -AllowClobber',
    '  }',
    '  Import-Module $Name -RequiredVersion $Ver -Force',
    '}',
    'Import-Pinned ''Pester'' ''5.5.0''',
    '$d = [IO.File]::ReadAllText($DataFile) | ConvertFrom-Json',
    '$h = @{}; foreach ($pr in $d.PSObject.Properties) { $h[$pr.Name] = [string]$pr.Value }',
    '$c = New-PesterContainer -Path $Tests -Data $h',
    '$r = Invoke-Pester -Container $c -PassThru -Output Minimal',
    '$t = @(); foreach ($x in @($r.Tests)) { $t += @{ name = [string]$x.ExpandedName; result = [string]$x.Result } }',
    '$o = @{ failed = [int]$r.FailedCount; skipped = [int]$r.SkippedCount; notrun = [int]$r.NotRunCount; passed = [int]$r.PassedCount; tests = $t }',
    '[IO.File]::WriteAllText($OutFile, (ConvertTo-Json -InputObject $o -Depth 5), (New-Object Text.UTF8Encoding $false))',
    'if ($r.FailedCount -gt 0) { exit 1 }',
    'exit 0')
  [IO.File]::WriteAllText($Path, ($lines -join "`r`n") + "`r`n", [Text.Encoding]::ASCII)
}
function Invoke-PesterSuite([string]$ShellExe, [string]$Tests, [hashtable]$Data, [string]$Tag) {
  $df = Join-Path $script:Work ('data-' + $Tag + '.json'); $of = Join-Path $script:Work ('pester-' + $Tag + '.json')
  if (Test-Path -LiteralPath $of) { Remove-Item -LiteralPath $of -Force }
  Write-Json $df $Data
  $a = '-NoProfile -ExecutionPolicy Bypass -File "' + $script:Runner + '" -Tests "' + $Tests + '" -DataFile "' + $df + '" -OutFile "' + $of + '"'
  if ($ModulesDir) { $a += ' -ModulesDir "' + $ModulesDir + '"' }
  if ($InstallModules) { $a += ' -InstallModules' }
  $r = Invoke-Proc $ShellExe $a ('pester-' + $Tag)
  if (-not (Test-Path -LiteralPath $of)) { return @{ ok = $false; failed = 1; skipped = 0; notrun = 0; tests = @(); error = ('runner sem resultado (exit ' + $r.Exit + '): ' + (ConvertTo-AsciiText ($r.Err.Trim()))) } }
  $j = Read-Json $of
  return @{ ok = $true; failed = [int]$j.failed; skipped = [int]$j.skipped; notrun = [int]$j.notrun; tests = @($j.tests); error = '' }
}
# status de um item a partir dos Its [Wnn] de uma execucao
function Get-ItemStatus($Run, [string]$Id) {
  if (-not $Run.ok) { return @('FAIL', $Run.error) }
  $mine = @($Run.tests | Where-Object { $_.name -like ('`[' + $Id + '`]*') })
  if ($mine.Count -eq 0) {
    if ($Run.failed -gt 0) { return @('FAIL', 'container Pester falhou antes dos Its (ver o log pester-*.log)') }
    return @('SKIP', 'nenhum It do item executado')
  }
  $bad = @($mine | Where-Object { $_.result -eq 'Failed' })
  if ($bad.Count -gt 0) { return @('FAIL', ('falhou: ' + (ConvertTo-AsciiText $bad[0].name))) }
  $sk = @($mine | Where-Object { $_.result -ne 'Passed' })
  if ($sk.Count -gt 0) { return @('SKIP', ('nao executado: ' + (ConvertTo-AsciiText $sk[0].name) + ' (insumo ausente ou skip explicito; ver o log)')) }
  return @('PASS', ($mine.Count.ToString() + ' It(s)'))
}
function Add-ItemFromRun($Run, [string]$Id) {
  $s = Get-ItemStatus $Run $Id
  Add-Result $Id $s[0] $s[1]
}

# --- recibos pelo Rscript.exe do bundle instalado -----------------------------------------------------------------
function Get-InstalledVersion { return ([IO.File]::ReadAllText((Join-Path $script:Root 'current')).Trim()) }
function Write-GateReceipt([string]$Gate, [string]$Resultado, [hashtable]$Extras) {
  $v = Get-InstalledVersion
  $vd = Join-Path $script:Root ('versions\' + $v)
  $rs = Join-Path $vd 'runtime\R\bin\Rscript.exe'
  $ex = Join-Path $script:Work ('extras-' + $Gate + '.json')
  Write-Json $ex $Extras
  $a = '--vanilla "' + (Join-Path $script:Repo 'tests\windows\write-receipt.R') + '" "--repo=' + $script:Repo + '" "--code=' + (Join-Path $vd 'code') + '" --gate=' + $Gate +
    ' "--bundle=' + $script:BundleFile + '" --platform=windows-x86_64 --resultado=' + $Resultado + ' "--runtime-json=' + (Join-Path $vd 'RUNTIME.json') + '" "--dir=' + $script:ReceiptsDir + '" "--extras=' + $ex + '"'
  $r = Invoke-Proc $rs $a ('receipt-' + $Gate)
  if ($r.Exit -ne 0) { throw ('recibo ' + $Gate + ' nao gravado: ' + (ConvertTo-AsciiText $r.Err)) }
  return $r.Out.Trim()
}
function Read-Facts([string]$Path) { if (Test-Path -LiteralPath $Path) { return (Read-Json $Path) } return $null }
function Test-Prop($Obj, [string]$Name) { return ($null -ne $Obj) -and ($Obj.PSObject.Properties.Name -contains $Name) }

# --- sumario ------------------------------------------------------------------------------------------------------------
function Write-Summary {
  $w = 8
  Write-Host ''
  Write-Host '=== GATE DA VM WINDOWS: SUMARIO ==='
  Write-Host ('ITEM  STATUS  DETALHE')
  Write-Host ('----  ------  -------')
  foreach ($r in $script:Results) { Write-Host (($r.Id).PadRight(6) + ($r.Status).PadRight($w) + (ConvertTo-AsciiText $r.Detail)) }
  $nf = @($script:Results | Where-Object { $_.Status -eq 'FAIL' }).Count
  $ns = @($script:Results | Where-Object { $_.Status -eq 'SKIP' }).Count
  $np = @($script:Results | Where-Object { $_.Status -eq 'PASS' }).Count
  Write-Host ('PASS=' + $np + ' FAIL=' + $nf + ' SKIP=' + $ns)
  return @($np, $nf, $ns)
}

# =============================================================================================================================
if (-not $Work) { $Work = Join-Path ([IO.Path]::GetTempPath()) ('pc-windows-gate-' + (Get-Date -Format 'yyyyMMdd-HHmmss')) }
[void](New-Item -ItemType Directory -Force -Path $Work)
$script:Work = (Resolve-Path -LiteralPath $Work).Path
$script:Logs = Join-Path $script:Work 'logs'
[void](New-Item -ItemType Directory -Force -Path $script:Logs)
if (-not $ReceiptsDir) { $ReceiptsDir = Join-Path $script:Repo 'tests\.receipts' }
[void](New-Item -ItemType Directory -Force -Path $ReceiptsDir)
$script:ReceiptsDir = (Resolve-Path -LiteralPath $ReceiptsDir).Path
$script:Root = $Root
if (-not $script:Root) { $script:Root = Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'prevcarga' }
$script:Root = $script:Root.TrimEnd('\', '/')
$PsExe = Join-Path ([Environment]::GetFolderPath('System')) 'WindowsPowerShell\v1.0\powershell.exe'
$pwCmd = Get-Command pwsh -ErrorAction SilentlyContinue
$PwshPath = ''
if ($PwshExe) { $PwshPath = $PwshExe } elseif ($pwCmd) { $PwshPath = $pwCmd.Source }

# --- entradas: bundle local ou release ----------------------------------------------------------------------------------
if (-not $Bundle -and -not $ReleaseBase) {
  Write-Host 'uso: Invoke-WindowsGate.ps1 -Bundle <zip> -Sha256 <h> -RefDir <ref> | -ReleaseBase <url> [-Version <v>|latest]  (docs/reference/windows-gate.md)'
  exit 3
}
if ($ReleaseBase) {
  [Net.ServicePointManager]::SecurityProtocol = [Net.ServicePointManager]::SecurityProtocol -bor [Net.SecurityProtocolType]::Tls12
  $base = $ReleaseBase.TrimEnd('/')
  if (-not $Version -or $Version -eq 'latest') { $Version = ((New-Object Net.WebClient).DownloadString($base + '/releases/latest.txt')).Trim() }
  $name = 'prevcarga-' + $Version + '-windows-x86_64.zip'
  $dl = Join-Path $script:Work 'download'
  [void](New-Item -ItemType Directory -Force -Path $dl)
  $wc = New-Object Net.WebClient
  $wc.DownloadFile($base + '/releases/' + $Version + '/' + $name, (Join-Path $dl $name))
  $wc.DownloadFile($base + '/releases/' + $Version + '/SHA256SUMS', (Join-Path $dl 'SHA256SUMS'))
  $line = @([IO.File]::ReadAllLines((Join-Path $dl 'SHA256SUMS')) | Where-Object { $_ -match ('\s\*?' + [regex]::Escape($name) + '$') })
  if ($line.Count -ne 1) { Write-Host ('SHA256SUMS sem ' + $name); exit 3 }
  $Bundle = Join-Path $dl $name
  $Sha256 = ($line[0] -split '\s+')[0]
}
if (-not (Test-Path -LiteralPath $Bundle -PathType Leaf)) { Write-Host ('bundle inexistente: ' + $Bundle); exit 3 }
$script:BundleFile = (Resolve-Path -LiteralPath $Bundle).Path
if (-not $Sha256) {
  $sf = $script:BundleFile + '.sha256'
  if (Test-Path -LiteralPath $sf) { $Sha256 = ([IO.File]::ReadAllText($sf).Trim() -split '\s+')[0] }
}
if ($Sha256 -notmatch '^[0-9a-fA-F]{64}$') { Write-Host 'informe -Sha256 <64 hex> (ou um .sha256 ao lado do zip): o hash e obrigatorio'; exit 3 }
$Sha256 = $Sha256.ToLowerInvariant()
if ((Get-Sha $script:BundleFile) -ne $Sha256) { Write-Host 'sha256 do zip diverge do informado: nada foi instalado'; exit 3 }
$script:Runner = Join-Path $script:Work 'run-pester.ps1'
Write-PesterRunner $script:Runner

$Tvm = Join-Path $script:Repo 'install\tests\windows-vm.Tests.ps1'
$Tcross = Join-Path $script:Repo 'install\tests\windows-vm-cross.Tests.ps1'
$Tinst = Join-Path $script:Repo 'install\tests\install.Tests.ps1'
$Tsync = Join-Path $script:Repo 'install\tests\prevcarga-sync.Tests.ps1'
$DeckRef = ''; $StateRef = ''
if ($RefDir) { $RefDir = (Resolve-Path -LiteralPath $RefDir).Path; $DeckRef = Join-Path $RefDir 'deck'; $StateRef = Join-Path $RefDir 'state' }

# --- W01 preflight ----------------------------------------------------------------------------------------------------
try {
  $problems = @()
  $isAdmin = (New-Object Security.Principal.WindowsPrincipal([Security.Principal.WindowsIdentity]::GetCurrent())).IsInRole([Security.Principal.SecurityIdentifier]'S-1-5-32-544')
  if ($isAdmin) { $problems += 'processo COM token de administrador (rode num usuario comum)' }
  if (Get-Command Rscript -ErrorAction SilentlyContinue) { $problems += 'Rscript no PATH (a VM deve ser limpa, sem R)' }
  if (Get-Command R.exe -ErrorAction SilentlyContinue) { $problems += 'R.exe no PATH (a VM deve ser limpa, sem R)' }
  if (-not (Test-Path -LiteralPath $PsExe)) { $problems += 'powershell.exe 5.1 ausente' }
  $v = $PSVersionTable.PSVersion.ToString()
  $pol = ((Get-ExecutionPolicy -List | ForEach-Object { $_.Scope + '=' + $_.ExecutionPolicy }) -join ',')
  $pw = if ($PwshPath) { 'pwsh=' + $PwshPath } else { 'pwsh 7 ausente' }
  if (-not $PwshPath) { $problems += 'pwsh 7 ausente (zip portatil sem admin; ver o runbook)' }
  Write-Log 'W01' ('PSVersion=' + $v + ' policy=' + $pol + ' ' + $pw + ' Root=' + $script:Root)
  if ($problems.Count -gt 0) { Add-Result 'W01' 'FAIL' ($problems -join '; ') } else { Add-Result 'W01' 'PASS' ('PS ' + $v + '; sem admin; sem R; ' + $pw) }
} catch { Add-Result 'W01' 'FAIL' (ConvertTo-AsciiText $_.Exception.Message) }

# --- W02 build x2 (so com -Build, num host com checkout git) -----------------------------------------------------------
if (-not $Build) {
  Add-Result 'W02' 'SKIP' 'nao solicitado: rode com -Build -BuildVersion <v> no host de build (precisa de checkout git e internet)'
} else {
  try {
    if (-not $BuildVersion) { throw '-BuildVersion obrigatorio com -Build' }
    $outs = @()
    foreach ($n in 1, 2) {
      $o = Join-Path $script:Work ('build-' + $n); $outs += $o
      $r = Invoke-Proc $PsExe ('-NoProfile -ExecutionPolicy Bypass -File "' + (Join-Path $script:Repo 'scripts\tools\build_bundle_windows.ps1') + '" -Version ' + $BuildVersion + ' -Out "' + $o + '" -SourceDateEpoch 1700000000') ('build-' + $n)
      if ($r.Exit -ne 0) { throw ('build ' + $n + ' falhou (exit ' + $r.Exit + '); ver logs\build-' + $n + '.log') }
    }
    $z1 = Join-Path $outs[0] ('prevcarga-' + $BuildVersion + '-windows-x86_64.zip'); $z2 = Join-Path $outs[1] ('prevcarga-' + $BuildVersion + '-windows-x86_64.zip')
    $m1 = Get-Sha ($z1.Replace('.zip', '.RUNTIME.json')); $m2 = Get-Sha ($z2.Replace('.zip', '.RUNTIME.json'))
    if ($m1 -ne $m2) { throw 'RUNTIME.json difere entre os dois builds (runtime_id nao reprodutivel)' }
    if ((Get-Sha $z1) -ne (Get-Sha $z2)) { throw 'o zip difere entre os dois builds (mesmo RUNTIME.json: investigue a ordem/timestamps do zip)' }
    Add-Result 'W02' 'PASS' ('zip e RUNTIME.json identicos nos dois builds, sha256 ' + (Get-Sha $z1).Substring(0, 12))
  } catch { Add-Result 'W02' 'FAIL' (ConvertTo-AsciiText $_.Exception.Message) }
}

# --- W03/W04/W08..W13: windows-vm.Tests.ps1 (instala e executa os bracos na instalacao) --------------------------------
$factsVm = Join-Path $script:Work 'facts-windows-vm.json'
$dataVm = @{ Bundle = $script:BundleFile; Sha256 = $Sha256; Root = $script:Root; Work = (Join-Path $script:Work 'vm'); Facts = $factsVm; PwshExe = $PwshPath }
if ($DeckRef -and (Test-Path -LiteralPath $DeckRef) -and (Test-Path -LiteralPath $StateRef)) {
  $o0 = $Origin
  if (-not $o0 -and (Test-Path -LiteralPath (Join-Path $RefDir 'linux-x86_64\run_manifest.json'))) { $o0 = [string](Read-Json (Join-Path $RefDir 'linux-x86_64\run_manifest.json')).origin }
  $dataVm['Deck'] = $DeckRef; $dataVm['State'] = $StateRef; $dataVm['Origin'] = $o0
}
$runVm = Invoke-PesterSuite $PsExe $Tvm $dataVm 'windows-vm'
foreach ($id in 'W03', 'W04', 'W08', 'W09', 'W10', 'W11', 'W12', 'W13') { Add-ItemFromRun $runVm $id }
# recibo windows-vm: PASS so com todos os Its passados; FAIL (com o passo) em qualquer falha; nada se so houve skip
try {
  $installed = Test-Path -LiteralPath (Join-Path $script:Root 'current')
  $f = Read-Facts $factsVm
  $failedNames = @($runVm.tests | Where-Object { $_.result -eq 'Failed' } | ForEach-Object { $_.name })
  $res = ''
  if (-not $runVm.ok -or $runVm.failed -gt 0) { $res = 'FAIL' } elseif ($runVm.skipped -eq 0 -and $runVm.notrun -eq 0) { $res = 'PASS' }
  if ($res -and $installed) {
    $sh = @{ cmd = $null; ps51 = $null; ps7 = $null }
    if (Test-Prop $f 'shells') { foreach ($k in 'cmd', 'ps51', 'ps7') { if (Test-Prop $f.shells $k) { $sh[$k] = $f.shells.$k } } }
    $pf = $null; if ($failedNames.Count -gt 0) { $pf = ConvertTo-AsciiText $failedNames[0] } elseif (-not $runVm.ok) { $pf = 'runner' }
    $b127 = 0; if (Test-Prop $f 'bytes_gt_127') { $b127 = [int]$f.bytes_gt_127 }
    $p = Write-GateReceipt 'windows-vm' $res @{ shells = $sh; bytes_gt_127 = $b127; passo_falho = $pf }
    Write-Host ('recibo windows-vm ' + $res + ': ' + $p)
  } elseif ($res) { Write-Host 'recibo windows-vm nao gravado: bundle nao instalado (sem Rscript.exe do bundle)' }
  else { Write-Host 'recibo windows-vm nao gravado: houve SKIP (insumo ausente); so PASS integral ou FAIL gravam recibo' }
} catch { Add-Result 'W03' 'FAIL' ('recibo windows-vm: ' + (ConvertTo-AsciiText $_.Exception.Message)) }

# --- W05/W06: as suites Pester existentes (nao duplicadas), em PS 5.1 e pwsh 7 ----------------------------------------------
$shellsAll = @(@{ Tag = 'ps51'; Exe = $PsExe })
if ($PwshPath) { $shellsAll += @{ Tag = 'ps7'; Exe = $PwshPath } }
$instRuns = @()
foreach ($sh in $shellsAll) { $instRuns += , (Invoke-PesterSuite $sh.Exe $Tinst @{ Candidate = $script:BundleFile } ('install-' + $sh.Tag)) }
$syncRuns = @()
foreach ($sh in $shellsAll) { $syncRuns += , (Invoke-PesterSuite $sh.Exe $Tsync @{} ('sync-' + $sh.Tag)) }
function Add-SuiteResult([string]$Id, $Runs, [string]$Label) {
  $bad = @($Runs | Where-Object { -not $_.ok -or $_.failed -gt 0 })
  $sk = @($Runs | Where-Object { ($_.ok) -and (($_.skipped -gt 0) -or ($_.notrun -gt 0)) })
  if (-not $PwshPath) { Add-Result $Id 'FAIL' ($Label + ': pwsh 7 ausente (o item exige 5.1 e 7)'); return }
  if ($bad.Count -gt 0) { Add-Result $Id 'FAIL' ($Label + ': ' + (($Runs | ForEach-Object { 'falhas=' + $_.failed }) -join ' / ') + '; ver logs\pester-*.log'); return }
  if ($sk.Count -gt 0) { Add-Result $Id 'SKIP' ($Label + ': ' + (($Runs | ForEach-Object { 'skip=' + $_.skipped }) -join ' / ') + ' (skip nao justificado reprova o gate de fechamento: veja o log)'); return }
  Add-Result $Id 'PASS' ($Label + ': ' + (($Runs | ForEach-Object { 'passou=' + $_.passed }) -join ' / '))
}
Add-SuiteResult 'W05' $instRuns 'install.Tests.ps1'
Add-SuiteResult 'W06' $syncRuns 'prevcarga-sync.Tests.ps1'
# --- W07 PSScriptAnalyzer -----------------------------------------------------------------------------------------------
try {
  $saf = Join-Path $script:Work 'run-analyzer.ps1'
  $la = @(
    'param([string]$Path, [string]$Settings, [string]$ModulesDir, [switch]$InstallModules)',
    '$ErrorActionPreference = ''Stop''',
    '$cand = $null; if ($ModulesDir) { $cand = Join-Path $ModulesDir ''PSScriptAnalyzer\1.22.0\PSScriptAnalyzer.psd1''; if (-not (Test-Path -LiteralPath $cand)) { $cand = $null } }',
    'if ($cand) { Import-Module $cand -Force }',
    'else {',
    '  if (-not (Get-Module -ListAvailable -Name PSScriptAnalyzer | Where-Object { $_.Version -eq [version]''1.22.0'' })) {',
    '    if (-not $InstallModules) { throw ''PSScriptAnalyzer 1.22.0 ausente: use -ModulesDir ou -InstallModules'' }',
    '    Install-Module -Name PSScriptAnalyzer -RequiredVersion 1.22.0 -Scope CurrentUser -Force -SkipPublisherCheck -AllowClobber',
    '  }',
    '  Import-Module PSScriptAnalyzer -RequiredVersion 1.22.0 -Force',
    '}',
    '$f = @(Invoke-ScriptAnalyzer -Path $Path -Settings $Settings | Where-Object { $_.Severity -in ''Error'', ''Warning'' })',
    '$f | ForEach-Object { Write-Output ($_.RuleName + '' '' + $_.Line + '' '' + $_.Message) }',
    'if ($f.Count -gt 0) { exit 1 }',
    'exit 0')
  [IO.File]::WriteAllText($saf, ($la -join "`r`n") + "`r`n", [Text.Encoding]::ASCII)
  $a = '-NoProfile -ExecutionPolicy Bypass -File "' + $saf + '" -Path "' + (Join-Path $script:Repo 'install\install.ps1') + '" -Settings "' + (Join-Path $script:Repo 'install\tests\PSScriptAnalyzerSettings.psd1') + '"'
  if ($ModulesDir) { $a += ' -ModulesDir "' + $ModulesDir + '"' }
  if ($InstallModules) { $a += ' -InstallModules' }
  $r = Invoke-Proc $PsExe $a 'W07'
  if ($r.Exit -eq 0) { Add-Result 'W07' 'PASS' 'sem Error/Warning (settings fixo)' }
  else { Add-Result 'W07' 'FAIL' ('exit ' + $r.Exit + ': ' + (ConvertTo-AsciiText (($r.Out + $r.Err).Trim().Split("`n")[0]))) }
} catch { Add-Result 'W07' 'FAIL' (ConvertTo-AsciiText $_.Exception.Message) }

# recibo install-e2e-windows (irmao do install-e2e-linux): W05 + W07 PASS
try {
  $w5 = @($script:Results | Where-Object { $_.Id -eq 'W05' })[0].Status; $w7 = @($script:Results | Where-Object { $_.Id -eq 'W07' })[0].Status
  if ((Test-Path -LiteralPath (Join-Path $script:Root 'current')) -and $w5 -ne 'SKIP' -and $w7 -ne 'SKIP') {
    $res = if ($w5 -eq 'PASS' -and $w7 -eq 'PASS') { 'PASS' } else { 'FAIL' }
    $p = Write-GateReceipt 'install-e2e-windows' $res @{ pester_install = $w5; analyzer = $w7 }
    Write-Host ('recibo install-e2e-windows ' + $res + ': ' + $p)
  }
} catch { Add-Result 'W05' 'FAIL' ('recibo install-e2e-windows: ' + (ConvertTo-AsciiText $_.Exception.Message)) }

# --- W14/W15: bracos Windows x Linux e NFR ------------------------------------------------------------------------------
$factsX = Join-Path $script:Work 'facts-windows-vm-cross.json'
$dataX = @{ Root = $script:Root; Work = (Join-Path $script:Work 'cross'); Facts = $factsX; PwshExe = $PwshPath; RefDir = $RefDir; Origin = $Origin
            NfrDeck = $NfrDeck; NfrState = $NfrState; NfrOrigin = $NfrOrigin }
$runX = Invoke-PesterSuite $PsExe $Tcross $dataX 'windows-vm-cross'
foreach ($id in 'W14', 'W15') { Add-ItemFromRun $runX $id }
try {
  $fx = Read-Facts $factsX
  $s14 = @($script:Results | Where-Object { $_.Id -eq 'W14' })[0].Status
  if ($s14 -eq 'PASS') {
    $p = Write-GateReceipt 'cross-platform' 'PASS' @{ referencia_platform = 'linux-x86_64'; jobs_equal = [bool]$fx.jobs_equal; verificar_exit = $fx.verificar_exit; control_exit = $fx.control_exit }
    Write-Host ('recibo cross-platform PASS (lado Windows): ' + $p)
  } elseif ($s14 -eq 'FAIL' -and (Test-Path -LiteralPath (Join-Path $script:Root 'current'))) {
    $p = Write-GateReceipt 'cross-platform' 'FAIL' @{ referencia_platform = 'linux-x86_64'; passo_falho = (@($runX.tests | Where-Object { $_.result -eq 'Failed' })[0].name) }
    Write-Host ('recibo cross-platform FAIL: ' + $p)
  }
  $s15 = @($script:Results | Where-Object { $_.Id -eq 'W15' })[0].Status
  if (($s15 -eq 'PASS' -or $s15 -eq 'FAIL') -and (Test-Prop $fx 'nfr') -and $null -ne $fx.nfr) {
    $res = if ($s15 -eq 'PASS') { 'PASS' } else { 'FAIL' }
    $p = Write-GateReceipt 'nfr-30min' $res @{ duracao_s = $fx.nfr.duracao_s; passos = $fx.nfr.passos; jobs = 1; passo_mais_lento = $fx.nfr.passo_mais_lento; platform_rodada = $fx.nfr.platform }
    Write-Host ('recibo nfr-30min ' + $res + ': ' + $p)
  }
} catch { Add-Result 'W14' 'FAIL' ('recibo: ' + (ConvertTo-AsciiText $_.Exception.Message)) }

# --- W16/W17: fechamento do M8 (M8.12): suite inteira e Python-free da release pelo Rscript.exe do bundle INSTALADO -----------------------
$installedOk = Test-Path -LiteralPath (Join-Path $script:Root 'current')
$vdNow = ''; $rsNow = ''
if ($installedOk) { $vdNow = Join-Path $script:Root ('versions\' + (Get-InstalledVersion)); $rsNow = Join-Path $vdNow 'runtime\R\bin\Rscript.exe' }
# W16: a suite inteira (testthat dos 4 diretorios + run_all_gates --tier fast dos membros) no Windows nativo, com o R do bundle
try {
  if (-not $installedOk -or -not (Test-Path -LiteralPath $rsNow)) { Add-Result 'W16' 'SKIP' 'bundle nao instalado (W03 nao passou): sem Rscript.exe do bundle' }
  elseif (-not $SuiteRepo -or -not (Test-Path -LiteralPath (Join-Path $SuiteRepo 'tests\run_suite.R'))) { Add-Result 'W16' 'SKIP' 'informe -SuiteRepo <arvore completa do repo: pasta repo\ do pacote, gerada por prepare_windows_gate.sh --suite-tree>' }
  elseif (-not $TestLib -or -not (Test-Path -LiteralPath $TestLib)) { Add-Result 'W16' 'SKIP' 'informe -TestLib <pasta com testthat e dependencias (binarios Windows do R 4.5.3)>: o bundle e zero-dep e nao traz testthat' }
  else {
    $a = '--vanilla "' + (Join-Path $SuiteRepo 'tests\run_suite.R') + '" "--root=' + $SuiteRepo + '" "--lib=' + $TestLib + '" --list-skips'
    $r = Invoke-Proc $rsNow $a 'W16'
    $tail = (ConvertTo-AsciiText (($r.Out.Trim() -split "`n" | Select-Object -Last 1)))
    if ($r.Exit -eq 0) { Add-Result 'W16' 'PASS' ('run_suite.R: ' + $tail) } else { Add-Result 'W16' 'FAIL' ('run_suite.R exit ' + $r.Exit + ': ' + $tail + ' (ver logs\W16.log)') }
  }
} catch { Add-Result 'W16' 'FAIL' (ConvertTo-AsciiText $_.Exception.Message) }
# W17: Python-free + rede/env + portabilidade + RUNTIME.json do bundle windows-x86_64 instalado (o mesmo conteudo do zip candidato)
try {
  if (-not $installedOk -or -not (Test-Path -LiteralPath $rsNow)) { Add-Result 'W17' 'SKIP' 'bundle nao instalado (W03 nao passou): sem Rscript.exe do bundle' }
  else {
    $a = '--vanilla "' + (Join-Path $script:Repo 'tests\windows\release-check.R') + '" "--repo=' + $script:Repo + '" "--bundle-root=' + $vdNow + '"'
    $r = Invoke-Proc $rsNow $a 'W17'
    if ($r.Exit -eq 0) { Add-Result 'W17' 'PASS' 'python_free, net_grep, portabilidade e RUNTIME.json (r_version 4.5.3, windows-x86_64) sem hits' }
    else { Add-Result 'W17' 'FAIL' ('release-check.R exit ' + $r.Exit + ': ' + (ConvertTo-AsciiText (($r.Out + $r.Err).Trim().Split("`n") | Where-Object { $_ -like '*FAIL*' } | Select-Object -First 1)) + ' (ver logs\W17.log)') }
  }
} catch { Add-Result 'W17' 'FAIL' (ConvertTo-AsciiText $_.Exception.Message) }

# --- sumario e exit -----------------------------------------------------------------------------------------------------
$c = Write-Summary
Write-Json (Join-Path $script:Work 'gate-result.json') @{ bundle = $script:BundleFile; sha256 = $Sha256; root = $script:Root; receipts = $script:ReceiptsDir
  pass = $c[0]; fail = $c[1]; skip = $c[2]; items = @($script:Results | ForEach-Object { @{ id = $_.Id; status = $_.Status; detail = $_.Detail } }) }
Write-Host ('logs: ' + $script:Logs + '   recibos: ' + $script:ReceiptsDir)
if ($c[1] -gt 0) { Write-Host 'RESULTADO: FAIL'; exit 1 }
if ($c[2] -gt 0 -and -not $AllowSkip) { Write-Host 'RESULTADO: INCOMPLETO (ha SKIP; rode com os insumos que faltam ou use -AllowSkip conscientemente)'; exit 2 }
Write-Host 'RESULTADO: PASS'
exit 0
