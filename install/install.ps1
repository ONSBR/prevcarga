# install.ps1 -- instalador user-local do prevcarga para Windows nativo (M8.4; ADR-0038, contrato de erros ADR-0040).
# Windows PowerShell 5.1 e pwsh 7; roda com `-ExecutionPolicy Bypass`; NAO precisa de administrador (HKCU, %LOCALAPPDATA%).
# Espelha install/install.sh (M8.3): mesmo layout <R>\current + <R>\versions\<v>, mesma semantica, mesmos error.id.
#
# Uso:
#   install.ps1 [-Version <v>|latest] [-ReleaseBase <url>] [-Bundle <zip> [-Sha256 <h>]] [-Root <dir>] [-Data <dir>]
#               [-State <dir>] [-SkipPlatformCheck] [-Format text|json]
#   install.ps1 -Rollback [-Root <dir>] [-Format json]
#   install.ps1 -Uninstall -Yes [-Purge] [-Root <dir>] [-Format json]
# A grafia dos docs (--bundle, --sha256, --root ...) tambem e aceita.
#
# O sha256 do bundle e OBRIGATORIO e conferido antes de qualquer extracao; nada e escrito em <R> antes disso.
# Nenhuma variavel de ambiente e lida: a raiz default vem de [Environment]::GetFolderPath('LocalApplicationData').

param(
  [string]$Version, [string]$ReleaseBase, [string]$Bundle, [string]$Sha256,
  [string]$Root, [string]$Data, [string]$State,
  [switch]$SkipPlatformCheck, [switch]$Rollback, [switch]$Uninstall, [switch]$Yes, [switch]$Purge,
  [string]$Format = 'text',
  [Parameter(ValueFromRemainingArguments = $true)][string[]]$Rest
)

Set-StrictMode -Version 3
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$script:PcExit = 0
$script:ShowHelp = $false
$script:PcStage = ''
$script:PcTemp = ''
$script:PcCatalog = Join-Path $PSScriptRoot 'error-catalog.tsv'  # GERADO; no bundle extraido: code\install\
$script:PcWarnings = New-Object System.Collections.Generic.List[string]
$script:PcMinFreeBytes = [long]2147483648

# --- saida ASCII pura (independente de code page) ---------------------------------------------------------------
function ConvertTo-PcJsonString([string]$Text) {
  $sb = New-Object System.Text.StringBuilder
  [void]$sb.Append('"')
  foreach ($c in $Text.ToCharArray()) {
    $n = [int]$c
    if ($c -eq '"') { [void]$sb.Append('\"') }
    elseif ($c -eq '\') { [void]$sb.Append('\\') }
    elseif ($n -lt 32 -or $n -gt 126) { [void]$sb.Append(('\u{0:x4}' -f $n)) }
    else { [void]$sb.Append($c) }
  }
  [void]$sb.Append('"')
  return $sb.ToString()
}

function ConvertTo-PcAsciiText([string]$Text) {
  $sb = New-Object System.Text.StringBuilder
  foreach ($c in $Text.ToCharArray()) {
    $n = [int]$c
    if ($n -eq 10 -or ($n -ge 32 -and $n -le 126)) { [void]$sb.Append($c) } else { [void]$sb.Append('?') }
  }
  return $sb.ToString()
}

function Write-PcStream([string]$Which, [string]$Text) {
  $bytes = [Text.Encoding]::ASCII.GetBytes($Text + "`n")
  if ($Which -eq 'out') { $s = [Console]::OpenStandardOutput() } else { $s = [Console]::OpenStandardError() }
  $s.Write($bytes, 0, $bytes.Length)
  $s.Flush()
}

function ConvertTo-PcSlash([string]$Path) { return ($Path -replace '\\', '/') }

# --- erros (ADR-0040) ----------------------------------------------------------------------------------------------
function Get-PcExitForClass([string]$Class) {
  switch ($Class) { 'bundle' { return 6 } 'config' { return 8 } 'integrity' { return 9 } 'network' { return 10 } default { return 1 } }
}

function Get-PcEmbeddedRow([string]$Id) {  # mesmo texto de scripts/lib/errors/install.tsv (usado antes da extracao)
  switch ($Id) {
    'install.download_failed' { return @('network', 'falha ao baixar o bundle', 'confira -ReleaseBase e a rede') }
    'install.io_failed' { return @('bundle', 'falha de escrita na raiz da instalacao', 'confira permissoes e espaco em disco da raiz') }
    'install.manifest_invalid' { return @('bundle', 'RUNTIME.json invalido ou arquivo do bundle divergente', 'reinstale o bundle da mesma versao; a versao atual nao foi alterada') }
    'install.no_previous_version' { return @('config', 'nao ha versao anterior retida para voltar', 'instale a versao desejada de novo com o instalador (--version / -Version)') }
    'install.preflight_failed' { return @('bundle', 'plataforma nao suportada', 'Linux x86_64 glibc >= 2.35 com zstd, tar, sha256sum e cliente HTTPS; ou Windows 64-bit com PowerShell >= 5.1 e espaco livre na raiz') }
    'install.sha_mismatch' { return @('integrity', 'sha256 do bundle nao confere', 'baixe o bundle novamente e confira o SHA256SUMS') }
    'install.sha_missing' { return @('integrity', 'sha256 do bundle nao informado', 'passe -Sha256 ou um SHA256SUMS ao lado do bundle') }
    'install.state_inside_versions' { return @('config', '-State aponta para dentro de versions\', 'use um diretorio de estado fora de <raiz>\versions') }
    default { return @('config', 'argumento invalido ou ausente', 'veja a ajuda do instalador (install.sh --help / install.ps1 -Help)') }
  }
}

function Read-PcCatalog([string]$Path, [string]$Id) {  # catalogo GERADO (ASCII, LF): colunas lidas pelo NOME no cabecalho (id, class, exit_code, message, hint); $null se ausente
  if (-not (Test-Path -LiteralPath $Path)) { return $null }
  $col = $null
  foreach ($line in [IO.File]::ReadAllLines($Path, [Text.Encoding]::ASCII)) {
    $f = $line -split "`t"
    if ($null -eq $col) {
      if ($f.Count -ge 1 -and $f[0] -eq 'id') {
        $col = @{}
        for ($i = 0; $i -lt $f.Count; $i++) { $col[$f[$i]] = $i }
        foreach ($n in 'class', 'message', 'hint') { if (-not $col.ContainsKey($n)) { return $null } }
      }
      continue
    }
    if ($f.Count -gt $col['hint'] -and $f[0] -eq $Id) { return @($f[$col['class']], $f[$col['message']], $f[$col['hint']]) }
  }
  return $null
}

function Write-PcErrorOut([string]$Id, [string]$Where, [string]$Details) {
  $row = Read-PcCatalog $script:PcCatalog $Id
  if (-not $row) { $row = Get-PcEmbeddedRow $Id }
  $code = Get-PcExitForClass $row[0]
  if ($Format -eq 'json') {  # UM unico JSON em stdout, stderr vazio
    $j = '{"ok":false,"exit_code":' + $code + ',"error":{"id":' + (ConvertTo-PcJsonString $Id) + ',"class":' + (ConvertTo-PcJsonString $row[0]) +
      ',"message":' + (ConvertTo-PcJsonString $row[1]) + ',"hint":' + (ConvertTo-PcJsonString $row[2]) +
      ',"where":' + (ConvertTo-PcJsonString $Where) + ',"details":' + (ConvertTo-PcJsonString $Details) + '}}'
    Write-PcStream 'out' $j
  } else {
    $t = 'prevcarga: erro [' + $row[0] + '] (exit ' + $code + ")`no que: " + $row[1] + "`nonde: " + $Where + "`ncomo corrigir: " + $row[2]
    Write-PcStream 'err' (ConvertTo-PcAsciiText $t)
  }
  return $code
}

function Write-PcError([string]$Id, [string]$Where, [string]$Details = '') {  # emite e interrompe (o cleanup roda no topo)
  $script:PcExit = Write-PcErrorOut $Id $Where $Details
  throw 'PCEXIT'
}

function Add-PcWarning([string]$Text) {
  $script:PcWarnings.Add($Text)
  if ($Format -ne 'json') { Write-PcStream 'err' (ConvertTo-PcAsciiText ('prevcarga: aviso: ' + $Text)) }
}

function Write-PcOk([string]$Action, [string]$ExtraJson, [string]$TextLine) {
  if ($Format -eq 'json') {
    $w = ''
    if ($script:PcWarnings.Count -gt 0) {
      $w = ',"warnings":[' + (($script:PcWarnings | ForEach-Object { ConvertTo-PcJsonString $_ }) -join ',') + ']'
    }
    $x = ''
    if ($ExtraJson) { $x = ',' + $ExtraJson }
    $j = '{"ok":true,"exit_code":0,"error":null,"result":{"action":' + (ConvertTo-PcJsonString $Action) + ',"root":' +
      (ConvertTo-PcJsonString (ConvertTo-PcSlash $script:Root)) + $x + '}' + $w + '}'
    Write-PcStream 'out' $j
  } else {
    Write-PcStream 'out' (ConvertTo-PcAsciiText $TextLine)
  }
}

# --- argumentos (aceita -Nome e --nome) --------------------------------------------------------------------------
function Resolve-PcRest([string[]]$Tokens) {
  $bad = $null
  $i = 0
  while ($Tokens -and $i -lt $Tokens.Count) {
    $tok = $Tokens[$i]
    $name = ''
    $val = $null
    if ($tok -match '^--?([A-Za-z][A-Za-z0-9-]*)(=(.*))?$') {
      $name = $Matches[1].ToLowerInvariant()
      if ($Matches[2]) { $val = $Matches[3] }
    } else {
      $bad = @($tok, 'argumento desconhecido'); break
    }
    if ($name -in 'version', 'releasebase', 'release-base', 'bundle', 'sha256', 'root', 'data', 'state', 'format') {
      if ($null -eq $val) {
        if ($i + 1 -ge $Tokens.Count) { $bad = @($tok, 'falta o valor'); break }
        $i++; $val = $Tokens[$i]
      }
      if (-not $val) { $bad = @($tok, 'valor vazio'); break }
      if ($name -eq 'version') { $script:Version = $val }
      elseif ($name -eq 'releasebase' -or $name -eq 'release-base') { $script:ReleaseBase = $val }
      elseif ($name -eq 'bundle') { $script:Bundle = $val }
      elseif ($name -eq 'sha256') { $script:Sha256 = $val }
      elseif ($name -eq 'root') { $script:Root = $val }
      elseif ($name -eq 'data') { $script:Data = $val }
      elseif ($name -eq 'state') { $script:State = $val }
      else { $script:Format = $val }
    }
    elseif ($name -eq 'skipplatformcheck' -or $name -eq 'skip-platform-check') { $script:SkipPlatformCheck = [switch]$true }
    elseif ($name -eq 'rollback') { $script:Rollback = [switch]$true }
    elseif ($name -eq 'uninstall') { $script:Uninstall = [switch]$true }
    elseif ($name -eq 'yes') { $script:Yes = [switch]$true }
    elseif ($name -eq 'purge') { $script:Purge = [switch]$true }
    elseif ($name -eq 'help' -or $name -eq 'h') { $script:ShowHelp = $true }
    else { $bad = @($tok, 'argumento desconhecido') }
    if ($bad) { break }
    $i++
  }
  return $bad
}

function Get-PcAbsPath([string]$Path) {
  return $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Path)
}

function Test-PcInside([string]$Path, [string]$Dir) {
  $p = (ConvertTo-PcSlash $Path).TrimEnd('/').ToLowerInvariant()
  $d = (ConvertTo-PcSlash $Dir).TrimEnd('/').ToLowerInvariant()
  return ($p -eq $d -or $p.StartsWith($d + '/'))
}

# --- utilitarios --------------------------------------------------------------------------------------------------
function Invoke-PcNative([string]$Exe, [string[]]$ArgList) {  # devolve @(exitcode, stdout); stderr descartado (EAP=Stop quebraria no 5.1)
  $prev = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try {
    $out = & $Exe @ArgList 2>$null
    $code = $LASTEXITCODE
  } finally { $ErrorActionPreference = $prev }
  return @($code, (@($out) -join "`n"))
}

function Read-PcLine([string]$Path) {
  if (-not (Test-Path -LiteralPath $Path -PathType Leaf)) { return '' }
  $lines = @([IO.File]::ReadAllLines($Path))
  if ($lines.Count -eq 0) { return '' }
  return $lines[0].Trim()
}

function Write-PcFileSwap([string]$Path, [string]$Content) {  # tempfile no mesmo diretorio + troca (nunca symlink/junction)
  $tmp = Join-Path (Split-Path -Parent $Path) ('.tmp.' + [guid]::NewGuid().ToString('N'))
  try {
    [IO.File]::WriteAllText($tmp, $Content + "`n", (New-Object Text.UTF8Encoding $false))
    if (Test-Path -LiteralPath $Path) { [IO.File]::Replace($tmp, $Path, $null) } else { [IO.File]::Move($tmp, $Path) }
  } catch {
    if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue }
    Write-PcError 'install.io_failed' $Path $_.Exception.Message
  }
}

function Publish-PcCurrent([string]$V) {  # $V ja validada e movida para versions\<v>
  if (-not (Test-Path -LiteralPath (Join-Path $script:Root ('versions\' + $V)) -PathType Container)) {
    Write-PcError 'install.manifest_invalid' (Join-Path $script:Root ('versions\' + $V))
  }
  Write-PcFileSwap (Join-Path $script:Root 'current') $V
}

function Get-PcInstalledVersions {
  $vd = Join-Path $script:Root 'versions'
  if (-not (Test-Path -LiteralPath $vd)) { return @() }
  return @(Get-ChildItem -LiteralPath $vd -Directory | Where-Object { -not $_.Name.StartsWith('.') } | ForEach-Object { $_.Name })
}

function Get-PcConfigValue([string]$Key) {
  $cfg = Join-Path $script:Root 'config.yaml'
  if (-not (Test-Path -LiteralPath $cfg)) { return '' }
  foreach ($line in [IO.File]::ReadAllLines($cfg)) {
    if ($line.StartsWith($Key + ':')) {
      $v = $line.Substring($Key.Length + 1).Trim()
      if ($v.Length -ge 2 -and $v.StartsWith("'") -and $v.EndsWith("'")) { $v = $v.Substring(1, $v.Length - 2).Replace("''", "'") }
      return $v
    }
  }
  return ''
}

function ConvertTo-PcYamlQuoted([string]$Text) { return "'" + $Text.Replace("'", "''") + "'" }

function Write-PcConfig {  # config.yaml da instalacao (schema v1 do M1.4a); preserva chaves existentes num upgrade
  $cfg = Join-Path $script:Root 'config.yaml'
  $d = $script:Data
  if (-not $d) { $d = Get-PcConfigValue 'data_dir' }
  $s = $script:State
  if (-not $s) { $s = Get-PcConfigValue 'state_dir' }
  if (-not $s) { $s = Join-Path $script:Root 'state' }
  $lines = New-Object System.Collections.Generic.List[string]
  if (Test-Path -LiteralPath $cfg) {
    $has = $false
    foreach ($line in [IO.File]::ReadAllLines($cfg)) {
      if ($line.StartsWith('data_dir:') -or $line.StartsWith('state_dir:')) { continue }
      if ($line.StartsWith('schema_version:')) { $has = $true }
      $lines.Add($line)
    }
    if (-not $has) { $lines.Add('schema_version: 1') }
  } else {
    $lines.Add('# config.yaml da instalacao (gerado por install.ps1; schema v1). Precedencia: flag da CLI > este arquivo > default.')
    $lines.Add('schema_version: 1'); $lines.Add('format: text'); $lines.Add('color: auto')
  }
  if ($d) { $lines.Add('data_dir: ' + (ConvertTo-PcYamlQuoted (ConvertTo-PcSlash $d))) }
  $lines.Add('state_dir: ' + (ConvertTo-PcYamlQuoted (ConvertTo-PcSlash $s)))
  $text = ($lines -join "`n") + "`n"
  $tmp = Join-Path $script:Root ('.config.' + [guid]::NewGuid().ToString('N'))
  try {
    [IO.File]::WriteAllText($tmp, $text, (New-Object Text.UTF8Encoding $false))
    if (Test-Path -LiteralPath $cfg) { [IO.File]::Replace($tmp, $cfg, $null) } else { [IO.File]::Move($tmp, $cfg) }
  } catch {
    Write-PcError 'install.io_failed' $cfg $_.Exception.Message
  }
  $script:StateEffective = $s
}

# --- PATH do usuario (HKCU; sem HKLM, sem elevacao) ---------------------------------------------------------------
function Send-PcSettingChange {  # avisa as janelas abertas (WM_SETTINGCHANGE); falha aqui nao e fatal
  try {
    if (-not ('PcNative.Win32' -as [type])) {
      Add-Type -Namespace PcNative -Name Win32 -MemberDefinition @'
[System.Runtime.InteropServices.DllImport("user32.dll", SetLastError = true, CharSet = System.Runtime.InteropServices.CharSet.Auto)]
public static extern System.IntPtr SendMessageTimeout(System.IntPtr hWnd, uint Msg, System.UIntPtr wParam, string lParam, uint fuFlags, uint uTimeout, out System.UIntPtr lpdwResult);
'@
    }
    $r = [UIntPtr]::Zero
    [void][PcNative.Win32]::SendMessageTimeout([IntPtr]0xffff, 0x1A, [UIntPtr]::Zero, 'Environment', 2, 5000, [ref]$r)
  } catch {
    Add-PcWarning ('path_notify_failed: ' + $_.Exception.Message)
  }
}

function Sync-PcUserPath([string]$BinDir, [bool]$Remove) {  # idempotente; devolve se o Path do usuario mudou
  $key = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey('Environment', $true)
  if (-not $key) { Write-PcError 'install.io_failed' 'HKCU:\Environment' }
  try {
    $cur = [string]$key.GetValue('Path', '', [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
    $norm = $BinDir.TrimEnd('\').ToLowerInvariant()
    $parts = @($cur -split ';' | Where-Object { $_ -ne '' })
    $keep = @($parts | Where-Object { $_.TrimEnd('\').ToLowerInvariant() -ne $norm })
    $present = ($keep.Count -ne $parts.Count)
    if ($Remove) { if (-not $present) { return $false }; $new = $keep }
    else { if ($present) { return $false }; $new = $keep + $BinDir }
    $key.SetValue('Path', ($new -join ';'), [Microsoft.Win32.RegistryValueKind]::ExpandString)
  } finally { $key.Dispose() }
  Send-PcSettingChange
  return $true
}

# --- preflight e aquisicao ----------------------------------------------------------------------------------------
function Test-PcPreflight {
  $fail = @()
  if (-not [Environment]::Is64BitOperatingSystem) { $fail += 'so=32-bit' }
  if ($PSVersionTable.PSVersion.Major -lt 5 -or ($PSVersionTable.PSVersion.Major -eq 5 -and $PSVersionTable.PSVersion.Minor -lt 1)) { $fail += ('powershell=' + $PSVersionTable.PSVersion) }
  $pr = [IO.Path]::GetPathRoot($script:Root)
  if ($pr -and $pr.Length -ge 2 -and $pr[1] -eq ':') {
    try {
      $free = (New-Object IO.DriveInfo($pr)).AvailableFreeSpace
      if ($free -lt $script:PcMinFreeBytes) { $fail += ('espaco_livre=' + $free) }
    } catch { $fail += 'espaco_livre=desconhecido' }
  }
  if ($fail.Count -gt 0) {
    if ($SkipPlatformCheck) { Add-PcWarning ('preflight ignorado (-SkipPlatformCheck): ' + ($fail -join ' ')) }
    else { Write-PcError 'install.preflight_failed' 'plataforma' ($fail -join ' ') }
  }
}

function Get-PcShaFromSums([string]$SumsFile, [string]$Name) {
  if (-not (Test-Path -LiteralPath $SumsFile -PathType Leaf)) { return '' }
  foreach ($line in [IO.File]::ReadAllLines($SumsFile)) {
    if ($line -match '^\s*([0-9A-Fa-f]{64})\s+\*?(.+?)\s*$' -and $Matches[2] -eq $Name) { return $Matches[1] }
  }
  return ''
}

function Get-PcFile([string]$Url, [string]$Dest) {
  try {
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile $Dest | Out-Null
    return $true
  } catch { return $false }
}

function Get-PcVerifiedBundle {  # devolve o caminho do zip (fora de <R>) so se o sha256 conferir
  $script:PcTemp = Join-Path ([IO.Path]::GetTempPath()) ('prevcarga-' + [guid]::NewGuid().ToString('N'))
  [void](New-Item -ItemType Directory -Path $script:PcTemp)
  if ($Bundle) {
    $zip = $script:Bundle
    if (-not (Test-Path -LiteralPath $zip -PathType Leaf)) { Write-PcError 'install.usage' '-Bundle' ('arquivo inexistente: ' + $zip) }
    $sha = $script:Sha256
    if (-not $sha) { $sha = Get-PcShaFromSums (Join-Path (Split-Path -Parent $zip) 'SHA256SUMS') (Split-Path -Leaf $zip) }
    if (-not $sha) { Write-PcError 'install.sha_missing' $zip }
  } else {
    $base = $script:ReleaseBase
    if (-not $base) { $base = 'https://prevcarga.s3.amazonaws.com' }
    $base = $base.TrimEnd('/')
    if (-not $script:Version -or $script:Version -eq 'latest') {
      $lt = Join-Path $script:PcTemp 'latest.txt'
      if (-not (Get-PcFile ($base + '/releases/latest.txt') $lt)) { Write-PcError 'install.download_failed' ($base + '/releases/latest.txt') }
      $script:Version = Read-PcLine $lt
      if (-not $script:Version) { Write-PcError 'install.download_failed' ($base + '/releases/latest.txt') 'versao vazia' }
    }
    $name = 'prevcarga-' + $script:Version + '-windows-x86_64.zip'
    $zip = Join-Path $script:PcTemp $name
    $sums = Join-Path $script:PcTemp 'SHA256SUMS'
    $u = $base + '/releases/' + $script:Version + '/'
    if (-not (Get-PcFile ($u + $name) $zip)) { Write-PcError 'install.download_failed' ($u + $name) }
    if (-not (Get-PcFile ($u + 'SHA256SUMS') $sums)) { Write-PcError 'install.download_failed' ($u + 'SHA256SUMS') }
    $sha = Get-PcShaFromSums $sums $name
    if (-not $sha) { Write-PcError 'install.sha_missing' ($u + 'SHA256SUMS') ($name + ' ausente do SHA256SUMS') }
  }
  $got = (Get-FileHash -Algorithm SHA256 -LiteralPath $zip).Hash.ToLowerInvariant()
  if ($got -ne $sha.ToLowerInvariant()) { Write-PcError 'install.sha_mismatch' $zip ('esperado=' + $sha.ToLowerInvariant() + ' obtido=' + $got) }
  return $zip
}

# --- ciclo de vida ------------------------------------------------------------------------------------------------
function Invoke-PcRollback {
  $cur = Read-PcLine (Join-Path $script:Root 'current')
  $prv = Read-PcLine (Join-Path $script:Root 'previous')
  if (-not $prv -or $prv -eq $cur -or -not (Test-Path -LiteralPath (Join-Path $script:Root ('versions\' + $prv)) -PathType Container)) {
    $c = $cur; if (-not $c) { $c = 'nenhum' }
    Write-PcError 'install.no_previous_version' $script:Root ('current=' + $c)
  }
  Publish-PcCurrent $prv
  Remove-Item -LiteralPath (Join-Path $script:Root 'previous') -Force -ErrorAction SilentlyContinue
  Write-PcOk 'rollback' ('"version":' + (ConvertTo-PcJsonString $prv) + ',"from":' + (ConvertTo-PcJsonString $cur)) `
    ('prevcarga: rollback de ' + $cur + ' para ' + $prv + ' em ' + $script:Root)
}

function Invoke-PcUninstall {
  if (-not $Yes) { Write-PcError 'install.usage' '-Uninstall' 'confirme com -Yes' }
  $st = Get-PcConfigValue 'state_dir'
  if (-not $st) { $st = Join-Path $script:Root 'state' }
  if ($script:State) { $st = $script:State }
  if (-not (Test-Path -LiteralPath $script:Root)) {
    Write-PcOk 'uninstall' '"removed":false' ('prevcarga: nada a remover em ' + $script:Root); return
  }
  try {
    foreach ($n in 'versions', 'bin') { $p = Join-Path $script:Root $n; if (Test-Path -LiteralPath $p) { Remove-Item -LiteralPath $p -Recurse -Force } }
    foreach ($n in 'current', 'previous', 'config.yaml') { $p = Join-Path $script:Root $n; if (Test-Path -LiteralPath $p) { Remove-Item -LiteralPath $p -Force } }
  } catch { Write-PcError 'install.io_failed' $script:Root $_.Exception.Message }
  [void](Sync-PcUserPath (Join-Path $script:Root 'bin') $true)
  $purged = $false
  if ($Purge) {
    $home1 = [Environment]::GetFolderPath('UserProfile')
    $stn = (ConvertTo-PcSlash $st).TrimEnd('/')
    if (-not $stn -or $stn -match '^[A-Za-z]:$' -or $stn -ieq (ConvertTo-PcSlash $home1).TrimEnd('/')) {
      Write-PcError 'install.usage' '-Purge' ('estado recusado: ' + $st)
    }
    try { if (Test-Path -LiteralPath $st) { Remove-Item -LiteralPath $st -Recurse -Force } } catch { Write-PcError 'install.io_failed' $st $_.Exception.Message }
    $purged = $true
  }
  if ((Test-Path -LiteralPath $script:Root) -and -not @(Get-ChildItem -LiteralPath $script:Root -Force).Count) {
    Remove-Item -LiteralPath $script:Root -Force -ErrorAction SilentlyContinue
  }
  $pj = 'false'; $pt = 'preservado'; if ($purged) { $pj = 'true'; $pt = 'removido' }
  Write-PcOk 'uninstall' ('"removed":true,"purged":' + $pj) ('prevcarga: desinstalado de ' + $script:Root + ' (estado ' + $pt + ')')
}

function Invoke-PcInstall {
  Test-PcPreflight
  $zip = Get-PcVerifiedBundle
  if (-not $script:Version) {  # -Bundle sem -Version: prevcarga-<v>-windows-x86_64.zip
    if ((Split-Path -Leaf $zip) -match '^prevcarga-(.+)-windows-x86_64\.zip$') { $script:Version = $Matches[1] }
    else { Write-PcError 'install.usage' '-Bundle' 'nao foi possivel inferir a versao; use -Version' }
  }
  $v = $script:Version
  if ($v -notmatch '^[A-Za-z0-9][A-Za-z0-9._+-]*$') { Write-PcError 'install.usage' '-Version' ('versao invalida: ' + $v) }

  # a partir daqui a raiz pode ser escrita
  $versions = Join-Path $script:Root 'versions'
  try { [void](New-Item -ItemType Directory -Force -Path $versions) } catch { Write-PcError 'install.io_failed' $script:Root $_.Exception.Message }
  $script:PcStage = Join-Path $versions ('.stage-' + $v)
  if (Test-Path -LiteralPath $script:PcStage) { Remove-Item -LiteralPath $script:PcStage -Recurse -Force }
  try { Expand-Archive -LiteralPath $zip -DestinationPath $script:PcStage -Force }
  catch { Write-PcError 'install.manifest_invalid' $zip ('falha ao extrair: ' + $_.Exception.Message) }
  $content = $script:PcStage
  $kids = @(Get-ChildItem -LiteralPath $script:PcStage -Force)
  if (-not (Test-Path -LiteralPath (Join-Path $script:PcStage 'RUNTIME.json')) -and $kids.Count -eq 1 -and $kids[0].PSIsContainer) { $content = $kids[0].FullName }
  $cat = Join-Path $content 'code\install\error-catalog.tsv'
  if (Test-Path -LiteralPath $cat) { $script:PcCatalog = $cat }

  # valida TODO files[] do RUNTIME.json com o Rscript.exe do proprio stage
  $rs = Join-Path $content 'runtime\R\bin\Rscript.exe'
  if (-not (Test-Path -LiteralPath $rs) -or -not (Test-Path -LiteralPath (Join-Path $content 'RUNTIME.json'))) {
    Write-PcError 'install.manifest_invalid' (Join-Path $content 'RUNTIME.json') 'bundle incompleto'
  }
  $rcode = "r <- commandArgs(TRUE)[1]; lib <- file.path(r, 'code/scripts/lib'); options(prevcarga.lib = lib); source(file.path(lib, 'errors.R')); source(file.path(lib, 'runtime.R')); prevcarga_runtime_validate(r, strict = TRUE)"
  $res = Invoke-PcNative $rs @('--vanilla', '-e', $rcode, $content)
  if ($res[0] -ne 0) { Write-PcError 'install.manifest_invalid' (Join-Path $content 'RUNTIME.json') }

  $prevCur = Read-PcLine (Join-Path $script:Root 'current')
  $dest = Join-Path $versions $v
  $old = Join-Path $versions ('.old-' + $v)
  try {
    if (Test-Path -LiteralPath $dest) {  # reinstalacao da mesma versao: troca o diretorio
      if (Test-Path -LiteralPath $old) { Remove-Item -LiteralPath $old -Recurse -Force }
      Move-Item -LiteralPath $dest -Destination $old
    }
    Move-Item -LiteralPath $content -Destination $dest
  } catch { Write-PcError 'install.io_failed' $dest $_.Exception.Message }
  Remove-Item -LiteralPath $script:PcStage -Recurse -Force -ErrorAction SilentlyContinue
  $script:PcStage = ''

  $bin = Join-Path $script:Root 'bin'
  $launcher = Join-Path $dest 'code\install\prevcarga.cmd'
  if (-not (Test-Path -LiteralPath $launcher)) { $launcher = Join-Path $PSScriptRoot 'prevcarga.cmd' }  # bundles antigos
  if (-not (Test-Path -LiteralPath $launcher)) { Write-PcError 'install.manifest_invalid' $launcher 'lancador ausente' }
  try {
    [void](New-Item -ItemType Directory -Force -Path $bin)
    $tmpl = Join-Path $bin '.prevcarga.cmd.new'
    Copy-Item -LiteralPath $launcher -Destination $tmpl -Force
    Move-Item -LiteralPath $tmpl -Destination (Join-Path $bin 'prevcarga.cmd') -Force
  } catch { Write-PcError 'install.io_failed' (Join-Path $bin 'prevcarga.cmd') $_.Exception.Message }
  Publish-PcCurrent $v
  if (Test-Path -LiteralPath $old) { Remove-Item -LiteralPath $old -Recurse -Force -ErrorAction SilentlyContinue }

  # previous = versao que estava em current (se outra); retencao N=2 = current + previous
  if ($prevCur -and $prevCur -ne $v -and (Test-Path -LiteralPath (Join-Path $versions $prevCur) -PathType Container)) {
    Write-PcFileSwap (Join-Path $script:Root 'previous') $prevCur
  } else {
    $prevCur = Read-PcLine (Join-Path $script:Root 'previous')
    if (-not $prevCur -or $prevCur -eq $v -or -not (Test-Path -LiteralPath (Join-Path $versions $prevCur) -PathType Container)) {
      Remove-Item -LiteralPath (Join-Path $script:Root 'previous') -Force -ErrorAction SilentlyContinue
      $prevCur = ''
    }
  }
  foreach ($x in (Get-PcInstalledVersions)) {
    if ($x -eq $v -or ($prevCur -and $x -eq $prevCur)) { continue }
    Remove-Item -LiteralPath (Join-Path $versions $x) -Recurse -Force -ErrorAction SilentlyContinue
  }

  Write-PcConfig
  try { [void](New-Item -ItemType Directory -Force -Path $script:StateEffective) } catch { Write-PcError 'install.io_failed' $script:StateEffective $_.Exception.Message }
  $pathUpdated = Sync-PcUserPath $bin $false

  $d = Invoke-PcNative (Join-Path $bin 'prevcarga.cmd') @('doctor', '--format', 'json')
  $doctor = $d[1].Trim()
  if (-not $doctor.StartsWith('{')) { $doctor = 'null' } else { $doctor = ($doctor -replace "[`r`n]+", '') }
  $retained = @($v); if ($prevCur) { $retained = @($prevCur, $v) }
  $pj = 'false'; if ($pathUpdated) { $pj = 'true' }
  $extra = '"install_root":' + (ConvertTo-PcJsonString (ConvertTo-PcSlash $script:Root)) + ',"version":' + (ConvertTo-PcJsonString $v) +
    ',"current":' + (ConvertTo-PcJsonString $v) + ',"previous":' + (ConvertTo-PcJsonString $prevCur) +
    ',"retained":[' + (($retained | ForEach-Object { ConvertTo-PcJsonString $_ }) -join ',') + ']' +
    ',"state_dir":' + (ConvertTo-PcJsonString (ConvertTo-PcSlash $script:StateEffective)) + ',"path_updated":' + $pj + ',"doctor":' + $doctor
  Write-PcOk 'install' $extra ('prevcarga: instalado ' + $v + ' em ' + $script:Root + ' (bin: ' + (Join-Path $bin 'prevcarga.cmd') + ')')
}

function Invoke-PcMain {
  $bad = Resolve-PcRest $Rest
  if ($script:ShowHelp) {
    Write-PcStream 'out' "uso: install.ps1 [-Version <v>|latest] [-ReleaseBase <url>] [-Bundle <zip> [-Sha256 <h>]] [-Root <dir>] [-Data <dir>] [-State <dir>] [-SkipPlatformCheck] [-Format text|json]`n     install.ps1 -Rollback [-Root <dir>]`n     install.ps1 -Uninstall -Yes [-Purge] [-Root <dir>]"
    return
  }
  if ($script:Format -ne 'json' -and $script:Format -ne 'text') { Write-PcError 'install.usage' '-Format' 'use text ou json' }
  if ($bad) { Write-PcError 'install.usage' $bad[0] $bad[1] }
  if ($Rollback -and $Uninstall) { Write-PcError 'install.usage' '-Rollback -Uninstall' 'opcoes exclusivas' }
  if ($Purge -and -not $Uninstall) { Write-PcError 'install.usage' '-Purge' 'so vale com -Uninstall' }
  if ($Yes -and -not $Uninstall) { Write-PcError 'install.usage' '-Yes' 'so vale com -Uninstall' }
  if ($script:Sha256 -and -not $script:Bundle) { Write-PcError 'install.usage' '-Sha256' 'so vale com -Bundle' }
  if ($script:Bundle -and $script:ReleaseBase) { Write-PcError 'install.usage' '-Bundle -ReleaseBase' 'opcoes exclusivas' }

  if (-not $script:Root) {  # so o DEFAULT da raiz consulta o sistema
    $la = [Environment]::GetFolderPath('LocalApplicationData')
    if (-not $la) { Write-PcError 'install.usage' '-Root' 'sem LocalApplicationData: informe -Root' }
    $script:Root = Join-Path $la 'prevcarga'
  }
  $script:Root = (Get-PcAbsPath $script:Root).TrimEnd('\', '/')
  if (-not $script:Root -or $script:Root -match '^[A-Za-z]:$') { Write-PcError 'install.usage' '-Root' 'raiz nao pode ser a raiz do disco' }
  if ($script:State) { $script:State = Get-PcAbsPath $script:State }
  if ($script:Data) { $script:Data = Get-PcAbsPath $script:Data }
  if ($script:Bundle) { $script:Bundle = Get-PcAbsPath $script:Bundle }
  if ($script:State -and (Test-PcInside $script:State (Join-Path $script:Root 'versions'))) { Write-PcError 'install.state_inside_versions' $script:State }
  $script:StateEffective = ''

  if ($Rollback) { Invoke-PcRollback }
  elseif ($Uninstall) { Invoke-PcUninstall }
  else { Invoke-PcInstall }
}

try {
  Invoke-PcMain
  $script:PcExit = 0
} catch {
  if ($_.Exception.Message -ne 'PCEXIT') {
    $script:PcExit = Write-PcErrorOut 'install.io_failed' 'install.ps1' $_.Exception.Message
  }
} finally {
  if ($script:PcStage -and (Test-Path -LiteralPath $script:PcStage)) { Remove-Item -LiteralPath $script:PcStage -Recurse -Force -ErrorAction SilentlyContinue }
  if ($script:PcTemp -and (Test-Path -LiteralPath $script:PcTemp)) { Remove-Item -LiteralPath $script:PcTemp -Recurse -Force -ErrorAction SilentlyContinue }
}
exit $script:PcExit
