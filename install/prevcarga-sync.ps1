# prevcarga-sync.ps1 -- sincroniza os artefatos e as inferencias OFICIAIS do bucket publico para o registry local, no Windows nativo
# (M8.6; ADR-0039, contrato de erros ADR-0040). Windows PowerShell 5.1 e pwsh 7; roda com `-ExecutionPolicy Bypass` (ver prevcarga-sync.cmd);
# NAO precisa de administrador. Espelha install/prevcarga-sync.sh: mesma gramatica, mesma semantica, mesmos error.id e exits.
# FORA do pacote R: so LEITURA HTTP(S), sem credencial, sem variavel de ambiente (o pacote R nunca fala rede).
#
# Uso:
#   prevcarga-sync -What artifacts|forecasts [-ReleaseBase <url>] [-State <dir>] [-Model <m>]... [-Area <a>]...
#                  [-Origin <AAAA-MM-DD>] [-Format text|json]
# A grafia do sh (--what, --release-base, --state, --model, --area, --origin, --format) tambem e aceita.
#
# Atomicidade: tudo e baixado e conferido (sha256 contra o SHA256SUMS da chave) num staging `.tmp-*` do registry ANTES de qualquer
# rename; so entao cada v_<hash> vira publico (Move-Item) e so DEPOIS o latest.txt local e regravado (tempfile `.tmp-*` + Move-Item -Force).
# Qualquer falha antes do rename deixa <State> byte-identico ao de antes (o finally remove o staging). Um v_<hash> local ja presente e
# CONFERIDO, nunca sobrescrito: sha divergente => sync.sha_mismatch (exit 9). Rede => sync.network (exit 10).

param(
  [string]$What, [string]$ReleaseBase, [string]$State, [string[]]$Model, [string[]]$Area, [string]$Origin,
  [string]$Format = 'text', [switch]$Help,
  [Parameter(ValueFromRemainingArguments = $true)][string[]]$Rest
)

Set-StrictMode -Version 3
$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$script:PcExit = 0
$script:PcVersion = '1.0.0'
$script:PcCatalog = Join-Path $PSScriptRoot 'error-catalog.tsv'   # GERADO (uniao de scripts/lib/errors/*.tsv), copiado ao lado pelo install.ps1
$script:PcStage = ''
$script:PcTemp = ''
$script:MadeS = $false
$script:MadeW = $false
$script:Models = New-Object System.Collections.Generic.List[string]
$script:Areas = New-Object System.Collections.Generic.List[string]

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

function ConvertTo-PcJsonArray([string[]]$Items) {
  $x = @($Items | Where-Object { $_ })
  return '[' + (($x | ForEach-Object { ConvertTo-PcJsonString $_ }) -join ',') + ']'
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

# --- erros (ADR-0040) ----------------------------------------------------------------------------------------------
function Get-PcExitForClass([string]$Class) {
  switch ($Class) { 'bundle' { return 6 } 'config' { return 8 } 'integrity' { return 9 } 'network' { return 10 } default { return 1 } }
}

function Get-PcEmbeddedRow([string]$Id) {  # mesmo texto de scripts/lib/errors/sync.tsv (usado sem o catalogo gerado ao lado)
  switch ($Id) {
    'sync.io_failed' { return @('bundle', 'falha de escrita ou utilitario ausente no registry local', 'confira permissoes e espaco em disco do --state e se o prevcarga instalado esta ao lado do prevcarga-sync') }
    'sync.network' { return @('network', 'falha de rede ao baixar do release-base', 'confira --release-base e a conexao e repita; nada foi gravado no registry local') }
    'sync.sha_mismatch' { return @('integrity', 'sha256 de um objeto nao confere com o SHA256SUMS publicado', 'baixe de novo; se persistir o publicado esta corrompido; nada foi gravado no registry local') }
    default { return @('config', 'argumento invalido ou ausente do prevcarga-sync', 'veja prevcarga-sync --help (-Help no PowerShell)') }
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
    $j = '{"ok":false,"exit_code":' + $code + ',"verb":"prevcarga-sync","error":{"id":' + (ConvertTo-PcJsonString $Id) + ',"class":' + (ConvertTo-PcJsonString $row[0]) +
      ',"message":' + (ConvertTo-PcJsonString $row[1]) + ',"hint":' + (ConvertTo-PcJsonString $row[2]) +
      ',"where":' + (ConvertTo-PcJsonString $Where) + ',"details":' + (ConvertTo-PcJsonString $Details) + '}}'
    Write-PcStream 'out' $j
  } else {
    $t = 'prevcarga: erro [' + $row[0] + '] (exit ' + $code + ")`no que: " + $row[1] + "`nonde: " + $Where + "`ncomo corrigir: " + $row[2]
    Write-PcStream 'err' (ConvertTo-PcAsciiText $t)
  }
  return $code
}

function Write-PcError([string]$Id, [string]$Where, [string]$Details = '') {  # emite e interrompe (o finally roda no topo)
  $script:PcExit = Write-PcErrorOut $Id $Where $Details
  throw 'PCEXIT'
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
    if ($name -in 'what', 'releasebase', 'release-base', 'state', 'model', 'area', 'origin', 'format') {
      if ($null -eq $val) {
        if ($i + 1 -ge $Tokens.Count) { $bad = @($tok, 'falta o valor'); break }
        $i++; $val = $Tokens[$i]
      }
      if (-not $val) { $bad = @($tok, 'valor vazio'); break }
      if ($name -eq 'what') { $script:What = $val }
      elseif ($name -eq 'releasebase' -or $name -eq 'release-base') { $script:ReleaseBase = $val }
      elseif ($name -eq 'state') { $script:State = $val }
      elseif ($name -eq 'model') { foreach ($x in ($val -split '[,\s]+')) { if ($x) { $script:Models.Add($x) } } }
      elseif ($name -eq 'area') { foreach ($x in ($val -split '[,\s]+')) { if ($x) { $script:Areas.Add($x) } } }
      elseif ($name -eq 'origin') { $script:Origin = $val }
      else { $script:Format = $val }
    }
    elseif ($name -eq 'help' -or $name -eq 'h') { $script:Help = [switch]$true }
    else { $bad = @($tok, 'argumento desconhecido') }
    if ($bad) { break }
    $i++
  }
  return $bad
}

function Get-PcAbsPath([string]$Path) {
  return $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath($Path)
}

function Join-PcKey([string]$Base, [string]$Key) {  # chave '/' do bucket -> caminho local do SO
  return [IO.Path]::Combine($Base, ($Key -replace '/', [string][IO.Path]::DirectorySeparatorChar))
}

function Test-PcSafeRel([string]$Rel) {  # rejeita caminho absoluto, '..' e vazio vindos do SHA256SUMS publicado
  if (-not $Rel) { return $false }
  if ($Rel.StartsWith('/') -or $Rel.StartsWith('\') -or $Rel -match '^[A-Za-z]:') { return $false }
  foreach ($seg in ($Rel -split '[\\/]')) { if ($seg -eq '..') { return $false } }
  return $true
}

# --- transporte ------------------------------------------------------------------------------------------------
function Get-PcFile([string]$Url, [string]$Dest) {  # 0 ok; 44 = HTTP 404 (objeto ausente); 1 = qualquer outra falha (conexao cortada, 5xx, DNS)
  try {
    [Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12
    Invoke-WebRequest -UseBasicParsing -Uri $Url -OutFile $Dest | Out-Null
    return 0
  } catch {
    $resp = $null
    if ($_.Exception.PSObject.Properties['Response']) { $resp = $_.Exception.Response }
    if ($resp -and ([int]$resp.StatusCode) -eq 404) { return 44 }
    return 1
  }
}

function Get-PcSha([string]$Path) { return (Get-FileHash -Algorithm SHA256 -LiteralPath $Path).Hash.ToLowerInvariant() }

function Read-PcLines([string]$Path) {
  return @([IO.File]::ReadAllText($Path) -split "`n" | ForEach-Object { $_.TrimEnd("`r") } | Where-Object { $_ })
}

# --- enumeracao do conjunto oficial (so artifacts; o bucket nunca e listado) -----------------------------------------
function Invoke-PcCli([string]$Verb) {  # JSON do CLI instalado ao lado (prevcarga.cmd no Windows)
  $cli = Join-Path $PSScriptRoot 'prevcarga.cmd'
  if (-not (Test-Path -LiteralPath $cli)) { $cli = Join-Path $PSScriptRoot 'prevcarga' }
  if (-not (Test-Path -LiteralPath $cli)) { Write-PcError 'sync.io_failed' $cli 'prevcarga instalado ao lado nao encontrado' }
  $prev = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try { $out = & $cli $Verb --format json 2>$null; $code = $LASTEXITCODE } finally { $ErrorActionPreference = $prev }
  $txt = (@($out) -join "`n").Trim()
  if ($code -ne 0 -or -not $txt.StartsWith('{')) { Write-PcError 'sync.io_failed' ($cli + ' ' + $Verb) ('falha ao enumerar: ' + $Verb) }
  try { return ($txt | ConvertFrom-Json) } catch { Write-PcError 'sync.io_failed' ($cli + ' ' + $Verb) 'JSON invalido' }
}

function Get-PcPrefixes {
  $m = Invoke-PcCli 'models'
  $a = Invoke-PcCli 'areas'
  $leaves = @($a.result.subsystems | ForEach-Object { $_.areas } | Where-Object { $_ })
  $order = @($a.result.order | Where-Object { $_ })
  $mem = @($m.result.members | ForEach-Object { $_ })
  if ($mem.Count -eq 0 -or $leaves.Count -eq 0 -or $order.Count -eq 0) { Write-PcError 'sync.io_failed' 'prevcarga' 'enumeracao vazia (models/areas)' }
  foreach ($x in $script:Models) {   # filtro que nao casa com nada do conjunto oficial e erro de uso (como o release-pack)
    if (-not ($mem | Where-Object { $_.mdir -ceq $x })) { Write-PcError 'sync.usage' '--model' ('modelo desconhecido: ' + $x) }
  }
  foreach ($x in $script:Areas) {
    if (($leaves -cnotcontains $x) -and ($order -cnotcontains $x)) { Write-PcError 'sync.usage' '--area' ('area desconhecida: ' + $x) }
  }
  $p = New-Object System.Collections.Generic.List[string]
  foreach ($e in $mem) {
    if ($script:Models.Count -gt 0 -and ($script:Models -cnotcontains $e.mdir)) { continue }
    $exo = ''
    if ($e.exogena) { $exo = '/' + $e.exogena }
    foreach ($l in $leaves) { $p.Add('artifacts/' + $e.mdir + $exo + '/' + $l) }
  }
  foreach ($l in $leaves) { $p.Add('artifacts/combina/' + $l) }
  foreach ($s in $order) { $p.Add('artifacts/reconcile/' + $s) }
  $sel = @($p | Where-Object { $script:Areas.Count -eq 0 -or ($script:Areas -ccontains ($_ -replace '^.*/', '')) })
  $u = New-Object 'System.Collections.Generic.SortedSet[string]' ([StringComparer]::Ordinal)   # ordem radix (= LC_ALL=C do sh), sem repetidos
  foreach ($x in $sel) { [void]$u.Add($x) }
  return @($u)
}

# --- protocolo por chave ------------------------------------------------------------------------------------------
$script:Baixados = New-Object System.Collections.Generic.List[string]
$script:Pulados = New-Object System.Collections.Generic.List[string]
$script:Trocados = New-Object System.Collections.Generic.List[string]
$script:Ausentes = New-Object System.Collections.Generic.List[string]
$script:Pending = New-Object System.Collections.Generic.List[string]
$script:Pointers = New-Object System.Collections.Generic.List[object]

function Initialize-PcStage {  # staging unico do registry (`.tmp-*`, ADR-0030), no MESMO volume dos destinos: criado so ao baixar o 1o objeto
  if ($script:PcStage) { return }
  $w = Join-Path $script:State $script:What
  if (-not (Test-Path -LiteralPath $script:State)) { $script:MadeS = $true }
  if (-not (Test-Path -LiteralPath $w)) { $script:MadeW = $true }
  try {
    [void](New-Item -ItemType Directory -Force -Path $w)
    $script:PcStage = Join-Path $w ('.tmp-' + [guid]::NewGuid().ToString('N').Substring(0, 8))
    [void](New-Item -ItemType Directory -Path $script:PcStage)
  } catch { Write-PcError 'sync.io_failed' $w $_.Exception.Message }
}

function Test-PcLocalKey([string]$Dir, [string]$SumsFile) {  # todos os arquivos listados e o SHA256SUMS local conferem
  $local = Join-Path $Dir 'SHA256SUMS'
  if (-not (Test-Path -LiteralPath $local -PathType Leaf)) { return $false }
  if ((Get-PcSha $local) -ne (Get-PcSha $SumsFile)) { return $false }
  foreach ($l in (Read-PcLines $SumsFile)) {
    $parts = $l -split '\s+', 2
    $f = $parts[1].TrimStart('*')
    if (-not (Test-PcSafeRel $f)) { return $false }
    $p = Join-PcKey $Dir $f
    if (-not (Test-Path -LiteralPath $p -PathType Leaf)) { return $false }
    if ((Get-PcSha $p) -ne $parts[0].ToLowerInvariant()) { return $false }
  }
  return $true
}

function Sync-PcKey([string]$Key) {  # chave relativa a <State>, ex.: artifacts/lgbm/SECO/v_ab12
  $url = $script:ReleaseBase + '/' + $Key
  $sums = Join-Path $script:PcTemp 'sums'
  if ((Get-PcFile ($url + '/SHA256SUMS') $sums) -ne 0) { Write-PcError 'sync.network' ($url + '/SHA256SUMS') }
  $local = Join-PcKey $script:State $Key
  if (Test-Path -LiteralPath $local -PathType Container) {
    if (-not (Test-PcLocalKey $local $sums)) { Write-PcError 'sync.sha_mismatch' $local 'versao local divergente do publicado; nao foi sobrescrita' }
    $script:Pulados.Add($Key)
    return
  }
  Initialize-PcStage
  $sd = Join-PcKey $script:PcStage ($Key -replace '^[^/]+/', '')
  [void](New-Item -ItemType Directory -Force -Path $sd)
  foreach ($l in (Read-PcLines $sums)) {
    $parts = $l -split '\s+', 2
    $h = $parts[0].ToLowerInvariant()
    $f = $parts[1].TrimStart('*')
    if (-not (Test-PcSafeRel $f)) { Write-PcError 'sync.sha_mismatch' ($Key + '/' + $f) 'caminho invalido no SHA256SUMS publicado' }
    $out = Join-PcKey $sd $f
    [void](New-Item -ItemType Directory -Force -Path (Split-Path -Parent $out))
    if ((Get-PcFile ($url + '/' + $f) $out) -ne 0) { Write-PcError 'sync.network' ($url + '/' + $f) }
    if ((Get-PcSha $out) -ne $h) { Write-PcError 'sync.sha_mismatch' ($Key + '/' + $f) ('esperado=' + $h) }
  }
  try { Copy-Item -LiteralPath $sums -Destination (Join-Path $sd 'SHA256SUMS') -Force } catch { Write-PcError 'sync.io_failed' $sd $_.Exception.Message }
  $script:Pending.Add($Key)
}

function Sync-PcPrefix([string]$P) {  # le o latest.txt do prefixo e sincroniza a versao apontada
  $lt = Join-Path $script:PcTemp 'latest'
  $rc = Get-PcFile ($script:ReleaseBase + '/' + $P + '/latest.txt') $lt
  if ($rc -eq 44) { $script:Ausentes.Add($P); return }
  if ($rc -ne 0) { Write-PcError 'sync.network' ($script:ReleaseBase + '/' + $P + '/latest.txt') }
  $lines = Read-PcLines $lt
  $v = ''
  if ($lines.Count -gt 0) { $v = $lines[0].Trim() }
  if ($v -notmatch '^v_[0-9a-f]+$') { Write-PcError 'sync.sha_mismatch' ($script:ReleaseBase + '/' + $P + '/latest.txt') 'latest.txt nao aponta para v_<hash>' }
  $script:Pointers.Add(@($P, $v))
  Sync-PcKey ($P + '/' + $v)
}

function Invoke-PcMain {
  $bad = $null
  foreach ($x in $Model) { foreach ($y in ($x -split '[,\s]+')) { if ($y) { $script:Models.Add($y) } } }
  foreach ($x in $Area) { foreach ($y in ($x -split '[,\s]+')) { if ($y) { $script:Areas.Add($y) } } }
  # pre-passe: o formato vale tambem para erros de argumento
  for ($i = 0; $Rest -and $i -lt $Rest.Count; $i++) {
    if ($Rest[$i] -match '^--?format=(.*)$' -and $Matches[1] -eq 'json') { $script:Format = 'json' }
    elseif ($Rest[$i] -match '^--?format$' -and ($i + 1) -lt $Rest.Count -and $Rest[$i + 1] -eq 'json') { $script:Format = 'json' }
  }
  $bad = Resolve-PcRest $Rest
  if ($script:Help) {
    Write-PcStream 'out' "uso: prevcarga-sync -What artifacts|forecasts [-ReleaseBase <url>] [-State <dir>] [-Model <m>]... [-Area <a>]... [-Origin <AAAA-MM-DD>] [-Format text|json]"
    return
  }
  if ($script:Format -ne 'json' -and $script:Format -ne 'text') { Write-PcError 'sync.usage' '-Format' 'use text ou json' }
  if ($bad) { Write-PcError 'sync.usage' $bad[0] $bad[1] }
  if (-not $script:What) { Write-PcError 'sync.usage' '-What' 'informe artifacts ou forecasts' }
  if ($script:What -ne 'artifacts' -and $script:What -ne 'forecasts') { Write-PcError 'sync.usage' '-What' 'use artifacts ou forecasts' }
  if ($script:What -eq 'forecasts') {
    if (-not $script:Origin) { Write-PcError 'sync.usage' '-Origin' 'informe -Origin AAAA-MM-DD com -What forecasts' }
    if ($script:Models.Count -gt 0 -or $script:Areas.Count -gt 0) { Write-PcError 'sync.usage' '-Model -Area' 'so valem com -What artifacts' }
    $dt = [datetime]::MinValue
    if ($script:Origin -notmatch '^[0-9]{4}-[0-9]{2}-[0-9]{2}$') { Write-PcError 'sync.usage' '-Origin' ('formato AAAA-MM-DD: ' + $script:Origin) }
    if (-not [datetime]::TryParseExact($script:Origin, 'yyyy-MM-dd', [Globalization.CultureInfo]::InvariantCulture, [Globalization.DateTimeStyles]::None, [ref]$dt)) {
      Write-PcError 'sync.usage' '-Origin' ('data invalida: ' + $script:Origin)
    }
  } elseif ($script:Origin) { Write-PcError 'sync.usage' '-Origin' 'so vale com -What forecasts' }

  if (-not $script:ReleaseBase) { $script:ReleaseBase = 'https://prevcarga.s3.amazonaws.com' }   # [TBD: ADR-0039]; o mesmo default do install.ps1
  $script:ReleaseBase = $script:ReleaseBase.TrimEnd('/')
  if ($script:ReleaseBase -notmatch '^https?://.+') { Write-PcError 'sync.usage' '-ReleaseBase' ('informe uma URL http(s): ' + $script:ReleaseBase) }
  if (-not $script:State) {  # flags > <R>\config.yaml > default
    $cfg = Join-Path (Split-Path -Parent $PSScriptRoot) 'config.yaml'
    if (Test-Path -LiteralPath $cfg) {
      $script:State = Join-Path (Split-Path -Parent $PSScriptRoot) 'state'
      foreach ($line in [IO.File]::ReadAllLines($cfg)) {
        if ($line.StartsWith('state_dir:')) {
          $v = $line.Substring(10).Trim()
          if ($v.Length -ge 2 -and $v.StartsWith("'") -and $v.EndsWith("'")) { $v = $v.Substring(1, $v.Length - 2).Replace("''", "'") }
          if ($v) { $script:State = $v }
          break
        }
      }
    }
  }
  if (-not $script:State) { Write-PcError 'sync.usage' '-State' 'informe -State <dir>' }
  $script:State = (Get-PcAbsPath $script:State).TrimEnd('\', '/')
  if (-not $script:State -or $script:State -match '^[A-Za-z]:$') { Write-PcError 'sync.usage' '-State' 'estado nao pode ser a raiz do disco' }

  $script:PcTemp = Join-Path ([IO.Path]::GetTempPath()) ('prevcarga-sync-' + [guid]::NewGuid().ToString('N'))
  try { [void](New-Item -ItemType Directory -Path $script:PcTemp) } catch { Write-PcError 'sync.io_failed' $script:PcTemp $_.Exception.Message }

  $prefixos = @()
  if ($script:What -eq 'artifacts') {
    $prefixos = @(Get-PcPrefixes)
    if ($prefixos.Count -eq 0) { Write-PcError 'sync.usage' '-Model -Area' 'os filtros nao selecionam nenhum prefixo oficial' }
    foreach ($p in $prefixos) { Sync-PcPrefix $p }
    if ($script:Pointers.Count -eq 0) { Write-PcError 'sync.network' $script:ReleaseBase 'nenhum prefixo oficial publicado em -ReleaseBase' }
  } else {
    $lt = Join-Path $script:PcTemp 'latest'
    $u = $script:ReleaseBase + '/forecasts/' + $script:Origin + '/latest.txt'
    if ((Get-PcFile $u $lt) -ne 0) { Write-PcError 'sync.network' $u }
    foreach ($k in (Read-PcLines $lt)) {
      $k = $k.Trim()
      if ($k -notmatch '^[0-9]{8}T[0-9]{4}/.+' -or -not (Test-PcSafeRel $k)) { Write-PcError 'sync.sha_mismatch' $u ('chave invalida: ' + $k) }
      $key = 'forecasts/' + $script:Origin + '/' + $k
      $prefixos += $key
      Sync-PcKey $key
    }
    if ($prefixos.Count -eq 0) { Write-PcError 'sync.network' $u 'nenhuma chave publicada' }
  }

  # fase 2: tudo conferido; so agora cada chave vira publica (rename) e, por ultimo, o latest.txt local (ponteiro mutavel)
  foreach ($key in $script:Pending) {
    $dst = Join-PcKey $script:State $key
    try {
      [void](New-Item -ItemType Directory -Force -Path (Split-Path -Parent $dst))
      Move-Item -LiteralPath (Join-PcKey $script:PcStage ($key -replace '^[^/]+/', '')) -Destination $dst
    } catch { Write-PcError 'sync.io_failed' $dst $_.Exception.Message }
    $script:Baixados.Add($key)
  }
  if ($script:What -eq 'artifacts') {
    foreach ($pv in $script:Pointers) {
      $dir = Join-PcKey $script:State $pv[0]
      $cur = Join-Path $dir 'latest.txt'
      $now = ''
      if (Test-Path -LiteralPath $cur -PathType Leaf) { $l = Read-PcLines $cur; if ($l.Count -gt 0) { $now = $l[0].Trim() } }
      if ($now -eq $pv[1]) { continue }
      $tmp = Join-Path $dir ('.tmp-latest-' + [guid]::NewGuid().ToString('N').Substring(0, 8))
      try {
        [IO.File]::WriteAllText($tmp, $pv[1] + "`n", (New-Object Text.UTF8Encoding $false))
        Move-Item -LiteralPath $tmp -Destination $cur -Force
      } catch {
        if (Test-Path -LiteralPath $tmp) { Remove-Item -LiteralPath $tmp -Force -ErrorAction SilentlyContinue }
        Write-PcError 'sync.io_failed' $cur $_.Exception.Message
      }
      $script:Trocados.Add($pv[0] + '/latest.txt')
    }
  }

  if ($Format -eq 'json') {
    $j = '{"ok":true,"exit_code":0,"verb":"prevcarga-sync","error":null,"warnings":[],"result":{"what":' + (ConvertTo-PcJsonString $script:What) +
      ',"prefixos":' + (ConvertTo-PcJsonArray $prefixos) + ',"baixados":' + (ConvertTo-PcJsonArray $script:Baixados.ToArray()) +
      ',"pulados":' + (ConvertTo-PcJsonArray $script:Pulados.ToArray()) + ',"latest_trocados":' + (ConvertTo-PcJsonArray $script:Trocados.ToArray()) +
      ',"ausentes":' + (ConvertTo-PcJsonArray $script:Ausentes.ToArray()) + '},"run_id":null,"version":' + (ConvertTo-PcJsonString $script:PcVersion) + '}'
    Write-PcStream 'out' $j
  } else {
    Write-PcStream 'out' (ConvertTo-PcAsciiText ('prevcarga-sync: ' + $script:What + ': ' + $script:Baixados.Count + ' baixados, ' + $script:Pulados.Count + ' ja presentes (estado ' + $script:State + ')'))
  }
}

try {
  Invoke-PcMain
  $script:PcExit = 0
} catch {
  if ($_.Exception.Message -ne 'PCEXIT') {
    $script:PcExit = Write-PcErrorOut 'sync.io_failed' 'prevcarga-sync.ps1' $_.Exception.Message
  }
} finally {
  if ($script:PcStage -and (Test-Path -LiteralPath $script:PcStage)) { Remove-Item -LiteralPath $script:PcStage -Recurse -Force -ErrorAction SilentlyContinue }
  if ($script:PcTemp -and (Test-Path -LiteralPath $script:PcTemp)) { Remove-Item -LiteralPath $script:PcTemp -Recurse -Force -ErrorAction SilentlyContinue }
  # diretorios que ESTA execucao criou e ficaram vazios (falha antes de qualquer rename): a arvore volta a ser a de antes
  if ($script:MadeW) {
    $w = Join-Path $script:State $script:What
    if ((Test-Path -LiteralPath $w) -and -not (Get-ChildItem -LiteralPath $w -Force)) { Remove-Item -LiteralPath $w -Force -ErrorAction SilentlyContinue }
  }
  if ($script:MadeS -and (Test-Path -LiteralPath $script:State) -and -not (Get-ChildItem -LiteralPath $script:State -Force)) {
    Remove-Item -LiteralPath $script:State -Force -ErrorAction SilentlyContinue
  }
}
exit $script:PcExit
