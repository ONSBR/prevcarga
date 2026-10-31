# prevcarga-sync.Tests.ps1 -- Pester 5.5.0 (versao FIXA) para install\prevcarga-sync.ps1 e install\prevcarga-sync.cmd (M8.6; ADR-0039, ADR-0040).
# Roda em Windows PowerShell 5.1 e pwsh 7 (o utilitario e executado nos DOIS, em processo filho). Espelha tests/install/test-sync.R:
# braco FORBIDDEN (tamper => exit 9, corte => exit 10, local divergente => exit 9) com o seu CONTROL (integro => arvore == bucket).
# NUNCA toca bucket real nem credencial: o "bucket" e uma pasta temporaria servida por um HttpListener local (127.0.0.1, porta livre)
# num job de fundo; o CLI instalado ao lado e um stub (.cmd) que imprime o JSON canonico de models/areas.
# [DEFERRED M8.6] NAO foi executado nesta maquina de dev (sem host Windows nem pwsh): owner Gabriel, no host Windows de release (recibo windows-vm).
#   $c = New-PesterContainer -Path install\tests\prevcarga-sync.Tests.ps1
#   Invoke-Pester -Container $c
param()

BeforeAll {
  $script:Repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
  $script:Sync = Join-Path $script:Repo 'install\prevcarga-sync.ps1'
  $script:Work = Join-Path ([IO.Path]::GetTempPath()) ('pc-pester-sync-' + [guid]::NewGuid().ToString('N'))
  [void](New-Item -ItemType Directory -Path $script:Work)
  $script:Shells = @('powershell.exe')
  if (Get-Command pwsh -ErrorAction SilentlyContinue) { $script:Shells += 'pwsh' }

  function Write-Bytes([string]$Path, [string]$Text) {
    [void](New-Item -ItemType Directory -Force -Path (Split-Path -Parent $Path))
    [IO.File]::WriteAllBytes($Path, [Text.Encoding]::ASCII.GetBytes($Text))
  }
  function Get-Sha([string]$Path) { return (Get-FileHash -Algorithm SHA256 -LiteralPath $Path).Hash.ToLowerInvariant() }

  # celula do bucket: <dir>\artifacts\<cell>\{latest.txt, v_<hex>\{artifact.rds, run_manifest.json, SHA256SUMS}}
  function New-Cell([string]$Bucket, [string]$Cell, [string]$Hex) {
    $v = 'v_' + $Hex
    $d = Join-Path $Bucket (('artifacts/' + $Cell + '/' + $v) -replace '/', '\')
    Write-Bytes (Join-Path $d 'artifact.rds') ('rds ' + $Cell + ' ' + $Hex)
    Write-Bytes (Join-Path $d 'run_manifest.json') ('{"cell":"' + $Cell + '"}')
    $sums = ''
    foreach ($f in 'artifact.rds', 'run_manifest.json') { $sums += (Get-Sha (Join-Path $d $f)) + '  ' + $f + "`n" }
    Write-Bytes (Join-Path $d 'SHA256SUMS') $sums
    Write-Bytes (Join-Path $d '..\latest.txt') ($v + "`n")
  }
  function New-Bucket {
    $b = Join-Path $script:Work ('bucket-' + [guid]::NewGuid().ToString('N').Substring(0, 8))
    $i = 4096
    foreach ($c in 'lgbm/A1', 'combina/A1', 'reconcile/SS1') { New-Cell $b $c ('{0:x12}' -f $i); $i++ }
    return $b
  }

  # CLI de teste ao lado do utilitario: imprime o JSON canonico (mesmo formato do prevcarga real) de models/areas
  function New-InstalledSync {
    $d = Join-Path $script:Work ('bin-' + [guid]::NewGuid().ToString('N').Substring(0, 8))
    [void](New-Item -ItemType Directory -Path $d)
    Copy-Item -LiteralPath $script:Sync -Destination $d
    $m = '{"ok":true,"verb":"models","exit_code":0,"error":null,"warnings":[],"result":{"schema_version":1,"members":[{"sigla":"LGBM","mdir":"lgbm","exogena":null,"split":"origin_agnostic"}]},"run_id":"x","version":"0"}'
    $a = '{"ok":true,"verb":"areas","exit_code":0,"error":null,"warnings":[],"result":{"schema_version":1,"order":["SS1"],"subsystems":[{"name":"SS1","loss":"P1","areas":["A1"]}]},"run_id":"x","version":"0"}'
    Write-Bytes (Join-Path $d 'models.json') $m
    Write-Bytes (Join-Path $d 'areas.json') $a
    [IO.File]::WriteAllText((Join-Path $d 'prevcarga.cmd'), "@echo off`r`nif ""%1""==""models"" type ""%~dp0models.json""`r`nif ""%1""==""areas"" type ""%~dp0areas.json""`r`n", [Text.Encoding]::ASCII)
    return (Join-Path $d 'prevcarga-sync.ps1')
  }

  # servidor HTTP local (job de fundo). Mode: ok | tamper | cut (corta apos N objetos)
  function Start-Bucket([string]$Dir, [string]$Mode = 'ok', [int]$N = 3) {
    $port = Get-Random -Minimum 20000 -Maximum 40000
    $job = Start-Job -ArgumentList $Dir, $Mode, $N, $port -ScriptBlock {
      param($Dir, $Mode, $N, $Port)
      $l = New-Object System.Net.HttpListener
      $l.Prefixes.Add('http://127.0.0.1:' + $Port + '/')
      $l.Start()
      $k = 0
      while ($l.IsListening) {
        $ctx = $l.GetContext()
        $rel = [Uri]::UnescapeDataString($ctx.Request.Url.AbsolutePath.TrimStart('/')) -replace '/', '\'
        $f = Join-Path $Dir $rel
        if (-not (Test-Path -LiteralPath $f -PathType Leaf)) { $ctx.Response.StatusCode = 404; $ctx.Response.Close(); continue }
        $k++
        if ($Mode -eq 'cut' -and $k -gt $N) { $ctx.Response.Abort(); continue }
        $b = [IO.File]::ReadAllBytes($f)
        if ($Mode -eq 'tamper' -and $rel -notmatch 'SHA256SUMS$|latest\.txt$' -and $b.Length -gt 0) { $b[0] = $b[0] -bxor 1 }
        $ctx.Response.OutputStream.Write($b, 0, $b.Length)
        $ctx.Response.Close()
      }
    }
    Start-Sleep -Milliseconds 800
    return @{ Url = 'http://127.0.0.1:' + $port; Job = $job }
  }
  function Stop-Bucket($S) { Stop-Job $S.Job -ErrorAction SilentlyContinue; Remove-Job $S.Job -Force -ErrorAction SilentlyContinue }

  function Get-Listing([string]$Dir) {
    if (-not (Test-Path -LiteralPath $Dir)) { return @() }
    $base = (Resolve-Path -LiteralPath $Dir).Path.TrimEnd('\')
    return @(Get-ChildItem -LiteralPath $Dir -Recurse -Force -File | Sort-Object { $_.FullName.Substring($base.Length).Replace('\', '/') } |
      ForEach-Object { $_.FullName.Substring($base.Length).Replace('\', '/') + ' ' + (Get-Sha $_.FullName) })
  }
  function Get-TmpLeft([string]$Dir) {
    if (-not (Test-Path -LiteralPath $Dir)) { return @() }
    return @(Get-ChildItem -LiteralPath $Dir -Recurse -Force | Where-Object { $_.Name.StartsWith('.tmp-') })
  }

  # roda o utilitario em processo filho; devolve @{ Status; Out; Err } (bytes crus lidos como Latin1: prova ASCII estrito)
  function Invoke-Sync([string]$Shell, [string]$Script, [string[]]$SyncArgs) {
    $psi = New-Object Diagnostics.ProcessStartInfo
    $psi.FileName = $Shell
    $q = ($SyncArgs | ForEach-Object { '"' + $_ + '"' }) -join ' '
    $psi.Arguments = '-NoProfile -ExecutionPolicy Bypass -File "' + $Script + '" ' + $q
    $psi.RedirectStandardOutput = $true; $psi.RedirectStandardError = $true; $psi.UseShellExecute = $false
    $psi.StandardOutputEncoding = [Text.Encoding]::GetEncoding(28591); $psi.StandardErrorEncoding = [Text.Encoding]::GetEncoding(28591)
    $p = [Diagnostics.Process]::Start($psi)
    $o = $p.StandardOutput.ReadToEnd(); $e = $p.StandardError.ReadToEnd(); $p.WaitForExit()
    return @{ Status = $p.ExitCode; Out = $o; Err = $e }
  }
}

AfterAll {
  if ($script:Work -and (Test-Path -LiteralPath $script:Work)) { Remove-Item -LiteralPath $script:Work -Recurse -Force -ErrorAction SilentlyContinue }
}

Describe 'prevcarga-sync.ps1' {
  It '[CONTROL] objetos integros: exit 0 e <State>\artifacts == bucket arquivo a arquivo; 2a execucao sem escrita' -ForEach @(@{ Shell = 'powershell.exe' }, @{ Shell = 'pwsh' }) {
    if (-not (Get-Command $Shell -ErrorAction SilentlyContinue)) { Set-ItResult -Skipped -Because ($Shell + ' ausente'); return }
    $b = New-Bucket; $srv = Start-Bucket $b; $sync = New-InstalledSync; $s = Join-Path $script:Work ('S' + [guid]::NewGuid().ToString('N').Substring(0, 6))
    try {
      $r = Invoke-Sync $Shell $sync @('--what', 'artifacts', '--release-base', $srv.Url, '--state', $s, '--format', 'json')
      $r.Status | Should -Be 0
      $r.Err | Should -Be ''
      (Get-Listing (Join-Path $s 'artifacts')) | Should -Be (Get-Listing (Join-Path $b 'artifacts'))
      (Get-TmpLeft $s).Count | Should -Be 0
      $before = Get-Listing (Join-Path $s 'artifacts')
      $r2 = Invoke-Sync $Shell $sync @('--what', 'artifacts', '--release-base', $srv.Url, '--state', $s, '--format', 'json')
      $r2.Status | Should -Be 0
      $j = $r2.Out | ConvertFrom-Json
      @($j.result.baixados).Count | Should -Be 0
      @($j.result.pulados).Count | Should -Be 3
      (Get-Listing (Join-Path $s 'artifacts')) | Should -Be $before
    } finally { Stop-Bucket $srv }
  }

  It '[FORBIDDEN] 1 byte adulterado: exit 9 sync.sha_mismatch, arvore intacta, sem .tmp-*' -ForEach @(@{ Shell = 'powershell.exe' }, @{ Shell = 'pwsh' }) {
    if (-not (Get-Command $Shell -ErrorAction SilentlyContinue)) { Set-ItResult -Skipped -Because ($Shell + ' ausente'); return }
    $b = New-Bucket; $srv = Start-Bucket $b 'tamper'; $sync = New-InstalledSync; $s = Join-Path $script:Work ('S' + [guid]::NewGuid().ToString('N').Substring(0, 6))
    try {
      $r = Invoke-Sync $Shell $sync @('--what', 'artifacts', '--release-base', $srv.Url, '--state', $s, '--format', 'json')
      $r.Status | Should -Be 9
      $r.Err | Should -Be ''
      ($r.Out | ConvertFrom-Json).error.id | Should -Be 'sync.sha_mismatch'
      (Get-Listing (Join-Path $s 'artifacts')).Count | Should -Be 0
      (Get-TmpLeft $s).Count | Should -Be 0
    } finally { Stop-Bucket $srv }
  }

  It '[FORBIDDEN] conexao cortada: exit 10 sync.network, arvore intacta, sem .tmp-* nem latest.txt' -ForEach @(@{ Shell = 'powershell.exe' }, @{ Shell = 'pwsh' }) {
    if (-not (Get-Command $Shell -ErrorAction SilentlyContinue)) { Set-ItResult -Skipped -Because ($Shell + ' ausente'); return }
    $b = New-Bucket; $srv = Start-Bucket $b 'cut' 6; $sync = New-InstalledSync; $s = Join-Path $script:Work ('S' + [guid]::NewGuid().ToString('N').Substring(0, 6))
    try {
      $r = Invoke-Sync $Shell $sync @('--what', 'artifacts', '--release-base', $srv.Url, '--state', $s, '--format', 'json')
      $r.Status | Should -Be 10
      ($r.Out | ConvertFrom-Json).error.id | Should -Be 'sync.network'
      (Get-Listing (Join-Path $s 'artifacts')).Count | Should -Be 0
      (Get-TmpLeft $s).Count | Should -Be 0
    } finally { Stop-Bucket $srv }
  }

  It 'v_<hash> local divergente: exit 9 sem sobrescrever' {
    $b = New-Bucket; $srv = Start-Bucket $b; $sync = New-InstalledSync; $s = Join-Path $script:Work ('S' + [guid]::NewGuid().ToString('N').Substring(0, 6))
    try {
      (Invoke-Sync 'powershell.exe' $sync @('--what', 'artifacts', '--release-base', $srv.Url, '--state', $s)).Status | Should -Be 0
      $f = Get-ChildItem -LiteralPath (Join-Path $s 'artifacts\lgbm\A1') -Recurse -File -Filter 'artifact.rds' | Select-Object -First 1
      [IO.File]::WriteAllText($f.FullName, 'adulterado localmente')
      $before = Get-Listing (Join-Path $s 'artifacts')
      $r = Invoke-Sync 'powershell.exe' $sync @('--what', 'artifacts', '--release-base', $srv.Url, '--state', $s, '--format', 'json')
      $r.Status | Should -Be 9
      (Get-Listing (Join-Path $s 'artifacts')) | Should -Be $before
    } finally { Stop-Bucket $srv }
  }

  It 'argumento invalido: exit 8 sync.usage; em texto a 1a linha de stderr e `prevcarga: erro [config] (exit 8)` e tudo ASCII' {
    $sync = New-InstalledSync
    $r = Invoke-Sync 'powershell.exe' $sync @('--bogus', '--format', 'json')
    $r.Status | Should -Be 8
    $r.Err | Should -Be ''
    ($r.Out | ConvertFrom-Json).error.id | Should -Be 'sync.usage'
    $t = Invoke-Sync 'powershell.exe' $sync @('--bogus')
    $t.Status | Should -Be 8
    ($t.Err -split "`n")[0].TrimEnd("`r") | Should -Be 'prevcarga: erro [config] (exit 8)'
    ([int[]][char[]]($t.Out + $t.Err) | Where-Object { $_ -gt 127 }).Count | Should -Be 0
  }

  It 'o shim .cmd repassa os argumentos e o exit code do .ps1' {
    $d = Split-Path -Parent (New-InstalledSync)
    Copy-Item -LiteralPath (Join-Path $script:Repo 'install\prevcarga-sync.cmd') -Destination $d
    & cmd.exe /d /c ('"' + (Join-Path $d 'prevcarga-sync.cmd') + '" --bogus --format json') | Out-Null
    $LASTEXITCODE | Should -Be 8
  }
}
