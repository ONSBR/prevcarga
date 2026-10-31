# install.Tests.ps1 -- Pester 5.5.0 (versao FIXA) para install\install.ps1 e install\prevcarga.cmd (M8.4; ADR-0038, ADR-0040).
# Roda em Windows PowerShell 5.1 e pwsh 7 (o instalador e executado nos DOIS, em processo filho, com code page 850).
# Nunca toca a rede, o upload nem o %LOCALAPPDATA% real: raizes e bundles em pasta temporaria; o Path de HKCU e salvo e restaurado.
# Precisa de um bundle candidato (zip do M8.2) em -Candidate; sem ele os testes que instalam fazem skip com motivo.
#   $c = New-PesterContainer -Path install\tests\install.Tests.ps1 -Data @{ Candidate = 'dist\prevcarga-X-windows-x86_64.zip' }
#   Invoke-Pester -Container $c
param([string]$Candidate = '')

# Discovery-time (Pester 5): -Skip: e avaliado antes do BeforeAll.
$script:HaveCandidate = [bool]$Candidate -and (Test-Path -LiteralPath $Candidate)

BeforeAll {
  $script:Repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
  $script:Installer = Join-Path $script:Repo 'install\install.ps1'
  $script:Launcher = Join-Path $script:Repo 'install\prevcarga.cmd'
  $script:Work = Join-Path ([IO.Path]::GetTempPath()) ('pc-pester-' + [guid]::NewGuid().ToString('N'))
  [void](New-Item -ItemType Directory -Path $script:Work)
  $script:HaveCandidate = [bool]$Candidate -and (Test-Path -LiteralPath $Candidate)
  $script:Candidate = $Candidate
  $script:Shells = @('powershell.exe')
  if (Get-Command pwsh -ErrorAction SilentlyContinue) { $script:Shells += 'pwsh' }
  $key = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey('Environment')
  $script:PathBackup = [string]$key.GetValue('Path', '', [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
  $key.Dispose()

  function Restore-UserPath {
    $k = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey('Environment', $true)
    $k.SetValue('Path', $script:PathBackup, [Microsoft.Win32.RegistryValueKind]::ExpandString)
    $k.Dispose()
  }
  function Get-UserPathParts {
    $k = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey('Environment')
    $v = [string]$k.GetValue('Path', '', [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
    $k.Dispose()
    return @($v -split ';' | Where-Object { $_ -ne '' })
  }
  function New-TestRoot { return (Join-Path $script:Work ('R' + [guid]::NewGuid().ToString('N').Substring(0, 8))) }

  # sha256 da listagem recursiva (caminho relativo + hash de cada arquivo), como pc_tree_sha() do helper do M8.3
  function Get-TreeSha([string]$Dir) {
    if (-not (Test-Path -LiteralPath $Dir)) { return 'ausente' }
    $base = (Resolve-Path -LiteralPath $Dir).Path.TrimEnd('\')
    $lines = Get-ChildItem -LiteralPath $Dir -Recurse -Force -File | Sort-Object { $_.FullName.ToLowerInvariant() } | ForEach-Object {
      $_.FullName.Substring($base.Length).Replace('\', '/') + ' ' + (Get-FileHash -Algorithm SHA256 -LiteralPath $_.FullName).Hash
    }
    $b = [Text.Encoding]::ASCII.GetBytes(($lines -join "`n"))
    $h = [Security.Cryptography.SHA256]::Create()
    return ([BitConverter]::ToString($h.ComputeHash($b)) -replace '-', '').ToLowerInvariant()
  }

  # reempacota o candidato como <nome>; Corrupt = 'manifest' troca 1 hex de files[].sha256 no RUNTIME.json
  function New-FixtureBundle([string]$Name, [string]$Corrupt = 'none') {
    $dst = Join-Path $script:Work $Name
    Copy-Item -LiteralPath $script:Candidate -Destination $dst -Force
    if ($Corrupt -eq 'manifest') {
      Add-Type -AssemblyName System.IO.Compression
      $fs = [IO.File]::Open($dst, 'Open', 'ReadWrite')
      $za = New-Object IO.Compression.ZipArchive($fs, [IO.Compression.ZipArchiveMode]::Update)
      $e = $za.Entries | Where-Object { $_.FullName -match '(^|/)RUNTIME\.json$' } | Select-Object -First 1
      $sr = New-Object IO.StreamReader($e.Open()); $txt = $sr.ReadToEnd(); $sr.Dispose()
      $name0 = $e.FullName; $e.Delete()
      $i = $txt.IndexOf('"files"')
      $m = [regex]::Match($txt.Substring($i), '"sha256"\s*:\s*"([0-9a-f])')
      $pos = $i + $m.Groups[1].Index
      $flip = if ($txt[$pos] -eq '0') { '1' } else { '0' }
      $txt = $txt.Substring(0, $pos) + $flip + $txt.Substring($pos + 1)
      $ne = $za.CreateEntry($name0)
      $sw = New-Object IO.StreamWriter($ne.Open(), (New-Object Text.UTF8Encoding $false)); $sw.Write($txt); $sw.Dispose()
      $za.Dispose(); $fs.Dispose()
    }
    return @{ Path = $dst; Sha = (Get-FileHash -Algorithm SHA256 -LiteralPath $dst).Hash.ToLowerInvariant() }
  }

  # roda o instalador em processo filho sob a code page 850; le os bytes crus (Latin1) para provar ASCII estrito
  function Invoke-Installer([string]$Shell, [string[]]$InstArgs) {
    $q = ($InstArgs | ForEach-Object { '"' + $_ + '"' }) -join ' '
    $line = 'chcp 850 >nul & "' + $Shell + '" -NoProfile -ExecutionPolicy Bypass -File "' + $script:Installer + '" ' + $q
    $psi = New-Object Diagnostics.ProcessStartInfo
    $psi.FileName = 'cmd.exe'; $psi.Arguments = '/d /s /c "' + $line + '"'
    $psi.RedirectStandardOutput = $true; $psi.RedirectStandardError = $true; $psi.UseShellExecute = $false
    $psi.StandardOutputEncoding = [Text.Encoding]::GetEncoding(28591); $psi.StandardErrorEncoding = [Text.Encoding]::GetEncoding(28591)
    $p = [Diagnostics.Process]::Start($psi)
    $so = $p.StandardOutput.ReadToEndAsync(); $se = $p.StandardError.ReadToEndAsync()
    $p.WaitForExit()
    return [pscustomobject]@{ Status = $p.ExitCode; Stdout = $so.Result.TrimEnd(); Stderr = $se.Result.TrimEnd() }
  }
  function Invoke-Inst([string]$Shell, [string]$Root, $B, [string[]]$More = @(), [string]$Sha = $B.Sha, [string]$Ver = '') {
    $a = @('-Bundle', $B.Path, '-Root', $Root, '-Format', 'json')
    if ($Sha) { $a += @('-Sha256', $Sha) }
    if ($Ver) { $a += @('-Version', $Ver) }
    $r = Invoke-Installer $Shell ($a + $More)
    $j = $null; try { $j = $r.Stdout | ConvertFrom-Json } catch { $j = $null }
    return [pscustomobject]@{ Status = $r.Status; Stdout = $r.Stdout; Stderr = $r.Stderr; Json = $j }
  }
  function Get-Current([string]$Root) { $f = Join-Path $Root 'current'; if (Test-Path -LiteralPath $f) { return ([IO.File]::ReadAllLines($f))[0] } else { return $null } }
  function Get-Versions([string]$Root) { return @(Get-ChildItem -LiteralPath (Join-Path $Root 'versions') -Force | ForEach-Object { $_.Name } | Sort-Object) }
  function Test-Ascii([string]$Text) { foreach ($c in $Text.ToCharArray()) { if ([int]$c -gt 127) { return $false } }; return $true }
}

AfterAll {
  Restore-UserPath
  if (Test-Path -LiteralPath $script:Work) { Remove-Item -LiteralPath $script:Work -Recurse -Force -ErrorAction SilentlyContinue }
}

Describe 'AC1 hash obrigatorio' -ForEach @('powershell.exe', 'pwsh') {
  BeforeAll { $script:Sh = $_ }
  It 'sem -Sha256 / com -Sha256 errado: exit 9, mesmo error.id do install.sh, raiz byte-identica; CONTROL: sha certo instala' -Skip:(-not $script:HaveCandidate) {
    if (-not (Get-Command $script:Sh -ErrorAction SilentlyContinue)) { Set-ItResult -Skipped -Because "$($script:Sh) ausente"; return }
    $R = New-TestRoot
    $v2 = New-FixtureBundle 'prevcarga-v2-windows-x86_64.zip'; $v3 = New-FixtureBundle 'prevcarga-v3-windows-x86_64.zip'
    (Invoke-Inst $script:Sh $R $v2).Status | Should -Be 0
    $antes = Get-TreeSha $R
    $a1 = Invoke-Inst $script:Sh $R $v3 -Sha ''
    $a2 = Invoke-Inst $script:Sh $R $v3 -Sha ('0' * 64)
    $a1.Status | Should -Be 9; $a1.Json.error.id | Should -Be 'install.sha_missing'
    $a2.Status | Should -Be 9; $a2.Json.error.id | Should -Be 'install.sha_mismatch'
    $a1.Stderr | Should -Be ''; $a2.Stderr | Should -Be ''
    Get-TreeSha $R | Should -Be $antes
    (Invoke-Inst $script:Sh $R $v3).Status | Should -Be 0
    Get-Current $R | Should -Be 'v3'
  }
}

Describe 'AC2 manifesto invalido' {
  It 'files[].sha256 errado: exit 6 install.manifest_invalid, current intacto, sem versions\v3 nem .stage-v3' -Skip:(-not $script:HaveCandidate) {
    $R = New-TestRoot
    $v2 = New-FixtureBundle 'prevcarga-v2-windows-x86_64.zip'; $bad = New-FixtureBundle 'prevcarga-v3-windows-x86_64.zip' 'manifest'
    (Invoke-Inst 'powershell.exe' $R $v2).Status | Should -Be 0
    $r = Invoke-Inst 'powershell.exe' $R $bad
    $r.Status | Should -Be 6; $r.Json.error.id | Should -Be 'install.manifest_invalid'
    Get-Current $R | Should -Be 'v2'
    Get-Versions $R | Should -Not -Contain 'v3'
    Get-Versions $R | Should -Not -Contain '.stage-v3'
  }
  It 'CONTROL: a variante que troca current ANTES de validar faz a assercao falhar (o teste e falsificavel)' -Skip:(-not $script:HaveCandidate) {
    $dir = Join-Path $script:Work 'variant'; [void](New-Item -ItemType Directory -Path $dir)
    $src = Get-Content -LiteralPath $script:Installer
    $i = ($src | Select-String -SimpleMatch '$res = Invoke-PcNative $rs').LineNumber
    $ins = '  [void](New-Item -ItemType Directory -Force -Path (Join-Path $versions $v)); Publish-PcCurrent $v   # VARIANTE DE TESTE'
    $out = @($src[0..($i - 2)]) + $ins + @($src[($i - 1)..($src.Count - 1)])
    [IO.File]::WriteAllLines((Join-Path $dir 'install.ps1'), $out, (New-Object Text.UTF8Encoding $false))
    $R = New-TestRoot
    $v2 = New-FixtureBundle 'prevcarga-v2-windows-x86_64.zip'; $bad = New-FixtureBundle 'prevcarga-v3-windows-x86_64.zip' 'manifest'
    (Invoke-Inst 'powershell.exe' $R $v2).Status | Should -Be 0
    $q = 'chcp 850 >nul & powershell.exe -NoProfile -ExecutionPolicy Bypass -File "' + (Join-Path $dir 'install.ps1') + '" -Bundle "' + $bad.Path + '" -Sha256 ' + $bad.Sha + ' -Root "' + $R + '" -Format json'
    & cmd.exe /d /s /c $q 2>$null | Out-Null
    Get-Current $R | Should -Not -Be 'v2'   # a variante trocou antes de validar: o teste do AC2 reprovaria
  }
}

Describe 'AC3 state\ intocado' {
  It 'upgrade, -Rollback e -Uninstall -Yes preservam state\ (FORBIDDEN); -Purge remove; -State em versions\ => exit 8; CONTROL' -Skip:(-not $script:HaveCandidate) {
    $R = New-TestRoot; $sh = 'powershell.exe'
    $b = @{}; foreach ($n in 'v1', 'v2', 'v3') { $b[$n] = New-FixtureBundle "prevcarga-$n-windows-x86_64.zip" }
    (Invoke-Inst $sh $R $b.v1).Status | Should -Be 0
    $st = Join-Path $R 'state'; [IO.File]::WriteAllText((Join-Path $st 'registry.rds'), 'x')
    $h0 = Get-TreeSha $st
    (Invoke-Inst $sh $R $b.v2).Status | Should -Be 0; Get-TreeSha $st | Should -Be $h0
    (Invoke-Installer $sh @('-Rollback', '-Root', $R, '-Format', 'json')).Status | Should -Be 0; Get-TreeSha $st | Should -Be $h0
    (Invoke-Installer $sh @('-Uninstall', '-Yes', '-Root', $R, '-Format', 'json')).Status | Should -Be 0; Get-TreeSha $st | Should -Be $h0
    [IO.File]::WriteAllText((Join-Path $st 'novo.txt'), 'y')
    Get-TreeSha $st | Should -Not -Be $h0   # CONTROL: o checksum acusa 1 arquivo
    (Invoke-Installer $sh @('-Uninstall', '-Yes', '-Purge', '-Root', $R, '-Format', 'json')).Status | Should -Be 0
    Test-Path -LiteralPath $st | Should -BeFalse
    $e = Invoke-Installer $sh @('-Bundle', $b.v1.Path, '-Sha256', $b.v1.Sha, '-Root', $R, '-State', (Join-Path $R 'versions\v2\x'), '-Format', 'json')
    $e.Status | Should -Be 8; ($e.Stdout | ConvertFrom-Json).error.id | Should -Be 'install.state_inside_versions'
  }
}

Describe 'AC4 retencao N=2 e rollback' {
  It 'v1,v2,v3 => ficam v2 e v3; -Rollback leva a v2 e o lancador imprime v2; sem anterior => exit 8 e current intacto' -Skip:(-not $script:HaveCandidate) {
    $R = New-TestRoot; $sh = 'powershell.exe'
    foreach ($n in 'v1', 'v2', 'v3') { (Invoke-Inst $sh $R (New-FixtureBundle "prevcarga-$n-windows-x86_64.zip")).Status | Should -Be 0 }
    Get-Versions $R | Should -Be @('v2', 'v3')
    (Invoke-Installer $sh @('-Rollback', '-Root', $R, '-Format', 'json')).Status | Should -Be 0
    Get-Current $R | Should -Be 'v2'
    ((& (Join-Path $R 'bin\prevcarga.cmd') --version 2>$null) -join "`n") | Should -Match 'v2'
    $r = Invoke-Installer $sh @('-Rollback', '-Root', $R, '-Format', 'json')
    $r.Status | Should -Be 8; ($r.Stdout | ConvertFrom-Json).error.id | Should -Be 'install.no_previous_version'
    Get-Current $R | Should -Be 'v2'
  }
}

Describe 'AC5 lancador' {
  It 'zero linhas set; doctor sai 0 com install_root = <R>; sem current: exit 6 e 1a linha de stderr contratual' -Skip:(-not $script:HaveCandidate) {
    # unica atribuicao permitida: set "R_HOME=" (limpa o R_HOME herdado so no processo do cmd, apos setlocal)
    @(Select-String -LiteralPath $script:Launcher -Pattern '(?i)^\s*set\s' | Where-Object { $_.Line.Trim() -ne 'set "R_HOME="' }).Count | Should -Be 0
    (Select-String -LiteralPath $script:Launcher -Pattern '(?i)^\s*setlocal\s*$').Count | Should -Be 1
    $R = New-TestRoot
    (Invoke-Inst 'powershell.exe' $R (New-FixtureBundle 'prevcarga-v2-windows-x86_64.zip')).Status | Should -Be 0
    $out = & cmd.exe /d /c ('"' + (Join-Path $R 'bin\prevcarga.cmd') + '" doctor --format json') 2>$null
    $LASTEXITCODE | Should -Be 0
    (($out -join "`n") | ConvertFrom-Json).result.install_root.Replace('\', '/') | Should -Be ($R.Replace('\', '/'))
    Remove-Item -LiteralPath (Join-Path $R 'current') -Force
    $err = & cmd.exe /d /c ('"' + (Join-Path $R 'bin\prevcarga.cmd') + '" doctor 2>&1 1>nul')
    $LASTEXITCODE | Should -Be 6
    $err[0] | Should -Be 'prevcarga: erro [bundle] (exit 6)'
    Test-Ascii ($err -join "`n") | Should -BeTrue
  }
}

Describe 'AC6 PATH do usuario e config.yaml' {
  It 'duas instalacoes: <R>\bin uma vez em HKCU, nunca em HKLM; config.yaml com -Data/-State' -Skip:(-not $script:HaveCandidate) {
    try {
      $R = New-TestRoot; $b = New-FixtureBundle 'prevcarga-v2-windows-x86_64.zip'
      $d = Join-Path $script:Work 'dados'; $s = Join-Path $script:Work 'estado'
      1..2 | ForEach-Object { (Invoke-Inst 'powershell.exe' $R $b @('-Data', $d, '-State', $s)).Status | Should -Be 0 }
      $bin = (Join-Path $R 'bin').TrimEnd('\').ToLowerInvariant()
      @(Get-UserPathParts | Where-Object { $_.TrimEnd('\').ToLowerInvariant() -eq $bin }).Count | Should -Be 1
      $m = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment')
      ([string]$m.GetValue('Path', '')).ToLowerInvariant().Contains($bin) | Should -BeFalse
      $cfg = Get-Content -LiteralPath (Join-Path $R 'config.yaml') -Raw
      $cfg | Should -Match ("data_dir: '" + [regex]::Escape($d.Replace('\', '/')) + "'")
      $cfg | Should -Match ("state_dir: '" + [regex]::Escape($s.Replace('\', '/')) + "'")
      $cfg.Contains("`r") | Should -BeFalse
      [IO.File]::ReadAllBytes((Join-Path $R 'config.yaml'))[0] | Should -Not -Be 0xEF   # sem BOM
    } finally { Restore-UserPath }
  }
}

Describe 'AC7 contrato de erros ASCII (code page 850)' -ForEach @('powershell.exe', 'pwsh') {
  BeforeAll { $script:Sh = $_ }
  It 'exits 8/9/10 com e sem -Format json: JSON unico, stderr vazio / 1a linha de stderr, nenhum byte > 127, error.id no catalogo' -Skip:(-not $script:HaveCandidate) {
    if (-not (Get-Command $script:Sh -ErrorAction SilentlyContinue)) { Set-ItResult -Skipped -Because "$($script:Sh) ausente"; return }
    $cat = Join-Path $script:Repo 'install\error-catalog.tsv'
    if (-not (Test-Path -LiteralPath $cat)) { Set-ItResult -Skipped -Because 'install\error-catalog.tsv ausente (gere por scripts/tools/export_error_catalog.R)'; return }
    $ids = [IO.File]::ReadAllLines($cat) | ForEach-Object { ($_ -split "`t")[0] }
    $R = New-TestRoot; $b = New-FixtureBundle 'prevcarga-v2-windows-x86_64.zip'
    $casos = @(
      @{ Code = 8; Args = @('-Root', $R, '-Bogus') },
      @{ Code = 9; Args = @('-Bundle', $b.Path, '-Sha256', ('0' * 64), '-Root', $R) },
      @{ Code = 10; Args = @('-Version', 'v9', '-ReleaseBase', 'https://127.0.0.1:9', '-Root', $R) }
    )
    foreach ($c in $casos) {
      $j = Invoke-Installer $script:Sh ($c.Args + @('-Format', 'json'))
      $j.Status | Should -Be $c.Code; $j.Stderr | Should -Be ''
      $o = $j.Stdout | ConvertFrom-Json
      $o.ok | Should -BeFalse; $o.exit_code | Should -Be $c.Code; $ids | Should -Contain $o.error.id
      $t = Invoke-Installer $script:Sh $c.Args
      $t.Status | Should -Be $c.Code
      ($t.Stderr -split "`n")[0].TrimEnd() | Should -Be ('prevcarga: erro [' + $o.error.class + '] (exit ' + $c.Code + ')')
      Test-Ascii ($j.Stdout + $j.Stderr + $t.Stdout + $t.Stderr) | Should -BeTrue
      $row = [IO.File]::ReadAllLines($cat) | Where-Object { $_.StartsWith($o.error.id + "`t") } | Select-Object -First 1
      $hdr = ([IO.File]::ReadAllLines($cat))[0] -split "`t"; $cells = $row -split "`t"
      $o.error.message | Should -Be $cells[[array]::IndexOf($hdr, 'message')]   # coluna pelo NOME (catalogo gerado tem exit_code)
      $o.error.hint | Should -Be $cells[[array]::IndexOf($hdr, 'hint')]
    }
  }
}

Describe 'PSScriptAnalyzer' {
  It 'zero achados Error/Warning em install.ps1 com o settings fixo' {
    if (-not (Get-Module -ListAvailable PSScriptAnalyzer)) { Set-ItResult -Skipped -Because 'PSScriptAnalyzer 1.22.0 ausente'; return }
    $f = Invoke-ScriptAnalyzer -Path $script:Installer -Settings (Join-Path $PSScriptRoot 'PSScriptAnalyzerSettings.psd1')
    @($f | Where-Object { $_.Severity -in 'Error', 'Warning' }).Count | Should -Be 0
  }
}
