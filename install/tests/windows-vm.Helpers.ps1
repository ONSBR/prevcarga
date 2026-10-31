# windows-vm.Helpers.ps1 -- funcoes compartilhadas pelas suites Pester da VM Windows (windows-vm.Tests.ps1, windows-vm-cross.Tests.ps1; M8.11).
# Carregado por dot-source dentro do BeforeAll de cada suite, DEPOIS de definir $script:Work, $script:Root, $script:F, $script:FactsPath,
# $script:PsExe e $script:Pwsh. Nao e executado sozinho. [DEFERRED windows-vm do M8, dono: Gabriel]: nunca rodou nesta maquina de dev.

# --- processo filho com bytes crus de stdout/stderr -------------------------------------------------------------
# Env: hashtable de ajustes (valor $null remove a variavel); o resto e herdado.
function Invoke-Capture([string]$Exe, [string]$ArgLine, [hashtable]$Env = @{}, [string]$Wd = '') {
  $psi = New-Object Diagnostics.ProcessStartInfo
  $psi.FileName = $Exe; $psi.Arguments = $ArgLine
  $psi.RedirectStandardOutput = $true; $psi.RedirectStandardError = $true; $psi.UseShellExecute = $false; $psi.CreateNoWindow = $true
  if ($Wd) { $psi.WorkingDirectory = $Wd }
  foreach ($k in $Env.Keys) {
    if ($null -eq $Env[$k]) { [void]$psi.EnvironmentVariables.Remove($k) } else { $psi.EnvironmentVariables[$k] = [string]$Env[$k] }
  }
  $p = New-Object Diagnostics.Process
  $p.StartInfo = $psi
  [void]$p.Start()
  $so = New-Object IO.MemoryStream; $se = New-Object IO.MemoryStream
  $t1 = $p.StandardOutput.BaseStream.CopyToAsync($so); $t2 = $p.StandardError.BaseStream.CopyToAsync($se)
  $p.WaitForExit(); [void]$t1.Wait(); [void]$t2.Wait()
  $r = @{ Exit = [int]$p.ExitCode; Out = $so.ToArray(); Err = $se.ToArray() }
  $p.Dispose()
  return $r
}
function Get-HighBytes([byte[]]$Bytes) { return @($Bytes | Where-Object { $_ -gt 127 }).Count }
function Get-AsciiText([byte[]]$Bytes) { return [Text.Encoding]::ASCII.GetString($Bytes) }
function Test-HasAdminToken {
  $id = [Security.Principal.WindowsIdentity]::GetCurrent()
  return (New-Object Security.Principal.WindowsPrincipal($id)).IsInRole([Security.Principal.SecurityIdentifier]'S-1-5-32-544')
}
function Get-UserPathParts {
  $k = [Microsoft.Win32.Registry]::CurrentUser.OpenSubKey('Environment')
  $v = [string]$k.GetValue('Path', '', [Microsoft.Win32.RegistryValueOptions]::DoNotExpandEnvironmentNames)
  $k.Dispose()
  return @($v -split ';' | Where-Object { $_ -ne '' })
}
function Get-CurrentVersion { return ([IO.File]::ReadAllText((Join-Path $script:Root 'current')).Trim()) }
function Get-Launcher([string]$R) { return (Join-Path $R 'bin\prevcarga.cmd') }
function Get-RscriptExe { return (Join-Path $script:Root ('versions\' + (Get-CurrentVersion) + '\runtime\R\bin\Rscript.exe')) }

# lancador pelo cmd, pelo PS 5.1 e pelo pwsh 7: devolve o mesmo formato de Invoke-Capture
function Invoke-Launcher([string]$Shell, [string]$R, [string]$Verb, [hashtable]$Env = @{}) {
  $l = Get-Launcher $R
  if ($Shell -eq 'cmd') { return (Invoke-Capture 'cmd.exe' ('/d /s /c ""' + $l + '" ' + $Verb + '"') $Env) }
  $exe = $script:PsExe
  if ($Shell -eq 'ps7') { $exe = $script:Pwsh }
  return (Invoke-Capture $exe ('-NoProfile -ExecutionPolicy Bypass -Command "& ''' + $l + ''' ' + $Verb + '; exit $LASTEXITCODE"') $Env)
}

# raiz de teste: copia do code\ da versao atual + junction (sem admin) para o runtime; current e lancador copiados
function New-StageRoot([string]$Name) {
  $r = Join-Path $script:Work $Name
  $v = Get-CurrentVersion
  $vd = Join-Path $r ('versions\' + $v)
  [void](New-Item -ItemType Directory -Force -Path (Join-Path $r 'bin'))
  [void](New-Item -ItemType Directory -Force -Path $vd)
  Copy-Item -LiteralPath (Join-Path $script:Root ('versions\' + $v + '\code')) -Destination (Join-Path $vd 'code') -Recurse -Force
  $rt = Join-Path $script:Root ('versions\' + $v + '\runtime')
  $out = & cmd.exe /d /c ('mklink /J "' + (Join-Path $vd 'runtime') + '" "' + $rt + '"') 2>&1
  if (-not (Test-Path -LiteralPath (Join-Path $vd 'runtime'))) { throw ('mklink /J falhou: ' + ($out -join ' ')) }
  Copy-Item -LiteralPath (Join-Path $script:Root 'bin\prevcarga.cmd') -Destination (Join-Path $r 'bin\prevcarga.cmd') -Force
  [IO.File]::WriteAllText((Join-Path $r 'current'), $v)
  return $r
}
function Remove-StageRoot([string]$R) {
  $v = Get-CurrentVersion
  $j = Join-Path $R ('versions\' + $v + '\runtime')
  if (Test-Path -LiteralPath $j) { & cmd.exe /d /c ('rmdir "' + $j + '"') | Out-Null }   # remove so a juncao, nunca o alvo
  Remove-Item -LiteralPath $R -Recurse -Force -ErrorAction SilentlyContinue
}

# sha256 da arvore (caminho relativo + hash de cada arquivo), como pc_tree_sha(); `Skip` = nomes de arquivo ignorados
function Get-TreeSha([string]$Dir, [string[]]$Skip = @()) {
  if (-not (Test-Path -LiteralPath $Dir)) { return 'ausente' }
  $base = (Resolve-Path -LiteralPath $Dir).Path.TrimEnd('\')
  $lines = Get-ChildItem -LiteralPath $Dir -Recurse -Force -File | Where-Object { $Skip -notcontains $_.Name } |
    Sort-Object { $_.FullName.ToLowerInvariant() } | ForEach-Object {
      $_.FullName.Substring($base.Length).Replace('\', '/') + ' ' + (Get-FileHash -Algorithm SHA256 -LiteralPath $_.FullName).Hash
    }
  $b = [Text.Encoding]::ASCII.GetBytes(($lines -join "`n"))
  $h = [Security.Cryptography.SHA256]::Create()
  return ([BitConverter]::ToString($h.ComputeHash($b)) -replace '-', '').ToLowerInvariant()
}
# campos de relogio da rodada (por desenho diferem entre duas invocacoes): fora da comparacao
function Remove-ClockFields([string]$Text) {
  $t = [regex]::Replace($Text, '"(run_id|generated_at|gerado_em)"\s*:\s*"[^"]*"', '"$1":"X"')
  return [regex]::Replace($t, '"duracao_s"\s*:\s*[-0-9.eE+]+', '"duracao_s":0')
}
function New-DirtyHome {
  $h = Join-Path $script:Work 'home-sujo'
  [void](New-Item -ItemType Directory -Force -Path (Join-Path $h 'Documents'))
  $rp = "Sys.setenv(TZ = 'Asia/Tokyo')`nSys.setlocale('LC_COLLATE', 'C')`noptions(digits = 3, OutDec = ',')`n"
  [IO.File]::WriteAllText((Join-Path $h 'Documents\.Rprofile'), $rp)
  [IO.File]::WriteAllText((Join-Path $h '.Rprofile'), $rp)
  return $h
}
function Get-DirtyEnv {
  $h = New-DirtyHome
  return @{ TZ = 'America/Sao_Paulo'; LC_ALL = 'pt_BR.UTF-8'; LANG = 'pt_BR.UTF-8'; OPENBLAS_NUM_THREADS = '4'; OMP_NUM_THREADS = '4'
            R_USER = $h; HOME = $h; R_PROFILE_USER = (Join-Path $h 'Documents\.Rprofile') }
}
function Get-CleanEnv {
  return @{ TZ = $null; LC_ALL = $null; LANG = $null; OPENBLAS_NUM_THREADS = $null; OMP_NUM_THREADS = $null; MKL_NUM_THREADS = $null
            R_USER = $null; R_PROFILE_USER = $null }
}
# forecast pelo lancador numa copia do estado; devolve a entrega normalizada
function Invoke-Forecast([string]$Tag, [hashtable]$Env, [string]$DeckDir, [string]$StateDir, [string]$OriginV) {
  $st = Join-Path $script:Work ('state-' + $Tag)
  if (Test-Path -LiteralPath $st) { Remove-Item -LiteralPath $st -Recurse -Force }
  Copy-Item -LiteralPath $StateDir -Destination $st -Recurse -Force
  $verb = 'forecast "--data=' + $DeckDir + '" "--state=' + $st + '" --origin ' + $OriginV + ' --skip-validate --format json'
  $r = Invoke-Capture 'cmd.exe' ('/d /s /c ""' + (Get-Launcher $script:Root) + '" ' + $verb + '"') $Env
  $txt = (Get-AsciiText $r.Out).Replace($st, 'STATE')
  $saida = ''
  $m = [regex]::Match((Get-AsciiText $r.Out), '"saida"\s*:\s*"((?:[^"\\]|\\.)*)"')
  if ($m.Success) { $saida = $m.Groups[1].Value.Replace('\\', '\') }
  $mf = ''
  if ($saida -and (Test-Path -LiteralPath (Join-Path $saida 'manifest.json'))) { $mf = Remove-ClockFields ([IO.File]::ReadAllText((Join-Path $saida 'manifest.json'))) }
  return @{ Exit = $r.Exit; Stdout = (Remove-ClockFields $txt); Stderr = (Get-AsciiText $r.Err); High = (Get-HighBytes ($r.Out + $r.Err))
            Saida = $saida; Tree = $(if ($saida) { Get-TreeSha $saida @('manifest.json') } else { 'sem-saida' }); Manifest = $mf }
}
function Save-Facts {
  if ($script:FactsPath) { [IO.File]::WriteAllText($script:FactsPath, ($script:F | ConvertTo-Json -Depth 5), (New-Object Text.UTF8Encoding $false)) }
}
