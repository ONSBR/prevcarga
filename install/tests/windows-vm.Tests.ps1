# windows-vm.Tests.ps1 -- Pester 5.5.0 (versao FIXA): gate da VM Windows LIMPA (sem R, sem administrador) + shell sujo no Windows (M8.11; ADR-0038, ADR-0040).
# Reexecutavel (idempotente) numa VM restaurada do snapshot limpo. Roda em Windows PowerShell 5.1 ou pwsh 7 (o runner tests\windows\Invoke-WindowsGate.ps1
# o chama; sozinho: New-PesterContainer -Path install\tests\windows-vm.Tests.ps1 -Data @{ Bundle = '<zip>'; Sha256 = '<h>' }).
# [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] NAO foi executado nesta maquina de dev (sem host Windows nem pwsh); nada aqui foi simulado.
#
# Parametros: -Bundle/-Sha256 (zip windows-x86_64 e o sha256 dele), -Root (raiz R; default = o do instalador, LocalAppData\prevcarga),
#   -Work (pasta de trabalho), -Facts (arquivo JSON com os fatos para o recibo windows-vm), -Deck/-State/-Origin (braco do shell sujo: deck, estado
#   sincronizado e origem; ausentes => os Its do forecast sao SKIP explicito `insumo-ausente`, e o gate acusa INCOMPLETO).
# Cada It e um PASSO do recibo: o nome do It que falhar vira `passo_falho`.
param(
  [string]$Bundle = '', [string]$Sha256 = '', [string]$Root = '', [string]$Work = '', [string]$Facts = '',
  [string]$Deck = '', [string]$State = '', [string]$Origin = '', [string]$PwshExe = ''
)

# Discovery-time (Pester 5): -Skip: e avaliado antes do BeforeAll.
$script:HaveBundle = [bool]$Bundle -and (Test-Path -LiteralPath $Bundle)
$script:HaveDeck = [bool]$Deck -and [bool]$State -and [bool]$Origin -and (Test-Path -LiteralPath $Deck) -and (Test-Path -LiteralPath $State)

BeforeAll {
  $script:Repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
  $script:Installer = Join-Path $script:Repo 'install\install.ps1'
  $script:LauncherSrc = Join-Path $script:Repo 'install\prevcarga.cmd'
  $script:Bundle = $Bundle
  $script:Sha = $Sha256.ToLowerInvariant()
  $script:Deck = $Deck; $script:State = $State; $script:Origin = $Origin
  $script:Work = $Work
  if (-not $script:Work) { $script:Work = Join-Path ([IO.Path]::GetTempPath()) ('pc-winvm-' + [guid]::NewGuid().ToString('N')) }
  [void](New-Item -ItemType Directory -Force -Path $script:Work)
  $script:Root = $Root
  if (-not $script:Root) { $script:Root = Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'prevcarga' }
  $script:Root = $script:Root.TrimEnd('\', '/')
  $script:FactsPath = $Facts
  $script:F = @{ shells = @{ cmd = $null; ps51 = $null; ps7 = $null }; bytes_gt_127 = 0; installer_exit = $null }
  $script:PsExe = Join-Path ([Environment]::GetFolderPath('System')) 'WindowsPowerShell\v1.0\powershell.exe'
  $pw = Get-Command pwsh -ErrorAction SilentlyContinue
  $script:Pwsh = if ($PwshExe) { $PwshExe } elseif ($pw) { $pw.Source } else { '' }

  . (Join-Path $PSScriptRoot 'windows-vm.Helpers.ps1')
}

AfterAll {
  Save-Facts
}

Describe 'VM Windows limpa' {
  It '[W03] pre-condicao: sem R no PATH e o token do processo sem Administrators ativo (S-1-5-32-544)' -Skip:(-not $script:HaveBundle) {
    Get-Command Rscript -ErrorAction SilentlyContinue | Should -BeNullOrEmpty
    Get-Command R.exe -ErrorAction SilentlyContinue | Should -BeNullOrEmpty
    Test-HasAdminToken | Should -BeFalse
  }

  It '[W03] instala com `powershell -ExecutionPolicy Bypass -File install.ps1 --bundle <zip> --sha256 <h>` (exit 0, sem elevacao)' -Skip:(-not $script:HaveBundle) {
    $script:Sha | Should -Match '^[0-9a-f]{64}$'
    $line = '-NoProfile -ExecutionPolicy Bypass -File "' + $script:Installer + '" --bundle "' + $script:Bundle + '" --sha256 ' + $script:Sha + ' --root "' + $script:Root + '"'
    $r = Invoke-Capture $script:PsExe $line
    $script:F.installer_exit = $r.Exit
    $r.Exit | Should -Be 0 -Because ((Get-AsciiText $r.Err) + (Get-AsciiText $r.Out))
    Test-Path -LiteralPath (Get-Launcher $script:Root) | Should -BeTrue
    Test-Path -LiteralPath (Get-RscriptExe) | Should -BeTrue
  }

  It '[W04] doctor exit 0 e ASCII puro em <Shell>' -Skip:(-not $script:HaveBundle) -TestCases @(
    @{ Shell = 'cmd' }, @{ Shell = 'ps51' }, @{ Shell = 'ps7' }) {
    param($Shell)
    if ($Shell -eq 'ps7') { $script:Pwsh | Should -Not -BeNullOrEmpty -Because 'pwsh 7 ausente: instale o zip portatil do PowerShell 7 (sem admin) conforme docs/reference/windows-gate.md' }
    $r = Invoke-Launcher $Shell $script:Root 'doctor'
    $script:F.shells[$Shell] = $r.Exit
    $script:F.bytes_gt_127 = [int]$script:F.bytes_gt_127 + (Get-HighBytes ($r.Out + $r.Err))
    $r.Exit | Should -Be 0 -Because ((Get-AsciiText $r.Err))
    $r.Out.Length | Should -BeGreaterThan 0
    (Get-HighBytes ($r.Out + $r.Err)) | Should -Be 0
  }

  It '[W03] bin no PATH do usuario (HKCU:\Environment), sem HKLM e sem elevacao' -Skip:(-not $script:HaveBundle) {
    (Get-UserPathParts) | Should -Contain (Join-Path $script:Root 'bin')
    $m = [Microsoft.Win32.Registry]::LocalMachine.OpenSubKey('SYSTEM\CurrentControlSet\Control\Session Manager\Environment')
    $mp = [string]$m.GetValue('Path', '')
    $m.Dispose()
    $mp | Should -Not -Match ([regex]::Escape((Join-Path $script:Root 'bin')))
    Test-HasAdminToken | Should -BeFalse
  }

  It '[W04] CONTROL: acento injetado no catalogo faz o grep de bytes > 127 reprovar (o assert tem dentes)' -Skip:(-not $script:HaveBundle) {
    $stage = New-StageRoot 'stage-acento'
    try {
      $tsv = Join-Path $stage ('versions\' + (Get-CurrentVersion) + '\code\scripts\lib\errors\core.tsv')
      $txt = [IO.File]::ReadAllText($tsv)
      $txt.Contains('uso invalido da linha de comando') | Should -BeTrue
      $acc = $txt.Replace('uso invalido da linha de comando', ('uso inv' + [char]0xE1 + 'lido da linha de comando'))
      [IO.File]::WriteAllText($tsv, $acc, (New-Object Text.UTF8Encoding $false))
      # o catalogo injetado TEM byte > 127 (o mesmo contador do assert acusa) ...
      (Get-HighBytes ([IO.File]::ReadAllBytes($tsv))) | Should -BeGreaterThan 0
      # ... e a mesma funcao reprova um fluxo de saida com acento
      (Get-HighBytes ([Text.Encoding]::UTF8.GetBytes('uso inv' + [char]0xE1 + 'lido'))) | Should -BeGreaterThan 0
      # defesa em profundidade: o CLI recusa o catalogo nao-ASCII (contract.error_catalog_schema, exit 2) e a saida CONTINUA ASCII
      $r = Invoke-Launcher 'cmd' $stage 'doctor --bogus'
      $r.Exit | Should -Be 2
      (Get-HighBytes ($r.Out + $r.Err)) | Should -Be 0
      (Get-AsciiText $r.Err) | Should -Match 'error_catalog|catalogo'
      # o catalogo REAL instalado nao foi alterado
      (Get-HighBytes ([IO.File]::ReadAllBytes((Join-Path $script:Root ('versions\' + (Get-CurrentVersion) + '\code\scripts\lib\errors\core.tsv'))))) | Should -Be 0
    } finally { Remove-StageRoot $stage }
  }
}

Describe 'Shell sujo no Windows' {
  It '[W13] prevcarga.cmd: so as 6 limpezas set "X=" (nenhum outro `set `)' -Skip:(-not $script:HaveBundle) {
    $allowed = @('R_HOME', 'R_LIBS', 'R_LIBS_USER', 'R_LIBS_SITE', 'R_PROFILE_USER', 'R_ENVIRON_USER') | ForEach-Object { 'set "' + $_ + '="' }
    $lines = [IO.File]::ReadAllLines((Get-Launcher $script:Root))
    $sets = @($lines | Where-Object { $_ -match '(?i)^\s*set\s' })
    @($sets | Where-Object { $allowed -notcontains $_.Trim() }).Count | Should -Be 0
    $sets.Count | Should -Be 6
    # CONTROL: uma atribuicao a mais seria acusada
    @((@($lines) + '  SET TZ=UTC') | Where-Object { $_ -match '(?i)^\s*set\s' } | Where-Object { $allowed -notcontains $_.Trim() }).Count | Should -Be 1
  }

  It '[W13] CONTROL: a sonda de hora SEM o guard do runtime difere entre o shell sujo e o limpo' -Skip:(-not $script:HaveBundle) {
    $probe = '--vanilla -e "cat(format(as.POSIXct(0, origin = ''1970-01-01'', tz = ''UTC''), tz = '''', usetz = TRUE))"'
    $a = Invoke-Capture (Get-RscriptExe) $probe (Get-DirtyEnv)
    $b = Invoke-Capture (Get-RscriptExe) $probe (Get-CleanEnv)
    $a.Exit | Should -Be 0; $b.Exit | Should -Be 0
    (Get-AsciiText $a.Out) | Should -Not -Be (Get-AsciiText $b.Out)
  }

  It '[W13] doctor sob o shell sujo mostra TZ/OPENBLAS encontrados e os efetivos fixados (UTC, 1)' -Skip:(-not $script:HaveBundle) {
    $r = Invoke-Capture 'cmd.exe' ('/d /s /c ""' + (Get-Launcher $script:Root) + '" doctor --format json"') (Get-DirtyEnv)
    $r.Exit | Should -Be 0
    $j = (Get-AsciiText $r.Out) | ConvertFrom-Json
    $tz = @($j.result.checks | Where-Object { $_.check -eq 'TZ' })[0]
    $bl = @($j.result.checks | Where-Object { $_.check -eq 'OPENBLAS_NUM_THREADS' })[0]
    $tz.found | Should -Be 'America/Sao_Paulo'; $tz.effective | Should -Be 'UTC'
    $bl.found | Should -Be '4'; $bl.effective | Should -Be '1'
  }

  It '[W13] CONTROL: setter de TZ injetado que nao fixa => exit 7 com stdout vazio; a copia sem a injecao roda (exit 0)' -Skip:(-not $script:HaveBundle) {
    $stage = New-StageRoot 'stage-setter'
    try {
      $ok = Invoke-Launcher 'cmd' $stage 'models' (Get-DirtyEnv)
      $ok.Exit | Should -Be 0 -Because (Get-AsciiText $ok.Err)
      $rt = Join-Path $stage ('versions\' + (Get-CurrentVersion) + '\code\scripts\lib\runtime.R')
      $inj = "`n## [CONTROL] setter de TZ que NAO fixa o fuso (so observa), apenas nesta copia`n.ctl_orig_setters <- prevcarga_runtime_setters`nprevcarga_runtime_setters <- function() { s <- .ctl_orig_setters(); s`$tz <- function() .prevcarga_tz_observed(); s }`n"
      [IO.File]::AppendAllText($rt, $inj, (New-Object Text.UTF8Encoding $false))
      $bad = Invoke-Launcher 'cmd' $stage 'models' (Get-DirtyEnv)
      $bad.Exit | Should -Be 7
      $bad.Out.Length | Should -Be 0
      ((Get-AsciiText $bad.Err) -split "`n")[0].Trim() | Should -Be 'prevcarga: erro [runtime] (exit 7)'
    } finally { Remove-StageRoot $stage }
  }

  It '[W13] FORBIDDEN: forecast --origin <O> pelo prevcarga.cmd, shell sujo x limpo: stdout, stderr e entrega byte-identicos' -Skip:(-not $script:HaveDeck) {
    $a = Invoke-Forecast 'sujo' (Get-DirtyEnv) $script:Deck $script:State $script:Origin
    $b = Invoke-Forecast 'limpo' (Get-CleanEnv) $script:Deck $script:State $script:Origin
    $a.Exit | Should -Be 0 -Because $a.Stderr
    $b.Exit | Should -Be 0 -Because $b.Stderr
    $a.High | Should -Be 0; $b.High | Should -Be 0
    $a.Stdout.Length | Should -BeGreaterThan 0
    $a.Stdout | Should -BeExactly $b.Stdout
    $a.Stderr | Should -BeExactly $b.Stderr
    $a.Tree | Should -BeExactly $b.Tree
    $a.Manifest | Should -BeExactly $b.Manifest
  }

  It '[W13] CONTROL: outro deck (copia com 1 byte alterado em um parquet) muda a entrega (a comparacao tem dentes)' -Skip:(-not $script:HaveDeck) {
    $d2 = Join-Path $script:Work 'deck-alterado'
    if (Test-Path -LiteralPath $d2) { Remove-Item -LiteralPath $d2 -Recurse -Force }
    Copy-Item -LiteralPath $script:Deck -Destination $d2 -Recurse -Force
    $pq = Get-ChildItem -LiteralPath $d2 -Recurse -File -Filter 'load*.parquet' | Select-Object -First 1
    $pq | Should -Not -BeNullOrEmpty
    $fs = [IO.File]::Open($pq.FullName, 'Open', 'ReadWrite')
    try { [void]$fs.Seek(-12, 'End'); $x = $fs.ReadByte(); [void]$fs.Seek(-12, 'End'); $fs.WriteByte([byte](($x + 1) % 256)) } finally { $fs.Dispose() }
    $base = Invoke-Forecast 'limpo-ref' (Get-CleanEnv) $script:Deck $script:State $script:Origin
    $alt = Invoke-Forecast 'limpo-alt' (Get-CleanEnv) $d2 $script:State $script:Origin
    ($alt.Exit -ne 0 -or $alt.Tree -ne $base.Tree -or $alt.Stdout -ne $base.Stdout) | Should -BeTrue
  }
}

Describe 'Itens diferidos da VM Windows (M8.4/M8.6)' {
  It '[W08] aspas do -e do R pelo Windows PowerShell 5.1 e pelo pwsh 7 (e por ProcessStartInfo com \" escapado)' -Skip:(-not $script:HaveBundle) {
    $rs = Get-RscriptExe
    $f = Join-Path $script:Work 'quote.ps1'
    [IO.File]::WriteAllText($f, ('$o = & ''' + $rs + ''' --vanilla -e "cat(paste0(''a'', ''b''))"' + "`n" + '[Console]::Out.Write(($o -join ""))' + "`n"))
    $exes = @($script:PsExe)
    if ($script:Pwsh) { $exes += $script:Pwsh }
    foreach ($x in $exes) {
      $r = Invoke-Capture $x ('-NoProfile -ExecutionPolicy Bypass -File "' + $f + '"')
      (Get-AsciiText $r.Out) | Should -Be 'ab' -Because ($x + ': ' + (Get-AsciiText $r.Err))
    }
    $p = Invoke-Capture $rs '--vanilla -e "cat(paste0(\"a\", \"b\"))"'
    (Get-AsciiText $p.Out) | Should -Be 'ab' -Because (Get-AsciiText $p.Err)
  }

  It '[W09] MAX_PATH: nenhum caminho da instalacao em %LOCALAPPDATA% passa de 259 caracteres (runtime\R\library incluida)' -Skip:(-not $script:HaveBundle) {
    $vd = Join-Path $script:Root ('versions\' + (Get-CurrentVersion))
    $max = 0; $worst = ''
    try {
      Get-ChildItem -LiteralPath $vd -Recurse -Force | ForEach-Object { if ($_.FullName.Length -gt $max) { $max = $_.FullName.Length; $worst = $_.FullName } }
    } catch { throw ('caminho longo demais para o proprio listador (>= 260): ' + $_.Exception.Message) }
    $script:F.max_path = $max
    $max | Should -BeLessThan 260 -Because ('raiz=' + $script:Root + ' pior=' + $worst)
    # CONTROL: o contador acusa um caminho sintetico longo
    (('C:\' + ('a' * 300)).Length -ge 260) | Should -BeTrue
  }

  It '[W10] FORBIDDEN: com R_LIBS_USER/R_LIBS/R_LIBS_SITE apontando uma library sombra o lancador so usa a do bundle; CONTROL: lancador sem as limpezas carrega a sombra' -Skip:(-not $script:HaveBundle) {
    $shadow = Join-Path $script:Work 'shadow-lib'; $src = Join-Path $script:Work 'shadow-src\jsonlite'
    foreach ($d in @($shadow, (Join-Path $src 'R'))) { [void](New-Item -ItemType Directory -Force -Path $d) }
    [IO.File]::WriteAllText((Join-Path $src 'DESCRIPTION'), "Package: jsonlite`nVersion: 99.0`nTitle: sombra`nDescription: sombra de teste.`nLicense: GPL-2`nEncoding: UTF-8`n")
    [IO.File]::WriteAllText((Join-Path $src 'NAMESPACE'), "exportPattern(`"^[^.]`")`n")
    [IO.File]::WriteAllText((Join-Path $src 'R\zzz.R'), ".onLoad <- function(libname, pkgname) stop(`"SHADOW LIB LOADED`")`n")
    $ins = Invoke-Capture (Get-RscriptExe) ('--vanilla -e "install.packages(''' + $src.Replace('\', '/') + ''', repos = NULL, type = ''source'', lib = ''' + $shadow.Replace('\', '/') + ''', INSTALL_opts = ''--no-test-load'')"')
    if (-not (Test-Path -LiteralPath (Join-Path $shadow 'jsonlite'))) { Set-ItResult -Skipped -Because ('R CMD INSTALL da sombra falhou: ' + (Get-AsciiText $ins.Err)); return }
    $h = New-DirtyHome
    $dirty = @{ R_LIBS_USER = $shadow; R_LIBS = $shadow; R_LIBS_SITE = $shadow; R_PROFILE_USER = (Join-Path $h '.Rprofile'); R_ENVIRON_USER = (Join-Path $h '.Rprofile') }
    $clean = Invoke-Launcher 'cmd' $script:Root 'models --format json' (Get-CleanEnv)
    $dirt = Invoke-Launcher 'cmd' $script:Root 'models --format json' $dirty
    $clean.Exit | Should -Be 0; $dirt.Exit | Should -Be 0
    (Remove-ClockFields (Get-AsciiText $dirt.Out)) | Should -BeExactly (Remove-ClockFields (Get-AsciiText $clean.Out))
    (Get-AsciiText $dirt.Err) | Should -BeExactly (Get-AsciiText $clean.Err)
    # CONTROL: o lancador antigo (so `set "R_HOME="`) com o mesmo ambiente carrega a sombra e cai
    $stage = New-StageRoot 'stage-antigo'
    try {
      $l = Get-Launcher $stage
      $kept = [IO.File]::ReadAllLines($l) | Where-Object { $_ -notmatch '^set "R_(LIBS|LIBS_USER|LIBS_SITE|PROFILE_USER|ENVIRON_USER)="$' }
      [IO.File]::WriteAllText($l, (($kept -join "`r`n") + "`r`n"))
      $old = Invoke-Launcher 'cmd' $stage 'models --format json' $dirty
      $old.Exit | Should -Not -Be 0
      ((Get-AsciiText $old.Out) + (Get-AsciiText $old.Err)) | Should -Match 'SHADOW LIB LOADED'
    } finally { Remove-StageRoot $stage }
  }

  It '[W11] matriz dos .ps1: argumento invalido (exit 8) em json e em texto, ASCII, em 5.1 e em 7 (install.ps1 e prevcarga-sync.ps1)' -Skip:(-not $script:HaveBundle) {
    $shells = @($script:PsExe)
    if ($script:Pwsh) { $shells += $script:Pwsh }
    foreach ($u in @('install.ps1', 'prevcarga-sync.ps1')) {
      foreach ($x in $shells) {
        foreach ($fmt in @('json', 'text')) {
          $r = Invoke-Capture $x ('-NoProfile -ExecutionPolicy Bypass -File "' + (Join-Path $script:Repo ('install\' + $u)) + '" --bogus --format ' + $fmt)
          $tag = $u + ' ' + $x + ' ' + $fmt
          $r.Exit | Should -Be 8 -Because $tag
          (Get-HighBytes ($r.Out + $r.Err)) | Should -Be 0 -Because $tag
          if ($fmt -eq 'json') { ((Get-AsciiText $r.Out) | ConvertFrom-Json).error.id | Should -Not -BeNullOrEmpty -Because $tag }
          else { (Get-AsciiText $r.Err) | Should -Match '^prevcarga: erro \[' -Because $tag }
        }
      }
    }
  }

  It '[W12] catalogo: <R>\bin\error-catalog.tsv == code\install\error-catalog.tsv do bundle instalado (bytes) e ASCII' -Skip:(-not $script:HaveBundle) {
    $a = [IO.File]::ReadAllBytes((Join-Path $script:Root 'bin\error-catalog.tsv'))
    $b = [IO.File]::ReadAllBytes((Join-Path $script:Root ('versions\' + (Get-CurrentVersion) + '\code\install\error-catalog.tsv')))
    $a.Length | Should -BeGreaterThan 0
    ([BitConverter]::ToString($a)) | Should -BeExactly ([BitConverter]::ToString($b))
    (Get-HighBytes $a) | Should -Be 0
  }
}
