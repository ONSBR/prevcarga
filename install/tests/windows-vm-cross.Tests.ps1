# windows-vm-cross.Tests.ps1 -- Pester 5.5.0 (versao FIXA): bracos Windows x Linux do gate da VM Windows (M8.11; ADR-0036, ADR-0038).
#   [W14] cross_os: a entrega do `forecast` do bundle Windows INSTALADO contra a entrega Linux de REFERENCIA (`verificar --trilho=entre-plataformas`,
#         |d| <= 0,001*|ref|); duas rodadas independentes identicas (o `forecast` nao tem --jobs: ver tests/ab/test-cross-platform.R); CONTROL +0,2% => exit 12.
#   [W15] NFR de 30 min no Windows: `forecast` pelo prevcarga.cmd <= 1800 s (duracao REGISTRADA pela rodada, nunca cronometrada aqui; protocolo em
#         tests/ab/test-nfr-30min.R). Sem deck real: SKIP `deck-real-ausente` (nunca verde silencioso).
# Pre-requisito: o bundle ja instalado por windows-vm.Tests.ps1 (mesma -Root). Dados dos recibos `cross-platform` e `nfr-30min` saem em -Facts.
# [DEFERRED windows-vm do M8, dono: Gabriel, 2026-10-31] NAO foi executado nesta maquina de dev (sem host Windows nem pwsh).
param(
  [string]$Root = '', [string]$Work = '', [string]$Facts = '', [string]$PwshExe = '',
  [string]$RefDir = '', [string]$Origin = '',
  [string]$NfrDeck = '', [string]$NfrState = '', [string]$NfrOrigin = ''
)

$script:HaveRef = [bool]$RefDir -and (Test-Path -LiteralPath (Join-Path $RefDir 'linux-x86_64\manifest.json')) -and (Test-Path -LiteralPath (Join-Path $RefDir 'deck')) -and (Test-Path -LiteralPath (Join-Path $RefDir 'state'))
$script:HaveNfr = [bool]$NfrDeck -and [bool]$NfrState -and [bool]$NfrOrigin -and (Test-Path -LiteralPath $NfrDeck) -and (Test-Path -LiteralPath $NfrState) -and
  (Test-Path -LiteralPath (Join-Path $NfrDeck 'manifest.json'))

BeforeAll {
  $script:Repo = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
  $script:Work = $Work
  if (-not $script:Work) { $script:Work = Join-Path ([IO.Path]::GetTempPath()) ('pc-winvm-cross-' + [guid]::NewGuid().ToString('N')) }
  [void](New-Item -ItemType Directory -Force -Path $script:Work)
  $script:Root = $Root
  if (-not $script:Root) { $script:Root = Join-Path ([Environment]::GetFolderPath('LocalApplicationData')) 'prevcarga' }
  $script:Root = $script:Root.TrimEnd('\', '/')
  $script:FactsPath = $Facts
  $script:F = @{ jobs_equal = $null; referencia_platform = 'linux-x86_64'; control_exit = $null; verificar_exit = $null; nfr = $null }
  $script:PsExe = Join-Path ([Environment]::GetFolderPath('System')) 'WindowsPowerShell\v1.0\powershell.exe'
  $pw = Get-Command pwsh -ErrorAction SilentlyContinue
  $script:Pwsh = if ($PwshExe) { $PwshExe } elseif ($pw) { $pw.Source } else { '' }
  $script:Ref = $RefDir
  $script:Origin = $Origin
  . (Join-Path $PSScriptRoot 'windows-vm.Helpers.ps1')
  if (-not $script:Origin -and $script:HaveRef) {
    $script:Origin = [string](([IO.File]::ReadAllText((Join-Path $RefDir 'linux-x86_64\run_manifest.json')) | ConvertFrom-Json).origin)
  }
}

AfterAll {
  Save-Facts
}

Describe 'cross_os: entrega Linux de referencia x bundle Windows instalado' {
  It '[W14] duas rodadas independentes do forecast no Windows dao entregas identicas (jobs 1 == 4: o forecast nao tem --jobs)' -Skip:(-not $script:HaveRef) {
    $a = Invoke-Forecast 'x1' (Get-CleanEnv) (Join-Path $script:Ref 'deck') (Join-Path $script:Ref 'state') $script:Origin
    $b = Invoke-Forecast 'x2' (Get-CleanEnv) (Join-Path $script:Ref 'deck') (Join-Path $script:Ref 'state') $script:Origin
    $a.Exit | Should -Be 0 -Because $a.Stderr
    $b.Exit | Should -Be 0 -Because $b.Stderr
    $script:SaidaWin = $a.Saida
    $script:F.jobs_equal = ($a.Tree -eq $b.Tree)
    $a.Tree | Should -BeExactly $b.Tree
  }

  It '[W14] verificar --trilho=entre-plataformas: a saida Windows contra a Linux de referencia passa (exit 0, |d| <= 0,001*|ref|)' -Skip:(-not $script:HaveRef) {
    $script:SaidaWin | Should -Not -BeNullOrEmpty -Because 'o It anterior produz a entrega Windows'
    $ref = Join-Path $script:Ref 'linux-x86_64'
    $r = Invoke-Launcher 'cmd' $script:Root ('verificar "--saida=' + $script:SaidaWin + '" "--referencia=' + $ref + '" --trilho=entre-plataformas --format json')
    $script:F.verificar_exit = $r.Exit
    $r.Exit | Should -Be 0 -Because ((Get-AsciiText $r.Err) + (Get-AsciiText $r.Out))
    (Get-HighBytes ($r.Out + $r.Err)) | Should -Be 0
  }

  It '[W14] CONTROL: +0,2% em um y_pred (manifest regerado) => verificar exit 12' -Skip:(-not $script:HaveRef) {
    $script:SaidaWin | Should -Not -BeNullOrEmpty
    $bad = Join-Path $script:Work 'entrega-perturbada'
    if (Test-Path -LiteralPath $bad) { Remove-Item -LiteralPath $bad -Recurse -Force }
    $p = Invoke-Capture (Get-RscriptExe) ('--vanilla "' + (Join-Path $script:Repo 'tests\windows\control-perturb.R') + '" "--saida=' + $script:SaidaWin + '" "--out=' + $bad + '"')
    $p.Exit | Should -Be 0 -Because (Get-AsciiText $p.Err)
    $ref = Join-Path $script:Ref 'linux-x86_64'
    $r = Invoke-Launcher 'cmd' $script:Root ('verificar "--saida=' + $bad + '" "--referencia=' + $ref + '" --trilho=entre-plataformas --format json')
    $script:F.control_exit = $r.Exit
    $r.Exit | Should -Be 12
  }
}

Describe 'NFR de 30 min no Windows' {
  It '[W15] forecast pelo prevcarga.cmd <= 1800 s (duracao registrada pela rodada; deck real, artefatos oficiais sincronizados)' -Skip:(-not $script:HaveNfr) {
    $st = Join-Path $script:Work 'state-nfr'
    if (Test-Path -LiteralPath $st) { Remove-Item -LiteralPath $st -Recurse -Force }
    Copy-Item -LiteralPath $NfrState -Destination $st -Recurse -Force
    $r = Invoke-Launcher 'cmd' $script:Root ('forecast "--data=' + $NfrDeck + '" "--state=' + $st + '" --origin ' + $NfrOrigin + ' --format json')
    $r.Exit | Should -Be 0 -Because (Get-AsciiText $r.Err)
    $j = (Get-AsciiText $r.Out) | ConvertFrom-Json
    $res = $j.result
    $res.duracao_s | Should -Not -BeNullOrEmpty
    $soma = 0.0; foreach ($p in @($res.passos)) { $soma += [double]$p.duracao_s }
    [math]::Abs($soma - [double]$res.duracao_s) | Should -BeLessThan 5.0
    $mais = (@($res.passos) | Sort-Object { [double]$_.duracao_s } -Descending | Select-Object -First 1).passo
    $script:F.nfr = @{ duracao_s = [double]$res.duracao_s; jobs = 1; passo_mais_lento = [string]$mais; platform = [string]$res.platform
                       passos = @(@($res.passos) | ForEach-Object { @{ passo = [string]$_.passo; duracao_s = [double]$_.duracao_s } }) }
    ([double]$res.duracao_s) | Should -BeLessOrEqual 1800 -Because ('passo mais lento: ' + $mais)
    @(@($res.passos) | Where-Object { $_.passo -eq 'score' }).Count | Should -Be 0
  }
}
