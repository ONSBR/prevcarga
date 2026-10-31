@echo off
setlocal
rem prevcarga.cmd -- lancador fino (Windows). Nao define variavel (so limpa as herdadas abaixo): runtime.R fixa TZ/locale/BLAS in-process.
rem Limpa so neste processo (equivale ao `env -u ...` do Linux) as variaveis herdadas que poriam uma library de fora no .libPaths() do bundle: R_HOME,
rem R_LIBS, R_LIBS_USER, R_LIBS_SITE, R_PROFILE_USER e R_ENVIRON_USER. Sao as UNICAS atribuicoes permitidas (todas vazias).
set "R_HOME="
set "R_LIBS="
set "R_LIBS_USER="
set "R_LIBS_SITE="
set "R_PROFILE_USER="
set "R_ENVIRON_USER="
rem <R> = pasta acima de bin\; o ponteiro current (1 linha) escolhe a versao. Sem blocos entre parenteses:
rem um caminho como "Program Files (x86)" quebraria o parser do cmd dentro deles.
if not exist "%~dp0..\current" goto nocurrent
for /f "usebackq delims=" %%v in ("%~dp0..\current") do if exist "%~dp0..\versions\%%v\runtime\R\bin\Rscript.exe" "%~dp0..\versions\%%v\runtime\R\bin\Rscript.exe" --vanilla "%~dp0..\versions\%%v\code\scripts\prevcarga" %* & exit /b
1>&2 echo prevcarga: erro [bundle] (exit 6)
1>&2 echo o que: versao apontada por current ausente
1>&2 echo onde: %~dp0..\versions
1>&2 echo como corrigir: install.ps1 -Rollback ou reinstale
exit /b 6
:nocurrent
1>&2 echo prevcarga: erro [bundle] (exit 6)
1>&2 echo o que: ponteiro current ausente
1>&2 echo onde: %~dp0..\current
1>&2 echo como corrigir: reinstale com install.ps1
exit /b 6
