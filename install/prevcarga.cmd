@echo off
rem prevcarga.cmd -- lancador fino (Windows). Nao define variavel: runtime.R fixa TZ/locale/BLAS in-process.
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
