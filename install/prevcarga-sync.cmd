@echo off
rem prevcarga-sync.cmd -- shim fino do prevcarga-sync.ps1 (Windows PowerShell 5.1 e pwsh 7 via powershell.exe). Nao define variavel.
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0prevcarga-sync.ps1" %*
exit /b %ERRORLEVEL%
