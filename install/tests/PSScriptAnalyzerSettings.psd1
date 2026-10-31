# PSScriptAnalyzer 1.22.0 (versao FIXA; o teste testthat de ponte instala/exige exatamente esta). Pester 5.5.0 fixo.
# Uso: Invoke-ScriptAnalyzer -Path install\install.ps1 -Settings install\tests\PSScriptAnalyzerSettings.psd1
# Criterio (AC 7): zero achados Error/Warning, com compatibilidade de sintaxe para Windows PowerShell 5.1 e pwsh 7.
@{
  Severity     = @('Error', 'Warning')
  IncludeRules = @(
    'PSUseCompatibleSyntax',
    'PSUseApprovedVerbs',
    'PSAvoidUsingWriteHost',
    'PSAvoidUsingInvokeExpression',
    'PSAvoidUsingEmptyCatchBlock',
    'PSAvoidUsingPlainTextForPassword',
    'PSAvoidUsingCmdletAliases',
    'PSAvoidAssignmentToAutomaticVariable',
    'PSUseShouldProcessForStateChangingFunctions',
    'PSUseBOMForUnicodeEncodedFile',
    'PSAvoidTrailingWhitespace',
    'PSMisleadingBacktick'
  )
  Rules        = @{
    PSUseCompatibleSyntax = @{
      Enable         = $true
      TargetVersions = @('5.1', '7.0')
    }
  }
}
