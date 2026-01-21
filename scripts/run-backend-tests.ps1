param(
  [string]$RepoRoot = (Resolve-Path "$PSScriptRoot\..")
)

$ErrorActionPreference = "Stop"

$repoRootPath = (Resolve-Path $RepoRoot).Path
$backendPath = Join-Path $repoRootPath "backend"
$evaluatorPath = Join-Path $backendPath "src\main\java\com\rulex\service\complex\ComplexRuleEvaluator.java"
$classPath = Join-Path $backendPath "target\classes\com\rulex\service\complex\ComplexRuleEvaluator.class"

if (Test-Path $evaluatorPath) {
  (Get-Item $evaluatorPath).LastWriteTime = Get-Date
}

Push-Location $backendPath
try {
  Write-Host "Compilando backend (skipTests)..."
  mvn -q -DskipTests compile

  if (-not (Test-Path $classPath)) {
    Write-Host "ComplexRuleEvaluator.class não encontrado, recompilando..."
    (Get-Item $evaluatorPath).LastWriteTime = Get-Date
    mvn -q -DskipTests compile
  }

  if (-not (Test-Path $classPath)) {
    throw "Falha ao gerar ComplexRuleEvaluator.class"
  }

  Write-Host "Executando testes backend..."
  mvn -q test
} finally {
  Pop-Location
}
