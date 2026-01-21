param(
  [string]$RepoRoot = (Resolve-Path "$PSScriptRoot\.."),
  [string]$MavenArgs = "test"
)

$ErrorActionPreference = "Stop"

$repoRootPath = (Resolve-Path $RepoRoot).Path
$tempRoot = Join-Path $env:TEMP ("rulex-backend-test-" + [guid]::NewGuid().ToString("N"))

Write-Host "Copiando backend para diretório temporário: $tempRoot"
New-Item -ItemType Directory -Path $tempRoot | Out-Null
Copy-Item -Path (Join-Path $repoRootPath "backend") -Destination $tempRoot -Recurse -Force

$tempBackendPath = Join-Path $tempRoot "backend"
$tempTargetPath = Join-Path $tempBackendPath "target"
if (Test-Path $tempTargetPath) {
  Write-Host "Removendo target copiado para forçar compilação limpa"
  Remove-Item -Path $tempTargetPath -Recurse -Force
}

Push-Location $tempBackendPath
try {
  $previousProfile = $env:SPRING_PROFILES_ACTIVE
  $env:SPRING_PROFILES_ACTIVE = "test"
  Write-Host "Executando Maven: mvn -q $MavenArgs"
  mvn -q $MavenArgs
} finally {
  $env:SPRING_PROFILES_ACTIVE = $previousProfile
  Pop-Location
  Write-Host "Removendo diretório temporário: $tempRoot"
  Remove-Item -Path $tempRoot -Recurse -Force
}
