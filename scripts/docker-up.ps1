Param(
  [switch]$NoBuild,
  [int]$TimeoutSeconds = 120,
  [string]$Url = "http://localhost:5173/",
  [string]$BackendHealthUrl = "http://localhost:8080/api/actuator/health"
)

$ErrorActionPreference = "Stop"

Write-Host "Starting RULEX via Docker Compose..." -ForegroundColor Cyan

$composeArgs = @("compose","up","-d")
if (-not $NoBuild) {
  $composeArgs += "--build"
}

docker @composeArgs | Out-Host

Write-Host "Waiting for web to be ready: $Url" -ForegroundColor Cyan

$webDeadline = (Get-Date).AddSeconds($TimeoutSeconds)
$webLastError = $null

while ((Get-Date) -lt $webDeadline) {
  try {
    $resp = Invoke-WebRequest -Uri $Url -UseBasicParsing -TimeoutSec 3
    if ($resp.StatusCode -ge 200 -and $resp.StatusCode -lt 400) {
      break
    }
  } catch {
    $webLastError = $_
  }

  Start-Sleep -Seconds 2
}

if ((Get-Date) -ge $webDeadline) {
  Write-Host "Timed out waiting for $Url" -ForegroundColor Red
  if ($webLastError) {
    Write-Host "Last error: $($webLastError.Exception.Message)" -ForegroundColor DarkGray
  }
  Write-Host "Tip: check containers with: docker compose ps" -ForegroundColor Yellow
  Write-Host "Tip: view logs with: docker compose logs -f" -ForegroundColor Yellow
  exit 1
}

Write-Host "Waiting for backend health to be ready: $BackendHealthUrl" -ForegroundColor Cyan

$backendDeadline = (Get-Date).AddSeconds($TimeoutSeconds)
$backendLastError = $null

while ((Get-Date) -lt $backendDeadline) {
  try {
    $backendResp = Invoke-WebRequest -Uri $BackendHealthUrl -UseBasicParsing -TimeoutSec 3
    if ($backendResp.StatusCode -ge 200 -and $backendResp.StatusCode -lt 400) {
      Write-Host "Backend is healthy (HTTP $($backendResp.StatusCode)). Opening browser..." -ForegroundColor Green
      Start-Process $Url
      exit 0
    }
  } catch {
    $backendLastError = $_
  }

  Start-Sleep -Seconds 2
}

Write-Host "Timed out waiting for backend health at $BackendHealthUrl" -ForegroundColor Red
if ($backendLastError) {
  Write-Host "Last error: $($backendLastError.Exception.Message)" -ForegroundColor DarkGray
}

Write-Host "Tip: check containers with: docker compose ps" -ForegroundColor Yellow
Write-Host "Tip: view logs with: docker compose logs -f" -ForegroundColor Yellow
exit 1
