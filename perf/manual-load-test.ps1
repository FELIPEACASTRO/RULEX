# Manual Load Test para RULEX (substituição temporária do k6)
# Objetivo: Validar performance antes/depois de otimizações

param(
    [int]$NumRequests = 100,
    [int]$Concurrency = 10,
    [string]$BaseUrl = "http://localhost:8080"
)

$ErrorActionPreference = "Stop"

Write-Host "=== RULEX Manual Load Test ===" -ForegroundColor Cyan
Write-Host "Requests: $NumRequests"
Write-Host "Concurrency: $Concurrency"
Write-Host "Base URL: $BaseUrl"
Write-Host ""

# Payload de teste (simulando transação)
$payload = @{
    externalTransactionId = "TXN-LOAD-{0}"
    customerId = "CUST-123"
    merchantId = "MERCH-456"
    transactionAmount = 150.75
    transactionCurrency = "BRL"
    transactionTimestamp = (Get-Date).ToString("yyyy-MM-ddTHH:mm:ss")
    cardPan = "4111111111111111"
    cardholderName = "TESTE LOAD"
    billingAddress = @{
        country = "BR"
        zipCode = "01310-100"
        city = "Sao Paulo"
        state = "SP"
    }
    paymentMethod = "CREDIT_CARD"
    transactionType = "PURCHASE"
} | ConvertTo-Json -Compress

$headers = @{
    "Content-Type" = "application/json"
    "X-Customer-ID" = "CUST-123"
}

# Função para enviar request e medir latência
function Invoke-LoadTest {
    param([int]$RequestId)
    
    $testPayload = $payload -replace "TXN-LOAD-\{0\}", "TXN-LOAD-$RequestId"
    $startTime = Get-Date
    
    try {
        $response = Invoke-RestMethod `
            -Uri "$BaseUrl/api/transactions/analyze" `
            -Method POST `
            -Headers $headers `
            -Body $testPayload `
            -TimeoutSec 10
        
        $endTime = Get-Date
        $duration = ($endTime - $startTime).TotalMilliseconds
        
        return @{
            Success = $true
            Duration = $duration
            StatusCode = 200
        }
    } catch {
        $endTime = Get-Date
        $duration = ($endTime - $startTime).TotalMilliseconds
        
        return @{
            Success = $false
            Duration = $duration
            StatusCode = 0
            Error = $_.Exception.Message
        }
    }
}

# Executar load test
Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Starting load test..." -ForegroundColor Yellow

$results = [System.Collections.ArrayList]::new()
$startTestTime = Get-Date

# Executar em batches (simulando concorrência limitada)
$batchSize = $Concurrency
for ($i = 0; $i -lt $NumRequests; $i += $batchSize) {
    $batchEnd = [Math]::Min($i + $batchSize, $NumRequests)
    $batchJobs = @()
    
    for ($j = $i; $j -lt $batchEnd; $j++) {
        $batchJobs += Start-Job -ScriptBlock {
            param($FunctionDef, $ReqId, $Url, $Hdrs, $Pld)
            
            # Re-definir função no job context
            Invoke-Expression $FunctionDef
            Invoke-LoadTest -RequestId $ReqId
        } -ArgumentList ${function:Invoke-LoadTest}.ToString(), $j, $BaseUrl, $headers, $payload
    }
    
    # Aguardar batch completar
    $batchResults = $batchJobs | Wait-Job | Receive-Job
    $batchJobs | Remove-Job
    
    foreach ($result in $batchResults) {
        [void]$results.Add($result)
    }
    
    $progress = [Math]::Floor(($i / $NumRequests) * 100)
    Write-Host "[$(Get-Date -Format 'HH:mm:ss')] Progress: $progress% ($($results.Count)/$NumRequests)" -ForegroundColor Gray
}

$endTestTime = Get-Date
$totalDuration = ($endTestTime - $startTestTime).TotalSeconds

# Analisar resultados
Write-Host ""
Write-Host "=== RESULTS ===" -ForegroundColor Cyan

$successfulResults = $results | Where-Object { $_.Success -eq $true }
$failedResults = $results | Where-Object { $_.Success -eq $false }

$successCount = $successfulResults.Count
$failCount = $failedResults.Count
$successRate = if ($results.Count -gt 0) { ($successCount / $results.Count) * 100 } else { 0 }

Write-Host "Total Requests:    $($results.Count)"
Write-Host "Successful:        $successCount ($([Math]::Round($successRate, 2))%)"
Write-Host "Failed:            $failCount"
Write-Host "Total Duration:    $([Math]::Round($totalDuration, 2))s"
Write-Host "Throughput:        $([Math]::Round($results.Count / $totalDuration, 2)) req/s"
Write-Host ""

if ($successfulResults.Count -gt 0) {
    $latencies = $successfulResults | Select-Object -ExpandProperty Duration | Sort-Object
    $p50Index = [Math]::Floor($latencies.Count * 0.50)
    $p95Index = [Math]::Floor($latencies.Count * 0.95)
    $p99Index = [Math]::Floor($latencies.Count * 0.99)
    
    $avgLatency = ($latencies | Measure-Object -Average).Average
    $minLatency = ($latencies | Measure-Object -Minimum).Minimum
    $maxLatency = ($latencies | Measure-Object -Maximum).Maximum
    $p50Latency = $latencies[$p50Index]
    $p95Latency = $latencies[$p95Index]
    $p99Latency = $latencies[$p99Index]
    
    Write-Host "Latency Stats (successful requests):"
    Write-Host "  Min:  $([Math]::Round($minLatency, 2))ms"
    Write-Host "  Avg:  $([Math]::Round($avgLatency, 2))ms"
    Write-Host "  p50:  $([Math]::Round($p50Latency, 2))ms"
    Write-Host "  p95:  $([Math]::Round($p95Latency, 2))ms"
    Write-Host "  p99:  $([Math]::Round($p99Latency, 2))ms"
    Write-Host "  Max:  $([Math]::Round($maxLatency, 2))ms"
    Write-Host ""
    
    # Validar SLO
    $sloP95 = 200
    $sloTPS = 1000
    $actualTPS = $results.Count / $totalDuration
    
    Write-Host "=== SLO VALIDATION ===" -ForegroundColor Cyan
    $p95Status = if ($p95Latency -le $sloP95) { "PASS" } else { "FAIL" }
    $tpsStatus = if ($actualTPS -ge $sloTPS) { "PASS" } else { "FAIL (need more load)" }
    
    Write-Host "p95 < 200ms:       $p95Status ($([Math]::Round($p95Latency, 2))ms)"
    Write-Host "TPS >= 1000:       $tpsStatus ($([Math]::Round($actualTPS, 2)) req/s)"
}

if ($failedResults.Count -gt 0) {
    Write-Host ""
    Write-Host "=== FAILED REQUESTS SAMPLE ===" -ForegroundColor Red
    $failedResults | Select-Object -First 5 | ForEach-Object {
        Write-Host "  Duration: $([Math]::Round($_.Duration, 2))ms, Error: $($_.Error)"
    }
}
