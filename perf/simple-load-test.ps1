# Simple sync load test
$numRequests = 30
$baseUrl = "http://localhost:8080"

# Payload completo com todos os campos obrigatórios
$payload = @{
    externalTransactionId = "TXN-{0}"
    customerIdFromHeader = "CUST-123"
    customerAcctNumber = 123456789
    pan = "4111111111111111"
    merchantId = "MERCH-456"
    transactionCurrencyCode = 986
    transactionType = "PURCHASE"
    transactionAmount = 150.75
    transactionDate = 20260105
    transactionTime = 201700
    mcc = 5411
    consumerAuthenticationScore = "05"
    externalScore3 = 50
    cavvResult = "2"
    eciIndicator = "05"
    atcCard = 1
    atcHost = 1
    tokenAssuranceLevel = "00"
    availableCredit = 5000.00
    cardCashBalance = 0.00
    cardDelinquentAmount = 0.00
} | ConvertTo-Json -Compress

$headers = @{
    "Content-Type" = "application/json"
    "X-Customer-ID" = "CUST-123"
    "Authorization" = "Basic " + [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes("admin:rulex"))
}

Write-Host "Starting $numRequests requests..." -ForegroundColor Cyan
$results = @()
$startTime = Get-Date

for ($i = 1; $i -le $numRequests; $i++) {
    $testPayload = $payload -replace "TXN-\{0\}", "TXN-SYNC-$i"
    $reqStart = Get-Date
    
    try {
        $response = Invoke-RestMethod `
            -Uri "$baseUrl/api/transactions/analyze" `
            -Method POST `
            -Headers $headers `
            -Body $testPayload `
            -TimeoutSec 10
        
        $reqEnd = Get-Date
        $duration = ($reqEnd - $reqStart).TotalMilliseconds
        
        $results += @{
            Success = $true
            Duration = $duration
        }
        
        Write-Host "[$i/$numRequests] OK - $([Math]::Round($duration, 0))ms" -ForegroundColor Green
    } catch {
        $reqEnd = Get-Date
        $duration = ($reqEnd - $reqStart).TotalMilliseconds
        
        $results += @{
            Success = $false
            Duration = $duration
            Error = $_.Exception.Message
        }
        
        Write-Host "[$i/$numRequests] FAIL - $([Math]::Round($duration, 0))ms" -ForegroundColor Red
    }
}

$endTime = Get-Date
$totalDuration = ($endTime - $startTime).TotalSeconds

Write-Host ""
Write-Host "=== RESULTS ===" -ForegroundColor Cyan

$successful = @($results | Where-Object { $_.Success -eq $true })
$failed = @($results | Where-Object { $_.Success -eq $false })

Write-Host "Successful: $($successful.Count)/$numRequests"
Write-Host "Failed: $($failed.Count)"
Write-Host "Total time: $([Math]::Round($totalDuration, 2))s"
Write-Host "Throughput: $([Math]::Round($numRequests / $totalDuration, 2)) req/s"

if ($successful.Count -gt 0) {
    $latencies = $successful | ForEach-Object { $_.Duration } | Sort-Object
    $p50Index = [Math]::Floor($latencies.Count * 0.50)
    $p95Index = [Math]::Floor($latencies.Count * 0.95)
    
    $avg = ($latencies | Measure-Object -Average).Average
    $min = ($latencies | Measure-Object -Minimum).Minimum
    $max = ($latencies | Measure-Object -Maximum).Maximum
    
    Write-Host ""
    Write-Host "Latency:"
    Write-Host "  Min: $([Math]::Round($min, 0))ms"
    Write-Host "  Avg: $([Math]::Round($avg, 0))ms"
    Write-Host "  p50: $([Math]::Round($latencies[$p50Index], 0))ms"
    Write-Host "  p95: $([Math]::Round($latencies[$p95Index], 0))ms"
    Write-Host "  Max: $([Math]::Round($max, 0))ms"
    
    $p95 = $latencies[$p95Index]
    $p95Status = if ($p95 -le 200) { "PASS" } else { "FAIL" }
    Write-Host ""
    Write-Host "SLO Check (p95 < 200ms): $p95Status"
}
