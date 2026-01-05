# Performance Report — RULEX Engine

## Hotspots Identificados (com Evidência)

### 1. Avaliação de Condições — O(N × M)

**Evidência:**
- Arquivo: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Método: `analyzeTransaction()` → `evaluateRulesAgainstTransaction()`
- Linhas: ~500-700

**Problema:**
Para cada transação, todas as regras habilitadas são avaliadas:
```
N transações × M regras = O(N × M) avaliações
```

**Mitigação Existente:**
- `RuleOrderingService` (linha 61): ordena regras por hit-rate para short-circuit
- `CandidateIndexCache` (linha 1673): cache de regras candidatas
- `BloomFilterService`: filtro probabilístico para descartar regras rapidamente

**Complexidade Atual:**
- Melhor caso: O(N × 1) — primeira regra dispara FRAUD
- Pior caso: O(N × M) — todas as regras são avaliadas
- Caso médio: O(N × M/2) — com short-circuit

### 2. Switch/Case de Operadores — O(K)

**Evidência:**
- Arquivo: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Método: `evaluateCondition()` — linhas 1677-1850
- 93 ocorrências de switch/case

**Problema:**
Switch/case com ~30 operadores = O(K) onde K = número de operadores.
Em Java, switch com String usa hashCode + equals, então é O(1) amortizado.

**Recomendação:**
Manter switch (já é O(1) amortizado) ou migrar para Map<String, Operator> para melhor extensibilidade.

### 3. Velocity Queries — O(1) com Redis

**Evidência:**
- Arquivo: `backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java`
- Métodos: `getCount()`, `getSum()` — linhas 141, 165

**Complexidade:**
- Redis GET: O(1)
- Redis INCR: O(1)

**Problema Potencial:**
Operações não atômicas (linhas 289-294):
```java
redisTemplate.opsForValue().increment(countKey);
// ...
redisTemplate.opsForValue().increment(sumKey, amountCents);
```

**Recomendação:**
Usar MULTI/EXEC ou Lua script para atomicidade.

### 4. Parsing de Expressões — O(L)

**Evidência:**
- Arquivo: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Método: `readComputedLeftValue()` — linhas 1851-1906
- Método: `evalNumericExpression()` — linhas 1950-1967

**Complexidade:**
- Parsing de campo: O(L) onde L = comprimento da expressão
- Parsing repetido para cada avaliação

**Recomendação:**
Pré-compilar expressões em cache (AST) para evitar re-parsing.

### 5. Regex Matching — O(L × P)

**Evidência:**
- Arquivo: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Método: `matchesRegex()` — linha 1786
- Usa `RegexValidator.safeFind()` com proteção ReDoS

**Complexidade:**
- Regex simples: O(L) onde L = tamanho do input
- Regex com backtracking: O(2^L) — mitigado por timeout

**Mitigação Existente:**
- `RegexValidator` com timeout para prevenir ReDoS

## Métricas de Performance

### Endpoints Críticos

| Endpoint | Complexidade | Latência Esperada |
|----------|--------------|-------------------|
| POST /api/evaluate | O(M) | < 50ms (p99) |
| POST /api/transactions/analyze | O(M) | < 100ms (p99) |
| GET /api/rules | O(1) | < 10ms (p99) |

### Recomendações de Monitoramento

1. **Métricas por Regra:**
   - Tempo de avaliação por regra
   - Taxa de disparo (hit-rate)
   - Número de condições avaliadas

2. **Métricas Globais:**
   - Latência p50/p95/p99 do endpoint /evaluate
   - Throughput (transações/segundo)
   - Cache hit-rate (Redis, BloomFilter)

## Otimizações Futuras

### Curto Prazo (P0)
1. Pré-compilar expressões de campo
2. Atomicidade em Redis (Lua script)

### Médio Prazo (P1)
1. Indexação de regras por tipo de transação
2. Cache de resultados de condições invariantes

### Longo Prazo (P2)
1. Avaliação paralela de regras independentes
2. Compilação JIT de regras frequentes

## Validação

```bash
# Executar testes de performance (se existirem)
cd ~/repos/RULEX && mvn -f backend/pom.xml test -Dtest=*Perf*

# Verificar métricas do Actuator
curl http://localhost:8080/actuator/metrics/http.server.requests
```
