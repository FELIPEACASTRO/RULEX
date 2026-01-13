# RULEX Performance Baseline

## Ambiente de Teste

- **Data:** 2026-01-05 22:39:40 UTC
- **Comando:** `docker run --rm -i --network host -v ${PWD}/perf:/perf grafana/k6 run --vus 50 --duration 30s /perf/load-test.js`
- **Infraestrutura:** Docker Compose (local Windows)
- **Backend:** Spring Boot 3.5.9 + Java 21 + Virtual Threads (habilitadas)
- **DB:** PostgreSQL 16-alpine (HikariCP pool: min=20, max=100, timeout=5s)
- **Cache:** Redis 7-alpine
- **Máquina:** Local Windows (detalhes não especificados)

## Configuração k6

- **VUs (Virtual Users):** 50
- **Duração:** 30 segundos
- **Timeout por request:** 5 segundos
- **Endpoint testado:** `POST http://localhost:8080/api/transactions/analyze`

## Resultados Baseline

### Métricas HTTP

| Métrica | Valor | SLO Target | Status |
|---------|-------|------------|--------|
| **TPS (Throughput)** | **45.9 req/s** | 1000 req/s | ❌ **22x ABAIXO** |
| **Latência p50** | 351ms | <200ms | ❌ 1.75x acima |
| **Latência p95** | **4.99s** | <200ms | ❌ **25x ACIMA** |
| **Latência p99** | **5.00s** | <200ms | ❌ **25x ACIMA** |
| **Latência média** | 924ms | - | ⚠️ Muito alta |
| **Latência min** | 87ms | - | ✅ Boa |
| **Latência max** | 5.01s | - | ❌ Timeout |
| **Taxa de falha HTTP** | **96.98%** | <1% | ❌ **96x ACIMA** |
| **Taxa de erro (critério custom)** | **100%** | <1% | ❌ **100x ACIMA** |

### Breakdown por Checks

| Check | Sucesso | Falha | Taxa de Sucesso |
|-------|---------|-------|-----------------|
| status is 200 | 48 | 1577 | **2.95%** |
| response has classification | 49 | 1576 | **3.02%** |
| latency < 200ms | 220 | 1405 | **13.53%** |

### Estatísticas Gerais

- **Total de requisições:** 1626
- **Iterações completas:** 1625
- **Duração média de iteração:** 933ms
- **Data received:** 758 kB (21 kB/s)
- **Data sent:** 1.1 MB (30 kB/s)

## Observações Críticas

### ❌ SLO NÃO ATINGIDO

O sistema está **criticamente abaixo** do SLO de 1000 TPS com p95/p99 < 200ms:

1. **Throughput:** Apenas 4.6% do target (46 vs 1000 req/s)
2. **Latência p95:** 25x acima do limite (4990ms vs 200ms)
3. **Timeouts:** Muitas requisições atingindo timeout de 5s

### Sintomas Observados

Durante o teste, foram observados:

- **Timeouts massivos:** ~150+ requisições com "request timeout" nos primeiros 10 segundos
- **Latências crescentes:** Sistema degrada progressivamente sob carga
- **Alta variabilidade:** p99 perto do timeout (5s) indica saturation/queueing

### Hipóteses de Bottleneck (A INVESTIGAR)

1. **DB writes síncronos:** Cada transação pode estar persistindo de forma síncrona (transactions, audit, velocity counters)
2. **Locks de banco:** Possível contenção em tabelas compartilhadas
3. **N+1 queries:** Possível carregamento lazy de entidades
4. **Overhead de auditoria:** Múltiplas inserções por request (AuditLog, AccessLog)
5. **Velocity counters:** Operações Redis ou DB para contadores podem estar serializando
6. **Bloom filters:** Reconstrução ou operações caras
7. **Thread pool saturation:** Apesar de virtual threads, pode haver blocking I/O
8. **Regras complexas:** Motor de avaliação pode ter lógica O(n²) ou pior
9. **Serialização JSON:** Payloads grandes sendo serializados múltiplas vezes
10. **Connection pool exhaustion:** HikariCP pool de 100 pode estar saturado

## Próximos Passos (FASE 4 — Performance)

### 1. Profiling (P0)
- Executar com Java Flight Recorder (JFR) ou async-profiler
- Gerar flamegraph para identificar hotspots
- Analisar logs de Hibernate (SQL queries)
- Verificar métricas Prometheus/Micrometer (se disponíveis)

### 2. Análise de Queries (P0)
- Habilitar log SQL com timing: `spring.jpa.show-sql=true` + `logging.level.org.hibernate.SQL=DEBUG`
- Identificar N+1 queries
- Verificar índices em tabelas (especialmente `transactions`, `audit_log`, `velocity_counter`)

### 3. Otimizações Imediatas (P1)
- Tornar auditoria assíncrona (@Async)
- Batch writes de velocity counters
- Avaliar cache de regras (evitar DB hits por request)
- Otimizar queries identificadas

### 4. Otimizações Algorítmicas (P1)
- Revisar lógica de motor de regras (complexidade Big O)
- Verificar loops aninhados
- Avaliar uso de estruturas de dados adequadas (HashMap vs List)

### 5. Re-teste Incremental (P0)
- Após cada otimização, reexecutar k6 com mesmos parâmetros
- Documentar delta de melhoria
- Registrar em `docs/perf-improvements.md`

## Validação Requerida

Para considerar SLO atingido, é necessário:

- ✅ TPS >= 1000 req/s
- ✅ p95 < 200ms
- ✅ p99 < 200ms (ou 500ms se negociado)
- ✅ Taxa de erro < 1%
- ✅ Taxa de falha HTTP < 1%

## Notas

- Este é o **primeiro baseline** capturado após commit `2d812cd`
- Sistema estava rodando em Docker Compose local (não otimizado para prod)
- Rate limiting foi **desabilitado** durante testes anteriores (via env var)
- Raw payload capture foi **desabilitado** durante testes anteriores (via env var)
- Logs foram reduzidos de INFO para DEBUG em hot paths

## Referências

- Script k6: [perf/load-test.js](../perf/load-test.js)
- Configuração HikariCP: [ADR-0002](adr/0002-hikaricp-pool-optimization.md)
- Commit base: `2d812cd` (branch: cursor/rulex-project-review-1c58)
