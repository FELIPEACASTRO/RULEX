# Performance Report — RULEX Engine

## SLO (Service Level Objectives)

| Métrica | Target | Status |
|---------|--------|--------|
| TPS | >= 1000 | ❌ NÃO ATINGIDO (max ~84 TPS) |
| p95 Latência | <= 200ms | ⚠️ Parcial (OK até 10 VUs) |
| p99 Latência | <= 500ms | ✅ OK |
| Taxa de Erro | < 1% | ⚠️ Parcial (OK até 10 VUs) |

## Baseline Medido (2026-01-05)

### Ambiente de Teste
- **Máquina**: Docker local (8 vCPUs, 32GB RAM)
- **Backend**: Spring Boot 3.5.9, Java 21
- **Banco**: PostgreSQL 16-alpine (HikariCP pool=20)
- **Cache**: Redis 7-alpine
- **Ferramenta**: k6 v1.5.0

### Resultados por Carga

| VUs | TPS | p50 | p95 | p99 | Erro % | Status |
|-----|-----|-----|-----|-----|--------|--------|
| 5 | 47 | 92ms | 109ms | 126ms | 0% | ✅ OK |
| 10 | 77 | 109ms | 170ms | 198ms | 0.98% | ✅ OK |
| 20 | 84 | 201ms | 356ms | 385ms | 53% | ❌ FAIL |
| 50 | ~10 | 5s | 5s | 5s | 100% | ❌ FAIL |

### Gargalos Identificados

#### 1. Pool de Conexões HikariCP (P0 - CRÍTICO)

**Evidência:**
```
java.sql.SQLTransientConnectionException: RulexHikariPool - Connection is not available, 
request timed out after 30000ms (total=20, active=20, idle=0, waiting=312)
```

**Problema:**
- Pool configurado com apenas 20 conexões
- Com 50 VUs, 312 requisições ficam esperando
- Timeout de 30s causa falha em cascata

**Solução Proposta:**
```yaml
# application.yml
spring:
  datasource:
    hikari:
      maximum-pool-size: 100
      minimum-idle: 20
      connection-timeout: 5000
      idle-timeout: 300000
```

**Impacto Esperado:**
- Suportar até 100 conexões simultâneas
- Reduzir timeouts significativamente

#### 2. Múltiplas Queries por Transação (P1)

**Evidência:**
- Cada transação executa: INSERT transaction + INSERT decision + INSERT audit
- 3 roundtrips ao banco por requisição

**Solução Proposta:**
- Batch inserts para audit logs
- Async audit logging (não bloquear resposta)
- Considerar COPY para bulk inserts

#### 3. Avaliação de Regras (P1)

**Evidência:**
- `RuleEngineService.java` com 2205 linhas
- Avaliação sequencial de todas as regras habilitadas

**Solução Proposta:**
- Cache de regras em memória (já existe parcialmente)
- Short-circuit em AND/OR
- Ordenar regras por seletividade

#### 4. Serialização JSON (P2)

**Evidência:**
- Payload de ~1KB por requisição
- Serialização/deserialização em cada request

**Solução Proposta:**
- Considerar compressão gzip para payloads grandes
- Validar apenas campos necessários

## Plano de Otimização

### Fase 1: Pool de Conexões (P0)
1. Aumentar `maximum-pool-size` para 100
2. Ajustar `connection-timeout` para 5s
3. Monitorar métricas do HikariCP

### Fase 2: Async Audit (P1)
1. Mover audit logging para thread separada
2. Usar batch inserts para audit_logs
3. Não bloquear resposta da transação

### Fase 3: Cache de Regras (P1)
1. Pré-carregar regras habilitadas no startup
2. Invalidar cache apenas quando regra é alterada
3. Usar cache local (não Redis) para regras

### Fase 4: Otimização de Queries (P2)
1. Analisar EXPLAIN ANALYZE das queries principais
2. Adicionar índices se necessário
3. Considerar read replicas para queries de leitura

## Comandos de Teste

```bash
# Instalar k6
sudo apt-get install k6

# Subir infraestrutura (sem rate limit)
cd ~/repos/RULEX
RULEX_RATE_LIMIT_ENABLED=false docker compose up -d

# Smoke test (5 VUs)
k6 run --vus 5 --duration 30s perf/load-test.js

# Load test (50 VUs)
k6 run --vus 50 --duration 60s perf/load-test.js

# Stress test (100 VUs)
k6 run --vus 100 --duration 120s perf/load-test.js
```

## Métricas de Monitoramento

### Actuator Endpoints
- `/actuator/health` - Health check
- `/actuator/metrics/hikaricp.connections` - Pool de conexões
- `/actuator/metrics/http.server.requests` - Latência HTTP
- `/actuator/prometheus` - Métricas Prometheus

### Alertas Recomendados
- HikariCP active connections > 80% do pool
- p95 latência > 200ms
- Taxa de erro > 1%
- TPS < 500 (50% do target)

## Conclusão

**Status Atual**: O sistema NÃO atinge o SLO de 1000 TPS.

**Capacidade Atual**: ~77 TPS com p95 < 200ms (10 VUs)

**Para atingir 1000 TPS**, é necessário:
1. ✅ Desabilitar rate limiting (feito para testes)
2. ⏳ Aumentar pool de conexões (P0)
3. ⏳ Implementar async audit (P1)
4. ⏳ Otimizar cache de regras (P1)
5. ⏳ Escalar horizontalmente (múltiplas instâncias)

**Estimativa**: Com as otimizações P0 e P1, espera-se atingir ~300-500 TPS. Para 1000 TPS, será necessário escalar horizontalmente com load balancer.
