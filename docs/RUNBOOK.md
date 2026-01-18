# Runbook de Operações - RULEX

## Índice

1. [Health Checks](#health-checks)
2. [Troubleshooting](#troubleshooting)
3. [Métricas Importantes](#métricas-importantes)
4. [Procedimentos de Emergência](#procedimentos-de-emergência)
5. [Manutenção](#manutenção)

---

## Health Checks

### Verificar Saúde Geral

```bash
curl http://localhost:8080/api/actuator/health
```

Resposta esperada:
```json
{
  "status": "UP",
  "components": {
    "db": { "status": "UP" },
    "redis": { "status": "UP" },
    "neo4j": { "status": "UP" }
  }
}
```

### Verificar Componentes Individuais

```bash
# PostgreSQL
curl http://localhost:8080/api/actuator/health/db

# Redis
curl http://localhost:8080/api/actuator/health/redisHealth

# Neo4j
curl http://localhost:8080/api/actuator/health/neo4jHealth

# Disk Space
curl http://localhost:8080/api/actuator/health/diskSpace
```

### Verificar Status dos Operadores

```bash
curl http://localhost:8080/api/operators/status
```

---

## Troubleshooting

### Circuit Breaker Aberto (Neo4j)

**Sintoma**: Logs mostram `Circuit breaker 'neo4j' is OPEN`

**Diagnóstico**:
```bash
# Verificar métricas do circuit breaker
curl http://localhost:8080/api/actuator/metrics/resilience4j.circuitbreaker.state?tag=name:neo4j

# Verificar conectividade Neo4j
curl http://neo4j:7474

# Verificar logs do Neo4j
docker logs neo4j --tail 100
```

**Ação**:
1. Verificar se Neo4j está rodando: `docker ps | grep neo4j`
2. Verificar memória do Neo4j: `docker stats neo4j`
3. Aguardar 30s para half-open (recuperação automática)
4. Se persistir, reiniciar Neo4j: `docker restart neo4j`

**Impacto**: Análises de fraud ring retornam 0 (conservador)

---

### Circuit Breaker Aberto (Redis)

**Sintoma**: Logs mostram `Circuit breaker 'redis' is OPEN`

**Diagnóstico**:
```bash
# Verificar conectividade Redis
redis-cli -h localhost -p 6379 ping

# Verificar memória Redis
redis-cli info memory
```

**Ação**:
1. Verificar se Redis está rodando: `docker ps | grep redis`
2. Verificar uso de memória: `docker stats redis`
3. Aguardar 10s para half-open
4. Se persistir, reiniciar Redis: `docker restart redis`

**Impacto**: Velocity checks usam cache local (menos preciso)

---

### Alta Latência em Avaliações

**Sintoma**: P95 de `/api/evaluate` > 500ms

**Diagnóstico**:
```bash
# Verificar latência de avaliação
curl http://localhost:8080/api/actuator/metrics/rulex.evaluate.latency

# Verificar cache hits
curl http://localhost:8080/api/actuator/metrics/cache.gets

# Verificar pool de conexões
curl http://localhost:8080/api/actuator/metrics/hikaricp.connections.active
```

**Possíveis Causas**:
1. **Pool de conexões esgotado**: Aumentar `HIKARI_MAX_POOL_SIZE`
2. **Cache miss alto**: Verificar TTLs e tamanho do cache
3. **Queries lentas**: Verificar índices no PostgreSQL
4. **Neo4j lento**: Verificar queries com EXPLAIN

**Ação**:
```bash
# Aumentar pool temporariamente
docker exec backend env HIKARI_MAX_POOL_SIZE=150

# Limpar cache se necessário
curl -X POST http://localhost:8080/api/admin/cache/clear
```

---

### Operador Não Implementado (HTTP 501)

**Sintoma**: Resposta com status 501 e mensagem "Operador X não está implementado"

**Diagnóstico**:
```bash
# Verificar status do operador
curl http://localhost:8080/api/operators/status/NOME_DO_OPERADOR
```

**Ação**:
1. Se status é `PLANNED`, o operador não está implementado
2. Consultar `/api/operators/list?status=STABLE` para alternativas
3. Atualizar regra para usar operador implementado

---

### Erro de Configuração (Startup Failure)

**Sintoma**: Backend não inicia, log mostra `ConfigurationException`

**Diagnóstico**:
```bash
docker logs backend --tail 50
```

**Causas Comuns**:
1. `RULEX_ADMIN_PASSWORD` não configurado
2. Senha muito fraca (< 12 caracteres)
3. Variáveis de ambiente faltando

**Ação**:
```bash
# Verificar variáveis
docker exec backend env | grep RULEX

# Configurar senha forte
export RULEX_ADMIN_PASSWORD="SenhaForte123!@#"
docker compose up -d backend
```

---

## Métricas Importantes

### Thresholds de Alerta

| Métrica | Warning | Critical | Ação |
|---------|---------|----------|------|
| `rulex.evaluate.latency.p95` | > 300ms | > 500ms | Investigar |
| `hikaricp.connections.active` | > 70% pool | > 90% pool | Aumentar pool |
| `resilience4j.circuitbreaker.state` | HALF_OPEN | OPEN | Verificar dependência |
| `jvm.memory.used` | > 70% | > 85% | Aumentar heap |
| `http.server.requests.error.rate` | > 1% | > 5% | Investigar erros |

### Dashboards Grafana

- **RULEX Overview**: Visão geral do sistema
- **RULEX Rules**: Performance de regras
- **RULEX Fraud**: Métricas de fraude detectada

### Prometheus Queries Úteis

```promql
# Taxa de erros
rate(http_server_requests_seconds_count{status=~"5.."}[5m])

# Latência P95
histogram_quantile(0.95, rate(http_server_requests_seconds_bucket[5m]))

# Circuit breaker state
resilience4j_circuitbreaker_state{name="neo4j"}

# Regras mais acionadas
topk(10, sum by (rule) (rulex_rule_triggered_total))
```

---

## Procedimentos de Emergência

### Desabilitar Regra Problemática

```bash
# Via API
curl -X PATCH http://localhost:8080/api/rules/{id}/toggle \
  -H "Content-Type: application/json" \
  -d '{"enabled": false}'
```

### Rollback de Regra

```bash
# Listar versões
curl http://localhost:8080/api/rules/{id}/history

# Rollback para versão anterior
curl -X POST http://localhost:8080/api/rules/{id}/rollback/{version}
```

### Modo Degradado (Desabilitar Neo4j)

```bash
# Desabilitar Neo4j temporariamente
docker exec backend env RULEX_NEO4J_ENABLED=false

# Reiniciar para aplicar
docker restart backend
```

### Limpar Cache de Emergência

```bash
# Limpar cache Redis
redis-cli FLUSHDB

# Limpar cache local
curl -X POST http://localhost:8080/api/admin/cache/clear
```

---

## Manutenção

### Backup do Banco

```bash
# PostgreSQL
docker exec postgres pg_dump -U postgres rulex_db > backup_$(date +%Y%m%d).sql

# Neo4j
docker exec neo4j neo4j-admin database dump neo4j --to-path=/backups
```

### Limpeza de Logs

```bash
# Limpar logs antigos (> 7 dias)
find /var/log/rulex -name "*.log" -mtime +7 -delete
```

### Atualização de Versão

1. Fazer backup do banco
2. Parar serviços: `docker compose down`
3. Atualizar imagens: `docker compose pull`
4. Iniciar serviços: `docker compose up -d`
5. Verificar health: `curl /api/actuator/health`
6. Verificar logs: `docker logs backend --tail 100`

### Rotação de Credenciais

1. Gerar nova senha forte
2. Atualizar variável de ambiente
3. Reiniciar backend
4. Atualizar clientes (se necessário)

---

## Contatos

- **Suporte L1**: support@rulex.local
- **Escalação L2**: oncall@rulex.local
- **Emergência**: +55 11 99999-9999
