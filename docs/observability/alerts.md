# OBS-002: Alertas e SLOs - RULEX

## Service Level Objectives (SLOs)

### SLOs Primários

| Métrica | Target | Crítico |
| ------- | ------ | ------- |
| Disponibilidade | 99.9% | < 99.5% |
| Latência P95 (evaluate) | < 100ms | > 500ms |
| Latência P99 (evaluate) | < 200ms | > 1s |
| Taxa de Erro | < 0.1% | > 1% |
| Throughput | > 1000 TPS | < 500 TPS |

### SLOs por Componente

| Componente | Disponibilidade | Latência P95 |
| ---------- | --------------- | ------------ |
| API Gateway | 99.95% | 50ms |
| Rule Engine | 99.9% | 80ms |
| PostgreSQL | 99.9% | 20ms |
| Redis | 99.95% | 5ms |
| Neo4j | 99.5% | 100ms |

## Alertas Prometheus

> Nota: métricas Micrometer com pontos (`.`) são exportadas no Prometheus com
> underscores (`_`). Ex.: `rulex.webhook.dlq.pending` ->
> `rulex_webhook_dlq_pending` e `rulex.flyway.migrations.failed` ->
> `rulex_flyway_migrations_failed`.

### 1. Disponibilidade

```yaml
# alerts/availability.yml
groups:
  - name: rulex-availability
    rules:
      - alert: RulexApiDown
        expr: up{job="rulex-backend"} == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "RULEX API está DOWN"
          description: >-
            Instância {{ $labels.instance }} não está respondendo há mais de 1 minuto

      - alert: RulexHighErrorRate
        expr: |
          sum(rate(http_server_requests_seconds_count{status=~"5.."}[5m])) 
          / sum(rate(http_server_requests_seconds_count[5m])) > 0.01
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Taxa de erro alta no RULEX"
          description: >-
            Taxa de erro 5xx está em {{ $value | humanizePercentage }}

      - alert: RulexCriticalErrorRate
        expr: |
          sum(rate(http_server_requests_seconds_count{status=~"5.."}[5m])) 
          / sum(rate(http_server_requests_seconds_count[5m])) > 0.05
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Taxa de erro CRÍTICA no RULEX"
          description: >-
            Taxa de erro 5xx está em {{ $value | humanizePercentage }} - AÇÃO
            IMEDIATA NECESSÁRIA
```

### 2. Latência

```yaml
# alerts/latency.yml
groups:
  - name: rulex-latency
    rules:
      - alert: RulexHighLatencyP95
        expr: |
          histogram_quantile(0.95, 
            sum(rate(http_server_requests_seconds_bucket{uri="/api/evaluate"}[5m]))
            by (le)
          ) > 0.1
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Latência P95 alta no endpoint /evaluate"
          description: "P95 está em {{ $value | humanizeDuration }}"

      - alert: RulexCriticalLatencyP99
        expr: |
          histogram_quantile(0.99, 
            sum(rate(http_server_requests_seconds_bucket{uri="/api/evaluate"}[5m]))
            by (le)
          ) > 1
        for: 2m
        labels:
          severity: critical
        annotations:
          summary: "Latência P99 CRÍTICA no endpoint /evaluate"
          description: >-
            P99 está em {{ $value | humanizeDuration }} - Investigar imediatamente

      - alert: RulexSlowRuleEvaluation
        expr: |
          histogram_quantile(0.95, 
            sum(rate(rulex_rule_evaluation_duration_seconds_bucket[5m])) by (le)
          ) > 0.05
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Avaliação de regras lenta"
          description: >-
            P95 de avaliação de regras está em {{ $value | humanizeDuration }}
```

### 3. Infraestrutura

```yaml
# alerts/infrastructure.yml
groups:
  - name: rulex-infrastructure
    rules:
      # PostgreSQL
      - alert: PostgresConnectionPoolExhausted
        expr: hikaricp_connections_active / hikaricp_connections_max > 0.9
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Pool de conexões PostgreSQL quase esgotado"
          description: "{{ $value | humanizePercentage }} do pool em uso"

      - alert: PostgresDown
        expr: pg_up == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "PostgreSQL está DOWN"

      # Redis
      - alert: RedisDown
        expr: redis_up == 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Redis está DOWN"

      - alert: RedisHighMemoryUsage
        expr: redis_memory_used_bytes / redis_memory_max_bytes > 0.9
        for: 5m
        labels:
          severity: warning
        annotations:
          summary: "Redis com uso de memória alto"
          description: "{{ $value | humanizePercentage }} da memória em uso"

      # Neo4j
      - alert: Neo4jDown
        expr: neo4j_up == 0
        for: 2m
        labels:
          severity: warning
        annotations:
          summary: "Neo4j está DOWN"
          description: "Operadores de grafo retornarão valores conservadores"

      - alert: Neo4jCircuitBreakerOpen
        expr: resilience4j_circuitbreaker_state{name="neo4j"} == 1
        for: 1m
        labels:
          severity: warning
        annotations:
          summary: "Circuit breaker do Neo4j está ABERTO"
```

### 4. Negócio

```yaml
# alerts/business.yml
groups:
  - name: rulex-business
    rules:
      - alert: RulexHighBlockRate
        expr: |
          sum(rate(rulex_transaction_decisions_total{decision="BLOCK"}[1h])) 
          / sum(rate(rulex_transaction_decisions_total[1h])) > 0.1
        for: 30m
        labels:
          severity: warning
        annotations:
          summary: "Taxa de bloqueio alta"
          description: "{{ $value | humanizePercentage }} das transações sendo bloqueadas"

      - alert: RulexNoTransactions
        expr: |
          sum(rate(rulex_transaction_decisions_total[5m])) == 0
        for: 10m
        labels:
          severity: warning
        annotations:
          summary: "Nenhuma transação processada"
          description: "Nenhuma transação nos últimos 10 minutos"

      - alert: RulexWebhookDLQGrowing
        expr: |
          sum(rulex_webhook_dlq_pending) > 100
        for: 15m
        labels:
          severity: warning
        annotations:
          summary: "DLQ de webhooks crescendo"
          description: "{{ $value }} webhooks pendentes na DLQ"

      - alert: RulexFlywayMigrationFailed
        expr: |
          rulex_flyway_migrations_failed > 0
        for: 1m
        labels:
          severity: critical
        annotations:
          summary: "Migration Flyway falhou"
          description: "Há migrations falhadas - verificar imediatamente"
```

## Configuração Alertmanager

```yaml
# alertmanager.yml
global:
  resolve_timeout: 5m

route:
  group_by: ['alertname', 'severity']
  group_wait: 30s
  group_interval: 5m
  repeat_interval: 4h
  receiver: 'default'
  routes:
    - match:
        severity: critical
      receiver: 'pagerduty-critical'
      repeat_interval: 15m
    - match:
        severity: warning
      receiver: 'slack-warnings'

receivers:
  - name: 'default'
    email_configs:
      - to: 'devops@empresa.com'

  - name: 'pagerduty-critical'
    pagerduty_configs:
      - service_key: '${PAGERDUTY_SERVICE_KEY}'
        severity: critical

  - name: 'slack-warnings'
    slack_configs:
      - api_url: '${SLACK_WEBHOOK_URL}'
        channel: '#rulex-alerts'
        title: '{{ .GroupLabels.alertname }}'
        text: '{{ .Annotations.description }}'
```

## Runbooks

### RulexApiDown

1. Verificar logs: `kubectl logs -l app=rulex-backend`
2. Verificar recursos: `kubectl top pods -l app=rulex-backend`
3. Verificar dependências (PostgreSQL, Redis)
4. Reiniciar se necessário: `kubectl rollout restart deployment/rulex-backend`

### RulexHighLatencyP95

1. Verificar métricas de regras: `/actuator/metrics/rulex.rule.evaluation`
2. Verificar conexões de banco: `/actuator/health`
3. Verificar cache Redis: `/admin/redis/stats`
4. Considerar escalar horizontalmente

### PostgresConnectionPoolExhausted

1. Verificar queries lentas: `pg_stat_activity`
2. Verificar locks: `pg_locks`
3. Aumentar pool se necessário (HIKARI_MAX_POOL_SIZE)
4. Considerar PgBouncer para connection pooling

### Neo4jDown

1. Sistema continua funcionando (graceful degradation)
2. Operadores de grafo retornam valores conservadores
3. Verificar logs do Neo4j
4. Reconexão automática será tentada

## Dashboards Relacionados

- [RULEX Overview](../grafana/rulex-overview-dashboard.json)
- [RULEX Rules](../grafana/rulex-rules-dashboard.json)
- [RULEX Fraud](../grafana/rulex-fraud-dashboard.json)
