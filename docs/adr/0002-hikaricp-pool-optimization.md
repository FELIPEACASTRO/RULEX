# ADR-0002: HikariCP Connection Pool Optimization

**Data:** 2026-01-05  
**Status:** Aceita  
**Autor:** GitHub Copilot  

## Contexto

### Problema Identificado

O baseline de performance (docs/perf.md) mostrou:

| VUs | TPS  | p95 (ms) | Erros |
|-----|------|----------|-------|
| 10  | 84   | 180      | 0%    |
| 20  | 72   | 450      | 53%   |
| 50  | 35   | >1000    | 100%  |

**Hipótese forte (com evidência no baseline):** exaustão de pool de conexões do HikariCP em carga.

### Evidência (baseline)

```
ERROR: Connection is not available, request timed out after 30000ms
at com.zaxxer.hikari.pool.HikariPool.getConnection()
```

Mensagens de erro indicam esgotamento do pool (timeout padrão de 30s no baseline).

## Decisão

### Mudanças Aplicadas

| Parâmetro | Antes | Depois | Motivo |
|-----------|-------|--------|--------|
| `minimum-idle` | 5 | 20 | Manter conexões prontas para picos |
| `maximum-pool-size` | 20 | 100 | Suportar 100+ VUs sem saturação |
| `connection-timeout` | 30000ms | 5000ms | Fail-fast em vez de aguardar |

### Arquivo Modificado

- `backend/src/main/resources/application.yml` (linhas 26-36)

### Configuração Resultante

```yaml
hikari:
  pool-name: RulexHikariPool
  minimum-idle: ${HIKARI_MIN_IDLE:20}
  maximum-pool-size: ${HIKARI_MAX_POOL_SIZE:100}
  idle-timeout: ${HIKARI_IDLE_TIMEOUT:300000}
  max-lifetime: ${HIKARI_MAX_LIFETIME:1800000}
  connection-timeout: ${HIKARI_CONNECTION_TIMEOUT:5000}
  leak-detection-threshold: ${HIKARI_LEAK_DETECTION:60000}
```

## Consequências

### Positivas

1. **Capacidade aumentada:** Pool de 100 conexões permite até ~100 VUs simultâneos
2. **Fail-fast:** Timeout de 5s permite resposta rápida em vez de bloquear 30s
3. **Warm pool:** 20 conexões mínimas mantém pool aquecido para picos

### Negativas

1. **Maior uso de memória:** Cada conexão PostgreSQL usa ~2-5MB
2. **Custo no DB:** 100 conexões = 100 slots no PostgreSQL (default max_connections=100)

### Riscos Mitigados

| Risco | Mitigação |
|-------|-----------|
| PostgreSQL max_connections < 100 | Verificar com `SHOW max_connections;` |
| Memory pressure | Monitorar via Actuator/Prometheus |
| Connection leak | leak-detection-threshold mantido em 60s |

## Validação Esperada

```bash
# Re-executar k6 após aplicar patch
k6 run --vus 50 --duration 60s perf/load-test.js
```

**Meta:**
- p95 < 200ms
- p99 < 500ms
- taxa de erro < 1%

## Referências

- [HikariCP Configuration](https://github.com/brettwooldridge/HikariCP#configuration-knobs-baby)
- [docs/perf.md](../perf.md) - Baseline de performance
- [PostgreSQL max_connections](https://www.postgresql.org/docs/current/runtime-config-connection.html)

## Changelog

| Data | Versão | Descrição |
|------|--------|-----------|
| 2026-01-05 | 1.0 | Patch P0 aplicado: pool 20→100, timeout 30s→5s |
