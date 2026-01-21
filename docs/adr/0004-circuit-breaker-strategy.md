# ADR-0004: Circuit Breaker Strategy

## Status
Aceito

## Data
2025-01-15

## Contexto

O RULEX depende de serviços externos (Neo4j, Redis) que podem falhar ou ficar lentos. Sem proteção adequada:

- Falhas em Neo4j causavam timeout em todas as avaliações
- Falhas em Redis degradavam performance de velocity checks
- Não havia recuperação automática após falhas
- Logs eram poluídos com erros repetitivos

## Decisão

Implementar **Circuit Breakers** usando Resilience4j para todas as dependências externas.

### Configuração

```yaml
resilience4j:
  circuitbreaker:
    instances:
      neo4j:
        slidingWindowSize: 10
        failureRateThreshold: 50
        waitDurationInOpenState: 30s
        permittedNumberOfCallsInHalfOpenState: 3
      redis:
        slidingWindowSize: 10
        failureRateThreshold: 50
        waitDurationInOpenState: 10s
```

### Estados

1. **CLOSED**: Operação normal, todas as chamadas passam
2. **OPEN**: Circuito aberto, chamadas falham imediatamente com fallback
3. **HALF_OPEN**: Testando recuperação, permite algumas chamadas

### Fallbacks

| Serviço | Fallback |
|---------|----------|
| Neo4j (fraud ring) | Retorna 0 (assume sem fraud ring) |
| Neo4j (network analysis) | Retorna vazio |
| Redis (velocity) | Usa cache local Caffeine |
| Redis (distinct count) | Retorna 0 |

### Health Indicators

Cada serviço tem um `HealthIndicator` customizado:
- `Neo4jHealthIndicator`: Verifica conexão e latência
- `RedisHealthIndicator`: Executa PING e mede latência

## Consequências

### Positivas

- Sistema continua funcionando com dependências degradadas
- Recuperação automática quando serviço volta
- Métricas de saúde expostas via Actuator
- Logs limpos (não repete erros durante OPEN state)
- Proteção contra cascata de falhas

### Negativas

- Algumas análises de fraude podem ser menos precisas durante degradação
- Complexidade adicional de configuração
- Necessidade de definir fallbacks sensatos

### Trade-offs

- **Neo4j offline**: Análises de fraud ring não funcionam, mas transações não são bloqueadas
- **Redis offline**: Velocity checks usam cache local (menos preciso, mas funcional)

## Métricas

Expostas via Actuator:
- `resilience4j.circuitbreaker.state`: Estado atual (CLOSED/OPEN/HALF_OPEN)
- `resilience4j.circuitbreaker.failure.rate`: Taxa de falha
- `resilience4j.circuitbreaker.calls`: Contagem de chamadas

## Referências

- [Resilience4j Documentation](https://resilience4j.readme.io/)
- [Circuit Breaker Pattern - Martin Fowler](https://martinfowler.com/bliki/CircuitBreaker.html)
