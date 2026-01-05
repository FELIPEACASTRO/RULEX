# Análise de Bottlenecks — RULEX Performance

**Data:** 2026-01-05  
**Branch:** refactor/hexagonal-arch  
**Baseline:** 46 TPS, p95=4.99s (target: 1000 TPS, p95<200ms)

## 🔴 TOP 3 BOTTLENECKS IDENTIFICADOS (Por Análise de Código)

### #1 — AUDITORIA SÍNCRONA (P0 — CRÍTICO)

**EVIDÊNCIA:**
- Arquivo: `backend/src/main/java/com/rulex/service/AuditService.java`
- Método: `logTransactionProcessed()` (linhas 32-61)
- Código:
```java
@Service
@Transactional // <-- Bloqueia até DB commit
public class AuditService {
  public void logTransactionProcessed(...) {
    // ... preparação do log ...
    auditLogRepository.save(auditLog); // <-- INSERT síncrono
    log.info("Auditoria registrada...");
  }
}
```

**PROBLEMA:**
- Cada request aguarda INSERT em `audit_log` (50-100ms+)
- Tabela de audit cresce rapidamente (sem particionamento)
- Possível contenção em índices/locks

**IMPACTO ESTIMADO:** ~50-150ms por request (10-30% da latência)

**SOLUÇÃO:**
- Tornar auditoria **assíncrona** com `@Async`
- Usar queue/buffer em memória com flush periódico (batch)
- Alternativa: escrever em tópico Kafka/Redis Stream

**RISCO:**
- Perda de audit logs em crash (solução: usar queue durável)
- Complexidade de troubleshooting (correlação temporal)

**VALIDAÇÃO:**
```bash
# Antes
k6 run --vus 50 --duration 30s perf/load-test.js  # p95 ~5s

# Depois (async audit)
k6 run --vus 50 --duration 30s perf/load-test.js  # p95 esperado: 3-4s (melhoria de 20-30%)
```

---

### #2 — MÚLTIPLOS INSERTS SÍNCRONOS POR REQUEST (P0 — CRÍTICO)

**EVIDÊNCIA:**
- Arquivo: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
- Linhas: 169, 195 (e duplicado em 299, 325)
- Código:
```java
transaction = transactionRepository.save(transaction);        // INSERT #1
decisionRepository.save(decision);                            // INSERT #2
auditService.logTransactionProcessed(transaction, ...);       // INSERT #3 (audit_log)
rawStoreService.store(externalTransactionId, ...);           // INSERT #4 (raw_store)
```

**PROBLEMA:**
- **4 INSERTs síncronos por request!**
- Cada INSERT = round-trip ao DB (~10-50ms cada)
- Total: 40-200ms+ só em writes

**IMPACTO ESTIMADO:** ~40-200ms por request (20-40% da latência)

**SOLUÇÃO:**
1. **Batch writes** — acumular em memória e flush periódico
2. **Async writes** — usar `@Async` para writes não-críticos
3. **Reduzir writes** — `raw_store` pode ser opcional (já temos `payload_hash`)

**RISCO:**
- Batch: complexidade de failure handling
- Async: eventual consistency, troubleshooting
- Remover raw_store: perda de auditabilidade granular

**VALIDAÇÃO:**
```bash
# Após implementação
k6 run --vus 100 --duration 60s perf/load-test.js  # TPS esperado: 100-200 (2-4x melhoria)
```

---

### #3 — POSSÍVEL N+1 QUERIES (P1 — INVESTIGAR)

**EVIDÊNCIA:**
- Não confirmado ainda (precisa profiling com SQL logging)
- Suspeitas:
  - Carregamento de `RuleConfiguration` + condições (lazy loading)
  - Queries de `VelocityService` (por PAN/merchant)
  - Lookups em `BloomFilterService`

**PROBLEMA POTENCIAL:**
- Se regras são carregadas com LAZY, cada acesso a `conditions` = 1 query
- Se 10 regras aplicam, pode gerar 10+ queries extras

**IMPACTO ESTIMADO:** ~50-100ms (se confirmado)

**SOLUÇÃO:**
- Eager fetch de `RuleConfiguration.conditions` (JOIN FETCH)
- Cache de regras em memória (evitar DB hit por request)
- Usar `@EntityGraph` ou JPQL com JOIN FETCH

**VALIDAÇÃO:**
```bash
# Habilitar SQL logging
SPRING_PROFILES_ACTIVE=profiling mvn spring-boot:run

# Executar teste
k6 run --vus 1 --iterations 10 perf/profiling-test.js

# Analisar logs: contar número de queries por request
grep "Hibernate: select" logs.txt | wc -l
```

---

## 🟡 OUTROS BOTTLENECKS SUSPEITOS (P2)

### #4 — HikariCP Pool (Já otimizado — ADR-0002)
- Pool configurado: min=20, max=100
- Pode ainda não ser suficiente para 1000 TPS
- **Ação:** Monitorar métricas JMX durante load test

### #5 — Redis Latency (Velocity Checks)
- Cada velocity check = round-trip Redis (~1-5ms)
- Se múltiplos checks por request, soma
- **Ação:** Pipeline Redis commands, ou cache local

### #6 — Serialização JSON
- `objectMapper.writeValueAsString(details)` em audit (linha 52)
- **Ação:** Lazy serialization ou formato binário

### #7 — Locks/Contenção em Tabelas
- Tabela `audit_log` sem particionamento
- Índices podem causar contenção em writes
- **Ação:** Adicionar particionamento temporal, ou mover audit para separado DB

---

## 📊 PLANO DE OTIMIZAÇÃO (PRIORIZADO)

### FASE 4.2 — Quick Win #1: Async Audit (P0)
**Objetivo:** Reduzir p95 de 5s para ~3s (40% melhoria)

1. Adicionar `@EnableAsync` em config
2. Anotar `AuditService` methods com `@Async`
3. Configurar thread pool dedicado
4. Re-testar com k6

**Estimativa:** 2 horas dev + test

---

### FASE 4.3 — Quick Win #2: Batch Writes (P0)
**Objetivo:** Reduzir 4 INSERTs síncronos para 1 batch async

1. Criar `BatchWriteService` com queue em memória
2. Flush periódico (a cada 100 registros ou 1s)
3. Migrar `transaction`, `decision`, `audit_log` para batch
4. Re-testar com k6

**Estimativa:** 4 horas dev + test

---

### FASE 4.4 — Quick Win #3: Cache de Regras (P1)
**Objetivo:** Evitar query de regras por request

1. Implementar `@Cacheable` em `RuleConfigurationRepository.findByEnabled()`
2. Invalidar cache em updates de regras
3. Re-testar com k6

**Estimativa:** 2 horas dev + test

---

### FASE 4.5 — Deep Dive: Profiling com SQL Logging (P1)
**Objetivo:** Confirmar/descartar N+1 queries

1. Executar backend com profile `profiling`
2. Analisar logs SQL gerados
3. Identificar queries duplicadas/lentas
4. Aplicar fix (EAGER fetch, JOIN FETCH, índices)

**Estimativa:** 3 horas análise + fix

---

### FASE 4.6 — Validação Final: Load Test (P0)
**Objetivo:** Validar SLO atingido

1. Executar k6 com 100 VUs / 60s
2. Verificar: TPS >= 1000, p95 < 200ms
3. Documentar em `docs/perf-improvements.md`

---

## 🎯 META DE MELHORIA

| Métrica | Baseline | Meta Intermediária | Meta Final (SLO) |
|---------|----------|--------------------|--------------------|
| TPS | 46 | 200+ (4x) | 1000 (22x) |
| p95 | 4.99s | 1s (5x melhor) | <200ms (25x melhor) |
| p99 | 5.0s | 2s | <200ms |

**Estratégia:** Melhorias incrementais com validação a cada etapa.

---

## REFERÊNCIAS

- Baseline: [docs/perf-baseline.md](perf-baseline.md)
- ADR HikariCP: [docs/adr/0002-hikaricp-pool-optimization.md](adr/0002-hikaricp-pool-optimization.md)
- Script k6: [perf/load-test.js](../perf/load-test.js)
