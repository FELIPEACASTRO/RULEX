# 🔧 Plano de Refatoração do Backend RULEX

## 📊 Status Atual

| Componente | Antes | Depois | Status |
|------------|-------|--------|--------|
| RuleEngineService | 2.344 linhas | ~200 linhas | 🟡 Em progresso |
| ComplexRuleEvaluator | 1.743 linhas | ~300 linhas | 🟡 Em progresso |
| Operadores (switch) | 278 cases | Strategy Pattern | 🟢 Implementado |
| Neo4j Resiliência | Sem Circuit Breaker | Com Resilience4j | 🟢 Implementado |
| Testes de Arquitetura | Básico | Completo | 🟢 Implementado |

## 🏗️ Arquitetura Refatorada

### Estrutura de Pacotes (Nova)

```
com.rulex.service.engine/
├── orchestrator/
│   └── TransactionAnalysisOrchestrator.java  # Coordena o fluxo
├── antitamper/
│   └── AntiTamperService.java                # Hash, idempotência
├── contract/
│   └── ContractValidationService.java        # Validação de entrada
├── decision/
│   └── DecisionPersistenceService.java       # Persistência
├── precheck/
│   └── PreCheckService.java                  # Bloom filter, etc.
├── response/
│   └── ResponseBuilderService.java           # Construção de respostas
├── operator/
│   └── strategy/
│       ├── OperatorStrategy.java             # Interface base
│       ├── OperatorStrategyRegistry.java     # Registry central
│       ├── ComparisonOperatorStrategy.java   # EQ, GT, LT, etc.
│       ├── StringOperatorStrategy.java       # CONTAINS, REGEX, etc.
│       ├── NullBooleanOperatorStrategy.java  # IS_NULL, IS_TRUE, etc.
│       └── ListOperatorStrategy.java         # IN, ARRAY_CONTAINS, etc.
└── ResilientNeo4jService.java                # Circuit Breaker Neo4j
```

### Fluxo de Processamento

```
┌─────────────────────────────────────────────────────────────────┐
│                TransactionAnalysisOrchestrator                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. AntiTamperService.checkPayload()                            │
│     └─ Verifica hash, idempotência, tamper                      │
│                                                                  │
│  2. ContractValidationService.validateRawPayload()              │
│     └─ Valida campos obrigatórios (sem 400)                     │
│                                                                  │
│  3. PreCheckService.runPreChecks()                              │
│     └─ Bloom filter, impossible travel, velocity                │
│                                                                  │
│  4. DecisionPersistenceService.persistTransaction()             │
│     └─ Salva transação no banco                                 │
│                                                                  │
│  5. RuleEvaluator.evaluate() [delegado]                         │
│     └─ Avalia regras configuradas                               │
│                                                                  │
│  6. DecisionPersistenceService.persistDecision()                │
│     └─ Salva decisão final                                      │
│                                                                  │
│  7. ResponseBuilderService.buildResponse()                      │
│     └─ Constrói resposta da API                                 │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## 🎯 Strategy Pattern para Operadores

### Antes (Switch Monster)

```java
// ComplexRuleEvaluator.java - 278 cases!
switch (operator) {
    case EQ -> evaluateEquals(...)
    case NEQ -> !evaluateEquals(...)
    case GT -> compareValues(...) > 0
    case CONTAINS -> evaluateContains(...)
    // ... 274 mais cases
}
```

### Depois (Strategy Pattern)

```java
// OperatorStrategyRegistry.java
public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
    OperatorStrategy strategy = operatorToStrategy.get(condition.getOperator());
    return strategy.evaluate(fieldValue, condition, context);
}

// ComparisonOperatorStrategy.java
@Component
public class ComparisonOperatorStrategy implements OperatorStrategy {
    @Override
    public Set<ConditionOperator> supportedOperators() {
        return Set.of(EQ, NEQ, GT, GTE, LT, LTE, BETWEEN, NOT_BETWEEN);
    }
    
    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext ctx) {
        // Implementação focada apenas em comparações
    }
}
```

### Benefícios

1. **Open/Closed Principle** - Adicionar operador = criar nova classe
2. **Single Responsibility** - Cada strategy cuida de operadores relacionados
3. **Testabilidade** - Cada strategy pode ser testada isoladamente
4. **Manutenibilidade** - Código organizado por domínio
5. **Auto-descoberta** - Spring injeta todas as strategies automaticamente

## 🛡️ Resiliência Neo4j

### Configuração Resilience4j

```yaml
resilience4j:
  circuitbreaker:
    instances:
      neo4j:
        slidingWindowSize: 10
        failureRateThreshold: 50
        waitDurationInOpenState: 30s
  retry:
    instances:
      neo4j:
        maxAttempts: 3
        waitDuration: 500ms
  timelimiter:
    instances:
      neo4j:
        timeoutDuration: 2s
```

### Uso

```java
@CircuitBreaker(name = "neo4j", fallbackMethod = "fallback")
@Retry(name = "neo4j")
@TimeLimiter(name = "neo4j")
public CompletableFuture<Integer> getWeaklyConnectedComponentId(String accountId) {
    return CompletableFuture.supplyAsync(() -> 
        neo4jGraphService.getWeaklyConnectedComponentId(accountId));
}

public CompletableFuture<Integer> fallback(String accountId, Throwable t) {
    log.warn("Neo4j fallback for {}: {}", accountId, t.getMessage());
    return CompletableFuture.completedFuture(-1);
}
```

## 📋 Testes de Arquitetura

### Regras Implementadas

| Regra | Descrição | Status |
|-------|-----------|--------|
| Services não dependem de Controllers | Camadas separadas | ✅ Ativo |
| Repositories não dependem de Services | Camadas separadas | ✅ Ativo |
| DTOs não dependem de Entities | Separação de concerns | ✅ Ativo |
| Max 7 dependências (engine) | Evitar God Classes | ✅ Ativo |
| Max 500 linhas (engine) | Código legível | ✅ Ativo |
| Max 300 linhas (strategies) | Código focado | ✅ Ativo |
| Max 500 linhas (todos) | Meta futura | 🟡 Disabled |
| Max 20 métodos (todos) | Meta futura | 🟡 Disabled |

## 🚀 Próximos Passos

### Fase 2: Migração Completa

1. [ ] Migrar RuleEngineService para usar TransactionAnalysisOrchestrator
2. [ ] Migrar ComplexRuleEvaluator para usar OperatorStrategyRegistry
3. [ ] Criar strategies para todos os 394 operadores
4. [ ] Remover código duplicado

### Fase 3: Consolidação

1. [ ] Consolidar migrations Flyway (V50 baseline)
2. [ ] Adicionar mais testes de integração
3. [ ] Performance testing
4. [ ] Documentação de operadores

## 📊 Métricas de Sucesso

| Métrica | Atual | Meta Sprint 2 | Meta Sprint 4 |
|---------|-------|---------------|---------------|
| Linhas RuleEngineService | 2.344 | 800 | 200 |
| Linhas ComplexRuleEvaluator | 1.743 | 600 | 300 |
| Cobertura de testes | ~50% | 70% | 85% |
| Operadores com Strategy | 0 | 50 | 394 |
| Testes de arquitetura | 1 | 10 | 20 |

---

**Última atualização:** 2024-01-22
**Autor:** Refactoring Team
