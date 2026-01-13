# ADR-0001: Adoção de Clean Architecture para o Engine de Regras

## Status
**Proposto** — 2026-01-05

## Contexto

O RULEX é um motor de regras duras (hard rules) para detecção de fraude em transações de crédito. O sistema atual apresenta os seguintes problemas arquiteturais identificados com evidência:

### Problemas Evidenciados

1. **Classe "Deus" — RuleEngineService**
   - Arquivo: `backend/src/main/java/com/rulex/service/RuleEngineService.java`
   - Linhas: 2205
   - Responsabilidades misturadas: avaliação, persistência, métricas, logging, enriquecimento

2. **Duplicação de Lógica de Operadores**
   - `RuleEngineService.evaluateCondition()` — linhas 1677-1850
   - `AstEvaluator.evalCondition()` — linhas 60-110
   - Frontend: `client/src/components/RuleFormDialog/schema.ts` — linhas 14-40

3. **Arquitetura Híbrida Incompleta**
   - Pacote `homolog/` implementa Clean Architecture (ports/adapters/usecases)
   - Pacote `service/` usa arquitetura tradicional Spring

### Versões em Uso (Evidência: pom.xml, package.json)
- Java 21 (LTS)
- Spring Boot 3.5.9
- React 19.2.1
- TypeScript 5.9.3

## Decisão

Adotar **Clean Architecture / Hexagonal** de forma **pragmática e incremental**, focando primeiro no core do engine de regras.

### Estrutura de Pacotes Proposta

```
backend/src/main/java/com/rulex/
├── domain/                    # Camada de Domínio (sem dependências externas)
│   ├── rule/                  # Agregado de Regras
│   │   ├── Rule.java          # Entidade raiz
│   │   ├── RuleCondition.java # Value Object
│   │   ├── RuleVersion.java   # Entidade
│   │   └── ConditionGroup.java
│   ├── operator/              # Operadores (Strategy Pattern)
│   │   ├── Operator.java      # Interface
│   │   ├── ComparisonOperator.java
│   │   ├── StringOperator.java
│   │   ├── ListOperator.java
│   │   ├── DateOperator.java
│   │   └── VelocityOperator.java
│   ├── evaluation/            # Engine de Avaliação
│   │   ├── RuleEvaluator.java # Interface
│   │   ├── ConditionEvaluator.java
│   │   └── EvaluationResult.java
│   └── exception/             # Exceções de Domínio
│       ├── RuleValidationException.java
│       └── EvaluationException.java
│
├── application/               # Camada de Aplicação (Use Cases)
│   ├── port/                  # Ports (Interfaces)
│   │   ├── in/                # Ports de Entrada
│   │   │   ├── EvaluateTransactionUseCase.java
│   │   │   ├── CreateRuleUseCase.java
│   │   │   └── SimulateRuleUseCase.java
│   │   └── out/               # Ports de Saída
│   │       ├── RulePersistencePort.java
│   │       ├── TransactionPersistencePort.java
│   │       ├── VelocityCachePort.java
│   │       └── MetricsPort.java
│   └── service/               # Implementação dos Use Cases
│       ├── EvaluateTransactionService.java
│       ├── CreateRuleService.java
│       └── SimulateRuleService.java
│
├── infrastructure/            # Camada de Infraestrutura (Adapters)
│   ├── persistence/           # Adapters JPA
│   │   ├── RuleJpaAdapter.java
│   │   ├── TransactionJpaAdapter.java
│   │   └── entity/            # Entidades JPA (separadas do domínio)
│   ├── cache/                 # Adapters Redis
│   │   └── RedisVelocityAdapter.java
│   ├── metrics/               # Adapters Métricas
│   │   └── MicrometerMetricsAdapter.java
│   └── config/                # Configurações Spring
│
└── interfaces/                # Camada de Interface (Web/API)
    ├── rest/                  # Controllers REST
    │   ├── EvaluateController.java
    │   ├── RuleController.java
    │   └── dto/               # DTOs de Request/Response
    └── handler/               # Exception Handlers
        └── GlobalExceptionHandler.java
```

### Regras de Dependência

```
┌─────────────────────────────────────────────────────────────┐
│                        interfaces/                          │
│                    (Controllers, DTOs)                      │
└─────────────────────────────┬───────────────────────────────┘
                              │ depende de
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                       application/                          │
│                  (Use Cases, Ports)                         │
└─────────────────────────────┬───────────────────────────────┘
                              │ depende de
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                         domain/                             │
│        (Entidades, Value Objects, Operadores)               │
│           SEM Spring, JPA, Redis, Jackson                   │
└─────────────────────────────────────────────────────────────┘
                              ▲
                              │ implementa ports
┌─────────────────────────────┴───────────────────────────────┐
│                      infrastructure/                        │
│              (JPA, Redis, Metrics Adapters)                 │
└─────────────────────────────────────────────────────────────┘
```

### Strategy Pattern para Operadores

```java
// domain/operator/Operator.java
public sealed interface Operator permits
    ComparisonOperator, StringOperator, ListOperator, DateOperator, VelocityOperator {
    
    String name();
    boolean evaluate(Object left, Object right);
    boolean isUnary();
}

// domain/operator/ComparisonOperator.java
public enum ComparisonOperator implements Operator {
    EQ("EQ", (l, r) -> Objects.equals(l, r)),
    NE("NE", (l, r) -> !Objects.equals(l, r)),
    GT("GT", (l, r) -> compare(l, r) > 0),
    GTE("GTE", (l, r) -> compare(l, r) >= 0),
    LT("LT", (l, r) -> compare(l, r) < 0),
    LTE("LTE", (l, r) -> compare(l, r) <= 0);
    
    // ... implementação
}
```

## Consequências

### Positivas
- **Testabilidade**: Domínio pode ser testado sem Spring/JPA
- **Extensibilidade**: Novos operadores sem alterar switch/case gigante
- **Manutenibilidade**: Responsabilidades bem definidas
- **Consistência**: Uma única implementação de operadores

### Negativas
- **Migração**: Requer refatoração incremental
- **Curva de Aprendizado**: Equipe precisa entender a arquitetura
- **Overhead Inicial**: Mais interfaces e classes

### Riscos
- Quebra de compatibilidade durante migração
- Regressões se testes não forem suficientes

## Plano de Migração

### Fase 1: Operadores (P0)
1. Criar `domain/operator/` com Strategy pattern
2. Migrar operadores de `RuleEngineService.evaluateCondition()`
3. Criar testes unitários para cada operador
4. Substituir switch/case por lookup em Map

### Fase 2: Avaliação (P0)
1. Criar `domain/evaluation/ConditionEvaluator`
2. Extrair lógica de `evaluateCondition()` para domínio
3. Criar testes de integração

### Fase 3: Use Cases (P1)
1. Criar ports em `application/port/`
2. Migrar `RuleEngineService.evaluate()` para `EvaluateTransactionService`
3. Criar adapters em `infrastructure/`

### Fase 4: Consolidação (P2)
1. Remover código legado de `RuleEngineService`
2. Unificar com estrutura `homolog/`
3. Atualizar documentação

## Validação

Cada fase deve passar:
```bash
cd ~/repos/RULEX && mvn -f backend/pom.xml test
cd ~/repos/RULEX && pnpm test
cd ~/repos/RULEX && pnpm e2e
```

## Referências

- Clean Architecture (Robert C. Martin)
- Hexagonal Architecture (Alistair Cockburn)
- Código existente em `homolog/` como referência
