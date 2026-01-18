# Guia de Contribuição - RULEX

## Índice

1. [Configuração do Ambiente](#configuração-do-ambiente)
2. [Adicionando um Novo Operador](#adicionando-um-novo-operador)
3. [Padrões de Código](#padrões-de-código)
4. [Testes](#testes)
5. [Pull Requests](#pull-requests)

---

## Configuração do Ambiente

### Pré-requisitos

- Java 21 (LTS)
- Node.js 22+
- pnpm 10+
- Docker e Docker Compose
- Maven 3.9+

### Setup Inicial

```bash
# Clone o repositório
git clone https://github.com/seu-org/RULEX.git
cd RULEX

# Inicie os serviços
docker compose up -d postgres redis neo4j

# Backend
cd backend
mvn clean install -DskipTests
mvn spring-boot:run

# Frontend (em outro terminal)
cd ..
pnpm install
pnpm dev
```

---

## Adicionando um Novo Operador

### 1. Declarar no Enum

Adicione o operador em `RuleCondition.ConditionOperator`:

```java
// backend/src/main/java/com/rulex/entity/complex/RuleCondition.java

public enum ConditionOperator {
    // ... operadores existentes ...
    
    // Categoria: VELOCITY (adicione na categoria correta)
    VELOCITY_NEW_OPERATOR,
}
```

### 2. Escolher o Evaluator Correto

| Categoria | Evaluator | Exemplos |
|-----------|-----------|----------|
| Comparação básica | `BasicComparisonEvaluator` | EQ, GT, IN, BETWEEN |
| Strings | `StringOperatorEvaluator` | CONTAINS, REGEX, LIKE |
| Data/Hora | `DateTimeOperatorEvaluator` | DATE_BEFORE, IS_WEEKEND |
| Arrays/Math | `ArrayMathOperatorEvaluator` | ARRAY_CONTAINS, MOD_EQ |
| Velocity | `VelocityOperatorEvaluator` | VELOCITY_*, COUNT_* |
| Geo | `GeoOperatorEvaluator` | GEO_DISTANCE, GEO_IN_POLYGON |
| Graph | `GraphOperatorEvaluator` | NEO4J_*, FRAUD_RING_* |

### 3. Implementar no Evaluator

```java
// backend/src/main/java/com/rulex/service/complex/evaluator/VelocityOperatorEvaluator.java

@Component
public class VelocityOperatorEvaluator implements OperatorEvaluator {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        // ... operadores existentes ...
        ConditionOperator.VELOCITY_NEW_OPERATOR  // Adicione aqui
    );

    @Override
    public boolean evaluate(RuleCondition condition, EvaluationContext context) {
        return switch (condition.getOperator()) {
            // ... cases existentes ...
            case VELOCITY_NEW_OPERATOR -> evaluateNewOperator(condition, context);
            default -> false;
        };
    }

    private boolean evaluateNewOperator(RuleCondition condition, EvaluationContext context) {
        // Sua implementação aqui
        String fieldName = condition.getFieldName();
        Object fieldValue = getFieldValue(context, fieldName);
        
        if (fieldValue == null) {
            return false;
        }
        
        // Lógica do operador
        return true;
    }
}
```

### 4. Remover do StubOperatorEvaluator (se estava lá)

Se o operador estava no `StubOperatorEvaluator`, remova-o:

```java
// backend/src/main/java/com/rulex/service/complex/evaluator/StubOperatorEvaluator.java

private static final Set<ConditionOperator> PLANNED_OPERATORS = Set.of(
    // REMOVA o operador daqui quando implementar
    // ConditionOperator.VELOCITY_NEW_OPERATOR,  // REMOVIDO
);
```

### 5. Criar Testes

```java
// backend/src/test/java/com/rulex/service/complex/evaluator/VelocityOperatorEvaluatorTest.java

@Test
@DisplayName("VELOCITY_NEW_OPERATOR deve retornar true quando condição satisfeita")
void velocityNewOperator_shouldReturnTrue_whenConditionMet() {
    RuleCondition condition = createCondition("transactionCount", 
        ConditionOperator.VELOCITY_NEW_OPERATOR, "10");
    EvaluationContext context = createContext(Map.of("transactionCount", 15));
    
    boolean result = evaluator.evaluate(condition, context);
    
    assertThat(result).isTrue();
}

@Test
@DisplayName("VELOCITY_NEW_OPERATOR deve retornar false quando campo é null")
void velocityNewOperator_shouldReturnFalse_whenFieldIsNull() {
    RuleCondition condition = createCondition("transactionCount", 
        ConditionOperator.VELOCITY_NEW_OPERATOR, "10");
    EvaluationContext context = createContext(Map.of());
    
    boolean result = evaluator.evaluate(condition, context);
    
    assertThat(result).isFalse();
}
```

### 6. Atualizar Frontend (opcional)

Se o operador deve aparecer no UI:

```typescript
// client/src/lib/operators.ts

export const OPERATORS: OperatorDefinition[] = [
    // ... operadores existentes ...
    { 
        value: 'VELOCITY_NEW_OPERATOR', 
        label: 'New Velocity Operator', 
        description: 'Descrição do operador',
        requiresValue: true, 
        category: 'Velocity' 
    },
];
```

### 7. Documentar

Atualize `openapi/rulex.yaml` se necessário.

---

## Padrões de Código

### Java

- **Máximo 500 linhas por classe**
- **Sem `catch (Exception e)` genérico** - use exceções específicas
- **Usar Lombok** para reduzir boilerplate
- **Documentar métodos públicos** com Javadoc
- **Seguir Google Java Style Guide**

```java
// ✅ BOM
try {
    neo4jService.query(cypher);
} catch (ServiceUnavailableException e) {
    throw new Neo4jConnectionException("Falha na query", e);
}

// ❌ RUIM
try {
    neo4jService.query(cypher);
} catch (Exception e) {
    log.error("Erro", e);
    return null;
}
```

### TypeScript

- **Usar TypeScript strict mode**
- **Preferir interfaces a types** para objetos
- **Documentar props de componentes**
- **Usar React Query** para data fetching

### Commits

Seguir [Conventional Commits](https://www.conventionalcommits.org/):

```
feat(operators): add VELOCITY_NEW_OPERATOR
fix(evaluator): handle null values in BasicComparisonEvaluator
docs(adr): add ADR-006 for caching strategy
test(velocity): add tests for new operator
```

---

## Testes

### Cobertura Mínima

| Camada | Linhas | Branches |
|--------|--------|----------|
| Backend | 80% | 70% |
| Frontend | 50% | 40% |

### Executar Testes

```bash
# Backend
cd backend
mvn test                    # Testes unitários
mvn test -Pcoverage         # Com cobertura
mvn verify -Pmutation       # Mutation testing

# Frontend
pnpm test                   # Testes unitários
pnpm test:coverage          # Com cobertura
pnpm e2e                    # Testes E2E
```

### Estrutura de Testes

```
backend/src/test/java/com/rulex/
├── service/
│   └── complex/
│       └── evaluator/
│           ├── BasicComparisonEvaluatorTest.java
│           ├── StringOperatorEvaluatorTest.java
│           └── OperatorEvaluatorRegistryTest.java
├── controller/
│   └── OperatorStatusControllerTest.java
└── integration/
    └── EvaluateIntegrationTest.java
```

---

## Pull Requests

### Checklist

- [ ] Testes passando (`mvn test` e `pnpm test`)
- [ ] Cobertura mantida ou aumentada
- [ ] Sem warnings de compilação
- [ ] Documentação atualizada (se necessário)
- [ ] ADR criado (para mudanças arquiteturais)
- [ ] Changelog atualizado

### Template

```markdown
## Descrição
Breve descrição da mudança.

## Tipo de Mudança
- [ ] Bug fix
- [ ] Nova feature
- [ ] Breaking change
- [ ] Documentação

## Como Testar
1. Passo 1
2. Passo 2

## Checklist
- [ ] Testes adicionados
- [ ] Documentação atualizada
```

---

## Dúvidas?

- Abra uma issue
- Consulte os ADRs em `docs/adr/`
- Verifique o status dos operadores em `/api/operators/status`
