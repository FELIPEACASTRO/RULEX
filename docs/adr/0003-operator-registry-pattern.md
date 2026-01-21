# ADR-0003: Operator Registry Pattern

## Status
Aceito

## Data
2025-01-15

## Contexto

O `ComplexRuleEvaluator` tinha 8.646 linhas de código com 423 métodos de avaliação de operadores, violando gravemente o Single Responsibility Principle (SRP). Isso causava:

- Dificuldade extrema de manutenção
- Impossibilidade de testar operadores isoladamente
- Tempo de compilação elevado
- Conflitos frequentes em merges
- Dificuldade para adicionar novos operadores

Além disso, 155 operadores estavam declarados mas não implementados (retornavam `false` silenciosamente), criando falsa sensação de capacidade.

## Decisão

Implementar o padrão **Strategy** com um **Registry** para operadores:

### Arquitetura

```
OperatorEvaluator (interface)
    ├── BasicComparisonEvaluator (EQ, NEQ, GT, LT, IN, BETWEEN, etc.)
    ├── StringOperatorEvaluator (CONTAINS, REGEX, LIKE, etc.)
    ├── DateTimeOperatorEvaluator (DATE_BEFORE, TIME_BETWEEN, etc.)
    ├── ArrayMathOperatorEvaluator (ARRAY_CONTAINS, MOD_EQ, etc.)
    ├── VelocityOperatorEvaluator (VELOCITY_*, COUNT_*, SUM_*)
    ├── GeoOperatorEvaluator (GEO_DISTANCE, GEO_IN_POLYGON, etc.)
    ├── GraphOperatorEvaluator (NEO4J_*, FRAUD_RING_*)
    └── StubOperatorEvaluator (operadores PLANNED - lança exceção)

OperatorEvaluatorRegistry
    └── Mapeia ConditionOperator → OperatorEvaluator
```

### Regras

1. Cada `OperatorEvaluator` é responsável por um conjunto coeso de operadores
2. O `OperatorEvaluatorRegistry` é o ponto único de acesso
3. Operadores não implementados devem estar no `StubOperatorEvaluator`
4. `StubOperatorEvaluator` SEMPRE lança `UnsupportedOperatorException`
5. O endpoint `/api/operators/status` expõe o status de cada operador

### Exceções

- `UnsupportedOperatorException`: Lançada quando operador PLANNED é usado
- Retorna HTTP 501 (Not Implemented) com detalhes do operador

## Consequências

### Positivas

- `ComplexRuleEvaluator` reduzido de 8.646 para <500 linhas
- Cada avaliador pode ser testado isoladamente
- Fácil adicionar novos operadores (criar classe, registrar)
- Operadores não implementados falham explicitamente (não silenciosamente)
- Documentação automática via `/api/operators/status`
- Melhor separação de responsabilidades

### Negativas

- Mais arquivos para gerenciar (~12 avaliadores)
- Indireção adicional na avaliação (registry lookup)
- Necessidade de manter `StubOperatorEvaluator` atualizado

### Neutras

- Performance: O overhead do registry lookup é negligenciável (<1μs)

## Alternativas Consideradas

1. **Manter monolítico**: Rejeitado por violar SRP e ser impossível de manter
2. **Usar reflection**: Rejeitado por ser frágil e difícil de debugar
3. **Code generation**: Rejeitado por adicionar complexidade de build

## Referências

- [Strategy Pattern - GoF](https://refactoring.guru/design-patterns/strategy)
- [Registry Pattern](https://martinfowler.com/eaaCatalog/registry.html)
