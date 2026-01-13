# DSL Avançada: Operadores de Agregação Temporal

**Versão:** 2.0
**Data:** 2026-01-06
**Autor:** Manus AI

---

## Visão Geral

Esta documentação descreve os **7 novos operadores de agregação temporal** adicionados à DSL do RULEX, permitindo que regras de fraude avaliem o comportamento histórico de transações sem a necessidade de código hardcoded.

Esses operadores são essenciais para detectar padrões de fraude sofisticados, como:
- **Card Testing**: Múltiplas transações pequenas em merchants distintos
- **Velocity Abuse**: Aumento súbito no volume ou frequência de transações
- **Geographic Anomalies**: Transações em múltiplos países em curto período

---

## Operadores Disponíveis

### 1. `SUM_LAST_N_DAYS`

**Descrição:** Calcula a soma de um campo numérico (ex: `amount`) nos últimos N dias e compara com um threshold.

**Formato do `valueSingle`:**
```
fieldName|nDays|threshold|operator
```

**Exemplo:**
```
amount|7|5000|GT
```
**Significado:** A soma de `amount` nos últimos 7 dias é maior que 5000.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar clientes que excedem um limite de gasto semanal.

---

### 2. `COUNT_LAST_N_HOURS`

**Descrição:** Conta o número de transações nas últimas N horas e compara com um threshold.

**Formato do `valueSingle`:**
```
nHours|threshold|operator
```

**Exemplo:**
```
24|100|GT
```
**Significado:** Mais de 100 transações nas últimas 24 horas.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar ataques de força bruta ou card testing.

---

### 3. `AVG_LAST_N_DAYS`

**Descrição:** Calcula a média de um campo numérico nos últimos N dias e compara com um threshold.

**Formato do `valueSingle`:**
```
fieldName|nDays|threshold|operator
```

**Exemplo:**
```
amount|30|500|GT
```
**Significado:** A média de `amount` nos últimos 30 dias é maior que 500.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar mudanças no padrão de gasto do cliente.

---

### 4. `COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS`

**Descrição:** Conta o número de merchants distintos nos últimos N dias e compara com um threshold.

**Formato do `valueSingle`:**
```
nDays|threshold|operator
```

**Exemplo:**
```
7|10|GT
```
**Significado:** Mais de 10 merchants distintos nos últimos 7 dias.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar card testing (fraudadores testam cartões em múltiplos merchants).

---

### 5. `COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS`

**Descrição:** Conta o número de países distintos nas últimas N horas e compara com um threshold.

**Formato do `valueSingle`:**
```
nHours|threshold|operator
```

**Exemplo:**
```
24|5|GT
```
**Significado:** Mais de 5 países distintos nas últimas 24 horas.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar impossibilidade geográfica (transações em múltiplos países em curto período).

---

### 6. `MAX_AMOUNT_LAST_N_DAYS`

**Descrição:** Retorna o valor máximo de transação nos últimos N dias e compara com um threshold.

**Formato do `valueSingle`:**
```
nDays|threshold|operator
```

**Exemplo:**
```
30|10000|GT
```
**Significado:** O valor máximo de transação nos últimos 30 dias é maior que 10000.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar transações atípicas de alto valor.

---

### 7. `MIN_AMOUNT_LAST_N_DAYS`

**Descrição:** Retorna o valor mínimo de transação nos últimos N dias e compara com um threshold.

**Formato do `valueSingle`:**
```
nDays|threshold|operator
```

**Exemplo:**
```
7|10|LT
```
**Significado:** O valor mínimo de transação nos últimos 7 dias é menor que 10.

**Operadores Suportados:** `GT`, `GTE`, `LT`, `LTE`, `EQ`

**Caso de Uso:** Detectar transações de teste ou micro-transações suspeitas.

---

## Exemplo de Regra Completa

### Cenário: Detectar Card Testing

**Descrição:** Bloquear transações se o cartão foi usado em mais de 10 merchants distintos nos últimos 7 dias E a soma das transações é menor que R$ 500.

**Regra JSON:**
```json
{
  "ruleName": "CARD_TESTING_DETECTION",
  "description": "Detecta card testing baseado em múltiplos merchants e baixo valor total",
  "severity": "HIGH",
  "score": 80,
  "rootGroup": {
    "logicOperator": "AND",
    "conditions": [
      {
        "fieldName": "cardNumber",
        "operator": "COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS",
        "valueSingle": "7|10|GT"
      },
      {
        "fieldName": "amount",
        "operator": "SUM_LAST_N_DAYS",
        "valueSingle": "amount|7|500|LT"
      }
    ]
  }
}
```

---

## Performance e Cache

Todos os operadores de agregação temporal utilizam o **`VelocityServiceFacade`**, que implementa cache em Redis para garantir latência ultrabaixa (<1ms).

O cache é atualizado em tempo real a cada transação processada, garantindo que as estatísticas estejam sempre atualizadas.

---

## Próximos Passos

- **Frontend:** Atualizar o `ComplexRuleBuilder` para suportar a criação visual dessas regras.
- **Documentação de API:** Adicionar exemplos de uso no Swagger/OpenAPI.
- **Monitoramento:** Adicionar métricas de uso desses operadores no dashboard de performance.

---

## Referências

- [Velocity Service Documentation](./VELOCITY_SERVICE.md)
- [Complex Rule Evaluator Source Code](../backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java)
- [Unit Tests](../backend/src/test/java/com/rulex/service/complex/ComplexRuleEvaluatorAggregationTest.java)
