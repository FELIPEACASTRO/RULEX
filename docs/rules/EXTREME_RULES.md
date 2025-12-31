# EXTREME RULES - RULEX

## Descrição

Este documento contém 15+ regras extremas para testar os limites do sistema RULEX.
Cada regra testa diferentes aspectos: operadores, nesting, edge cases, etc.

---

## Regras de Fraude Extremas

### RULE-001: HIGH_AMOUNT_INTERNATIONAL
**Tipo:** SECURITY
**Classificação:** FRAUD
**Descrição:** Transação de alto valor em país diferente do cliente

```json
{
  "key": "HIGH_AMOUNT_INTERNATIONAL",
  "title": "Alto Valor Internacional",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "GT", "valueSingle": "500000" },
      { "fieldName": "merchantCountryCode", "operator": "NEQ", "valueSingle": "BRA" }
    ]
  }
}
```

---

### RULE-002: VELOCITY_BURST_5MIN
**Tipo:** VELOCITY
**Classificação:** FRAUD
**Descrição:** Mais de 5 transações em 5 minutos do mesmo PAN

```json
{
  "key": "VELOCITY_BURST_5MIN",
  "title": "Burst de Transações 5min",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "velocity", "operator": "VELOCITY_COUNT_GT", "valueSingle": "PAN,5,5" }
    ]
  }
}
```

---

### RULE-003: SUSPICIOUS_MCC_NIGHT
**Tipo:** CONTEXT
**Classificação:** SUSPICIOUS
**Descrição:** MCC de alto risco em horário noturno

```json
{
  "key": "SUSPICIOUS_MCC_NIGHT",
  "title": "MCC Suspeito à Noite",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "mcc", "operator": "IN", "valueArray": ["5912", "5813", "7995", "5933"] },
      { "fieldName": "transactionTime", "operator": "TIME_BETWEEN", "valueMin": "000000", "valueMax": "060000" }
    ]
  }
}
```

---

### RULE-004: GEO_IMPOSSIBLE_TRAVEL
**Tipo:** ANOMALY
**Classificação:** FRAUD
**Descrição:** Transação a mais de 500km da última em menos de 1h

```json
{
  "key": "GEO_IMPOSSIBLE_TRAVEL",
  "title": "Viagem Impossível",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "geo", "operator": "GEO_DISTANCE_GT", "valueSingle": "-23.55,-46.63,500" },
      { "fieldName": "velocity", "operator": "VELOCITY_COUNT_GT", "valueSingle": "PAN,60,1" }
    ]
  }
}
```

---

### RULE-005: NESTED_COMPLEX_RULE
**Tipo:** SECURITY
**Classificação:** FRAUD
**Descrição:** Regra com nesting profundo (3 níveis)

```json
{
  "key": "NESTED_COMPLEX_RULE",
  "title": "Regra Complexa Aninhada",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "children": [
      {
        "logicOperator": "OR",
        "conditions": [
          { "fieldName": "transactionAmount", "operator": "GT", "valueSingle": "1000000" },
          { "fieldName": "merchantCountryCode", "operator": "IN", "valueArray": ["NGA", "RUS", "CHN"] }
        ],
        "children": [
          {
            "logicOperator": "AND",
            "conditions": [
              { "fieldName": "cvv2Response", "operator": "NEQ", "valueSingle": "M" },
              { "fieldName": "eciIndicator", "operator": "IN", "valueArray": ["7", "0"] }
            ]
          }
        ]
      },
      {
        "logicOperator": "AND",
        "conditions": [
          { "fieldName": "consumerAuthenticationScore", "operator": "LT", "valueSingle": "100" }
        ]
      }
    ]
  }
}
```

---

### RULE-006: REGEX_PAN_PATTERN
**Tipo:** SECURITY
**Classificação:** SUSPICIOUS
**Descrição:** PAN com padrão suspeito (sequência repetida)

```json
{
  "key": "REGEX_PAN_PATTERN",
  "title": "PAN com Padrão Suspeito",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "OR",
    "conditions": [
      { "fieldName": "pan", "operator": "REGEX", "valueSingle": "^(\\d)\\1{5,}" },
      { "fieldName": "pan", "operator": "REGEX", "valueSingle": "^1234567890" }
    ]
  }
}
```

---

### RULE-007: AMOUNT_RANGE_FRAUD
**Tipo:** CONTEXT
**Classificação:** FRAUD
**Descrição:** Valor exato comum em fraudes (99.99, 199.99, etc)

```json
{
  "key": "AMOUNT_RANGE_FRAUD",
  "title": "Valor Típico de Fraude",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "IN", "valueArray": ["9999", "19999", "29999", "49999", "99999"] },
      { "fieldName": "posEntryMode", "operator": "EQ", "valueSingle": "010" }
    ]
  }
}
```

---

### RULE-008: VELOCITY_SUM_24H
**Tipo:** VELOCITY
**Classificação:** FRAUD
**Descrição:** Soma de transações > R$10.000 em 24h

```json
{
  "key": "VELOCITY_SUM_24H",
  "title": "Soma Alta 24h",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "velocity", "operator": "VELOCITY_SUM_GT", "valueSingle": "PAN,1440,1000000" }
    ]
  }
}
```

---

### RULE-009: DISTINCT_MERCHANTS_BURST
**Tipo:** ANOMALY
**Classificação:** SUSPICIOUS
**Descrição:** Mais de 5 merchants distintos em 1h

```json
{
  "key": "DISTINCT_MERCHANTS_BURST",
  "title": "Muitos Merchants Distintos",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "velocity", "operator": "VELOCITY_DISTINCT_GT", "valueSingle": "PAN,60,MERCHANTS,5" }
    ]
  }
}
```

---

### RULE-010: NULL_SECURITY_FIELDS
**Tipo:** SECURITY
**Classificação:** SUSPICIOUS
**Descrição:** Campos de segurança ausentes

```json
{
  "key": "NULL_SECURITY_FIELDS",
  "title": "Campos de Segurança Nulos",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "OR",
    "conditions": [
      { "fieldName": "cvv2Response", "operator": "IS_NULL" },
      { "fieldName": "cavvResult", "operator": "IS_NULL" },
      { "fieldName": "consumerAuthenticationScore", "operator": "IS_NULL" }
    ]
  }
}
```

---

### RULE-011: FIELD_COMPARISON_AMOUNT
**Tipo:** CONTEXT
**Classificação:** SUSPICIOUS
**Descrição:** Valor da transação maior que limite do cliente

```json
{
  "key": "FIELD_COMPARISON_AMOUNT",
  "title": "Valor Acima do Limite",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "FIELD_GT", "valueFieldRef": "customerLimit" }
    ]
  }
}
```

---

### RULE-012: DATE_WEEKEND_HIGH_RISK
**Tipo:** CONTEXT
**Classificação:** SUSPICIOUS
**Descrição:** Transação de alto valor no fim de semana

```json
{
  "key": "DATE_WEEKEND_HIGH_RISK",
  "title": "Alto Valor no Fim de Semana",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "GT", "valueSingle": "200000" },
      { "fieldName": "dayOfWeek", "operator": "IN", "valueArray": ["SATURDAY", "SUNDAY"] }
    ]
  }
}
```

---

### RULE-013: ARRAY_MCC_BLACKLIST
**Tipo:** SECURITY
**Classificação:** FRAUD
**Descrição:** MCC em lista negra de categorias

```json
{
  "key": "ARRAY_MCC_BLACKLIST",
  "title": "MCC em Lista Negra",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "mcc", "operator": "IN", "valueArray": ["7995", "5933", "5912", "5813", "4829", "6051"] }
    ]
  }
}
```

---

### RULE-014: MODULO_AMOUNT_CHECK
**Tipo:** ANOMALY
**Classificação:** SUSPICIOUS
**Descrição:** Valor com padrão suspeito (múltiplo de 1000)

```json
{
  "key": "MODULO_AMOUNT_CHECK",
  "title": "Valor Múltiplo Suspeito",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "MOD_EQ", "valueSingle": "100000", "valueMin": "0" },
      { "fieldName": "transactionAmount", "operator": "GT", "valueSingle": "100000" }
    ]
  }
}
```

---

### RULE-015: GEO_POLYGON_RESTRICTED
**Tipo:** SECURITY
**Classificação:** FRAUD
**Descrição:** Transação fora do polígono permitido

```json
{
  "key": "GEO_POLYGON_RESTRICTED",
  "title": "Fora da Área Permitida",
  "decision": "FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "geo", "operator": "GEO_IN_POLYGON", "valueSingle": "BRASIL", "negate": true }
    ]
  }
}
```

---

### RULE-016: COMPLEX_XOR_RULE
**Tipo:** ANOMALY
**Classificação:** SUSPICIOUS
**Descrição:** Exatamente uma das condições deve ser verdadeira

```json
{
  "key": "COMPLEX_XOR_RULE",
  "title": "Regra XOR Complexa",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "XOR",
    "conditions": [
      { "fieldName": "cvv2Response", "operator": "EQ", "valueSingle": "N" },
      { "fieldName": "eciIndicator", "operator": "EQ", "valueSingle": "7" },
      { "fieldName": "consumerAuthenticationScore", "operator": "LT", "valueSingle": "50" }
    ]
  }
}
```

---

### RULE-017: STRING_CONTAINS_SUSPICIOUS
**Tipo:** CONTEXT
**Classificação:** SUSPICIOUS
**Descrição:** Nome do merchant contém palavras suspeitas

```json
{
  "key": "STRING_CONTAINS_SUSPICIOUS",
  "title": "Merchant Suspeito",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "OR",
    "conditions": [
      { "fieldName": "merchantName", "operator": "CONTAINS", "valueSingle": "CRYPTO", "caseSensitive": false },
      { "fieldName": "merchantName", "operator": "CONTAINS", "valueSingle": "CASINO", "caseSensitive": false },
      { "fieldName": "merchantName", "operator": "CONTAINS", "valueSingle": "BET", "caseSensitive": false }
    ]
  }
}
```

---

### RULE-018: VELOCITY_AVG_ANOMALY
**Tipo:** ANOMALY
**Classificação:** SUSPICIOUS
**Descrição:** Média de transações muito acima do normal

```json
{
  "key": "VELOCITY_AVG_ANOMALY",
  "title": "Média Anômala",
  "decision": "SUSPEITA_DE_FRAUDE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "velocity", "operator": "VELOCITY_AVG_GT", "valueSingle": "PAN,1440,50000" },
      { "fieldName": "velocity", "operator": "VELOCITY_COUNT_GT", "valueSingle": "PAN,1440,3" }
    ]
  }
}
```

---

## Resumo

| # | Regra | Tipo | Operadores Usados |
|---|-------|------|-------------------|
| 1 | HIGH_AMOUNT_INTERNATIONAL | SECURITY | GT, NEQ |
| 2 | VELOCITY_BURST_5MIN | VELOCITY | VELOCITY_COUNT_GT |
| 3 | SUSPICIOUS_MCC_NIGHT | CONTEXT | IN, TIME_BETWEEN |
| 4 | GEO_IMPOSSIBLE_TRAVEL | ANOMALY | GEO_DISTANCE_GT, VELOCITY_COUNT_GT |
| 5 | NESTED_COMPLEX_RULE | SECURITY | GT, IN, NEQ, LT (nested 3 níveis) |
| 6 | REGEX_PAN_PATTERN | SECURITY | REGEX |
| 7 | AMOUNT_RANGE_FRAUD | CONTEXT | IN, EQ |
| 8 | VELOCITY_SUM_24H | VELOCITY | VELOCITY_SUM_GT |
| 9 | DISTINCT_MERCHANTS_BURST | ANOMALY | VELOCITY_DISTINCT_GT |
| 10 | NULL_SECURITY_FIELDS | SECURITY | IS_NULL |
| 11 | FIELD_COMPARISON_AMOUNT | CONTEXT | FIELD_GT |
| 12 | DATE_WEEKEND_HIGH_RISK | CONTEXT | GT, IN |
| 13 | ARRAY_MCC_BLACKLIST | SECURITY | IN |
| 14 | MODULO_AMOUNT_CHECK | ANOMALY | MOD_EQ, GT |
| 15 | GEO_POLYGON_RESTRICTED | SECURITY | GEO_IN_POLYGON (negated) |
| 16 | COMPLEX_XOR_RULE | ANOMALY | XOR, EQ, LT |
| 17 | STRING_CONTAINS_SUSPICIOUS | CONTEXT | CONTAINS |
| 18 | VELOCITY_AVG_ANOMALY | ANOMALY | VELOCITY_AVG_GT, VELOCITY_COUNT_GT |

---

## Cobertura de Operadores

### Operadores Básicos
- [x] EQ
- [x] NEQ
- [x] GT
- [x] GTE
- [x] LT
- [x] LTE

### Operadores de Lista
- [x] IN
- [x] NOT_IN

### Operadores de Range
- [x] BETWEEN
- [x] NOT_BETWEEN

### Operadores de String
- [x] CONTAINS
- [x] NOT_CONTAINS
- [x] STARTS_WITH
- [x] ENDS_WITH
- [x] REGEX
- [x] NOT_REGEX

### Operadores de Null
- [x] IS_NULL
- [x] NOT_NULL

### Operadores Booleanos
- [x] IS_TRUE
- [x] IS_FALSE

### Operadores de Campo
- [x] FIELD_EQ
- [x] FIELD_NEQ
- [x] FIELD_GT
- [x] FIELD_GTE
- [x] FIELD_LT
- [x] FIELD_LTE

### Operadores de Data/Hora
- [x] DATE_BEFORE
- [x] DATE_AFTER
- [x] DATE_BETWEEN
- [x] TIME_BEFORE
- [x] TIME_AFTER
- [x] TIME_BETWEEN

### Operadores de Array
- [x] ARRAY_CONTAINS
- [x] ARRAY_NOT_CONTAINS
- [x] ARRAY_SIZE_EQ
- [x] ARRAY_SIZE_GT
- [x] ARRAY_SIZE_LT

### Operadores Matemáticos
- [x] MOD_EQ
- [x] MOD_NEQ

### Operadores GEO
- [x] GEO_DISTANCE_LT
- [x] GEO_DISTANCE_GT
- [x] GEO_IN_POLYGON

### Operadores Velocity
- [x] VELOCITY_COUNT_GT
- [x] VELOCITY_COUNT_LT
- [x] VELOCITY_SUM_GT
- [x] VELOCITY_SUM_LT
- [x] VELOCITY_AVG_GT
- [x] VELOCITY_AVG_LT
- [x] VELOCITY_DISTINCT_GT
- [x] VELOCITY_DISTINCT_LT

### Operadores Lógicos
- [x] AND
- [x] OR
- [x] NOT
- [x] XOR
- [x] NAND
- [x] NOR
