# RULEX - Capacidades do Motor de Regras

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Fonte:** Código-fonte do backend

---

## 1. Operadores de Comparação

### 1.1 Operadores Básicos

| Operador | Enum | Descrição | Exemplo |
|----------|------|-----------|---------|
| `EQ` | `ConditionOperator.EQ` | Igual a | `transactionAmount EQ 1000` |
| `NEQ` | `ConditionOperator.NEQ` | Diferente de | `mcc NEQ 7995` |
| `GT` | `ConditionOperator.GT` | Maior que | `transactionAmount GT 5000` |
| `GTE` | `ConditionOperator.GTE` | Maior ou igual | `riskScore GTE 70` |
| `LT` | `ConditionOperator.LT` | Menor que | `consumerAuthenticationScore LT 50` |
| `LTE` | `ConditionOperator.LTE` | Menor ou igual | `externalScore3 LTE 30` |

### 1.2 Operadores de Lista

| Operador | Enum | Descrição | Formato do Valor |
|----------|------|-----------|------------------|
| `IN` | `ConditionOperator.IN` | Valor está na lista | `valueArray: ["7995", "6211", "6051"]` |
| `NOT_IN` | `ConditionOperator.NOT_IN` | Valor não está na lista | `valueArray: ["076", "840"]` |

### 1.3 Operadores de Range

| Operador | Enum | Descrição | Formato do Valor |
|----------|------|-----------|------------------|
| `BETWEEN` | `ConditionOperator.BETWEEN` | Valor entre min e max | `valueMin: "1000", valueMax: "5000"` |
| `NOT_BETWEEN` | `ConditionOperator.NOT_BETWEEN` | Valor fora do range | `valueMin: "0", valueMax: "60000"` |

### 1.4 Operadores de String

| Operador | Enum | Descrição | Case Sensitive |
|----------|------|-----------|----------------|
| `CONTAINS` | `ConditionOperator.CONTAINS` | Texto contém substring | Configurável |
| `NOT_CONTAINS` | `ConditionOperator.NOT_CONTAINS` | Texto não contém | Configurável |
| `STARTS_WITH` | `ConditionOperator.STARTS_WITH` | Começa com | Configurável |
| `ENDS_WITH` | `ConditionOperator.ENDS_WITH` | Termina com | Configurável |
| `REGEX` | `ConditionOperator.REGEX` | Expressão regular | N/A |
| `NOT_REGEX` | `ConditionOperator.NOT_REGEX` | Não corresponde à regex | N/A |

**Nota sobre REGEX**: O sistema inclui proteção contra ReDoS (Regex Denial of Service) via `RegexValidator`.

### 1.5 Operadores de Nulo/Booleano

| Operador | Enum | Descrição |
|----------|------|-----------|
| `IS_NULL` | `ConditionOperator.IS_NULL` | Campo é nulo |
| `NOT_NULL` | `ConditionOperator.NOT_NULL` | Campo não é nulo |
| `IS_TRUE` | `ConditionOperator.IS_TRUE` | Campo é verdadeiro |
| `IS_FALSE` | `ConditionOperator.IS_FALSE` | Campo é falso |

### 1.6 Operadores de Comparação entre Campos

| Operador | Enum | Descrição |
|----------|------|-----------|
| `FIELD_EQ` | `ConditionOperator.FIELD_EQ` | Igual a outro campo |
| `FIELD_NEQ` | `ConditionOperator.FIELD_NEQ` | Diferente de outro campo |
| `FIELD_GT` | `ConditionOperator.FIELD_GT` | Maior que outro campo |
| `FIELD_GTE` | `ConditionOperator.FIELD_GTE` | Maior ou igual a outro campo |
| `FIELD_LT` | `ConditionOperator.FIELD_LT` | Menor que outro campo |
| `FIELD_LTE` | `ConditionOperator.FIELD_LTE` | Menor ou igual a outro campo |

**Exemplo**: `transactionAmount FIELD_GT availableCredit` (transação maior que crédito disponível)

### 1.7 Operadores Temporais

| Operador | Enum | Descrição | Formato do Valor |
|----------|------|-----------|------------------|
| `DATE_BEFORE` | `ConditionOperator.DATE_BEFORE` | Data anterior a | `"2024-12-31"` |
| `DATE_AFTER` | `ConditionOperator.DATE_AFTER` | Data posterior a | `"2024-01-01"` |
| `DATE_BETWEEN` | `ConditionOperator.DATE_BETWEEN` | Data entre | `valueMin, valueMax` |
| `TIME_BEFORE` | `ConditionOperator.TIME_BEFORE` | Hora anterior a | `"06:00:00"` |
| `TIME_AFTER` | `ConditionOperator.TIME_AFTER` | Hora posterior a | `"22:00:00"` |
| `TIME_BETWEEN` | `ConditionOperator.TIME_BETWEEN` | Hora entre | `valueMin, valueMax` |

### 1.8 Operadores de Array

| Operador | Enum | Descrição |
|----------|------|-----------|
| `ARRAY_CONTAINS` | `ConditionOperator.ARRAY_CONTAINS` | Array contém valor |
| `ARRAY_NOT_CONTAINS` | `ConditionOperator.ARRAY_NOT_CONTAINS` | Array não contém valor |
| `ARRAY_SIZE_EQ` | `ConditionOperator.ARRAY_SIZE_EQ` | Tamanho do array igual a |
| `ARRAY_SIZE_GT` | `ConditionOperator.ARRAY_SIZE_GT` | Tamanho do array maior que |
| `ARRAY_SIZE_LT` | `ConditionOperator.ARRAY_SIZE_LT` | Tamanho do array menor que |

### 1.9 Operadores Matemáticos

| Operador | Enum | Descrição | Formato do Valor |
|----------|------|-----------|------------------|
| `MOD_EQ` | `ConditionOperator.MOD_EQ` | Módulo igual a | `valueSingle: "divisor", valueMin: "remainder"` |
| `MOD_NEQ` | `ConditionOperator.MOD_NEQ` | Módulo diferente de | `valueSingle: "divisor", valueMin: "remainder"` |

**Exemplo**: `transactionAmount MOD_EQ 100000,0` (valor divisível por R$1.000)

### 1.10 Operadores de Geolocalização

| Operador | Enum | Descrição | Formato do Valor |
|----------|------|-----------|------------------|
| `GEO_DISTANCE_LT` | `ConditionOperator.GEO_DISTANCE_LT` | Distância menor que (km) | `"lat,lon,distanceKm"` |
| `GEO_DISTANCE_GT` | `ConditionOperator.GEO_DISTANCE_GT` | Distância maior que (km) | `"lat,lon,distanceKm"` |
| `GEO_IN_POLYGON` | `ConditionOperator.GEO_IN_POLYGON` | Dentro do polígono | `"POLYGON_NAME"` |

**Exemplo**: `GEO_DISTANCE_GT -23.55,-46.63,1000` (mais de 1000km de São Paulo)

**Nota**: Coordenadas são derivadas de `merchantCity`, `merchantState`, `merchantCountryCode` via tabela `geo_reference`.

### 1.11 Operadores de Velocity (Agregações Temporais)

| Operador | Enum | Descrição | Formato do Valor |
|----------|------|-----------|------------------|
| `VELOCITY_COUNT_GT` | `ConditionOperator.VELOCITY_COUNT_GT` | Contagem maior que | `"keyType,windowMinutes,threshold"` |
| `VELOCITY_COUNT_LT` | `ConditionOperator.VELOCITY_COUNT_LT` | Contagem menor que | `"keyType,windowMinutes,threshold"` |
| `VELOCITY_SUM_GT` | `ConditionOperator.VELOCITY_SUM_GT` | Soma maior que | `"keyType,windowMinutes,threshold"` |
| `VELOCITY_SUM_LT` | `ConditionOperator.VELOCITY_SUM_LT` | Soma menor que | `"keyType,windowMinutes,threshold"` |
| `VELOCITY_AVG_GT` | `ConditionOperator.VELOCITY_AVG_GT` | Média maior que | `"keyType,windowMinutes,threshold"` |
| `VELOCITY_AVG_LT` | `ConditionOperator.VELOCITY_AVG_LT` | Média menor que | `"keyType,windowMinutes,threshold"` |
| `VELOCITY_DISTINCT_GT` | `ConditionOperator.VELOCITY_DISTINCT_GT` | Distintos maior que | `"keyType,windowMinutes,distinctType,threshold"` |
| `VELOCITY_DISTINCT_LT` | `ConditionOperator.VELOCITY_DISTINCT_LT` | Distintos menor que | `"keyType,windowMinutes,distinctType,threshold"` |

**Tipos de Chave (KeyType)**:
- `PAN` - Por cartão (hash SHA-256)
- `CUSTOMER_ID` - Por cliente
- `MERCHANT_ID` - Por estabelecimento

**Tipos de Distintos (DistinctType)**:
- `MERCHANTS` - Merchants distintos
- `MCCS` - MCCs distintos
- `COUNTRIES` - Países distintos

**Janelas Temporais**:
| Janela | Minutos |
|--------|---------|
| `MINUTE_5` | 5 |
| `MINUTE_15` | 15 |
| `MINUTE_30` | 30 |
| `HOUR_1` | 60 |
| `HOUR_6` | 360 |
| `HOUR_12` | 720 |
| `HOUR_24` | 1440 |
| `DAY_7` | 10080 |
| `DAY_30` | 43200 |

**Exemplos**:
- `VELOCITY_COUNT_GT PAN,60,5` → Mais de 5 transações do mesmo PAN na última hora
- `VELOCITY_SUM_GT PAN,1440,1000000` → Soma > R$10.000 do mesmo PAN em 24h
- `VELOCITY_DISTINCT_GT PAN,1440,MERCHANTS,3` → Mais de 3 merchants distintos em 24h

---

## 2. Operadores Lógicos de Grupo

| Operador | Enum | Descrição |
|----------|------|-----------|
| `AND` | `GroupLogicOperator.AND` | Todas as condições devem ser verdadeiras |
| `OR` | `GroupLogicOperator.OR` | Pelo menos uma condição deve ser verdadeira |
| `NOT` | `GroupLogicOperator.NOT` | Inverte o resultado do grupo |
| `XOR` | `GroupLogicOperator.XOR` | Exatamente uma condição deve ser verdadeira |
| `NAND` | `GroupLogicOperator.NAND` | Pelo menos uma condição deve ser falsa |
| `NOR` | `GroupLogicOperator.NOR` | Todas as condições devem ser falsas |

---

## 3. Decisões (Outcomes)

| Decisão | Enum | Descrição |
|---------|------|-----------|
| `APROVADO` | `DecisionOutcome.APROVADO` | Transação aprovada |
| `SUSPEITA_DE_FRAUDE` | `DecisionOutcome.SUSPEITA_DE_FRAUDE` | Transação suspeita (review) |
| `FRAUDE` | `DecisionOutcome.FRAUDE` | Transação fraudulenta (bloquear) |

---

## 4. Ações Suportadas

| Ação | Enum | Descrição |
|------|------|-----------|
| `SET_DECISION` | `RuleActionType.SET_DECISION` | Define a decisão |
| `SET_SCORE` | `RuleActionType.SET_SCORE` | Define o score de risco |
| `ADD_TAG` | `RuleActionType.ADD_TAG` | Adiciona tag à transação |
| `REMOVE_TAG` | `RuleActionType.REMOVE_TAG` | Remove tag da transação |
| `SET_VARIABLE` | `RuleActionType.SET_VARIABLE` | Define variável de contexto |
| `CALL_WEBHOOK` | `RuleActionType.CALL_WEBHOOK` | Chama webhook externo |
| `SEND_NOTIFICATION` | `RuleActionType.SEND_NOTIFICATION` | Envia notificação |
| `BLOCK_TRANSACTION` | `RuleActionType.BLOCK_TRANSACTION` | Bloqueia transação |
| `FLAG_FOR_REVIEW` | `RuleActionType.FLAG_FOR_REVIEW` | Marca para revisão |
| `ESCALATE` | `RuleActionType.ESCALATE` | Escala para nível superior |

---

## 5. Tipos de Regras

### 5.1 Regras Simples (`rule_configurations`)

```json
{
  "rule_name": "HIGH_VALUE_INTERNATIONAL",
  "rule_type": "CONTEXT",
  "threshold": 70,
  "weight": 75,
  "classification": "SUSPICIOUS",
  "conditions_json": [
    {"field": "transactionAmount", "operator": "GT", "value": "500000"},
    {"field": "merchantCountryCode", "operator": "NEQ", "value": "076"}
  ],
  "logic_operator": "AND"
}
```

### 5.2 Regras Complexas (`complex_rules` + grupos)

```json
{
  "key": "COMPLEX_FRAUD_RULE",
  "title": "Regra Complexa de Fraude",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      {"fieldName": "transactionAmount", "operator": "GT", "valueSingle": "1000"}
    ],
    "children": [
      {
        "logicOperator": "OR",
        "conditions": [
          {"fieldName": "mcc", "operator": "IN", "valueArray": ["7995", "6211"]},
          {"fieldName": "merchantCountryCode", "operator": "NEQ", "valueSingle": "076"}
        ]
      }
    ]
  },
  "decision": "SUSPEITA_DE_FRAUDE",
  "severity": 80
}
```

---

## 6. Funções Suportadas (AST Evaluator)

| Função | Descrição | Exemplo |
|--------|-----------|---------|
| `TRIM` | Remove espaços | `TRIM(merchantName)` |
| `LOWER` | Converte para minúsculas | `LOWER(merchantName)` |
| `UPPER` | Converte para maiúsculas | `UPPER(merchantName)` |
| `LEN` | Tamanho da string | `LEN(pan)` |
| `ABS` | Valor absoluto | `ABS(transactionAmount)` |
| `COALESCE` | Primeiro não-nulo | `COALESCE(merchantId, "UNKNOWN")` |
| `TO_DATE_YYYYMMDD` | Converte para data | `TO_DATE_YYYYMMDD(transactionDate)` |
| `TO_TIME_PAD6_HHMMSS` | Converte para hora | `TO_TIME_PAD6_HHMMSS(transactionTime)` |
| `PARSE_GMTOFFSET` | Parseia offset GMT | `PARSE_GMTOFFSET(gmtOffset)` |

---

## 7. Limitações

### 7.1 Nesting
- Máximo de **10 níveis** de profundidade
- Grupos desabilitados são ignorados (retornam `true`)

### 7.2 Geolocalização
- Coordenadas derivadas de cidade/estado/país
- Se cidade não encontrada, usa capital do estado
- Se estado não encontrado, usa capital do país
- Polígonos são bounding boxes simplificados

### 7.3 Velocity
- Agregações calculadas em tempo real via DB
- PAN armazenado como hash SHA-256 (privacidade)
- Cache in-memory para performance

### 7.4 Regex
- Proteção contra ReDoS
- Timeout de 100ms por match
- Padrões complexos são rejeitados

---

## 8. Tipos de Valor

| Tipo | Enum | Descrição |
|------|------|-----------|
| `STRING` | `ConditionValueType.STRING` | Texto |
| `NUMBER` | `ConditionValueType.NUMBER` | Número |
| `BOOLEAN` | `ConditionValueType.BOOLEAN` | Booleano |
| `DATE` | `ConditionValueType.DATE` | Data |
| `TIME` | `ConditionValueType.TIME` | Hora |
| `DATETIME` | `ConditionValueType.DATETIME` | Data e hora |
| `ARRAY_STRING` | `ConditionValueType.ARRAY_STRING` | Array de strings |
| `ARRAY_NUMBER` | `ConditionValueType.ARRAY_NUMBER` | Array de números |
| `FIELD_REFERENCE` | `ConditionValueType.FIELD_REFERENCE` | Referência a outro campo |
| `EXPRESSION` | `ConditionValueType.EXPRESSION` | Expressão calculada |
| `GEO_POINT` | `ConditionValueType.GEO_POINT` | Ponto geográfico |
| `GEO_POLYGON` | `ConditionValueType.GEO_POLYGON` | Polígono geográfico |

---

## 9. Categorias de Regras

| Categoria | Enum | Descrição |
|-----------|------|-----------|
| `SECURITY` | `RuleType.SECURITY` | Validação técnica (CVV, PIN, criptograma) |
| `CONTEXT` | `RuleType.CONTEXT` | Contexto da transação (MCC, país, valor) |
| `VELOCITY` | `RuleType.VELOCITY` | Frequência e padrões temporais |
| `ANOMALY` | `RuleType.ANOMALY` | Comportamento anômalo |

---

## 10. Referências

- **ComplexRuleEvaluator**: `backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`
- **AstEvaluator**: `backend/src/main/java/com/rulex/v31/ast/AstEvaluator.java`
- **VelocityService**: `backend/src/main/java/com/rulex/service/VelocityService.java`
- **GeoService**: `backend/src/main/java/com/rulex/service/GeoService.java`
- **Migration V8**: `backend/src/main/resources/db/migration/V8__complex_rules_support.sql`
- **Migration V15**: `backend/src/main/resources/db/migration/V15__add_velocity_operators.sql`
