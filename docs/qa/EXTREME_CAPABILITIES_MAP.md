# EXTREME CAPABILITIES MAP - RULEX

## Data da Auditoria
2024-12-31T23:00:00Z

---

## 1. OPERADORES SUPORTADOS

### 1.1 Backend (RuleCondition.java)
**Total: 50 operadores**

```java
// Comparação básica (6)
EQ, NEQ, GT, GTE, LT, LTE

// Listas (2)
IN, NOT_IN

// Strings (6)
CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH, REGEX, NOT_REGEX

// Nulos (2)
IS_NULL, NOT_NULL

// Booleanos (2)
IS_TRUE, IS_FALSE

// Range (2)
BETWEEN, NOT_BETWEEN

// Comparação entre campos (6)
FIELD_EQ, FIELD_NEQ, FIELD_GT, FIELD_GTE, FIELD_LT, FIELD_LTE

// Data/Tempo (6)
DATE_BEFORE, DATE_AFTER, DATE_BETWEEN, TIME_BEFORE, TIME_AFTER, TIME_BETWEEN

// Arrays (5)
ARRAY_CONTAINS, ARRAY_NOT_CONTAINS, ARRAY_SIZE_EQ, ARRAY_SIZE_GT, ARRAY_SIZE_LT

// Matemáticos (2)
MOD_EQ, MOD_NEQ

// Geolocalização (3)
GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON

// Velocity (8)
VELOCITY_COUNT_GT, VELOCITY_COUNT_LT, VELOCITY_SUM_GT, VELOCITY_SUM_LT,
VELOCITY_AVG_GT, VELOCITY_AVG_LT, VELOCITY_DISTINCT_GT, VELOCITY_DISTINCT_LT
```

**Evidência:** `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java:85-130`

### 1.2 Frontend - RuleFormDialog (types.ts)
**Total: 52 operadores** (inclui 2 legacy para compatibilidade)

```typescript
// Todos os 50 do backend + 2 legacy:
// - NE (legacy para NEQ)
// - MATCHES_REGEX (legacy para REGEX)
// - IS_NOT_NULL (legacy para NOT_NULL)
```

**Evidência:** `client/src/components/RuleFormDialog/types.ts:60-120`

### 1.3 Frontend - ComplexRuleBuilder (types.ts)
**Total: 52 operadores** (paridade com RuleFormDialog)

**Evidência:** `client/src/components/ComplexRuleBuilder/types.ts:25-85`

---

## 2. OPERADORES LÓGICOS

### Backend + Frontend
```
AND  - Todas as condições devem ser verdadeiras
OR   - Pelo menos uma condição deve ser verdadeira
NOT  - Inverte o resultado do grupo
XOR  - Exatamente uma condição deve ser verdadeira
NAND - Pelo menos uma condição deve ser falsa
NOR  - Todas as condições devem ser falsas
```

**Evidência:** 
- Backend: `RuleConditionGroup.java:LogicOperator`
- Frontend: `types.ts:LOGIC_OPERATORS`

---

## 3. ENGINES DE AVALIAÇÃO

### 3.1 ComplexRuleEvaluator
**Localização:** `service/complex/ComplexRuleEvaluator.java`

**Capacidades:**
- Avaliação de grupos aninhados (até 10 níveis)
- Suporte a todos os 50 operadores
- Integração com GeoService e VelocityService
- Proteção contra ReDoS (RegexValidator)

### 3.2 HomologRuleEvaluator
**Localização:** `homolog/service/HomologRuleEvaluator.java`

**Capacidades:**
- Avaliação de regras simples
- Integração com RuleSet e versões

### 3.3 V31 Engine
**Localização:** `v31/rules/RulesV31Controller.java`

**Capacidades:**
- Avaliação de transações
- Integração com field dictionary

---

## 4. SERVIÇOS ESPECIALIZADOS

### 4.1 GeoService
**Localização:** `service/GeoService.java`

**Capacidades:**
- Cálculo de distância Haversine
- Verificação de ponto em polígono
- Lookup de coordenadas por referência

**Tabelas:**
- `geo_reference` - Referências geográficas (lat/lon)
- `geo_polygon` - Polígonos para verificação

### 4.2 VelocityService
**Localização:** `service/VelocityService.java`

**Capacidades:**
- COUNT - Contagem de eventos
- SUM - Soma de valores
- AVG - Média de valores
- DISTINCT - Valores únicos

**Tabelas:**
- `velocity_counters` - Contadores agregados
- `velocity_transaction_log` - Log de transações para janela temporal

### 4.3 RegexValidator
**Localização:** `util/RegexValidator.java`

**Capacidades:**
- Validação de padrões regex
- Proteção contra ReDoS (padrões perigosos)
- Timeout de execução

---

## 5. LIMITES ANTI-ABUSO

### Backend (RuleValidationService.java)
```java
MAX_NESTING_DEPTH = 10      // Profundidade máxima de aninhamento
MAX_CONDITIONS_PER_GROUP = 50  // Condições por grupo
MAX_GROUPS_PER_RULE = 100   // Grupos por regra
MAX_RULE_JSON_SIZE = 1MB    // Tamanho máximo do JSON
MAX_LIST_SIZE = 1000        // Itens em IN/NOT_IN
MAX_REGEX_LENGTH = 500      // Tamanho do padrão regex
```

### Frontend (schema.ts)
```typescript
MAX_CONDITIONS = 50         // Condições por regra
MAX_RULE_NAME_LENGTH = 100  // Nome da regra
MAX_DESCRIPTION_LENGTH = 500 // Descrição
```

**Evidência:** 
- Backend: `service/complex/RuleValidationService.java`
- Frontend: `components/RuleFormDialog/schema.ts`

---

## 6. TIPOS DE VALOR SUPORTADOS

```java
STRING, NUMBER, BOOLEAN, DATE, TIME, DATETIME,
ARRAY_STRING, ARRAY_NUMBER, FIELD_REFERENCE,
EXPRESSION, GEO_POINT, GEO_POLYGON
```

**Evidência:** `RuleCondition.java:ConditionValueType`

---

## 7. CAMPOS DO PAYLOAD (TransactionRequest)

### Identificação
- externalTransactionId, customerIdFromHeader, customerAcctNumber, pan

### Valores
- transactionAmount, availableCredit, cardCashBalance, cardDelinquentAmount

### Data/Hora
- transactionDate (YYYYMMDD), transactionTime (HHMMSS)

### Merchant
- merchantId, merchantName, merchantCountryCode, merchantCity, merchantState, merchantPostalCode

### Segurança
- consumerAuthenticationScore, externalScore3, cavvResult, eciIndicator, cryptogramValid, cvv2Response

### Categoria
- mcc, posEntryMode, customerPresent

**Evidência:** `client/src/lib/javaApi.ts:TransactionRequest`

---

## 8. PERSISTÊNCIA

### Tabelas Principais
| Tabela | Descrição | Migration |
|--------|-----------|-----------|
| rules | Regras simples | V1 |
| rule_configurations | Configurações de regra | V2 |
| complex_rules | Regras complexas | V8, V12 |
| rule_condition_groups | Grupos de condições | V8 |
| rule_conditions | Condições individuais | V8 |
| velocity_counters | Contadores de velocidade | V14 |
| geo_reference | Referências geográficas | V13 |
| geo_polygon | Polígonos geográficos | V13 |
| audit_log | Log de auditoria | V9 |

### Constraints Importantes
- `chk_condition_groups_has_parent` - Garante FK em rule_condition_groups (V18)
- `chk_no_self_reference` - Evita auto-referência em grupos

---

## 9. CAPACIDADES NÃO IMPLEMENTADAS

| Feature | Status | Observação |
|---------|--------|------------|
| Rate Limiting | ❌ | Não implementado |
| JWT Auth | ❌ | Usa Basic Auth |
| OpenTelemetry | ❌ | Logs simples |
| Dashboards Grafana | ❌ | Não configurado |

---

## Última Atualização
2024-12-31T23:00:00Z
