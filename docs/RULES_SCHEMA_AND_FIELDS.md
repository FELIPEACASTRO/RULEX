# RULEX - Schema de Regras e Campos

## 1. Schema de Regra Complexa (ComplexRuleDTO)

```typescript
interface ComplexRule {
  id?: string;                    // UUID gerado automaticamente
  key: string;                    // Chave única da regra (ex: "HIGH_AMOUNT_GAMBLING")
  title: string;                  // Título descritivo
  description?: string;           // Descrição detalhada
  version?: number;               // Versão (incrementada automaticamente)
  status: RuleStatus;             // DRAFT | PUBLISHED | ARCHIVED | TESTING
  priority: number;               // 0-100 (maior = mais prioritário)
  severity: number;               // 0-100 (maior = mais severo)
  decision: DecisionType;         // APROVADO | SUSPEITA_DE_FRAUDE | FRAUDE
  reasonTemplate?: string;        // Template de motivo (suporta variáveis)
  enabled: boolean;               // Se a regra está ativa
  rootConditionGroup: ConditionGroup;  // Grupo raiz de condições
  expressions?: Expression[];     // Expressões calculadas
  contextVariables?: ContextVariable[];  // Variáveis de contexto
  actions?: RuleAction[];         // Ações a executar
  tags?: string[];                // Tags para categorização
  createdAt?: string;             // Data de criação
  updatedAt?: string;             // Data de atualização
}
```

## 2. Schema de Grupo de Condições

```typescript
interface ConditionGroup {
  id: string;                     // UUID
  logicOperator: LogicOperator;   // AND | OR | NOT | XOR | NAND | NOR
  name?: string;                  // Nome do grupo (opcional)
  description?: string;           // Descrição
  position?: number;              // Ordem no grupo pai
  enabled?: boolean;              // Se o grupo está ativo
  conditions: Condition[];        // Condições do grupo
  children: ConditionGroup[];     // Subgrupos (nesting)
}
```

## 3. Schema de Condição

```typescript
interface Condition {
  id: string;                     // UUID
  fieldName: string;              // Nome do campo (ex: "transactionAmount")
  fieldPath?: string;             // Caminho para campos aninhados
  operator: ComparisonOperator;   // Operador de comparação
  valueType: ValueType;           // Tipo do valor
  valueSingle?: string;           // Valor único
  valueArray?: string[];          // Lista de valores (para IN, NOT_IN)
  valueMin?: string;              // Valor mínimo (para BETWEEN)
  valueMax?: string;              // Valor máximo (para BETWEEN)
  valueFieldRef?: string;         // Referência a outro campo (para FIELD_*)
  valueExpression?: string;       // Expressão calculada
  caseSensitive?: boolean;        // Se comparação é case-sensitive
  negate?: boolean;               // Inverte o resultado
  enabled?: boolean;              // Se a condição está ativa
}
```

## 4. Enums

### 4.1 LogicOperator
```typescript
type LogicOperator = 'AND' | 'OR' | 'NOT' | 'XOR' | 'NAND' | 'NOR';
```

### 4.2 ComparisonOperator
```typescript
type ComparisonOperator =
  // Básicos
  | 'EQ' | 'NEQ' | 'GT' | 'GTE' | 'LT' | 'LTE'
  // Listas
  | 'IN' | 'NOT_IN'
  // Range
  | 'BETWEEN' | 'NOT_BETWEEN'
  // Strings
  | 'CONTAINS' | 'NOT_CONTAINS' | 'STARTS_WITH' | 'ENDS_WITH' | 'REGEX' | 'NOT_REGEX'
  // Nulos
  | 'IS_NULL' | 'NOT_NULL'
  // Booleanos
  | 'IS_TRUE' | 'IS_FALSE'
  // Comparação entre campos
  | 'FIELD_EQ' | 'FIELD_NEQ' | 'FIELD_GT' | 'FIELD_GTE' | 'FIELD_LT' | 'FIELD_LTE'
  // Data/Hora
  | 'DATE_BEFORE' | 'DATE_AFTER' | 'DATE_BETWEEN'
  | 'TIME_BEFORE' | 'TIME_AFTER' | 'TIME_BETWEEN'
  // Arrays
  | 'ARRAY_CONTAINS' | 'ARRAY_NOT_CONTAINS' | 'ARRAY_SIZE_EQ' | 'ARRAY_SIZE_GT' | 'ARRAY_SIZE_LT'
  // Matemáticos
  | 'MOD_EQ' | 'MOD_NEQ'
  // Geolocalização
  | 'GEO_DISTANCE_LT' | 'GEO_DISTANCE_GT' | 'GEO_IN_POLYGON';
```

### 4.3 ValueType
```typescript
type ValueType =
  | 'STRING'
  | 'NUMBER'
  | 'BOOLEAN'
  | 'DATE'
  | 'TIME'
  | 'DATETIME'
  | 'ARRAY_STRING'
  | 'ARRAY_NUMBER'
  | 'FIELD_REFERENCE'
  | 'EXPRESSION'
  | 'GEO_POINT'
  | 'GEO_POLYGON';
```

### 4.4 RuleStatus
```typescript
type RuleStatus = 'DRAFT' | 'PUBLISHED' | 'ARCHIVED' | 'TESTING';
```

### 4.5 DecisionType
```typescript
type DecisionType = 'APROVADO' | 'SUSPEITA_DE_FRAUDE' | 'FRAUDE';
```

---

## 5. Campos do TransactionRequest

### 5.1 Campos Obrigatórios

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `externalTransactionId` | String | ID único da transação |
| `customerIdFromHeader` | String | ID do cliente |
| `customerAcctNumber` | Long | Número da conta |
| `pan` | String | Número do cartão |
| `transactionCurrencyCode` | Integer | Código da moeda |
| `transactionAmount` | BigDecimal | Valor da transação |
| `transactionDate` | Integer | Data (YYYYMMDD) |
| `transactionTime` | Integer | Hora (HHMMSS) |
| `mcc` | Integer | Merchant Category Code |
| `consumerAuthenticationScore` | Integer | Score de autenticação |
| `externalScore3` | Integer | Score externo |
| `cavvResult` | Integer | Resultado CAVV |
| `eciIndicator` | Integer | Indicador ECI |
| `atcCard` | Integer | ATC do cartão |
| `atcHost` | Integer | ATC do host |
| `tokenAssuranceLevel` | Integer | Nível de garantia do token |
| `availableCredit` | BigDecimal | Crédito disponível |
| `cardCashBalance` | BigDecimal | Saldo do cartão |
| `cardDelinquentAmount` | BigDecimal | Valor em atraso |

### 5.2 Campos Opcionais

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `merchantId` | String | ID do estabelecimento |
| `merchantName` | String | Nome do estabelecimento |
| `merchantCity` | String | Cidade do estabelecimento |
| `merchantState` | String | Estado (2 letras) |
| `merchantCountryCode` | String | Código do país (3 dígitos) |
| `merchantPostalCode` | String | CEP |
| `gmtOffset` | String | Fuso horário |
| `cryptogramValid` | String | Criptograma válido (Y/N) |
| `cvv2Response` | String | Resposta CVV2 |
| `cvv2Present` | String | CVV2 presente |
| `pinVerifyCode` | String | Código de verificação PIN |
| `cvvVerifyCode` | String | Código de verificação CVV |
| `posEntryMode` | String | Modo de entrada POS |
| `customerPresent` | String | Cliente presente (Y/N) |
| `cardMediaType` | String | Tipo de mídia do cartão |
| `tokenizationIndicator` | String | Indicador de tokenização |
| `workflow` | String | Workflow |
| `recordType` | String | Tipo de registro |
| `clientIdFromHeader` | String | ID do cliente do header |

---

## 6. Campos Derivados (DerivedContext)

Campos calculados automaticamente sem alterar o payload:

| Campo | Tipo | Descrição |
|-------|------|-----------|
| `transactionTimestamp` | OffsetDateTime | Data/hora completa |
| `bin` | String | BIN do cartão (6-8 dígitos) |
| `maskedPan` | String | PAN mascarado |
| `last4` | String | Últimos 4 dígitos |
| `normalizedCountry` | String | País normalizado |
| `normalizedState` | String | Estado normalizado |
| `normalizedPostalCode` | String | CEP normalizado |

---

## 7. Campos de Enriquecimento (EnrichmentContext)

### 7.1 BIN Enrichment
| Campo | Tipo | Descrição |
|-------|------|-----------|
| `cardBrand` | String | Bandeira (VISA, MASTERCARD, etc.) |
| `cardType` | String | Tipo (CREDIT, DEBIT, PREPAID) |
| `cardLevel` | String | Nível (CLASSIC, GOLD, PLATINUM) |
| `issuerName` | String | Nome do emissor |
| `issuerCountry` | String | País do emissor |
| `isRegulated` | Boolean | Se é regulado |
| `isCommercial` | Boolean | Se é comercial |
| `isPrepaid` | Boolean | Se é pré-pago |

### 7.2 MCC Enrichment
| Campo | Tipo | Descrição |
|-------|------|-----------|
| `mccCategory` | String | Categoria do MCC |
| `mccSubcategory` | String | Subcategoria |
| `mccDescription` | String | Descrição |
| `mccRiskLevel` | String | Nível de risco |
| `mccIsHighRisk` | Boolean | Se é alto risco |
| `mccIsGambling` | Boolean | Se é gambling |
| `mccIsCrypto` | Boolean | Se é crypto |
| `mccIsAdult` | Boolean | Se é adulto |
| `mccIsCashAdvance` | Boolean | Se é saque |

---

## 8. Tabelas do Banco de Dados

### 8.1 Regras
- `rule_configurations` - Regras simples
- `complex_rules` - Regras complexas
- `rule_condition_groups` - Grupos de condições
- `rule_conditions` - Condições individuais
- `rule_expressions` - Expressões calculadas
- `rule_context_variables` - Variáveis de contexto
- `rule_actions` - Ações

### 8.2 Transações
- `transactions` - Transações processadas
- `transaction_decisions` - Decisões
- `transaction_raw_store` - Payload raw

### 8.3 Geolocalização
- `geo_reference` - Referência de cidades/países
- `geo_polygon` - Polígonos geográficos

### 8.4 Velocidade
- `velocity_counters` - Contadores pré-computados
- `velocity_transaction_log` - Log de transações

### 8.5 Auditoria
- `audit_log` - Log de auditoria
- `decision_log` - Log de decisões

---

## 9. Validações

### 9.1 Validações de Regra
- `key` deve ser único
- `key` deve seguir padrão UPPER_SNAKE_CASE
- `title` é obrigatório
- `rootConditionGroup` é obrigatório
- `decision` é obrigatório
- Profundidade máxima de nesting: 10 níveis

### 9.2 Validações de Condição
- `fieldName` é obrigatório
- `operator` é obrigatório
- Operadores que requerem valor: todos exceto IS_NULL, NOT_NULL, IS_TRUE, IS_FALSE
- BETWEEN requer `valueMin` e `valueMax`
- IN/NOT_IN requer `valueArray` com pelo menos 1 item
- REGEX deve ser expressão válida
- FIELD_* requer `valueFieldRef`
- GEO_DISTANCE_* requer formato "lat,lon,distanceKm"
- GEO_IN_POLYGON requer nome de polígono existente

---

## 10. Exemplos de JSON

### 10.1 Regra Simples
```json
{
  "ruleName": "HIGH_AMOUNT_RULE",
  "description": "Bloqueia transações acima de R$ 10.000",
  "ruleType": "SECURITY",
  "classification": "SUSPICIOUS",
  "threshold": 1000000,
  "weight": 80,
  "enabled": true,
  "logicOperator": "AND",
  "conditions": [
    {
      "field": "transactionAmount",
      "operator": "GT",
      "value": "1000000"
    }
  ]
}
```

### 10.2 Regra Complexa
```json
{
  "key": "GAMBLING_INTERNATIONAL_NIGHT",
  "title": "Gambling Internacional à Noite",
  "description": "Detecta transações de gambling fora do Brasil durante a madrugada",
  "status": "PUBLISHED",
  "priority": 90,
  "severity": 85,
  "decision": "FRAUDE",
  "reasonTemplate": "Transação de gambling (MCC ${mcc}) no país ${merchantCountryCode} às ${transactionTime}",
  "enabled": true,
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      {
        "fieldName": "mcc",
        "operator": "IN",
        "valueType": "NUMBER",
        "valueArray": ["7995", "7994", "7993"]
      },
      {
        "fieldName": "merchantCountryCode",
        "operator": "NEQ",
        "valueType": "STRING",
        "valueSingle": "076"
      }
    ],
    "children": [
      {
        "logicOperator": "OR",
        "conditions": [
          {
            "fieldName": "transactionTime",
            "operator": "TIME_BETWEEN",
            "valueType": "TIME",
            "valueMin": "00:00:00",
            "valueMax": "06:00:00"
          },
          {
            "fieldName": "transactionAmount",
            "operator": "GT",
            "valueType": "NUMBER",
            "valueSingle": "500000"
          }
        ]
      }
    ]
  }
}
```

### 10.3 Regra com GEO
```json
{
  "key": "DISTANT_HIGH_VALUE",
  "title": "Alto Valor Distante",
  "description": "Transação de alto valor muito distante de São Paulo",
  "status": "PUBLISHED",
  "priority": 75,
  "severity": 70,
  "decision": "SUSPEITA_DE_FRAUDE",
  "enabled": true,
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      {
        "fieldName": "merchantLocation",
        "operator": "GEO_DISTANCE_GT",
        "valueType": "GEO_POINT",
        "valueSingle": "-23.5505,-46.6333,2000"
      },
      {
        "fieldName": "transactionAmount",
        "operator": "GT",
        "valueType": "NUMBER",
        "valueSingle": "1000000"
      },
      {
        "fieldName": "merchantLocation",
        "operator": "GEO_IN_POLYGON",
        "valueType": "GEO_POLYGON",
        "valueSingle": "AMERICA_DO_SUL",
        "negate": true
      }
    ]
  }
}
```
