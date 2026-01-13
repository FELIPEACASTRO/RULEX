# RULEX - Mapa de Capacidades Extremas

## Visão Geral

O RULEX é um motor de regras de fraude que suporta desde regras simples até extremamente complexas, com:
- Nesting profundo de condições (até 10 níveis)
- Todos os operadores de comparação, lógicos, temporais e geográficos
- Agregações de velocidade em tempo real
- Integração completa Front + Back + DB

---

## 1. Operadores Suportados

### 1.1 Operadores de Comparação Básica
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `EQ` | Igual a | `transactionAmount EQ 1000` |
| `NEQ` | Diferente de | `mcc NEQ 7995` |
| `GT` | Maior que | `transactionAmount GT 5000` |
| `GTE` | Maior ou igual | `riskScore GTE 70` |
| `LT` | Menor que | `consumerAuthenticationScore LT 50` |
| `LTE` | Menor ou igual | `externalScore3 LTE 30` |

### 1.2 Operadores de Lista
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `IN` | Valor está na lista | `mcc IN [7995, 6211, 6051]` |
| `NOT_IN` | Valor não está na lista | `merchantCountryCode NOT_IN [076, 840]` |

### 1.3 Operadores de Range
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `BETWEEN` | Valor entre min e max | `transactionAmount BETWEEN 1000,5000` |
| `NOT_BETWEEN` | Valor fora do range | `transactionTime NOT_BETWEEN 000000,060000` |

### 1.4 Operadores de String
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `CONTAINS` | Texto contém | `merchantName CONTAINS "CASINO"` |
| `NOT_CONTAINS` | Texto não contém | `merchantName NOT_CONTAINS "BANK"` |
| `STARTS_WITH` | Começa com | `pan STARTS_WITH "4"` |
| `ENDS_WITH` | Termina com | `merchantId ENDS_WITH "999"` |
| `REGEX` | Expressão regular | `merchantName REGEX "^(CASINO|BET|POKER).*"` |
| `NOT_REGEX` | Não corresponde à regex | `workflow NOT_REGEX "^TEST.*"` |

### 1.5 Operadores de Nulo/Booleano
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `IS_NULL` | Campo é nulo | `merchantId IS_NULL` |
| `NOT_NULL` | Campo não é nulo | `cryptogramValid NOT_NULL` |
| `IS_TRUE` | Campo é verdadeiro | `enabled IS_TRUE` |
| `IS_FALSE` | Campo é falso | `enabled IS_FALSE` |

### 1.6 Operadores de Comparação entre Campos
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `FIELD_EQ` | Igual a outro campo | `atcCard FIELD_EQ atcHost` |
| `FIELD_NEQ` | Diferente de outro campo | `transactionAmount FIELD_NEQ availableCredit` |
| `FIELD_GT` | Maior que outro campo | `transactionAmount FIELD_GT cardCashBalance` |
| `FIELD_GTE` | Maior ou igual a outro campo | - |
| `FIELD_LT` | Menor que outro campo | - |
| `FIELD_LTE` | Menor ou igual a outro campo | - |

### 1.7 Operadores Temporais
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `DATE_BEFORE` | Data anterior a | `transactionDate DATE_BEFORE 2024-12-31` |
| `DATE_AFTER` | Data posterior a | `transactionDate DATE_AFTER 2024-01-01` |
| `DATE_BETWEEN` | Data entre | `transactionDate DATE_BETWEEN 2024-01-01,2024-12-31` |
| `TIME_BEFORE` | Hora anterior a | `transactionTime TIME_BEFORE 06:00:00` |
| `TIME_AFTER` | Hora posterior a | `transactionTime TIME_AFTER 22:00:00` |
| `TIME_BETWEEN` | Hora entre | `transactionTime TIME_BETWEEN 00:00:00,06:00:00` |

### 1.8 Operadores de Array
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `ARRAY_CONTAINS` | Array contém valor | `tags ARRAY_CONTAINS "HIGH_RISK"` |
| `ARRAY_NOT_CONTAINS` | Array não contém | `tags ARRAY_NOT_CONTAINS "WHITELISTED"` |
| `ARRAY_SIZE_EQ` | Tamanho do array igual | `rules ARRAY_SIZE_EQ 5` |
| `ARRAY_SIZE_GT` | Tamanho maior que | `rules ARRAY_SIZE_GT 10` |
| `ARRAY_SIZE_LT` | Tamanho menor que | `rules ARRAY_SIZE_LT 3` |

### 1.9 Operadores Matemáticos
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `MOD_EQ` | Módulo igual a | `transactionAmount MOD_EQ 100,0` (divisível por 100) |
| `MOD_NEQ` | Módulo diferente de | `mcc MOD_NEQ 1000,0` |

### 1.10 Operadores de Geolocalização
| Operador | Descrição | Exemplo |
|----------|-----------|---------|
| `GEO_DISTANCE_LT` | Distância menor que (km) | `merchantLocation GEO_DISTANCE_LT -23.55,-46.63,100` |
| `GEO_DISTANCE_GT` | Distância maior que (km) | `merchantLocation GEO_DISTANCE_GT -23.55,-46.63,500` |
| `GEO_IN_POLYGON` | Dentro do polígono | `merchantLocation GEO_IN_POLYGON BRASIL` |

**Nota:** Coordenadas são derivadas de `merchantCity`, `merchantState`, `merchantCountryCode` via tabela `geo_reference`.

### 1.11 Operadores de Velocity (Agregações Temporais)
| Operador | Descrição | Formato do Valor |
|----------|-----------|------------------|
| `VELOCITY_COUNT_GT` | Contagem de transações maior que | `keyType,windowMinutes,threshold` |
| `VELOCITY_COUNT_LT` | Contagem de transações menor que | `keyType,windowMinutes,threshold` |
| `VELOCITY_SUM_GT` | Soma de valores maior que | `keyType,windowMinutes,threshold` |
| `VELOCITY_SUM_LT` | Soma de valores menor que | `keyType,windowMinutes,threshold` |
| `VELOCITY_AVG_GT` | Média de valores maior que | `keyType,windowMinutes,threshold` |
| `VELOCITY_AVG_LT` | Média de valores menor que | `keyType,windowMinutes,threshold` |
| `VELOCITY_DISTINCT_GT` | Valores distintos maior que | `keyType,windowMinutes,distinctType,threshold` |
| `VELOCITY_DISTINCT_LT` | Valores distintos menor que | `keyType,windowMinutes,distinctType,threshold` |

**Exemplos:**
- `VELOCITY_COUNT_GT PAN,60,5` → Mais de 5 transações do mesmo PAN na última hora
- `VELOCITY_SUM_GT PAN,1440,10000` → Soma > R$10.000 do mesmo PAN nas últimas 24h
- `VELOCITY_DISTINCT_GT PAN,1440,MERCHANTS,3` → Mais de 3 merchants distintos em 24h

**Nota:** Agregações calculadas via `velocity_transaction_log` sem alterar o payload de entrada.

---

## 2. Operadores Lógicos de Grupo

| Operador | Descrição |
|----------|-----------|
| `AND` | Todas as condições devem ser verdadeiras |
| `OR` | Pelo menos uma condição deve ser verdadeira |
| `NOT` | Inverte o resultado do grupo |
| `XOR` | Exatamente uma condição deve ser verdadeira |
| `NAND` | Pelo menos uma condição deve ser falsa |
| `NOR` | Todas as condições devem ser falsas |

---

## 3. Sistema de Velocidade/Agregação (NOVO)

### 3.1 Tipos de Agregação
| Tipo | Descrição |
|------|-----------|
| `COUNT` | Conta transações |
| `SUM` | Soma valores |
| `AVG` | Média de valores |
| `MIN` | Valor mínimo |
| `MAX` | Valor máximo |
| `DISTINCT_MERCHANTS` | Merchants distintos |
| `DISTINCT_MCCS` | MCCs distintos |
| `DISTINCT_COUNTRIES` | Países distintos |
| `FRAUD_COUNT` | Transações marcadas como fraude |

### 3.2 Chaves de Agregação
| Chave | Descrição |
|-------|-----------|
| `PAN` | Por cartão (hash SHA-256) |
| `CUSTOMER_ID` | Por cliente |
| `MERCHANT_ID` | Por estabelecimento |

### 3.3 Janelas Temporais
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

### 3.4 Exemplos de Uso
```java
// Contar transações do PAN nas últimas 24h
velocityService.countPanTransactionsInHours(request, 24);

// Somar valores do PAN na última hora
velocityService.sumPanAmountInHours(request, 1);

// Detectar burst (mais de 5 transações em 5 minutos)
velocityService.isBurst(request, 5, 5);
```

---

## 4. Campos do Payload (TransactionRequest)

### 4.1 Identificação
- `externalTransactionId` - ID único da transação
- `customerIdFromHeader` - ID do cliente
- `customerAcctNumber` - Número da conta
- `pan` - Número do cartão

### 4.2 Merchant
- `merchantId` - ID do estabelecimento
- `merchantName` - Nome do estabelecimento
- `merchantCity` - Cidade
- `merchantState` - Estado
- `merchantCountryCode` - Código do país
- `merchantPostalCode` - CEP
- `mcc` - Merchant Category Code

### 4.3 Transação
- `transactionAmount` - Valor
- `transactionCurrencyCode` - Moeda
- `transactionDate` - Data (YYYYMMDD)
- `transactionTime` - Hora (HHMMSS)
- `gmtOffset` - Fuso horário

### 4.4 Autenticação
- `consumerAuthenticationScore` - Score de autenticação
- `externalScore3` - Score externo
- `cavvResult` - Resultado CAVV
- `cryptogramValid` - Criptograma válido
- `cvv2Response` - Resposta CVV2
- `eciIndicator` - Indicador ECI

### 4.5 Financeiro
- `availableCredit` - Crédito disponível
- `cardCashBalance` - Saldo
- `cardDelinquentAmount` - Valor em atraso

---

## 5. Exemplos de Regras Extremas

### 5.1 Regra com Nesting Profundo
```json
{
  "key": "EXTREME_NESTED_RULE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "GT", "valueSingle": "1000" }
    ],
    "children": [
      {
        "logicOperator": "OR",
        "conditions": [
          { "fieldName": "mcc", "operator": "IN", "valueArray": ["7995", "6211"] }
        ],
        "children": [
          {
            "logicOperator": "AND",
            "conditions": [
              { "fieldName": "merchantCountryCode", "operator": "NEQ", "valueSingle": "076" },
              { "fieldName": "transactionTime", "operator": "TIME_BETWEEN", "valueMin": "00:00:00", "valueMax": "06:00:00" }
            ]
          }
        ]
      }
    ]
  }
}
```

### 5.2 Regra com GEO
```json
{
  "key": "GEO_DISTANCE_RULE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "merchantLocation", "operator": "GEO_DISTANCE_GT", "valueSingle": "-23.55,-46.63,1000" },
      { "fieldName": "transactionAmount", "operator": "GT", "valueSingle": "5000" }
    ]
  },
  "decision": "SUSPEITA_DE_FRAUDE",
  "reasonTemplate": "Transação de alto valor a mais de 1000km de São Paulo"
}
```

### 5.3 Regra com Comparação entre Campos
```json
{
  "key": "FIELD_COMPARISON_RULE",
  "rootConditionGroup": {
    "logicOperator": "AND",
    "conditions": [
      { "fieldName": "transactionAmount", "operator": "FIELD_GT", "valueFieldRef": "availableCredit" },
      { "fieldName": "atcCard", "operator": "FIELD_NEQ", "valueFieldRef": "atcHost" }
    ]
  },
  "decision": "FRAUDE"
}
```

---

## 6. Limitações e Fallbacks

### 6.1 GEO
- Coordenadas derivadas de merchantCity/State/CountryCode
- Se cidade não encontrada, usa capital do estado
- Se estado não encontrado, usa capital do país
- Polígonos são bounding boxes simplificados

### 6.2 Velocity
- Agregações calculadas em tempo real via DB
- Cache in-memory para performance
- PAN armazenado como hash SHA-256

### 6.3 Nesting
- Máximo de 10 níveis de profundidade
- Grupos desabilitados são ignorados

---

## 7. Endpoints da API

### 7.1 Regras Simples
- `GET /api/rules` - Listar regras
- `POST /api/rules` - Criar regra
- `PUT /api/rules/{id}` - Atualizar regra
- `DELETE /api/rules/{id}` - Deletar regra

### 7.2 Regras Complexas
- `GET /api/v1/complex-rules` - Listar regras complexas
- `POST /api/v1/complex-rules` - Criar regra complexa
- `PUT /api/v1/complex-rules/{id}` - Atualizar
- `DELETE /api/v1/complex-rules/{id}` - Deletar
- `POST /api/v1/complex-rules/validate` - Validar sem salvar
- `POST /api/v1/complex-rules/{id}/duplicate` - Duplicar

### 7.3 Avaliação
- `POST /api/evaluate` - Avaliar transação
- `POST /api/transactions/analyze` - Analisar transação

### 7.4 Simulação
- `POST /api/simulation/run` - Executar simulação
- `GET /api/simulation/results/{id}` - Resultados

---

## 8. Referências

- [Backend: ComplexRuleEvaluator.java](../backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java)
- [Backend: GeoService.java](../backend/src/main/java/com/rulex/service/GeoService.java)
- [Backend: VelocityService.java](../backend/src/main/java/com/rulex/service/VelocityService.java)
- [Frontend: ComplexRuleBuilder](../client/src/components/ComplexRuleBuilder/)
- [DB: V13__geo_reference_table.sql](../backend/src/main/resources/db/migration/V13__geo_reference_table.sql)
- [DB: V14__velocity_counters.sql](../backend/src/main/resources/db/migration/V14__velocity_counters.sql)
