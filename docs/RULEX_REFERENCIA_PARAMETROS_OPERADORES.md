# RULEX — Referência Completa de Parâmetros, Operadores, Funções e Saídas

> Objetivo: documentar **todos os parâmetros de entrada**
> (payloads e query params), **todos os operadores/operações/funções** disponíveis
> na modelagem de regras e **como os parâmetros de saída são gerados**.
>
> Fonte: `openapi/rulex.yaml` + implementação Java
> (motor padrão `RuleEngineService`, motor avançado `AdvancedRuleEngineService`
> e AST V3.1 em `com.rulex.v31.ast`).

---

## 1) Endpoints e Entradas (API)

### 1.1) POST `/api/transactions/analyze`
Analisa uma transação usando regras configuráveis (motor padrão).

**Body:** `AnalyzeTransactionRequest` (ver seção 2)

**Response 200:** `AnalyzeTransactionResponse` (ver seção 6)

### 1.2) POST `/api/transactions/analyze-advanced`
Analisa uma transação usando as **28 regras avançadas (hard rules)**.

**Body:** `AnalyzeTransactionRequest` (ver seção 2)

**Response 200:** `AnalyzeTransactionResponse` (ver seção 6).

Há diferenças no cálculo do score/versão no modo avançado (ver seção 6.3).

### 1.3) GET `/api/transactions`
Lista transações com filtros.

**Query params:**
- `customerId` (string)
- `merchantId` (string)
- `classification` (enum: `APPROVED | SUSPICIOUS | FRAUD`)
- `mcc` (integer)
- `minAmount` (number)
- `maxAmount` (number)
- `startDate` (string, `date-time`)
- `endDate` (string, `date-time`)
- `page` (integer, default `0`)
- `size` (integer, default `20`)

**Response 200:** `PageAnalyzeTransactionResponse`

### 1.4) GET `/api/transactions/export`
Exporta transações.

**Query params:**
- `format` (enum: `csv | json`, default `csv`)
- `customerId` (string)
- `merchantId` (string)
- `classification` (enum: `APPROVED | SUSPICIOUS | FRAUD`)
- `mcc` (integer)
- `minAmount` (number)
- `maxAmount` (number)
- `startDate` (string, `date-time`)
- `endDate` (string, `date-time`)
- `limit` (integer, default `10000`, min `1`, max `50000`)

**Response 200:**
- `text/csv` (string)
- ou `application/json` (array de `AnalyzeTransactionResponse`)

### 1.5) CRUD de regras — `/api/rules`

#### GET `/api/rules`
Lista regras.

**Query params:**
- `page` (integer, default `0`)
- `size` (integer, default `20`)

**Response 200:** `PageRule`

#### POST `/api/rules`
Cria regra.

**Body:** `Rule`

**Response 201:** `Rule`

#### GET `/api/rules/{id}`
Obtém regra.

**Path params:**
- `id` (int64)

**Response 200:** `Rule`

#### PUT `/api/rules/{id}`
Atualiza regra.

**Path params:**
- `id` (int64)

**Body:** `Rule`

**Response 200:** `Rule`

#### DELETE `/api/rules/{id}`
Remove regra.

**Path params:**
- `id` (int64)

**Response 204:** sem body

#### PATCH `/api/rules/{id}/toggle`
Alterna `enabled`.

**Path params:**
- `id` (int64)

**Response 200:** `Rule`

### 1.6) GET `/api/transactions/{id}`
Obtém transação por ID interno.

**Path params:**
- `id` (int64)

**Response 200:** `AnalyzeTransactionResponse`

### 1.7) GET `/api/transactions/external/{externalId}`
Obtém transação por ID externo.

**Path params:**
- `externalId` (string)

**Response 200:** `AnalyzeTransactionResponse`

### 1.8) GET `/api/audit`
Lista eventos de auditoria.

**Query params:**
- `actionType` (string)
- `result` (string)
- `startDate` (string, `date-time`)
- `endDate` (string, `date-time`)
- `page` (integer, default `0`)
- `size` (integer, default `20`)

**Response 200:** `PageAuditLog`

### 1.9) GET `/api/audit/export`
Exporta auditoria.

**Query params:**
- `format` (enum: `csv | json`, default `csv`)
- `actionType` (string)
- `result` (string)
- `startDate` (string, `date-time`)
- `endDate` (string, `date-time`)
- `limit` (integer, default `10000`, min `1`, max `50000`)

**Response 200:**
- `text/csv` (string)
- ou `application/json` (array de `AuditLog`)

### 1.10) GET `/api/metrics`
Métricas gerais do sistema.

**Query params:**
- `period` (string, exemplo: `24h`)

**Response 200:** `Metrics`

### 1.11) GET `/api/metrics/mcc`
Métricas agrupadas por MCC.

**Query params:**
- `period` (string, exemplo: `24h`)

**Response 200:** `object` (map/objeto)

### 1.12) GET `/api/metrics/merchant`
Métricas agrupadas por merchant.

**Query params:**
- `period` (string, exemplo: `24h`)

**Response 200:** `object` (map/objeto)

### 1.13) GET `/api/metrics/timeline`
Métricas em série temporal.

**Query params:**
- `granularity` (string, exemplo: `hour`)

**Response 200:** `object` (map/objeto)

---

## 2) Parâmetros de Entrada — `AnalyzeTransactionRequest`

### 2.1) Campos obrigatórios (required)
- `externalTransactionId` (string)
- `customerIdFromHeader` (string)
- `customerAcctNumber` (int64)
- `pan` (string)
- `transactionAmount` (number)
- `transactionDate` (integer, `YYYYMMDD`)
- `transactionTime` (integer, `HHMMSS`)
- `transactionCurrencyCode` (integer)
- `mcc` (integer)
- `consumerAuthenticationScore` (integer)
- `externalScore3` (integer)
- `cavvResult` (integer)
- `eciIndicator` (integer)
- `atcCard` (integer)
- `atcHost` (integer)
- `tokenAssuranceLevel` (integer)
- `availableCredit` (number)
- `cardCashBalance` (number)
- `cardDelinquentAmount` (number)

### 2.2) Todos os campos (properties) com tipo
> Observação: `pan` é descrito como “Tokenizado/mascarado;
> nunca armazenar PAN em claro.”

| Campo | Tipo | Observações/Formato |
| --- | ---: | --- |
| `workflow` | string | |
| `recordType` | string | |
| `dataSpecificationVersion` | number | |
| `clientIdFromHeader` | string | |
| `externalTransactionId` | string | **Obrigatório** |
| `customerIdFromHeader` | string | **Obrigatório** |
| `customerAcctNumber` | int64 | **Obrigatório** |
| `pan` | string | **Obrigatório** (tokenizado/mascarado) |
| `merchantId` | string | |
| `merchantName` | string | |
| `transactionAmount` | number | **Obrigatório** |
| `transactionDate` | integer | **Obrigatório** (`YYYYMMDD`) |
| `transactionTime` | integer | **Obrigatório** (`HHMMSS`) |
| `gmtOffset` | string | Ex.: `-03.00` |
| `transactionCurrencyCode` | integer | **Obrigatório** |
| `transactionCurrencyConversionRate` | number | |
| `merchantCountryCode` | string | |
| `merchantCity` | string | |
| `merchantState` | string | |
| `merchantPostalCode` | string | |
| `mcc` | integer | **Obrigatório** |
| `posEntryMode` | string | |
| `customerPresent` | string | |
| `authPostFlag` | string | |
| `authDecisionCode` | string | |
| `authResponseCode` | string | |
| `authId` | string | |
| `authIndicator` | integer | |
| `processorAuthReasonCode` | string | |
| `standinAdvice` | string | |
| `transactionType` | string | |
| `transactionCategory` | string | |
| `consumerAuthenticationScore` | integer | **Obrigatório** |
| `externalScore3` | integer | **Obrigatório** |
| `cavvResult` | integer | **Obrigatório** |
| `cavvKeyIndicator` | integer | |
| `secondFactorAuthCode` | string | |
| `cryptogramValid` | string | |
| `cvv2Response` | string | |
| `cvv2Present` | string | |
| `pinVerifyCode` | string | |
| `cvvVerifyCode` | string | |
| `cvrofflinePinVerificationPerformed` | integer | |
| `cvrofflinePinVerificationFailed` | integer | |
| `cvvPinTryLimitExceeded` | integer | |
| `eciIndicator` | integer | **Obrigatório** |
| `atcCard` | integer | **Obrigatório** |
| `atcHost` | integer | **Obrigatório** |
| `tokenAssuranceLevel` | integer | **Obrigatório** |
| `tokenizationIndicator` | string | |
| `tokenId` | string | |
| `tokenRequestorId` | string | |
| `paymentInstrumentId` | string | |
| `availableCredit` | number | **Obrigatório** |
| `cardCashBalance` | number | **Obrigatório** |
| `cardDelinquentAmount` | number | **Obrigatório** |
| `cardSeqNum` | integer | nullable |
| `cardExpireDate` | integer | |
| `cardMediaType` | string | |
| `cardAipStatic` | string | |
| `cardAipDynamic` | string | |
| `cardAipVerify` | string | |
| `cardAipRisk` | string | |
| `cardAipIssuerAuthentication` | string | |
| `cardAipCombined` | string | |
| `terminalId` | string | |
| `terminalType` | string | |
| `terminalEntryCapability` | string | |
| `posConditionCode` | string | |
| `posOffPremises` | integer | |
| `posCardCapture` | integer | |
| `posSecurity` | integer | |
| `terminalVerificationResults` | string | |
| `cardVerificationResults` | string | |
| `networkId` | string | |
| `atmOwner` | string | |
| `acquirerId` | string | |
| `acquirerCountry` | string | |
| `acquirerBin` | string | nullable |
| `expandedBIN` | string | |
| `tranCode` | string | |
| `avsRequest` | string | |
| `checkNumber` | string | |
| `recordCreationDate` | integer | |
| `recordCreationTime` | integer | |
| `recordCreationMilliseconds` | integer | |
| `portfolio` | string | |
| `onUsMerchantId` | string | |
| `userIndicator01` | string | |
| `userIndicator03` | string | |
| `userIndicator04` | string | |
| `userIndicator05` | string | |
| `userIndicator08` | string | |
| `idMethod` | integer | |
| `userData01` | string | |
| `userData02` | string | |
| `userData03` | string | |
| `userData04` | string | |
| `userData05` | string | |
| `userData06` | string | |
| `userData06_2` | string | |
| `userData09` | string | |

---

## 3) Parâmetros de Entrada — `Rule` (CRUD /api/rules)

Schema (OpenAPI):

| Campo | Tipo | Obrigatório | Regras/Notas |
| --- | ---: | :---: | --- |
| `id` | int64 | não | definido pelo backend |
| `ruleName` | string | sim | nome chave da regra |
| `description` | string | não | |
| `ruleType` | enum | sim | `SECURITY, CONTEXT, VELOCITY, ANOMALY` |
| `weight` | int (0..100) | sim | usado para contribuição no `riskScore` |
| `threshold` | int (0..100) | sim | Risk score threshold (ver 6.2) |
| `enabled` | boolean | sim | habilita/desabilita |
| `classification` | enum | sim | `APPROVED, SUSPICIOUS, FRAUD, UNKNOWN` |
| `conditions` | array | sim | lista de `RuleCondition` |
| `logicOperator` | enum | sim | `AND` ou `OR` (para `conditions`) |
| `version` | integer | não | controle de versão |
| `createdAt` | date-time | não | |
| `updatedAt` | date-time | não | |

---

## 4) Parâmetros de Entrada — `RuleCondition`

Schema (OpenAPI):

| Campo | Tipo | Obrigatório | Significado |
| --- | ---: | :---: | --- |
| `field` | string | sim | Campo do payload ou expressão/função (ver seção 5) |
| `operator` | enum string | sim | Operador (ver seção 4.1) |
| `value` | string | sim | Valor serializado em string (ver seção 4.2) |

### 4.1) Operadores (OpenAPI — enum completo)

#### Comparação básica
- `EQ` (igual)
- `NEQ` (diferente)
- `GT` (maior)
- `GTE` (maior ou igual)
- `LT` (menor)
- `LTE` (menor ou igual)

#### Listas
- `IN`
- `NOT_IN`

#### Strings
- `CONTAINS`
- `NOT_CONTAINS`
- `STARTS_WITH`
- `ENDS_WITH`
- `REGEX`
- `NOT_REGEX`

#### Nulos
- `IS_NULL`
- `NOT_NULL`

#### Booleanos
- `IS_TRUE`
- `IS_FALSE`

#### Range
- `BETWEEN`
- `NOT_BETWEEN`

#### Comparação entre campos
- `FIELD_EQ`
- `FIELD_NEQ`
- `FIELD_GT`
- `FIELD_GTE`
- `FIELD_LT`
- `FIELD_LTE`

#### Data/Hora
- `DATE_BEFORE`
- `DATE_AFTER`
- `DATE_BETWEEN`
- `TIME_BEFORE`
- `TIME_AFTER`
- `TIME_BETWEEN`

#### Array
- `ARRAY_CONTAINS`
- `ARRAY_NOT_CONTAINS`
- `ARRAY_SIZE_EQ`
- `ARRAY_SIZE_GT`
- `ARRAY_SIZE_LT`

#### Matemática
- `MOD_EQ`
- `MOD_NEQ`

#### Geolocalização
- `GEO_DISTANCE_LT`
- `GEO_DISTANCE_GT`
- `GEO_IN_POLYGON`

#### Velocity
- `VELOCITY_COUNT_GT`
- `VELOCITY_COUNT_LT`
- `VELOCITY_SUM_GT`
- `VELOCITY_SUM_LT`
- `VELOCITY_AVG_GT`
- `VELOCITY_AVG_LT`
- `VELOCITY_DISTINCT_GT`
- `VELOCITY_DISTINCT_LT`

#### Aliases “legados” aceitos (documentados no OpenAPI)
- `==`, `!=`, `>`, `<`, `>=`, `<=`
- `NE`
- `MATCHES_REGEX`
- `IS_NOT_NULL`

> Importante (prático): o motor padrão normaliza `== != > < >= <=`
> para `EQ NE GT LT GTE LTE`. Para *NOT NULL*, na prática o nome mais
> aceito no backend é `IS_NOT_NULL`.

### 4.2) Formatos do `value` (string)

#### Números
- Ex.: `"100"`, `"10000"`, `"10.5"`

#### Listas (IN / NOT_IN)
O motor padrão aceita múltiplos formatos:
- CSV simples: `"7995,6211,6051"`
- JSON-like: `"[7995, 6211]"`
- Com aspas: `"['RU','CN']"` ou `"[\"RU\",\"CN\"]"`

#### BETWEEN / NOT_BETWEEN
- Lista com 2 itens: `"10,20"`
- Intervalo com `..`: `"10..20"`

#### REGEX
- Padrão diretamente (sem barras): `"^ABC[0-9]+$"`

---

## 5) Funções e Operações (como o motor interpreta `field` e expressões)

O RULEX tem **dois formatos principais** de avaliação de condições,
dependendo do “motor” usado:

1) **Motor padrão** (regras de `/api/rules`, avaliadas em
  `RuleEngineService.evaluateCondition`) — simples e direto.
2) **AST V3.1** (motor determinístico em `com.rulex.v31.ast`) —
  mais estruturado, com nós `FIELD/FUNC/CONST` e validação de segurança.

### 5.1) Motor padrão — expressões permitidas em `RuleCondition.field`
Além de `field` ser um nome de propriedade do payload
(`transactionAmount`, `mcc`, etc.), o backend aceita algumas
**funções no lado esquerdo (LHS)**.

#### Funções unárias
Formato: `FN(campo)` onde `campo` é uma propriedade de `TransactionRequest`.
- `ABS(field)` → valor absoluto (BigDecimal)
- `LEN(field)` → tamanho em caracteres (int)
- `LOWER(field)` → lowercase
- `UPPER(field)` → uppercase
- `TRIM(field)` → trim

Ex.: `ABS(transactionAmount)`

#### ABS com expressão mínima
Formato: `ABS(expr)` onde `expr` pode ser `campo - campo`.
- Ex.: `ABS(atcCard - atcHost)`

#### Diferença absoluta entre dois campos
- `ABS_DIFF(fieldA, fieldB)`

#### Coalesce (fallback para literal)
- `COALESCE(field, literal)`
  - `literal` pode ser `'texto'` ou `"texto"`

Ex.: `COALESCE(merchantCountryCode, '076')`

> Observação: este avaliador numérico é propositalmente limitado
> (focado em suportar padrões reais usados no YAML/export).

### 5.2) Motor padrão — operadores realmente implementados na avaliação genérica
Apesar do OpenAPI listar um enum amplo, o método
`RuleEngineService.evaluateCondition()` implementa (no formato atual)
principalmente:

- Unários: `IS_NULL`, `IS_NOT_NULL`, `IS_TRUE`, `IS_FALSE`
- Numéricos: `EQ`, `NE`, `GT`, `GTE`, `LT`, `LTE`, `IN`, `NOT_IN`, `BETWEEN`, `NOT_BETWEEN`
- Strings: `EQ`, `NE`, `CONTAINS`, `NOT_CONTAINS`, `STARTS_WITH`,
  `ENDS_WITH`, `MATCHES_REGEX`, `IN`, `NOT_IN`

E faz normalização de operadores simbólicos:
- `== → EQ`, `!= → NE`, `> → GT`, `< → LT`, `>= → GTE`, `<= → LTE`

### 5.3) AST V3.1 — formato, operadores e funções

#### Tipos de nó
- `GROUP`: `{ "type": "GROUP", "op": "AND|OR|NOT", "children": [ ... ] }`
- `CONDITION`:
  `{ "type": "CONDITION", "left": <expr>, "operator": "...",`
  `"right": <expr|valor> }`
- `FIELD`:
  `{ "type": "FIELD", "jsonPath": "$.campo", "dataType": "STRING|NUMBER|..." }`
- `CONST`: `{ "type": "CONST", "value": <literal> }`
- `FUNC`: `{ "type": "FUNC", "name": "...", "args": [ <expr|literal>, ... ] }`

#### Operadores do AST (canônicos)
- Comparação: `EQ`, `NE`, `GT`, `GTE`, `LT`, `LTE`
- Listas: `IN`, `NOT_IN`
- Range: `BETWEEN`, `NOT_BETWEEN` (usa objeto `{min:..., max:...}`)
- Strings: `CONTAINS`, `NOT_CONTAINS`, `STARTS_WITH`, `ENDS_WITH`, `MATCHES_REGEX`
- Unários: `IS_NULL`, `IS_NOT_NULL`, `IS_TRUE`, `IS_FALSE`
- Datas (comparação de date): `BEFORE`, `AFTER`, `ON`, `NOT_ON`

#### Aliases normalizados pelo validador
- `NEQ` → `NE`
- `REGEX` → `MATCHES_REGEX`

#### Funções permitidas (allowlist)
- `TRIM`, `LOWER`, `UPPER`, `LEN`, `ABS`, `COALESCE`
- `TO_DATE_YYYYMMDD` (limpa não-dígitos e retorna int `YYYYMMDD`)
- `TO_TIME_PAD6_HHMMSS` (limpa não-dígitos e pad-left para 6 dígitos)
- `PARSE_GMTOFFSET` (normaliza offsets no formato `+HH.MM` / `-HH.MM`)

#### Limites de segurança do AST (validação)
- profundidade máxima (default): `20`
- nós máximos (default): `500`
- itens máximos em `IN` (default): `200`
- tamanho máximo de regex (default): `128`

---

## 6) Parâmetros de Saída — `AnalyzeTransactionResponse` (e como são gerados)

Schema (OpenAPI):

| Campo | Tipo | Como é gerado |
| --- | ---: | --- |
| `id` | int64 | ID interno (DB). Opcional. |
| `transactionId` | string | Igual ao `externalTransactionId` do request |
| `customerIdFromHeader` | string | ecoado do request (quando disponível). |
| `merchantId` | string | ecoado do request (quando disponível). |
| `merchantName` | string | ecoado do request (quando disponível). |
| `transactionAmount` | number | ecoado do request (quando disponível). |
| `transactionDate` | integer | ecoado do request. |
| `transactionTime` | integer | ecoado do request. |
| `classification` | enum | Decisão final: `APPROVED, SUSPICIOUS, FRAUD` |
| `riskScore` | int (0..100) | score agregado (0..100), ver 6.2/6.3 |
| `triggeredRules` | array | regras acionadas (ver 6.1) |
| `reason` | string | texto sintético gerado pelo backend |
| `rulesetVersion` | string | conjunto de regras (ex.: advanced, contract) |
| `processingTimeMs` | int64 | tempo decorrido da execução do endpoint |
| `timestamp` | date-time | timestamp de geração da resposta |
| `success` | boolean | `true` no fluxo normal; varia em falhas controladas |

### 6.1) `triggeredRules` (estrutura)
Cada item (`TriggeredRule`) tem:
- `name`: nome da regra
- `weight`: peso configurado (0..100)
- `contribution`: contribuição aplicada ao score (0..100)
- `detail`: string livre com detalhe/explicação

### 6.2) Como o motor padrão gera `riskScore`, `classification`, `reason`

#### 6.2.1) Geração do `riskScore`
No motor padrão (`RuleEngineService.evaluateRules`):
- Para cada regra habilitada que **disparar**, soma
  `contribution = clamp(weight)` ao total.
- Ao final, faz `riskScore = min(totalScore, 100)`.

> Nota: `threshold` existe no schema de regra, mas **a decisão final
> (classification) não é baseada no score** no fluxo V3.1 — o score é telemetria.

#### 6.2.2) Geração da `classification`
- A classificação final é a **maior severidade** dentre as regras disparadas.
- Existe short-circuit: se atingir `FRAUD`, o motor para cedo.

#### 6.2.3) `reason`
- Texto construído pelo backend com base na classificação e nas regras acionadas
  (ex.: concatenação de explicações por condição e/ou sumarização por severidade).

### 6.3) Motor avançado (28 hard rules) — diferenças principais
No endpoint `/api/transactions/analyze-advanced`:
- A classificação vem de `AdvancedRuleEngineService.executeAllAdvancedRulesDetailed()`.
- O `riskScore` é mapeado por severidade fixa:
  - `FRAUD` → `90`
  - `SUSPICIOUS` → `60`
  - `APPROVED` → `10`
- `rulesetVersion` = `"advanced"`
- `reason` inclui a lista de regras acionadas quando houver.

### 6.4) Anti-tamper e idempotência (impacto na saída)
No motor padrão (`/analyze`):
- O backend calcula hash SHA-256 do payload **raw** recebido
  (quando disponível) e persiste.
- Se receber novamente o mesmo `externalTransactionId`:
  - se o hash for igual → retorna a **decisão anterior** (idempotência)
  - se o hash for diferente → registra tentativa e retorna decisão **FRAUD** (anti-tamper)

### 6.5) Outras saídas do OpenAPI (páginas, audit, metrics)

#### `PageAnalyzeTransactionResponse` (lista paginada de transações)
- `content`: array de `AnalyzeTransactionResponse`
- `totalElements`: int64
- `totalPages`: int
- `size`: int
- `number`: int

#### `PageRule` (lista paginada de regras)
- `content`: array de `Rule`
- `totalElements`: int64
- `totalPages`: int
- `size`: int
- `number`: int

#### `PageAuditLog` (lista paginada de auditoria)
- `content`: array de `AuditLog`
- `totalElements`: int64
- `totalPages`: int
- `size`: int
- `number`: int

#### `AuditLog`
- `id` (int64)
- `transactionId` (int64)
- `actionType` (string)
- `description` (string)
- `details` (string)
- `performedBy` (string)
- `result` (string)
- `errorMessage` (string)
- `sourceIp` (string)
- `createdAt` (date-time)

#### `Metrics`
- `totalTransactions` (int64)
- `approvedTransactions` (int64)
- `suspiciousTransactions` (int64)
- `fraudTransactions` (int64)
- `approvalRate` (number)
- `suspiciousRate` (number)
- `fraudRate` (number)
- `period` (string)
- `timestamp` (date-time)

---

## 7) Exemplos

### 7.1) Exemplo mínimo de request (campos obrigatórios)
```json
{
  "externalTransactionId": "tx-123",
  "customerIdFromHeader": "cust-1",
  "customerAcctNumber": 1234567890,
  "pan": "411111******1111",
  "transactionAmount": 120.50,
  "transactionDate": 20260102,
  "transactionTime": 235959,
  "transactionCurrencyCode": 986,
  "mcc": 5411,
  "consumerAuthenticationScore": 250,
  "externalScore3": 260,
  "cavvResult": 0,
  "eciIndicator": 5,
  "atcCard": 10,
  "atcHost": 10,
  "tokenAssuranceLevel": 60,
  "availableCredit": 5000,
  "cardCashBalance": 0,
  "cardDelinquentAmount": 0
}
```

### 7.2) Exemplo de condição usando função no LHS
- `field`: `ABS(atcCard - atcHost)`
- `operator`: `GT`
- `value`: `"5"`

### 7.3) Exemplo de resposta
```json
{
  "transactionId": "tx-123",
  "classification": "SUSPICIOUS",
  "riskScore": 60,
  "triggeredRules": [
    {
      "name": "SUSPICIOUS_MERCHANT_POSTAL",
      "weight": 50,
      "contribution": 50,
      "detail": "advanced"
    }
  ],
  "reason": "Resultado de regras avançadas. Regras acionadas: SUSPICIOUS_MERCHANT_POSTAL",
  "rulesetVersion": "advanced",
  "processingTimeMs": 12,
  "timestamp": "2026-01-02T12:00:00Z",
  "success": true
}
```

---

## 8) Observações de compatibilidade (importante)

- O OpenAPI enumera um conjunto amplo de operadores em `RuleCondition.operator`,
  mas o motor padrão (condições simples) implementa um subconjunto.
  Operadores adicionais podem existir em outros fluxos
  (ex.: AST V3.1, regras avançadas, motor de velocity por persistência).
- Para `NOT NULL`, o backend do motor padrão reconhece `IS_NOT_NULL` como canônico.
- Para regex, o backend do motor padrão usa `MATCHES_REGEX` como canônico.

Se você quiser, eu também posso gerar uma versão “pronta para o time de negócio”
com:
- matriz **Campo x Operadores permitidos**,
- exemplos por categoria (CP, CNP, ATO, cashout, triangulação),
- e validações/caveats por tipo de campo.

