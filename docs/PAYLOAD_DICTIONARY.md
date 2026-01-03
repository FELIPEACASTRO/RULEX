# PAYLOAD_DICTIONARY (FASE 0)

Objetivo: documentar o **contrato de entrada** usado por `/transactions/analyze` e a base do catálogo (`field_dictionary`).

## 1) Contrato de entrada: TransactionRequest

O backend valida o payload via Bean Validation (`@NotBlank`, `@NotNull`, etc.). Campos “obrigatórios” são aqueles anotados com constraints.

### 1.1 Campos obrigatórios (evidência direta)

Conjunto mínimo observado como obrigatório no trecho lido:
- `externalTransactionId` (String, not blank)
- `customerIdFromHeader` (String, not blank)
- `customerAcctNumber` (Long, not null)
- `pan` (String, not blank)
- `transactionCurrencyCode` (Integer, not null)
- `transactionAmount` (BigDecimal, not null, > 0)
- `transactionDate` (Integer, not null)
- `transactionTime` (Integer, not null)
- `mcc` (Integer, not null)
- `consumerAuthenticationScore` (Integer, not null)
- `externalScore3` (Integer, not null)
- `cavvResult` (Integer, not null)
- `eciIndicator` (Integer, not null)
- `atcCard` (Integer, not null)
- `atcHost` (Integer, not null)
- `tokenAssuranceLevel` (Integer, not null)
- `availableCredit` (BigDecimal, not null)
- `cardCashBalance` (BigDecimal, not null)
- `cardDelinquentAmount` (BigDecimal, not null)

**EVIDÊNCIA**
- Definições e validações: [backend/src/main/java/com/rulex/dto/TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java#L15-L60)
- Valores obrigatórios adicionais no fim do DTO (mcc/3DS/ATC/limits): [backend/src/main/java/com/rulex/dto/TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java#L260-L351)

### 1.2 Campos opcionais (não exaustivo)

O DTO contém muitos campos adicionais (merchant/terminal/tokenization/userData/etc.) sem constraints, que portanto podem ser omitidos.

**EVIDÊNCIA**
- Lista parcial de campos opcionais no corpo do DTO: [backend/src/main/java/com/rulex/dto/TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java#L60-L200)

## 2) Captura do payload “as received” (bytes crus)

O controller de transações lê bytes crus do request para permitir:
- Hash SHA-256 do payload como recebido
- Persistência do payload raw
- Anti-tamper (mesmo `externalTransactionId` com hash diferente)

**EVIDÊNCIA**
- Captura de bytes crus no controller: [backend/src/main/java/com/rulex/controller/TransactionController.java](../backend/src/main/java/com/rulex/controller/TransactionController.java#L32-L49)
- Uso do hash + anti-tamper/idempotência: [backend/src/main/java/com/rulex/service/RuleEngineService.java](../backend/src/main/java/com/rulex/service/RuleEngineService.java#L88-L170)

## 3) Field Dictionary (catálogo de campos para o FE)

### 3.1 Schema

`field_dictionary` registra campos “catalogáveis” com:
- `json_path` (ex: `$.externalTransactionId`)
- `data_type` (string/number/boolean/unknown)
- `allowed_operators` e `allowed_functions`
- `domain_json`, `sentinel_values_json`, `security_constraints`, `requiredness_by_context`

**EVIDÊNCIA**
- Tabela `field_dictionary`: [backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql](../backend/src/main/resources/db/migration/V6__v31_exec_log_field_dictionary.sql#L18-L51)

### 3.2 Seeder: como o catálogo é derivado

- O seeding é “best-effort” no startup.
- Deriva o catálogo por reflexão do DTO `TransactionRequest` e `@JsonProperty`.
- Defaults:
  - workflow: `BRZLCREDIT`
  - recordType: `CRTRAN25`
  - portfolio: `*`
- `allowedOperators` e `allowedFunctions` variam por `dataType`.
- `securityConstraints`: marca `pan` e campos contendo `paymentInstrument` como PCI (`neverLog=true`).

**EVIDÊNCIA**
- Seeder: [backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java](../backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java#L1-L155)

### 3.3 API: como o FE consome

- `GET /api/field-dictionary` retorna shape estável e parseia colunas JSONB para JSON nodes.

**EVIDÊNCIA**
- Controller: [backend/src/main/java/com/rulex/v31/field/FieldDictionaryController.java](../backend/src/main/java/com/rulex/v31/field/FieldDictionaryController.java#L1-L80)

## 4) Pontos a validar

- **VALIDAR** se há outros DTOs/recordTypes além de `CRTRAN25` em runtime.
- **VALIDAR** se existe uma “contract versioning” formal além de `refdata_versions` (migrations V6) — não há evidência adicional aqui.
