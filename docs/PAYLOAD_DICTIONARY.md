# RULEX - Dicionário de Campos do Payload

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Fonte:** `TransactionRequest.java` + `field_dictionary` (DB)

---

## 1. Visão Geral

Este documento lista **TODOS os campos disponíveis** no payload de transação que podem ser usados em regras. Campos que NÃO existem aqui **NÃO PODEM** ser usados em regras.

### 1.1 Workflow Padrão
- **Workflow:** `BRZLCREDIT`
- **Record Type:** `CRTRAN25`
- **Portfolio:** `*` (todos)

---

## 2. Campos de Identificação

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `externalTransactionId` | String | ✅ | ID único da transação | `"TXN-2024-001"` |
| `customerIdFromHeader` | String | ✅ | ID do cliente | `"CUST-12345"` |
| `customerAcctNumber` | Long | ✅ | Número da conta | `1234567890` |
| `pan` | String | ✅ | Número do cartão (PCI) | `"4111111111111111"` |
| `clientIdFromHeader` | String | ❌ | ID do cliente (header) | `"CLIENT-001"` |

---

## 3. Campos do Merchant (Estabelecimento)

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `merchantId` | String | ❌ | ID do estabelecimento | `"MERCH-001"` |
| `merchantName` | String | ❌ | Nome do estabelecimento | `"LOJA ABC"` |
| `merchantCity` | String | ❌ | Cidade | `"SAO PAULO"` |
| `merchantState` | String | ❌ | Estado (2 letras) | `"SP"` |
| `merchantCountryCode` | String | ❌ | Código do país (ISO 3166) | `"076"` (Brasil) |
| `merchantPostalCode` | String | ❌ | CEP | `"01310-100"` |
| `mcc` | Integer | ✅ | Merchant Category Code | `5411` (supermercado) |
| `onUsMerchantId` | String | ❌ | ID interno do merchant | `"ONUS-001"` |

---

## 4. Campos da Transação

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `transactionAmount` | BigDecimal | ✅ | Valor (centavos) | `150000` (R$1.500,00) |
| `transactionCurrencyCode` | Integer | ✅ | Código da moeda (ISO 4217) | `986` (BRL) |
| `transactionDate` | Integer | ✅ | Data (YYYYMMDD) | `20241231` |
| `transactionTime` | Integer | ✅ | Hora (HHMMSS) | `143052` |
| `gmtOffset` | String | ❌ | Fuso horário | `"-03.00"` |
| `transactionCurrencyConversionRate` | BigDecimal | ❌ | Taxa de conversão | `5.25` |
| `transactionType` | String | ❌ | Tipo de transação | `"PURCHASE"` |
| `transactionCategory` | String | ❌ | Categoria | `"RETAIL"` |

---

## 5. Campos de Autenticação e Segurança

| Campo | Tipo | Obrigatório | Descrição | Valores |
|-------|------|-------------|-----------|---------|
| `consumerAuthenticationScore` | Integer | ✅ | Score de autenticação | 0-999 |
| `externalScore3` | Integer | ✅ | Score externo | 0-999 |
| `cavvResult` | Integer | ✅ | Resultado CAVV | 0-9 |
| `cryptogramValid` | String | ❌ | Criptograma válido | `"Y"`, `"N"` |
| `cvv2Response` | String | ❌ | Resposta CVV2 | `"M"` (match), `"N"` (no match) |
| `cvv2Present` | String | ❌ | CVV2 presente | `"Y"`, `"N"` |
| `pinVerifyCode` | String | ❌ | Código verificação PIN | - |
| `cvvVerifyCode` | String | ❌ | Código verificação CVV | - |
| `eciIndicator` | Integer | ✅ | Indicador ECI (3DS) | 1-7 |
| `atcCard` | Integer | ✅ | ATC do cartão | 0-65535 |
| `atcHost` | Integer | ✅ | ATC do host | 0-65535 |
| `tokenAssuranceLevel` | Integer | ✅ | Nível de garantia do token | 0-99 |

---

## 6. Campos do POS (Point of Sale)

| Campo | Tipo | Obrigatório | Descrição | Valores |
|-------|------|-------------|-----------|---------|
| `posEntryMode` | String | ❌ | Modo de entrada | `"1"` (manual), `"5"` (chip), `"9"` (e-commerce) |
| `customerPresent` | String | ❌ | Cliente presente | `"0"` (não), `"1"` (sim) |
| `posOffPremises` | Integer | ❌ | Fora do estabelecimento | 0, 1 |
| `posCardCapture` | Integer | ❌ | Captura de cartão | 0, 1 |
| `posSecurity` | Integer | ❌ | Indicador de segurança | 0-9 |
| `posConditionCode` | String | ❌ | Código de condição | - |
| `terminalId` | String | ❌ | ID do terminal | `"TERM-001"` |
| `terminalType` | String | ❌ | Tipo de terminal | - |
| `terminalEntryCapability` | String | ❌ | Capacidade de entrada | - |

---

## 7. Campos do Cartão

| Campo | Tipo | Obrigatório | Descrição | Valores |
|-------|------|-------------|-----------|---------|
| `cardMediaType` | String | ❌ | Tipo de mídia | `"C"` (chip), `"M"` (magnético) |
| `cardExpireDate` | Integer | ❌ | Data de expiração (YYMM) | `2512` |
| `cardSeqNum` | Integer | ❌ | Número sequencial | 1-99 |
| `expandedBIN` | String | ❌ | BIN expandido | `"411111"` |
| `tokenizationIndicator` | String | ❌ | Indicador de tokenização | `"Y"`, `"N"` |
| `tokenId` | String | ❌ | ID do token | - |
| `tokenRequestorId` | String | ❌ | ID do solicitante do token | - |
| `paymentInstrumentId` | String | ❌ | ID do instrumento de pagamento | - |

---

## 8. Campos Financeiros

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `availableCredit` | BigDecimal | ✅ | Crédito disponível (centavos) | `500000` |
| `cardCashBalance` | BigDecimal | ✅ | Saldo em dinheiro | `0` |
| `cardDelinquentAmount` | BigDecimal | ✅ | Valor em atraso | `0` |

---

## 9. Campos de Verificação PIN/CVV

| Campo | Tipo | Obrigatório | Descrição | Valores |
|-------|------|-------------|-----------|---------|
| `cvrofflinePinVerificationPerformed` | Integer | ❌ | PIN offline verificado | 0, 1 |
| `cvrofflinePinVerificationFailed` | Integer | ❌ | PIN offline falhou | 0, 1 |
| `cvvPinTryLimitExceeded` | Integer | ❌ | Limite de tentativas excedido | 0, 1 |

---

## 10. Campos de Rede/Adquirente

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `acquirerId` | String | ❌ | ID do adquirente | `"ACQ-001"` |
| `acquirerBin` | String | ❌ | BIN do adquirente | `"123456"` |
| `acquirerCountry` | String | ❌ | País do adquirente | `"076"` |
| `networkId` | String | ❌ | ID da rede | `"VISA"` |

---

## 11. Campos de Autorização

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `authDecisionCode` | String | ❌ | Código de decisão | `"00"` (aprovado) |
| `authResponseCode` | String | ❌ | Código de resposta | `"00"` |
| `authId` | String | ❌ | ID da autorização | `"AUTH-001"` |
| `authIndicator` | Integer | ❌ | Indicador de autorização | - |
| `authPostFlag` | String | ❌ | Flag pós-autorização | - |
| `processorAuthReasonCode` | String | ❌ | Código de razão do processador | - |
| `standinAdvice` | String | ❌ | Conselho stand-in | - |
| `secondFactorAuthCode` | String | ❌ | Código 2FA | - |
| `cavvKeyIndicator` | Integer | ❌ | Indicador de chave CAVV | - |

---

## 12. Campos de Metadados

| Campo | Tipo | Obrigatório | Descrição | Exemplo |
|-------|------|-------------|-----------|---------|
| `workflow` | String | ❌ | Workflow | `"BRZLCREDIT"` |
| `recordType` | String | ❌ | Tipo de registro | `"CRTRAN25"` |
| `portfolio` | String | ❌ | Portfolio | `"DEFAULT"` |
| `dataSpecificationVersion` | BigDecimal | ❌ | Versão da especificação | `2.5` |
| `recordCreationDate` | Integer | ❌ | Data de criação (YYYYMMDD) | `20241231` |
| `recordCreationTime` | Integer | ❌ | Hora de criação (HHMMSS) | `143052` |
| `recordCreationMilliseconds` | Integer | ❌ | Milissegundos | `500` |
| `tranCode` | String | ❌ | Código da transação | - |
| `checkNumber` | String | ❌ | Número do cheque | - |
| `atmOwner` | String | ❌ | Proprietário do ATM | - |
| `avsRequest` | String | ❌ | Requisição AVS | - |

---

## 13. Campos de Usuário (Customizáveis)

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `userIndicator01` | String | ❌ | Indicador customizado 1 |
| `userIndicator03` | String | ❌ | Indicador customizado 3 |
| `userIndicator04` | String | ❌ | Indicador customizado 4 |
| `userIndicator05` | String | ❌ | Indicador customizado 5 |
| `userIndicator08` | String | ❌ | Indicador customizado 8 |
| `userData01` | String | ❌ | Dados customizados 1 |
| `userData02` | String | ❌ | Dados customizados 2 |
| `userData03` | String | ❌ | Dados customizados 3 |
| `userData04` | String | ❌ | Dados customizados 4 |
| `userData05` | String | ❌ | Dados customizados 5 |
| `userData06` | String | ❌ | Dados customizados 6 |
| `userData06_2` | String | ❌ | Dados customizados 6.2 |
| `userData09` | String | ❌ | Dados customizados 9 |

---

## 14. Campos EMV (Chip)

| Campo | Tipo | Obrigatório | Descrição |
|-------|------|-------------|-----------|
| `cardAipStatic` | String | ❌ | AIP estático |
| `cardAipDynamic` | String | ❌ | AIP dinâmico |
| `cardAipVerify` | String | ❌ | AIP verificação |
| `cardAipRisk` | String | ❌ | AIP risco |
| `cardAipIssuerAuthentication` | String | ❌ | AIP autenticação emissor |
| `cardAipCombined` | String | ❌ | AIP combinado |
| `terminalVerificationResults` | String | ❌ | TVR |
| `cardVerificationResults` | String | ❌ | CVR |
| `idMethod` | Integer | ❌ | Método de identificação |

---

## 15. Campos DERIVADOS (Calculados pelo Sistema)

Estes campos **NÃO estão no payload**, mas são calculados pelo sistema:

| Campo Derivado | Fonte | Descrição |
|----------------|-------|-----------|
| `merchantLocation` | `merchantCity` + `merchantState` + `merchantCountryCode` | Coordenadas via `geo_reference` |
| `panHash` | `pan` | Hash SHA-256 para velocity |
| `binInfo` | `pan` (6 primeiros dígitos) | Lookup na tabela `bin_lookup` |

---

## 16. GAPS - Campos Comuns de Fraude NÃO Disponíveis

Os seguintes campos são comuns em sistemas de fraude mas **NÃO EXISTEM** no payload atual:

| Campo | Descrição | Impacto |
|-------|-----------|---------|
| `deviceId` | ID do dispositivo | Não é possível fazer device fingerprinting |
| `ipAddress` | Endereço IP | Não é possível detectar proxies/VPNs |
| `userAgent` | User agent do browser | Não é possível detectar bots |
| `sessionId` | ID da sessão | Não é possível rastrear sessões |
| `browserFingerprint` | Fingerprint do browser | Não é possível identificar dispositivos |
| `geoLocation` | Coordenadas GPS | Usa lookup por cidade (menos preciso) |
| `emailAddress` | Email do cliente | Não é possível validar domínio |
| `phoneNumber` | Telefone do cliente | Não é possível validar número |
| `billingAddress` | Endereço de cobrança | Não é possível fazer AVS completo |
| `shippingAddress` | Endereço de entrega | Não é possível detectar drop shipping |

### 16.1 Recomendações para Enriquecimento

Para implementar regras mais sofisticadas, considerar adicionar:

1. **Device Fingerprinting**: Integrar com serviço de device ID
2. **IP Intelligence**: Integrar com serviço de geolocalização IP
3. **Email Validation**: Integrar com serviço de validação de email
4. **Phone Validation**: Integrar com serviço de validação de telefone

---

## 17. Operadores Permitidos por Tipo

### 17.1 Campos Numéricos
- `EQ`, `NE`, `GT`, `LT`, `GTE`, `LTE`
- `IN`, `NOT_IN`
- `BETWEEN`, `NOT_BETWEEN`
- `IS_NULL`, `IS_NOT_NULL`
- `FIELD_EQ`, `FIELD_GT`, etc.
- `MOD_EQ`, `MOD_NEQ`

### 17.2 Campos String
- `EQ`, `NE`
- `IN`, `NOT_IN`
- `CONTAINS`, `NOT_CONTAINS`
- `STARTS_WITH`, `ENDS_WITH`
- `REGEX`, `NOT_REGEX`
- `IS_NULL`, `IS_NOT_NULL`

### 17.3 Campos Booleanos
- `IS_TRUE`, `IS_FALSE`
- `IS_NULL`, `IS_NOT_NULL`

---

## 18. Referências

- **Código fonte**: `backend/src/main/java/com/rulex/dto/TransactionRequest.java`
- **Seeder**: `backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java`
- **Tabela DB**: `field_dictionary`
