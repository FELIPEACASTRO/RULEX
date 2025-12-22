# RULEX — Payload Field Dictionary (v0)

Fonte primária: `openapi/rulex.yaml` (schema `AnalyzeTransactionRequest`).
Fonte secundária de validações/tipos: `backend/src/main/java/com/rulex/dto/TransactionRequest.java`.

> Observação: o payload atual é **flat** (sem objetos aninhados). Portanto o `path` coincide com o nome do campo.
> 
> Restrições: o payload de entrada **não pode ser alterado**. Enriquecimentos devem ocorrer via feature store / side tables.

## Campos (path → tipo)

Legenda:
- **Tipo**: conforme OpenAPI (`string|integer|number`) + observações
- **Req.**: requerido pelo OpenAPI
- **Sensível**: indica dado potencialmente sensível (PAN/PII/IDs persistentes)
- **Operadores**: categorias permitidas (alto nível). O catálogo completo de operadores será definido pelo motor.

| path | tipo | req. | sensível | descrição/observações | operadores (macro) |
|---|---:|:---:|:---:|---|---|
| workflow | string | não | não | Tipo de workflow/processo | STRING, DATA_QUALITY |
| recordType | string | não | não | Tipo de registro | STRING, DATA_QUALITY |
| dataSpecificationVersion | number | não | não | Versão da especificação | NUMERIC, DATA_QUALITY |
| clientIdFromHeader | string | não | sim | Identificador de cliente (header) | STRING, DATA_QUALITY |
| externalTransactionId | string | sim | não | ID externo da transação | STRING, DATA_QUALITY |
| customerIdFromHeader | string | sim | sim | Identificador do cliente (header) | STRING, DATA_QUALITY |
| customerAcctNumber | integer(int64) | sim | sim | Número/ID de conta do cliente | NUMERIC, STRING, DATA_QUALITY |
| pan | string | sim | sim (PAN) | Tokenizado/mascarado; **nunca** armazenar PAN em claro | STRING/REGEX (somente sobre token/mascara), DATA_QUALITY |
| merchantId | string | não | não | Identificador do merchant | STRING, DATA_QUALITY |
| merchantName | string | não | não | Nome do merchant | STRING, DATA_QUALITY |
| transactionAmount | number | sim | não | Valor da transação | NUMERIC, DATA_QUALITY, SCORE |
| transactionDate | integer | sim | não | Data `YYYYMMDD` | TEMPORAL, NUMERIC, DATA_QUALITY |
| transactionTime | integer | sim | não | Hora `HHMMSS` | TEMPORAL, NUMERIC, DATA_QUALITY |
| gmtOffset | string | não | não | Offset GMT (ex: `-0300`) | STRING/REGEX, DATA_QUALITY |
| transactionCurrencyCode | integer | sim | não | Código moeda (ex: 986=BRL) | NUMERIC, DATA_QUALITY |
| transactionCurrencyConversionRate | number | não | não | Taxa conversão | NUMERIC, DATA_QUALITY |
| merchantCountryCode | string | não | não | País do merchant | STRING, DATA_QUALITY |
| merchantCity | string | não | não | Cidade do merchant | STRING, DATA_QUALITY |
| merchantState | string | não | não | UF/estado do merchant | STRING, DATA_QUALITY |
| merchantPostalCode | string | não | não | CEP | STRING/REGEX, DATA_QUALITY |
| mcc | integer | sim | não | MCC (categoria do merchant) | NUMERIC, IN_SET/NOT_IN_SET, DATA_QUALITY |
| posEntryMode | string | não | não | Modo de entrada POS | STRING, DATA_QUALITY |
| customerPresent | string | não | não | Cliente presente (Y/N) | STRING/BOOLEAN, DATA_QUALITY |
| authPostFlag | string | não | não | Flag pós-autorização | STRING, DATA_QUALITY |
| authDecisionCode | string | não | não | Código decisão auth | STRING, DATA_QUALITY |
| authResponseCode | string | não | não | Código resposta auth | STRING, DATA_QUALITY |
| authId | string | não | não | ID de autorização | STRING, DATA_QUALITY |
| authIndicator | integer | não | não | Indicador auth | NUMERIC, DATA_QUALITY |
| processorAuthReasonCode | string | não | não | Motivo auth do processador | STRING, DATA_QUALITY |
| standinAdvice | string | não | não | Stand-in advice | STRING, DATA_QUALITY |
| transactionType | string | não | não | Tipo transação | STRING, DATA_QUALITY |
| transactionCategory | string | não | não | Categoria transação | STRING, DATA_QUALITY |
| consumerAuthenticationScore | integer | sim | não | Score autenticação (0-999) | NUMERIC, SCORE, DATA_QUALITY |
| externalScore3 | integer | sim | não | Score externo (0-999) | NUMERIC, SCORE, DATA_QUALITY |
| cavvResult | integer | sim | não | Resultado CAVV/3DS | NUMERIC, DATA_QUALITY |
| cavvKeyIndicator | integer | não | não | Indicador chave CAVV | NUMERIC, DATA_QUALITY |
| secondFactorAuthCode | string | não | não | Código 2FA | STRING, DATA_QUALITY |
| cryptogramValid | string | não | não | Indicador criptograma | STRING, DATA_QUALITY |
| cvv2Response | string | não | não | Resposta CVV2 | STRING, DATA_QUALITY |
| cvv2Present | integer | não | não | CVV2 presente? (OpenAPI) | NUMERIC/BOOLEAN, DATA_QUALITY |
| pinVerifyCode | string | não | não | Resultado PIN | STRING, DATA_QUALITY |
| cvvVerifyCode | string | não | não | Resultado CVV | STRING, DATA_QUALITY |
| cvrofflinePinVerificationPerformed | integer | não | não | Offline PIN verificado? | NUMERIC/BOOLEAN, DATA_QUALITY |
| cvrofflinePinVerificationFailed | integer | não | não | Offline PIN falhou? | NUMERIC/BOOLEAN, DATA_QUALITY |
| cvvPinTryLimitExceeded | integer | não | não | Limite tentativas PIN/CVV excedido | NUMERIC/BOOLEAN, DATA_QUALITY |
| eciIndicator | integer | sim | não | ECI indicator | NUMERIC, DATA_QUALITY |
| atcCard | integer | sim | não | ATC cartão | NUMERIC, VELOCITY/DELTA, DATA_QUALITY |
| atcHost | integer | sim | não | ATC host | NUMERIC, VELOCITY/DELTA, DATA_QUALITY |
| tokenAssuranceLevel | integer | sim | não | Nível segurança token | NUMERIC, DATA_QUALITY |
| tokenizationIndicator | string | não | não | Indicador tokenização | STRING, DATA_QUALITY |
| tokenId | string | não | sim | Token ID | STRING/REGEX, DATA_QUALITY |
| tokenRequestorId | string | não | sim | Token requestor | STRING, DATA_QUALITY |
| paymentInstrumentId | string | não | sim | Payment instrument id | STRING, DATA_QUALITY |
| availableCredit | number | sim | sim | Crédito disponível | NUMERIC, DATA_QUALITY |
| cardCashBalance | number | sim | sim | Saldo cash | NUMERIC, DATA_QUALITY |
| cardDelinquentAmount | number | sim | sim | Valor em atraso | NUMERIC, DATA_QUALITY |
| cardSeqNum | integer | não | não | Sequência do cartão | NUMERIC, DATA_QUALITY |
| cardExpireDate | integer | não | sim | Expiração (ex: YYMM) | TEMPORAL/NUMERIC, DATA_QUALITY |
| cardMediaType | string | não | não | Tipo mídia cartão | STRING, DATA_QUALITY |
| cardAipStatic | string | não | não | AIP estático | STRING/REGEX, DATA_QUALITY |
| cardAipDynamic | string | não | não | AIP dinâmico | STRING/REGEX, DATA_QUALITY |
| cardAipVerify | string | não | não | AIP verify | STRING/REGEX, DATA_QUALITY |
| cardAipRisk | string | não | não | AIP risk | STRING/REGEX, DATA_QUALITY |
| cardAipIssuerAuthentication | string | não | não | AIP issuer auth | STRING/REGEX, DATA_QUALITY |
| cardAipCombined | string | não | não | AIP combinado | STRING/REGEX, DATA_QUALITY |
| terminalId | string | não | não | Terminal ID | STRING, DATA_QUALITY |
| terminalType | string | não | não | Terminal type | STRING, DATA_QUALITY |
| terminalEntryCapability | string | não | não | Entry capability | STRING, DATA_QUALITY |
| posConditionCode | string | não | não | POS condition | STRING, DATA_QUALITY |
| posOffPremises | integer | não | não | POS off premises | NUMERIC/BOOLEAN, DATA_QUALITY |
| posCardCapture | integer | não | não | POS card capture | NUMERIC/BOOLEAN, DATA_QUALITY |
| posSecurity | integer | não | não | POS security | NUMERIC, DATA_QUALITY |
| terminalVerificationResults | string | não | não | TVR | STRING/REGEX, DATA_QUALITY |
| cardVerificationResults | string | não | não | CVR | STRING/REGEX, DATA_QUALITY |
| networkId | string | não | não | Network ID | STRING, DATA_QUALITY |
| atmOwner | string | não | não | Dono ATM | STRING, DATA_QUALITY |
| acquirerId | string | não | não | Acquirer ID | STRING, DATA_QUALITY |
| acquirerCountry | string | não | não | País do acquirer | STRING, DATA_QUALITY |
| acquirerBin | string | não | não | BIN acquirer | STRING/REGEX, DATA_QUALITY |
| expandedBIN | string | não | não | BIN expandido | STRING/REGEX, DATA_QUALITY |
| tranCode | string | não | não | Tran code | STRING, DATA_QUALITY |
| avsRequest | string | não | não | AVS request | STRING, DATA_QUALITY |
| checkNumber | string | não | não | Check number | STRING, DATA_QUALITY |
| recordCreationDate | integer | não | não | Criação registro (data) | TEMPORAL/NUMERIC, DATA_QUALITY |
| recordCreationTime | integer | não | não | Criação registro (hora) | TEMPORAL/NUMERIC, DATA_QUALITY |
| recordCreationMilliseconds | integer | não | não | Criação registro (ms) | NUMERIC, DATA_QUALITY |
| portfolio | string | não | não | Portfolio | STRING, DATA_QUALITY |
| onUsMerchantId | string | não | não | On-us merchant | STRING, DATA_QUALITY |
| userIndicator01 | string | não | não | Indicador usuário | STRING, DATA_QUALITY |
| userIndicator03 | string | não | não | Indicador usuário | STRING, DATA_QUALITY |
| userIndicator04 | string | não | não | Indicador usuário | STRING, DATA_QUALITY |
| userIndicator05 | string | não | não | Indicador usuário | STRING, DATA_QUALITY |
| userIndicator08 | string | não | não | Indicador usuário | STRING, DATA_QUALITY |
| idMethod | integer | não | não | Método de identificação | NUMERIC, DATA_QUALITY |
| userData01 | string | não | não | User data | STRING, DATA_QUALITY |
| userData02 | string | não | não | User data | STRING, DATA_QUALITY |
| userData03 | string | não | não | User data | STRING, DATA_QUALITY |
| userData04 | string | não | não | User data | STRING, DATA_QUALITY |
| userData05 | string | não | não | User data | STRING, DATA_QUALITY |
| userData06 | string | não | não | User data | STRING, DATA_QUALITY |
| userData06_2 | string | não | não | User data | STRING, DATA_QUALITY |
| userData09 | string | não | não | User data | STRING, DATA_QUALITY |

## Sensibilidade e mascaramento

Regras mínimas para evitar vazamento:
- `pan`: mascarar sempre (ex: manter apenas últimos 4) e **nunca** persistir em claro.
- IDs persistentes e identificadores de conta (`customerIdFromHeader`, `customerAcctNumber`, `paymentInstrumentId`, `tokenId`, `tokenRequestorId`) devem ser tratados como sensíveis (hash/irreversível para indexação quando necessário).

## Gaps observados (serão endereçados na implementação)

- Divergências de tipo entre OpenAPI e DTO Java (ex.: `cvv2Present` é `integer` no OpenAPI e `String` no DTO).
- Frontend possui campos adicionais (ex.: `cvv2EntryLimitExceeded`, `pinEntryLimitExceeded`) que não constam no OpenAPI/DTO atual: devem virar **features derivadas** (enrichment) ou alinhar o simulador ao schema oficial.
