# Inventário do Payload (imutável)

Este inventário foi derivado exclusivamente de artefatos do próprio repositório (sem enriquecimento externo):
- Contrato OpenAPI: [openapi/rulex.yaml](openapi/rulex.yaml)
- DTO Java (backend): [backend/src/main/java/com/rulex/dto/TransactionRequest.java](backend/src/main/java/com/rulex/dto/TransactionRequest.java)
- Interface TS usada no frontend: [client/src/lib/javaApi.ts](client/src/lib/javaApi.ts)

## Baseline (payload real fornecido)
O payload abaixo (família **CRTRAN25 / BRZLCREDIT**) foi fornecido pelo usuário e passa a ser o **baseline canônico** para regras duras payload-only.

Observação: o repositório historicamente tinha **definições divergentes** de payload (frontend vs Java vs OpenAPI). Nesta execução, o backend Java e o OpenAPI foram atualizados para aceitar os campos observados no exemplo.

## Campos canônicos (OpenAPI + Java) — transação de cartão (CRTRAN25)
Campos presentes em [backend/src/main/java/com/rulex/dto/TransactionRequest.java](backend/src/main/java/com/rulex/dto/TransactionRequest.java) e refletidos em [openapi/rulex.yaml](openapi/rulex.yaml).

### Identificação
- `externalTransactionId` (string)
- `customerIdFromHeader` (string)
- `customerAcctNumber` (int64)
- `pan` (string) — **deve ser tokenizado/mascarado**
- `merchantId` (string, opcional)
- `merchantName` (string, opcional)

### Protocolo/versão
- `workflow` (string)
- `recordType` (string)
- `dataSpecificationVersion` (number)
- `clientIdFromHeader` (string)

### Valor e tempo
- `transactionAmount` (number/decimal)
- `transactionDate` (int, YYYYMMDD)
- `transactionTime` (int, HHMMSS)
- `gmtOffset` (string, opcional)
- `transactionCurrencyCode` (int)
- `transactionCurrencyConversionRate` (number/decimal, opcional)

### Criação/registro (lag)
- `recordCreationDate` (int, YYYYMMDD)
- `recordCreationTime` (int, HHMMSS)
- `recordCreationMilliseconds` (int)

### Localização
- `merchantCountryCode` (string, opcional)
- `merchantCity` (string, opcional)
- `merchantState` (string, opcional)
- `merchantPostalCode` (string, opcional)

### Autorização / decisão
- `authPostFlag` (string)
- `authDecisionCode` (string)
- `authResponseCode` (string)
- `authId` (string)
- `authIndicator` (int)
- `processorAuthReasonCode` (string)
- `standinAdvice` (string)

### Contexto/terminal
- `mcc` (int)
- `posEntryMode` (string, opcional)
- `customerPresent` (string, opcional)

### Terminal/POS (sinais)
- `terminalId` (string)
- `terminalType` (string)
- `terminalEntryCapability` (string)
- `posConditionCode` (string)
- `posOffPremises` (int, 0/1)
- `posCardCapture` (int, 0/1)
- `posSecurity` (int, 0/1)
- `terminalVerificationResults` (string)
- `cardVerificationResults` (string)
- `networkId` (string)

### Autenticação/segurança
- `consumerAuthenticationScore` (int)
- `externalScore3` (int)
- `cavvResult` (int)
- `cavvKeyIndicator` (int)
- `cryptogramValid` (string, opcional)
- `cvv2Response` (string, opcional)
- `cvv2Present` (int)
- `pinVerifyCode` (string, opcional)
- `cvvVerifyCode` (string, opcional)
- `secondFactorAuthCode` (string)
- `avsRequest` (string)
- `cvrofflinePinVerificationPerformed` (int, 0/1)
- `cvrofflinePinVerificationFailed` (int, 0/1)
- `cvvPinTryLimitExceeded` (int, 0/1)
- `eciIndicator` (int)
- `atcCard` (int)
- `atcHost` (int)
- `tokenAssuranceLevel` (int)
- `tokenizationIndicator` (string, opcional)

### EMV AIP / mídia
- `cardAipStatic` (string)
- `cardAipDynamic` (string)
- `cardAipVerify` (string)
- `cardAipRisk` (string)
- `cardAipIssuerAuthentication` (string)
- `cardAipCombined` (string)
- `cardMediaType` (string)
- `cardSeqNum` (int|null)
- `cardExpireDate` (int, YYYYMMDD)

### Linha de crédito
- `availableCredit` (number/decimal)
- `cardCashBalance` (number/decimal)
- `cardDelinquentAmount` (number/decimal)

### Adquirência
- `acquirerId` (string)
- `acquirerCountry` (string)
- `acquirerBin` (string|null)
- `atmOwner` (string)

### Tokens/identificadores
- `tokenId` (string)
- `tokenRequestorId` (string)
- `paymentInstrumentId` (string)
- `expandedBIN` (string)
- `tranCode` (string)

### Campos “usuário/custom”
- `userIndicator01`, `userIndicator03`, `userIndicator04`, `userIndicator05`, `userIndicator08` (string)
- `idMethod` (int)
- `userData01`, `userData02`, `userData03`, `userData04`, `userData05`, `userData06`, `userData06_2`, `userData09` (string)
- `portfolio` (string)
- `onUsMerchantId` (string)
- `checkNumber` (string)

## Campos adicionais vistos no frontend (TS)
O frontend declara um payload maior em [client/src/lib/javaApi.ts](client/src/lib/javaApi.ts), incluindo (não exaustivo):
- `billingCurrencyCode`, `conversionRate`, `authorizationIdResponse`, `responseCode`, `additionalResponseData`
- `cvv2EntryLimitExceeded`, `pinEntryLimitExceeded` (booleans)
- `posSecurity`, `posOffPremises`
- `cardAipStatic`, `cardAipDynamic`, `terminalVerificationResults`, `cardExpireDate`, `cardCaptured`, `recurringTransaction`
- `panSequenceNumber`
- `acquirerId`, `acquirerCountryCode`
- `custom1..custom20`

**Status**: estes campos **não estão** no DTO Java atual e **não são garantidos** no contrato OpenAPI atual.

## Cobertura Pix
Não há campos Pix (ex.: chave Pix, PSP, ISPB, tipo de chave, iniciação, QR, etc.) em nenhuma definição canônica acima.
Logo, regras Pix **não podem** ser expressas de forma “payload-only” com a informação presente no repo hoje.
