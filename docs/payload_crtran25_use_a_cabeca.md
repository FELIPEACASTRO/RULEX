# RULEX — Curso Completo: Payload de Entrada CRTRAN25 (Use a Cabeça)

> **Bem-vindo ao curso mais detalhado sobre o payload CRTRAN25 do RULEX.**
> Este não é "só uma lista de campos". É um **curso estruturado** para que você domine
> o payload de transação como um cientista de dados, um integrador e um auditor ao
> mesmo tempo.

## Para quem é este curso

✅ **Integradores** que precisam enviar payloads corretos ao RULEX  
✅ **Engenheiros de dados** construindo pipelines que alimentam o motor de fraude  
✅ **Analistas de fraude** que querem entender o que cada campo significa  
✅ **Auditores** validando conformidade e rastreabilidade  
✅ **QA/testers** criando cenários de teste realistas

## O que você vai aprender

Ao final deste documento, você será capaz de:

1) 🎯 Montar um payload **válido, estável e auditável**
2) 🚨 Evitar armadilhas comuns (tipos, horário, padding, IDs, campos desconhecidos)
3) 🧪 Tratar o payload como **dataset de alta qualidade** (validação, consistência)
4) 🔍 Distinguir **contrato executável** (DTO/validação) vs. **contrato documental** (OpenAPI)
5) 🛡️ Aplicar boas práticas de segurança (PCI, PII, auditoria)
6) 🧩 Interpretar erros do sistema e corrigi-los rapidamente

## Como usar este curso

- **Seção 0-3**: fundamentos (escopo, modelo mental, strictness, erros)
- **Seção 4-7**: anatomia do payload (exemplo real, contrato rígido, dictionary)
- **Seção 8-9**: pontos críticos e checklist de qualidade
- **Seção 10**: referência campo-a-campo completa (90+ campos)
- **Seção 11+**: exercícios, anti-padrões, cenários de teste, quiz

📖 **Dica**: se você é iniciante, leia na ordem. Se você é experiente, vá direto
para a seção 10 (referência) ou 12 (anti-padrões).

---

## 0) Escopo exato (o que este doc cobre)

### 0.1 RecordType

- RecordType alvo: `CRTRAN25`.

### 0.2 Endpoints

Context path `/api`:

- `POST /api/transactions/analyze`
- `POST /api/transactions/analyze-advanced`

### 0.3 Fontes de verdade (prova material)

Quando há conflito entre fontes, siga esta ordem de prioridade:

1) **Contrato executável (o que o backend realmente desserializa e valida)**
   - DTO: [backend/src/main/java/com/rulex/dto/TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java)
   - Controller binding: [backend/src/main/java/com/rulex/controller/TransactionController.java](../backend/src/main/java/com/rulex/controller/TransactionController.java)
2) **Configuração que muda o comportamento do parse/strictness**
   - Jackson strict: [backend/src/main/resources/application.yml](../backend/src/main/resources/application.yml)
3) **Contrato documental (OpenAPI)**
   - [openapi/rulex.yaml](../openapi/rulex.yaml) (schema `AnalyzeTransactionRequest`)
4) **Exemplo baseline “real” que passa em testes**
   - [fixtures/crtran.json](../fixtures/crtran.json)
5) **Catálogo (Field Dictionary) usado pela UI/regras v3.1**
   - Seeder: [backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java](../backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java)

---

## 1) Modelo mental (Use a Cabeça): pense no payload como uma “ficha clínica”

Uma ficha clínica boa responde 4 perguntas:

1) **Quem**? (cliente/conta/instrumento)
2) **O quê**? (evento/valor/tipo)
3) **Quando**? (data/hora/offset)
4) **Onde/como**? (merchant, terminal, canal, autenticação)

Se você tentar “apenas preencher campos”, você gera ruído.
Se você pensar como “ficha clínica”, você consegue:

- manter consistência temporal
- evitar duplicidade de evento
- preservar auditabilidade
- reduzir falso positivo em regra dura

---

## 2) A regra mais importante: payload estrito (sem campos desconhecidos)

### 2.1 O que o sistema está configurado para fazer

O backend está com:

```yaml
spring:
  jackson:
    deserialization:
      fail-on-unknown-properties: true
```

Fonte: [backend/src/main/resources/application.yml](../backend/src/main/resources/application.yml)

Em termos práticos:

- Qualquer chave JSON não reconhecida pelo DTO pode quebrar a requisição.

### 2.2 Consequência operacional

- “Campo extra” não é enriquecimento. **É quebra de contrato.**
- Enriquecimentos devem ocorrer fora do payload (feature store / side tables), como
  já documentado em [docs/payload_dictionary.md](payload_dictionary.md).

---

## 3) Como o backend responde quando algo dá errado (erro “observável”)

### 3.1 Erro de validação (Bean Validation)

Quando o DTO falha em `@NotNull`, `@NotBlank`, `@Min/@Max`, etc., o handler
global retorna:

- Status: **400**
- Body: `ApiErrorResponse` com `message = "Falha de validação"`

Fonte: [backend/src/main/java/com/rulex/api/GlobalExceptionHandler.java](../backend/src/main/java/com/rulex/api/GlobalExceptionHandler.java)

Observação importante (para integradores): hoje a mensagem é genérica; ela não
retorna qual campo falhou.

### 3.2 Erro de parse/desserialização

(JSON inválido, tipos incompatíveis, campo desconhecido)

Não existe handler específico para exceções de parse (por exemplo, `HttpMessageNotReadableException`).

Isso significa que:

- Dependendo do caminho de resolução do Spring, você pode ver **400**
  (com message genérica do Spring) ou cair no handler genérico e receber **500**
  com `message = "Erro interno"`.

Fonte: [backend/src/main/java/com/rulex/api/GlobalExceptionHandler.java](../backend/src/main/java/com/rulex/api/GlobalExceptionHandler.java)

Regra prática: trate erro de parse como **falha de contrato** e corrija no emissor.

---

## 4) Auditoria: “as received” (o que foi recebido, byte a byte)

O sistema captura o corpo bruto (bytes) para endpoints críticos:

- `/transactions/analyze`
- `/transactions/analyze-advanced`
- `/evaluate`

Fonte: [backend/src/main/java/com/rulex/api/RawPayloadCaptureFilter.java](../backend/src/main/java/com/rulex/api/RawPayloadCaptureFilter.java)

Detalhe importante:

- O controller de `/transactions/analyze` injeta esses bytes no serviço.
- O controller de `/transactions/analyze-advanced` **não** passa os bytes adiante
  (ele executa regras avançadas diretamente).

Fonte: [backend/src/main/java/com/rulex/controller/TransactionController.java](../backend/src/main/java/com/rulex/controller/TransactionController.java)

Implicação: para auditoria “perfeita”, a captura existe no request, mas o
consumo/armazenamento pode variar por endpoint.

---

## 5) “Hello world” CRTRAN25: payload mínimo que passa (prova material)

O arquivo abaixo é um baseline aceito em testes:

- [fixtures/crtran.json](../fixtures/crtran.json)

### 5.1 Payload mínimo (exatamente como no fixture)

```json
{
  "externalTransactionId": "crtran-it-1",
  "customerIdFromHeader": "cust-crtran-1",
  "customerAcctNumber": 1234567890123456,
  "pan": "4111111111111111",
  "merchantId": "m-1",
  "merchantName": "Merchant",
  "transactionAmount": 10.00,
  "transactionDate": 20251218,
  "transactionTime": 120000,
  "transactionCurrencyCode": 986,
  "mcc": 5999,
  "consumerAuthenticationScore": 200,
  "externalScore3": 200,
  "cavvResult": 0,
  "eciIndicator": 5,
  "atcCard": 1,
  "atcHost": 1,
  "tokenAssuranceLevel": 80,
  "availableCredit": 1000.00,
  "cardCashBalance": 0.00,
  "cardDelinquentAmount": 0.00
}
```

### 5.2 Exercícios (Use a Cabeça — pratique)

1) Remova `eciIndicator` → deve virar 400 (validação falha).
2) Troque `transactionAmount` para `0` → deve virar 400 (`transactionAmount > 0`).
3) Adicione `"campoQueNaoExiste": 1` → deve falhar (payload estrito).

---

## 6) Contrato rígido: o que é “obrigatório” de verdade

Para ser rigoroso, existem duas noções de obrigatório:

1) **Obrigatório no OpenAPI** (documental): listado em `required` no schema.
2) **Obrigatório no runtime** (executável): anotado com `@NotNull` ou `@NotBlank`
  no DTO.

No RULEX, a regra de ouro é:

- Se o campo é obrigatório no DTO, ele é obrigatório “de verdade”.
- Se o campo é obrigatório no OpenAPI mas não no DTO, pode haver drift de contrato.

Fontes:

- OpenAPI: [openapi/rulex.yaml](../openapi/rulex.yaml)
- DTO: [backend/src/main/java/com/rulex/dto/TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java)

### 6.1 Lista do “mínimo obrigatório” (runtime)

Obrigatórios no DTO (validação executável):

- `externalTransactionId` (string, `@NotBlank`)
- `customerIdFromHeader` (string, `@NotBlank`)
- `customerAcctNumber` (int64, `@NotNull`)
- `pan` (string, `@NotBlank`)
- `transactionCurrencyCode` (int, `@NotNull`)
- `transactionAmount` (number, `@NotNull`, `> 0`)
- `transactionDate` (int, `@NotNull`)
- `transactionTime` (int, `@NotNull`)
- `mcc` (int, `@NotNull`)
- `consumerAuthenticationScore` (int, `@NotNull`, 0..999)
- `externalScore3` (int, `@NotNull`, 0..999)
- `cavvResult` (int, `@NotNull`)
- `eciIndicator` (int, `@NotNull`)
- `atcCard` (int, `@NotNull`)
- `atcHost` (int, `@NotNull`)
- `tokenAssuranceLevel` (int, `@NotNull`)
- `availableCredit` (number, `@NotNull`)
- `cardCashBalance` (number, `@NotNull`)
- `cardDelinquentAmount` (number, `@NotNull`)

---

## 7) Field Dictionary (catálogo) — o “mapa” para UI e regras v3.1

### 7.1 O que é e o que NÃO é

- É um catálogo de campos (`$.campo`) com tipo/operadores/funções permitidas.
- Não altera o payload aceito pelos endpoints de análise.
- Ajuda o frontend a ser “catalog-driven” e o motor de regras a validar AST.

### 7.2 Como ele é gerado para CRTRAN25

O seeder cria entradas para todos os campos do DTO anotados com `@JsonProperty`:

- workflow = `BRZLCREDIT`
- recordType = `CRTRAN25`
- portfolio = `*`

E define:

- `dataType`: `string | number | boolean | unknown`
- `allowedOperators`: defaults por tipo
- `allowedFunctions`: defaults por tipo
- `securityConstraints`: marca `pan` e `*paymentInstrument*` como PCI (`neverLog=true`)

Fonte: [backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java](../backend/src/main/java/com/rulex/v31/field/FieldDictionarySeeder.java)

---

## 8) Divergências e “pontos perigosos” (onde integrador costuma cair)

### 8.1 `cvv2Present` (OpenAPI vs DTO)

- OpenAPI: `integer`
- DTO: `String`

Implicação: se você mandar `"cvv2Present": 1` pode ocorrer erro de parse por tipo.

Fontes:

- OpenAPI: [openapi/rulex.yaml](../openapi/rulex.yaml)
- DTO: [backend/src/main/java/com/rulex/dto/TransactionRequest.java](../backend/src/main/java/com/rulex/dto/TransactionRequest.java)

### 8.2 `transactionTime` como inteiro

O contrato descreve como `HHMMSS`, mas o tipo é inteiro.

Armadilha clássica:

- 09:05:07 → número vira `90507` e perde o zero à esquerda.

Recomendação de ingestão (sem mudar contrato):

- pad left para 6 dígitos na origem
- validar faixa (00..23, 00..59, 00..59)

Fonte documental do formato: [openapi/rulex.yaml](../openapi/rulex.yaml)

### 8.3 `transactionDate` como inteiro

Mesma armadilha: é `YYYYMMDD` mas é inteiro.

Recomendação:

- validar data real (mês 1..12, dia válido)

Fonte documental do formato: [openapi/rulex.yaml](../openapi/rulex.yaml)

---

## 9) Checklist de qualidade (padrão “data science grade”)

Esta seção é o que você colocaria como validação “antes de bater no RULEX”.

### 9.1 Tipos e coerção

- Nunca envie números como string (`"10.00"`).
- Nunca envie boolean como string (`"true"`).
- Para `Long` (ex.: `customerAcctNumber`), garanta que não excede 64-bit.

### 9.2 Temporal

- `transactionDate` deve ser data real.
- `transactionTime` deve ser hora real.
- Se `gmtOffset` for enviado, ele deve ter um padrão consistente
  (evitar múltiplos formatos no mesmo pipeline).

### 9.3 Semântica mínima

- `transactionAmount > 0` (requisito do DTO).
- Scores em 0..999 (requisito do DTO).

### 9.4 Segurança

- `pan` é PCI: não logar, não persistir em claro.
- IDs persistentes (`customerIdFromHeader`, `customerAcctNumber`,
  `paymentInstrumentId`, `tokenId`, etc.) devem ser tratados como sensíveis no
  pipeline.

---

## 10) Referência campo-a-campo (CRTRAN25)

Aqui está a referência completa baseada no **DTO + OpenAPI**.

Formato de cada campo:

- **JSON**: nome do campo
- **Tipo (DTO)**: tipo Java que o backend espera
- **Tipo (OpenAPI)**: tipo publicado no schema
- **Obrigatório (runtime)**: se há validação `@NotNull/@NotBlank`
- **Obrigatório (OpenAPI)**: se está em `required:`
- **Regras/semântica**: o que significa e como tratar
- **Pitfalls**: onde falha na prática

### 10.1 Identificadores e roteamento

#### externalTransactionId

- JSON: `externalTransactionId`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): sim (`@NotBlank`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Identificador externo do evento (idempotência/auditoria).
  - Deve ser estável: o mesmo evento → o mesmo ID.
- Pitfalls:
  - Colocar PII dentro do ID (e-mail/CPF) → risco de vazamento em logs/telemetria.

#### customerIdFromHeader

- JSON: `customerIdFromHeader`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): sim (`@NotBlank`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Identificador do cliente (origem “header”).
- Pitfalls:
  - Alternar o mesmo cliente entre múltiplos IDs → quebra features e regras baseadas
    em histórico.

#### clientIdFromHeader

- JSON: `clientIdFromHeader`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não
- Regras/semântica:
  - Identificador do “cliente da API” (aplicação/canal/tenant).

#### customerAcctNumber

- JSON: `customerAcctNumber`
- Tipo (DTO): `Long`
- Tipo (OpenAPI): `integer(int64)`
- Obrigatório (runtime): sim (`@NotNull`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Identificador numérico de conta.
- Pitfalls:
  - Se o número puder ter zeros à esquerda, representar como inteiro perde informação.
  - Se o emissor ultrapassar 64-bit, vai falhar.

#### workflow / recordType / portfolio

- JSON: `workflow`, `recordType`, `portfolio`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não
- Regras/semântica:
  - Metacampos de roteamento/categorização.
  - O Field Dictionary usa `workflow=BRZLCREDIT`, `recordType=CRTRAN25`,
    `portfolio=*` como defaults.

### 10.2 Instrumento de pagamento (PCI)

#### pan

- JSON: `pan`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string` (descrição: tokenizado/mascarado)
- Obrigatório (runtime): sim (`@NotBlank`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Deve ser tokenizado/mascarado conforme contrato documental.
- Segurança:
  - O Field Dictionary marca como `pci=true` e `neverLog=true`.
- Pitfalls:
  - Enviar PAN em claro.
  - Variar máscara/tokenização ao longo do tempo (dificulta correlação).

#### paymentInstrumentId

- JSON: `paymentInstrumentId`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não
- Segurança:
  - Marcado como PCI pelo seeder (contém `paymentInstrument`).

### 10.3 Merchant e localização

#### merchantId / merchantName

- JSON: `merchantId`, `merchantName`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

#### mcc

- JSON: `mcc`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim (`@NotNull`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - MCC do merchant.
- Pitfalls:
  - Preencher com `0`/valor desconhecido sem acordo; regras podem reagir.

#### merchantCountryCode / merchantCity / merchantState / merchantPostalCode

- JSON: `merchantCountryCode`, `merchantCity`, `merchantState`, `merchantPostalCode`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.4 Valor, moeda e conversão

#### transactionAmount

- JSON: `transactionAmount`
- Tipo (DTO): `BigDecimal`
- Tipo (OpenAPI): `number`
- Obrigatório (runtime): sim (`@NotNull` + `@DecimalMin(>0)`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Valor da transação.
- Pitfalls:
  - Enviar `0` → falha.
  - Enviar string → pode falhar no parse.

#### transactionCurrencyCode

- JSON: `transactionCurrencyCode`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim (`@NotNull`)
- Obrigatório (OpenAPI): sim

#### transactionCurrencyConversionRate

- JSON: `transactionCurrencyConversionRate`
- Tipo (DTO): `BigDecimal`
- Tipo (OpenAPI): `number`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.5 Tempo

#### transactionDate

- JSON: `transactionDate`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer` (descrição: `YYYYMMDD`)
- Obrigatório (runtime): sim (`@NotNull`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Data do evento.
- Pitfalls:
  - Datas inválidas (ex.: 20250230) não são bloqueadas por Bean Validation hoje;
    valide no emissor.

#### transactionTime

- JSON: `transactionTime`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer` (descrição: `HHMMSS`)
- Obrigatório (runtime): sim (`@NotNull`)
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Hora do evento.
- Pitfalls:
  - Perda de zeros à esquerda.
  - Horas inválidas não são bloqueadas por Bean Validation hoje; valide no emissor.

#### gmtOffset

- JSON: `gmtOffset`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não
- Regras/semântica:
  - Offset do fuso.
- Pitfalls:
  - Múltiplos formatos (`-0300` vs `-03:00`) no mesmo pipeline.

#### recordCreationDate / recordCreationTime / recordCreationMilliseconds

- JSON: `recordCreationDate`, `recordCreationTime`, `recordCreationMilliseconds`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.6 Autenticação e scores

#### consumerAuthenticationScore

- JSON: `consumerAuthenticationScore`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim (`@NotNull`, 0..999)
- Obrigatório (OpenAPI): sim
- Pitfalls:
  - Fora de 0..999 → falha.

#### externalScore3

- JSON: `externalScore3`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim (`@NotNull`, 0..999)
- Obrigatório (OpenAPI): sim

#### cavvResult / cavvKeyIndicator

- JSON: `cavvResult`, `cavvKeyIndicator`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): `cavvResult` sim, `cavvKeyIndicator` não
- Obrigatório (OpenAPI): `cavvResult` sim, `cavvKeyIndicator` não

#### eciIndicator

- JSON: `eciIndicator`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim
- Obrigatório (OpenAPI): sim

#### tokenAssuranceLevel

- JSON: `tokenAssuranceLevel`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim
- Obrigatório (OpenAPI): sim

#### cryptogramValid

- JSON: `cryptogramValid`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

#### cvv2Response / cvv2Present / pinVerifyCode / cvvVerifyCode

- JSON: `cvv2Response`, `cvv2Present`, `pinVerifyCode`, `cvvVerifyCode`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `cvv2Present` é `integer`, os demais `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não
- Pitfalls:
  - `cvv2Present` é o maior ponto de drift.

#### cvrofflinePinVerificationPerformed / cvrofflinePinVerificationFailed / cvvPinTryLimitExceeded

- JSON: `cvrofflinePinVerificationPerformed`, `cvrofflinePinVerificationFailed`,
  `cvvPinTryLimitExceeded`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.7 Contadores e deltas

#### atcCard / atcHost

- JSON: `atcCard`, `atcHost`
- Tipo (DTO): `Integer`
- Tipo (OpenAPI): `integer`
- Obrigatório (runtime): sim
- Obrigatório (OpenAPI): sim
- Regras/semântica:
  - Application Transaction Counter (cartão vs host).
- Pitfalls:
  - Reset/queda pode indicar troca de cartão, reemissão, etc. (as regras podem reagir).

### 10.8 Tokenização

#### tokenizationIndicator / tokenId / tokenRequestorId

- JSON: `tokenizationIndicator`, `tokenId`, `tokenRequestorId`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.9 POS/Terminal

#### posEntryMode / posConditionCode / posOffPremises / posCardCapture / posSecurity

- JSON: `posEntryMode`, `posConditionCode` (string), `posOffPremises`,
  `posCardCapture`, `posSecurity` (integer)
- Tipo (DTO): conforme acima
- Tipo (OpenAPI): conforme acima
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

#### terminalId / terminalType / terminalEntryCapability

- JSON: `terminalId`, `terminalType`, `terminalEntryCapability`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

#### terminalVerificationResults / cardVerificationResults

- JSON: `terminalVerificationResults`, `cardVerificationResults`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.10 Adquirência e rede

#### networkId / acquirerId / acquirerCountry / acquirerBin / expandedBIN

- JSON: `networkId`, `acquirerId`, `acquirerCountry`, `acquirerBin`, `expandedBIN`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string` (OpenAPI marca `acquirerBin` como nullable)
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.11 Campos “usuário/indicadores”

#### userIndicator01 / userIndicator03 / userIndicator04 / userIndicator05 / userIndicator08

- JSON: conforme nome
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não
- Regras/semântica:
  - Campos genéricos: sem dicionário interno aqui, trate como “categóricos livres”.

#### userData01..06, userData06_2, userData09

- JSON: `userData01`, `userData02`, `userData03`, `userData04`, `userData05`,
  `userData06`, `userData06_2`, `userData09`
- Tipo (DTO): `String`
- Tipo (OpenAPI): `string`
- Obrigatório (runtime): não
- Obrigatório (OpenAPI): não

### 10.12 Outros campos existentes no contrato

Os campos abaixo existem no DTO/OpenAPI e seguem o mesmo padrão (tipo/optional):

- `dataSpecificationVersion` (number)
- `authPostFlag` (string)
- `authDecisionCode` (string)
- `authResponseCode` (string)
- `authId` (string)
- `authIndicator` (integer)
- `processorAuthReasonCode` (string)
- `standinAdvice` (string)
- `transactionType` (string)
- `transactionCategory` (string)
- `secondFactorAuthCode` (string)
- `avsRequest` (string)
- `checkNumber` (string)
- `atmOwner` (string)
- `tranCode` (string)
- `onUsMerchantId` (string)
- `idMethod` (integer)
- `cardSeqNum` (integer)
- `cardExpireDate` (integer)
- `cardMediaType` (string)
- `cardAipStatic`, `cardAipDynamic`, `cardAipVerify`, `cardAipRisk`,
  `cardAipIssuerAuthentication`, `cardAipCombined` (string)

---

## 11) O que você precisa me responder para fechar “domínios fechados” sem achismo

Eu consigo documentar **tudo que é determinístico** do repo (tipos, requiredness,
faixas), mas não consigo inventar enums/dominios.

Para fechar a versão “100% completa” (com domínios e regras de coerência por
campo), preciso de 3 decisões:

1) Formato oficial de `gmtOffset` (ex.: `-03:00` ou `-0300`)?
2) `customerAcctNumber` é identificador que pode ter zeros à esquerda? Se sim,
   precisamos discutir mudança de tipo no contrato (hoje é inteiro).
3) Padrão oficial de `cvv2Present` (0/1, Y/N, algo else)?
  Hoje há drift (OpenAPI vs DTO).
