# RULEX - Extração de Capacidades Determinísticas

**Versão:** 1.0.0
**Data:** 2025-01-03
**Status:** FASE 1 - EM PROGRESSO

---

## Atualizações (2025-01-03 19:45)
- Atualização com capacidades extraídas da navegação web
- Adição de novas tipologias: Scams, Mules, ATO, Card Testing
- Mapeamento de regras HAWK:AI e Verafin

## Atualizações (2025-01-03 19:00)
- Criação inicial do documento
- Estrutura preparada para extração de capacidades

---

## 1. Objetivo

Extrair capacidades de detecção de fraude de produtos de mercado e **traduzir para controles determinísticos** compatíveis com o RULEX.

**Regra Absoluta**:
- ❌ NÃO usar ML/AI
- ✅ APENAS controles determinísticos (if/else, thresholds, velocity, listas, regex)

---

## 2. Mapeamento: Capacidade de Mercado → Controle RULEX

### 2.1 Legenda de Tradução

| Termo de Mercado | Tradução Determinística RULEX |
|------------------|-------------------------------|
| "AI-powered detection" | Regras com thresholds calibrados |
| "Machine learning model" | Combinação de regras determinísticas |
| "Anomaly detection" | Velocity checks + desvio de padrão fixo |
| "Behavioral analytics" | Comparação com histórico (velocity) |
| "Risk scoring" | Score determinístico por pontos |
| "Real-time decisioning" | Avaliação de regras em tempo real |
| "Pattern recognition" | Regex + velocity + correlação de campos |

---

## 3. Capacidades Extraídas por Categoria

### 3.1 Validação Técnica (SECURITY)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| CVV Mismatch | FICO, Visa | cvv2Response ≠ "M" | NEQ | cvv2Response |
| PIN Failure | FICO | cvrofflinePinVerificationFailed = 1 | EQ | cvrofflinePinVerificationFailed |
| Cryptogram Invalid | Mastercard | cryptogramValid = "N" | EQ | cryptogramValid |
| ATC Mismatch | EMV | atcCard ≠ atcHost | FIELD_NEQ | atcCard, atcHost |
| Low Auth Score | Geral | consumerAuthenticationScore < 50 | LT | consumerAuthenticationScore |
| ECI High Risk | 3DS | eciIndicator IN (1, 6, 7) | IN | eciIndicator |

### 3.2 MCC de Alto Risco (CONTEXT)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| Gambling | FICO, Visa | mcc IN (7995, 7994, 7993) | IN | mcc |
| Crypto/Quasi-Cash | AML | mcc IN (6051, 6211) | IN | mcc |
| Wire Transfer | AML | mcc IN (4829, 6010, 6012) | IN | mcc |
| Adult Content | Visa | mcc IN (5967, 7273) | IN | mcc |
| Pawn Shops | AML | mcc = 5933 | EQ | mcc |
| Money Orders | AML | mcc IN (6051, 6540) | IN | mcc |

### 3.3 Velocity Checks (VELOCITY)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| High Frequency | FICO, IBM | COUNT(PAN, 60min) > 5 | VELOCITY_COUNT_GT | pan |
| Daily Limit | Geral | SUM(PAN, 24h) > R$10.000 | VELOCITY_SUM_GT | pan, transactionAmount |
| Multiple Merchants | FICO | DISTINCT(PAN, 24h, MERCHANTS) > 5 | VELOCITY_DISTINCT_GT | pan, merchantId |
| Multiple Countries | Geral | DISTINCT(PAN, 24h, COUNTRIES) > 2 | VELOCITY_DISTINCT_GT | pan, merchantCountryCode |
| Card Testing | Visa | COUNT(PAN, 5min) > 3 AND AVG < R$10 | VELOCITY_COUNT_GT + VELOCITY_AVG_LT | pan |

### 3.4 Geolocalização (CONTEXT)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| International | Geral | merchantCountryCode ≠ "076" | NEQ | merchantCountryCode |
| High-Risk Country | FATF | merchantCountryCode IN (lista) | IN | merchantCountryCode |
| Impossible Travel | FICO | GEO_DISTANCE > 1000km em < 1h | GEO_DISTANCE_GT + VELOCITY | merchantCity, merchantState |

### 3.5 Valor e Padrões (CONTEXT)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| High Value | Geral | transactionAmount > R$5.000 | GT | transactionAmount |
| Round Amount | AML | transactionAmount MOD 10000 = 0 | MOD_EQ | transactionAmount |
| Just Below Limit | AML | transactionAmount BETWEEN 4900-4999 | BETWEEN | transactionAmount |
| Exceeds Credit | Geral | transactionAmount > availableCredit | FIELD_GT | transactionAmount, availableCredit |

### 3.6 Horário (CONTEXT)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| Night Transaction | Geral | transactionTime BETWEEN 000000-060000 | TIME_BETWEEN | transactionTime |
| Weekend | Geral | (derivado de transactionDate) | DATE derivado | transactionDate |

### 3.7 Canal/POS (CONTEXT)

| Capacidade | Fonte | Controle RULEX | Operador | Campos |
|------------|-------|----------------|----------|--------|
| E-commerce | Geral | posEntryMode = "9" | EQ | posEntryMode |
| Manual Entry | Geral | posEntryMode = "1" | EQ | posEntryMode |
| Card Not Present | CNP | customerPresent = "0" | EQ | customerPresent |
| Fallback | EMV | posEntryMode IN ("80", "90") | IN | posEntryMode |

---

## 4. Capacidades por Vendor (Resumo)

### 4.1 FICO Falcon
- **Foco**: Velocity, behavioral patterns, card testing
- **Controles Determinísticos**:
  - Frequency limits
  - Amount thresholds
  - Geographic rules
  - MCC restrictions

### 4.2 IBM Safer Payments
- **Foco**: Real-time decisioning, rule management
- **Controles Determinísticos**:
  - Configurable thresholds
  - Velocity counters
  - List management (blacklist/whitelist)

### 4.3 NICE Actimize
- **Foco**: AML, transaction monitoring
- **Controles Determinísticos**:
  - Structuring detection (round amounts)
  - Velocity patterns
  - Cross-border rules

### 4.4 ACI Worldwide
- **Foco**: Payment fraud, card fraud
- **Controles Determinísticos**:
  - Real-time rules
  - Velocity checks
  - MCC-based rules

### 4.5 Visa/Mastercard
- **Foco**: Network-level fraud prevention
- **Controles Determinísticos**:
  - CVV/CVC validation
  - 3DS/ECI indicators
  - BIN-based rules
  - Velocity limits

### 4.6 HAWK:AI (Navegado)
- **Foco**: Transaction fraud, multi-rail support
- **Controles Determinísticos**:
  - Real-time detection (150ms)
  - **Self-serve rule management** (similar ao RULEX)
  - Round amount rules (structuring)
  - ATM fraud rules
  - Merchant fraud rules
  - Sandbox testing
  - Payment interdiction (block/hold/release)

### 4.7 Verafin (Navegado)
- **Foco**: Cross-channel fraud, consortium data
- **Controles Determinísticos**:
  - Cross-channel analysis
  - Consortium data profiling
  - Deposit fraud patterns
  - Check fraud detection
  - Wire fraud rules
  - ACH fraud rules
  - ATO indicators

### 4.8 Kount (Navegado)
- **Foco**: E-commerce fraud, card testing
- **Controles Determinísticos**:
  - Risk score calculation
  - Policy/threshold evaluation
  - Accept/Block/Challenge decisions
  - Card testing detection

### 4.9 Sift (Navegado)
- **Foco**: Fintech fraud, CNP
- **Controles Determinísticos**:
  - CNP fraud detection
  - Account takeover indicators
  - Chargeback fraud patterns
  - Payment fraud rules
  - Policy abuse detection

### 4.10 BioCatch (Navegado)
- **Foco**: Behavioral, ATO, Mules
- **Controles Determinísticos**:
  - ATO detection patterns
  - Mule account indicators
  - Social engineering scam patterns
  - **GAP**: Behavioral biometrics requer session data

---

## 5. Novas Tipologias Identificadas (Web Research)

### 5.1 Scams & Social Engineering
| Tipologia | Indicadores Determinísticos | Implementável |
|-----------|----------------------------|---------------|
| APP Scam (Authorized Push Payment) | Novo beneficiário + valor alto + horário atípico | ✅ Parcial |
| Romance Scam | Múltiplas transferências para mesmo destino | ⚠️ Requer histórico |
| Investment Scam | MCC específico + valor alto + recorrência | ✅ Sim |
| Impersonation | Alteração súbita de padrão | ⚠️ Requer velocity |

### 5.2 Mule Account Indicators
| Indicador | Controle RULEX | Implementável |
|-----------|----------------|---------------|
| Rapid fund movement | VELOCITY_SUM_GT em janela curta | ✅ Sim |
| Multiple incoming + single outgoing | Velocity de entrada vs saída | ⚠️ Parcial |
| New account + high volume | Idade da conta + velocity | ⚠️ Requer campo |
| Round amounts | MOD_EQ | ✅ Sim |

### 5.3 Account Takeover (ATO)
| Indicador | Controle RULEX | Implementável |
|-----------|----------------|---------------|
| Device change | deviceId diferente | ❌ GAP |
| Location change | merchantCountryCode/City diferente | ✅ Sim |
| Time pattern change | transactionTime fora do padrão | ✅ Sim |
| Multiple failed auth | consumerAuthenticationScore baixo | ✅ Sim |

### 5.4 Card Testing / BIN Attack
| Indicador | Controle RULEX | Implementável |
|-----------|----------------|---------------|
| High frequency, low value | VELOCITY_COUNT_GT + transactionAmount LT | ✅ Sim |
| Sequential card numbers | Regex no PAN | ⚠️ Parcial |
| Same merchant, multiple cards | VELOCITY por merchantId | ✅ Sim |
| Declined followed by approved | Histórico de decisões | ⚠️ Parcial |

---

## 6. Gaps Identificados (PAYLOAD_IMUTAVEL)

Capacidades que **NÃO PODEM** ser implementadas devido a campos ausentes:

| Capacidade | Campo Necessário | Status |
|------------|------------------|--------|
| Device Fingerprinting | deviceId | GAP_DE_DADO |
| IP Geolocation | ipAddress | GAP_DE_DADO |
| Browser Detection | userAgent | GAP_DE_DADO |
| Session Tracking | sessionId | GAP_DE_DADO |
| Email Validation | emailAddress | GAP_DE_DADO |
| Phone Validation | phoneNumber | GAP_DE_DADO |
| Shipping Address | shippingAddress | GAP_DE_DADO |

**Ação**: Documentar em GAPS_DA_SOLUCAO.md e PLAN_ENRICHMENT.md

---

## 6. Mapeamento para Operadores RULEX

### 6.1 Operadores Disponíveis (Confirmados)

| Categoria | Operadores |
|-----------|------------|
| Comparação | EQ, NEQ, GT, GTE, LT, LTE |
| Lista | IN, NOT_IN |
| Range | BETWEEN, NOT_BETWEEN |
| String | CONTAINS, STARTS_WITH, ENDS_WITH, REGEX |
| Nulo | IS_NULL, NOT_NULL |
| Campo | FIELD_EQ, FIELD_GT, FIELD_LT, etc. |
| Temporal | DATE_BEFORE, DATE_AFTER, TIME_BETWEEN |
| Matemático | MOD_EQ, MOD_NEQ |
| Geo | GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON |
| Velocity | VELOCITY_COUNT_GT, VELOCITY_SUM_GT, VELOCITY_DISTINCT_GT |

### 6.2 Operadores Lógicos

| Operador | Uso |
|----------|-----|
| AND | Todas as condições verdadeiras |
| OR | Pelo menos uma condição verdadeira |
| NOT | Inverte resultado |
| XOR | Exatamente uma condição verdadeira |
| NAND | Pelo menos uma condição falsa |
| NOR | Todas as condições falsas |

---

## 7. Próximos Passos

1. [ ] Completar navegação de URLs (01_DOSSIE)
2. [ ] Extrair capacidades específicas por vendor
3. [ ] Mapear para operadores RULEX
4. [ ] Identificar gaps de dados
5. [ ] Compilar Top 50 regras (03_RULES_CATALOG)
