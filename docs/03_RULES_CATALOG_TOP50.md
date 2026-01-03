# RULEX - Catálogo Top 50 Regras de Fraude

**Versão:** 1.0.0
**Data:** 2025-01-03
**Status:** FASE 2 - EM PROGRESSO

---

## Atualizações (2025-01-03 19:00)
- Criação inicial do documento
- Regras baseadas em V22__fraud_detection_rules_seed.sql existente
- Estrutura preparada para novas regras da pesquisa web

---

## 1. Objetivo

Catalogar as **Top 50 regras de fraude** mais valiosas e implementáveis no RULEX, com:
- Evidência de fonte (web ou repo)
- Condição determinística
- Compatibilidade com payload atual

---

## 2. Regras Existentes (V22 Seed)

### Categoria: SECURITY (Validação Técnica)

#### RULE-001: CVV_MISMATCH
| Atributo | Valor |
|----------|-------|
| **ID** | CVV_MISMATCH |
| **Tipologia** | Card Fraud - CVV Validation |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `cvv2Response NEQ "M"` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Payload Check** | ✅ OK |

#### RULE-002: PIN_VERIFICATION_FAILED
| Atributo | Valor |
|----------|-------|
| **ID** | PIN_VERIFICATION_FAILED |
| **Tipologia** | Card Fraud - PIN Validation |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `cvrofflinePinVerificationFailed EQ 1` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 75 |
| **Payload Check** | ✅ OK |

#### RULE-003: CRYPTOGRAM_INVALID
| Atributo | Valor |
|----------|-------|
| **ID** | CRYPTOGRAM_INVALID |
| **Tipologia** | Card Fraud - EMV Validation |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `cryptogramValid EQ "N"` |
| **Decisão** | FRAUDE |
| **Severidade** | 95 |
| **Payload Check** | ✅ OK |

#### RULE-004: ATC_MISMATCH
| Atributo | Valor |
|----------|-------|
| **ID** | ATC_MISMATCH |
| **Tipologia** | Card Fraud - ATC Validation |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `atcCard FIELD_NEQ atcHost` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Payload Check** | ✅ OK |

#### RULE-005: LOW_AUTH_SCORE
| Atributo | Valor |
|----------|-------|
| **ID** | LOW_AUTH_SCORE |
| **Tipologia** | Authentication Risk |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `consumerAuthenticationScore LT 50` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 65 |
| **Payload Check** | ✅ OK |

#### RULE-006: PIN_TRY_LIMIT_EXCEEDED
| Atributo | Valor |
|----------|-------|
| **ID** | PIN_TRY_LIMIT_EXCEEDED |
| **Tipologia** | Card Fraud - Brute Force |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `cvvPinTryLimitExceeded EQ 1` |
| **Decisão** | FRAUDE |
| **Severidade** | 95 |
| **Payload Check** | ✅ OK |

---

### Categoria: CONTEXT (MCC Alto Risco)

#### RULE-007: MCC_GAMBLING_HIGH_VALUE
| Atributo | Valor |
|----------|-------|
| **ID** | MCC_GAMBLING_HIGH_VALUE |
| **Tipologia** | High-Risk MCC - Gambling |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, FICO, Visa |
| **Condição** | `mcc IN (7995, 7994, 7993) AND transactionAmount GT 50000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 75 |
| **Payload Check** | ✅ OK |

#### RULE-008: MCC_CRYPTO_QUASI_CASH
| Atributo | Valor |
|----------|-------|
| **ID** | MCC_CRYPTO_QUASI_CASH |
| **Tipologia** | High-Risk MCC - Crypto |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, AML Guidelines |
| **Condição** | `mcc IN (6051, 6211)` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 80 |
| **Payload Check** | ✅ OK |

#### RULE-009: MCC_WIRE_TRANSFER
| Atributo | Valor |
|----------|-------|
| **ID** | MCC_WIRE_TRANSFER |
| **Tipologia** | High-Risk MCC - Wire Transfer |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `mcc IN (4829, 6010, 6012)` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Payload Check** | ✅ OK |

#### RULE-010: MCC_ADULT_CONTENT
| Atributo | Valor |
|----------|-------|
| **ID** | MCC_ADULT_CONTENT |
| **Tipologia** | High-Risk MCC - Adult |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `mcc IN (5967, 7273)` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 65 |
| **Payload Check** | ✅ OK |

---

### Categoria: CONTEXT (Valor e Padrões)

#### RULE-011: HIGH_VALUE_TRANSACTION
| Atributo | Valor |
|----------|-------|
| **ID** | HIGH_VALUE_TRANSACTION |
| **Tipologia** | High Value |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `transactionAmount GT 500000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Threshold** | R$ 5.000,00 (500000 centavos) |
| **Payload Check** | ✅ OK |

#### RULE-012: EXCEEDS_AVAILABLE_CREDIT
| Atributo | Valor |
|----------|-------|
| **ID** | EXCEEDS_AVAILABLE_CREDIT |
| **Tipologia** | Credit Limit Violation |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `transactionAmount FIELD_GT availableCredit` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Payload Check** | ✅ OK |

#### RULE-013: ROUND_AMOUNT_STRUCTURING
| Atributo | Valor |
|----------|-------|
| **ID** | ROUND_AMOUNT_STRUCTURING |
| **Tipologia** | AML - Structuring |
| **Status** | PRODUÇÃO |
| **Confiança** | MÉDIA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, AML Guidelines |
| **Condição** | `transactionAmount MOD_EQ 100000,0` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 60 |
| **Nota** | Valor divisível por R$ 1.000 |
| **Payload Check** | ✅ OK |

---

### Categoria: CONTEXT (Geolocalização)

#### RULE-014: INTERNATIONAL_TRANSACTION
| Atributo | Valor |
|----------|-------|
| **ID** | INTERNATIONAL_TRANSACTION |
| **Tipologia** | Cross-Border |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `merchantCountryCode NEQ "076"` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 50 |
| **Payload Check** | ✅ OK |

#### RULE-015: HIGH_RISK_COUNTRY
| Atributo | Valor |
|----------|-------|
| **ID** | HIGH_RISK_COUNTRY |
| **Tipologia** | FATF High-Risk |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, FATF |
| **Condição** | `merchantCountryCode IN (lista FATF)` |
| **Decisão** | FRAUDE |
| **Severidade** | 85 |
| **Payload Check** | ✅ OK |

---

### Categoria: CONTEXT (Horário)

#### RULE-016: NIGHT_TRANSACTION
| Atributo | Valor |
|----------|-------|
| **ID** | NIGHT_TRANSACTION |
| **Tipologia** | Temporal Anomaly |
| **Status** | PRODUÇÃO |
| **Confiança** | MÉDIA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `transactionTime TIME_BETWEEN 000000,060000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 40 |
| **Payload Check** | ✅ OK |

---

### Categoria: CONTEXT (Canal/POS)

#### RULE-017: ECOMMERCE_HIGH_VALUE
| Atributo | Valor |
|----------|-------|
| **ID** | ECOMMERCE_HIGH_VALUE |
| **Tipologia** | CNP Fraud |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `posEntryMode EQ "9" AND transactionAmount GT 200000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Payload Check** | ✅ OK |

#### RULE-018: MANUAL_ENTRY_HIGH_VALUE
| Atributo | Valor |
|----------|-------|
| **ID** | MANUAL_ENTRY_HIGH_VALUE |
| **Tipologia** | Manual Entry Fraud |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `posEntryMode EQ "1" AND transactionAmount GT 100000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 75 |
| **Payload Check** | ✅ OK |

#### RULE-019: CARD_NOT_PRESENT
| Atributo | Valor |
|----------|-------|
| **ID** | CARD_NOT_PRESENT |
| **Tipologia** | CNP Fraud |
| **Status** | PRODUÇÃO |
| **Confiança** | MÉDIA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `customerPresent EQ "0"` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 45 |
| **Payload Check** | ✅ OK |

#### RULE-020: FALLBACK_TRANSACTION
| Atributo | Valor |
|----------|-------|
| **ID** | FALLBACK_TRANSACTION |
| **Tipologia** | EMV Fallback |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `posEntryMode IN ("80", "90")` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Payload Check** | ✅ OK |

---

### Categoria: VELOCITY

#### RULE-021: HIGH_FREQUENCY_PAN
| Atributo | Valor |
|----------|-------|
| **ID** | HIGH_FREQUENCY_PAN |
| **Tipologia** | Velocity - Frequency |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, FICO |
| **Condição** | `VELOCITY_COUNT_GT PAN,60,5` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 75 |
| **Threshold** | > 5 transações em 60 minutos |
| **Payload Check** | ✅ OK |

#### RULE-022: DAILY_AMOUNT_LIMIT
| Atributo | Valor |
|----------|-------|
| **ID** | DAILY_AMOUNT_LIMIT |
| **Tipologia** | Velocity - Amount |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `VELOCITY_SUM_GT PAN,1440,1000000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 80 |
| **Threshold** | > R$ 10.000 em 24 horas |
| **Payload Check** | ✅ OK |

#### RULE-023: MULTIPLE_MERCHANTS_24H
| Atributo | Valor |
|----------|-------|
| **ID** | MULTIPLE_MERCHANTS_24H |
| **Tipologia** | Velocity - Diversity |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, FICO |
| **Condição** | `VELOCITY_DISTINCT_GT PAN,1440,MERCHANTS,5` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Threshold** | > 5 merchants distintos em 24h |
| **Payload Check** | ✅ OK |

#### RULE-024: MULTIPLE_COUNTRIES_24H
| Atributo | Valor |
|----------|-------|
| **ID** | MULTIPLE_COUNTRIES_24H |
| **Tipologia** | Velocity - Geographic |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `VELOCITY_DISTINCT_GT PAN,1440,COUNTRIES,2` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Threshold** | > 2 países distintos em 24h |
| **Payload Check** | ✅ OK |

#### RULE-025: CARD_TESTING_PATTERN
| Atributo | Valor |
|----------|-------|
| **ID** | CARD_TESTING_PATTERN |
| **Tipologia** | BIN Attack / Card Testing |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql, Visa |
| **Condição** | `VELOCITY_COUNT_GT PAN,5,3 AND transactionAmount LT 1000` |
| **Decisão** | FRAUDE |
| **Severidade** | 95 |
| **Threshold** | > 3 transações em 5 min com valor < R$ 10 |
| **Payload Check** | ✅ OK |

---

### Categoria: COMPLEX (Regras Compostas)

#### RULE-026: INTL_GAMBLING_NIGHT
| Atributo | Valor |
|----------|-------|
| **ID** | INTL_GAMBLING_NIGHT_COMPLEX |
| **Tipologia** | Complex - Multi-factor |
| **Status** | PRODUÇÃO |
| **Confiança** | ALTA |
| **Evidência** | V22__fraud_detection_rules_seed.sql |
| **Condição** | `(mcc IN (7995,7994,7993)) AND (merchantCountryCode NEQ "076") AND (transactionTime TIME_BETWEEN 000000,060000) AND (transactionAmount GT 50000)` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Payload Check** | ✅ OK |

---

## 3. Regras a Implementar (Pesquisa Web)

### Categoria: Card Testing / BIN Attack

#### RULE-027: CARD_TESTING_SAME_MERCHANT
| Atributo | Valor |
|----------|-------|
| **ID** | CARD_TESTING_SAME_MERCHANT |
| **Tipologia** | BIN Attack - Same Merchant |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | Kount, HAWK:AI - "Card testing fraud - stop criminals testing stolen payment info" |
| **Condição** | `VELOCITY_COUNT_GT MERCHANT_ID,5,10 AND transactionAmount LT 500` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Threshold** | > 10 transações no mesmo merchant em 5 min, valor < R$5 |
| **Payload Check** | ✅ OK |

#### RULE-028: SEQUENTIAL_DECLINED_APPROVED
| Atributo | Valor |
|----------|-------|
| **ID** | SEQUENTIAL_DECLINED_APPROVED |
| **Tipologia** | BIN Attack - Probing |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | Verafin, ACI - "Card testing patterns" |
| **Condição** | `VELOCITY_COUNT_GT PAN,15,5 AND consumerAuthenticationScore LT 30` |
| **Decisão** | FRAUDE |
| **Severidade** | 95 |
| **Threshold** | > 5 transações em 15 min com score baixo |
| **Payload Check** | ✅ OK |

### Categoria: Scams & APP Fraud

#### RULE-029: HIGH_VALUE_NEW_BENEFICIARY
| Atributo | Valor |
|----------|-------|
| **ID** | HIGH_VALUE_NEW_BENEFICIARY |
| **Tipologia** | APP Scam - New Beneficiary |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | NICE Actimize, BioCatch - "Scams & Mule Defense" |
| **Condição** | `VELOCITY_DISTINCT_GT PAN,1440,MERCHANTS,0 AND transactionAmount GT 300000 AND merchantId IS_NOT_NULL` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 75 |
| **Threshold** | Primeiro uso de merchant + valor > R$3.000 |
| **Payload Check** | ✅ OK |

#### RULE-030: NIGHT_HIGH_VALUE_WIRE
| Atributo | Valor |
|----------|-------|
| **ID** | NIGHT_HIGH_VALUE_WIRE |
| **Tipologia** | APP Scam - Night Wire |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | HAWK:AI - "Wire fraud rules" |
| **Condição** | `mcc IN (4829, 6010, 6012) AND transactionTime TIME_BETWEEN 000000,060000 AND transactionAmount GT 200000` |
| **Decisão** | FRAUDE |
| **Severidade** | 85 |
| **Payload Check** | ✅ OK |

### Categoria: Mule Account

#### RULE-031: RAPID_FUND_MOVEMENT
| Atributo | Valor |
|----------|-------|
| **ID** | RAPID_FUND_MOVEMENT |
| **Tipologia** | Mule Account - Rapid Movement |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | BioCatch, NICE Actimize - "Mule Account Detection" |
| **Condição** | `VELOCITY_SUM_GT PAN,60,500000 AND VELOCITY_COUNT_GT PAN,60,3` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 80 |
| **Threshold** | > R$5.000 em 60 min com > 3 transações |
| **Payload Check** | ✅ OK |

#### RULE-032: ROUND_AMOUNTS_MULTIPLE
| Atributo | Valor |
|----------|-------|
| **ID** | ROUND_AMOUNTS_MULTIPLE |
| **Tipologia** | Mule Account - Structuring |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | HAWK:AI - "Round Amount Rule", ACI - "Structuring detection" |
| **Condição** | `transactionAmount MOD_EQ 50000,0 AND VELOCITY_COUNT_GT PAN,1440,3` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Threshold** | Valor divisível por R$500 + > 3 transações em 24h |
| **Payload Check** | ✅ OK |

### Categoria: ATO (Account Takeover)

#### RULE-033: LOCATION_CHANGE_HIGH_VALUE
| Atributo | Valor |
|----------|-------|
| **ID** | LOCATION_CHANGE_HIGH_VALUE |
| **Tipologia** | ATO - Location Change |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | BioCatch, Sift - "Account Takeover Protection" |
| **Condição** | `VELOCITY_DISTINCT_GT PAN,360,COUNTRIES,1 AND transactionAmount GT 100000` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Threshold** | > 1 país em 6h + valor > R$1.000 |
| **Payload Check** | ✅ OK |

#### RULE-034: AUTH_SCORE_DROP_HIGH_VALUE
| Atributo | Valor |
|----------|-------|
| **ID** | AUTH_SCORE_DROP_HIGH_VALUE |
| **Tipologia** | ATO - Auth Anomaly |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | Feedzai - "Behavioral data analysis" |
| **Condição** | `consumerAuthenticationScore LT 30 AND externalScore3 LT 30 AND transactionAmount GT 200000` |
| **Decisão** | FRAUDE |
| **Severidade** | 95 |
| **Threshold** | Ambos scores < 30 + valor > R$2.000 |
| **Payload Check** | ✅ OK |

### Categoria: CNP (Card Not Present)

#### RULE-035: CNP_HIGH_RISK_MCC
| Atributo | Valor |
|----------|-------|
| **ID** | CNP_HIGH_RISK_MCC |
| **Tipologia** | CNP - High Risk MCC |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | Sift - "$9.5B CNP fraud losses in 2023" |
| **Condição** | `customerPresent EQ "0" AND mcc IN (7995, 6051, 6211, 5967) AND transactionAmount GT 100000` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Payload Check** | ✅ OK |

#### RULE-036: CNP_INTERNATIONAL_NIGHT
| Atributo | Valor |
|----------|-------|
| **ID** | CNP_INTERNATIONAL_NIGHT |
| **Tipologia** | CNP - International Night |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | ACI - "Real-time anomaly detection" |
| **Condição** | `customerPresent EQ "0" AND merchantCountryCode NEQ "076" AND transactionTime TIME_BETWEEN 000000,060000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 75 |
| **Payload Check** | ✅ OK |

### Categoria: Velocity Avançado

#### RULE-037: VELOCITY_SPIKE_HOURLY
| Atributo | Valor |
|----------|-------|
| **ID** | VELOCITY_SPIKE_HOURLY |
| **Tipologia** | Velocity - Spike |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | FICO - "Velocity patterns", Feedzai - "Real-time risk assessment" |
| **Condição** | `VELOCITY_COUNT_GT PAN,60,10` |
| **Decisão** | FRAUDE |
| **Severidade** | 85 |
| **Threshold** | > 10 transações em 60 min |
| **Payload Check** | ✅ OK |

#### RULE-038: VELOCITY_AMOUNT_SPIKE
| Atributo | Valor |
|----------|-------|
| **ID** | VELOCITY_AMOUNT_SPIKE |
| **Tipologia** | Velocity - Amount Spike |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | ACI - ">85% Detection rates" |
| **Condição** | `VELOCITY_SUM_GT PAN,360,2000000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 80 |
| **Threshold** | > R$20.000 em 6 horas |
| **Payload Check** | ✅ OK |

### Categoria: ECI/3DS

#### RULE-039: ECI_NO_AUTH_HIGH_VALUE
| Atributo | Valor |
|----------|-------|
| **ID** | ECI_NO_AUTH_HIGH_VALUE |
| **Tipologia** | 3DS - No Authentication |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | Visa/Mastercard - "3DS/ECI indicators" |
| **Condição** | `eciIndicator IN (7) AND transactionAmount GT 300000` |
| **Decisão** | SUSPEITA_DE_FRAUDE |
| **Severidade** | 70 |
| **Nota** | ECI 7 = No authentication |
| **Payload Check** | ✅ OK |

#### RULE-040: ECI_FAILED_AUTH
| Atributo | Valor |
|----------|-------|
| **ID** | ECI_FAILED_AUTH |
| **Tipologia** | 3DS - Failed Authentication |
| **Status** | IMPLEMENT_NOW |
| **Confiança** | ALTA |
| **Evidência** | Visa/Mastercard - "CVV/CVC validation" |
| **Condição** | `eciIndicator IN (1, 6) AND cavvResult LT 2` |
| **Decisão** | FRAUDE |
| **Severidade** | 90 |
| **Nota** | ECI 1/6 = Failed auth + CAVV inválido |
| **Payload Check** | ✅ OK |

---

## 4. Resumo de Cobertura

| Categoria | Implementadas (V22) | A Implementar (Web) | Total |
|-----------|---------------------|---------------------|-------|
| SECURITY | 6 | 0 | 6 |
| CONTEXT (MCC) | 4 | 0 | 4 |
| CONTEXT (Valor) | 3 | 0 | 3 |
| CONTEXT (Geo) | 2 | 0 | 2 |
| CONTEXT (Horário) | 1 | 0 | 1 |
| CONTEXT (Canal) | 4 | 0 | 4 |
| VELOCITY | 5 | 2 | 7 |
| COMPLEX | 1 | 0 | 1 |
| Card Testing | 1 | 2 | 3 |
| Scams/APP | 0 | 2 | 2 |
| Mule Account | 0 | 2 | 2 |
| ATO | 0 | 2 | 2 |
| CNP | 2 | 2 | 4 |
| ECI/3DS | 0 | 2 | 2 |
| **TOTAL** | **26** | **14** | **40** |

### 4.1 Status das Novas Regras

| Status | Quantidade | Descrição |
|--------|------------|-----------|
| IMPLEMENT_NOW | 14 | Prontas para implementar |
| DO_NOT_IMPLEMENT | 0 | Requerem campos ausentes |
| TEÓRICA | 0 | Aguardando validação |

---

## 5. Próximos Passos

1. [x] Completar pesquisa web (Fase 1) - 12 URLs navegadas
2. [x] Adicionar regras de ATO - 2 regras adicionadas
3. [x] Adicionar regras de Mule Account - 2 regras adicionadas
4. [x] Adicionar regras de APP Scam - 2 regras adicionadas
5. [ ] Adicionar regras específicas BR (Pix, MED) - Pendente (fontes BR não retornaram dados)
6. [ ] Validar thresholds com dados reais - A DEFINIR (CALIBRAÇÃO DETERMINÍSTICA)
7. [ ] Implementar regras aprovadas (Fase 5) - 14 regras prontas

## 6. Evidências Web

| Vendor | URL | Capacidades Extraídas |
|--------|-----|----------------------|
| FICO Falcon | https://www.fico.com/en/products/fico-falcon-fraud-manager | Real-time scoring, consortium data, case management |
| NICE Actimize | https://www.niceactimize.com/fraud-management | Scams & Mule defense, timing patterns, payment type rules |
| ACI Worldwide | https://www.aciworldwide.com/solutions/aci-fraud-management-banking | 85%+ detection, anomaly detection, SAR triggers |
| LexisNexis ThreatMetrix | https://risk.lexisnexis.com/global/en/products/threatmetrix | Digital identity, behavioral intelligence |
| Feedzai | https://www.feedzai.com/solutions/transaction-fraud/ | 3ms decisioning, behavioral data, false positive reduction |
| Kount | https://kount.com/fraud-detection-software | Risk score, policy evaluation, card testing detection |
| Sift | https://sift.com/solutions/fintech-finance/ | CNP fraud ($9.5B losses), ATO, chargeback patterns |
| BioCatch | https://www.biocatch.com/solutions/account-takeover-protection | ATO, mule detection, social engineering |
| HAWK:AI | https://hawk.ai/solutions/fraud/transaction-fraud | 150ms detection, self-serve rules, round amount rules |
| Verafin | https://verafin.com/product/fraud-detection-management/ | Cross-channel, consortium profiling, check/wire/ACH fraud |
