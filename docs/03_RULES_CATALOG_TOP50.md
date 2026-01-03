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

*[A ser preenchido após navegação das URLs na Fase 1]*

### Categoria: ATO (Account Takeover)

#### RULE-027: SUDDEN_PROFILE_CHANGE (A IMPLEMENTAR)
| Atributo | Valor |
|----------|-------|
| **ID** | SUDDEN_PROFILE_CHANGE |
| **Tipologia** | ATO - Profile Change |
| **Status** | TEÓRICA |
| **Confiança** | MÉDIA |
| **Evidência** | A DEFINIR (pesquisa web) |
| **Condição** | A DEFINIR |
| **Campos Necessários** | Histórico de transações (velocity) |
| **Payload Check** | ⚠️ VALIDAR |

### Categoria: Mule Account

#### RULE-028: RAPID_FUND_MOVEMENT (A IMPLEMENTAR)
| Atributo | Valor |
|----------|-------|
| **ID** | RAPID_FUND_MOVEMENT |
| **Tipologia** | Mule Account |
| **Status** | TEÓRICA |
| **Confiança** | MÉDIA |
| **Evidência** | A DEFINIR |
| **Condição** | Múltiplas transferências em curto período |
| **Payload Check** | ⚠️ VALIDAR |

---

## 4. Resumo de Cobertura

| Categoria | Implementadas | A Implementar | Total |
|-----------|---------------|---------------|-------|
| SECURITY | 6 | 0 | 6 |
| CONTEXT (MCC) | 4 | 0 | 4 |
| CONTEXT (Valor) | 3 | 0 | 3 |
| CONTEXT (Geo) | 2 | 0 | 2 |
| CONTEXT (Horário) | 1 | 0 | 1 |
| CONTEXT (Canal) | 4 | 0 | 4 |
| VELOCITY | 5 | 0 | 5 |
| COMPLEX | 1 | 0 | 1 |
| ATO | 0 | 1 | 1 |
| MULE | 0 | 1 | 1 |
| **TOTAL** | **26** | **2** | **28** |

---

## 5. Próximos Passos

1. [ ] Completar pesquisa web (Fase 1)
2. [ ] Adicionar regras de ATO
3. [ ] Adicionar regras de Mule Account
4. [ ] Adicionar regras de APP Scam
5. [ ] Adicionar regras específicas BR (Pix, MED)
6. [ ] Validar thresholds com dados reais
7. [ ] Implementar regras aprovadas (Fase 5)
