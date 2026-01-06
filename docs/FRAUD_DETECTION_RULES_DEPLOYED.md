# RULEX - Regras de Fraude Implantadas

**Versão:** 1.0.0  
**Data:** 2025-01-03  
**Fonte:** V22__fraud_detection_rules_seed.sql + V31__seed_precheck_rules.sql

---

## Atualizações (2025-01-03 19:00)
- Criação inicial do documento
- Inventário de regras existentes no seed V22

## Atualizações (2026-01-05)
- Inclusão das regras precheck (blocklist + travel) como regras DB-driven (V31)

---

## 1. Visão Geral

Este documento lista todas as regras de detecção de fraude **já implantadas** no RULEX via migration V22.

---

## 2. Regras Simples (rule_configurations)

### 2.1 Categoria: SECURITY

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 1 | CVV_MISMATCH | CVV não corresponde | 70 | 70 | SUSPICIOUS |
| 2 | PIN_VERIFICATION_FAILED | Verificação PIN falhou | 75 | 75 | SUSPICIOUS |
| 3 | CRYPTOGRAM_INVALID | Criptograma EMV inválido | 95 | 95 | FRAUD |
| 4 | ATC_MISMATCH | ATC card ≠ ATC host | 90 | 90 | FRAUD |
| 5 | LOW_AUTH_SCORE | Score autenticação < 50 | 65 | 65 | SUSPICIOUS |
| 6 | PIN_TRY_LIMIT_EXCEEDED | Limite tentativas PIN excedido | 95 | 95 | FRAUD |

### 2.2 Categoria: CONTEXT (MCC)

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 7 | MCC_GAMBLING_HIGH_VALUE | Gambling MCC + valor alto | 70 | 75 | SUSPICIOUS |
| 8 | MCC_CRYPTO_QUASI_CASH | Crypto/Quasi-cash MCC | 75 | 80 | SUSPICIOUS |
| 9 | MCC_WIRE_TRANSFER | Wire transfer MCC | 65 | 70 | SUSPICIOUS |
| 10 | MCC_ADULT_CONTENT | Adult content MCC | 60 | 65 | SUSPICIOUS |
| 11 | MCC_PAWN_SHOP | Pawn shop MCC | 55 | 60 | SUSPICIOUS |
| 12 | MCC_MONEY_ORDER | Money order MCC | 65 | 70 | SUSPICIOUS |

### 2.3 Categoria: CONTEXT (Valor)

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 13 | HIGH_VALUE_TRANSACTION | Transação > R$ 5.000 | 70 | 70 | SUSPICIOUS |
| 14 | EXCEEDS_AVAILABLE_CREDIT | Valor > crédito disponível | 90 | 90 | FRAUD |
| 15 | ROUND_AMOUNT_STRUCTURING | Valor redondo (structuring) | 60 | 60 | SUSPICIOUS |
| 16 | JUST_BELOW_LIMIT | Valor logo abaixo do limite | 55 | 55 | SUSPICIOUS |

### 2.4 Categoria: CONTEXT (Geolocalização)

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 17 | INTERNATIONAL_TRANSACTION | País ≠ Brasil | 50 | 50 | SUSPICIOUS |
| 18 | HIGH_RISK_COUNTRY | País FATF alto risco | 85 | 85 | FRAUD |
| 19 | IMPOSSIBLE_TRAVEL | Viagem impossível (geo) | 90 | 90 | FRAUD |

### 2.5 Categoria: CONTEXT (Horário)

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 20 | NIGHT_TRANSACTION | Transação 00:00-06:00 | 40 | 40 | SUSPICIOUS |
| 21 | WEEKEND_HIGH_VALUE | Final de semana + valor alto | 50 | 50 | SUSPICIOUS |

### 2.6 Categoria: CONTEXT (Canal/POS)

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 22 | ECOMMERCE_HIGH_VALUE | E-commerce + valor alto | 70 | 70 | SUSPICIOUS |
| 23 | MANUAL_ENTRY_HIGH_VALUE | Entrada manual + valor alto | 75 | 75 | SUSPICIOUS |
| 24 | CARD_NOT_PRESENT | Cliente não presente | 45 | 45 | SUSPICIOUS |
| 25 | FALLBACK_TRANSACTION | Fallback EMV | 70 | 70 | SUSPICIOUS |
| 26 | OFF_PREMISES_HIGH_VALUE | Fora do estabelecimento | 65 | 65 | SUSPICIOUS |

### 2.7 Categoria: VELOCITY

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 27 | HIGH_FREQUENCY_PAN | > 5 tx em 60 min | 75 | 75 | SUSPICIOUS |
| 28 | DAILY_AMOUNT_LIMIT | > R$ 10.000 em 24h | 80 | 80 | SUSPICIOUS |
| 29 | MULTIPLE_MERCHANTS_24H | > 5 merchants em 24h | 70 | 70 | SUSPICIOUS |
| 30 | MULTIPLE_COUNTRIES_24H | > 2 países em 24h | 90 | 90 | FRAUD |
| 31 | CARD_TESTING_PATTERN | Card testing (BIN attack) | 95 | 95 | FRAUD |
| 32 | RAPID_SUCCESSION | Transações em sequência rápida | 70 | 70 | SUSPICIOUS |

### 2.8 Categoria: ANOMALY

| # | rule_name | Descrição | threshold | weight | classification |
|---|-----------|-----------|-----------|--------|----------------|
| 33 | FIRST_INTERNATIONAL | Primeira transação internacional | 55 | 55 | SUSPICIOUS |
| 34 | UNUSUAL_MCC | MCC incomum para cliente | 50 | 50 | SUSPICIOUS |
| 35 | AMOUNT_SPIKE | Pico de valor vs histórico | 65 | 65 | SUSPICIOUS |

---

## 3. Regras Complexas (complex_rules)

### 3.1 Regras Multi-Condição

| # | key | title | severity | decision |
|---|-----|-------|----------|----------|
| 1 | INTL_GAMBLING_NIGHT_COMPLEX | International Gambling at Night | 90 | FRAUDE |
| 2 | HIGH_VALUE_CNP_NEW_MERCHANT | High Value CNP New Merchant | 85 | FRAUDE |
| 3 | VELOCITY_GEOGRAPHIC_ANOMALY | Velocity + Geographic Anomaly | 88 | FRAUDE |
| 4 | STRUCTURING_PATTERN | Structuring Pattern Detection | 80 | SUSPEITA_DE_FRAUDE |
| 5 | CARD_TESTING_ADVANCED | Advanced Card Testing | 95 | FRAUDE |

---

## 4. Estatísticas

### 4.1 Por Categoria

| Categoria | Simples | Complexas | Total |
|-----------|---------|-----------|-------|
| SECURITY | 6 | 0 | 6 |
| CONTEXT | 15 | 2 | 17 |
| VELOCITY | 6 | 2 | 8 |
| ANOMALY | 3 | 1 | 4 |
| **TOTAL** | **30** | **5** | **35** |

### 4.2 Por Classificação

| Classificação | Quantidade |
|---------------|------------|
| FRAUD | 12 |
| SUSPICIOUS | 23 |
| **TOTAL** | **35** |

### 4.3 Por Severidade

| Faixa | Quantidade |
|-------|------------|
| 90-100 (Crítico) | 8 |
| 70-89 (Alto) | 15 |
| 50-69 (Médio) | 10 |
| 0-49 (Baixo) | 2 |

---

## 5. Campos Utilizados

### 5.1 Campos de Payload

| Campo | Regras que Usam |
|-------|-----------------|
| transactionAmount | 12 |
| mcc | 8 |
| merchantCountryCode | 5 |
| posEntryMode | 4 |
| transactionTime | 3 |
| cvv2Response | 1 |
| cryptogramValid | 1 |
| atcCard, atcHost | 1 |
| consumerAuthenticationScore | 1 |
| availableCredit | 1 |
| customerPresent | 1 |

### 5.2 Campos Derivados (Velocity)

| Campo | Descrição |
|-------|-----------|
| panHash | Hash SHA-256 do PAN |
| velocity_count | Contagem de transações |
| velocity_sum | Soma de valores |
| velocity_distinct | Contagem de distintos |

---

## 6. Operadores Utilizados

| Operador | Quantidade |
|----------|------------|
| EQ | 8 |
| NEQ | 3 |
| GT | 10 |
| LT | 2 |
| IN | 12 |
| BETWEEN | 3 |
| FIELD_GT | 1 |
| FIELD_NEQ | 1 |
| TIME_BETWEEN | 2 |
| MOD_EQ | 1 |
| VELOCITY_COUNT_GT | 3 |
| VELOCITY_SUM_GT | 2 |
| VELOCITY_DISTINCT_GT | 2 |

---

## 7. Referências

- **Migration**: `backend/src/main/resources/db/migration/V22__fraud_detection_rules_seed.sql`
- **Linhas**: 1-1053
- **Tabelas**: `rule_configurations`, `complex_rules`, `rule_condition_groups`, `rule_conditions`
