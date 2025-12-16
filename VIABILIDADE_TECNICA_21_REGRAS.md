# 🔧 VIABILIDADE TÉCNICA - 21 REGRAS PAYLOAD-ONLY

## Implementação SQL/Java para Regras Aprovadas

---

## REGRA 1: AUTH_SCORE_CRITICAL

### Condição
```
consumerAuthenticationScore < 50
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN consumerAuthenticationScore < 50 THEN 'FRAUD'
        ELSE 'PASS'
    END AS classification,
    85 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateAuthScoreCritical(Transaction tx) {
    if (tx.getConsumerAuthenticationScore() < 50) {
        return new RuleResult(
            "AUTH_SCORE_CRITICAL",
            Classification.FRAUD,
            85,
            "Score de autenticação crítico: " + tx.getConsumerAuthenticationScore()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 2: AUTH_SCORE_LOW

### Condição
```
consumerAuthenticationScore < 100 AND consumerAuthenticationScore >= 50
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN consumerAuthenticationScore >= 50 AND consumerAuthenticationScore < 100 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    70 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateAuthScoreLow(Transaction tx) {
    int score = tx.getConsumerAuthenticationScore();
    if (score >= 50 && score < 100) {
        return new RuleResult(
            "AUTH_SCORE_LOW",
            Classification.SUSPICIOUS,
            70,
            "Score de autenticação baixo: " + score
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 3: AUTH_EXTERNAL_SCORE_LOW

### Condição
```
externalScore3 < 50
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN externalScore3 < 50 THEN 'FRAUD'
        ELSE 'PASS'
    END AS classification,
    80 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateExternalScoreLow(Transaction tx) {
    if (tx.getExternalScore3() < 50) {
        return new RuleResult(
            "AUTH_EXTERNAL_SCORE_LOW",
            Classification.FRAUD,
            80,
            "Score externo crítico: " + tx.getExternalScore3()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 4: AMOUNT_EXTREME_OUTLIER

### Condição
```
transactionAmount > 30000 OR transactionAmount < 0.01
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN transactionAmount > 30000 OR transactionAmount < 0.01 THEN 'FRAUD'
        ELSE 'PASS'
    END AS classification,
    90 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateAmountExtremeOutlier(Transaction tx) {
    BigDecimal amount = tx.getTransactionAmount();
    if (amount.compareTo(new BigDecimal("30000")) > 0 || 
        amount.compareTo(new BigDecimal("0.01")) < 0) {
        return new RuleResult(
            "AMOUNT_EXTREME_OUTLIER",
            Classification.FRAUD,
            90,
            "Valor extremo: " + amount
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 5: AMOUNT_HIGH_SCORE_LOW

### Condição
```
transactionAmount > 5000 AND consumerAuthenticationScore < 100
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN transactionAmount > 5000 AND consumerAuthenticationScore < 100 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    80 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateAmountHighScoreLow(Transaction tx) {
    if (tx.getTransactionAmount().compareTo(new BigDecimal("5000")) > 0 &&
        tx.getConsumerAuthenticationScore() < 100) {
        return new RuleResult(
            "AMOUNT_HIGH_SCORE_LOW",
            Classification.SUSPICIOUS,
            80,
            "Valor alto com score baixo: " + tx.getTransactionAmount() + 
            " / Score: " + tx.getConsumerAuthenticationScore()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 6: MCC_CRITICAL_RISK

### Condição
```
mcc IN (7995, 7994, 5967, 7841, 7273, 5122, 5912, 6051, 8398)
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN mcc IN (7995, 7994, 5967, 7841, 7273, 5122, 5912, 6051, 8398) THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    50 AS weight_bonus
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
private static final Set<Integer> CRITICAL_RISK_MCCS = Set.of(
    7995, 7994, 5967, 7841, 7273, 5122, 5912, 6051, 8398
);

public RuleResult evaluateMccCriticalRisk(Transaction tx) {
    if (CRITICAL_RISK_MCCS.contains(tx.getMcc())) {
        return new RuleResult(
            "MCC_CRITICAL_RISK",
            Classification.SUSPICIOUS,
            50,
            "MCC de altíssimo risco: " + tx.getMcc()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 7: MCC_HIGH_RISK

### Condição
```
mcc IN (4829, 5094, 5511, 5968, 7021, 7991, 7922, 4722)
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN mcc IN (4829, 5094, 5511, 5968, 7021, 7991, 7922, 4722) THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    30 AS weight_bonus
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
private static final Set<Integer> HIGH_RISK_MCCS = Set.of(
    4829, 5094, 5511, 5968, 7021, 7991, 7922, 4722
);

public RuleResult evaluateMccHighRisk(Transaction tx) {
    if (HIGH_RISK_MCCS.contains(tx.getMcc())) {
        return new RuleResult(
            "MCC_HIGH_RISK",
            Classification.SUSPICIOUS,
            30,
            "MCC de alto risco: " + tx.getMcc()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 8: MCC_HIGH_RISK_SMALL_AMOUNT

### Condição
```
mcc IN (alto_risco) AND transactionAmount < 10
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN mcc IN (7995, 7994, 5967, 7841, 7273, 5122, 5912, 6051, 8398, 
                     4829, 5094, 5511, 5968, 7021, 7991, 7922, 4722) 
             AND transactionAmount < 10 THEN 'FRAUD'
        ELSE 'PASS'
    END AS classification,
    85 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
private static final Set<Integer> ALL_HIGH_RISK_MCCS = Set.of(
    7995, 7994, 5967, 7841, 7273, 5122, 5912, 6051, 8398,
    4829, 5094, 5511, 5968, 7021, 7991, 7922, 4722
);

public RuleResult evaluateMccHighRiskSmallAmount(Transaction tx) {
    if (ALL_HIGH_RISK_MCCS.contains(tx.getMcc()) &&
        tx.getTransactionAmount().compareTo(new BigDecimal("10")) < 0) {
        return new RuleResult(
            "MCC_HIGH_RISK_SMALL_AMOUNT",
            Classification.FRAUD,
            85,
            "MCC alto risco com valor pequeno (card testing): " + 
            tx.getMcc() + " / " + tx.getTransactionAmount()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 9: MCC_GAMBLING_HIGH_AMOUNT

### Condição
```
mcc = 7995 AND transactionAmount > 5000
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN mcc = 7995 AND transactionAmount > 5000 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    80 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateMccGamblingHighAmount(Transaction tx) {
    if (tx.getMcc() == 7995 &&
        tx.getTransactionAmount().compareTo(new BigDecimal("5000")) > 0) {
        return new RuleResult(
            "MCC_GAMBLING_HIGH_AMOUNT",
            Classification.SUSPICIOUS,
            80,
            "Gambling com valor muito alto: " + tx.getTransactionAmount()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 10: MCC_CRYPTO_NO_AUTH

### Condição
```
mcc = 6051 AND eciIndicator = 7
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN mcc = 6051 AND eciIndicator = 7 THEN 'FRAUD'
        ELSE 'PASS'
    END AS classification,
    85 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateMccCryptoNoAuth(Transaction tx) {
    if (tx.getMcc() == 6051 && tx.getEciIndicator() == 7) {
        return new RuleResult(
            "MCC_CRYPTO_NO_AUTH",
            Classification.FRAUD,
            85,
            "Cryptocurrency sem autenticação 3DS"
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 11: MCC_MODERATE_RISK

### Condição
```
mcc IN (5964, 5966, 5969, 5921, 5993, 4814, 4816)
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN mcc IN (5964, 5966, 5969, 5921, 5993, 4814, 4816) THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    10 AS weight_bonus
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
private static final Set<Integer> MODERATE_RISK_MCCS = Set.of(
    5964, 5966, 5969, 5921, 5993, 4814, 4816
);

public RuleResult evaluateMccModerateRisk(Transaction tx) {
    if (MODERATE_RISK_MCCS.contains(tx.getMcc())) {
        return new RuleResult(
            "MCC_MODERATE_RISK",
            Classification.SUSPICIOUS,
            10,
            "MCC de risco moderado: " + tx.getMcc()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 12: CARD_EXPIRED

### Condição
```
cardExpireDate < transactionDate
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN cardExpireDate < transactionDate THEN 'FRAUD'
        ELSE 'PASS'
    END AS classification,
    95 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateCardExpired(Transaction tx) {
    // cardExpireDate formato: YYYYMMDD (20211029)
    // transactionDate formato: YYYYMMDD (20250210)
    if (tx.getCardExpireDate() < tx.getTransactionDate()) {
        return new RuleResult(
            "CARD_EXPIRED",
            Classification.FRAUD,
            95,
            "Cartão expirado: " + tx.getCardExpireDate() + 
            " / Transação: " + tx.getTransactionDate()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 13: CARD_NEAR_EXPIRY

### Condição
```
(cardExpireDate - transactionDate) <= 30 dias AND transactionAmount > 1000
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN DATEDIFF(
                STR_TO_DATE(CAST(cardExpireDate AS CHAR), '%Y%m%d'),
                STR_TO_DATE(CAST(transactionDate AS CHAR), '%Y%m%d')
             ) <= 30 
             AND transactionAmount > 1000 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    60 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateCardNearExpiry(Transaction tx) {
    // Converter YYYYMMDD para LocalDate
    LocalDate expiry = parseYYYYMMDD(tx.getCardExpireDate());
    LocalDate txDate = parseYYYYMMDD(tx.getTransactionDate());
    
    long daysDiff = ChronoUnit.DAYS.between(txDate, expiry);
    
    if (daysDiff <= 30 && daysDiff >= 0 &&
        tx.getTransactionAmount().compareTo(new BigDecimal("1000")) > 0) {
        return new RuleResult(
            "CARD_NEAR_EXPIRY",
            Classification.SUSPICIOUS,
            60,
            "Cartão próximo de expirar (" + daysDiff + " dias) com valor alto: " + 
            tx.getTransactionAmount()
        );
    }
    return RuleResult.pass();
}

private LocalDate parseYYYYMMDD(int yyyymmdd) {
    String str = String.valueOf(yyyymmdd);
    int year = Integer.parseInt(str.substring(0, 4));
    int month = Integer.parseInt(str.substring(4, 6));
    int day = Integer.parseInt(str.substring(6, 8));
    return LocalDate.of(year, month, day);
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 14: EXTERNAL_SCORE_CRITICAL

### Condição
```
externalScore3 < 50
```

### Implementação: **IDÊNTICA À REGRA 3**

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 15: EXTERNAL_SCORE_LOW

### Condição
```
externalScore3 < 100 AND externalScore3 >= 50
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN externalScore3 >= 50 AND externalScore3 < 100 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    70 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateExternalScoreLow(Transaction tx) {
    int score = tx.getExternalScore3();
    if (score >= 50 && score < 100) {
        return new RuleResult(
            "EXTERNAL_SCORE_LOW",
            Classification.SUSPICIOUS,
            70,
            "Score externo baixo: " + score
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 16: EXTERNAL_SCORE_INCONSISTENT

### Condição
```
ABS(externalScore3 - consumerAuthenticationScore) > 100
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN ABS(externalScore3 - consumerAuthenticationScore) > 100 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    65 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateExternalScoreInconsistent(Transaction tx) {
    int diff = Math.abs(tx.getExternalScore3() - tx.getConsumerAuthenticationScore());
    if (diff > 100) {
        return new RuleResult(
            "EXTERNAL_SCORE_INCONSISTENT",
            Classification.SUSPICIOUS,
            65,
            "Score externo inconsistente com score de autenticação: " + 
            tx.getExternalScore3() + " vs " + tx.getConsumerAuthenticationScore()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 17: MERCHANT_INVALID_POSTAL_CODE

### Condição
```
merchantPostalCode = INVALID OR merchantPostalCode = NULL
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN merchantPostalCode IS NULL OR merchantPostalCode = '' THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    70 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateMerchantInvalidPostalCode(Transaction tx) {
    String postalCode = tx.getMerchantPostalCode();
    if (postalCode == null || postalCode.trim().isEmpty()) {
        return new RuleResult(
            "MERCHANT_INVALID_POSTAL_CODE",
            Classification.SUSPICIOUS,
            70,
            "Merchant com CEP inválido ou ausente"
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 18: CONTEXT_ABSENT_NO_AUTH

### Condição
```
customerPresent = false AND eciIndicator = 7
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN customerPresent = 'N' AND eciIndicator = 7 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    70 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateContextAbsentNoAuth(Transaction tx) {
    if ("N".equals(tx.getCustomerPresent()) && tx.getEciIndicator() == 7) {
        return new RuleResult(
            "CONTEXT_ABSENT_NO_AUTH",
            Classification.SUSPICIOUS,
            70,
            "Transação CNP sem autenticação 3DS"
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 19: CONTEXT_CASH_ADVANCE

### Condição
```
transactionType = "cash_advance"
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN transactionType = 'cash_advance' THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    75 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateContextCashAdvance(Transaction tx) {
    if ("cash_advance".equals(tx.getTransactionType())) {
        return new RuleResult(
            "CONTEXT_CASH_ADVANCE",
            Classification.SUSPICIOUS,
            75,
            "Transação de saque em dinheiro (alto risco)"
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 20: GEO_HIGH_RISK_COUNTRY

### Condição
```
merchantCountryCode IN (lista países alto risco) AND transactionAmount > 100
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN merchantCountryCode IN ('RU', 'CN', 'NG', 'PK', 'VN', 'ID', 'UA', 'RO', 'BG') 
             AND transactionAmount > 100 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    60 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
private static final Set<String> HIGH_RISK_COUNTRIES = Set.of(
    "RU", "CN", "NG", "PK", "VN", "ID", "UA", "RO", "BG"
);

public RuleResult evaluateGeoHighRiskCountry(Transaction tx) {
    if (HIGH_RISK_COUNTRIES.contains(tx.getMerchantCountryCode()) &&
        tx.getTransactionAmount().compareTo(new BigDecimal("100")) > 0) {
        return new RuleResult(
            "GEO_HIGH_RISK_COUNTRY",
            Classification.SUSPICIOUS,
            60,
            "Transação em país de alto risco: " + tx.getMerchantCountryCode()
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## REGRA 21: TIME_HIGH_RISK_HOUR

### Condição
```
HOUR(transactionTime) = 7
```

### Implementação SQL
```sql
SELECT 
    CASE 
        WHEN FLOOR(transactionTime / 10000) = 7 THEN 'SUSPICIOUS'
        ELSE 'PASS'
    END AS classification,
    50 AS weight
FROM transactions
WHERE transactionId = ?;
```

### Implementação Java
```java
public RuleResult evaluateTimeHighRiskHour(Transaction tx) {
    // transactionTime formato: HHMMSS (11413 = 01:14:13)
    int hour = tx.getTransactionTime() / 10000;
    
    if (hour == 7) {
        return new RuleResult(
            "TIME_HIGH_RISK_HOUR",
            Classification.SUSPICIOUS,
            50,
            "Transação em hora de alto risco: " + hour + "h"
        );
    }
    return RuleResult.pass();
}
```

### Viabilidade: ✅ **100% VIÁVEL**

---

## 📊 RESUMO DE VIABILIDADE TÉCNICA

| Regra | SQL | Java | Complexidade | Viabilidade |
|-------|-----|------|--------------|-------------|
| 1. AUTH_SCORE_CRITICAL | ✅ | ✅ | Baixa | 100% |
| 2. AUTH_SCORE_LOW | ✅ | ✅ | Baixa | 100% |
| 3. AUTH_EXTERNAL_SCORE_LOW | ✅ | ✅ | Baixa | 100% |
| 4. AMOUNT_EXTREME_OUTLIER | ✅ | ✅ | Baixa | 100% |
| 5. AMOUNT_HIGH_SCORE_LOW | ✅ | ✅ | Média | 100% |
| 6. MCC_CRITICAL_RISK | ✅ | ✅ | Baixa | 100% |
| 7. MCC_HIGH_RISK | ✅ | ✅ | Baixa | 100% |
| 8. MCC_HIGH_RISK_SMALL_AMOUNT | ✅ | ✅ | Média | 100% |
| 9. MCC_GAMBLING_HIGH_AMOUNT | ✅ | ✅ | Média | 100% |
| 10. MCC_CRYPTO_NO_AUTH | ✅ | ✅ | Média | 100% |
| 11. MCC_MODERATE_RISK | ✅ | ✅ | Baixa | 100% |
| 12. CARD_EXPIRED | ✅ | ✅ | Baixa | 100% |
| 13. CARD_NEAR_EXPIRY | ✅ | ✅ | Alta | 100% |
| 14. EXTERNAL_SCORE_CRITICAL | ✅ | ✅ | Baixa | 100% |
| 15. EXTERNAL_SCORE_LOW | ✅ | ✅ | Baixa | 100% |
| 16. EXTERNAL_SCORE_INCONSISTENT | ✅ | ✅ | Média | 100% |
| 17. MERCHANT_INVALID_POSTAL_CODE | ✅ | ✅ | Baixa | 100% |
| 18. CONTEXT_ABSENT_NO_AUTH | ✅ | ✅ | Média | 100% |
| 19. CONTEXT_CASH_ADVANCE | ✅ | ✅ | Baixa | 100% |
| 20. GEO_HIGH_RISK_COUNTRY | ✅ | ✅ | Baixa | 100% |
| 21. TIME_HIGH_RISK_HOUR | ✅ | ✅ | Média | 100% |

---

## 🎯 CONCLUSÃO

**TODAS as 21 regras PAYLOAD-ONLY são 100% VIÁVEIS** para implementação em SQL/Java puro.

**Complexidade**:
- **Baixa** (10 regras): Comparação simples de valores
- **Média** (9 regras): Combinação de 2-3 condições
- **Alta** (2 regras): Parsing de data/hora (YYYYMMDD, HHMMSS)

**Tempo de Implementação Estimado**:
- **Backend Java**: 4-6 horas (21 métodos + testes unitários)
- **SQL Queries**: 2-3 horas (21 queries + otimização)
- **Integração**: 2 horas (controller + service)
- **Testes**: 3-4 horas (unit tests + integration tests)

**Total**: 11-15 horas de desenvolvimento

---

## 🚀 PRÓXIMOS PASSOS

1. ✅ Implementar as 21 regras no `AdvancedRuleEngineService.java`
2. ✅ Criar testes unitários para cada regra
3. ✅ Adicionar endpoint `/api/transactions/analyze-payload-only`
4. ✅ Atualizar frontend para exibir regras PAYLOAD-ONLY
5. ✅ Documentar cada regra no sistema
