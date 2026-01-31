# 🏗️ RULEX ARCHITECTURE IMPLEMENTATION PLAN

**Data**: 12 de Janeiro de 2026  
**Autor**: Arquiteto de Software  
**Versão**: 1.0.0  
**Baseado em**: QUADRUPLE-CHECK 1000X + Roadmaps anteriores

---

## 📋 ÍNDICE

1. [Visão Geral da Arquitetura Atual](#1-visão-geral-da-arquitetura-atual)
2. [Arquitetura Proposta V3.0](#2-arquitetura-proposta-v30)
3. [Implementação por Prioridade](#3-implementação-por-prioridade)
4. [Detalhamento Técnico por Sprint](#4-detalhamento-técnico-por-sprint)
5. [Padrões e Convenções](#5-padrões-e-convenções)
6. [Testes e Qualidade](#6-testes-e-qualidade)

---

## 1. VISÃO GERAL DA ARQUITETURA ATUAL

### 1.1 Estrutura de Pacotes

```
backend/src/main/java/com/rulex/
├── api/                          # API versioning
├── config/                       # Spring configurations
├── controller/                   # REST endpoints
├── dto/                          # Data Transfer Objects
├── entity/                       # JPA entities
│   └── complex/                  # Complex rule entities
│       ├── RuleCondition.java    # 110 ConditionOperators
│       └── RuleConditionGroup.java
├── repository/                   # Spring Data repositories
├── service/                      # Business logic
│   ├── complex/                  # Complex rule evaluation
│   │   └── ComplexRuleEvaluator.java  # 2,222 linhas, 93 cases
│   ├── enrichment/               # 8 arquivos de enrichment
│   │   ├── AnomalyEnrichment.java     # 400 linhas
│   │   ├── AuthEnrichment.java        # 322 linhas
│   │   ├── CardEnrichment.java        # 373 linhas
│   │   ├── CustomerEnrichment.java    # 352 linhas
│   │   ├── DeviceEnrichment.java      # 392 linhas
│   │   ├── GeoEnrichment.java         # 334 linhas
│   │   ├── VelocityEnrichment.java    # 307 linhas
│   │   └── TransactionEnrichmentFacade.java  # 345 linhas ⚠️
│   ├── EnrichmentService.java    # BIN/MCC enrichment
│   ├── RuleEngineService.java    # ~120 linhas - Adapter (porta de entrada)
│   ├── core/engine/usecase/RuleEngineUseCase.java  # ~1.000 linhas - Core engine
│   ├── VelocityService.java      # 380 linhas - 11 campos
│   └── ... (30+ services)
└── util/                         # Utilities
```

### 1.2 Fluxo Atual de Avaliação

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          FLUXO ATUAL (INCOMPLETO)                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  TransactionRequest                                                          │
│        │                                                                     │
│        ▼                                                                     │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │                    RuleEngineService.analyzeTransaction()            │    │
│  │                         (adapter fino)                               │    │
│  │  ┌──────────────────────────────────────────────────────────────┐   │    │
│  │  │  TransactionEnrichmentFacade.enrichFull() ✅ INTEGRADO       │   │    │
│  │  └──────────────────────────────────────────────────────────────┘   │    │
│  │                             │                                        │    │
│  │                             ▼                                        │    │
│  │  ┌──────────────────────────────────────────────────────────────┐   │    │
│  │  │  ComplexRuleEvaluator.evaluate()                              │   │    │
│  │  │  - 110 operadores no enum                                     │   │    │
│  │  │  - 93 cases implementados                                     │   │    │
│  │  │  - 17 operadores retornam FALSE! ❌                           │   │    │
│  │  └──────────────────────────────────────────────────────────────┘   │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│        │                                                                     │
│        ▼                                                                     │
│  TransactionResponse                                                         │
│                                                                              │
│  ✅ TransactionEnrichmentFacade integrado via port/use case.                │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.3 Gaps Identificados

| # | Gap | Localização | Impacto |
|---|-----|-------------|---------|
| 1 | 17 operadores sem case | ComplexRuleEvaluator.java | Regras falham silenciosamente |
| 2 | TransactionEnrichmentFacade integrado (resolvido) | RuleEngineUseCase.java | Resolvido |
| 3 | VelocityStats falta 10 campos | VelocityService.java | Operadores não funcionam |
| 4 | 5 formatos valueSingle diferentes | ComplexRuleEvaluator.java | Inconsistência |

---

## 2. ARQUITETURA PROPOSTA V3.0

### 2.1 Diagrama de Componentes

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          ARQUITETURA V3.0 PROPOSTA                          │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  TransactionRequest                                                          │
│        │                                                                     │
│        ▼                                                                     │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │                    RuleEngineService.analyzeTransaction()            │    │
│  │                                                                      │    │
│  │  ┌──────────────────────────────────────────────────────────────┐   │    │
│  │  │  TransactionEnrichmentFacade.enrichFull() ✅ INTEGRAR        │   │    │
│  │  │       │                                                       │   │    │
│  │  │       ├── AuthEnrichment.enrich()                            │   │    │
│  │  │       ├── VelocityEnrichment.enrich()                        │   │    │
│  │  │       ├── DeviceEnrichment.enrich()                          │   │    │
│  │  │       ├── GeoEnrichment.enrich()                             │   │    │
│  │  │       ├── CustomerEnrichment.enrich()                        │   │    │
│  │  │       ├── CardEnrichment.enrich()                            │   │    │
│  │  │       └── AnomalyEnrichment.enrich()                         │   │    │
│  │  │                                                               │   │    │
│  │  │  FullEnrichmentContext → toFlatMap() → 100+ campos derivados │   │    │
│  │  └──────────────────────────────────────────────────────────────┘   │    │
│  │                             │                                        │    │
│  │                             ▼                                        │    │
│  │  ┌──────────────────────────────────────────────────────────────┐   │    │
│  │  │  ComplexRuleEvaluator.evaluate(enrichedPayload)               │   │    │
│  │  │  - 110 operadores implementados ✅ (0 pendentes)              │   │    │
│  │  │  - ValueSingleParser unificado ✅                             │   │    │
│  │  └──────────────────────────────────────────────────────────────┘   │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│        │                                                                     │
│        ▼                                                                     │
│  TransactionResponse (com explicação completa)                              │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 2.2 Novos Componentes (Fase 2+)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          COMPONENTES FUTUROS (FASE 2-4)                     │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  LAYER 5: GRAPH ANALYTICS (Neo4j)                                           │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  GraphAnalyticsService                                                  │ │
│  │       │                                                                 │ │
│  │       ├── Neo4jClient (Spring Data Neo4j)                              │ │
│  │       ├── MoneyMuleDetectionService                                    │ │
│  │       ├── CommunityDetectionService                                    │ │
│  │       └── PathAnalysisService                                          │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│  LAYER 6: REGULATORY COMPLIANCE                                             │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  RegulatoryComplianceService                                            │ │
│  │       │                                                                 │ │
│  │       ├── FATF40Service                                                │ │
│  │       ├── PSD3Service                                                  │ │
│  │       ├── DORAService                                                  │ │
│  │       └── SanctionsScreeningService                                    │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│  LAYER 7: FEDERATED RULES                                                   │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  FederatedRulesService                                                  │ │
│  │       │                                                                 │ │
│  │       ├── KafkaRuleSync (encrypted)                                    │ │
│  │       ├── ConsensusEngine                                              │ │
│  │       └── RuleProposalService                                          │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
│  ISO 20022 PARSER                                                            │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │  ISO20022ParserService                                                  │ │
│  │       │                                                                 │ │
│  │       ├── Pacs008Parser (pain.001, pacs.008, camt.053)                 │ │
│  │       ├── OriginatorValidationService                                  │ │
│  │       └── RemittanceAnomalyDetector                                    │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. IMPLEMENTAÇÃO POR PRIORIDADE

### 3.1 PRIORIDADE 1 - CRÍTICO (Semanas 1-2)

#### 3.1.1 Integrar TransactionEnrichmentFacade

**Arquivo**: `RuleEngineService.java`

**Mudança**:
```java
// ANTES (linha 59):
private final EnrichmentService enrichmentService;

// DEPOIS:
private final EnrichmentService enrichmentService;
private final TransactionEnrichmentFacade transactionEnrichmentFacade; // ADICIONAR

// No construtor (gerado pelo @RequiredArgsConstructor do Lombok)
// Lombok irá adicionar automaticamente

// No método analyzeTransaction(), ANTES de chamar ComplexRuleEvaluator:
public TransactionResponse analyzeTransaction(TransactionRequest request, 
                                               byte[] rawPayload, 
                                               String contentType) {
    // ... código existente ...
    
    // ADICIONAR: Enriquecer transação com TODOS os 7 enrichments
    FullEnrichmentContext enrichedContext = transactionEnrichmentFacade.enrichFull(request);
    Map<String, Object> enrichedPayload = enrichedContext.toFlatMap();
    
    // Passar enrichedPayload para o EvaluationContext
    EvaluationContext ctx = EvaluationContext.builder()
        .payload(enrichedPayload)  // ← Agora com 100+ campos derivados
        .transactionRequest(request)
        .build();
    
    // ... resto do código ...
}
```

**Estimativa**: 3 story points  
**Risco**: Baixo (TransactionEnrichmentFacade já existe e está testado)

---

#### 3.1.2 Implementar 17 Operadores Pendentes

**Arquivo**: `ComplexRuleEvaluator.java` (linhas 220-379)

**Padrão de Implementação**:
```java
// Adicionar case para cada operador antes do "default"

// Exemplo 1: IN_LIST (alias para IN)
case IN_LIST -> evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());

// Exemplo 2: CONTAINS_SUSPICIOUS_KEYWORDS
case CONTAINS_SUSPICIOUS_KEYWORDS -> evaluateContainsSuspiciousKeywords(fieldValue, condition);

// Exemplo 3: COUNT_LAST_N_DAYS
case COUNT_LAST_N_DAYS -> evaluateCountLastNDays(condition, context);

// Exemplo 4: DAYS_SINCE_LAST_ACTIVITY  
case DAYS_SINCE_LAST_ACTIVITY -> evaluateDaysSinceLastActivity(condition, context);

// Exemplo 5: DEVICE_CHANGED_IN_SESSION
case DEVICE_CHANGED_IN_SESSION -> evaluateDeviceChangedInSession(condition, context);

// ... e assim por diante para os 17 operadores
```

**Novos Métodos a Criar**:
```java
// ========== OPERADORES V28-V30 (17 novos métodos) ==========

private boolean evaluateInList(Object fieldValue, RuleCondition condition) {
    // Alias para evaluateIn - compatibilidade com migrações
    return evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
}

private boolean evaluateHasFailed3dsLastNMinutes(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "minutes"
    int minutes = Integer.parseInt(condition.getValueSingle());
    Map<String, Object> payload = context.getPayload();
    
    // Verificar no AuthEnrichment
    Boolean failed3ds = (Boolean) payload.get("auth_has_failed_3ds_recently");
    Integer lastFailureMinutes = (Integer) payload.get("auth_last_3ds_failure_minutes_ago");
    
    return Boolean.TRUE.equals(failed3ds) && lastFailureMinutes != null && lastFailureMinutes <= minutes;
}

private boolean evaluateCountMfaAbandonments(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold:hours"
    String[] parts = condition.getValueSingle().split(":");
    int threshold = Integer.parseInt(parts[0]);
    int hours = parts.length > 1 ? Integer.parseInt(parts[1]) : 24;
    
    Map<String, Object> payload = context.getPayload();
    Integer count = (Integer) payload.get("auth_mfa_abandonment_count_" + hours + "h");
    
    return count != null && count >= threshold;
}

private boolean evaluateHasIncomingTransferLastNHours(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "hours"
    int hours = Integer.parseInt(condition.getValueSingle());
    Map<String, Object> payload = context.getPayload();
    
    Boolean hasIncoming = (Boolean) payload.get("velocity_has_incoming_transfer_" + hours + "h");
    return Boolean.TRUE.equals(hasIncoming);
}

private boolean evaluateIsImpossibleCombination(RuleCondition condition, EvaluationContext context) {
    // Verifica combinações impossíveis de dados (ex: idade < 18 com conta corporativa)
    Map<String, Object> payload = context.getPayload();
    
    // Lista de combinações impossíveis (configurável via valueSingle)
    String combinationType = condition.getValueSingle();
    
    return switch (combinationType) {
        case "age_corporate" -> {
            Integer age = (Integer) payload.get("customer_age");
            String accountType = (String) payload.get("account_type");
            yield age != null && age < 18 && "CORPORATE".equals(accountType);
        }
        case "country_currency" -> {
            String country = (String) payload.get("country");
            String currency = (String) payload.get("currency");
            // Moeda não compatível com país
            yield !isValidCurrencyForCountry(country, currency);
        }
        default -> false;
    };
}

private boolean evaluatePixKeyChangedLastNDays(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "days"
    int days = Integer.parseInt(condition.getValueSingle());
    Map<String, Object> payload = context.getPayload();
    
    Integer daysSinceChange = (Integer) payload.get("customer_pix_key_changed_days_ago");
    return daysSinceChange != null && daysSinceChange <= days;
}

private boolean evaluateContainsSuspiciousKeywords(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null) return false;
    
    String text = String.valueOf(fieldValue).toLowerCase();
    List<String> keywords = condition.getValueArray();
    
    if (keywords == null || keywords.isEmpty()) {
        // Keywords padrão se não especificado
        keywords = List.of("urgente", "transferir agora", "bloqueio", "segurança", 
                          "atualizar dados", "conta suspensa", "prêmio", "herança");
    }
    
    return keywords.stream()
        .map(String::toLowerCase)
        .anyMatch(text::contains);
}

private boolean evaluateCountCryptoTxnLastNDays(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|days"
    String[] parts = condition.getValueSingle().split("\\|");
    int threshold = Integer.parseInt(parts[0]);
    int days = parts.length > 1 ? Integer.parseInt(parts[1]) : 30;
    
    Map<String, Object> payload = context.getPayload();
    Long count = (Long) payload.get("velocity_crypto_txn_count_" + days + "d");
    
    return count != null && count >= threshold;
}

private boolean evaluateCountDistinctInstrumentsLastNDays(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|days"
    String[] parts = condition.getValueSingle().split("\\|");
    int threshold = Integer.parseInt(parts[0]);
    int days = parts.length > 1 ? Integer.parseInt(parts[1]) : 30;
    
    Map<String, Object> payload = context.getPayload();
    Long count = (Long) payload.get("velocity_distinct_instruments_" + days + "d");
    
    return count != null && count >= threshold;
}

private boolean evaluateCountDistinctPayersLastNDays(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|days"
    String[] parts = condition.getValueSingle().split("\\|");
    int threshold = Integer.parseInt(parts[0]);
    int days = parts.length > 1 ? Integer.parseInt(parts[1]) : 30;
    
    Map<String, Object> payload = context.getPayload();
    Long count = (Long) payload.get("velocity_distinct_payers_" + days + "d");
    
    return count != null && count >= threshold;
}

private boolean evaluateCountDistinctUserAgentsLastNHours(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|hours"
    String[] parts = condition.getValueSingle().split("\\|");
    int threshold = Integer.parseInt(parts[0]);
    int hours = parts.length > 1 ? Integer.parseInt(parts[1]) : 24;
    
    Map<String, Object> payload = context.getPayload();
    Long count = (Long) payload.get("device_distinct_user_agents_" + hours + "h");
    
    return count != null && count >= threshold;
}

private boolean evaluateCountLastNDays(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|days" ou "keyType|threshold|days"
    String[] parts = condition.getValueSingle().split("\\|");
    
    String keyType = parts.length == 3 ? parts[0] : "PAN";
    int threshold = parts.length == 3 ? Integer.parseInt(parts[1]) : Integer.parseInt(parts[0]);
    int days = parts.length == 3 ? Integer.parseInt(parts[2]) : 
               (parts.length == 2 ? Integer.parseInt(parts[1]) : 30);
    
    // Usar VelocityServiceFacade
    VelocityService.TimeWindow window = getTimeWindowForDays(days);
    VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
        context.getTransactionRequest(), 
        VelocityService.KeyType.valueOf(keyType), 
        window
    );
    
    return stats.getTransactionCount() >= threshold;
}

private boolean evaluateCountMfaDenialsLastNHours(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold:hours"
    String[] parts = condition.getValueSingle().split(":");
    int threshold = Integer.parseInt(parts[0]);
    int hours = parts.length > 1 ? Integer.parseInt(parts[1]) : 24;
    
    Map<String, Object> payload = context.getPayload();
    Integer count = (Integer) payload.get("auth_mfa_denial_count_" + hours + "h");
    
    return count != null && count >= threshold;
}

private boolean evaluateDaysSinceLastActivity(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|operator" (ex: "30|GT" = mais de 30 dias)
    String[] parts = condition.getValueSingle().split("\\|");
    int threshold = Integer.parseInt(parts[0]);
    String operator = parts.length > 1 ? parts[1] : "GT";
    
    Map<String, Object> payload = context.getPayload();
    Integer days = (Integer) payload.get("customer_days_since_last_activity");
    
    if (days == null) return false;
    
    return switch (operator) {
        case "GT" -> days > threshold;
        case "GTE" -> days >= threshold;
        case "LT" -> days < threshold;
        case "LTE" -> days <= threshold;
        case "EQ" -> days == threshold;
        default -> days > threshold;
    };
}

private boolean evaluateDeviceChangedInSession(RuleCondition condition, EvaluationContext context) {
    Map<String, Object> payload = context.getPayload();
    Boolean changed = (Boolean) payload.get("device_changed_in_session");
    return Boolean.TRUE.equals(changed);
}

private boolean evaluateIsCryptoRansomAmount(RuleCondition condition, EvaluationContext context) {
    Map<String, Object> payload = context.getPayload();
    BigDecimal amount = getBigDecimal(payload.get("amount"));
    
    if (amount == null) return false;
    
    // Valores típicos de ransom (BTC convertido)
    // 0.1 BTC, 0.25 BTC, 0.5 BTC, 1 BTC, 2 BTC (em valor fiat aproximado)
    List<BigDecimal> typicalRansomAmounts = List.of(
        new BigDecimal("500"),
        new BigDecimal("1000"),
        new BigDecimal("2500"),
        new BigDecimal("5000"),
        new BigDecimal("10000"),
        new BigDecimal("25000"),
        new BigDecimal("50000")
    );
    
    // Verificar se valor está próximo de valores típicos (±10%)
    for (BigDecimal typical : typicalRansomAmounts) {
        BigDecimal lower = typical.multiply(new BigDecimal("0.9"));
        BigDecimal upper = typical.multiply(new BigDecimal("1.1"));
        if (amount.compareTo(lower) >= 0 && amount.compareTo(upper) <= 0) {
            return true;
        }
    }
    
    return false;
}

private boolean evaluateOutflowRateLastNDays(RuleCondition condition, EvaluationContext context) {
    // valueSingle format: "threshold|days" (threshold é percentual de outflow)
    String[] parts = condition.getValueSingle().split("\\|");
    double threshold = Double.parseDouble(parts[0]);
    int days = parts.length > 1 ? Integer.parseInt(parts[1]) : 30;
    
    Map<String, Object> payload = context.getPayload();
    Double outflowRate = (Double) payload.get("velocity_outflow_rate_" + days + "d");
    
    return outflowRate != null && outflowRate >= threshold;
}

// Método auxiliar
private VelocityService.TimeWindow getTimeWindowForDays(int days) {
    return switch (days) {
        case 1 -> VelocityService.TimeWindow.HOUR_24;
        case 7 -> VelocityService.TimeWindow.DAY_7;
        case 30 -> VelocityService.TimeWindow.DAY_30;
        default -> VelocityService.TimeWindow.DAY_30;
    };
}

private boolean isValidCurrencyForCountry(String country, String currency) {
    // Mapeamento simplificado
    Map<String, String> countryToCurrency = Map.of(
        "BR", "BRL",
        "US", "USD",
        "GB", "GBP",
        "EU", "EUR"
        // ... expandir conforme necessário
    );
    String expected = countryToCurrency.get(country);
    return expected == null || expected.equals(currency);
}
```

**Estimativa**: 15 story points (17 operadores × ~1SP cada, com alguns mais complexos)  
**Risco**: Médio (dependem de campos nos enrichments)

---

#### 3.1.3 Expandir VelocityStats

**Arquivo**: `VelocityService.java` (linha 44)

**Mudança**:
```java
@Data
@Builder
public static class VelocityStats {
    // EXISTENTES (11 campos)
    private final long transactionCount;
    private final BigDecimal totalAmount;
    private final BigDecimal avgAmount;
    private final BigDecimal minAmount;
    private final BigDecimal maxAmount;
    private final long distinctMerchants;
    private final long distinctMccs;
    private final long distinctCountries;
    private final long fraudCount;
    private final boolean found;
    private final String source;
    
    // NOVOS (10 campos) - ADICIONAR
    private final long distinctPans;           // Para COUNT_DISTINCT_PANS
    private final long distinctDevices;        // Para device fingerprinting  
    private final long distinctIps;            // Para IP rotation detection
    private final long distinctUserAgents;     // Para bot detection
    private final long distinctBeneficiaries;  // Para money mule detection
    private final OffsetDateTime firstTransactionAt;  // Para days_since_first
    private final OffsetDateTime lastTransactionAt;   // Para days_since_last
    private final long chargebackCount;        // Para chargeback_rate
    private final long declinedCount;          // Para decline_rate
    private final long cryptoTransactionCount; // Para COUNT_CRYPTO_TXN
    
    public static VelocityStats empty() {
        return VelocityStats.builder()
            .transactionCount(0)
            .totalAmount(BigDecimal.ZERO)
            .avgAmount(BigDecimal.ZERO)
            .minAmount(BigDecimal.ZERO)
            .maxAmount(BigDecimal.ZERO)
            .distinctMerchants(0)
            .distinctMccs(0)
            .distinctCountries(0)
            .fraudCount(0)
            .found(false)
            .source("EMPTY")
            // NOVOS
            .distinctPans(0)
            .distinctDevices(0)
            .distinctIps(0)
            .distinctUserAgents(0)
            .distinctBeneficiaries(0)
            .firstTransactionAt(null)
            .lastTransactionAt(null)
            .chargebackCount(0)
            .declinedCount(0)
            .cryptoTransactionCount(0)
            .build();
    }
}
```

**Atualizar computeStats()** (linha 216):
```java
private VelocityStats computeStats(KeyType keyType, String keyValue, TimeWindow window) {
    // ... código existente ...
    
    // ADICIONAR queries para novos campos
    long distinctPans = logRepository.countDistinctPans(keyValue, startTime);
    long distinctDevices = logRepository.countDistinctDevices(keyValue, startTime);
    long distinctIps = logRepository.countDistinctIps(keyValue, startTime);
    long distinctUserAgents = logRepository.countDistinctUserAgents(keyValue, startTime);
    long distinctBeneficiaries = logRepository.countDistinctBeneficiaries(keyValue, startTime);
    OffsetDateTime firstTxn = logRepository.findFirstTransactionAt(keyValue);
    OffsetDateTime lastTxn = logRepository.findLastTransactionAt(keyValue);
    long chargebackCount = logRepository.countChargebacks(keyValue, startTime);
    long declinedCount = logRepository.countDeclined(keyValue, startTime);
    long cryptoCount = logRepository.countCryptoTransactions(keyValue, startTime);
    
    return VelocityStats.builder()
        // ... campos existentes ...
        .distinctPans(distinctPans)
        .distinctDevices(distinctDevices)
        .distinctIps(distinctIps)
        .distinctUserAgents(distinctUserAgents)
        .distinctBeneficiaries(distinctBeneficiaries)
        .firstTransactionAt(firstTxn)
        .lastTransactionAt(lastTxn)
        .chargebackCount(chargebackCount)
        .declinedCount(declinedCount)
        .cryptoTransactionCount(cryptoCount)
        .build();
}
```

**Estimativa**: 8 story points  
**Risco**: Médio (requer novas queries no repository)

---

### 3.2 PRIORIDADE 2 - IMPORTANTE (Semanas 3-4)

#### 3.2.1 Criar ValueSingleParser Unificado

**Novo Arquivo**: `service/complex/ValueSingleParser.java`

```java
package com.rulex.service.complex;

import lombok.Builder;
import lombok.Data;
import java.util.regex.Pattern;

/**
 * Parser unificado para os 5 formatos de valueSingle.
 * 
 * Formatos suportados:
 * 1. PIPE: "field|nDays|threshold|op" ou "threshold|hours"
 * 2. COMMA: "value1,value2,value3"
 * 3. COLON: "threshold:hours"
 * 4. SIMPLE: "threshold"
 * 5. MIN_MAX: usa valueMin + valueMax (não valueSingle)
 */
@Component
public class ValueSingleParser {

    private static final Pattern PIPE_PATTERN = Pattern.compile("\\|");
    private static final Pattern COMMA_PATTERN = Pattern.compile(",");
    private static final Pattern COLON_PATTERN = Pattern.compile(":");
    
    @Data
    @Builder
    public static class ParsedValue {
        private final ValueFormat format;
        private final String[] parts;
        private final String raw;
        
        // Getters de conveniência
        public int getThreshold() {
            return switch (format) {
                case PIPE -> parts.length > 0 ? Integer.parseInt(parts[0]) : 0;
                case COLON -> parts.length > 0 ? Integer.parseInt(parts[0]) : 0;
                case SIMPLE -> Integer.parseInt(raw);
                default -> 0;
            };
        }
        
        public int getTimeValue() {
            return switch (format) {
                case PIPE -> parts.length > 1 ? Integer.parseInt(parts[1]) : 24;
                case COLON -> parts.length > 1 ? Integer.parseInt(parts[1]) : 24;
                default -> 24;
            };
        }
        
        public String getOperator() {
            return switch (format) {
                case PIPE -> parts.length > 2 ? parts[2] : "GT";
                default -> "GT";
            };
        }
        
        public List<String> getList() {
            return format == ValueFormat.COMMA ? Arrays.asList(parts) : List.of();
        }
    }
    
    public enum ValueFormat {
        PIPE,    // field|nDays|threshold|op
        COMMA,   // value1,value2,value3
        COLON,   // threshold:hours
        SIMPLE,  // threshold
        MIN_MAX  // usa valueMin + valueMax
    }
    
    public ParsedValue parse(String valueSingle) {
        if (valueSingle == null || valueSingle.isEmpty()) {
            return ParsedValue.builder()
                .format(ValueFormat.SIMPLE)
                .parts(new String[0])
                .raw("")
                .build();
        }
        
        // Detectar formato
        if (valueSingle.contains("|")) {
            return ParsedValue.builder()
                .format(ValueFormat.PIPE)
                .parts(PIPE_PATTERN.split(valueSingle))
                .raw(valueSingle)
                .build();
        }
        
        if (valueSingle.contains(",") && !valueSingle.matches("\\d+,\\d+")) {
            return ParsedValue.builder()
                .format(ValueFormat.COMMA)
                .parts(COMMA_PATTERN.split(valueSingle))
                .raw(valueSingle)
                .build();
        }
        
        if (valueSingle.contains(":")) {
            return ParsedValue.builder()
                .format(ValueFormat.COLON)
                .parts(COLON_PATTERN.split(valueSingle))
                .raw(valueSingle)
                .build();
        }
        
        return ParsedValue.builder()
            .format(ValueFormat.SIMPLE)
            .parts(new String[]{valueSingle})
            .raw(valueSingle)
            .build();
    }
}
```

**Estimativa**: 5 story points  
**Risco**: Baixo

---

### 3.3 PRIORIDADE 3 - FASE 2 (Semanas 5-12)

#### 3.3.1 Neo4j Integration

**Novos Arquivos**:
```
service/graph/
├── GraphConfig.java              # Configuração Neo4j
├── GraphRepository.java          # Interface base
├── MoneyMuleDetectionService.java
├── CommunityDetectionService.java
├── PathAnalysisService.java
└── GraphOperators.java           # 10 novos operadores
```

**GraphConfig.java**:
```java
@Configuration
@EnableNeo4jRepositories
public class GraphConfig {
    
    @Value("${neo4j.uri}")
    private String uri;
    
    @Value("${neo4j.username}")
    private String username;
    
    @Value("${neo4j.password}")
    private String password;
    
    @Bean
    public Driver neo4jDriver() {
        return GraphDatabase.driver(uri, AuthTokens.basic(username, password));
    }
    
    @Bean
    public Neo4jClient neo4jClient(Driver driver) {
        return Neo4jClient.create(driver);
    }
}
```

**Estimativa**: 40 story points (Fase 2 completa)

---

## 4. DETALHAMENTO TÉCNICO POR SPRINT

### Sprint 1 (Semanas 1-2): Correções Críticas

| Task | Arquivo | Tipo | SP | Status |
|------|---------|------|-----|--------|
| Injetar TransactionEnrichmentFacade | RuleEngineService.java | Modificação | 1 | ❌ |
| Chamar enrichFull() | RuleEngineService.java | Modificação | 2 | ❌ |
| Case IN_LIST | ComplexRuleEvaluator.java | Adição | 0.5 | ❌ |
| Case CONTAINS_SUSPICIOUS_KEYWORDS | ComplexRuleEvaluator.java | Adição | 1 | ❌ |
| Case COUNT_LAST_N_DAYS | ComplexRuleEvaluator.java | Adição | 1 | ❌ |
| Case DAYS_SINCE_LAST_ACTIVITY | ComplexRuleEvaluator.java | Adição | 1 | ❌ |
| Case DEVICE_CHANGED_IN_SESSION | ComplexRuleEvaluator.java | Adição | 0.5 | ❌ |
| Case IS_CRYPTO_RANSOM_AMOUNT | ComplexRuleEvaluator.java | Adição | 1 | ❌ |
| Case OUTFLOW_RATE_LAST_N_DAYS | ComplexRuleEvaluator.java | Adição | 1 | ❌ |
| Demais 10 cases | ComplexRuleEvaluator.java | Adição | 8 | ❌ |
| Testes unitários (17 operadores) | ComplexRuleEvaluatorTest.java | Novo | 8 | ❌ |

**Total Sprint 1**: 25 SP

### Sprint 2 (Semanas 3-4): VelocityStats + Parser

| Task | Arquivo | Tipo | SP | Status |
|------|---------|------|-----|--------|
| Adicionar 10 campos VelocityStats | VelocityService.java | Modificação | 3 | ❌ |
| Atualizar computeStats() | VelocityService.java | Modificação | 5 | ❌ |
| Novas queries repository | VelocityTransactionLogRepository.java | Adição | 5 | ❌ |
| Criar ValueSingleParser | ValueSingleParser.java | Novo | 5 | ❌ |
| Refatorar para usar parser | ComplexRuleEvaluator.java | Refatoração | 5 | ❌ |
| Testes VelocityStats | VelocityServiceTest.java | Adição | 3 | ❌ |
| Testes ValueSingleParser | ValueSingleParserTest.java | Novo | 3 | ❌ |

**Total Sprint 2**: 29 SP

### Sprint 3-4 (Semanas 5-8): Neo4j Setup + Graph Operators

| Task | Arquivo | Tipo | SP |
|------|---------|------|-----|
| Neo4j Aura setup | Infraestrutura | Config | 3 |
| Docker Compose Neo4j | docker-compose.yml | Modificação | 2 |
| GraphConfig.java | Novo | Novo | 3 |
| GraphRepository interface | Novo | Novo | 3 |
| 10 Graph Operators enum | RuleCondition.java | Adição | 2 |
| 10 Graph Operators cases | ComplexRuleEvaluator.java | Adição | 15 |
| MoneyMuleDetectionService | Novo | Novo | 8 |
| CommunityDetectionService | Novo | Novo | 8 |
| Testes Graph | GraphOperatorsTest.java | Novo | 8 |

**Total Sprint 3-4**: 52 SP

---

## 5. PADRÕES E CONVENÇÕES

### 5.1 Naming Conventions

| Tipo | Padrão | Exemplo |
|------|--------|---------|
| Operador | SCREAMING_SNAKE_CASE | `COUNT_DISTINCT_PANS_LAST_N_HOURS` |
| Método evaluate | `evaluate` + OperadorName | `evaluateCountDistinctPansLastNHours` |
| Campo enrichment | snake_case | `velocity_distinct_pans_24h` |
| Serviço | PascalCase + Service | `MoneyMuleDetectionService` |

### 5.2 valueSingle Patterns

| Formato | Pattern | Uso |
|---------|---------|-----|
| Pipe | `threshold\|timeValue\|operator` | Velocity operators |
| Colon | `threshold:timeValue` | Time-based operators |
| Comma | `value1,value2,value3` | List operators |
| Simple | `threshold` | Simple comparison |

### 5.3 Enrichment Field Naming

```
{enrichment_type}_{field_name}_{time_window}

Exemplos:
- velocity_transaction_count_24h
- auth_mfa_denial_count_1h
- device_distinct_user_agents_7d
- customer_days_since_last_activity
```

---

## 6. TESTES E QUALIDADE

### 6.1 Cobertura Mínima

| Componente | Cobertura Atual | Target |
|------------|-----------------|--------|
| ComplexRuleEvaluator | ~80% | 95% |
| VelocityService | ~70% | 90% |
| EnrichmentFacade | ~75% | 90% |
| Novos operadores | 0% | 100% |

### 6.2 Testes por Tipo

```java
// Unit Test Pattern
@Test
void evaluateInList_shouldMatchValue_whenInList() {
    // Given
    RuleCondition condition = RuleCondition.builder()
        .operator(ConditionOperator.IN_LIST)
        .valueArray(List.of("BRL", "USD", "EUR"))
        .build();
    
    Object fieldValue = "BRL";
    
    // When
    boolean result = evaluator.evaluateOperator(condition, fieldValue, context);
    
    // Then
    assertThat(result).isTrue();
}

// Integration Test Pattern
@SpringBootTest
@Testcontainers
class ComplexRuleEvaluatorIntegrationTest {
    
    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:15");
    
    @Test
    void evaluate_shouldEnrichAndEvaluate_whenFullFlow() {
        // Given
        TransactionRequest request = createTestRequest();
        
        // When
        TransactionResponse response = ruleEngineService.analyzeTransaction(request);
        
        // Then
        assertThat(response.getEnrichedFields()).containsKey("velocity_transaction_count_24h");
    }
}
```

### 6.3 Performance Benchmarks

| Operação | Latência Atual | Target P99 |
|----------|----------------|------------|
| enrichFull() | - | <15ms |
| evaluate() (93 ops) | ~45ms | <25ms |
| evaluate() (110 ops) | - | <30ms |
| Neo4j query | - | <25ms |
| **Total** | ~60ms | <50ms |

---

## 📋 RESUMO EXECUTIVO

### Esforço Total Estimado

| Fase | Story Points | Semanas | Prioridade |
|------|--------------|---------|------------|
| Sprint 1 (Crítico) | 25 | 2 | 🔴 P1 |
| Sprint 2 (Importante) | 29 | 2 | 🟡 P2 |
| Sprint 3-4 (Neo4j) | 52 | 4 | 🟢 P3 |
| Sprint 5-6 (Regulatory) | 40 | 4 | 🟢 P3 |
| Sprint 7-12 (Advanced) | 80 | 12 | 🔵 P4 |
| **TOTAL** | **226** | **24** | - |

### Quick Wins (Esta Semana)

1. ✅ Injetar `TransactionEnrichmentFacade` no `RuleEngineService` (1 SP)
2. ✅ Implementar case `IN_LIST` (0.5 SP) - é só delegar para `IN`
3. ✅ Implementar case `DEVICE_CHANGED_IN_SESSION` (0.5 SP) - simples boolean
4. ✅ Testes para os 3 acima (2 SP)

**Total Quick Wins**: 4 SP em ~1 dia

---

**FIM DO PLANO DE IMPLEMENTAÇÃO**
