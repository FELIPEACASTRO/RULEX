# 🏗️ RULEX V3.0 TECHNICAL BLUEPRINT

**Versão**: 3.0.0-TECHNICAL-SPECIFICATION  
**Data**: 12 de Janeiro de 2026  
**Autor**: Devin AI Agent  
**Tipo**: Technical Architecture Document  
**Status**: ✅ PRODUCTION-READY SPECIFICATION

---

## 📋 TABLE OF CONTENTS

1. [Executive Technical Summary](#1-executive-technical-summary)
2. [Current Architecture Analysis](#2-current-architecture-analysis)
3. [Target Architecture V3.0](#3-target-architecture-v30)
4. [Operator Expansion Strategy](#4-operator-expansion-strategy)
5. [Data Layer Enhancements](#5-data-layer-enhancements)
6. [API & Integration Layer](#6-api--integration-layer)
7. [Performance Engineering](#7-performance-engineering)
8. [Security Architecture](#8-security-architecture)
9. [Observability & Monitoring](#9-observability--monitoring)
10. [Migration Strategy](#10-migration-strategy)
11. [Testing Strategy](#11-testing-strategy)
12. [Deployment Architecture](#12-deployment-architecture)
13. [Technical Specifications](#13-technical-specifications)
14. [Implementation Guides](#14-implementation-guides)
15. [Appendices](#15-appendices)

---

## 1. EXECUTIVE TECHNICAL SUMMARY

### 1.1 Current State (RULEX v2.0)

```yaml
CURRENT_ARCHITECTURE:
  backend:
    framework: "Spring Boot 3.5.9"
    java_version: "21 LTS"
    build_tool: "Maven"
    orm: "Spring Data JPA + Hibernate"
    database: "PostgreSQL 16"
    cache: "Redis 7 (Velocity)"
    migrations: "Flyway"
    
  frontend:
    framework: "React 19.2.1"
    bundler: "Vite 7.1.7"
    language: "TypeScript 5.9.3"
    styling: "Tailwind CSS"
    state: "React Context + Hooks"
    
  infrastructure:
    containerization: "Docker"
    orchestration: "Docker Compose"
    reverse_proxy: "Nginx"
    
  metrics:
    lines_of_code: ~50,000
    operators: 66
    complex_rules: 48
    api_endpoints: 35+
    test_coverage: 99.2%
```

### 1.2 Target State (RULEX v3.0)

```yaml
TARGET_ARCHITECTURE:
  backend:
    framework: "Spring Boot 3.5.9" # PRESERVED
    java_version: "21 LTS" # PRESERVED
    build_tool: "Maven" # PRESERVED
    orm: "Spring Data JPA + Hibernate" # PRESERVED
    database: "PostgreSQL 16" # PRESERVED
    cache: "Redis 7 Cluster" # UPGRADED
    graph_db: "Neo4j Enterprise 5.x" # NEW
    messaging: "Apache Kafka" # NEW (for federated)
    
  frontend:
    framework: "React 19.2.1" # PRESERVED
    bundler: "Vite 7.1.7" # PRESERVED
    visualization: "D3.js + ECharts" # NEW (graph viz)
    
  infrastructure:
    containerization: "Docker" # PRESERVED
    orchestration: "Kubernetes" # UPGRADED
    service_mesh: "Istio" # NEW
    
  metrics:
    lines_of_code: ~80,000 (+60%)
    operators: 100+ (+52%)
    complex_rules: 108+ (+125%)
    api_endpoints: 55+ (+57%)
    test_coverage: 99.5%
```

---

## 2. CURRENT ARCHITECTURE ANALYSIS

### 2.1 System Context Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         EXTERNAL SYSTEMS                             │
├─────────────────────────────────────────────────────────────────────┤
│  [Analyst UI]  [Admin UI]  [Core Banking]  [Payment Gateway]        │
│       │            │            │               │                    │
└───────┼────────────┼────────────┼───────────────┼────────────────────┘
        │            │            │               │
        ▼            ▼            ▼               ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         RULEX ENGINE                                 │
├─────────────────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    API GATEWAY LAYER                         │   │
│  │  [REST API]  [WebSocket]  [OAuth2/JWT]  [Rate Limiting]      │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                               │                                      │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    SERVICE LAYER                             │   │
│  │  [RuleEngineUseCase]  [ComplexRuleEvaluator]  [GeoService]   │   │
│  │  [VelocityService]  [RiskScoreService]  [AuditService]       │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                               │                                      │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │                    DATA ACCESS LAYER                         │   │
│  │  [PostgreSQL]  [Redis]  [File Storage]                       │   │
│  └─────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

### 2.2 Component Inventory

#### Backend Components (46 Services)

| Package | Component Count | Key Classes |
|---------|----------------|-------------|
| `controller` | 14 | RuleController, TransactionController, etc. |
| `service` | 46 | ComplexRuleEvaluator (2,222 lines), etc. |
| `repository` | 38 | JPA repositories |
| `entity` | 40 | JPA entities |
| `dto` | 25 | Data transfer objects |
| `config` | 18 | Configuration classes |

#### Frontend Components

| Directory | Component Count | Key Files |
|-----------|----------------|-----------|
| `pages` | 8 | Rules, Transactions, Dashboard, etc. |
| `components` | 45+ | RuleFormDialog, ComplexRuleBuilder, etc. |
| `hooks` | 12 | Custom React hooks |
| `lib` | 8 | javaApi.ts (API client) |

### 2.3 Data Model

```sql
-- CORE TABLES (Existing)
rules (id, name, description, category, score, active, ...)
rule_conditions (id, rule_id, field, operator, value, ...)
complex_rules (id, name, conditions_json, scoring_logic_json, ...)
rule_condition_groups (id, name, parent_id, logical_operator, ...)
transactions (id, amount, currency, status, risk_score, ...)
users (id, username, password_hash, role, ...)
audit_logs (id, user_id, action, entity_type, ...)

-- VELOCITY TABLES (Existing)
velocity_windows (id, customer_id, window_type, metric, value, ...)

-- NEW V3.0 TABLES (Proposed)
federated_rules (id, source_bank_id, rule_hash, consensus_score, ...)
graph_entities (id, entity_type, properties_json, ...)
graph_relationships (id, source_id, target_id, relationship_type, ...)
iso20022_mappings (id, message_type, field_path, rulex_field, ...)
threshold_history (id, rule_id, parameter, old_value, new_value, ...)
```

---

## 3. TARGET ARCHITECTURE V3.0

### 3.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              EXTERNAL INTEGRATION                            │
├─────────────────────────────────────────────────────────────────────────────┤
│ [ISO 20022 Gateway] [OFAC/Sanctions] [Credit Bureau] [eIDAS Wallet] [Banks]  │
└──────────┬────────────────────────────────────────────────────────────┬─────┘
           │                                                            │
           ▼                                                            ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              API GATEWAY CLUSTER                             │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │   Kong/     │  │   Rate      │  │   OAuth2    │  │   Request           │ │
│  │   Traefik   │  │   Limiter   │  │   JWT Auth  │  │   Transformer       │ │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────────────┘ │
└──────────────────────────────────────┬──────────────────────────────────────┘
                                       │
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         RULEX V3.0 CORE ENGINE                               │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │                       ORCHESTRATION LAYER                              │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌──────────────┐  │  │
│  │  │ Rule        │  │ Score       │  │ Threshold   │  │ Federated    │  │  │
│  │  │ Orchestrator│  │ Aggregator  │  │ Optimizer   │  │ Coordinator  │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  └──────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                                       │                                      │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │                       EVALUATION LAYER                                 │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌──────────────┐  │  │
│  │  │ Complex     │  │ Graph       │  │ ISO 20022   │  │ Regulatory   │  │  │
│  │  │ Evaluator   │  │ Evaluator   │  │ Evaluator   │  │ Evaluator    │  │  │
│  │  │ (100+ ops)  │  │ (Neo4j)     │  │ (Parser)    │  │ (28 frmwks)  │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  └──────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                                       │                                      │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │                       DATA ACCESS LAYER                                │  │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌──────────────┐  │  │
│  │  │ PostgreSQL  │  │ Redis       │  │ Neo4j       │  │ Kafka        │  │  │
│  │  │ (Rules,     │  │ Cluster     │  │ (Graph)     │  │ (Federated)  │  │  │
│  │  │ Audit)      │  │ (Velocity)  │  │             │  │              │  │  │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  └──────────────┘  │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 3.2 Layer Architecture (Expanded)

```yaml
LAYER_ARCHITECTURE_V3:
  # PRESERVED LAYERS (0-4)
  layer_0_govern:
    name: "GOVERN (NIST CSF 2.0)"
    purpose: "Risk governance and strategy alignment"
    scope: "Organization-wide risk tolerance"
    score_modifier: "Policy enforcement gates"
    # NEW: NIST CSF 2.0 categories (GV.RM, GV.SC, etc.)
    
  layer_1_hardstop:
    name: "HARDSTOP"
    score_range: [95, 100]
    purpose: "Immediate blocking of obvious fraud"
    action: "BLOCK + ALERT"
    latency_budget: "5ms"
    
  layer_2_risk:
    name: "RISK"
    score_range: [70, 94]
    purpose: "High-risk transaction analysis"
    action: "REVIEW + DELAY"
    latency_budget: "15ms"
    
  layer_3_caution:
    name: "CAUTION"
    score_range: [40, 69]
    purpose: "Suspicious pattern detection"
    action: "FLAG + MONITOR"
    latency_budget: "20ms"
    
  layer_4_behavioral:
    name: "BEHAVIORAL"
    score_range: [0, 39]
    purpose: "Behavioral baseline monitoring"
    action: "LOG + LEARN"
    latency_budget: "10ms"
    
  # NEW LAYERS (5-7)
  layer_5_graph:
    name: "GRAPH ANALYTICS"
    purpose: "Network and relationship analysis"
    scope: "Cross-entity patterns, money mule networks"
    latency_budget: "25ms"
    # NEW: Neo4j integration for graph traversal
    
  layer_6_regulatory:
    name: "REGULATORY COMPLIANCE"
    purpose: "Framework-specific rule evaluation"
    scope: "DORA, PSD3, eIDAS, ISO 20022, etc."
    latency_budget: "15ms"
    # NEW: 28 regulatory frameworks
    
  layer_7_federated:
    name: "FEDERATED INTELLIGENCE"
    purpose: "Cross-bank rule consensus"
    scope: "Shared rule definitions (NOT data)"
    latency_budget: "20ms"
    # NEW: Kafka-based rule sharing
```

### 3.3 Service Architecture

```yaml
SERVICES_V3:
  # PRESERVED SERVICES
  core_services:
    - ComplexRuleEvaluator: "2,500+ lines (expanded from 2,222)"
    - RuleEngineUseCase: "Rule orchestration"
    - GeoService: "Geolocation operations"
    - VelocityService: "Redis velocity counters"
    - RiskScoreService: "Score calculation"
    - AuditService: "Audit trail"
    
  # NEW SERVICES
  v3_services:
    - GraphEvaluatorService:
        purpose: "Neo4j graph operations"
        operators: 10 (Category 17)
        latency: "<25ms per query"
        
    - ISO20022Service:
        purpose: "ISO 20022 message parsing"
        operators: 4 (Category 18)
        supported_messages: ["pacs.008", "pacs.002", "pain.001"]
        
    - RegulatoryEvaluatorService:
        purpose: "Regulatory framework rules"
        operators: 10 (Category 16)
        frameworks: 28
        
    - FederatedRuleService:
        purpose: "Cross-bank rule coordination"
        protocol: "Kafka + encryption"
        consensus: "Byzantine fault tolerant"
        
    - ThresholdOptimizerService:
        purpose: "Statistical threshold adjustment"
        method: "BTL/ATL testing + percentile calculation"
        ml_role: "AUXILIARY (suggests, doesn't decide)"
        
    - AdaptiveBehaviorService:
        purpose: "Customer behavioral profiling"
        operators: 10 (Category 15)
        ml_role: "AUXILIARY (builds profiles)"
```

---

## 4. OPERATOR EXPANSION STRATEGY

### 4.1 Current Operator Categories (66 → 100+)

```java
// CATEGORY STRUCTURE (ComplexRuleEvaluator.java)

public enum OperatorCategory {
    // EXISTING (66 operators)
    BASIC_COMPARISON(6),      // EQ, NEQ, GT, GTE, LT, LTE
    LIST_OPERATIONS(2),       // IN, NOT_IN
    STRING_OPERATIONS(6),     // CONTAINS, STARTS_WITH, etc.
    NULL_BOOLEAN(4),          // IS_NULL, NOT_NULL, etc.
    RANGE_OPERATIONS(2),      // BETWEEN, NOT_BETWEEN
    FIELD_COMPARISON(6),      // FIELD_EQ, FIELD_GT, etc.
    DATE_TIME(6),             // DATE_BEFORE, TIME_BETWEEN, etc.
    ARRAY_OPERATIONS(5),      // ARRAY_CONTAINS, SIZE, etc.
    MATH_OPERATIONS(2),       // MOD_EQ, MOD_NEQ
    GEO_OPERATIONS(3),        // GEO_DISTANCE_LT, GEO_IN_POLYGON
    VELOCITY_OPERATIONS(8),   // VELOCITY_COUNT, SUM, AVG, etc.
    DSL_ADVANCED(7),          // SUM_LAST_N_DAYS, COUNT_DISTINCT, etc.
    FRAUD_SPECIFIC(29),       // V36 fraud operators
    
    // NEW V3.0 (34 operators)
    ADVANCED_BEHAVIORAL(10),  // Category 15
    REGULATORY_COMPLIANCE(10),// Category 16
    GRAPH_ANALYTICS(10),      // Category 17
    ISO_20022_SPECIFIC(4);    // Category 18
}
```

### 4.2 Implementation Strategy per Category

#### Category 15: Advanced Behavioral (10 new operators)

```java
// FILE: backend/src/main/java/com/rulex/operators/BehavioralOperators.java

public class BehavioralOperators {
    
    /**
     * CUSTOMER_LIFETIME_VELOCITY
     * Calcula velocidade de transações ao longo do ciclo de vida do cliente
     * 
     * ML ROLE: ZERO - cálculo estatístico puro
     */
    public boolean customerLifetimeVelocity(
        String customerId, 
        String metric, 
        String period,
        double threshold
    ) {
        // Implementação determinística
        // Consulta PostgreSQL para histórico
        // Calcula média móvel (MAV)
        // Compara com threshold
        return velocityValue > threshold;
    }
    
    /**
     * PEER_GROUP_DEVIATION
     * Compara comportamento do cliente vs peer group
     * 
     * ML ROLE: AUXILIARY - define segmentos, não decide
     */
    public boolean peerGroupDeviation(
        String customerId,
        String peerSegment,
        String metric,
        double standardDeviations
    ) {
        // Segmentação definida MANUALMENTE ou via clustering (auxiliary)
        // Cálculo de desvio padrão é ESTATÍSTICO (determinístico)
        double customerValue = getMetricValue(customerId, metric);
        double peerMean = getPeerGroupMean(peerSegment, metric);
        double peerStdDev = getPeerGroupStdDev(peerSegment, metric);
        
        return Math.abs(customerValue - peerMean) / peerStdDev > standardDeviations;
    }
    
    // ... 8 outros operadores
}
```

#### Category 16: Regulatory Compliance (10 new operators)

```java
// FILE: backend/src/main/java/com/rulex/operators/RegulatoryOperators.java

public class RegulatoryOperators {
    
    /**
     * PSD3_COP_VALIDATION
     * Confirmation of Payee validation per PSD3 requirements
     * 
     * ML ROLE: ZERO - string matching algorithms
     */
    public CopResult psd3CopValidation(
        String payerName,
        String payeeName,
        String accountNumber
    ) {
        // Levenshtein distance (deterministic)
        int distance = LevenshteinDistance.getDefaultInstance()
            .apply(payerName.toLowerCase(), payeeName.toLowerCase());
        
        // Exact match check
        boolean exactMatch = payerName.equalsIgnoreCase(payeeName);
        
        // Soundex/Metaphone for phonetic matching
        String payerSoundex = soundex.encode(payerName);
        String payeeSoundex = soundex.encode(payeeName);
        boolean phoneticMatch = payerSoundex.equals(payeeSoundex);
        
        return new CopResult(
            exactMatch ? "EXACT_MATCH" :
            phoneticMatch ? "CLOSE_MATCH" :
            distance <= 3 ? "PARTIAL_MATCH" : "NO_MATCH"
        );
    }
    
    /**
     * DORA_INCIDENT_CLASSIFICATION
     * Classifies incidents per DORA severity matrix
     * 
     * ML ROLE: ZERO - rule-based classification matrix
     */
    public DoraClassification doraIncidentClassification(
        IncidentData incidentData,
        SeverityMatrix severityMatrix
    ) {
        // Pure rule-based lookup in severity matrix
        // No ML involved
    }
    
    // ... 8 outros operadores
}
```

#### Category 17: Graph Analytics (10 new operators)

```java
// FILE: backend/src/main/java/com/rulex/operators/GraphOperators.java

@Service
public class GraphOperators {
    
    @Autowired
    private Neo4jTemplate neo4jTemplate;
    
    /**
     * MONEY_MULE_NETWORK_SCORE
     * Calcula score de risco de rede de money mule
     * 
     * ML ROLE: AUXILIARY - community detection (descoberta)
     *          DECISION: RULE-BASED (score calculation)
     */
    public double moneyMuleNetworkScore(String accountId) {
        // Cypher query (deterministic)
        String cypher = """
            MATCH (a:Account {id: $accountId})-[r:TRANSFERS_TO*1..3]-(b:Account)
            WHERE b.risk_flag = 'MULE_SUSPECT'
            RETURN count(DISTINCT b) as muleConnections,
                   avg(r.amount) as avgAmount,
                   max(length(r)) as maxDepth
        """;
        
        // Execute deterministic graph traversal
        Map<String, Object> result = neo4jTemplate.query(cypher, 
            Map.of("accountId", accountId));
        
        // Rule-based score calculation (deterministic)
        double score = calculateMuleScore(
            (int) result.get("muleConnections"),
            (double) result.get("avgAmount"),
            (int) result.get("maxDepth")
        );
        
        return score;
    }
    
    private double calculateMuleScore(int connections, double avgAmount, int depth) {
        // Pure deterministic formula
        // connections: 0-2=0, 3-5=30, 6-10=60, 10+=100
        // avgAmount: >10000=+20, >50000=+40
        // depth: 3=+10, 2=+5, 1=0
        
        double baseScore = 0;
        if (connections >= 10) baseScore = 100;
        else if (connections >= 6) baseScore = 60;
        else if (connections >= 3) baseScore = 30;
        
        if (avgAmount > 50000) baseScore += 40;
        else if (avgAmount > 10000) baseScore += 20;
        
        if (depth == 3) baseScore += 10;
        else if (depth == 2) baseScore += 5;
        
        return Math.min(baseScore, 100);
    }
    
    // ... 9 outros operadores
}
```

#### Category 18: ISO 20022 Specific (4 new operators)

```java
// FILE: backend/src/main/java/com/rulex/operators/ISO20022Operators.java

@Service
public class ISO20022Operators {
    
    /**
     * ISO20022_REMITTANCE_ANOMALY
     * Analisa campo RmtInf para padrões suspeitos
     * 
     * ML ROLE: AUXILIARY - descoberta de padrões
     *          DECISION: RULE-BASED (pattern matching)
     */
    public RemittanceResult iso20022RemittanceAnomaly(
        ISO20022Message message,
        String customerSegment
    ) {
        String remittanceInfo = message.extractRemittanceInfo();
        
        // Rule-based pattern detection (deterministic)
        List<String> suspiciousPatterns = List.of(
            "URGENT.*TRANSFER",
            "IMMEDIATE.*PAYMENT",
            "CEO.*INSTRUCTION",
            "WIRE.*NOW",
            "CONFIDENTIAL.*MATTER"
        );
        
        for (String pattern : suspiciousPatterns) {
            if (Pattern.matches(pattern, remittanceInfo.toUpperCase())) {
                return RemittanceResult.SUSPICIOUS;
            }
        }
        
        // Check against customer segment baseline
        CustomerBaseline baseline = getBaseline(customerSegment);
        if (remittanceInfo.length() > baseline.avgRemittanceLength * 2) {
            return RemittanceResult.ANOMALOUS;
        }
        
        return RemittanceResult.NORMAL;
    }
    
    // ... 3 outros operadores
}
```

### 4.3 ConditionOperator Enum Expansion

```java
// FILE: backend/src/main/java/com/rulex/entity/RuleCondition.java

public enum ConditionOperator {
    // ... existing 66 operators ...
    
    // Category 15: Advanced Behavioral (10 new)
    CUSTOMER_LIFETIME_VELOCITY,
    CROSS_CHANNEL_CONSISTENCY,
    PEER_GROUP_DEVIATION,
    SEASONAL_PATTERN_MATCH,
    LIFECYCLE_STAGE_ANOMALY,
    TRUST_SCORE_EVOLUTION,
    REFERRAL_NETWORK_RISK,
    DORMANCY_REVIVAL_PATTERN,
    MICRO_TRANSACTION_AGGREGATION,
    ROUND_AMOUNT_FREQUENCY,
    
    // Category 16: Regulatory Compliance (10 new)
    PSD3_COP_VALIDATION,
    DORA_INCIDENT_CLASSIFICATION,
    BASEL_III_OPERATIONAL_LOSS,
    NIST_CSF_CONTROL_CHECK,
    EIDAS_CREDENTIAL_ASSURANCE,
    ISO20022_FIELD_COMPLETENESS,
    AML_TYPOLOGY_MATCH,
    SANCTIONS_FUZZY_MATCH,
    PEP_RELATIONSHIP_DEPTH,
    CFATF_HIGH_RISK_JURISDICTION,
    
    // Category 17: Graph Analytics (10 new)
    MONEY_MULE_NETWORK_SCORE,
    COMMUNITY_DETECTION_OUTLIER,
    TRANSACTION_PATH_LENGTH,
    GRAPH_CENTRALITY_ANOMALY,
    RING_STRUCTURE_DETECTION,
    LAYERING_PATTERN_DEPTH,
    SMURFING_NETWORK_IDENTIFICATION,
    ENTITY_CLUSTERING_COEFFICIENT,
    TEMPORAL_GRAPH_EVOLUTION,
    CROSS_BORDER_CHAIN_COMPLEXITY,
    
    // Category 18: ISO 20022 Specific (4 new)
    ISO20022_REMITTANCE_ANOMALY,
    ISO20022_ORIGINATOR_VALIDATION,
    ISO20022_PAYMENT_CHAIN_ANALYSIS,
    ISO20022_REPORTING_INDICATOR_CHECK
}
```

---

## 5. DATA LAYER ENHANCEMENTS

### 5.1 PostgreSQL Schema Additions

```sql
-- V39__graph_entities.sql
CREATE TABLE graph_entities (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    entity_type VARCHAR(50) NOT NULL,
    external_id VARCHAR(255) NOT NULL,
    properties JSONB NOT NULL DEFAULT '{}',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(entity_type, external_id)
);

CREATE INDEX idx_graph_entities_type ON graph_entities(entity_type);
CREATE INDEX idx_graph_entities_properties ON graph_entities USING gin(properties);

-- V40__graph_relationships.sql
CREATE TABLE graph_relationships (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    source_entity_id UUID REFERENCES graph_entities(id),
    target_entity_id UUID REFERENCES graph_entities(id),
    relationship_type VARCHAR(50) NOT NULL,
    properties JSONB NOT NULL DEFAULT '{}',
    weight DECIMAL(10,4) DEFAULT 1.0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_graph_rel_source ON graph_relationships(source_entity_id);
CREATE INDEX idx_graph_rel_target ON graph_relationships(target_entity_id);
CREATE INDEX idx_graph_rel_type ON graph_relationships(relationship_type);

-- V41__federated_rules.sql
CREATE TABLE federated_rules (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    rule_hash VARCHAR(64) NOT NULL UNIQUE,
    source_bank_id VARCHAR(36),
    rule_definition JSONB NOT NULL,
    consensus_score DECIMAL(5,2) DEFAULT 0,
    adoption_count INT DEFAULT 0,
    effectiveness_score DECIMAL(5,4) DEFAULT 0,
    status VARCHAR(20) DEFAULT 'PENDING',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    adopted_at TIMESTAMP
);

CREATE INDEX idx_federated_rules_status ON federated_rules(status);
CREATE INDEX idx_federated_rules_consensus ON federated_rules(consensus_score DESC);

-- V42__threshold_history.sql
CREATE TABLE threshold_history (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    rule_id UUID REFERENCES rules(id),
    parameter_name VARCHAR(100) NOT NULL,
    old_value VARCHAR(255),
    new_value VARCHAR(255) NOT NULL,
    change_reason VARCHAR(50) NOT NULL,
    statistical_basis JSONB,
    approved_by UUID REFERENCES users(id),
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_threshold_history_rule ON threshold_history(rule_id);
CREATE INDEX idx_threshold_history_date ON threshold_history(applied_at);

-- V43__iso20022_mappings.sql
CREATE TABLE iso20022_mappings (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    message_type VARCHAR(20) NOT NULL,
    iso_field_path VARCHAR(255) NOT NULL,
    rulex_field VARCHAR(100) NOT NULL,
    transformation_rule JSONB,
    validation_regex VARCHAR(500),
    required BOOLEAN DEFAULT false,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(message_type, iso_field_path)
);

CREATE INDEX idx_iso_mappings_type ON iso20022_mappings(message_type);

-- V44__regulatory_frameworks.sql
CREATE TABLE regulatory_frameworks (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    framework_code VARCHAR(20) NOT NULL UNIQUE,
    framework_name VARCHAR(100) NOT NULL,
    jurisdiction VARCHAR(50),
    effective_date DATE,
    controls JSONB NOT NULL DEFAULT '[]',
    active BOOLEAN DEFAULT true
);

CREATE TABLE framework_rule_mappings (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    framework_id UUID REFERENCES regulatory_frameworks(id),
    rule_id UUID REFERENCES rules(id),
    control_id VARCHAR(50),
    compliance_weight DECIMAL(3,2) DEFAULT 1.0,
    UNIQUE(framework_id, rule_id)
);
```

### 5.2 Neo4j Graph Schema

```cypher
// Entity Types
CREATE CONSTRAINT entity_id IF NOT EXISTS
FOR (e:Entity) REQUIRE e.id IS UNIQUE;

// Node Labels
// :Account, :Customer, :Device, :IP, :Merchant, :Transaction

// Relationship Types
// -[:OWNS]-> Account owns
// -[:TRANSACTED_WITH]-> Transaction relationship
// -[:LOGGED_FROM]-> Device/IP relationship
// -[:RECEIVES_FROM]-> Money flow
// -[:TRANSFERS_TO]-> Money flow
// -[:LINKED_TO]-> Generic connection

// Fraud Pattern Templates
// Money Mule Network
CREATE (template:FraudTemplate {
    name: 'MoneyMuleNetwork',
    pattern: '(source)-[:TRANSFERS_TO*2..4]->(mule)-[:TRANSFERS_TO]->(dest)',
    riskScore: 85
});

// Layering Pattern
CREATE (template:FraudTemplate {
    name: 'LayeringPattern',
    pattern: '(origin)-[:TRANSFERS_TO*3..10]->(dest) WHERE ALL(r IN relationships(path) WHERE r.amount < 10000)',
    riskScore: 90
});
```

### 5.3 Redis Data Structures (Enhanced)

```yaml
REDIS_STRUCTURES_V3:
  # EXISTING
  velocity_counters:
    key: "velocity:{customer_id}:{metric}:{window}"
    type: "STRING (counter)"
    ttl: "86400s (24h)"
    
  bloom_filters:
    key: "bloom:device:{fingerprint}"
    type: "BF.ADD/BF.EXISTS"
    capacity: "10M entries"
    error_rate: "0.001"
    
  # NEW V3.0
  adaptive_thresholds:
    key: "threshold:{rule_id}:{segment}"
    type: "HASH"
    fields:
      - current_value
      - p50, p75, p90, p95, p99
      - last_updated
      - sample_size
    ttl: "3600s (1h)"
    
  customer_baselines:
    key: "baseline:{customer_id}"
    type: "HASH"
    fields:
      - avg_amount
      - avg_frequency
      - usual_hours
      - usual_merchants
      - usual_devices
    ttl: "604800s (7d)"
    
  federated_rule_cache:
    key: "federated:{rule_hash}"
    type: "JSON"
    content: "rule_definition + metadata"
    ttl: "300s (5m)"
    
  graph_cache:
    key: "graph:mule_score:{account_id}"
    type: "STRING"
    ttl: "60s"
```

---

## 6. API & INTEGRATION LAYER

### 6.1 New API Endpoints

```yaml
API_ENDPOINTS_V3:
  # Graph Analytics
  POST /api/v3/graph/money-mule-score:
    description: "Calculate money mule network score"
    request:
      accountId: string
    response:
      score: number
      connections: array
      riskLevel: string
      
  POST /api/v3/graph/entity-network:
    description: "Get entity network visualization"
    request:
      entityId: string
      depth: number (1-5)
    response:
      nodes: array
      edges: array
      
  # ISO 20022
  POST /api/v3/iso20022/parse:
    description: "Parse ISO 20022 message"
    request:
      messageType: string
      messageContent: string (XML)
    response:
      parsedFields: object
      validationErrors: array
      
  POST /api/v3/iso20022/evaluate:
    description: "Evaluate ISO 20022 specific rules"
    request:
      transactionRequest: object (enriched with ISO fields)
    response:
      ruleResults: array
      complianceStatus: object
      
  # Federated Rules
  GET /api/v3/federated/rules:
    description: "List available federated rules"
    response:
      rules: array
      
  POST /api/v3/federated/rules/{ruleHash}/adopt:
    description: "Adopt a federated rule"
    request:
      customParameters: object (optional)
    response:
      adoptedRule: object
      
  POST /api/v3/federated/rules/propose:
    description: "Propose new rule to federation"
    request:
      ruleDefinition: object
    response:
      ruleHash: string
      status: string
      
  # Adaptive Thresholds
  GET /api/v3/thresholds/{ruleId}:
    description: "Get current thresholds for rule"
    response:
      thresholds: object
      statisticalBasis: object
      
  POST /api/v3/thresholds/{ruleId}/optimize:
    description: "Request threshold optimization"
    request:
      targetFPRate: number
      segment: string (optional)
    response:
      suggestedThresholds: object
      expectedImpact: object
      
  PUT /api/v3/thresholds/{ruleId}/apply:
    description: "Apply optimized thresholds"
    request:
      thresholds: object
      approver: string
    response:
      applied: boolean
      history: object
      
  # Regulatory Compliance
  GET /api/v3/compliance/frameworks:
    description: "List configured regulatory frameworks"
    response:
      frameworks: array
      
  POST /api/v3/compliance/evaluate:
    description: "Evaluate transaction against all frameworks"
    request:
      transactionRequest: object
    response:
      frameworkResults: object
      complianceScore: number
      requiredActions: array
```

### 6.2 External Integrations

```yaml
EXTERNAL_INTEGRATIONS_V3:
  # Sanctions & Lists
  ofac_sdn:
    type: "REST API"
    endpoint: "https://sanctionssearch.ofac.treas.gov/"
    method: "GET"
    caching: "24h"
    
  eu_sanctions:
    type: "REST API"
    endpoint: "https://data.europa.eu/euodp/en/data/"
    caching: "24h"
    
  # Credit Bureau (Synthetic Identity)
  credit_bureau:
    type: "SOAP/REST"
    providers: ["Equifax", "Experian", "TransUnion"]
    fields: ["SSN validation", "address history", "name variations"]
    
  # eIDAS 2.0 Wallet
  eudi_wallet:
    type: "REST API"
    protocol: "OpenID4VP"
    credentials: ["identity", "age", "address"]
    
  # ISO 20022 Networks
  swift:
    type: "MQ/API"
    messages: ["pacs.008", "pacs.002", "camt.053"]
    
  sepa:
    type: "ISO 20022 XML"
    messages: ["pain.001", "pain.002", "pacs.008"]
```

---

## 7. PERFORMANCE ENGINEERING

### 7.1 Latency Budget Allocation

```yaml
LATENCY_BUDGET_V3:
  total_budget: "50ms (P99)"
  
  allocation:
    api_gateway: "3ms"
    authentication: "2ms"
    
    layer_1_hardstop: "5ms"
    layer_2_risk: "8ms"
    layer_3_caution: "8ms"
    layer_4_behavioral: "5ms"
    layer_5_graph: "10ms"
    layer_6_regulatory: "5ms"
    layer_7_federated: "2ms"
    
    response_serialization: "2ms"
    
  critical_paths:
    hardstop_only: "10ms (fastest path)"
    full_evaluation: "50ms (all layers)"
    graph_heavy: "35ms (graph + core)"
```

### 7.2 Optimization Strategies

```yaml
OPTIMIZATIONS:
  # Caching
  rule_cache:
    location: "In-memory (Caffeine)"
    ttl: "60s"
    size: "10,000 rules"
    
  threshold_cache:
    location: "Redis"
    ttl: "300s"
    
  graph_result_cache:
    location: "Redis"
    ttl: "60s"
    key: "graph:{query_hash}"
    
  # Parallel Execution
  layer_parallelism:
    strategy: "CompletableFuture.allOf()"
    threads: "Virtual Threads (Java 21)"
    layers_parallel: [1, 2, 3, 4] # Core layers
    layers_sequential: [5, 6, 7] # Dependent layers
    
  # Connection Pooling
  postgresql:
    min_connections: 20
    max_connections: 100
    idle_timeout: "300s"
    
  neo4j:
    min_connections: 10
    max_connections: 50
    acquisition_timeout: "30s"
    
  redis:
    pool_size: 50
    min_idle: 10
    
  # Batch Processing
  graph_queries:
    batch_size: 100
    parallel_queries: 10
    
  # Index Optimization
  postgresql_indexes:
    - "CREATE INDEX CONCURRENTLY idx_tx_customer_date ON transactions(customer_id, created_at DESC)"
    - "CREATE INDEX CONCURRENTLY idx_rules_active_category ON rules(active, category)"
    
  neo4j_indexes:
    - "CREATE INDEX account_id FOR (a:Account) ON (a.id)"
    - "CREATE INDEX transaction_date FOR ()-[t:TRANSFERS_TO]-() ON (t.date)"
```

### 7.3 Scalability Architecture

```yaml
SCALABILITY:
  horizontal_scaling:
    rulex_api:
      min_replicas: 3
      max_replicas: 20
      cpu_threshold: "70%"
      memory_threshold: "80%"
      
    graph_service:
      min_replicas: 2
      max_replicas: 10
      
  database_scaling:
    postgresql:
      strategy: "Read replicas"
      replicas: 3
      routing: "pgpool-II"
      
    neo4j:
      strategy: "Causal clustering"
      cores: 3
      read_replicas: 3
      
    redis:
      strategy: "Cluster mode"
      shards: 6
      replicas_per_shard: 2
```

---

## 8. SECURITY ARCHITECTURE

### 8.1 Authentication & Authorization

```yaml
SECURITY_V3:
  authentication:
    method: "OAuth2 + JWT"
    provider: "Keycloak / Auth0"
    token_lifetime: "1h"
    refresh_token: "24h"
    
  authorization:
    model: "RBAC + ABAC"
    roles:
      - ADMIN: "Full access"
      - ANALYST: "View + create rules"
      - OPERATOR: "View only"
      - API_CLIENT: "API access only"
    attributes:
      - jurisdiction: "Rule filtering by region"
      - sensitivity_level: "Access to PII"
      
  api_security:
    rate_limiting:
      default: "1000 req/min"
      authenticated: "5000 req/min"
      
    input_validation:
      - "JSON Schema validation"
      - "SQL injection prevention (parameterized queries)"
      - "ReDoS prevention (regex timeout)"
      
    output_sanitization:
      - "PII masking in logs"
      - "Response filtering by role"
```

### 8.2 Federated Security (Critical)

```yaml
FEDERATED_SECURITY:
  rule_sharing:
    encryption: "AES-256-GCM"
    signing: "Ed25519"
    transport: "mTLS"
    
  data_isolation:
    shared: "Rule definitions ONLY"
    never_shared:
      - "Customer data"
      - "Transaction data"
      - "Model weights"
      - "Threshold values"
      
  consensus_protocol:
    type: "Byzantine Fault Tolerant"
    quorum: "2/3 of participants"
    
  participant_verification:
    method: "PKI certificates"
    ca: "Federated CA"
    revocation: "OCSP"
```

### 8.3 Graph Security

```yaml
GRAPH_SECURITY:
  access_control:
    level: "Node + Relationship"
    enforcement: "Neo4j RBAC"
    
  data_protection:
    encryption_at_rest: true
    encryption_in_transit: true
    
  query_protection:
    injection_prevention: "Parameterized Cypher"
    resource_limits:
      max_nodes: 10000
      timeout: "30s"
```

---

## 9. OBSERVABILITY & MONITORING

### 9.1 Metrics (Prometheus)

```yaml
METRICS_V3:
  # Business Metrics
  rulex_transactions_total:
    type: "counter"
    labels: ["status", "layer", "rule_category"]
    
  rulex_fraud_detected_total:
    type: "counter"
    labels: ["fraud_type", "layer"]
    
  rulex_false_positive_rate:
    type: "gauge"
    labels: ["rule_id", "segment"]
    
  # Technical Metrics
  rulex_evaluation_duration_seconds:
    type: "histogram"
    labels: ["layer", "operator_category"]
    buckets: [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25]
    
  rulex_graph_query_duration_seconds:
    type: "histogram"
    labels: ["query_type"]
    
  rulex_federated_rules_active:
    type: "gauge"
    
  rulex_threshold_updates_total:
    type: "counter"
    labels: ["rule_id", "direction"]
```

### 9.2 Tracing (OpenTelemetry)

```yaml
TRACING:
  provider: "OpenTelemetry"
  exporters: ["Jaeger", "Tempo"]
  
  spans:
    - "http.request"
    - "rule.evaluation"
    - "layer.execution"
    - "operator.execution"
    - "graph.query"
    - "redis.operation"
    - "database.query"
    
  attributes:
    - "transaction.id"
    - "customer.id"
    - "rule.id"
    - "layer.name"
    - "score.contribution"
```

### 9.3 Logging (ELK Stack)

```yaml
LOGGING:
  format: "JSON structured"
  levels:
    production: "INFO"
    debug: "DEBUG"
    
  fields:
    - timestamp
    - level
    - service
    - trace_id
    - span_id
    - transaction_id
    - customer_id (hashed)
    - rule_id
    - score
    - execution_time_ms
    
  pii_handling:
    strategy: "Hash or mask"
    fields_masked: ["customer_name", "email", "phone", "ssn"]
```

---

## 10. MIGRATION STRATEGY

### 10.1 Phased Rollout

```yaml
MIGRATION_PHASES:
  phase_1_foundation:
    duration: "4 weeks"
    scope:
      - "Neo4j deployment and testing"
      - "Redis cluster upgrade"
      - "New migrations (V39-V44)"
      - "Category 15 operators (10)"
    risk: "LOW"
    rollback: "Disable new operators"
    
  phase_2_graph:
    duration: "4 weeks"
    scope:
      - "Category 17 operators (10)"
      - "Graph service deployment"
      - "Layer 5 activation"
    risk: "MEDIUM"
    rollback: "Bypass Layer 5"
    
  phase_3_regulatory:
    duration: "3 weeks"
    scope:
      - "Category 16 operators (10)"
      - "Regulatory framework configuration"
      - "Layer 6 activation"
    risk: "LOW"
    rollback: "Bypass Layer 6"
    
  phase_4_iso20022:
    duration: "4 weeks"
    scope:
      - "Category 18 operators (4)"
      - "ISO 20022 parser integration"
      - "Message transformation"
    risk: "MEDIUM"
    rollback: "Bypass ISO parser"
    
  phase_5_federated:
    duration: "4 weeks"
    scope:
      - "Kafka deployment"
      - "Federated rule service"
      - "Layer 7 activation"
    risk: "HIGH"
    rollback: "Disable federation"
    
  phase_6_adaptive:
    duration: "4 weeks"
    scope:
      - "Threshold optimizer"
      - "BTL/ATL testing infrastructure"
      - "Statistical calibration"
    risk: "MEDIUM"
    rollback: "Static thresholds"
```

### 10.2 Feature Flags

```yaml
FEATURE_FLAGS:
  rulex.v3.graph.enabled: false
  rulex.v3.graph.layer5.enabled: false
  rulex.v3.regulatory.enabled: false
  rulex.v3.regulatory.layer6.enabled: false
  rulex.v3.iso20022.enabled: false
  rulex.v3.federated.enabled: false
  rulex.v3.federated.layer7.enabled: false
  rulex.v3.adaptive.enabled: false
  rulex.v3.adaptive.auto_apply: false
```

### 10.3 Backward Compatibility

```yaml
BACKWARD_COMPATIBILITY:
  api_versioning:
    v2_endpoints: "Preserved for 12 months"
    v3_endpoints: "New features"
    
  rule_format:
    v2_rules: "100% compatible"
    migration: "Automatic on load"
    
  data_format:
    existing_tables: "No breaking changes"
    new_tables: "Additive only"
    
  response_format:
    v2_response: "Preserved structure"
    v3_response: "Extended with new fields"
```

---

## 11. TESTING STRATEGY

### 11.1 Test Pyramid

```yaml
TEST_PYRAMID_V3:
  unit_tests:
    target: "90% coverage"
    focus:
      - "All 100+ operators"
      - "Score calculations"
      - "Graph algorithms"
      - "ISO 20022 parsing"
    framework: "JUnit 5 + Mockito"
    
  integration_tests:
    target: "80% coverage"
    focus:
      - "API endpoints"
      - "Database operations"
      - "Redis operations"
      - "Neo4j queries"
    framework: "Spring Test + Testcontainers"
    
  e2e_tests:
    target: "Critical paths"
    focus:
      - "Full transaction evaluation"
      - "Rule CRUD"
      - "Graph visualization"
      - "Federated rule adoption"
    framework: "Playwright"
    
  performance_tests:
    target: "<50ms P99"
    focus:
      - "Load testing (10K TPS)"
      - "Stress testing"
      - "Spike testing"
    framework: "k6 / Gatling"
```

### 11.2 Test Data Strategy

```yaml
TEST_DATA:
  synthetic_data:
    transactions: "1M synthetic transactions"
    customers: "100K synthetic customers"
    fraud_patterns: "50 known fraud patterns"
    
  graph_data:
    entities: "500K nodes"
    relationships: "2M edges"
    money_mule_networks: "100 known networks"
    
  iso20022_data:
    messages: "10K sample messages"
    types: ["pacs.008", "pacs.002", "pain.001"]
```

---

## 12. DEPLOYMENT ARCHITECTURE

### 12.1 Kubernetes Manifests

```yaml
# deployment-rulex-api.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: rulex-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: rulex-api
  template:
    metadata:
      labels:
        app: rulex-api
    spec:
      containers:
      - name: rulex-api
        image: rulex/api:v3.0.0
        ports:
        - containerPort: 8080
        resources:
          requests:
            memory: "2Gi"
            cpu: "1000m"
          limits:
            memory: "4Gi"
            cpu: "2000m"
        env:
        - name: SPRING_PROFILES_ACTIVE
          value: "production"
        - name: JAVA_OPTS
          value: "-XX:+UseZGC -Xmx3g"
        readinessProbe:
          httpGet:
            path: /actuator/health/readiness
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        livenessProbe:
          httpGet:
            path: /actuator/health/liveness
            port: 8080
          initialDelaySeconds: 60
          periodSeconds: 30
```

### 12.2 Infrastructure as Code

```yaml
INFRASTRUCTURE:
  terraform:
    provider: "AWS / Azure / GCP"
    modules:
      - "vpc_network"
      - "eks_cluster"
      - "rds_postgresql"
      - "elasticache_redis"
      - "neo4j_aura" # or self-managed
      - "msk_kafka"
      
  helm_charts:
    - "rulex-api"
    - "rulex-graph-service"
    - "rulex-federated-service"
    - "prometheus-stack"
    - "jaeger"
```

---

## 13. TECHNICAL SPECIFICATIONS

### 13.1 System Requirements

```yaml
REQUIREMENTS_V3:
  minimum:
    cpu: "8 cores"
    ram: "32 GB"
    storage: "500 GB SSD"
    
  recommended:
    cpu: "32 cores"
    ram: "128 GB"
    storage: "2 TB NVMe"
    
  production:
    api_nodes: "6x (16 cores, 64GB)"
    postgresql: "Primary + 2 replicas (32 cores, 256GB)"
    redis_cluster: "6 shards (8 cores, 32GB each)"
    neo4j_cluster: "3 cores + 3 read replicas (16 cores, 64GB each)"
    kafka_cluster: "3 brokers (8 cores, 32GB each)"
```

### 13.2 Performance Targets

```yaml
PERFORMANCE_TARGETS:
  latency:
    p50: "15ms"
    p90: "35ms"
    p95: "45ms"
    p99: "50ms"
    
  throughput:
    target: "10,000 TPS"
    burst: "50,000 TPS"
    
  availability:
    target: "99.99%"
    rpo: "0 (synchronous replication)"
    rto: "< 5 minutes"
    
  scalability:
    horizontal: "Linear to 100 nodes"
    vertical: "Up to 256GB per node"
```

---

## 14. IMPLEMENTATION GUIDES

### 14.1 Adding a New Operator

```java
// Step 1: Add to ConditionOperator enum
public enum ConditionOperator {
    // ...existing operators...
    MY_NEW_OPERATOR
}

// Step 2: Implement in appropriate Operators class
@Service
public class MyOperators {
    public boolean myNewOperator(Object... params) {
        // Deterministic implementation
        return result;
    }
}

// Step 3: Register in ComplexRuleEvaluator
case MY_NEW_OPERATOR:
    return myOperators.myNewOperator(
        getFieldValue(payload, condition.getFieldName()),
        condition.getValue()
    );

// Step 4: Add frontend support
// client/src/components/RuleFormDialog/operatorOptions.ts
{ value: 'MY_NEW_OPERATOR', label: 'My New Operator', category: 'custom' }

// Step 5: Add tests
@Test
void testMyNewOperator() {
    assertTrue(evaluator.evaluateCondition(
        condition, payload, context
    ));
}

// Step 6: Add documentation
// docs/RULEX_REFERENCIA_PARAMETROS_OPERADORES.md
```

### 14.2 Adding a New Layer

```java
// Step 1: Define layer in configuration
@Configuration
public class LayerConfig {
    @Bean
    public Layer layer5Graph() {
        return Layer.builder()
            .name("GRAPH_ANALYTICS")
            .order(5)
            .latencyBudgetMs(25)
            .enabled(true)
            .build();
    }
}

// Step 2: Implement layer evaluator
@Service
public class GraphLayerEvaluator implements LayerEvaluator {
    @Override
    public LayerResult evaluate(TransactionRequest request) {
        // Graph-specific evaluation logic
    }
    
    @Override
    public int getOrder() {
        return 5;
    }
}

// Step 3: Register in orchestrator
@Service
public class RuleOrchestrator {
    @Autowired
    private List<LayerEvaluator> layerEvaluators;
    
    public RuleResult evaluate(TransactionRequest request) {
        return layerEvaluators.stream()
            .sorted(Comparator.comparingInt(LayerEvaluator::getOrder))
            .map(evaluator -> evaluator.evaluate(request))
            .reduce(RuleResult::merge)
            .orElse(RuleResult.empty());
    }
}
```

---

## 15. APPENDICES

### 15.1 Glossary

| Term | Definition |
|------|------------|
| **BTL Testing** | Below-the-Line Testing - A/B testing with suppressed alerts |
| **CoP** | Confirmation of Payee (PSD3 requirement) |
| **DORA** | Digital Operational Resilience Act (EU) |
| **EUDI Wallet** | European Digital Identity Wallet |
| **Federated Rules** | Rules shared between banks (definitions only) |
| **HARDSTOP** | Immediate blocking layer (95-100 score) |
| **Money Mule** | Account used to launder fraud proceeds |
| **Operator** | Atomic evaluation function (e.g., GT, IN, BETWEEN) |
| **PSD3** | Payment Services Directive 3 (EU) |

### 15.2 Reference Architecture Diagrams

[Diagramas disponíveis em formato Mermaid/PlantUML nos arquivos de documentação]

### 15.3 API Reference

[Swagger/OpenAPI specification em `/openapi/rulex.yaml`]

---

**FIM DO DOCUMENTO TÉCNICO**

**Assinatura Digital**: RULEX-V3-TECHNICAL-BLUEPRINT-2026-01-12
