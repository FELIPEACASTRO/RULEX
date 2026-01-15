# 📋 Inventário Completo de Operadores - RULEX

> **Data de Geração:** 2026-01-15
> **Versão:** 1.0
> **Total de Operadores Únicos:** 465

---

## 📊 Resumo Executivo

| Camada | Total Operadores | Status |
|--------|------------------|--------|
| **FrontEnd** (operators.ts) | 448 | ✅ |
| **BackEnd** (RuleCondition.java) | 457 | ✅ |
| **PostgreSQL** (V34 migration) | 448 | ✅ |
| **Redis** (VelocityService) | 17 | ✅ |
| **Neo4j** (GraphService) | 18 | ✅ |

---

## 🖥️ FrontEnd Operators

### Localização
- **Arquivo Principal:** `client/src/lib/operators.ts`
- **Tipos:** `client/src/lib/operatorTypes.ts`
- **Schema:** `client/src/components/RuleFormDialog/schema.ts`
- **Testes:** `client/src/components/RuleFormDialog/operators.test.ts`

### Categorias de Operadores (448 total)

| Categoria | Quantidade | Exemplos |
|-----------|------------|----------|
| Behavioral Phase 1B | 215 | ADAPTIVE_BEHAVIORAL_ANALYTICS, DOCUMENT_FORGERY_DETECTION |
| Velocity Phase 1 | 40 | VELOCITY_COUNT_GT, VELOCITY_SUM_LT |
| Agregações Temporais | 34 | SUM_LAST_N_DAYS, COUNT_LAST_N_HOURS |
| Fraude Avançada | 26 | STRUCTURING_DETECTION, BUST_OUT_PATTERN |
| Neo4j Graph | 18 | NEO4J_FRAUD_RING_DETECTION, NEO4J_PAGERANK |
| Merchant/MCC | 13 | MCC_HIGH_RISK, MERCHANT_VELOCITY_SPIKE |
| Velocity Avançado | 10 | VELOCITY_CROSS_CHANNEL, VELOCITY_TREND |
| Velocity | 8 | VELOCITY_COUNT_GT, VELOCITY_AVG_LT |
| Synthetic ID | 8 | CPF_SSN_VALIDATION, IDENTITY_VELOCITY |
| Regulatory | 8 | SCA_EXEMPTION_TRA, GDPR_DATA_RETENTION |
| Graph/Network | 8 | FAN_OUT_COUNT, CIRCULAR_TRANSFER_DETECTION |
| Behavioral | 8 | DORMANCY_REVIVAL, AMOUNT_DEVIATION_FROM_AVG |
| AML | 8 | STRUCTURING_DETECTION, LAYERING_PATTERN |
| Sanctions | 7 | OFAC_LIST_CHECK, PEP_LIST_CHECK |
| Device | 7 | DEVICE_JAILBREAK_ROOTED, VPN_PROXY_DETECTION |
| Strings | 6 | CONTAINS, STARTS_WITH, REGEX |
| Data/Hora | 6 | DATE_BEFORE, TIME_BETWEEN |
| Comparação entre Campos | 6 | FIELD_EQ, FIELD_GT |
| Comparação Básica | 6 | EQ, NEQ, GT, LT, GTE, LTE |
| Estatísticos | 5 | BENFORD_LAW_DEVIATION, Z_SCORE_GT |
| Arrays | 5 | ARRAY_CONTAINS, ARRAY_SIZE_EQ |
| Nulos/Booleanos | 4 | IS_NULL, NOT_NULL, IS_TRUE |
| Geolocalização | 3 | GEO_DISTANCE_LT, GEO_IN_POLYGON |
| Range | 2 | BETWEEN, NOT_BETWEEN |
| Matemáticos | 2 | MOD_EQ, MOD_NEQ |
| Listas | 2 | IN, NOT_IN |

---

## ⚙️ BackEnd Operators (Java/Spring Boot)

### Localização
- **Entity:** `backend/src/main/java/com/rulex/entity/complex/RuleCondition.java`
- **DTO:** `backend/src/main/java/com/rulex/dto/complex/ConditionDTO.java`
- **Evaluator:** `backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`

### Enum ConditionOperator (457 valores)

#### Operadores Básicos (6)
```java
EQ, NEQ, GT, GTE, LT, LTE
```

#### Operadores de Lista (4)
```java
IN, NOT_IN, BETWEEN, NOT_BETWEEN
```

#### Operadores de String (6)
```java
CONTAINS, NOT_CONTAINS, STARTS_WITH, ENDS_WITH, REGEX, NOT_REGEX
```

#### Operadores Nulos/Booleanos (4)
```java
IS_NULL, NOT_NULL, IS_TRUE, IS_FALSE
```

#### Operadores de Campo (6)
```java
FIELD_EQ, FIELD_NEQ, FIELD_GT, FIELD_GTE, FIELD_LT, FIELD_LTE
```

#### Operadores de Data/Hora (6)
```java
DATE_BEFORE, DATE_AFTER, DATE_BETWEEN, TIME_BEFORE, TIME_AFTER, TIME_BETWEEN
```

#### Operadores de Array (5)
```java
ARRAY_CONTAINS, ARRAY_NOT_CONTAINS, ARRAY_SIZE_EQ, ARRAY_SIZE_GT, ARRAY_SIZE_LT
```

#### Operadores Geográficos (3)
```java
GEO_DISTANCE_LT, GEO_DISTANCE_GT, GEO_IN_POLYGON
```

#### Operadores de Velocity (17)
```java
VELOCITY_COUNT_GT, VELOCITY_COUNT_LT, VELOCITY_SUM_GT, VELOCITY_SUM_LT,
VELOCITY_AVG_GT, VELOCITY_AVG_LT, VELOCITY_DISTINCT_GT, VELOCITY_DISTINCT_LT,
VELOCITY_SPIKE, VELOCITY_TREND, VELOCITY_ACCELERATION, VELOCITY_CROSS_CHANNEL,
VELOCITY_PERCENTILE, VELOCITY_RATIO_GT, VELOCITY_ROLLING_WINDOW
```

#### Operadores Neo4j (18)
```java
NEO4J_WEAKLY_CONNECTED_COMPONENTS, NEO4J_DEGREE_CENTRALITY,
NEO4J_PAGERANK_FRAUD_SCORE, NEO4J_LOUVAIN_COMMUNITY_DETECTION,
NEO4J_PAIRWISE_SIMILARITY_PII, NEO4J_ENTITY_RESOLUTION_SHARED_PII,
NEO4J_FRAUD_RING_DETECTION, NEO4J_MONEY_MULE_NETWORK_ANALYSIS,
NEO4J_CIRCULAR_TRANSACTION_DETECTION, NEO4J_FIRST_PARTY_FRAUD_CLUSTERING,
NEO4J_SECOND_LEVEL_FRAUDSTER_ID, NEO4J_BETWEENNESS_CENTRALITY_MULE,
NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD, NEO4J_SHORTEST_PATH_AML_TRACKING,
NEO4J_TRIANGLE_COUNT_COLLUSION, NEO4J_NODE_SIMILARITY_SYNTHETIC_ID,
NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION, NEO4J_TEMPORAL_MOTIF_PATTERN
```

#### Operadores FATF (28)
```java
FATF_RECOMMENDATION_1, FATF_RECOMMENDATION_2, ... FATF_RECOMMENDATION_28
```

#### Operadores PLT (28)
```java
PLT_RULE_001, PLT_RULE_002, ... PLT_RULE_028
```

#### Operadores BSL (14)
```java
BSL_BUCKET_CLASSIFICATION, BSL_BUSINESS_INDICATOR, BSL_CONTROL_DEFICIENCY, ...
```

---

## 🗄️ PostgreSQL Operators

### Localização
- **Migration Principal:** `backend/src/main/resources/db/migration/V34__add_v31_plus_operators.sql`
- **Migrations Anteriores:**
  - `V15__add_velocity_operators.sql`
  - `V28__add_missing_condition_operators.sql`
  - `V32__add_missing_tables_for_operators.sql`

### Enum `condition_operator` (448 valores)

O enum PostgreSQL `condition_operator` contém todos os operadores definidos via:
```sql
ALTER TYPE condition_operator ADD VALUE IF NOT EXISTS 'OPERATOR_NAME';
```

---

## 🔴 Redis Operators (Velocity)

### Localização
- **Service Principal:** `backend/src/main/java/com/rulex/service/RedisVelocityService.java`
- **Cache Service:** `backend/src/main/java/com/rulex/service/RedisVelocityCacheService.java`
- **Facade:** `backend/src/main/java/com/rulex/service/VelocityServiceFacade.java`

### Operadores Suportados (17)

| Operador | Descrição | Janelas Temporais |
|----------|-----------|-------------------|
| VELOCITY_COUNT_GT | Contagem maior que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_COUNT_LT | Contagem menor que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_SUM_GT | Soma maior que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_SUM_LT | Soma menor que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_AVG_GT | Média maior que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_AVG_LT | Média menor que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_DISTINCT_GT | Distintos maior que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_DISTINCT_LT | Distintos menor que | 5min, 15min, 30min, 1h, 6h, 12h, 24h, 7d, 30d |
| VELOCITY_SPIKE | Pico de velocidade | Comparação entre janelas |
| VELOCITY_TREND | Tendência | Análise temporal |
| VELOCITY_ACCELERATION | Aceleração | Derivada da velocidade |
| VELOCITY_CROSS_CHANNEL | Cross-channel | Múltiplos canais |
| VELOCITY_PERCENTILE | Percentil | Distribuição estatística |
| VELOCITY_RATIO_GT | Ratio maior que | Comparação de ratios |
| VELOCITY_ROLLING_WINDOW | Janela deslizante | Sliding window |

### Estruturas de Dados Redis

```
operators:velocity:{keyType}:{windowMinutes} -> SortedSet
operators:velocity:distinct:{keyType}:{windowMinutes} -> HyperLogLog
operators:velocity:sum:{keyType}:{windowMinutes} -> String (BigDecimal)
```

---

## 🔵 Neo4j Operators (Graph)

### Localização
- **Service:** `backend/src/main/java/com/rulex/service/Neo4jGraphService.java`
- **Evaluator:** `backend/src/main/java/com/rulex/service/complex/ComplexRuleEvaluator.java`

### Operadores Implementados (18)

| Operador | Código | Descrição | Algoritmo |
|----------|--------|-----------|-----------|
| NEO4J_WEAKLY_CONNECTED_COMPONENTS | NEO001 | Componentes conectados | WCC |
| NEO4J_DEGREE_CENTRALITY | NEO002 | Centralidade de grau | Degree |
| NEO4J_PAGERANK_FRAUD_SCORE | NEO003 | Score PageRank | PageRank |
| NEO4J_LOUVAIN_COMMUNITY_DETECTION | NEO004 | Detecção de comunidade | Louvain |
| NEO4J_PAIRWISE_SIMILARITY_PII | NEO005 | Similaridade PII | Jaccard |
| NEO4J_ENTITY_RESOLUTION_SHARED_PII | NEO006 | Resolução de entidade | Entity Resolution |
| NEO4J_FRAUD_RING_DETECTION | NEO007 | Detecção de anel | Cycle Detection |
| NEO4J_MONEY_MULE_NETWORK_ANALYSIS | NEO008 | Análise money mule | Network Analysis |
| NEO4J_CIRCULAR_TRANSACTION_DETECTION | NEO009 | Transação circular | Cycle Detection |
| NEO4J_FIRST_PARTY_FRAUD_CLUSTERING | NEO010 | Clustering 1ª parte | Clustering |
| NEO4J_SECOND_LEVEL_FRAUDSTER_ID | NEO011 | Fraudador 2º nível | BFS |
| NEO4J_BETWEENNESS_CENTRALITY_MULE | NEO012 | Centralidade intermediação | Betweenness |
| NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD | NEO013 | Propagação de label | Label Propagation |
| NEO4J_SHORTEST_PATH_AML_TRACKING | NEO014 | Caminho mais curto | Dijkstra |
| NEO4J_TRIANGLE_COUNT_COLLUSION | NEO015 | Contagem triângulos | Triangle Count |
| NEO4J_NODE_SIMILARITY_SYNTHETIC_ID | NEO016 | Similaridade de nó | Node Similarity |
| NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION | NEO017 | Embedding de grafo | Graph Embedding |
| NEO4J_TEMPORAL_MOTIF_PATTERN | NEO018 | Padrão temporal | Motif Detection |

### Modelo de Grafo Neo4j

```cypher
// Nós
(:Account {id, customerId, type, createdAt})
(:Transaction {id, amount, timestamp, channel})
(:Device {id, fingerprint, type})
(:IP {address, country, isVPN})

// Relacionamentos
(a:Account)-[:TRANSFERRED_TO {amount, timestamp}]->(b:Account)
(a:Account)-[:USES_DEVICE]->(d:Device)
(a:Account)-[:CONNECTS_FROM]->(ip:IP)
(t:Transaction)-[:FROM]->(a:Account)
(t:Transaction)-[:TO]->(b:Account)
```

---

## 📁 Arquivos de Referência

| Camada | Arquivo | Linhas | Operadores |
|--------|---------|--------|------------|
| FrontEnd | operators.ts | ~1500 | 448 |
| FrontEnd | operatorTypes.ts | ~500 | 448 |
| FrontEnd | schema.ts | ~800 | 448 |
| BackEnd | RuleCondition.java | ~1200 | 457 |
| BackEnd | ConditionDTO.java | ~1000 | 457 |
| BackEnd | ComplexRuleEvaluator.java | ~3500 | 520 cases |
| PostgreSQL | V34__add_v31_plus_operators.sql | ~500 | 448 |
| Redis | RedisVelocityService.java | ~600 | 17 |
| Neo4j | Neo4jGraphService.java | ~500 | 18 |

---

## 🔗 Referências

- [RuleCondition.java](../backend/src/main/java/com/rulex/entity/complex/RuleCondition.java)
- [operators.ts](../client/src/lib/operators.ts)
- [Neo4jGraphService.java](../backend/src/main/java/com/rulex/service/Neo4jGraphService.java)
- [RedisVelocityService.java](../backend/src/main/java/com/rulex/service/RedisVelocityService.java)
