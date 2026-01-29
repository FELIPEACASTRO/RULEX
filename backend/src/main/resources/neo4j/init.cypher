// =====================================================================
// RULEX Neo4j Graph Database Initialization Script
// V5.0: Fraud Ring Detection Graph Schema
// =====================================================================

// =====================================================================
// 1. CLEANUP (opcional - descomentar para reset completo)
// =====================================================================
// MATCH (n) DETACH DELETE n;

// =====================================================================
// 2. CONSTRAINTS - Garantir unicidade dos nodes
// =====================================================================

// Account nodes - primary entity for customers
CREATE CONSTRAINT account_id_unique IF NOT EXISTS
FOR (a:Account)
REQUIRE a.id IS UNIQUE;

// Merchant nodes - transaction destinations
CREATE CONSTRAINT merchant_id_unique IF NOT EXISTS
FOR (m:Merchant)
REQUIRE m.id IS UNIQUE;

// Customer nodes - alternative representation
CREATE CONSTRAINT customer_id_unique IF NOT EXISTS
FOR (c:Customer)
REQUIRE c.id IS UNIQUE;

// Device nodes - for device fingerprinting
CREATE CONSTRAINT device_id_unique IF NOT EXISTS
FOR (d:Device)
REQUIRE d.id IS UNIQUE;

// IP Address nodes - for network analysis
CREATE CONSTRAINT ip_address_unique IF NOT EXISTS
FOR (ip:IPAddress)
REQUIRE ip.address IS UNIQUE;

// =====================================================================
// 3. INDEXES - Performance optimization for fraud queries
// =====================================================================

// Account indexes
CREATE INDEX account_created_idx IF NOT EXISTS
FOR (a:Account)
ON (a.createdAt);

CREATE INDEX account_risk_score_idx IF NOT EXISTS
FOR (a:Account)
ON (a.riskScore);

// Merchant indexes
CREATE INDEX merchant_mcc_idx IF NOT EXISTS
FOR (m:Merchant)
ON (m.mcc);

CREATE INDEX merchant_country_idx IF NOT EXISTS
FOR (m:Merchant)
ON (m.country);

// Transaction relationship properties index
CREATE INDEX tx_timestamp_idx IF NOT EXISTS
FOR ()-[t:TRANSACTED_WITH]-()
ON (t.timestamp);

CREATE INDEX tx_amount_idx IF NOT EXISTS
FOR ()-[t:TRANSACTED_WITH]-()
ON (t.amount);

// =====================================================================
// 4. VERIFY SCHEMA
// =====================================================================

// Show all constraints
SHOW CONSTRAINTS;

// Show all indexes
SHOW INDEXES;

// =====================================================================
// 5. SAMPLE DATA (para teste - pode ser removido em produção)
// =====================================================================

// Create sample accounts for testing
MERGE (a1:Account {id: 'ACCT_1001'})
ON CREATE SET a1.createdAt = datetime(), a1.riskScore = 0;

MERGE (a2:Account {id: 'ACCT_1002'})
ON CREATE SET a2.createdAt = datetime(), a2.riskScore = 0;

MERGE (a3:Account {id: 'ACCT_1003'})
ON CREATE SET a3.createdAt = datetime(), a3.riskScore = 0;

// Create sample merchants
MERGE (m1:Merchant {id: 'MERCH_AMAZON_001'})
ON CREATE SET m1.name = 'Amazon', m1.mcc = '5411', m1.country = 'US';

MERGE (m2:Merchant {id: 'MERCH_WALMART_001'})
ON CREATE SET m2.name = 'Walmart', m2.mcc = '5411', m2.country = 'US';

// Create sample transactions
MERGE (a1)-[:TRANSACTED_WITH {
  amount: 150.00,
  timestamp: datetime() - duration('P1D'),
  txId: 'TX_SAMPLE_001'
}]->(m1);

MERGE (a2)-[:TRANSACTED_WITH {
  amount: 200.00,
  timestamp: datetime() - duration('P2D'),
  txId: 'TX_SAMPLE_002'
}]->(m1);

MERGE (a1)-[:TRANSACTED_WITH {
  amount: 75.00,
  timestamp: datetime(),
  txId: 'TX_SAMPLE_003'
}]->(m2);

// =====================================================================
// 6. VERIFY DATA
// =====================================================================

// Count nodes by label
MATCH (n)
RETURN labels(n) AS label, count(n) AS count
ORDER BY count DESC;

// Count relationships
MATCH ()-[r]->()
RETURN type(r) AS relationship, count(r) AS count;

// =====================================================================
// END OF INITIALIZATION SCRIPT
// =====================================================================
