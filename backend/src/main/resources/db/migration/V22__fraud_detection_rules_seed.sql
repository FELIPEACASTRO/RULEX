-- V22__fraud_detection_rules_seed.sql
-- ===================================================================================
-- RULEX FRAUD DETECTION RULES - COMPREHENSIVE SEED DATA
-- ===================================================================================
-- Based on industry research from: FBI, ACFE, FICO Falcon, IBM Fraud Detection,
-- Chargebacks911, Neoway, PwC, Visa/Mastercard best practices, and academic papers.
-- ===================================================================================
-- Note: Using {variable} syntax instead of {variable} in reason_template fields
-- to avoid Flyway placeholder interpretation issues.
-- ===================================================================================
-- Categories:
--   1. Technical Validation Rules (card integrity, crypto validation)
--   2. High-Risk MCC Rules (gambling, crypto, money transfer, adult)
--   3. Velocity Check Rules (transaction frequency, geographic anomalies)
--   4. Behavioral Anomaly Rules (unusual patterns, first-time behaviors)
--   5. Money Laundering Detection Rules (smurfing, layering, structuring)
--   6. Card-Not-Present (CNP) Fraud Rules
--   7. Account Takeover (ATO) Indicators
--   8. BIN Attack / Card Testing Rules
-- ===================================================================================

-- =========================================
-- PART 1: SIMPLE RULES (rule_configurations)
-- Technical Validation + Basic Detection
-- =========================================

-- Ensure table exists (idempotent)
CREATE TABLE IF NOT EXISTS rule_configurations (
  id BIGSERIAL PRIMARY KEY,
  rule_name VARCHAR(100) NOT NULL UNIQUE,
  description TEXT,
  rule_type VARCHAR(20) NOT NULL CHECK (rule_type IN ('SECURITY', 'CONTEXT', 'VELOCITY', 'ANOMALY')),
  threshold INTEGER NOT NULL,
  weight INTEGER NOT NULL CHECK (weight >= 0 AND weight <= 100),
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  classification VARCHAR(20) NOT NULL CHECK (classification IN ('APPROVED', 'SUSPICIOUS', 'FRAUD')),
  parameters TEXT,
  conditions_json JSONB,
  logic_operator VARCHAR(3) DEFAULT 'AND' CHECK (logic_operator IN ('AND', 'OR')),
  shadow_mode VARCHAR(20) DEFAULT 'DISABLED',
  canary_percentage INTEGER DEFAULT 0,
  version INTEGER NOT NULL DEFAULT 1,
  created_at TIMESTAMP NOT NULL DEFAULT now(),
  updated_at TIMESTAMP NOT NULL DEFAULT now()
);

-- ===================================================================================
-- CATEGORY 1: TECHNICAL VALIDATION RULES
-- These rules validate card/transaction integrity
-- ===================================================================================

-- 1.1 Expired Card Detection
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARD_EXPIRED_OBJECTIVE',
  'Card expiration date is before transaction date - objective inconsistency indicating potential fraud',
  'SECURITY',
  100,
  95,
  true,
  'FRAUD',
  '[{"field":"cardExpireDate","operator":"LT","value":"transactionDate","valueType":"FIELD_REFERENCE"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.2 ATC Mismatch Detection (Application Transaction Counter)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ATC_MISMATCH_HIGH',
  'ATC card value differs significantly from host value - potential cloned card or replay attack',
  'SECURITY',
  80,
  90,
  true,
  'FRAUD',
  '[{"field":"atcCard","operator":"GT","value":"atcHost","valueType":"FIELD_REFERENCE"},{"field":"ABS_DIFF(atcCard,atcHost)","operator":"GT","value":"10","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.3 Invalid Cryptogram
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CRYPTOGRAM_INVALID',
  'EMV cryptogram validation failed - indicates counterfeit card or tampering',
  'SECURITY',
  100,
  95,
  true,
  'FRAUD',
  '[{"field":"cryptogramValid","operator":"EQ","value":"N"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.4 CVV2 Failed with High Amount
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CVV2_FAILED_HIGH_AMOUNT',
  'CVV2 verification failed on transaction above R$1,000 - elevated risk',
  'SECURITY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"cvv2Response","operator":"NEQ","value":"M"},{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.5 PIN Verification Failed
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIN_VERIFICATION_FAILED',
  'Offline PIN verification was performed but failed - potential stolen card with wrong PIN attempts',
  'SECURITY',
  70,
  85,
  true,
  'SUSPICIOUS',
  '[{"field":"cvrofflinePinVerificationPerformed","operator":"EQ","value":"1"},{"field":"cvrofflinePinVerificationFailed","operator":"EQ","value":"1"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.6 Low ECI Indicator (No 3DS)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'LOW_ECI_HIGH_AMOUNT',
  'E-commerce transaction without 3D Secure authentication (ECI 7) above R$2,000 - higher CNP fraud risk',
  'SECURITY',
  60,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"eciIndicator","operator":"EQ","value":"7"},{"field":"transactionAmount","operator":"GT","value":"200000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.7 Token Assurance Level Zero
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'TOKEN_ASSURANCE_ZERO',
  'Tokenized transaction with lowest assurance level (0) - token provisioning without strong ID verification',
  'SECURITY',
  50,
  60,
  true,
  'SUSPICIOUS',
  '[{"field":"tokenAssuranceLevel","operator":"EQ","value":"0"},{"field":"transactionAmount","operator":"GT","value":"50000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.8 Consumer Auth Score Very Low
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'LOW_CONSUMER_AUTH_SCORE',
  'Consumer authentication score below threshold indicates weak authentication',
  'SECURITY',
  55,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"consumerAuthenticationScore","operator":"LT","value":"30"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 2: HIGH-RISK MCC (MERCHANT CATEGORY CODE) RULES
-- Industry-standard high-risk merchant categories
-- ===================================================================================

-- 2.1 Gambling MCC - High Amount
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_GAMBLING_HIGH_VALUE',
  'Gambling/betting transaction (MCC 7995, 7994, 7993) above R$500 - high-risk category',
  'CONTEXT',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"7995,7994,7993"},{"field":"transactionAmount","operator":"GT","value":"50000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.2 Crypto / Quasi-Cash MCC
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_CRYPTO_QUASI_CASH',
  'Cryptocurrency or quasi-cash purchase (MCC 6051, 6211) - high money laundering risk',
  'CONTEXT',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"6051,6211"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.3 Wire Transfer / Money Order
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_WIRE_TRANSFER',
  'Wire transfer or money order (MCC 4829) - frequently used in scam/fraud schemes',
  'CONTEXT',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"4829,6012"},{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.4 Dating Services
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_DATING_SERVICES',
  'Dating/escort services (MCC 7273, 7297) - high chargeback and fraud category',
  'CONTEXT',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"7273,7297"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.5 Direct Marketing / Telemarketing
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_TELEMARKETING',
  'Telemarketing (MCC 5966, 5967) - high fraud and chargeback risk category',
  'CONTEXT',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"5966,5967"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.6 Manual Cash Disbursement (ATM with operator)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_MANUAL_CASH',
  'Manual cash disbursement (MCC 6010) - high risk for card cloning exploitation',
  'CONTEXT',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"EQ","value":"6010"},{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.7 Pawn Shops
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_PAWN_SHOPS',
  'Pawn shop transaction (MCC 5933) - potential money laundering or stolen goods',
  'CONTEXT',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"EQ","value":"5933"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.8 Jewelry High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_JEWELRY_HIGH_VALUE',
  'High-value jewelry purchase (MCC 5944) above R$5,000 - high resale value target for fraudsters',
  'CONTEXT',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"EQ","value":"5944"},{"field":"transactionAmount","operator":"GT","value":"500000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.9 Electronics High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_ELECTRONICS_HIGH_VALUE',
  'High-value electronics purchase (MCC 5732, 5734) above R$3,000 - frequently targeted by fraudsters',
  'CONTEXT',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"5732,5734"},{"field":"transactionAmount","operator":"GT","value":"300000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.10 Money Services Business
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_MONEY_SERVICES',
  'Money services business (MCC 6012, 6050, 6051) - AML red flag category',
  'CONTEXT',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"6012,6050,6051"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 3: VELOCITY CHECK RULES
-- Transaction frequency and pattern analysis
-- ===================================================================================

-- 3.1 Multiple Transactions Same Card Short Period
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_SAME_CARD_RAPID',
  'Multiple transactions from same card within 5 minutes - potential automated fraud',
  'VELOCITY',
  80,
  85,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"VELOCITY_COUNT_5MIN","value":"3"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.2 High Daily Transaction Count
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_HIGH_DAILY_COUNT',
  'Card used more than 10 times in 24 hours - unusual activity pattern',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"VELOCITY_COUNT_24H","value":"10"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.3 High Daily Volume
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_HIGH_DAILY_VOLUME',
  'Daily transaction volume exceeds R$20,000 - potential bust-out or account takeover',
  'VELOCITY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"VELOCITY_SUM_24H","value":"2000000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.4 Multiple Merchants Same Card Short Period
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_MULTI_MERCHANT_RAPID',
  'Same card at 5+ different merchants within 1 hour - card testing or fraud spree',
  'VELOCITY',
  85,
  90,
  true,
  'FRAUD',
  '[{"field":"pan","operator":"VELOCITY_DISTINCT_MERCHANT_1H","value":"5"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.5 Geographic Velocity Anomaly
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_GEO_IMPOSSIBLE',
  'Transactions in different countries within 2 hours - physically impossible travel',
  'VELOCITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field":"pan","operator":"VELOCITY_DISTINCT_COUNTRY_2H","value":"2"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.6 Rapid Decline then Approve Pattern
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_DECLINE_APPROVE',
  'Multiple declined transactions followed by approval - potential card testing',
  'VELOCITY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"VELOCITY_DECLINE_COUNT_1H","value":"3"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.7 Same Amount Multiple Times
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_SAME_AMOUNT_REPEAT',
  'Same exact amount charged 3+ times in 24 hours - potential duplicate or fraudulent billing',
  'VELOCITY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"VELOCITY_SAME_AMOUNT_24H","value":"3"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 4: BEHAVIORAL ANOMALY RULES
-- Unusual patterns that deviate from normal behavior
-- ===================================================================================

-- 4.1 Night Transaction International High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_NIGHT_INTERNATIONAL',
  'International transaction (non-Brazil) between midnight and 5am above R$1,000 - high risk window',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"merchantCountryCode","operator":"NEQ","value":"076"},{"field":"transactionTime","operator":"BETWEEN","valueMin":"0","valueMax":"50000"},{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.2 First Transaction High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_FIRST_TXN_HIGH',
  'First transaction on card above R$2,000 - unusual for newly activated cards',
  'ANOMALY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"IS_FIRST_TRANSACTION"},{"field":"transactionAmount","operator":"GT","value":"200000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.3 Round Amount Suspicious
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_ROUND_AMOUNT',
  'Perfectly round amount (multiple of R$1,000) above R$5,000 - unusual pattern in legitimate commerce',
  'ANOMALY',
  55,
  60,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"MOD_EQ","value":"100000,0"},{"field":"transactionAmount","operator":"GT","value":"500000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.4 POS Entry Mode Mismatch
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_POS_ENTRY_MISMATCH',
  'Manual key entry (fallback) on chip card - potential skimming or card cloning',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"posEntryMode","operator":"EQ","value":"1"},{"field":"cardMediaType","operator":"EQ","value":"C"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.5 Customer Not Present High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_CNP_HIGH_VALUE',
  'Card-not-present transaction above R$3,000 - elevated CNP fraud risk',
  'ANOMALY',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"customerPresent","operator":"EQ","value":"0"},{"field":"transactionAmount","operator":"GT","value":"300000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.6 Available Credit Exhaustion
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_CREDIT_EXHAUSTION',
  'Transaction consumes more than 90% of available credit - potential bust-out fraud',
  'ANOMALY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"GT","value":"availableCredit*0.9","valueType":"EXPRESSION"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.7 Delinquent Account Activity
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_DELINQUENT_ACTIVE',
  'Transaction on account with significant delinquent balance - potential fraud or bust-out',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"cardDelinquentAmount","operator":"GT","value":"50000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.8 Off-Premises POS High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_OFF_PREMISES_HIGH',
  'Off-premises POS transaction (mobile vendor) above R$2,000 - higher fraud risk environment',
  'ANOMALY',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"posOffPremises","operator":"EQ","value":"1"},{"field":"transactionAmount","operator":"GT","value":"200000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.9 Security Indicator Suspicious
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANOMALY_LOW_SECURITY',
  'Transaction with low security indicator combined with high value',
  'ANOMALY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"posSecurity","operator":"EQ","value":"0"},{"field":"transactionAmount","operator":"GT","value":"150000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 5: MONEY LAUNDERING DETECTION RULES
-- AML patterns: structuring, smurfing, layering
-- ===================================================================================

-- 5.1 Structuring (Just Under Reporting Threshold)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'AML_STRUCTURING_THRESHOLD',
  'Transaction just under R$10,000 reporting threshold (R$9,500-R$9,999) - potential structuring',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"BETWEEN","valueMin":"950000","valueMax":"999999"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 5.2 Multiple Small Transactions (Smurfing)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'AML_SMURFING_PATTERN',
  'Multiple small transactions (R$200-R$500 range) totaling over R$5,000 in 24h - smurfing indicator',
  'VELOCITY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"BETWEEN","valueMin":"20000","valueMax":"50000"},{"field":"pan","operator":"VELOCITY_COUNT_24H","value":"10"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 5.3 Rapid Fund Movement
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'AML_RAPID_MOVEMENT',
  'Large deposit followed by immediate withdrawals at MSB - layering indicator',
  'VELOCITY',
  80,
  85,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"6010,6011,6012,6051"},{"field":"pan","operator":"VELOCITY_SUM_1H","value":"500000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 5.4 Cross-Border High Frequency
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'AML_CROSS_BORDER_FREQ',
  'High frequency of cross-border transactions in 24h - potential money movement scheme',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"merchantCountryCode","operator":"NEQ","value":"076"},{"field":"pan","operator":"VELOCITY_COUNT_24H","value":"5"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 6: BIN ATTACK / CARD TESTING RULES
-- Patterns indicating automated card testing
-- ===================================================================================

-- 6.1 Sequential Amounts
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BIN_SEQUENTIAL_AMOUNTS',
  'Sequential small amounts (R$1, R$2, R$3...) - classic card testing pattern',
  'VELOCITY',
  90,
  95,
  true,
  'FRAUD',
  '[{"field":"transactionAmount","operator":"LTE","value":"1000"},{"field":"pan","operator":"VELOCITY_COUNT_10MIN","value":"5"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 6.2 Same BIN Multiple Cards
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BIN_ATTACK_SAME_BIN',
  'Multiple different cards from same BIN at same merchant in short time - BIN attack',
  'VELOCITY',
  90,
  95,
  true,
  'FRAUD',
  '[{"field":"pan","operator":"VELOCITY_SAME_BIN_DIFF_CARD_1H","value":"5"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 6.3 Micro-Transaction Testing
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BIN_MICRO_TRANSACTION',
  'Transaction under R$2 - classic card validation test amount',
  'ANOMALY',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"LT","value":"200"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 6.4 Donation/Charity Low Amount Testing
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BIN_CHARITY_TEST',
  'Small donation to charity (MCC 8398) under R$10 - frequently used for card testing',
  'ANOMALY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"EQ","value":"8398"},{"field":"transactionAmount","operator":"LT","value":"1000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 7: GEOGRAPHIC RISK RULES
-- Location-based fraud indicators
-- ===================================================================================

-- 7.1 High Risk Countries
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_HIGH_RISK_COUNTRY',
  'Transaction from high-risk country (historically high fraud rates)',
  'CONTEXT',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"merchantCountryCode","operator":"IN","value":"NG,GH,CI,CM,RO,UA,RU,PH,ID,VN"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 7.2 Sanctioned Countries
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_SANCTIONED_COUNTRY',
  'Transaction from OFAC sanctioned country - compliance requirement',
  'CONTEXT',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"merchantCountryCode","operator":"IN","value":"KP,IR,SY,CU"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 7.3 Cross-Border First Transaction
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_CROSS_BORDER_FIRST',
  'First international transaction on domestically-issued card - potential compromise',
  'ANOMALY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"merchantCountryCode","operator":"NEQ","value":"076"},{"field":"pan","operator":"IS_FIRST_INTL_TRANSACTION"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 8: ACCOUNT TAKEOVER INDICATORS
-- Signs of compromised accounts
-- ===================================================================================

-- 8.1 Sudden High Value After Dormancy
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ATO_DORMANT_ACTIVATION',
  'High value transaction on previously dormant card (no activity 60+ days)',
  'ANOMALY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"DAYS_SINCE_LAST_TXN","value":"60"},{"field":"transactionAmount","operator":"GT","value":"200000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 8.2 Rapid Address Change then High Value
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ATO_ADDRESS_CHANGE',
  'High value transaction shortly after address change - potential ATO',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"pan","operator":"ADDRESS_CHANGED_WITHIN_DAYS","value":"7"},{"field":"transactionAmount","operator":"GT","value":"300000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 8.3 CVV Retry Success
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ATO_CVV_RETRY',
  'Multiple CVV attempts (2+) before success - potential credential stuffing',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"cvvPinTryLimitExceeded","operator":"EQ","value":"1"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- PART 2: COMPLEX RULES (complex_rules + condition groups)
-- Advanced multi-condition rules with nested logic
-- ===================================================================================

-- Helper function to insert complex rule with conditions
-- We'll create the complex rules and their condition groups in sequence

-- Complex Rule 1: International Gambling Night
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'INTL_GAMBLING_NIGHT_COMPLEX',
  'International Gambling Transaction at Night',
  'Detects gambling transactions outside Brazil during night hours (00:00-06:00) with elevated amounts. Combines geographic, temporal, and MCC risk factors.',
  1,
  'PUBLISHED',
  95,
  90,
  'FRAUDE',
  'Transação de jogos/apostas (MCC {mcc}) detectada em {merchantCountryCode} às {transactionTime} no valor de R$ {transactionAmount/100}',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 2: Bust-Out Fraud Pattern
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'BUST_OUT_PATTERN_COMPLEX',
  'Bust-Out Fraud Pattern Detection',
  'Identifies potential bust-out fraud: high credit utilization combined with delinquency and high-value purchases at easily resellable goods merchants',
  1,
  'PUBLISHED',
  90,
  85,
  'FRAUDE',
  'Padrão de bust-out detectado: utilização de {(transactionAmount/availableCredit)*100}% do crédito em {merchantName} (MCC {mcc})',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 3: Card Testing Sequence
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'CARD_TESTING_SEQUENCE_COMPLEX',
  'Card Testing Attack Sequence',
  'Detects automated card testing: multiple low-value transactions at e-commerce/digital merchants in rapid succession',
  1,
  'PUBLISHED',
  98,
  95,
  'FRAUDE',
  'Ataque de teste de cartão detectado: {velocityCount} transações em {velocityMinutes} minutos, valores baixos em merchant digital',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 4: Money Mule Pattern
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'MONEY_MULE_PATTERN_COMPLEX',
  'Money Mule Activity Pattern',
  'Identifies potential money mule behavior: receives funds then immediately transfers/withdraws at MSBs',
  1,
  'PUBLISHED',
  88,
  82,
  'SUSPEITA_DE_FRAUDE',
  'Padrão de money mule detectado: transações em MSB (MCC {mcc}) após recebimento de fundos',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 5: Device Fingerprint Mismatch
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'DEVICE_ANOMALY_COMPLEX',
  'Device and Authentication Anomaly',
  'Combines weak authentication indicators with unusual device/channel patterns',
  1,
  'PUBLISHED',
  85,
  80,
  'SUSPEITA_DE_FRAUDE',
  'Anomalia de dispositivo/autenticação: ECI={eciIndicator}, Token Assurance={tokenAssuranceLevel}, Auth Score={consumerAuthenticationScore}',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 6: Cross-Border Velocity Attack
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'CROSS_BORDER_VELOCITY_COMPLEX',
  'Cross-Border Velocity Attack',
  'Detects rapid cross-border transactions that would be physically impossible - indicates card compromise',
  1,
  'PUBLISHED',
  99,
  98,
  'FRAUDE',
  'Velocidade impossível: transações em {country1} e {country2} dentro de {timeGapMinutes} minutos',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 7: High Risk MCC Cascade
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'HIGH_RISK_MCC_CASCADE_COMPLEX',
  'High-Risk MCC Pattern Cascade',
  'Multiple transactions across different high-risk MCCs in short period - indicates compromised card being exploited',
  1,
  'PUBLISHED',
  92,
  88,
  'FRAUDE',
  'Cascata de MCCs de alto risco: {mccList} em {timeWindow}h, total R$ {totalAmount/100}',
  true
) ON CONFLICT (key) DO NOTHING;

-- Complex Rule 8: First Time Cardholder Risk Profile
INSERT INTO complex_rules (key, title, description, version, status, priority, severity, decision, reason_template, enabled)
VALUES (
  'FIRST_TIME_HIGH_RISK_COMPLEX',
  'First-Time Cardholder Risk Profile',
  'New cardholder making high-risk first transaction: combines first-transaction flag with multiple risk indicators',
  1,
  'PUBLISHED',
  80,
  75,
  'SUSPEITA_DE_FRAUDE',
  'Primeira transação de alto risco: R$ {transactionAmount/100} em {merchantName}, perfil não estabelecido',
  true
) ON CONFLICT (key) DO NOTHING;

-- Now create condition groups for the complex rules

-- Condition groups for INTL_GAMBLING_NIGHT_COMPLEX
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'INTL_GAMBLING_NIGHT_COMPLEX')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: AND all conditions', true
FROM rule
ON CONFLICT DO NOTHING;

-- Insert conditions for this group
WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'INTL_GAMBLING_NIGHT_COMPLEX' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_array, value_min, value_max, value_single, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  positions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.varray,
  conditions.vmin,
  conditions.vmax,
  conditions.vsingle,
  true
FROM grp, 
(VALUES
  (0, 'mcc', 'IN', 'ARRAY_NUMBER', ARRAY['7995','7994','7993'], NULL, NULL, NULL),
  (1, 'merchantCountryCode', 'NEQ', 'STRING', NULL, NULL, NULL, '076'),
  (2, 'transactionTime', 'LTE', 'NUMBER', NULL, NULL, NULL, '60000'),
  (3, 'transactionAmount', 'GT', 'NUMBER', NULL, NULL, NULL, '50000')
) AS conditions(pos, field_name, op, vtype, varray, vmin, vmax, vsingle),
(VALUES (0), (1), (2), (3)) AS positions(pos)
WHERE positions.pos = conditions.pos
ON CONFLICT DO NOTHING;

-- Condition groups for BUST_OUT_PATTERN_COMPLEX
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'BUST_OUT_PATTERN_COMPLEX')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Bust-out indicators', true
FROM rule
ON CONFLICT DO NOTHING;

-- Insert conditions for bust-out rule
WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'BUST_OUT_PATTERN_COMPLEX' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  true
FROM grp, 
(VALUES
  (0, 'cardDelinquentAmount', 'GT', 'NUMBER', '10000'),
  (1, 'transactionAmount', 'GT', 'NUMBER', '200000'),
  (2, 'mcc', 'IN', 'STRING', '5732,5734,5944,5311')
) AS conditions(pos, field_name, op, vtype, vsingle)
ON CONFLICT DO NOTHING;

-- Condition groups for CARD_TESTING_SEQUENCE_COMPLEX  
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'CARD_TESTING_SEQUENCE_COMPLEX')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Card testing indicators', true
FROM rule
ON CONFLICT DO NOTHING;

WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'CARD_TESTING_SEQUENCE_COMPLEX' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  true
FROM grp, 
(VALUES
  (0, 'transactionAmount', 'LTE', 'NUMBER', '500'),
  (1, 'customerPresent', 'EQ', 'STRING', '0')
) AS conditions(pos, field_name, op, vtype, vsingle)
ON CONFLICT DO NOTHING;

-- ===================================================================================
-- SUMMARY STATISTICS VIEW
-- ===================================================================================

CREATE OR REPLACE VIEW fraud_rules_summary AS
SELECT 
  'Simple Rules' as rule_type,
  rule_type as category,
  COUNT(*) as rule_count,
  SUM(CASE WHEN enabled THEN 1 ELSE 0 END) as enabled_count,
  AVG(weight) as avg_weight
FROM rule_configurations
GROUP BY rule_type
UNION ALL
SELECT 
  'Complex Rules' as rule_type,
  status as category,
  COUNT(*) as rule_count,
  SUM(CASE WHEN enabled THEN 1 ELSE 0 END) as enabled_count,
  AVG(severity) as avg_weight
FROM complex_rules
GROUP BY status;

-- ===================================================================================
-- AUDIT LOG ENTRY (only if table exists)
-- ===================================================================================
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'access_log') THEN
    INSERT INTO access_log (action, entity_type, entity_id, details, created_at)
    VALUES (
      'SEED_FRAUD_RULES',
      'MIGRATION',
      'V22',
      jsonb_build_object(
        'simple_rules_count', (SELECT COUNT(*) FROM rule_configurations),
        'complex_rules_count', (SELECT COUNT(*) FROM complex_rules),
        'categories', ARRAY['SECURITY', 'CONTEXT', 'VELOCITY', 'ANOMALY'],
        'source', 'V22__fraud_detection_rules_seed.sql'
      ),
      now()
    );
  END IF;
END $$;

-- ===================================================================================
-- COMMENTS
-- ===================================================================================
COMMENT ON TABLE rule_configurations IS 'Simple fraud detection rules with single-level conditions';

-- Print summary
DO $$
DECLARE
  simple_count INT;
  complex_count INT;
BEGIN
  SELECT COUNT(*) INTO simple_count FROM rule_configurations;
  SELECT COUNT(*) INTO complex_count FROM complex_rules;
  RAISE NOTICE 'Fraud Detection Rules Seed Complete:';
  RAISE NOTICE '  - Simple Rules: %', simple_count;
  RAISE NOTICE '  - Complex Rules: %', complex_count;
END $$;
