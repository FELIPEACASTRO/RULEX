-- V2__core_schema.sql
-- Schema core do fluxo transacional (JPA) + governança por migrations.

-- =========================================
-- CORE: transactions
-- =========================================
CREATE TABLE IF NOT EXISTS transactions (
  id BIGSERIAL PRIMARY KEY,

  external_transaction_id VARCHAR(64) NOT NULL,
  customer_id_from_header VARCHAR(64) NOT NULL,
  customer_acct_number BIGINT NOT NULL,
  pan VARCHAR(64) NOT NULL,

  merchant_id VARCHAR(64),
  merchant_name VARCHAR(255),

  transaction_amount NUMERIC(18,2) NOT NULL,
  transaction_date INTEGER NOT NULL,
  transaction_time INTEGER NOT NULL,
  gmt_offset VARCHAR(10),

  transaction_currency_code INTEGER NOT NULL,
  transaction_currency_conversion_rate NUMERIC(10,4),

  merchant_country_code VARCHAR(10),
  merchant_city VARCHAR(255),
  merchant_state VARCHAR(2),
  merchant_postal_code VARCHAR(20),

  mcc INTEGER NOT NULL,
  pos_entry_mode VARCHAR(1),
  customer_present VARCHAR(1),

  pos_off_premises INTEGER,
  pos_card_capture INTEGER,
  pos_security INTEGER,

  cvv_pin_try_limit_exceeded INTEGER,
  cvroffline_pin_verification_performed INTEGER,
  cvroffline_pin_verification_failed INTEGER,

  card_media_type VARCHAR(1),

  consumer_authentication_score INTEGER NOT NULL,
  external_score3 INTEGER NOT NULL,
  cavv_result INTEGER NOT NULL,

  cryptogram_valid VARCHAR(1),
  cvv2_response VARCHAR(1),
  cvv2_present VARCHAR(1),
  pin_verify_code VARCHAR(1),
  cvv_verify_code VARCHAR(1),

  eci_indicator INTEGER NOT NULL,
  atc_card INTEGER NOT NULL,
  atc_host INTEGER NOT NULL,
  token_assurance_level INTEGER NOT NULL,
  tokenization_indicator VARCHAR(1),

  available_credit NUMERIC(18,2) NOT NULL,
  card_cash_balance NUMERIC(18,2) NOT NULL,
  card_delinquent_amount NUMERIC(18,2) NOT NULL,

  workflow VARCHAR(1),
  record_type VARCHAR(20),
  client_id_from_header VARCHAR(64),

  created_at TIMESTAMP NOT NULL DEFAULT now(),
  updated_at TIMESTAMP NOT NULL DEFAULT now()
);

DO $$
BEGIN
  ALTER TABLE transactions
    ADD CONSTRAINT uq_transactions_external_transaction_id UNIQUE (external_transaction_id);
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

CREATE INDEX IF NOT EXISTS idx_customer_id ON transactions(customer_id_from_header);
CREATE INDEX IF NOT EXISTS idx_merchant_id ON transactions(merchant_id);
CREATE INDEX IF NOT EXISTS idx_transaction_date ON transactions(transaction_date);

-- =========================================
-- CORE: transaction_decisions
-- =========================================
CREATE TABLE IF NOT EXISTS transaction_decisions (
  id BIGSERIAL PRIMARY KEY,
  transaction_id BIGINT NOT NULL,

  classification VARCHAR(20) NOT NULL,
  risk_score INTEGER NOT NULL,

  rules_applied TEXT,
  score_details TEXT,
  reason TEXT,
  rules_version VARCHAR(20),

  created_at TIMESTAMP NOT NULL DEFAULT now()
);

DO $$
BEGIN
  ALTER TABLE transaction_decisions
    ADD CONSTRAINT fk_transaction_decisions_transaction
    FOREIGN KEY (transaction_id) REFERENCES transactions(id) ON DELETE RESTRICT;
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

DO $$
BEGIN
  ALTER TABLE transaction_decisions
    ADD CONSTRAINT chk_transaction_decisions_classification
    CHECK (classification IN ('APPROVED','SUSPICIOUS','FRAUD'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

CREATE INDEX IF NOT EXISTS idx_transaction_id ON transaction_decisions(transaction_id);
CREATE INDEX IF NOT EXISTS idx_classification ON transaction_decisions(classification);
CREATE INDEX IF NOT EXISTS idx_decision_date ON transaction_decisions(created_at);

-- =========================================
-- CORE: rule_configurations
-- =========================================
CREATE TABLE IF NOT EXISTS rule_configurations (
  id BIGSERIAL PRIMARY KEY,

  rule_name VARCHAR(100) NOT NULL,
  description TEXT,

  rule_type VARCHAR(20) NOT NULL,
  threshold INTEGER NOT NULL,
  weight INTEGER NOT NULL,
  enabled BOOLEAN NOT NULL DEFAULT TRUE,
  classification VARCHAR(20) NOT NULL,

  parameters TEXT,
  conditions_json TEXT,
  logic_operator VARCHAR(3) NOT NULL DEFAULT 'AND',

  version INTEGER NOT NULL DEFAULT 1,

  created_at TIMESTAMP NOT NULL DEFAULT now(),
  updated_at TIMESTAMP NOT NULL DEFAULT now()
);

DO $$
BEGIN
  ALTER TABLE rule_configurations
    ADD CONSTRAINT uq_rule_configurations_rule_name UNIQUE (rule_name);
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

DO $$
BEGIN
  ALTER TABLE rule_configurations
    ADD CONSTRAINT chk_rule_configurations_rule_type
    CHECK (rule_type IN ('SECURITY','CONTEXT','VELOCITY','ANOMALY'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

DO $$
BEGIN
  ALTER TABLE rule_configurations
    ADD CONSTRAINT chk_rule_configurations_classification
    CHECK (classification IN ('APPROVED','SUSPICIOUS','FRAUD'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

DO $$
BEGIN
  ALTER TABLE rule_configurations
    ADD CONSTRAINT chk_rule_configurations_logic_operator
    CHECK (logic_operator IN ('AND','OR'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

CREATE INDEX IF NOT EXISTS idx_rule_name ON rule_configurations(rule_name);
CREATE INDEX IF NOT EXISTS idx_enabled ON rule_configurations(enabled);

-- =========================================
-- CORE: rule_configuration_history (append-only)
-- =========================================
CREATE TABLE IF NOT EXISTS rule_configuration_history (
  id BIGSERIAL PRIMARY KEY,

  rule_id BIGINT NOT NULL,
  rule_name VARCHAR(100) NOT NULL,
  version INTEGER NOT NULL,

  previous_json TEXT,
  current_json TEXT,
  performed_by VARCHAR(100),

  created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_rule_hist_rule_id ON rule_configuration_history(rule_id);
CREATE INDEX IF NOT EXISTS idx_rule_hist_created_at ON rule_configuration_history(created_at);

-- =========================================
-- CORE: audit_logs
-- =========================================
CREATE TABLE IF NOT EXISTS audit_logs (
  id BIGSERIAL PRIMARY KEY,

  transaction_id BIGINT,

  action_type VARCHAR(50) NOT NULL,
  description TEXT,
  details TEXT,
  performed_by VARCHAR(100),
  result VARCHAR(20) NOT NULL,
  error_message TEXT,
  source_ip VARCHAR(45),

  created_at TIMESTAMP NOT NULL DEFAULT now()
);

DO $$
BEGIN
  ALTER TABLE audit_logs
    ADD CONSTRAINT chk_audit_logs_action_type
    CHECK (action_type IN (
      'TRANSACTION_PROCESSED',
      'RULE_CREATED',
      'RULE_UPDATED',
      'RULE_DELETED',
      'CONFIG_CHANGED',
      'DECISION_MADE',
      'REPORT_GENERATED'
    ));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

DO $$
BEGIN
  ALTER TABLE audit_logs
    ADD CONSTRAINT chk_audit_logs_result
    CHECK (result IN ('SUCCESS','FAILURE'));
EXCEPTION
  WHEN duplicate_object THEN NULL;
END $$;

CREATE INDEX IF NOT EXISTS idx_audit_logs_transaction_id ON audit_logs(transaction_id);
CREATE INDEX IF NOT EXISTS idx_action_type ON audit_logs(action_type);
CREATE INDEX IF NOT EXISTS idx_audit_date ON audit_logs(created_at);
