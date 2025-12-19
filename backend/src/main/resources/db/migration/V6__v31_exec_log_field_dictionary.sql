-- V6__v31_exec_log_field_dictionary.sql
-- V3.1: execution log (append-only) + field_dictionary + refdata_versions (minimal scaffold)

-- =========================================
-- REF DATA VERSIONING (minimal)
-- =========================================
CREATE TABLE IF NOT EXISTS refdata_versions (
  id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key         TEXT NOT NULL UNIQUE,
  notes       TEXT,
  created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- =========================================
-- FIELD DICTIONARY
-- =========================================
CREATE TABLE IF NOT EXISTS field_dictionary (
  id                      UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  workflow                TEXT,
  record_type             TEXT,
  portfolio               TEXT,

  json_path               TEXT NOT NULL,
  data_type               TEXT NOT NULL,

  -- domain_json can represent: {"kind":"enum","values":[...]} | {"kind":"range","min":..,"max":..} | {"kind":"regex","pattern":..}
  domain_json             JSONB,
  sentinel_values_json    JSONB,

  allowed_operators        TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],
  allowed_functions        TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],

  -- requiredness_by_context is an arbitrary JSON map of context flags -> REQUIRED/CONDITIONAL/OPTIONAL
  requiredness_by_context JSONB,
  security_constraints    JSONB,
  normalization_allowed   BOOLEAN NOT NULL DEFAULT FALSE,

  created_at              TIMESTAMPTZ NOT NULL DEFAULT now(),

  UNIQUE (workflow, record_type, portfolio, json_path)
);

CREATE INDEX IF NOT EXISTS idx_field_dictionary_lookup
  ON field_dictionary (workflow, record_type, portfolio);

CREATE INDEX IF NOT EXISTS idx_field_dictionary_json_path
  ON field_dictionary (json_path);

-- =========================================
-- RULE EXECUTION LOG (append-only)
-- =========================================
DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'execution_event_type') THEN
    CREATE TYPE execution_event_type AS ENUM ('EVALUATE', 'SIMULATE', 'ANTI_TAMPER');
  END IF;
END $$;

CREATE TABLE IF NOT EXISTS rule_execution_log (
  id                      UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  event_type              execution_event_type NOT NULL,

  correlation_id          TEXT,
  external_transaction_id VARCHAR(64),

  -- hash of the raw bytes used for evaluation
  payload_raw_hash        VARCHAR(64),

  -- if event_type=ANTI_TAMPER, attempted hash is stored here
  attempted_payload_hash  VARCHAR(64),

  is_tamper               BOOLEAN NOT NULL DEFAULT FALSE,

  ruleset_version_id      UUID,
  refdata_version_id      UUID,

  decision                decision_outcome NOT NULL,
  risk_score              INT NOT NULL CHECK (risk_score >= 0 AND risk_score <= 100),

  rules_fired_json        JSONB NOT NULL DEFAULT '[]'::jsonb,
  decision_path_json      JSONB NOT NULL DEFAULT '{}'::jsonb,
  why_not_fired_json      JSONB,
  context_flags_json      JSONB,
  error_json              JSONB,

  created_at              TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_rule_execution_log_extid
  ON rule_execution_log (external_transaction_id);

CREATE INDEX IF NOT EXISTS idx_rule_execution_log_created_at
  ON rule_execution_log (created_at);

CREATE INDEX IF NOT EXISTS idx_rule_execution_log_payload_hash
  ON rule_execution_log (payload_raw_hash);

-- Exactly 1 "normal" evaluation row per external_transaction_id (idempotency lookup)
CREATE UNIQUE INDEX IF NOT EXISTS uq_rule_execution_log_extid_normal
  ON rule_execution_log (external_transaction_id)
  WHERE external_transaction_id IS NOT NULL AND is_tamper = FALSE;
