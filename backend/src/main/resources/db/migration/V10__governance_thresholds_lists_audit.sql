-- V10__governance_thresholds_lists_audit.sql
-- Governance + audit hardening for RULEX hard-rules engine.
-- Adds versioned thresholds/lists, feature usage audit, and indexes for deterministic feature lookups.
-- NOTE: Does NOT alter inbound payload contract.

-- =========================================
-- THRESHOLDS (versioned, deterministic)
-- =========================================
CREATE TABLE IF NOT EXISTS thresholds_config (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name       TEXT NOT NULL,
  value      NUMERIC NOT NULL,
  version    INT NOT NULL DEFAULT 1,
  active     BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (name, version)
);

CREATE INDEX IF NOT EXISTS idx_thresholds_config_active
  ON thresholds_config (name) WHERE active = TRUE;

-- =========================================
-- LISTS (versioned, deterministic)
-- =========================================
CREATE TABLE IF NOT EXISTS lists_config (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name       TEXT NOT NULL,
  values_jsonb JSONB NOT NULL,
  version    INT NOT NULL DEFAULT 1,
  active     BOOLEAN NOT NULL DEFAULT TRUE,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (name, version)
);

CREATE INDEX IF NOT EXISTS idx_lists_config_active
  ON lists_config (name) WHERE active = TRUE;

CREATE INDEX IF NOT EXISTS idx_lists_config_values_gin
  ON lists_config USING GIN (values_jsonb);

-- =========================================
-- AUDIT: capture feature usage (masked/hash)
-- =========================================
ALTER TABLE executions
  ADD COLUMN IF NOT EXISTS payload_raw_hash VARCHAR(64);

ALTER TABLE execution_reasons
  ADD COLUMN IF NOT EXISTS feature_name TEXT,
  ADD COLUMN IF NOT EXISTS feature_version INT,
  ADD COLUMN IF NOT EXISTS window_name TEXT,
  ADD COLUMN IF NOT EXISTS value_hash VARCHAR(64),
  ADD COLUMN IF NOT EXISTS value_masked TEXT;

-- V3.1 audit log: add features_used_json
ALTER TABLE rule_execution_log
  ADD COLUMN IF NOT EXISTS features_used_json JSONB NOT NULL DEFAULT '[]'::jsonb;

CREATE INDEX IF NOT EXISTS idx_rule_execution_log_features_used_gin
  ON rule_execution_log USING GIN (features_used_json);

-- =========================================
-- INDEXES: speed up feature lookups
-- =========================================
CREATE INDEX IF NOT EXISTS idx_feature_store_feature_entity
  ON feature_store (feature_name, entity_key);

-- velocity_store already has (entity_key, metric_name, window_name, bucket_ts)
-- graph_edges already has src/dst indexes

-- =========================================
-- RETENTION HELPERS (manual invocation)
-- =========================================
CREATE OR REPLACE FUNCTION clean_old_graph_edges(retention_days INT DEFAULT 30)
RETURNS INT AS $$
DECLARE
  deleted_count INT;
BEGIN
  DELETE FROM graph_edges
  WHERE last_seen < now() - (retention_days || ' days')::INTERVAL;
  GET DIAGNOSTICS deleted_count = ROW_COUNT;
  RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;
