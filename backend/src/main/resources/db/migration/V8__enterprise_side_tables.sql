-- V8__enterprise_side_tables.sql
-- Enterprise side tables to enable advanced operators (velocity/graph/geo/holidays/features)
-- NOTE: Does NOT alter the inbound payload contract.

-- =========================================
-- PAYLOAD FIELD DICTIONARY (contract catalog)
-- =========================================
CREATE TABLE IF NOT EXISTS payload_field_dictionary (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  path          TEXT NOT NULL UNIQUE,
  data_type     TEXT NOT NULL,
  required      BOOLEAN NOT NULL DEFAULT FALSE,
  example_jsonb JSONB,
  sensitive     BOOLEAN NOT NULL DEFAULT FALSE,
  allowed_ops   TEXT[] NOT NULL DEFAULT ARRAY[]::TEXT[],
  created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_payload_field_dictionary_type
  ON payload_field_dictionary (data_type);

-- =========================================
-- EXECUTIONS (enterprise audit query model)
-- =========================================
CREATE TABLE IF NOT EXISTS executions (
  id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  ts             TIMESTAMPTZ NOT NULL DEFAULT now(),
  entity_keys    JSONB,
  decision       decision_outcome NOT NULL,
  score          INT NOT NULL CHECK (score >= 0 AND score <= 100),
  rule_version   UUID,
  duration_ms    INT,
  created_at     TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_executions_ts
  ON executions (ts);

-- Entity key lookup (GIN for flexible maps)
CREATE INDEX IF NOT EXISTS idx_executions_entity_keys_gin
  ON executions USING GIN (entity_keys);

CREATE TABLE IF NOT EXISTS execution_reasons (
  id            UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  execution_id  UUID NOT NULL REFERENCES executions(id) ON DELETE CASCADE,
  rule_id       UUID,
  operator      TEXT,
  field_path    TEXT,
  reason_code   TEXT,
  severity      INT,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_execution_reasons_execution
  ON execution_reasons (execution_id);

-- =========================================
-- FEATURE STORE (generic)
-- =========================================
CREATE TABLE IF NOT EXISTS feature_store (
  id           UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  entity_key   TEXT NOT NULL,
  feature_name TEXT NOT NULL,
  value_jsonb  JSONB NOT NULL,
  updated_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
  ttl_seconds  INT,
  UNIQUE (entity_key, feature_name)
);

CREATE INDEX IF NOT EXISTS idx_feature_store_entity
  ON feature_store (entity_key);

-- =========================================
-- VELOCITY STORE (bucketed metrics)
-- =========================================
CREATE TABLE IF NOT EXISTS velocity_store (
  id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  entity_key  TEXT NOT NULL,
  metric_name TEXT NOT NULL,
  window_name TEXT NOT NULL,
  value       NUMERIC,
  bucket_ts   TIMESTAMPTZ NOT NULL,
  updated_at  TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_velocity_store_lookup
  ON velocity_store (entity_key, metric_name, window_name, bucket_ts);

-- =========================================
-- GRAPH EDGES (relationship facts)
-- =========================================
CREATE TABLE IF NOT EXISTS graph_edges (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  src_type   TEXT NOT NULL,
  src_id     TEXT NOT NULL,
  dst_type   TEXT NOT NULL,
  dst_id     TEXT NOT NULL,
  first_seen TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_seen  TIMESTAMPTZ NOT NULL DEFAULT now(),
  weight     NUMERIC,
  UNIQUE (src_type, src_id, dst_type, dst_id)
);

CREATE INDEX IF NOT EXISTS idx_graph_edges_src
  ON graph_edges (src_type, src_id);

CREATE INDEX IF NOT EXISTS idx_graph_edges_dst
  ON graph_edges (dst_type, dst_id);

-- =========================================
-- GEO POLYGONS (geofencing)
-- =========================================
CREATE TABLE IF NOT EXISTS geo_polygons (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name       TEXT NOT NULL,
  polygon    JSONB NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_geo_polygons_name
  ON geo_polygons (name);

-- =========================================
-- HOLIDAYS (country/UF calendar)
-- =========================================
CREATE TABLE IF NOT EXISTS holidays (
  id         UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  country    TEXT NOT NULL,
  uf         TEXT,
  date       DATE NOT NULL,
  name       TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (country, uf, date)
);

CREATE INDEX IF NOT EXISTS idx_holidays_lookup
  ON holidays (country, uf, date);
