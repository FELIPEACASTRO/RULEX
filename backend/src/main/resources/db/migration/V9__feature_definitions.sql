-- V9__feature_definitions.sql
-- Feature Definitions catalog: metadados oficiais das features disponíveis para regras duras.
-- Todas as features são DETERMINÍSTICAS (mesma entrada → mesma saída).

-- =========================================
-- FEATURE DEFINITIONS (catalog metadata)
-- =========================================
CREATE TABLE IF NOT EXISTS feature_definitions (
  id             UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  feature_name   TEXT NOT NULL UNIQUE,
  feature_type   TEXT NOT NULL CHECK (feature_type IN (
    'PAYLOAD_FIELD',      -- campo direto do payload
    'TEMPORAL',           -- janelas temporais, bucket, sazonalidade
    'VELOCITY',           -- count/sum/rate por janela
    'GRAPH',              -- relacionamentos entre entidades
    'GEO',                -- geolocalização, distância, polygons
    'TEXT',               -- normalização, similaridade, entropy
    'SCHEMA',             -- validação de schema, campos faltando
    'DERIVED',            -- cálculo determinístico em runtime
    'CONTEXTUAL'          -- contexto externo (holidays, refdata)
  )),
  entity_type    TEXT,  -- card, customer, device, ip, merchant, etc.
  window_name    TEXT,  -- 5m, 1h, 24h, 7d, 30d, etc.
  formula        TEXT,  -- fórmula determinística (ex.: COUNT(*) WHERE entity=? AND ts >= now()-window)
  description    TEXT,
  source         TEXT NOT NULL CHECK (source IN (
    'payload',            -- lido diretamente do payload
    'feature_store',      -- pré-calculado e persistido
    'velocity_store',     -- agregação por bucket
    'runtime'             -- calculado em tempo de avaliação
  )),
  data_type      TEXT NOT NULL CHECK (data_type IN (
    'number', 'integer', 'boolean', 'string', 'date', 'timestamp', 'object', 'array'
  )),
  allowed_operators TEXT[] NOT NULL DEFAULT ARRAY['EQ', 'NE', 'GT', 'GTE', 'LT', 'LTE']::TEXT[],
  refresh_strategy TEXT CHECK (refresh_strategy IN (
    'REALTIME',           -- calculado a cada request
    'BATCH_5M',           -- atualizado a cada 5 minutos
    'BATCH_1H',           -- atualizado a cada hora
    'BATCH_1D',           -- atualizado diariamente
    'ON_DEMAND'           -- sob demanda
  )),
  version        INT NOT NULL DEFAULT 1,
  active         BOOLEAN NOT NULL DEFAULT TRUE,
  created_at     TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at     TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS idx_feature_definitions_type
  ON feature_definitions (feature_type);

CREATE INDEX IF NOT EXISTS idx_feature_definitions_entity
  ON feature_definitions (entity_type);

CREATE INDEX IF NOT EXISTS idx_feature_definitions_active
  ON feature_definitions (active) WHERE active = TRUE;

-- =========================================
-- SEED: Payload field features (from field_dictionary)
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, source, data_type, description, allowed_operators)
VALUES
  ('transactionAmount', 'PAYLOAD_FIELD', NULL, 'payload', 'number', 'Valor da transação', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[]),
  ('transactionDate', 'PAYLOAD_FIELD', NULL, 'payload', 'date', 'Data da transação (YYYYMMDD)', ARRAY['EQ','NE','BEFORE','AFTER','ON','BETWEEN']::TEXT[]),
  ('transactionTime', 'PAYLOAD_FIELD', NULL, 'payload', 'string', 'Hora da transação (HHMMSS)', ARRAY['EQ','NE','GT','GTE','LT','LTE','IN_TIME_RANGE']::TEXT[]),
  ('mcc', 'PAYLOAD_FIELD', 'merchant', 'payload', 'string', 'Merchant Category Code', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('merchantCountryCode', 'PAYLOAD_FIELD', 'merchant', 'payload', 'string', 'País do merchant', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('merchantId', 'PAYLOAD_FIELD', 'merchant', 'payload', 'string', 'ID do merchant', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('merchantName', 'PAYLOAD_FIELD', 'merchant', 'payload', 'string', 'Nome do merchant', ARRAY['EQ','NE','CONTAINS','STARTS_WITH','ENDS_WITH','MATCHES_REGEX','SIMILARITY_JARO']::TEXT[]),
  ('pan', 'PAYLOAD_FIELD', 'card', 'payload', 'string', 'Primary Account Number (PAN)', ARRAY['EQ','NE','STARTS_WITH','ENDS_WITH']::TEXT[]),
  ('customerIdFromHeader', 'PAYLOAD_FIELD', 'customer', 'payload', 'string', 'ID do cliente', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('acquirerId', 'PAYLOAD_FIELD', 'acquirer', 'payload', 'string', 'ID do adquirente', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('acquirerCountryCode', 'PAYLOAD_FIELD', 'acquirer', 'payload', 'string', 'País do adquirente', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('eciIndicator', 'PAYLOAD_FIELD', NULL, 'payload', 'integer', 'ECI Indicator (e-commerce)', ARRAY['EQ','NE','IN','NOT_IN','GT','GTE','LT','LTE']::TEXT[]),
  ('cvv2Response', 'PAYLOAD_FIELD', NULL, 'payload', 'string', 'Resposta CVV2', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('responseCode', 'PAYLOAD_FIELD', NULL, 'payload', 'string', 'Código de resposta', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('posEntryMode', 'PAYLOAD_FIELD', NULL, 'payload', 'string', 'Modo de entrada POS', ARRAY['EQ','NE','IN','NOT_IN']::TEXT[]),
  ('recurringTransaction', 'PAYLOAD_FIELD', NULL, 'payload', 'boolean', 'Transação recorrente', ARRAY['IS_TRUE','IS_FALSE','EQ']::TEXT[]),
  ('cardCaptured', 'PAYLOAD_FIELD', NULL, 'payload', 'boolean', 'Cartão capturado', ARRAY['IS_TRUE','IS_FALSE','EQ']::TEXT[]),
  ('cryptogramValid', 'PAYLOAD_FIELD', NULL, 'payload', 'boolean', 'Criptograma válido', ARRAY['IS_TRUE','IS_FALSE','EQ']::TEXT[]),
  ('consumerAuthenticationScore', 'PAYLOAD_FIELD', NULL, 'payload', 'integer', 'Score de autenticação', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[]),
  ('externalScore3', 'PAYLOAD_FIELD', NULL, 'payload', 'integer', 'Score externo 3', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[])
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- SEED: Temporal features
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, window_name, source, data_type, formula, description, allowed_operators, refresh_strategy)
VALUES
  ('is_weekend', 'TEMPORAL', NULL, NULL, 'runtime', 'boolean', 'IS_WEEKEND(transactionDate)', 'Transação em fim de semana', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('is_holiday_br', 'TEMPORAL', NULL, NULL, 'runtime', 'boolean', 'IS_HOLIDAY("BR", transactionDate)', 'Transação em feriado nacional BR', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('is_business_day_br', 'TEMPORAL', NULL, NULL, 'runtime', 'boolean', 'IS_BUSINESS_DAY("BR", transactionDate)', 'Dia útil no Brasil', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('time_since_last_txn', 'TEMPORAL', 'card', NULL, 'feature_store', 'number', 'TIME_SINCE_LAST("TXN", pan)', 'Segundos desde última transação do cartão', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'REALTIME'),
  ('same_day_txn', 'TEMPORAL', 'card', '1d', 'runtime', 'boolean', 'SAME_DAY(transactionDate, lastTxnDate)', 'Transação no mesmo dia da anterior', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('in_high_risk_hours', 'TEMPORAL', NULL, NULL, 'runtime', 'boolean', 'IN_TIME_RANGE(transactionTime, 000000, 060000)', 'Transação entre 00h-06h', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME')
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- SEED: Velocity/Counter features
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, window_name, source, data_type, formula, description, allowed_operators, refresh_strategy)
VALUES
  ('txn_count_1h', 'VELOCITY', 'card', '1h', 'velocity_store', 'integer', 'COUNT(*) WHERE pan=? AND ts >= now()-1h', 'Quantidade de transações do cartão na última hora', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('txn_count_24h', 'VELOCITY', 'card', '24h', 'velocity_store', 'integer', 'COUNT(*) WHERE pan=? AND ts >= now()-24h', 'Quantidade de transações do cartão nas últimas 24h', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('txn_count_7d', 'VELOCITY', 'card', '7d', 'velocity_store', 'integer', 'COUNT(*) WHERE pan=? AND ts >= now()-7d', 'Quantidade de transações do cartão nos últimos 7 dias', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_1H'),
  ('txn_sum_1h', 'VELOCITY', 'card', '1h', 'velocity_store', 'number', 'SUM(amount) WHERE pan=? AND ts >= now()-1h', 'Soma de valores do cartão na última hora', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('txn_sum_24h', 'VELOCITY', 'card', '24h', 'velocity_store', 'number', 'SUM(amount) WHERE pan=? AND ts >= now()-24h', 'Soma de valores do cartão nas últimas 24h', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('unique_merchants_24h', 'VELOCITY', 'card', '24h', 'velocity_store', 'integer', 'COUNT(DISTINCT merchantId) WHERE pan=? AND ts >= now()-24h', 'Merchants únicos do cartão nas últimas 24h', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('unique_countries_24h', 'VELOCITY', 'card', '24h', 'velocity_store', 'integer', 'COUNT(DISTINCT merchantCountryCode) WHERE pan=? AND ts >= now()-24h', 'Países únicos do cartão nas últimas 24h', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('decline_count_1h', 'VELOCITY', 'card', '1h', 'velocity_store', 'integer', 'COUNT(*) WHERE pan=? AND responseCode NOT IN ("00","10") AND ts >= now()-1h', 'Declines do cartão na última hora', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('decline_rate_1h', 'VELOCITY', 'card', '1h', 'velocity_store', 'number', 'decline_count_1h / txn_count_1h', 'Taxa de decline do cartão na última hora (0-1)', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_5M'),
  ('avg_txn_amount_7d', 'VELOCITY', 'card', '7d', 'velocity_store', 'number', 'AVG(amount) WHERE pan=? AND ts >= now()-7d', 'Valor médio de transação do cartão nos últimos 7 dias', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_1H'),
  ('txn_amount_spike_vs_7d', 'VELOCITY', 'card', '7d', 'runtime', 'number', 'transactionAmount / avg_txn_amount_7d', 'Razão entre valor atual e média 7d (spike)', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'REALTIME')
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- SEED: Graph/Relational features
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, window_name, source, data_type, formula, description, allowed_operators, refresh_strategy)
VALUES
  ('is_new_merchant', 'GRAPH', 'card', '30d', 'runtime', 'boolean', 'IS_NEW_LINK(pan, merchantId, 30d)', 'Primeira transação do cartão neste merchant (30d)', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('card_merchant_count_30d', 'GRAPH', 'card', '30d', 'velocity_store', 'integer', 'DEGREE(pan, "merchant", 30d)', 'Quantidade de merchants distintos do cartão (30d)', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_1H'),
  ('merchant_card_count_30d', 'GRAPH', 'merchant', '30d', 'velocity_store', 'integer', 'DEGREE(merchantId, "card", 30d)', 'Quantidade de cartões distintos no merchant (30d)', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'BATCH_1H'),
  ('shared_merchants_with_fraud', 'GRAPH', 'card', '7d', 'velocity_store', 'integer', 'SHARED_NEIGHBOR_COUNT(pan, fraudPanSet, "merchant", 7d)', 'Merchants em comum com cartões fraudulentos (7d)', ARRAY['EQ','NE','GT','GTE','LT','LTE']::TEXT[], 'BATCH_1H')
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- SEED: Geo features
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, window_name, source, data_type, formula, description, allowed_operators, refresh_strategy)
VALUES
  ('geo_distance_last_txn_km', 'GEO', 'card', NULL, 'runtime', 'number', 'GEO_DISTANCE_KM(lastTxnLat, lastTxnLon, currentLat, currentLon)', 'Distância em km da última transação do cartão', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'REALTIME'),
  ('impossible_travel_flag', 'GEO', 'card', NULL, 'runtime', 'boolean', 'IMPOSSIBLE_TRAVEL(pan, 900)', 'Viagem impossível (>900 km/h)', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('is_high_risk_country', 'GEO', NULL, NULL, 'runtime', 'boolean', 'IN(merchantCountryCode, highRiskCountries)', 'País de alto risco', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('cross_border_txn', 'GEO', NULL, NULL, 'runtime', 'boolean', 'NE(merchantCountryCode, acquirerCountryCode)', 'Transação cross-border', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME')
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- SEED: Text features
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, window_name, source, data_type, formula, description, allowed_operators, refresh_strategy)
VALUES
  ('merchant_name_entropy', 'TEXT', 'merchant', NULL, 'runtime', 'number', 'ENTROPY(merchantName)', 'Entropia do nome do merchant', ARRAY['EQ','NE','GT','GTE','LT','LTE','BETWEEN']::TEXT[], 'REALTIME'),
  ('merchant_name_token_count', 'TEXT', 'merchant', NULL, 'runtime', 'integer', 'TOKEN_COUNT(merchantName)', 'Quantidade de tokens no nome do merchant', ARRAY['EQ','NE','GT','GTE','LT','LTE']::TEXT[], 'REALTIME'),
  ('merchant_name_blacklist_match', 'TEXT', 'merchant', NULL, 'runtime', 'boolean', 'KEYWORD_BLACKLIST(merchantName, "gambling,casino,porn")', 'Match com blacklist de palavras', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME')
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- SEED: Schema/Robustness features
-- =========================================
INSERT INTO feature_definitions (feature_name, feature_type, entity_type, window_name, source, data_type, formula, description, allowed_operators, refresh_strategy)
VALUES
  ('has_pan', 'SCHEMA', NULL, NULL, 'runtime', 'boolean', 'HAS_FIELD("$.pan")', 'Campo PAN presente no payload', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('has_merchant_id', 'SCHEMA', NULL, NULL, 'runtime', 'boolean', 'HAS_FIELD("$.merchantId")', 'Campo merchantId presente', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('unknown_fields_present', 'SCHEMA', NULL, NULL, 'runtime', 'boolean', 'UNKNOWN_FIELDS_PRESENT()', 'Campos desconhecidos no payload', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME'),
  ('pan_format_valid', 'SCHEMA', NULL, NULL, 'runtime', 'boolean', 'MATCHES_REGEX(pan, "^[0-9]{13,19}$")', 'PAN com formato válido (13-19 dígitos)', ARRAY['IS_TRUE','IS_FALSE']::TEXT[], 'REALTIME')
ON CONFLICT (feature_name) DO NOTHING;

-- =========================================
-- Add audit columns to velocity_store for traceability
-- =========================================
ALTER TABLE velocity_store ADD COLUMN IF NOT EXISTS version INT NOT NULL DEFAULT 1;
ALTER TABLE velocity_store ADD COLUMN IF NOT EXISTS source TEXT;

-- =========================================
-- Partitioning hint: velocity_store by bucket_ts (future optimization)
-- For now, add BRIN index for time-series queries
-- =========================================
CREATE INDEX IF NOT EXISTS idx_velocity_store_bucket_ts_brin
  ON velocity_store USING BRIN (bucket_ts);

-- =========================================
-- Retention policy helper: function to clean old velocity data
-- =========================================
CREATE OR REPLACE FUNCTION clean_old_velocity_data(retention_days INT DEFAULT 30)
RETURNS INT AS $$
DECLARE
  deleted_count INT;
BEGIN
  DELETE FROM velocity_store
  WHERE bucket_ts < now() - (retention_days || ' days')::INTERVAL;
  GET DIAGNOSTICS deleted_count = ROW_COUNT;
  RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;

-- =========================================
-- Retention policy helper: function to clean old feature_store data
-- =========================================
CREATE OR REPLACE FUNCTION clean_expired_features()
RETURNS INT AS $$
DECLARE
  deleted_count INT;
BEGIN
  DELETE FROM feature_store
  WHERE ttl_seconds IS NOT NULL
    AND updated_at + (ttl_seconds || ' seconds')::INTERVAL < now();
  GET DIAGNOSTICS deleted_count = ROW_COUNT;
  RETURN deleted_count;
END;
$$ LANGUAGE plpgsql;
