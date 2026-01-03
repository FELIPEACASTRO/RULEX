-- V20__shadow_mode_and_device_fingerprinting.sql
-- Migration for Shadow Mode support and Device Fingerprinting

-- =============================================================================
-- SHADOW MODE SUPPORT
-- =============================================================================

-- Add shadow_mode column to rules table
ALTER TABLE rules ADD COLUMN IF NOT EXISTS shadow_mode VARCHAR(20) DEFAULT 'DISABLED';
ALTER TABLE rules ADD COLUMN IF NOT EXISTS canary_percentage INTEGER DEFAULT 0;

-- Add comment explaining shadow modes
COMMENT ON COLUMN rules.shadow_mode IS 'Shadow mode: DISABLED (active rule), SHADOW (evaluate but dont act), CANARY (percentage rollout)';
COMMENT ON COLUMN rules.canary_percentage IS 'Percentage of traffic to evaluate when in CANARY mode (0-100)';

-- Create index for shadow rules lookup
CREATE INDEX IF NOT EXISTS idx_rules_shadow_mode ON rules(shadow_mode) WHERE shadow_mode != 'DISABLED';

-- Shadow rule evaluation log (separate from main execution log for performance)
CREATE TABLE IF NOT EXISTS shadow_evaluation_log (
    id BIGSERIAL PRIMARY KEY,
    rule_id BIGINT NOT NULL REFERENCES rules(id) ON DELETE CASCADE,
    transaction_id BIGINT,
    pan_hash VARCHAR(64),
    
    -- Evaluation result
    triggered BOOLEAN NOT NULL DEFAULT FALSE,
    score INTEGER DEFAULT 0,
    recommended_action VARCHAR(50),
    
    -- What actually happened (for comparison)
    actual_decision VARCHAR(50),
    actual_score INTEGER,
    
    -- Performance
    latency_micros BIGINT,
    
    -- Metadata
    evaluated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB
);

-- Indexes for shadow evaluation queries
CREATE INDEX IF NOT EXISTS idx_shadow_eval_rule_id ON shadow_evaluation_log(rule_id);
CREATE INDEX IF NOT EXISTS idx_shadow_eval_triggered ON shadow_evaluation_log(rule_id, triggered);
CREATE INDEX IF NOT EXISTS idx_shadow_eval_date ON shadow_evaluation_log(evaluated_at DESC);

-- Partitioning hint (apply manually for large deployments)
-- PARTITION BY RANGE (evaluated_at);

-- =============================================================================
-- DEVICE FINGERPRINTING
-- =============================================================================

-- Device fingerprints table
CREATE TABLE IF NOT EXISTS device_fingerprints (
    id BIGSERIAL PRIMARY KEY,
    fingerprint_hash VARCHAR(64) NOT NULL UNIQUE,
    
    -- Core fingerprint components
    user_agent_family VARCHAR(100),
    user_agent_version VARCHAR(50),
    platform VARCHAR(50),
    screen_resolution VARCHAR(20),
    color_depth INTEGER,
    device_memory INTEGER,
    hardware_concurrency INTEGER,
    language VARCHAR(20),
    timezone VARCHAR(100),
    
    -- Canvas/WebGL hashes (unique per device)
    canvas_hash VARCHAR(64),
    webgl_hash VARCHAR(64),
    audio_hash VARCHAR(64),
    
    -- Additional signals
    plugins_hash VARCHAR(64),
    fonts_hash VARCHAR(64),
    
    -- Risk indicators
    is_tor BOOLEAN DEFAULT FALSE,
    is_vpn BOOLEAN DEFAULT FALSE,
    is_datacenter BOOLEAN DEFAULT FALSE,
    is_emulator BOOLEAN DEFAULT FALSE,
    
    -- Statistics
    first_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    transaction_count BIGINT DEFAULT 0,
    
    -- Metadata
    raw_data JSONB
);

-- Device to PAN associations
CREATE TABLE IF NOT EXISTS device_pan_associations (
    id BIGSERIAL PRIMARY KEY,
    fingerprint_hash VARCHAR(64) NOT NULL,
    pan_hash VARCHAR(64) NOT NULL,
    
    first_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    transaction_count BIGINT DEFAULT 1,
    
    -- Risk flags
    is_primary_device BOOLEAN DEFAULT FALSE,
    risk_level VARCHAR(20) DEFAULT 'LOW',
    
    UNIQUE(fingerprint_hash, pan_hash)
);

-- Indexes for device lookups
CREATE INDEX IF NOT EXISTS idx_device_fp_hash ON device_fingerprints(fingerprint_hash);
CREATE INDEX IF NOT EXISTS idx_device_pan_fp ON device_pan_associations(fingerprint_hash);
CREATE INDEX IF NOT EXISTS idx_device_pan_pan ON device_pan_associations(pan_hash);
CREATE INDEX IF NOT EXISTS idx_device_risk ON device_pan_associations(risk_level) WHERE risk_level != 'LOW';

-- Device farming detection view
CREATE OR REPLACE VIEW v_device_farming_suspects AS
SELECT 
    dpa.fingerprint_hash,
    COUNT(DISTINCT dpa.pan_hash) as card_count,
    MIN(dpa.first_seen) as first_seen,
    MAX(dpa.last_seen) as last_seen,
    SUM(dpa.transaction_count) as total_transactions,
    df.platform,
    df.user_agent_family
FROM device_pan_associations dpa
JOIN device_fingerprints df ON df.fingerprint_hash = dpa.fingerprint_hash
GROUP BY dpa.fingerprint_hash, df.platform, df.user_agent_family
HAVING COUNT(DISTINCT dpa.pan_hash) >= 5;

-- =============================================================================
-- IMPOSSIBLE TRAVEL TRACKING
-- =============================================================================

-- Location history for impossible travel detection
CREATE TABLE IF NOT EXISTS pan_location_history (
    id BIGSERIAL PRIMARY KEY,
    pan_hash VARCHAR(64) NOT NULL,
    
    -- Location data
    latitude DOUBLE PRECISION NOT NULL,
    longitude DOUBLE PRECISION NOT NULL,
    city VARCHAR(100),
    country VARCHAR(3),
    
    -- Transaction info
    transaction_id BIGINT,
    transaction_time TIMESTAMP WITH TIME ZONE NOT NULL,
    is_card_present BOOLEAN DEFAULT TRUE,
    
    -- Travel analysis (from previous location)
    distance_km DOUBLE PRECISION,
    elapsed_minutes DOUBLE PRECISION,
    speed_kmh DOUBLE PRECISION,
    travel_risk VARCHAR(20),
    
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for location queries
CREATE INDEX IF NOT EXISTS idx_location_pan ON pan_location_history(pan_hash);
CREATE INDEX IF NOT EXISTS idx_location_time ON pan_location_history(pan_hash, transaction_time DESC);
CREATE INDEX IF NOT EXISTS idx_location_risk ON pan_location_history(travel_risk) WHERE travel_risk IN ('HIGH', 'IMPOSSIBLE');

-- =============================================================================
-- BLOOM FILTER METADATA (for tracking rebuilds)
-- =============================================================================

CREATE TABLE IF NOT EXISTS bloom_filter_metadata (
    id BIGSERIAL PRIMARY KEY,
    filter_type VARCHAR(50) NOT NULL,  -- 'BLACKLIST', 'WHITELIST', 'GREYLIST'
    entity_type VARCHAR(50) NOT NULL,  -- 'PAN', 'MERCHANT_ID', etc.
    
    -- Filter statistics
    element_count BIGINT DEFAULT 0,
    bit_count BIGINT DEFAULT 0,
    hash_functions INTEGER DEFAULT 7,
    false_positive_rate DOUBLE PRECISION DEFAULT 0.01,
    
    -- Rebuild tracking
    last_rebuild TIMESTAMP WITH TIME ZONE,
    rebuild_duration_ms BIGINT,
    next_scheduled_rebuild TIMESTAMP WITH TIME ZONE,
    
    -- Performance metrics
    total_lookups BIGINT DEFAULT 0,
    bloom_hits BIGINT DEFAULT 0,  -- Definite negatives (no DB query)
    bloom_misses BIGINT DEFAULT 0,  -- Possible positives (DB query needed)
    confirmed_positives BIGINT DEFAULT 0,  -- DB confirmed in list
    false_positives BIGINT DEFAULT 0,  -- Bloom said maybe, DB said no
    
    UNIQUE(filter_type, entity_type)
);

-- =============================================================================
-- VELOCITY SERVICE METRICS
-- =============================================================================

-- Velocity check statistics (for monitoring)
CREATE TABLE IF NOT EXISTS velocity_metrics (
    id BIGSERIAL PRIMARY KEY,
    metric_date DATE NOT NULL DEFAULT CURRENT_DATE,
    metric_hour INTEGER NOT NULL DEFAULT EXTRACT(HOUR FROM CURRENT_TIMESTAMP),
    
    -- Counters
    total_checks BIGINT DEFAULT 0,
    cache_hits BIGINT DEFAULT 0,
    cache_misses BIGINT DEFAULT 0,
    db_queries BIGINT DEFAULT 0,
    
    -- Performance
    avg_latency_micros DOUBLE PRECISION,
    p95_latency_micros DOUBLE PRECISION,
    p99_latency_micros DOUBLE PRECISION,
    
    -- Alerts triggered
    threshold_breaches BIGINT DEFAULT 0,
    
    UNIQUE(metric_date, metric_hour)
);

CREATE INDEX IF NOT EXISTS idx_velocity_metrics_date ON velocity_metrics(metric_date DESC);

-- =============================================================================
-- RULE A/B TESTING SUPPORT
-- =============================================================================

-- A/B test configurations
CREATE TABLE IF NOT EXISTS rule_ab_tests (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    description TEXT,
    
    -- Control group (existing rule)
    control_rule_id BIGINT NOT NULL REFERENCES rules(id),
    
    -- Treatment group (new rule to test)
    treatment_rule_id BIGINT NOT NULL REFERENCES rules(id),
    
    -- Traffic split
    treatment_percentage INTEGER NOT NULL DEFAULT 50,
    
    -- Targeting (optional)
    target_segment JSONB,  -- e.g., {"mcc": ["5411"], "country": ["BR"]}
    
    -- Status
    status VARCHAR(20) DEFAULT 'DRAFT',  -- DRAFT, RUNNING, PAUSED, COMPLETED
    started_at TIMESTAMP WITH TIME ZONE,
    ended_at TIMESTAMP WITH TIME ZONE,
    
    -- Results
    control_triggers BIGINT DEFAULT 0,
    treatment_triggers BIGINT DEFAULT 0,
    control_false_positives BIGINT DEFAULT 0,
    treatment_false_positives BIGINT DEFAULT 0,
    
    created_by VARCHAR(100),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- A/B test assignment log
CREATE TABLE IF NOT EXISTS rule_ab_test_assignments (
    id BIGSERIAL PRIMARY KEY,
    test_id BIGINT NOT NULL REFERENCES rule_ab_tests(id),
    transaction_id BIGINT NOT NULL,
    pan_hash VARCHAR(64) NOT NULL,
    
    assigned_group VARCHAR(20) NOT NULL,  -- 'CONTROL' or 'TREATMENT'
    evaluated_rule_id BIGINT NOT NULL,
    
    -- Result
    triggered BOOLEAN DEFAULT FALSE,
    score INTEGER DEFAULT 0,
    action VARCHAR(50),
    
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX IF NOT EXISTS idx_ab_test_id ON rule_ab_test_assignments(test_id);
CREATE INDEX IF NOT EXISTS idx_ab_test_pan ON rule_ab_test_assignments(test_id, pan_hash);

-- =============================================================================
-- CLEANUP POLICIES (for data retention)
-- =============================================================================

-- Comment on tables for retention guidance
COMMENT ON TABLE shadow_evaluation_log IS 'Retain for 30 days, partition by evaluated_at monthly';
COMMENT ON TABLE pan_location_history IS 'Retain for 72 hours, cleanup via scheduled job';
COMMENT ON TABLE velocity_metrics IS 'Retain for 90 days, aggregate older data';
COMMENT ON TABLE rule_ab_test_assignments IS 'Retain while test is active + 30 days after completion';

-- =============================================================================
-- GRANTS
-- =============================================================================

-- Grant permissions (adjust role name as needed)
-- GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO rulex_app;
-- GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO rulex_app;
