-- ============================================================================
-- Migration V35: Adicionar índices para operadores de agregação temporal
-- GAP-006: Otimizar queries de agregação temporal
-- Data: 2025-01-15
-- ============================================================================

-- Índice composto para queries de velocity por customer e timestamp
CREATE INDEX IF NOT EXISTS idx_velocity_customer_timestamp 
ON velocity_counters (customer_id, timestamp DESC);

-- Índice para queries de velocity por account e timestamp
CREATE INDEX IF NOT EXISTS idx_velocity_account_timestamp 
ON velocity_counters (account_id, timestamp DESC);

-- Índice para queries de velocity por device e timestamp
CREATE INDEX IF NOT EXISTS idx_velocity_device_timestamp 
ON velocity_counters (device_id, timestamp DESC);

-- Índice para queries de velocity por IP e timestamp
CREATE INDEX IF NOT EXISTS idx_velocity_ip_timestamp 
ON velocity_counters (ip_address, timestamp DESC);

-- Índice para queries de agregação por merchant
CREATE INDEX IF NOT EXISTS idx_velocity_merchant_timestamp 
ON velocity_counters (merchant_id, timestamp DESC);

-- Índice parcial para transações das últimas 24 horas (mais comum)
CREATE INDEX IF NOT EXISTS idx_velocity_recent_24h 
ON velocity_counters (customer_id, timestamp DESC)
WHERE timestamp > NOW() - INTERVAL '24 hours';

-- Índice parcial para transações dos últimos 7 dias
CREATE INDEX IF NOT EXISTS idx_velocity_recent_7d 
ON velocity_counters (customer_id, timestamp DESC)
WHERE timestamp > NOW() - INTERVAL '7 days';

-- Índice para rule_conditions por operator (para análise)
CREATE INDEX IF NOT EXISTS idx_conditions_operator 
ON rule_conditions (operator);

-- Índice para rule_conditions por field_name (para análise)
CREATE INDEX IF NOT EXISTS idx_conditions_field_name 
ON rule_conditions (field_name);

-- Comentário explicativo
COMMENT ON INDEX idx_velocity_customer_timestamp IS 
'Índice para otimizar queries de agregação temporal por customer (SUM_LAST_N_DAYS, COUNT_LAST_N_HOURS, etc.)';

COMMENT ON INDEX idx_velocity_recent_24h IS 
'Índice parcial para queries frequentes das últimas 24 horas - melhora performance significativamente';
