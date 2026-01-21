-- ============================================================================
-- Migration V35: Adicionar índices para operadores de agregação temporal
-- GAP-006: Otimizar queries de agregação temporal
-- Data: 2025-01-15
-- CORRIGIDO: Usar colunas corretas das tabelas velocity_counters e velocity_transaction_log
-- NOTA: Índices parciais com NOW() removidos (PostgreSQL requer funções IMMUTABLE)
-- ============================================================================

-- ============================================================================
-- ÍNDICES PARA velocity_counters (usa key_type/key_value como chaves genéricas)
-- ============================================================================

-- Índice composto para queries de velocity por key_type e window_end
CREATE INDEX IF NOT EXISTS idx_velocity_key_type_window 
ON velocity_counters (key_type, key_value, window_end DESC);

-- Índice para queries por window_type específico
CREATE INDEX IF NOT EXISTS idx_velocity_window_type_end 
ON velocity_counters (window_type, window_end DESC);

-- ============================================================================
-- ÍNDICES PARA velocity_transaction_log (tem colunas específicas)
-- ============================================================================

-- Índice composto para queries de velocity por customer e timestamp
CREATE INDEX IF NOT EXISTS idx_vtl_customer_timestamp 
ON velocity_transaction_log (customer_id, transaction_at DESC)
WHERE customer_id IS NOT NULL;

-- Índice para queries de velocity por merchant e timestamp
CREATE INDEX IF NOT EXISTS idx_vtl_merchant_timestamp 
ON velocity_transaction_log (merchant_id, transaction_at DESC)
WHERE merchant_id IS NOT NULL;

-- Índice para queries por MCC e timestamp
CREATE INDEX IF NOT EXISTS idx_vtl_mcc_timestamp 
ON velocity_transaction_log (mcc, transaction_at DESC)
WHERE mcc IS NOT NULL;

-- Índice para queries por pan_hash e timestamp (agregações por cartão)
CREATE INDEX IF NOT EXISTS idx_vtl_pan_timestamp 
ON velocity_transaction_log (pan_hash, transaction_at DESC);

-- ============================================================================
-- ÍNDICES PARA rule_conditions (análise de operadores)
-- ============================================================================

-- Índice para rule_conditions por operator (para análise)
CREATE INDEX IF NOT EXISTS idx_conditions_operator 
ON rule_conditions (operator);

-- Índice para rule_conditions por field_name (para análise)
CREATE INDEX IF NOT EXISTS idx_conditions_field_name 
ON rule_conditions (field_name);

-- ============================================================================
-- COMENTÁRIOS
-- ============================================================================

COMMENT ON INDEX idx_velocity_key_type_window IS 
'Índice para otimizar queries de agregação por key_type genérico (CUSTOMER_ID, PAN, MERCHANT_ID, etc.)';

COMMENT ON INDEX idx_vtl_customer_timestamp IS 
'Índice para otimizar queries de agregação temporal por customer (SUM_LAST_N_DAYS, COUNT_LAST_N_HOURS, etc.)';
