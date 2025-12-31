-- V14: Sistema de contadores de velocidade para detecção de fraude
-- Permite agregações temporais sem alterar o payload de entrada

-- Tabela de contadores de velocidade (agregações pré-computadas)
CREATE TABLE IF NOT EXISTS velocity_counters (
    id BIGSERIAL PRIMARY KEY,
    
    -- Chave de agregação (pode ser PAN, customerId, merchantId, etc.)
    key_type VARCHAR(50) NOT NULL,        -- PAN, CUSTOMER_ID, MERCHANT_ID, PAN_MCC, etc.
    key_value VARCHAR(255) NOT NULL,      -- Valor da chave (mascarado se PAN)
    
    -- Janela temporal
    window_type VARCHAR(20) NOT NULL,     -- MINUTE_5, HOUR_1, HOUR_24, DAY_7, DAY_30
    window_start TIMESTAMP WITH TIME ZONE NOT NULL,
    window_end TIMESTAMP WITH TIME ZONE NOT NULL,
    
    -- Contadores
    transaction_count INTEGER DEFAULT 0,
    total_amount NUMERIC(18,2) DEFAULT 0,
    avg_amount NUMERIC(18,2) DEFAULT 0,
    min_amount NUMERIC(18,2),
    max_amount NUMERIC(18,2),
    
    -- Contadores de decisão
    approved_count INTEGER DEFAULT 0,
    suspicious_count INTEGER DEFAULT 0,
    fraud_count INTEGER DEFAULT 0,
    
    -- Contadores de características
    distinct_merchants INTEGER DEFAULT 0,
    distinct_mccs INTEGER DEFAULT 0,
    distinct_countries INTEGER DEFAULT 0,
    
    -- Metadados
    last_transaction_id VARCHAR(64),
    last_transaction_at TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT uk_velocity_counter UNIQUE (key_type, key_value, window_type, window_start)
);

-- Índices para busca eficiente
CREATE INDEX IF NOT EXISTS idx_velocity_key ON velocity_counters(key_type, key_value);
CREATE INDEX IF NOT EXISTS idx_velocity_window ON velocity_counters(window_type, window_start, window_end);
CREATE INDEX IF NOT EXISTS idx_velocity_key_window ON velocity_counters(key_type, key_value, window_type, window_end DESC);

-- Tabela de histórico de transações para agregações ad-hoc
-- (complementa a tabela transactions existente com campos otimizados para velocity)
CREATE TABLE IF NOT EXISTS velocity_transaction_log (
    id BIGSERIAL PRIMARY KEY,
    
    external_transaction_id VARCHAR(64) NOT NULL,
    
    -- Chaves de agregação (normalizadas)
    pan_hash VARCHAR(64) NOT NULL,        -- SHA-256 do PAN (para privacidade)
    customer_id VARCHAR(64),
    merchant_id VARCHAR(64),
    
    -- Dados da transação
    amount NUMERIC(18,2) NOT NULL,
    currency_code INTEGER,
    mcc INTEGER,
    merchant_country VARCHAR(10),
    
    -- Resultado
    decision VARCHAR(20),                  -- APPROVED, SUSPICIOUS, FRAUD
    risk_score INTEGER,
    
    -- Timestamp
    transaction_at TIMESTAMP WITH TIME ZONE NOT NULL,
    
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT uk_velocity_log_ext_id UNIQUE (external_transaction_id)
);

-- Índices para agregações eficientes
CREATE INDEX IF NOT EXISTS idx_velocity_log_pan ON velocity_transaction_log(pan_hash, transaction_at DESC);
CREATE INDEX IF NOT EXISTS idx_velocity_log_customer ON velocity_transaction_log(customer_id, transaction_at DESC);
CREATE INDEX IF NOT EXISTS idx_velocity_log_merchant ON velocity_transaction_log(merchant_id, transaction_at DESC);
CREATE INDEX IF NOT EXISTS idx_velocity_log_time ON velocity_transaction_log(transaction_at DESC);

-- Função para calcular agregações em tempo real
CREATE OR REPLACE FUNCTION get_velocity_stats(
    p_key_type VARCHAR,
    p_key_value VARCHAR,
    p_window_minutes INTEGER
) RETURNS TABLE (
    transaction_count BIGINT,
    total_amount NUMERIC,
    avg_amount NUMERIC,
    min_amount NUMERIC,
    max_amount NUMERIC,
    distinct_merchants BIGINT,
    distinct_mccs BIGINT
) AS $$
BEGIN
    RETURN QUERY
    SELECT 
        COUNT(*)::BIGINT as transaction_count,
        COALESCE(SUM(amount), 0) as total_amount,
        COALESCE(AVG(amount), 0) as avg_amount,
        MIN(amount) as min_amount,
        MAX(amount) as max_amount,
        COUNT(DISTINCT merchant_id)::BIGINT as distinct_merchants,
        COUNT(DISTINCT mcc)::BIGINT as distinct_mccs
    FROM velocity_transaction_log
    WHERE 
        CASE p_key_type
            WHEN 'PAN' THEN pan_hash = p_key_value
            WHEN 'CUSTOMER_ID' THEN customer_id = p_key_value
            WHEN 'MERCHANT_ID' THEN merchant_id = p_key_value
            ELSE FALSE
        END
        AND transaction_at >= NOW() - (p_window_minutes || ' minutes')::INTERVAL;
END;
$$ LANGUAGE plpgsql;

-- Comentários
COMMENT ON TABLE velocity_counters IS 'Contadores pré-computados de velocidade para detecção de fraude';
COMMENT ON TABLE velocity_transaction_log IS 'Log otimizado de transações para agregações de velocidade';
COMMENT ON FUNCTION get_velocity_stats IS 'Calcula estatísticas de velocidade em tempo real para uma chave';
