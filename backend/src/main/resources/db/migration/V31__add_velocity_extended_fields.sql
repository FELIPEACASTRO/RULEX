-- V31__add_velocity_extended_fields.sql
-- Adiciona campos estendidos para suportar operadores V28-V30
-- e otimiza queries de velocity com índices compostos

-- =====================================================
-- PARTE 1: Adicionar colunas faltantes
-- =====================================================

-- Campos para device fingerprinting e tracking
ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS device_fingerprint VARCHAR(64);

ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS ip_address INET;

ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS user_agent VARCHAR(500);

ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS beneficiary_id VARCHAR(50);

ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS session_id VARCHAR(64);

ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS pix_key VARCHAR(100);

ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS pix_key_type VARCHAR(20);

-- Campo para identificar transações crypto
ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS is_crypto_transaction BOOLEAN DEFAULT FALSE;

-- Campo para status da transação (para contagens de chargebacks, declines)
ALTER TABLE velocity_transaction_log 
ADD COLUMN IF NOT EXISTS transaction_status VARCHAR(20) DEFAULT 'APPROVED';

-- =====================================================
-- PARTE 2: Índices compostos para performance
-- =====================================================

-- Índice principal para queries de velocity por customer
CREATE INDEX IF NOT EXISTS idx_vtl_customer_created_desc 
ON velocity_transaction_log(customer_id, created_at DESC);

-- Índice para queries por PAN hash
CREATE INDEX IF NOT EXISTS idx_vtl_pan_hash_created_desc 
ON velocity_transaction_log(pan_hash, created_at DESC);

-- Índice para device fingerprint
CREATE INDEX IF NOT EXISTS idx_vtl_device_fingerprint 
ON velocity_transaction_log(device_fingerprint) 
WHERE device_fingerprint IS NOT NULL;

-- Índice para IP address
CREATE INDEX IF NOT EXISTS idx_vtl_ip_address 
ON velocity_transaction_log(ip_address) 
WHERE ip_address IS NOT NULL;

-- Índice para beneficiary (money mule detection)
CREATE INDEX IF NOT EXISTS idx_vtl_beneficiary 
ON velocity_transaction_log(beneficiary_id, created_at DESC) 
WHERE beneficiary_id IS NOT NULL;

-- Índice parcial para transações crypto (MCCs de crypto)
CREATE INDEX IF NOT EXISTS idx_vtl_crypto_transactions 
ON velocity_transaction_log(customer_id, created_at) 
WHERE mcc IN ('6051', '6012', '6211') OR is_crypto_transaction = TRUE;

-- Índice para status de transação
CREATE INDEX IF NOT EXISTS idx_vtl_status 
ON velocity_transaction_log(transaction_status, created_at DESC);

-- =====================================================
-- PARTE 3: Tabela para tracking de mudanças de device
-- =====================================================

CREATE TABLE IF NOT EXISTS customer_device_history (
    id BIGSERIAL PRIMARY KEY,
    customer_id VARCHAR(50) NOT NULL,
    device_fingerprint VARCHAR(64) NOT NULL,
    first_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_seen TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    transaction_count BIGINT DEFAULT 1,
    is_trusted BOOLEAN DEFAULT FALSE,
    CONSTRAINT uk_customer_device UNIQUE (customer_id, device_fingerprint)
);

CREATE INDEX IF NOT EXISTS idx_cdh_customer ON customer_device_history(customer_id);
CREATE INDEX IF NOT EXISTS idx_cdh_last_seen ON customer_device_history(last_seen DESC);

-- =====================================================
-- PARTE 4: Tabela para tracking de PIX keys
-- =====================================================

CREATE TABLE IF NOT EXISTS customer_pix_key_history (
    id BIGSERIAL PRIMARY KEY,
    customer_id VARCHAR(50) NOT NULL,
    pix_key VARCHAR(100) NOT NULL,
    pix_key_type VARCHAR(20) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    changed_at TIMESTAMP WITH TIME ZONE,
    is_active BOOLEAN DEFAULT TRUE
);

CREATE INDEX IF NOT EXISTS idx_cpkh_customer ON customer_pix_key_history(customer_id);
CREATE INDEX IF NOT EXISTS idx_cpkh_changed ON customer_pix_key_history(changed_at DESC) 
WHERE changed_at IS NOT NULL;

-- =====================================================
-- PARTE 5: Tabela para MFA events
-- =====================================================

CREATE TABLE IF NOT EXISTS mfa_event_log (
    id BIGSERIAL PRIMARY KEY,
    customer_id VARCHAR(50) NOT NULL,
    event_type VARCHAR(20) NOT NULL, -- 'SUCCESS', 'FAILURE', 'ABANDONMENT', 'DENIAL'
    mfa_method VARCHAR(20), -- 'SMS', 'APP', 'EMAIL', 'BIOMETRIC'
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    session_id VARCHAR(64),
    device_fingerprint VARCHAR(64),
    ip_address INET
);

CREATE INDEX IF NOT EXISTS idx_mfa_customer_created ON mfa_event_log(customer_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_mfa_event_type ON mfa_event_log(event_type, created_at DESC);

-- =====================================================
-- PARTE 6: Tabela para 3DS events
-- =====================================================

CREATE TABLE IF NOT EXISTS three_ds_event_log (
    id BIGSERIAL PRIMARY KEY,
    customer_id VARCHAR(50) NOT NULL,
    pan_hash VARCHAR(64) NOT NULL,
    event_type VARCHAR(20) NOT NULL, -- 'SUCCESS', 'FAILURE', 'CHALLENGE', 'FRICTIONLESS'
    three_ds_version VARCHAR(10), -- '1.0', '2.1', '2.2'
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    merchant_id VARCHAR(50),
    amount NUMERIC(19,2)
);

CREATE INDEX IF NOT EXISTS idx_3ds_customer_created ON three_ds_event_log(customer_id, created_at DESC);
CREATE INDEX IF NOT EXISTS idx_3ds_pan_created ON three_ds_event_log(pan_hash, created_at DESC);

-- =====================================================
-- PARTE 7: Atualizar estatísticas
-- =====================================================

ANALYZE velocity_transaction_log;
ANALYZE customer_device_history;
ANALYZE customer_pix_key_history;
ANALYZE mfa_event_log;
ANALYZE three_ds_event_log;

-- =====================================================
-- COMENTÁRIOS
-- =====================================================

COMMENT ON COLUMN velocity_transaction_log.device_fingerprint IS 'Hash único do dispositivo';
COMMENT ON COLUMN velocity_transaction_log.ip_address IS 'Endereço IP da transação';
COMMENT ON COLUMN velocity_transaction_log.user_agent IS 'User agent do navegador/app';
COMMENT ON COLUMN velocity_transaction_log.beneficiary_id IS 'ID do beneficiário (para P2P/PIX)';
COMMENT ON COLUMN velocity_transaction_log.is_crypto_transaction IS 'Flag para transações crypto';
COMMENT ON TABLE customer_device_history IS 'Histórico de dispositivos por cliente';
COMMENT ON TABLE customer_pix_key_history IS 'Histórico de chaves PIX por cliente';
COMMENT ON TABLE mfa_event_log IS 'Log de eventos MFA (sucesso, falha, abandono)';
COMMENT ON TABLE three_ds_event_log IS 'Log de eventos 3DS para detecção de fraude';
