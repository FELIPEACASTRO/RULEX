-- V32: Adiciona tabelas faltantes para operadores incompletos
-- Corrige gaps identificados no Triple Check Imparcial

-- =========================================
-- 1. TABELA DE FERIADOS (para IS_HOLIDAY)
-- =========================================
CREATE TABLE IF NOT EXISTS holidays (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    holiday_date DATE NOT NULL,
    holiday_name VARCHAR(255) NOT NULL,
    country_code VARCHAR(3) NOT NULL DEFAULT 'BRA',
    state_code VARCHAR(2) NOT NULL DEFAULT '', -- '' = feriado nacional
    city_code VARCHAR(10) NOT NULL DEFAULT '', -- '' = feriado estadual/nacional
    holiday_type VARCHAR(50) NOT NULL DEFAULT 'NATIONAL', -- NATIONAL, STATE, MUNICIPAL, BANK
    is_bank_holiday BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(holiday_date, country_code, state_code, city_code)
);

-- Índices para consulta rápida
CREATE INDEX IF NOT EXISTS idx_holidays_date ON holidays(holiday_date);
CREATE INDEX IF NOT EXISTS idx_holidays_country ON holidays(country_code);
CREATE INDEX IF NOT EXISTS idx_holidays_date_country ON holidays(holiday_date, country_code);

-- Inserir feriados brasileiros 2024-2026
INSERT INTO holidays (holiday_date, holiday_name, country_code, holiday_type) VALUES
-- 2024
('2024-01-01', 'Confraternização Universal', 'BRA', 'NATIONAL'),
('2024-02-12', 'Carnaval', 'BRA', 'NATIONAL'),
('2024-02-13', 'Carnaval', 'BRA', 'NATIONAL'),
('2024-03-29', 'Sexta-feira Santa', 'BRA', 'NATIONAL'),
('2024-04-21', 'Tiradentes', 'BRA', 'NATIONAL'),
('2024-05-01', 'Dia do Trabalho', 'BRA', 'NATIONAL'),
('2024-05-30', 'Corpus Christi', 'BRA', 'NATIONAL'),
('2024-09-07', 'Independência do Brasil', 'BRA', 'NATIONAL'),
('2024-10-12', 'Nossa Senhora Aparecida', 'BRA', 'NATIONAL'),
('2024-11-02', 'Finados', 'BRA', 'NATIONAL'),
('2024-11-15', 'Proclamação da República', 'BRA', 'NATIONAL'),
('2024-11-20', 'Consciência Negra', 'BRA', 'NATIONAL'),
('2024-12-25', 'Natal', 'BRA', 'NATIONAL'),
-- 2025
('2025-01-01', 'Confraternização Universal', 'BRA', 'NATIONAL'),
('2025-03-03', 'Carnaval', 'BRA', 'NATIONAL'),
('2025-03-04', 'Carnaval', 'BRA', 'NATIONAL'),
('2025-04-18', 'Sexta-feira Santa', 'BRA', 'NATIONAL'),
('2025-04-21', 'Tiradentes', 'BRA', 'NATIONAL'),
('2025-05-01', 'Dia do Trabalho', 'BRA', 'NATIONAL'),
('2025-06-19', 'Corpus Christi', 'BRA', 'NATIONAL'),
('2025-09-07', 'Independência do Brasil', 'BRA', 'NATIONAL'),
('2025-10-12', 'Nossa Senhora Aparecida', 'BRA', 'NATIONAL'),
('2025-11-02', 'Finados', 'BRA', 'NATIONAL'),
('2025-11-15', 'Proclamação da República', 'BRA', 'NATIONAL'),
('2025-11-20', 'Consciência Negra', 'BRA', 'NATIONAL'),
('2025-12-25', 'Natal', 'BRA', 'NATIONAL'),
-- 2026
('2026-01-01', 'Confraternização Universal', 'BRA', 'NATIONAL'),
('2026-02-16', 'Carnaval', 'BRA', 'NATIONAL'),
('2026-02-17', 'Carnaval', 'BRA', 'NATIONAL'),
('2026-04-03', 'Sexta-feira Santa', 'BRA', 'NATIONAL'),
('2026-04-21', 'Tiradentes', 'BRA', 'NATIONAL'),
('2026-05-01', 'Dia do Trabalho', 'BRA', 'NATIONAL'),
('2026-06-04', 'Corpus Christi', 'BRA', 'NATIONAL'),
('2026-09-07', 'Independência do Brasil', 'BRA', 'NATIONAL'),
('2026-10-12', 'Nossa Senhora Aparecida', 'BRA', 'NATIONAL'),
('2026-11-02', 'Finados', 'BRA', 'NATIONAL'),
('2026-11-15', 'Proclamação da República', 'BRA', 'NATIONAL'),
('2026-11-20', 'Consciência Negra', 'BRA', 'NATIONAL'),
('2026-12-25', 'Natal', 'BRA', 'NATIONAL')
ON CONFLICT DO NOTHING;

-- =========================================
-- 2. TABELA DE CHARGEBACKS (para CHARGEBACK_RATE_GT e IN_CUSTOMER_CHARGEBACK_MERCHANTS)
-- =========================================
CREATE TABLE IF NOT EXISTS merchant_chargebacks (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    merchant_id VARCHAR(100) NOT NULL,
    merchant_name VARCHAR(255),
    mcc VARCHAR(4),
    total_transactions BIGINT NOT NULL DEFAULT 0,
    total_chargebacks BIGINT NOT NULL DEFAULT 0,
    chargeback_rate DECIMAL(5,4) NOT NULL DEFAULT 0, -- Ex: 0.0150 = 1.5%
    total_amount DECIMAL(18,2) NOT NULL DEFAULT 0,
    chargeback_amount DECIMAL(18,2) NOT NULL DEFAULT 0,
    last_chargeback_date TIMESTAMPTZ,
    first_transaction_date TIMESTAMPTZ,
    last_transaction_date TIMESTAMPTZ,
    risk_level VARCHAR(20) NOT NULL DEFAULT 'LOW', -- LOW, MEDIUM, HIGH, CRITICAL
    is_blocked BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(merchant_id)
);

-- Índices para consulta rápida
CREATE INDEX IF NOT EXISTS idx_merchant_chargebacks_merchant ON merchant_chargebacks(merchant_id);
CREATE INDEX IF NOT EXISTS idx_merchant_chargebacks_rate ON merchant_chargebacks(chargeback_rate);
CREATE INDEX IF NOT EXISTS idx_merchant_chargebacks_risk ON merchant_chargebacks(risk_level);
CREATE INDEX IF NOT EXISTS idx_merchant_chargebacks_blocked ON merchant_chargebacks(is_blocked);

-- Tabela de histórico de chargebacks por cliente
CREATE TABLE IF NOT EXISTS customer_chargeback_history (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100) NOT NULL,
    merchant_id VARCHAR(100) NOT NULL,
    transaction_id VARCHAR(100),
    chargeback_date TIMESTAMPTZ NOT NULL,
    chargeback_amount DECIMAL(18,2) NOT NULL,
    chargeback_reason VARCHAR(100),
    chargeback_code VARCHAR(20),
    status VARCHAR(20) NOT NULL DEFAULT 'PENDING', -- PENDING, APPROVED, DENIED, REVERSED
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_customer_chargeback_customer ON customer_chargeback_history(customer_id);
CREATE INDEX IF NOT EXISTS idx_customer_chargeback_merchant ON customer_chargeback_history(merchant_id);
CREATE INDEX IF NOT EXISTS idx_customer_chargeback_date ON customer_chargeback_history(chargeback_date);

-- =========================================
-- 3. TABELA DE TRANSFERÊNCIAS RECEBIDAS (para GTE_PERCENT_OF_LAST_INCOMING)
-- =========================================
CREATE TABLE IF NOT EXISTS customer_incoming_transfers (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100) NOT NULL,
    account_number VARCHAR(50),
    transfer_amount DECIMAL(18,2) NOT NULL,
    transfer_date TIMESTAMPTZ NOT NULL,
    transfer_type VARCHAR(50) NOT NULL, -- PIX, TED, DOC, SALARY, REFUND
    source_bank VARCHAR(10),
    source_account VARCHAR(50),
    source_name VARCHAR(255),
    description VARCHAR(500),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_incoming_transfers_customer ON customer_incoming_transfers(customer_id);
CREATE INDEX IF NOT EXISTS idx_incoming_transfers_date ON customer_incoming_transfers(transfer_date DESC);
CREATE INDEX IF NOT EXISTS idx_incoming_transfers_customer_date ON customer_incoming_transfers(customer_id, transfer_date DESC);

-- =========================================
-- 4. TABELA DE FALHAS DE AUTENTICAÇÃO (para COUNT_FAILURES_LAST_N_HOURS)
-- =========================================
CREATE TABLE IF NOT EXISTS authentication_failures (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100),
    account_number VARCHAR(50),
    card_number_hash VARCHAR(64), -- Hash do PAN
    failure_type VARCHAR(50) NOT NULL, -- CVV, PIN, 3DS, OTP, PASSWORD, BIOMETRIC
    failure_reason VARCHAR(100),
    ip_address VARCHAR(45),
    device_id VARCHAR(255),
    user_agent TEXT,
    failure_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_auth_failures_customer ON authentication_failures(customer_id);
CREATE INDEX IF NOT EXISTS idx_auth_failures_card ON authentication_failures(card_number_hash);
CREATE INDEX IF NOT EXISTS idx_auth_failures_timestamp ON authentication_failures(failure_timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_auth_failures_type ON authentication_failures(failure_type);

-- =========================================
-- 5. TABELA DE ÚLTIMA TRANSAÇÃO (para TIME_SINCE_LAST_LT)
-- =========================================
CREATE TABLE IF NOT EXISTS customer_last_transaction (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100) NOT NULL UNIQUE,
    card_number_hash VARCHAR(64),
    last_transaction_id VARCHAR(100),
    last_transaction_date TIMESTAMPTZ NOT NULL,
    last_transaction_amount DECIMAL(18,2),
    last_merchant_id VARCHAR(100),
    last_merchant_name VARCHAR(255),
    last_mcc VARCHAR(4),
    last_country_code VARCHAR(3),
    last_city VARCHAR(100),
    transaction_count_today BIGINT NOT NULL DEFAULT 0,
    transaction_count_week BIGINT NOT NULL DEFAULT 0,
    transaction_count_month BIGINT NOT NULL DEFAULT 0,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_last_txn_customer ON customer_last_transaction(customer_id);
CREATE INDEX IF NOT EXISTS idx_last_txn_card ON customer_last_transaction(card_number_hash);
CREATE INDEX IF NOT EXISTS idx_last_txn_date ON customer_last_transaction(last_transaction_date DESC);

-- =========================================
-- 6. TABELA DE NÚMEROS VOIP (para IS_VOIP)
-- =========================================
CREATE TABLE IF NOT EXISTS voip_phone_ranges (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    country_code VARCHAR(5) NOT NULL,
    area_code VARCHAR(5),
    prefix_start VARCHAR(10) NOT NULL,
    prefix_end VARCHAR(10),
    carrier_name VARCHAR(100),
    is_voip BOOLEAN NOT NULL DEFAULT TRUE,
    risk_level VARCHAR(20) NOT NULL DEFAULT 'HIGH',
    notes TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_voip_country ON voip_phone_ranges(country_code);
CREATE INDEX IF NOT EXISTS idx_voip_prefix ON voip_phone_ranges(prefix_start);

-- Inserir alguns ranges conhecidos de VoIP no Brasil
INSERT INTO voip_phone_ranges (country_code, area_code, prefix_start, carrier_name, is_voip, risk_level) VALUES
('55', NULL, '0300', 'Serviços Especiais', TRUE, 'HIGH'),
('55', NULL, '0303', 'Telemarketing', TRUE, 'HIGH'),
('55', NULL, '0500', 'Serviços Especiais', TRUE, 'MEDIUM'),
('55', NULL, '0800', 'Ligação Gratuita', FALSE, 'LOW'),
('55', NULL, '0900', 'Serviços Premium', TRUE, 'HIGH'),
('55', NULL, '4000', 'VoIP Corporativo', TRUE, 'MEDIUM'),
('55', NULL, '4003', 'VoIP Corporativo', TRUE, 'MEDIUM'),
('55', NULL, '4004', 'VoIP Corporativo', TRUE, 'MEDIUM'),
('55', NULL, '4020', 'VoIP', TRUE, 'HIGH'),
('55', NULL, '4062', 'VoIP', TRUE, 'HIGH')
ON CONFLICT DO NOTHING;

-- =========================================
-- 7. TABELA DE PADRÕES DE ESCADA (para PATTERN_ESCALATION)
-- =========================================
CREATE TABLE IF NOT EXISTS transaction_escalation_patterns (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100) NOT NULL,
    card_number_hash VARCHAR(64),
    pattern_start_date TIMESTAMPTZ NOT NULL,
    pattern_end_date TIMESTAMPTZ,
    transaction_count INT NOT NULL DEFAULT 0,
    amounts DECIMAL(18,2)[] NOT NULL DEFAULT '{}',
    escalation_factor DECIMAL(5,2), -- Ex: 1.5 = cada transação 50% maior
    total_amount DECIMAL(18,2) NOT NULL DEFAULT 0,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    risk_score INT NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_escalation_customer ON transaction_escalation_patterns(customer_id);
CREATE INDEX IF NOT EXISTS idx_escalation_card ON transaction_escalation_patterns(card_number_hash);
CREATE INDEX IF NOT EXISTS idx_escalation_active ON transaction_escalation_patterns(is_active);

-- =========================================
-- 8. TABELA DE HISTÓRICO DE BENEFICIÁRIOS (para NOT_IN_HISTORICAL)
-- =========================================
CREATE TABLE IF NOT EXISTS customer_beneficiary_history (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100) NOT NULL,
    beneficiary_id VARCHAR(100) NOT NULL,
    beneficiary_name VARCHAR(255),
    beneficiary_document VARCHAR(20),
    beneficiary_bank VARCHAR(10),
    beneficiary_account VARCHAR(50),
    beneficiary_type VARCHAR(50), -- PIX_KEY, ACCOUNT, CARD
    first_transaction_date TIMESTAMPTZ NOT NULL,
    last_transaction_date TIMESTAMPTZ NOT NULL,
    transaction_count BIGINT NOT NULL DEFAULT 1,
    total_amount DECIMAL(18,2) NOT NULL DEFAULT 0,
    is_trusted BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    UNIQUE(customer_id, beneficiary_id)
);

CREATE INDEX IF NOT EXISTS idx_beneficiary_customer ON customer_beneficiary_history(customer_id);
CREATE INDEX IF NOT EXISTS idx_beneficiary_id ON customer_beneficiary_history(beneficiary_id);
CREATE INDEX IF NOT EXISTS idx_beneficiary_trusted ON customer_beneficiary_history(is_trusted);

-- =========================================
-- 9. TABELA DE IDADE DE CONTA (para ACCOUNT_AGE_LT_MINUTES)
-- =========================================
CREATE TABLE IF NOT EXISTS customer_account_info (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id VARCHAR(100) NOT NULL UNIQUE,
    account_number VARCHAR(50),
    account_created_at TIMESTAMPTZ NOT NULL,
    account_type VARCHAR(50), -- CHECKING, SAVINGS, CREDIT
    kyc_level VARCHAR(20) NOT NULL DEFAULT 'BASIC', -- BASIC, STANDARD, ENHANCED
    kyc_verified_at TIMESTAMPTZ,
    first_transaction_at TIMESTAMPTZ,
    total_transactions BIGINT NOT NULL DEFAULT 0,
    total_amount DECIMAL(18,2) NOT NULL DEFAULT 0,
    risk_score INT NOT NULL DEFAULT 0,
    is_blocked BOOLEAN NOT NULL DEFAULT FALSE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_account_info_customer ON customer_account_info(customer_id);
CREATE INDEX IF NOT EXISTS idx_account_info_created ON customer_account_info(account_created_at);
CREATE INDEX IF NOT EXISTS idx_account_info_kyc ON customer_account_info(kyc_level);

-- Comentários nas tabelas
COMMENT ON TABLE holidays IS 'Tabela de feriados para operador IS_HOLIDAY';
COMMENT ON TABLE merchant_chargebacks IS 'Estatísticas de chargeback por merchant para CHARGEBACK_RATE_GT';
COMMENT ON TABLE customer_chargeback_history IS 'Histórico de chargebacks por cliente para IN_CUSTOMER_CHARGEBACK_MERCHANTS';
COMMENT ON TABLE customer_incoming_transfers IS 'Transferências recebidas para GTE_PERCENT_OF_LAST_INCOMING';
COMMENT ON TABLE authentication_failures IS 'Falhas de autenticação para COUNT_FAILURES_LAST_N_HOURS';
COMMENT ON TABLE customer_last_transaction IS 'Última transação por cliente para TIME_SINCE_LAST_LT';
COMMENT ON TABLE voip_phone_ranges IS 'Ranges de telefones VoIP para IS_VOIP';
COMMENT ON TABLE transaction_escalation_patterns IS 'Padrões de escada para PATTERN_ESCALATION';
COMMENT ON TABLE customer_beneficiary_history IS 'Histórico de beneficiários para NOT_IN_HISTORICAL';
COMMENT ON TABLE customer_account_info IS 'Informações de conta para ACCOUNT_AGE_LT_MINUTES';
