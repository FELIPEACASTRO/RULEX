-- V11__bin_lookup_table.sql
-- Tabela de lookup de BIN (Bank Identification Number) para enriquecimento de transações
-- Permite identificar emissor, bandeira e país do cartão sem depender de serviço externo

-- =========================================
-- TABELA DE BIN LOOKUP
-- =========================================

CREATE TABLE IF NOT EXISTS bin_lookup (
    id              BIGSERIAL PRIMARY KEY,
    bin             VARCHAR(8) NOT NULL,           -- BIN de 6 ou 8 dígitos
    card_brand      VARCHAR(50),                   -- VISA, MASTERCARD, AMEX, ELO, etc.
    card_type       VARCHAR(20),                   -- CREDIT, DEBIT, PREPAID
    card_level      VARCHAR(50),                   -- CLASSIC, GOLD, PLATINUM, BLACK, INFINITE
    issuer_name     VARCHAR(255),                  -- Nome do banco emissor
    issuer_country  VARCHAR(3),                    -- Código ISO 3166-1 alpha-3 do país
    issuer_country_numeric VARCHAR(3),             -- Código numérico do país (ex: 076 para Brasil)
    is_regulated    BOOLEAN DEFAULT FALSE,         -- Se é cartão regulado
    is_commercial   BOOLEAN DEFAULT FALSE,         -- Se é cartão comercial/corporativo
    is_prepaid      BOOLEAN DEFAULT FALSE,         -- Se é pré-pago
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    
    CONSTRAINT uq_bin_lookup_bin UNIQUE (bin)
);

-- Índices para performance
CREATE INDEX IF NOT EXISTS idx_bin_lookup_bin ON bin_lookup(bin);
CREATE INDEX IF NOT EXISTS idx_bin_lookup_card_brand ON bin_lookup(card_brand);
CREATE INDEX IF NOT EXISTS idx_bin_lookup_issuer_country ON bin_lookup(issuer_country);

-- =========================================
-- SEED: BINs comuns brasileiros (exemplo)
-- =========================================

INSERT INTO bin_lookup (bin, card_brand, card_type, card_level, issuer_name, issuer_country, issuer_country_numeric, is_regulated, is_commercial, is_prepaid) VALUES
-- Visa Brasil
('411111', 'VISA', 'CREDIT', 'CLASSIC', 'Banco Teste', 'BRA', '076', FALSE, FALSE, FALSE),
('400000', 'VISA', 'CREDIT', 'CLASSIC', 'Banco Genérico', 'BRA', '076', FALSE, FALSE, FALSE),
('450001', 'VISA', 'DEBIT', 'ELECTRON', 'Banco Genérico', 'BRA', '076', FALSE, FALSE, FALSE),

-- Mastercard Brasil
('510000', 'MASTERCARD', 'CREDIT', 'STANDARD', 'Banco Teste', 'BRA', '076', FALSE, FALSE, FALSE),
('520000', 'MASTERCARD', 'CREDIT', 'GOLD', 'Banco Genérico', 'BRA', '076', FALSE, FALSE, FALSE),
('540000', 'MASTERCARD', 'CREDIT', 'PLATINUM', 'Banco Premium', 'BRA', '076', FALSE, FALSE, FALSE),

-- Elo Brasil
('636368', 'ELO', 'CREDIT', 'CLASSIC', 'Banco Elo', 'BRA', '076', FALSE, FALSE, FALSE),
('506699', 'ELO', 'DEBIT', 'CLASSIC', 'Banco Elo', 'BRA', '076', FALSE, FALSE, FALSE),

-- Amex Brasil
('376411', 'AMEX', 'CREDIT', 'GREEN', 'American Express', 'BRA', '076', FALSE, FALSE, FALSE),
('378282', 'AMEX', 'CREDIT', 'GOLD', 'American Express', 'BRA', '076', FALSE, FALSE, FALSE),

-- Hipercard Brasil
('606282', 'HIPERCARD', 'CREDIT', 'CLASSIC', 'Hipercard', 'BRA', '076', FALSE, FALSE, FALSE),

-- Visa Internacional (exemplos)
('400001', 'VISA', 'CREDIT', 'CLASSIC', 'Bank of America', 'USA', '840', FALSE, FALSE, FALSE),
('400002', 'VISA', 'CREDIT', 'SIGNATURE', 'Chase', 'USA', '840', FALSE, FALSE, FALSE),

-- Mastercard Internacional (exemplos)
('510001', 'MASTERCARD', 'CREDIT', 'WORLD', 'Citibank', 'USA', '840', FALSE, FALSE, FALSE),
('520001', 'MASTERCARD', 'CREDIT', 'WORLD_ELITE', 'HSBC', 'GBR', '826', FALSE, FALSE, FALSE)

ON CONFLICT (bin) DO NOTHING;

-- =========================================
-- TABELA DE CATEGORIZAÇÃO DE MCC
-- =========================================

CREATE TABLE IF NOT EXISTS mcc_category (
    id              BIGSERIAL PRIMARY KEY,
    mcc             INTEGER NOT NULL,              -- Merchant Category Code
    category        VARCHAR(100) NOT NULL,         -- Categoria principal
    subcategory     VARCHAR(100),                  -- Subcategoria
    description     VARCHAR(500),                  -- Descrição do MCC
    risk_level      VARCHAR(20) DEFAULT 'LOW',     -- LOW, MEDIUM, HIGH, CRITICAL
    is_high_risk    BOOLEAN DEFAULT FALSE,         -- Flag de alto risco
    is_gambling     BOOLEAN DEFAULT FALSE,         -- Flag de jogos/apostas
    is_crypto       BOOLEAN DEFAULT FALSE,         -- Flag de criptomoedas
    is_adult        BOOLEAN DEFAULT FALSE,         -- Flag de conteúdo adulto
    is_cash_advance BOOLEAN DEFAULT FALSE,         -- Flag de saque/adiantamento
    created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
    
    CONSTRAINT uq_mcc_category_mcc UNIQUE (mcc)
);

-- Índices para performance
CREATE INDEX IF NOT EXISTS idx_mcc_category_mcc ON mcc_category(mcc);
CREATE INDEX IF NOT EXISTS idx_mcc_category_risk_level ON mcc_category(risk_level);
CREATE INDEX IF NOT EXISTS idx_mcc_category_is_high_risk ON mcc_category(is_high_risk);

-- =========================================
-- SEED: MCCs de alto risco
-- =========================================

INSERT INTO mcc_category (mcc, category, subcategory, description, risk_level, is_high_risk, is_gambling, is_crypto, is_adult, is_cash_advance) VALUES
-- Jogos e Apostas
(7995, 'GAMBLING', 'BETTING', 'Apostas e jogos de azar', 'CRITICAL', TRUE, TRUE, FALSE, FALSE, FALSE),
(7994, 'GAMBLING', 'VIDEO_GAMES', 'Arcades e jogos de vídeo', 'HIGH', TRUE, TRUE, FALSE, FALSE, FALSE),
(7993, 'GAMBLING', 'AMUSEMENT', 'Parques de diversão e circos', 'MEDIUM', FALSE, FALSE, FALSE, FALSE, FALSE),

-- Criptomoedas e Investimentos de Alto Risco
(6051, 'FINANCIAL', 'CRYPTO', 'Criptomoedas e moedas digitais', 'CRITICAL', TRUE, FALSE, TRUE, FALSE, FALSE),
(6211, 'FINANCIAL', 'SECURITIES', 'Corretoras de valores', 'HIGH', TRUE, FALSE, FALSE, FALSE, FALSE),
(6012, 'FINANCIAL', 'BANK', 'Instituições financeiras', 'MEDIUM', FALSE, FALSE, FALSE, FALSE, FALSE),

-- Serviços de Encontros e Adulto
(7273, 'SERVICES', 'DATING', 'Serviços de encontros e acompanhantes', 'HIGH', TRUE, FALSE, FALSE, TRUE, FALSE),
(5967, 'RETAIL', 'ADULT', 'Produtos para adultos', 'HIGH', TRUE, FALSE, FALSE, TRUE, FALSE),

-- Transferências de Dinheiro
(4829, 'FINANCIAL', 'MONEY_TRANSFER', 'Transferências de dinheiro', 'HIGH', TRUE, FALSE, FALSE, FALSE, TRUE),
(6010, 'FINANCIAL', 'CASH_ADVANCE', 'Adiantamento de dinheiro', 'HIGH', TRUE, FALSE, FALSE, FALSE, TRUE),
(6011, 'FINANCIAL', 'ATM', 'Saques em caixas eletrônicos', 'MEDIUM', FALSE, FALSE, FALSE, FALSE, TRUE),

-- Viagens (risco médio por fraude comum)
(4511, 'TRAVEL', 'AIRLINES', 'Companhias aéreas', 'MEDIUM', FALSE, FALSE, FALSE, FALSE, FALSE),
(7011, 'TRAVEL', 'HOTELS', 'Hotéis e hospedagem', 'MEDIUM', FALSE, FALSE, FALSE, FALSE, FALSE),
(7512, 'TRAVEL', 'CAR_RENTAL', 'Aluguel de veículos', 'MEDIUM', FALSE, FALSE, FALSE, FALSE, FALSE),

-- Varejo comum (baixo risco)
(5411, 'RETAIL', 'GROCERY', 'Supermercados', 'LOW', FALSE, FALSE, FALSE, FALSE, FALSE),
(5812, 'FOOD', 'RESTAURANT', 'Restaurantes', 'LOW', FALSE, FALSE, FALSE, FALSE, FALSE),
(5541, 'AUTOMOTIVE', 'GAS_STATION', 'Postos de combustível', 'LOW', FALSE, FALSE, FALSE, FALSE, FALSE),
(5311, 'RETAIL', 'DEPARTMENT_STORE', 'Lojas de departamento', 'LOW', FALSE, FALSE, FALSE, FALSE, FALSE),
(5912, 'HEALTH', 'PHARMACY', 'Farmácias', 'LOW', FALSE, FALSE, FALSE, FALSE, FALSE)

ON CONFLICT (mcc) DO NOTHING;

-- =========================================
-- COMENTÁRIOS PARA DOCUMENTAÇÃO
-- =========================================

COMMENT ON TABLE bin_lookup IS 'Tabela de lookup de BIN para identificação de emissor, bandeira e país do cartão';
COMMENT ON TABLE mcc_category IS 'Tabela de categorização de MCC com níveis de risco para análise antifraude';

COMMENT ON COLUMN bin_lookup.bin IS 'Bank Identification Number - primeiros 6-8 dígitos do PAN';
COMMENT ON COLUMN bin_lookup.card_brand IS 'Bandeira do cartão (VISA, MASTERCARD, ELO, etc.)';
COMMENT ON COLUMN bin_lookup.issuer_country_numeric IS 'Código numérico ISO do país do emissor';

COMMENT ON COLUMN mcc_category.mcc IS 'Merchant Category Code - código de 4 dígitos';
COMMENT ON COLUMN mcc_category.risk_level IS 'Nível de risco: LOW, MEDIUM, HIGH, CRITICAL';
COMMENT ON COLUMN mcc_category.is_high_risk IS 'Flag indicando se o MCC é considerado de alto risco para fraude';
