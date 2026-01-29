-- V52__test_card_patterns.sql
-- Tabela para BINs e PANs de teste/blacklist configuráveis

CREATE TABLE IF NOT EXISTS test_card_patterns (
    id BIGSERIAL PRIMARY KEY,
    pattern VARCHAR(64) NOT NULL,
    pattern_type VARCHAR(20) NOT NULL,
    category VARCHAR(20) NOT NULL,
    description VARCHAR(255),
    card_brand VARCHAR(50),
    is_active BOOLEAN DEFAULT true,
    created_by VARCHAR(100),
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    
    CONSTRAINT chk_pattern_type CHECK (pattern_type IN ('BIN', 'PAN', 'REGEX')),
    CONSTRAINT chk_category CHECK (category IN ('TEST', 'BLACKLISTED', 'SUSPICIOUS'))
);

-- Índices para busca rápida
CREATE INDEX IF NOT EXISTS idx_test_card_pattern ON test_card_patterns(pattern);
CREATE INDEX IF NOT EXISTS idx_test_card_type ON test_card_patterns(pattern_type);
CREATE INDEX IF NOT EXISTS idx_test_card_category ON test_card_patterns(category);
CREATE INDEX IF NOT EXISTS idx_test_card_active ON test_card_patterns(is_active);

-- Seed com os dados que antes eram hardcoded

-- BINs de teste conhecidos
INSERT INTO test_card_patterns (pattern, pattern_type, category, description, card_brand, created_by)
VALUES
    ('400000', 'BIN', 'TEST', 'BIN de teste Visa - range 400000-400099', 'VISA', 'system'),
    ('411111', 'BIN', 'TEST', 'BIN de teste comum Visa', 'VISA', 'system'),
    ('555555', 'BIN', 'TEST', 'BIN de teste Mastercard', 'MASTERCARD', 'system'),
    ('378282', 'BIN', 'TEST', 'BIN de teste American Express', 'AMEX', 'system'),
    ('370000', 'BIN', 'TEST', 'BIN de teste American Express alternativo', 'AMEX', 'system'),
    ('601100', 'BIN', 'TEST', 'BIN de teste Discover', 'DISCOVER', 'system'),
    ('300000', 'BIN', 'TEST', 'BIN de teste Diners Club', 'DINERS', 'system'),
    ('353011', 'BIN', 'TEST', 'BIN de teste JCB', 'JCB', 'system')
ON CONFLICT DO NOTHING;

-- PANs de teste completos (usados em ambientes de desenvolvimento)
INSERT INTO test_card_patterns (pattern, pattern_type, category, description, card_brand, created_by)
VALUES
    ('4111111111111111', 'PAN', 'TEST', 'Visa test card - aceita qualquer CVV', 'VISA', 'system'),
    ('4000000000000002', 'PAN', 'TEST', 'Visa test card - recusa por CVV', 'VISA', 'system'),
    ('4000000000000069', 'PAN', 'TEST', 'Visa test card - expired card', 'VISA', 'system'),
    ('4000000000000127', 'PAN', 'TEST', 'Visa test card - incorrect CVV', 'VISA', 'system'),
    ('5500000000000004', 'PAN', 'TEST', 'Mastercard test card', 'MASTERCARD', 'system'),
    ('5105105105105100', 'PAN', 'TEST', 'Mastercard prepaid test', 'MASTERCARD', 'system'),
    ('340000000000009', 'PAN', 'TEST', 'American Express test card', 'AMEX', 'system'),
    ('378282246310005', 'PAN', 'TEST', 'American Express corporate test', 'AMEX', 'system'),
    ('30000000000004', 'PAN', 'TEST', 'Diners Club test card', 'DINERS', 'system'),
    ('6011000000000004', 'PAN', 'TEST', 'Discover test card', 'DISCOVER', 'system'),
    ('6011111111111117', 'PAN', 'TEST', 'Discover test card alternativo', 'DISCOVER', 'system'),
    ('3530111333300000', 'PAN', 'TEST', 'JCB test card', 'JCB', 'system')
ON CONFLICT DO NOTHING;

-- Padrões regex para detecção de cartões de teste
INSERT INTO test_card_patterns (pattern, pattern_type, category, description, created_by)
VALUES
    ('^4[0]{7,}$', 'REGEX', 'TEST', 'Padrão Visa com muitos zeros', 'system'),
    ('^5[0-5]{3}0{8,}$', 'REGEX', 'TEST', 'Padrão Mastercard com muitos zeros', 'system'),
    ('^(0{16}|1{16}|9{16})$', 'REGEX', 'SUSPICIOUS', 'PAN com dígitos repetidos', 'system'),
    ('^12345678901234', 'REGEX', 'SUSPICIOUS', 'PAN sequencial óbvio', 'system')
ON CONFLICT DO NOTHING;

-- Comentário para referência
COMMENT ON TABLE test_card_patterns IS 'Tabela de BINs e PANs de teste/blacklist para detecção de fraude. Substitui listas hardcoded no código.';
