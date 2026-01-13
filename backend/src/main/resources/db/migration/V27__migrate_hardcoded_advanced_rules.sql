-- V27: Migração das regras avançadas para o banco de dados
-- Usando o schema correto de rule_configurations (threshold, parameters, conditions_json)
-- Data: 2026-01-05

-- GRUPO 1: EMV SECURITY (2 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('EMV_SECURITY_CHECK', 'Valida indicadores de segurança EMV - Transações > 1000 com AIP inválido', true, 30, 'SUSPICIOUS', 'SECURITY', 0, NULL, '[{"field":"cardAipStatic","operator":"NE","value":"Y"}]', 'OR', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('TERMINAL_VERIFICATION_FAILED', 'Detecta falhas em verificação do terminal', true, 100, 'FRAUD', 'SECURITY', 0, NULL, '[{"field":"terminalVerificationResults","operator":"CONTAINS","value":"FAIL"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 2: TRANSACTION CONTEXT (3 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('EXPIRED_CARD', 'Valida se o cartão não está expirado', true, 100, 'FRAUD', 'CONTEXT', 0, NULL, '[{"field":"cardExpireDate","operator":"LT","value":"transactionDate"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('SUSPICIOUS_TRANSACTION_TYPE', 'Detecta tipos de transação suspeitos (Reversal, Void)', true, 40, 'SUSPICIOUS', 'CONTEXT', 0, NULL, '[{"field":"transactionType","operator":"IN","value":"R,V"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('UNUSUAL_CARD_MEDIA', 'Detecta mídia de cartão anômala', true, 30, 'SUSPICIOUS', 'CONTEXT', 0, NULL, '[{"field":"cardMediaType","operator":"NOT_IN","value":"C,M"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 3: TERMINAL & NETWORK (4 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('SUSPICIOUS_TERMINAL', 'Detecta terminais suspeitos (ATM)', true, 40, 'SUSPICIOUS', 'SECURITY', 0, NULL, '[{"field":"terminalType","operator":"EQ","value":"A"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('ECOMMERCE_NO_AVS', 'Detecta E-commerce sem AVS', true, 35, 'SUSPICIOUS', 'SECURITY', 0, NULL, '[{"field":"eciIndicator","operator":"EQ","value":"5"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('POS_SECURITY_MISSING', 'Detecta falta de segurança no POS', true, 35, 'SUSPICIOUS', 'SECURITY', 0, NULL, '[{"field":"posSecurity","operator":"EQ","value":"0"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('CARD_CAPTURE_FRAUD', 'Detecta múltiplas capturas do mesmo cartão', true, 100, 'FRAUD', 'VELOCITY', 2, NULL, '[{"field":"posCardCapture","operator":"GT","value":"2"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 4: PIN/CVV VERIFICATION (3 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('PIN_CVV_LIMIT_EXCEEDED', 'Detecta limite de tentativas de PIN/CVV excedido', true, 100, 'FRAUD', 'SECURITY', 0, NULL, '[{"field":"cvvPinTryLimitExceeded","operator":"EQ","value":"1"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('OFFLINE_PIN_FAILED', 'Detecta falha de PIN offline', true, 100, 'FRAUD', 'SECURITY', 0, NULL, '[{"field":"cvrofflinePinVerificationFailed","operator":"EQ","value":"1"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
VALUES 
('MISSING_CVV2_HIGH_RISK', 'Detecta CVV2 ausente em transação de alto risco', true, 40, 'SUSPICIOUS', 'SECURITY', 0, NULL, '[{"field":"cvv2Present","operator":"EQ","value":"0"}]', 'AND', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- Criar índices para otimizar queries
CREATE INDEX IF NOT EXISTS idx_rule_configurations_rule_type ON rule_configurations(rule_type);
CREATE INDEX IF NOT EXISTS idx_rule_configurations_enabled_classification ON rule_configurations(enabled, classification);

-- Comentário de auditoria
COMMENT ON TABLE rule_configurations IS 'Tabela unificada de regras de fraude - V27 migrou regras avançadas';
