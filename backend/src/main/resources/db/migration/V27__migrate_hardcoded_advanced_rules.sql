-- V27: Migração das 28 regras hardcoded do AdvancedRuleEngineService para o banco de dados
-- GAP-FIX: Elimina a fragmentação de regras (hardcoded vs banco vs especificadas)
-- Autor: Manus AI - Correção de GAPs Arquiteturais
-- Data: 2026-01-05

-- Inserir as 28 regras avançadas que estavam hardcoded no AdvancedRuleEngineService
-- Usando ON CONFLICT para evitar duplicatas caso já existam

-- GRUPO 1: EMV SECURITY (2 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('EMV_SECURITY_CHECK', 'Valida indicadores de segurança EMV (cardAipStatic, cardAipDynamic, etc) - Transações > 1000 com AIP inválido', true, 30, 'SUSPICIOUS', 'COMPLEX', 'cardAipStatic', 'COMPLEX_EXPRESSION', 'cardAipStatic != "Y" OR cardAipDynamic != "Y" OR cardAipVerify != "Y"', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('TERMINAL_VERIFICATION_FAILED', 'Detecta falhas em verificação do terminal ou cartão', true, 100, 'FRAUD', 'COMPLEX', 'terminalVerificationResults', 'CONTAINS', 'FAIL', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 2: TRANSACTION CONTEXT (3 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('EXPIRED_CARD', 'Valida se o cartão não está expirado', true, 100, 'FRAUD', 'COMPARISON', 'cardExpireDate', 'LESS_THAN', 'transactionDate', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('SUSPICIOUS_TRANSACTION_TYPE', 'Detecta tipos de transação suspeitos (Reversal, Void) com valor > 2x média', true, 40, 'SUSPICIOUS', 'COMPLEX', 'transactionType', 'IN', 'R,V', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('UNUSUAL_CARD_MEDIA', 'Detecta mídia de cartão anômala (Chip em E-commerce)', true, 30, 'SUSPICIOUS', 'COMPLEX', 'cardMediaType', 'NOT_IN', 'C,M', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 3: TERMINAL & NETWORK (4 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('SUSPICIOUS_TERMINAL', 'Detecta terminais suspeitos (ATM com transação fora do estabelecimento > 5000)', true, 40, 'SUSPICIOUS', 'COMPLEX', 'terminalType', 'EQUALS', 'A', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('ECOMMERCE_NO_AVS', 'Detecta E-commerce sem AVS com valor > 1000', true, 35, 'SUSPICIOUS', 'COMPLEX', 'eciIndicator', 'EQUALS', '5', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('POS_SECURITY_MISSING', 'Detecta falta de segurança no POS com chip e valor > 2000', true, 35, 'SUSPICIOUS', 'COMPLEX', 'posSecurity', 'EQUALS', '0', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('CARD_CAPTURE_FRAUD', 'Detecta múltiplas capturas do mesmo cartão (> 2 em 30 dias)', true, 100, 'FRAUD', 'VELOCITY', 'posCardCapture', 'GREATER_THAN', '2', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 4: PIN/CVV VERIFICATION (3 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('PIN_CVV_LIMIT_EXCEEDED', 'Detecta limite de tentativas de PIN/CVV excedido', true, 100, 'FRAUD', 'COMPARISON', 'cvvPinTryLimitExceeded', 'EQUALS', '1', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('OFFLINE_PIN_FAILED', 'Detecta falha de PIN offline', true, 100, 'FRAUD', 'COMPLEX', 'cvrofflinePinVerificationPerformed', 'EQUALS', '1', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('MISSING_CVV2_HIGH_RISK', 'Detecta CVV2 ausente em transação de alto risco (MCC alto risco + valor > 1000)', true, 40, 'SUSPICIOUS', 'COMPLEX', 'cvv2Present', 'EQUALS', '0', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 5: CUSTOM INDICATORS (1 Regra)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('CUSTOM_INDICATOR_FRAUD', 'Detecta indicadores customizados como flags de fraude (userIndicator01=F, BLOCKED, FRAUD)', true, 100, 'FRAUD', 'COMPLEX', 'userIndicator01', 'EQUALS', 'F', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 6: TEMPORAL ADVANCED (2 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('PROCESSING_LAG_ANOMALY', 'Detecta lag de processamento anômalo (> 60 min com valor > 5000)', true, 35, 'SUSPICIOUS', 'COMPLEX', 'recordCreationTime', 'LAG_GREATER_THAN', '60', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('TIMEZONE_NORMALIZED_CHECK', 'Normaliza transações por timezone (madrugada fora do fuso esperado)', true, 30, 'SUSPICIOUS', 'COMPLEX', 'gmtOffset', 'NOT_IN', '-03.00,-02.00', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 7: UNIQUE IDENTIFIERS (3 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('DUPLICATE_TRANSACTION', 'Detecta transações duplicadas', true, 100, 'FRAUD', 'VELOCITY', 'externalTransactionId', 'DUPLICATE', '0', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('SUSPICIOUS_MERCHANT_POSTAL', 'Detecta CEP do merchant inválido (null, 000000, vazio)', true, 25, 'SUSPICIOUS', 'VALIDATION', 'merchantPostalCode', 'INVALID_POSTAL', 'null', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('SUSPICIOUS_TOKEN', 'Detecta token suspeito (TEST, DEMO) com valor > 1000', true, 30, 'SUSPICIOUS', 'COMPLEX', 'tokenId', 'CONTAINS', 'TEST,DEMO', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 8: CURRENCY & CONVERSION (2 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('UNEXPECTED_CURRENCY', 'Detecta moeda não esperada (não BRL em merchant BR com valor > 1000)', true, 30, 'SUSPICIOUS', 'COMPLEX', 'transactionCurrencyCode', 'NOT_EQUALS', '986', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('ANOMALOUS_CONVERSION_RATE', 'Detecta taxa de conversão anômala (desvio > 10% da média)', true, 30, 'SUSPICIOUS', 'COMPLEX', 'transactionCurrencyConversionRate', 'DEVIATION_GREATER_THAN', '0.1', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 9: AUTH SEQUENCE (1 Regra)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('INCOHERENT_AUTH_SEQUENCE', 'Detecta sequência de autenticação incoerente (cryptogram válido mas CVV inválido, etc)', true, 35, 'SUSPICIOUS', 'COMPLEX', 'cryptogramValid', 'INCOHERENT_AUTH', 'V', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 10: CONTEXT COHERENCE (1 Regra)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('INCOHERENT_CONTEXT', 'Detecta contexto incoerente (e-commerce com cliente presente, ATM com e-commerce, chip sem criptograma)', true, 35, 'SUSPICIOUS', 'COMPLEX', 'posEntryMode', 'INCOHERENT_CONTEXT', 'E', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 11: AUTHORIZATION CONTRADICTION (1 Regra)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('CONTRADICTORY_AUTHORIZATION', 'Detecta autorização contraditória (aprovado mas response diferente, flag mas valor zero, aprovado sem authId)', true, 35, 'SUSPICIOUS', 'COMPLEX', 'authDecisionCode', 'CONTRADICTORY_AUTH', 'A', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- GRUPO 12: ACQUIRER PATTERN (2 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('SUSPICIOUS_ACQUIRER', 'Detecta adquirente suspeito (país não BR/US/JP com valor > 10000)', true, 35, 'SUSPICIOUS', 'COMPLEX', 'acquirerCountry', 'NOT_IN', '076,840,392', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('ACQUIRER_COUNTRY_MISMATCH', 'Detecta mismatch entre país do adquirente e merchant com valor > 5000', true, 30, 'SUSPICIOUS', 'COMPLEX', 'acquirerCountry', 'NOT_EQUALS_FIELD', 'merchantCountryCode', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- REGRAS CONSOLIDADAS (3 Regras)
INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('COMBINED_SCORE_CHECK', 'Consolidação de múltiplas regras de score (média consumerAuthenticationScore + externalScore3)', true, 50, 'SUSPICIOUS', 'COMPLEX', 'consumerAuthenticationScore', 'COMBINED_SCORE_BELOW', '200', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('VELOCITY_CHECK_CONSOLIDATED', 'Consolidação de múltiplas regras de velocidade (3 em 5min=FRAUD, 10 em 1h=SUSPICIOUS, 50 em 24h=SUSPICIOUS)', true, 60, 'SUSPICIOUS', 'VELOCITY', 'customerIdFromHeader', 'VELOCITY_CONSOLIDATED', '3,10,50', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

INSERT INTO rule_configurations (rule_name, description, enabled, weight, classification, rule_type, field_name, operator, threshold_value, created_at, updated_at)
VALUES 
('CUSTOM_INDICATORS_COMPREHENSIVE', 'Análise abrangente de indicadores customizados (userIndicator01-08 com flags BLOCK, FRAUD, ALERT, RISK)', true, 100, 'FRAUD', 'COMPLEX', 'userIndicator01', 'COMPREHENSIVE_INDICATORS', 'F,BLOCK,FRAUD,ALERT,RISK', NOW(), NOW())
ON CONFLICT (rule_name) DO UPDATE SET description = EXCLUDED.description, updated_at = NOW();

-- Criar índice para otimizar queries de regras por tipo
CREATE INDEX IF NOT EXISTS idx_rule_configurations_rule_type ON rule_configurations(rule_type);
CREATE INDEX IF NOT EXISTS idx_rule_configurations_enabled_classification ON rule_configurations(enabled, classification);

-- Comentário de auditoria
COMMENT ON TABLE rule_configurations IS 'Tabela unificada de regras de fraude - V27 migrou 28 regras hardcoded do AdvancedRuleEngineService';
