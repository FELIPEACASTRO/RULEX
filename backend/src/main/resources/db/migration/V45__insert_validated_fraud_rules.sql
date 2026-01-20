-- ============================================================================
-- Migration V37: Regras de Fraude VALIDADAS (Apenas Campos do Payload)
-- Data: 2026-01-06
-- Objetivo: Implementar 50 regras de fraude usando APENAS campos existentes
--           no TransactionRequest.java (102 campos validados)
-- ============================================================================

-- ============================================================================
-- CATEGORIA 1: REGRAS DE VALOR (10 regras)
-- Campos usados: transactionAmount, availableCredit, cardDelinquentAmount
-- ============================================================================

INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('VAL001_HIGH_AMOUNT', 'Valor Alto', 'Transação acima de R$ 10.000', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('VAL002_VERY_HIGH_AMOUNT', 'Valor Muito Alto', 'Transação acima de R$ 50.000', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('VAL003_EXTREME_AMOUNT', 'Valor Extremo', 'Transação acima de R$ 100.000', 'AND', 'FRAUDE', 95, 99, 'PUBLISHED', false, NULL),
('VAL004_MICRO_AMOUNT', 'Valor Micro', 'Transação abaixo de R$ 1 (card testing)', 'AND', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('VAL005_ROUND_AMOUNT', 'Valor Redondo Suspeito', 'Transação com valor redondo alto', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('VAL006_EXCEEDS_CREDIT', 'Excede Crédito', 'Transação excede crédito disponível', 'AND', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('VAL007_DELINQUENT_HIGH', 'Inadimplência Alta', 'Cliente com inadimplência alta', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('VAL008_STRUCTURING_9999', 'Structuring 9999', 'Valor próximo ao limite de reporte', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('VAL009_ODD_CENTS', 'Centavos Estranhos', 'Valor com centavos incomuns', 'AND', 'SUSPEITA_DE_FRAUDE', 45, 50, 'PUBLISHED', false, NULL),
('VAL010_ZERO_AMOUNT', 'Valor Zero', 'Transação com valor zero', 'AND', 'FRAUDE', 95, 99, 'PUBLISHED', false, NULL);

-- Condições para VAL001
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL001_HIGH_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '10000', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL001_HIGH_AMOUNT';

-- Condições para VAL002
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL002_VERY_HIGH_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '50000', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL002_VERY_HIGH_AMOUNT';

-- Condições para VAL003
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL003_EXTREME_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '100000', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL003_EXTREME_AMOUNT';

-- Condições para VAL004
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL004_MICRO_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'LT', '1', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL004_MICRO_AMOUNT';

-- Condições para VAL006
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL006_EXCEEDS_CREDIT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_field_ref, position)
SELECT rcg.id, 'transactionAmount', 'FIELD_GT', 'availableCredit', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL006_EXCEEDS_CREDIT';

-- Condições para VAL007
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL007_DELINQUENT_HIGH';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cardDelinquentAmount', 'GT', '1000', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL007_DELINQUENT_HIGH';

-- Condições para VAL008
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL008_STRUCTURING_9999';
INSERT INTO rule_conditions (group_id, field_name, operator, value_min, value_max, position)
SELECT rcg.id, 'transactionAmount', 'BETWEEN', '9000', '9999', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL008_STRUCTURING_9999';

-- Condições para VAL010
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'VAL010_ZERO_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'EQ', '0', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'VAL010_ZERO_AMOUNT';

-- ============================================================================
-- CATEGORIA 2: REGRAS DE MCC (10 regras)
-- Campos usados: mcc
-- ============================================================================

INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('MCC001_GIFT_CARDS', 'MCC Gift Cards', 'Transação em MCC de gift cards (5944)', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('MCC002_CRYPTO', 'MCC Cripto', 'Transação em MCC de criptomoedas (6051)', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('MCC003_GAMBLING', 'MCC Apostas', 'Transação em MCC de apostas (7995)', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('MCC004_JEWELRY', 'MCC Joias', 'Transação em MCC de joias (5944)', 'AND', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('MCC005_WIRE_TRANSFER', 'MCC Wire Transfer', 'Transação em MCC de transferência (4829)', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('MCC006_MONEY_ORDER', 'MCC Money Order', 'Transação em MCC de ordem de pagamento (6050)', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('MCC007_PAWN_SHOP', 'MCC Casa de Penhores', 'Transação em MCC de casa de penhores (5933)', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('MCC008_DATING', 'MCC Dating', 'Transação em MCC de dating (5968)', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('MCC009_ADULT', 'MCC Adult', 'Transação em MCC de conteúdo adulto (5967)', 'AND', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('MCC010_HIGH_RISK_COMBO', 'MCC Alto Risco Combo', 'Transação em MCCs de alto risco combinados', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

-- Condições para MCC001-MCC009 (IN list)
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'MCC001_GIFT_CARDS';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'EQ', '5944', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'MCC001_GIFT_CARDS';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'MCC002_CRYPTO';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'EQ', '6051', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'MCC002_CRYPTO';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'MCC003_GAMBLING';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'EQ', '7995', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'MCC003_GAMBLING';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'MCC005_WIRE_TRANSFER';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'EQ', '4829', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'MCC005_WIRE_TRANSFER';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'MCC006_MONEY_ORDER';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'EQ', '6050', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'MCC006_MONEY_ORDER';

-- ============================================================================
-- CATEGORIA 3: REGRAS DE PAÍS (10 regras)
-- Campos usados: merchantCountryCode, acquirerCountry
-- ============================================================================

INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('GEO001_HIGH_RISK_COUNTRY', 'País Alto Risco', 'Transação em país de alto risco', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('GEO002_NIGERIA', 'País Nigéria', 'Transação na Nigéria', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('GEO003_RUSSIA', 'País Rússia', 'Transação na Rússia', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('GEO004_UKRAINE', 'País Ucrânia', 'Transação na Ucrânia', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('GEO005_NORTH_KOREA', 'País Coreia do Norte', 'Transação na Coreia do Norte', 'AND', 'FRAUDE', 99, 99, 'PUBLISHED', false, NULL),
('GEO006_IRAN', 'País Irã', 'Transação no Irã', 'AND', 'FRAUDE', 99, 99, 'PUBLISHED', false, NULL),
('GEO007_CUBA', 'País Cuba', 'Transação em Cuba', 'AND', 'FRAUDE', 95, 99, 'PUBLISHED', false, NULL),
('GEO008_SYRIA', 'País Síria', 'Transação na Síria', 'AND', 'FRAUDE', 99, 99, 'PUBLISHED', false, NULL),
('GEO009_VENEZUELA', 'País Venezuela', 'Transação na Venezuela', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('GEO010_CROSS_BORDER', 'Cross-Border', 'Transação cross-border (acquirer != merchant)', 'AND', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL);

-- Condições para GEO002-GEO009
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO002_NIGERIA';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountryCode', 'EQ', 'NG', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO002_NIGERIA';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO003_RUSSIA';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountryCode', 'EQ', 'RU', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO003_RUSSIA';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO005_NORTH_KOREA';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountryCode', 'EQ', 'KP', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO005_NORTH_KOREA';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO006_IRAN';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountryCode', 'EQ', 'IR', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO006_IRAN';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO007_CUBA';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountryCode', 'EQ', 'CU', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO007_CUBA';

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO008_SYRIA';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountryCode', 'EQ', 'SY', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO008_SYRIA';

-- Cross-border: acquirerCountry != merchantCountryCode
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'GEO010_CROSS_BORDER';
INSERT INTO rule_conditions (group_id, field_name, operator, value_field_ref, position)
SELECT rcg.id, 'acquirerCountry', 'FIELD_NEQ', 'merchantCountryCode', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'GEO010_CROSS_BORDER';

-- ============================================================================
-- CATEGORIA 4: REGRAS DE CVV/AVS (10 regras)
-- Campos usados: cvv2Response, cvv2Present, avsRequest, cvvVerifyCode
-- ============================================================================

INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('CVV001_CVV_FAIL', 'CVV Falhou', 'CVV não corresponde', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('CVV002_CVV_NOT_PRESENT', 'CVV Ausente', 'CVV não fornecido', 'AND', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('CVV003_CVV_FAIL_HIGH_AMOUNT', 'CVV Falhou + Alto Valor', 'CVV falhou em transação de alto valor', 'AND', 'FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('CVV004_AVS_FAIL', 'AVS Falhou', 'AVS não corresponde', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('CVV005_CVV_AVS_BOTH_FAIL', 'CVV e AVS Falharam', 'Ambos CVV e AVS falharam', 'AND', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('CVV006_NO_VERIFICATION', 'Sem Verificação', 'Transação sem CVV nem AVS', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('CVV007_CVV_ISSUER_UNABLE', 'CVV Issuer Unable', 'Emissor não conseguiu verificar CVV', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('CVV008_CVV_NOT_PROCESSED', 'CVV Não Processado', 'CVV não foi processado', 'AND', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('CVV009_PIN_FAIL', 'PIN Falhou', 'Verificação de PIN falhou', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('CVV010_PIN_LIMIT_EXCEEDED', 'PIN Limite Excedido', 'Limite de tentativas de PIN excedido', 'AND', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL);

-- Condições para CVV001
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'CVV001_CVV_FAIL';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cvv2Response', 'EQ', 'N', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'CVV001_CVV_FAIL';

-- Condições para CVV002
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'CVV002_CVV_NOT_PRESENT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cvv2Present', 'EQ', 'N', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'CVV002_CVV_NOT_PRESENT';

-- Condições para CVV003 (CVV fail + high amount)
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'CVV003_CVV_FAIL_HIGH_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cvv2Response', 'EQ', 'N', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'CVV003_CVV_FAIL_HIGH_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '5000', 1
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'CVV003_CVV_FAIL_HIGH_AMOUNT';

-- Condições para CVV010
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'CVV010_PIN_LIMIT_EXCEEDED';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cvvPinTryLimitExceeded', 'EQ', '1', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'CVV010_PIN_LIMIT_EXCEEDED';

-- ============================================================================
-- CATEGORIA 5: REGRAS DE TERMINAL/POS (10 regras)
-- Campos usados: terminalType, posEntryMode, customerPresent, posConditionCode
-- ============================================================================

INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('POS001_ECOM', 'Terminal E-Commerce', 'Transação e-commerce', 'AND', 'SUSPEITA_DE_FRAUDE', 40, 45, 'PUBLISHED', false, NULL),
('POS002_ECOM_HIGH_AMOUNT', 'E-Commerce Alto Valor', 'E-commerce com valor alto', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('POS003_CNP', 'Card Not Present', 'Transação sem cartão presente', 'AND', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('POS004_MANUAL_ENTRY', 'Entrada Manual', 'Dados do cartão digitados manualmente', 'AND', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('POS005_FALLBACK', 'Fallback Magnético', 'Fallback para tarja magnética', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('POS006_MOTO', 'MOTO', 'Transação Mail Order/Telephone Order', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('POS007_RECURRING', 'Recorrente', 'Transação recorrente', 'AND', 'SUSPEITA_DE_FRAUDE', 35, 40, 'PUBLISHED', false, NULL),
('POS008_INSTALLMENT', 'Parcelamento', 'Transação parcelada', 'AND', 'SUSPEITA_DE_FRAUDE', 30, 35, 'PUBLISHED', false, NULL),
('POS009_OFF_PREMISES', 'Off Premises', 'Terminal fora do estabelecimento', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('POS010_NO_SECURITY', 'Sem Segurança POS', 'POS sem recursos de segurança', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL);

-- Condições para POS001
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'POS001_ECOM';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'terminalType', 'EQ', 'ECOM', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'POS001_ECOM';

-- Condições para POS002 (ECOM + high amount)
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'POS002_ECOM_HIGH_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'terminalType', 'EQ', 'ECOM', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'POS002_ECOM_HIGH_AMOUNT';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '3000', 1
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'POS002_ECOM_HIGH_AMOUNT';

-- Condições para POS003
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'POS003_CNP';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'customerPresent', 'EQ', 'N', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'POS003_CNP';

-- Condições para POS009
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'POS009_OFF_PREMISES';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'posOffPremises', 'EQ', '1', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'POS009_OFF_PREMISES';

-- Condições para POS010
INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'POS010_NO_SECURITY';
INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'posSecurity', 'EQ', '0', 0
FROM rule_condition_groups rcg JOIN complex_rules cr ON rcg.complex_rule_id = cr.id WHERE cr.key = 'POS010_NO_SECURITY';

-- ============================================================================
-- FIM DA MIGRATION V37
-- Total de regras criadas: 50 regras VALIDADAS usando apenas campos do payload
-- ============================================================================
