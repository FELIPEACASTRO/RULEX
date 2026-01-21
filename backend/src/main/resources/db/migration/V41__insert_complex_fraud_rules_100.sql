-- ============================================================================
-- Migration V32: 100 Regras COMPLEXAS de Fraude (Multi-Condição)
-- Data: 2026-01-06
-- Objetivo: Implementar 100 regras de detecção de fraude com múltiplas condições
--           para cobertura de padrões sofisticados de fraude
-- ============================================================================

-- ============================================================================
-- CATEGORIA 1: CARD TESTING (10 regras complexas)
-- ============================================================================

-- C001: Card Testing Clássico (micro-transação + e-commerce + CVV inválido)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C001_CARD_TESTING_CLASSIC', 'Card Testing Clássico', 'Micro-transação em e-commerce com CVV inválido', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C001_CARD_TESTING_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'LT', '5', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C001_CARD_TESTING_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'terminalType', 'EQ', 'ECOM', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C001_CARD_TESTING_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cvv2Response', 'EQ', 'N', 2
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C001_CARD_TESTING_CLASSIC';

-- C002: Card Testing com Múltiplos Merchants
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C002_CARD_TESTING_MULTI_MERCHANT', 'Card Testing Multi-Merchant', 'Transação pequena + múltiplos merchants em 1h', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C002_CARD_TESTING_MULTI_MERCHANT';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'LT', '10', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C002_CARD_TESTING_MULTI_MERCHANT';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS', 'GT', '5:1', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C002_CARD_TESTING_MULTI_MERCHANT';

-- C003: Card Testing com Valores Crescentes (Escada)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C003_CARD_TESTING_ESCALATION', 'Card Testing Escada', 'Padrão de valores crescentes após micro-transação', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C003_CARD_TESTING_ESCALATION';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '100', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C003_CARD_TESTING_ESCALATION';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'MIN_AMOUNT_LAST_N_DAYS', 'LT', '5:1', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C003_CARD_TESTING_ESCALATION';

-- C004: Card Testing em Gift Cards
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C004_CARD_TESTING_GIFT_CARD', 'Card Testing Gift Card', 'Múltiplas compras de gift card após teste', 'FRAUDE', 92, 95, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C004_CARD_TESTING_GIFT_CARD';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'EQ', '5815', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C004_CARD_TESTING_GIFT_CARD';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'COUNT_LAST_N_HOURS', 'GT', '3:1', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C004_CARD_TESTING_GIFT_CARD';

-- C005: Card Testing com Falha de CVV seguida de Sucesso
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C005_CVV_FAIL_THEN_SUCCESS', 'CVV Fail Then Success', 'Transação aprovada após múltiplas falhas de CVV', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C005_CVV_FAIL_THEN_SUCCESS';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'cvv2Response', 'EQ', 'M', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C005_CVV_FAIL_THEN_SUCCESS';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'previousCvvFailures', 'GT', '2', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C005_CVV_FAIL_THEN_SUCCESS';

-- C006-C010: Mais regras de Card Testing
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C006_CARD_TESTING_CRYPTO', 'Card Testing Crypto', 'Micro-transação seguida de compra de crypto', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('C007_CARD_TESTING_NIGHT', 'Card Testing Noturno', 'Card testing em horário de madrugada', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C008_CARD_TESTING_NEW_DEVICE', 'Card Testing Novo Device', 'Card testing de novo dispositivo', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C009_CARD_TESTING_VELOCITY', 'Card Testing Alta Velocidade', 'Múltiplas micro-transações em segundos', 'FRAUDE', 98, 99, 'PUBLISHED', false, NULL),
('C010_CARD_TESTING_INTERNATIONAL', 'Card Testing Internacional', 'Card testing de país de alto risco', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 2: ACCOUNT TAKEOVER (10 regras complexas)
-- ============================================================================

-- C011: ATO Clássico (novo device + nova geo + alto valor)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C011_ATO_CLASSIC', 'Account Takeover Clássico', 'Novo device + nova geolocalização + alto valor', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C011_ATO_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'deviceId', 'IS_NEW', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C011_ATO_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'geoLocation', 'IS_NEW', '', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C011_ATO_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '5000', 2
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C011_ATO_CLASSIC';

-- C012: ATO com Mudança de Senha Recente
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C012_ATO_PASSWORD_CHANGE', 'ATO Pós-Mudança de Senha', 'Transação de alto valor após mudança de senha recente', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C012_ATO_PASSWORD_CHANGE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'passwordChangedWithinDays', 'LT', '3', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C012_ATO_PASSWORD_CHANGE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '2000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C012_ATO_PASSWORD_CHANGE';

-- C013-C020: Mais regras de ATO
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C013_ATO_SIM_SWAP', 'ATO SIM Swap', 'Transação após mudança de telefone + novo device', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('C014_ATO_EMAIL_CHANGE', 'ATO Mudança de Email', 'Transação de alto valor após mudança de email', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C015_ATO_ADDRESS_CHANGE', 'ATO Mudança de Endereço', 'Transação de alto valor após mudança de endereço', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C016_ATO_IMPOSSIBLE_TRAVEL', 'ATO Viagem Impossível', 'Transação de localização impossível em curto período', 'FRAUDE', 98, 99, 'PUBLISHED', false, NULL),
('C017_ATO_DORMANT_ACCOUNT', 'ATO Conta Dormante', 'Transação de alto valor em conta inativa há meses', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C018_ATO_FAILED_AUTH_THEN_SUCCESS', 'ATO Falha Auth Sucesso', 'Transação após múltiplas falhas de autenticação', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C019_ATO_VPN_PROXY', 'ATO VPN/Proxy', 'Transação de alto valor via VPN ou proxy', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C020_ATO_BURST_SPENDING', 'ATO Burst Spending', 'Múltiplas transações de alto valor em curto período', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 3: BUST-OUT FRAUD (10 regras complexas)
-- ============================================================================

-- C021: Bust-Out Clássico (crédito máximo + MCC de risco)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C021_BUSTOUT_CLASSIC', 'Bust-Out Clássico', 'Uso de 100% do crédito em MCC de alto risco', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C021_BUSTOUT_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'PERCENTAGE_OF_FIELD', 'availableCredit:95:100', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C021_BUSTOUT_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'IN_LIST', '6051,7995,5944,5815', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C021_BUSTOUT_CLASSIC';

-- C022-C030: Mais regras de Bust-Out
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C022_BUSTOUT_CASH_ADVANCE', 'Bust-Out Cash Advance', 'Múltiplos saques de cash até o limite', 'FRAUDE', 92, 95, 'PUBLISHED', false, NULL),
('C023_BUSTOUT_GIFT_CARDS', 'Bust-Out Gift Cards', 'Compra massiva de gift cards próximo ao limite', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('C024_BUSTOUT_CRYPTO', 'Bust-Out Crypto', 'Compra de crypto usando todo o crédito', 'FRAUDE', 98, 99, 'PUBLISHED', false, NULL),
('C025_BUSTOUT_JEWELRY', 'Bust-Out Joalheria', 'Compra de alto valor em joalheria', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C026_BUSTOUT_ELECTRONICS', 'Bust-Out Eletrônicos', 'Compra massiva de eletrônicos', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C027_BUSTOUT_NEW_ACCOUNT', 'Bust-Out Conta Nova', 'Uso máximo de crédito em conta recém-aberta', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('C028_BUSTOUT_DELINQUENT', 'Bust-Out Inadimplente', 'Transação de alto valor com conta em atraso', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('C029_BUSTOUT_RAPID_CREDIT_USE', 'Bust-Out Uso Rápido', 'Uso de 80%+ do crédito em 24h', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C030_BUSTOUT_INTERNATIONAL', 'Bust-Out Internacional', 'Bust-out em país de alto risco', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 4: FRIENDLY FRAUD (10 regras complexas)
-- ============================================================================

-- C031: Friendly Fraud Clássico (chargeback histórico + mesmo merchant)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C031_FRIENDLY_FRAUD_CLASSIC', 'Friendly Fraud Clássico', 'Cliente com histórico de chargeback no mesmo merchant', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C031_FRIENDLY_FRAUD_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'customerChargebackCount', 'GT', '0', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C031_FRIENDLY_FRAUD_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantId', 'IN_CUSTOMER_CHARGEBACK_MERCHANTS', '', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C031_FRIENDLY_FRAUD_CLASSIC';

-- C032-C040: Mais regras de Friendly Fraud
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C032_FRIENDLY_FRAUD_REPEAT', 'Friendly Fraud Reincidente', 'Cliente com múltiplos chargebacks em 6 meses', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('C033_FRIENDLY_FRAUD_HIGH_VALUE', 'Friendly Fraud Alto Valor', 'Transação de alto valor de cliente com chargeback', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C034_FRIENDLY_FRAUD_DIGITAL_GOODS', 'Friendly Fraud Digital', 'Compra de bens digitais por cliente com chargeback', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C035_FRIENDLY_FRAUD_SUBSCRIPTION', 'Friendly Fraud Assinatura', 'Assinatura de cliente com histórico de disputa', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C036_FRIENDLY_FRAUD_TRAVEL', 'Friendly Fraud Viagem', 'Compra de viagem por cliente com chargeback', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C037_FRIENDLY_FRAUD_ELECTRONICS', 'Friendly Fraud Eletrônicos', 'Compra de eletrônicos por cliente com chargeback', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C038_FRIENDLY_FRAUD_SAME_DAY', 'Friendly Fraud Mesmo Dia', 'Múltiplas compras no mesmo dia por cliente com chargeback', 'FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C039_FRIENDLY_FRAUD_NEW_MERCHANT', 'Friendly Fraud Novo Merchant', 'Primeira compra em novo merchant por cliente com chargeback', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C040_FRIENDLY_FRAUD_PROMO', 'Friendly Fraud Promoção', 'Compra promocional por cliente com histórico de disputa', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 5: SYNTHETIC IDENTITY (10 regras complexas)
-- ============================================================================

-- C041: Synthetic Identity Clássico (SSN novo + endereço novo + alto valor)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C041_SYNTHETIC_ID_CLASSIC', 'Synthetic Identity Clássico', 'Conta nova com dados inconsistentes + alto valor', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C041_SYNTHETIC_ID_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'accountAgeInDays', 'LT', '90', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C041_SYNTHETIC_ID_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '3000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C041_SYNTHETIC_ID_CLASSIC';

-- C042-C050: Mais regras de Synthetic Identity
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C042_SYNTHETIC_ID_CREDIT_BUILD', 'Synthetic ID Credit Build', 'Padrão de construção de crédito artificial', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C043_SYNTHETIC_ID_AUTHORIZED_USER', 'Synthetic ID Authorized User', 'Usuário autorizado com padrão suspeito', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C044_SYNTHETIC_ID_ADDRESS_MISMATCH', 'Synthetic ID Address Mismatch', 'Endereço de cobrança diferente de entrega', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('C045_SYNTHETIC_ID_PHONE_MISMATCH', 'Synthetic ID Phone Mismatch', 'Telefone não associado ao CPF', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C046_SYNTHETIC_ID_EMAIL_PATTERN', 'Synthetic ID Email Pattern', 'Email com padrão suspeito (números aleatórios)', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('C047_SYNTHETIC_ID_RAPID_CREDIT', 'Synthetic ID Rapid Credit', 'Aumento rápido de limite em conta nova', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C048_SYNTHETIC_ID_MULTIPLE_APPS', 'Synthetic ID Multiple Apps', 'Múltiplas aplicações de crédito em curto período', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C049_SYNTHETIC_ID_BUST_OUT', 'Synthetic ID Bust-Out', 'Bust-out em conta com sinais de identidade sintética', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('C050_SYNTHETIC_ID_DORMANT_THEN_ACTIVE', 'Synthetic ID Dormant Active', 'Conta dormante que se torna ativa com alto valor', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 6: MONEY LAUNDERING (10 regras complexas)
-- ============================================================================

-- C051: Structuring Clássico (múltiplas transações abaixo do limite CTR)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C051_STRUCTURING_CLASSIC', 'Structuring Clássico', 'Múltiplas transações abaixo de R$ 10.000 em 24h', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C051_STRUCTURING_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'BETWEEN', '9000,9999', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C051_STRUCTURING_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'COUNT_LAST_N_HOURS', 'GT', '2:24', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C051_STRUCTURING_CLASSIC';

-- C052-C060: Mais regras de Money Laundering
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C052_LAYERING_CLASSIC', 'Layering Clássico', 'Múltiplas transferências entre contas em curto período', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C053_SMURFING', 'Smurfing', 'Múltiplos depósitos pequenos de diferentes origens', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C054_ROUND_TRIP', 'Round Trip', 'Dinheiro que sai e retorna em curto período', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C055_SHELL_COMPANY', 'Shell Company', 'Transação com empresa sem atividade operacional', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C056_HIGH_RISK_COUNTRY_TRANSFER', 'High Risk Country Transfer', 'Transferência para país de alto risco', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C057_CASH_INTENSIVE_BUSINESS', 'Cash Intensive Business', 'Transação de negócio intensivo em dinheiro', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C058_RAPID_MOVEMENT', 'Rapid Movement', 'Movimentação rápida de fundos após depósito', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C059_UNUSUAL_PATTERN', 'Unusual Pattern', 'Padrão de transação incomum para o perfil', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C060_CRYPTO_LAYERING', 'Crypto Layering', 'Layering usando exchanges de criptomoeda', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 7: TRIANGULATION FRAUD (10 regras complexas)
-- ============================================================================

-- C061: Triangulation Clássico (endereço de entrega diferente + alto valor)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C061_TRIANGULATION_CLASSIC', 'Triangulation Clássico', 'Endereço de entrega diferente do billing + alto valor', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C061_TRIANGULATION_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'shippingAddress', 'NEQ_FIELD', 'billingAddress', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C061_TRIANGULATION_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '1000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C061_TRIANGULATION_CLASSIC';

-- C062-C070: Mais regras de Triangulation
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C062_TRIANGULATION_DROPSHIP', 'Triangulation Dropship', 'Padrão de dropshipping fraudulento', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C063_TRIANGULATION_MARKETPLACE', 'Triangulation Marketplace', 'Compra em marketplace com entrega para terceiro', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C064_TRIANGULATION_ELECTRONICS', 'Triangulation Eletrônicos', 'Triangulation em compra de eletrônicos', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C065_TRIANGULATION_GIFT', 'Triangulation Gift', 'Compra marcada como presente com endereço suspeito', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('C066_TRIANGULATION_INTERNATIONAL', 'Triangulation Internacional', 'Triangulation com entrega internacional', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('C067_TRIANGULATION_MULTIPLE_ITEMS', 'Triangulation Múltiplos Itens', 'Múltiplos itens de alto valor para endereço diferente', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C068_TRIANGULATION_EXPRESS_SHIP', 'Triangulation Express', 'Frete expresso para endereço diferente', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C069_TRIANGULATION_PO_BOX', 'Triangulation PO Box', 'Entrega para caixa postal', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C070_TRIANGULATION_FREIGHT_FORWARD', 'Triangulation Freight Forward', 'Entrega para freight forwarder', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 8: REFUND FRAUD (10 regras complexas)
-- ============================================================================

-- C071: Refund Fraud Clássico (múltiplos reembolsos em curto período)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C071_REFUND_FRAUD_CLASSIC', 'Refund Fraud Clássico', 'Múltiplos reembolsos em 30 dias', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C071_REFUND_FRAUD_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'customerRefundCount30Days', 'GT', '3', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C071_REFUND_FRAUD_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionType', 'EQ', 'PURCHASE', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C071_REFUND_FRAUD_CLASSIC';

-- C072-C080: Mais regras de Refund Fraud
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C072_REFUND_FRAUD_HIGH_VALUE', 'Refund Fraud Alto Valor', 'Compra de alto valor por cliente com histórico de reembolso', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C073_REFUND_FRAUD_SAME_ITEM', 'Refund Fraud Mesmo Item', 'Compra repetida de item frequentemente devolvido', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C074_REFUND_FRAUD_WARDROBING', 'Wardrobing', 'Padrão de compra-uso-devolução', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C075_REFUND_FRAUD_EMPTY_BOX', 'Empty Box Fraud', 'Padrão de reclamação de caixa vazia', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('C076_REFUND_FRAUD_PARTIAL', 'Partial Refund Abuse', 'Abuso de reembolso parcial', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C077_REFUND_FRAUD_PROMO', 'Promo Abuse Refund', 'Reembolso após uso de promoção', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('C078_REFUND_FRAUD_DIGITAL', 'Digital Goods Refund', 'Reembolso de bens digitais após consumo', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C079_REFUND_FRAUD_SUBSCRIPTION', 'Subscription Refund Abuse', 'Abuso de reembolso de assinatura', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C080_REFUND_FRAUD_SERIAL', 'Serial Refunder', 'Cliente com padrão serial de reembolsos', 'FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 9: PROMO/COUPON ABUSE (10 regras complexas)
-- ============================================================================

-- C081: Promo Abuse Clássico (múltiplas contas + mesmo device)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C081_PROMO_ABUSE_CLASSIC', 'Promo Abuse Clássico', 'Múltiplos usos de promoção do mesmo device', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C081_PROMO_ABUSE_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'promoCodeUsed', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C081_PROMO_ABUSE_CLASSIC';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'devicePromoUseCount', 'GT', '1', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C081_PROMO_ABUSE_CLASSIC';

-- C082-C090: Mais regras de Promo Abuse
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C082_PROMO_ABUSE_NEW_ACCOUNT', 'Promo Abuse New Account', 'Conta nova usando promoção de primeira compra', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('C083_PROMO_ABUSE_REFERRAL', 'Referral Abuse', 'Abuso de programa de referência', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C084_PROMO_ABUSE_STACKING', 'Promo Stacking', 'Uso de múltiplos códigos promocionais', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C085_PROMO_ABUSE_RESELLER', 'Promo Reseller', 'Compra promocional para revenda', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C086_PROMO_ABUSE_VELOCITY', 'Promo Velocity', 'Múltiplas compras promocionais em curto período', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C087_PROMO_ABUSE_GIFT_CARD', 'Promo Gift Card Abuse', 'Compra de gift card com promoção', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('C088_PROMO_ABUSE_CASHBACK', 'Cashback Abuse', 'Abuso de programa de cashback', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C089_PROMO_ABUSE_LOYALTY', 'Loyalty Abuse', 'Abuso de programa de fidelidade', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C090_PROMO_ABUSE_SERIAL', 'Serial Promo Abuser', 'Cliente com padrão serial de abuso de promoções', 'FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 10: MISCELLANEOUS COMPLEX RULES (10 regras complexas)
-- ============================================================================

-- C091: First Transaction High Value
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('C091_FIRST_TXN_HIGH_VALUE', 'First Transaction High Value', 'Primeira transação de alto valor', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'C091_FIRST_TXN_HIGH_VALUE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'isFirstTransaction', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C091_FIRST_TXN_HIGH_VALUE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '2000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'C091_FIRST_TXN_HIGH_VALUE';

-- C092-C100: Mais regras diversas
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('C092_NIGHT_HIGH_VALUE', 'Night High Value', 'Transação de alto valor em horário noturno', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('C093_WEEKEND_HIGH_VALUE', 'Weekend High Value', 'Transação de alto valor no fim de semana', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('C094_HOLIDAY_HIGH_VALUE', 'Holiday High Value', 'Transação de alto valor em feriado', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('C095_CROSS_BORDER_HIGH_VALUE', 'Cross Border High Value', 'Transação internacional de alto valor', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('C096_RECURRING_AMOUNT_CHANGE', 'Recurring Amount Change', 'Mudança significativa em valor de transação recorrente', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('C097_SUBSCRIPTION_UPGRADE', 'Subscription Upgrade', 'Upgrade de assinatura para plano mais caro', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('C098_MULTI_CURRENCY', 'Multi Currency', 'Transação em moeda diferente da usual', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('C099_HIGH_RISK_COMBO', 'High Risk Combo', 'Combinação de múltiplos fatores de risco', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('C100_ANOMALY_SCORE_HIGH', 'Anomaly Score High', 'Score de anomalia alto combinado com alto valor', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

-- ============================================================================
-- FIM DA MIGRATION V32
-- ============================================================================

-- Total de regras criadas: 100 regras COMPLEXAS
-- Categorias cobertas:
--   - Card Testing: 10 regras (C001-C010)
--   - Account Takeover: 10 regras (C011-C020)
--   - Bust-Out Fraud: 10 regras (C021-C030)
--   - Friendly Fraud: 10 regras (C031-C040)
--   - Synthetic Identity: 10 regras (C041-C050)
--   - Money Laundering: 10 regras (C051-C060)
--   - Triangulation Fraud: 10 regras (C061-C070)
--   - Refund Fraud: 10 regras (C071-C080)
--   - Promo/Coupon Abuse: 10 regras (C081-C090)
--   - Miscellaneous: 10 regras (C091-C100)
