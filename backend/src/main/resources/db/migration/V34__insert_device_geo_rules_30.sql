-- ============================================================================
-- Migration V34: 30 Regras de DEVICE FINGERPRINT e GEOLOCALIZAÇÃO
-- Data: 2026-01-06
-- Objetivo: Implementar 30 regras de detecção de fraude baseadas em
--           device fingerprinting, geolocalização e análise de IP
-- ============================================================================

-- ============================================================================
-- CATEGORIA 1: DEVICE FINGERPRINT (15 regras)
-- ============================================================================

-- D001: Novo device + alto valor
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('D001_NEW_DEVICE_HIGH_VALUE', 'Novo Device Alto Valor', 'Transação > R$ 2.000 de novo dispositivo', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'D001_NEW_DEVICE_HIGH_VALUE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'deviceId', 'IS_NEW', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'D001_NEW_DEVICE_HIGH_VALUE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'transactionAmount', 'GT', '2000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'D001_NEW_DEVICE_HIGH_VALUE';

-- D002: Device compartilhado entre múltiplas contas
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('D002_SHARED_DEVICE', 'Device Compartilhado', 'Device usado por múltiplas contas', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'D002_SHARED_DEVICE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'deviceAccountCount', 'GT', '1', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'D002_SHARED_DEVICE';

-- D003: Device com histórico de fraude
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('D003_FRAUD_DEVICE', 'Device com Histórico de Fraude', 'Device associado a fraude anterior', 'AND', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'D003_FRAUD_DEVICE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'deviceFraudHistory', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'D003_FRAUD_DEVICE';

-- D004-D015: Mais regras de device fingerprint
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES 
('D004_EMULATOR_DETECTED', 'Emulador Detectado', 'Transação de emulador de dispositivo', 'AND', 'BLOCK', 90, 95, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D005_ROOTED_DEVICE', 'Device Rooted/Jailbroken', 'Transação de dispositivo rooted ou jailbroken', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D006_VPN_DETECTED', 'VPN Detectada', 'Transação via VPN', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D007_PROXY_DETECTED', 'Proxy Detectado', 'Transação via proxy', 'AND', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D008_TOR_DETECTED', 'TOR Detectado', 'Transação via rede TOR', 'AND', 'BLOCK', 90, 95, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D009_DEVICE_AGE_LOW', 'Device Muito Novo', 'Device com menos de 24h de uso', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D010_BROWSER_MISMATCH', 'Browser Mismatch', 'User-agent inconsistente com fingerprint', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D011_TIMEZONE_MISMATCH', 'Timezone Mismatch', 'Timezone inconsistente com geolocalização', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D012_LANGUAGE_MISMATCH', 'Language Mismatch', 'Idioma do browser inconsistente com país', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D013_SCREEN_ANOMALY', 'Screen Anomaly', 'Resolução de tela anômala', 'AND', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D014_DEVICE_VELOCITY', 'Device Velocity', 'Múltiplos cartões usados no mesmo device', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('D015_DEVICE_BLACKLIST', 'Device Blacklist', 'Device na lista negra', 'AND', 'BLOCK', 98, 99, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

-- ============================================================================
-- CATEGORIA 2: GEOLOCALIZAÇÃO (15 regras)
-- ============================================================================

-- G001: Viagem impossível
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('G001_IMPOSSIBLE_TRAVEL', 'Viagem Impossível', 'Transação em localização fisicamente impossível', 'AND', 'BLOCK', 98, 99, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'G001_IMPOSSIBLE_TRAVEL';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'impossibleTravel', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'G001_IMPOSSIBLE_TRAVEL';

-- G002: País de alto risco
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('G002_HIGH_RISK_COUNTRY', 'País de Alto Risco', 'Transação de país de alto risco', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'G002_HIGH_RISK_COUNTRY';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'merchantCountry', 'IN_LIST', 'NG,RU,UA,BY,IR,KP,SY,VE,MM,AF', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'G002_HIGH_RISK_COUNTRY';

-- G003: Nova geolocalização + alto valor
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('G003_NEW_GEO_HIGH_VALUE', 'Nova Geo Alto Valor', 'Transação > R$ 3.000 de nova localização', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'G003_NEW_GEO_HIGH_VALUE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'geoLocation', 'IS_NEW', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'G003_NEW_GEO_HIGH_VALUE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'transactionAmount', 'GT', '3000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'G003_NEW_GEO_HIGH_VALUE';

-- G004-G015: Mais regras de geolocalização
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES 
('G004_IP_COUNTRY_MISMATCH', 'IP Country Mismatch', 'País do IP diferente do país do cartão', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G005_BILLING_SHIPPING_MISMATCH', 'Billing Shipping Mismatch', 'País de cobrança diferente do país de entrega', 'AND', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G006_CROSS_BORDER', 'Cross Border', 'Transação internacional', 'AND', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G007_CROSS_BORDER_HIGH_VALUE', 'Cross Border Alto Valor', 'Transação internacional > R$ 5.000', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G008_SANCTIONED_COUNTRY', 'País Sancionado', 'Transação de país sob sanção', 'AND', 'BLOCK', 98, 99, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G009_IP_BLACKLIST', 'IP Blacklist', 'IP na lista negra', 'AND', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G010_IP_DATACENTER', 'IP Datacenter', 'IP de datacenter/hosting', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G011_GEO_VELOCITY', 'Geo Velocity', 'Múltiplas localizações em curto período', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G012_DISTANCE_FROM_HOME', 'Distância de Casa', 'Transação > 500km do endereço cadastrado', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G013_FIRST_INTERNATIONAL', 'Primeira Internacional', 'Primeira transação internacional do cliente', 'AND', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G014_TIMEZONE_ANOMALY', 'Timezone Anomaly', 'Transação em timezone incomum para o cliente', 'AND', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34'),
('G015_GEO_BLACKLIST', 'Geo Blacklist', 'Geolocalização na lista negra', 'AND', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V34');

-- ============================================================================
-- FIM DA MIGRATION V34
-- ============================================================================

-- Total de regras criadas: 30 regras de DEVICE FINGERPRINT e GEOLOCALIZAÇÃO
-- Categorias cobertas:
--   - Device Fingerprint: 15 regras (D001-D015)
--   - Geolocalização: 15 regras (G001-G015)
