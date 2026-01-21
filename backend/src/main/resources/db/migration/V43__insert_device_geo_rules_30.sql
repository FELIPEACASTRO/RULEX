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
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('D001_NEW_DEVICE_HIGH_VALUE', 'Novo Device Alto Valor', 'Transação > R$ 2.000 de novo dispositivo', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'D001_NEW_DEVICE_HIGH_VALUE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'deviceId', 'IS_NEW', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'D001_NEW_DEVICE_HIGH_VALUE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '2000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'D001_NEW_DEVICE_HIGH_VALUE';

-- D002: Device compartilhado entre múltiplas contas
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('D002_SHARED_DEVICE', 'Device Compartilhado', 'Device usado por múltiplas contas', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'D002_SHARED_DEVICE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'deviceAccountCount', 'GT', '1', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'D002_SHARED_DEVICE';

-- D003: Device com histórico de fraude
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('D003_FRAUD_DEVICE', 'Device com Histórico de Fraude', 'Device associado a fraude anterior', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'D003_FRAUD_DEVICE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'deviceFraudHistory', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'D003_FRAUD_DEVICE';

-- D004-D015: Mais regras de device fingerprint
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('D004_EMULATOR_DETECTED', 'Emulador Detectado', 'Transação de emulador de dispositivo', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('D005_ROOTED_DEVICE', 'Device Rooted/Jailbroken', 'Transação de dispositivo rooted ou jailbroken', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('D006_VPN_DETECTED', 'VPN Detectada', 'Transação via VPN', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('D007_PROXY_DETECTED', 'Proxy Detectado', 'Transação via proxy', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('D008_TOR_DETECTED', 'TOR Detectado', 'Transação via rede TOR', 'FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('D009_DEVICE_AGE_LOW', 'Device Muito Novo', 'Device com menos de 24h de uso', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('D010_BROWSER_MISMATCH', 'Browser Mismatch', 'User-agent inconsistente com fingerprint', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('D011_TIMEZONE_MISMATCH', 'Timezone Mismatch', 'Timezone inconsistente com geolocalização', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('D012_LANGUAGE_MISMATCH', 'Language Mismatch', 'Idioma do browser inconsistente com país', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('D013_SCREEN_ANOMALY', 'Screen Anomaly', 'Resolução de tela anômala', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('D014_DEVICE_VELOCITY', 'Device Velocity', 'Múltiplos cartões usados no mesmo device', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('D015_DEVICE_BLACKLIST', 'Device Blacklist', 'Device na lista negra', 'FRAUDE', 98, 99, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 2: GEOLOCALIZAÇÃO (15 regras)
-- ============================================================================

-- G001: Viagem impossível
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('G001_IMPOSSIBLE_TRAVEL', 'Viagem Impossível', 'Transação em localização fisicamente impossível', 'FRAUDE', 98, 99, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'G001_IMPOSSIBLE_TRAVEL';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'impossibleTravel', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'G001_IMPOSSIBLE_TRAVEL';

-- G002: País de alto risco
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('G002_HIGH_RISK_COUNTRY', 'País de Alto Risco', 'Transação de país de alto risco', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'G002_HIGH_RISK_COUNTRY';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'merchantCountry', 'IN_LIST', 'NG,RU,UA,BY,IR,KP,SY,VE,MM,AF', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'G002_HIGH_RISK_COUNTRY';

-- G003: Nova geolocalização + alto valor
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('G003_NEW_GEO_HIGH_VALUE', 'Nova Geo Alto Valor', 'Transação > R$ 3.000 de nova localização', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'G003_NEW_GEO_HIGH_VALUE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'geoLocation', 'IS_NEW', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'G003_NEW_GEO_HIGH_VALUE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT', '3000', 1
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'G003_NEW_GEO_HIGH_VALUE';

-- G004-G015: Mais regras de geolocalização
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('G004_IP_COUNTRY_MISMATCH', 'IP Country Mismatch', 'País do IP diferente do país do cartão', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('G005_BILLING_SHIPPING_MISMATCH', 'Billing Shipping Mismatch', 'País de cobrança diferente do país de entrega', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('G006_CROSS_BORDER', 'Cross Border', 'Transação internacional', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('G007_CROSS_BORDER_HIGH_VALUE', 'Cross Border Alto Valor', 'Transação internacional > R$ 5.000', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('G008_SANCTIONED_COUNTRY', 'País Sancionado', 'Transação de país sob sanção', 'FRAUDE', 98, 99, 'PUBLISHED', false, NULL),
('G009_IP_BLACKLIST', 'IP Blacklist', 'IP na lista negra', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('G010_IP_DATACENTER', 'IP Datacenter', 'IP de datacenter/hosting', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('G011_GEO_VELOCITY', 'Geo Velocity', 'Múltiplas localizações em curto período', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('G012_DISTANCE_FROM_HOME', 'Distância de Casa', 'Transação > 500km do endereço cadastrado', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('G013_FIRST_INTERNATIONAL', 'Primeira Internacional', 'Primeira transação internacional do cliente', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('G014_TIMEZONE_ANOMALY', 'Timezone Anomaly', 'Transação em timezone incomum para o cliente', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('G015_GEO_BLACKLIST', 'Geo Blacklist', 'Geolocalização na lista negra', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

-- ============================================================================
-- FIM DA MIGRATION V34
-- ============================================================================

-- Total de regras criadas: 30 regras de DEVICE FINGERPRINT e GEOLOCALIZAÇÃO
-- Categorias cobertas:
--   - Device Fingerprint: 15 regras (D001-D015)
--   - Geolocalização: 15 regras (G001-G015)
