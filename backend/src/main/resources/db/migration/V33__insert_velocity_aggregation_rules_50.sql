-- ============================================================================
-- Migration V33: 50 Regras de VELOCIDADE e AGREGAÇÃO
-- Data: 2026-01-06
-- Objetivo: Implementar 50 regras de detecção de fraude baseadas em
--           velocidade, frequência e agregações temporais
-- ============================================================================

-- ============================================================================
-- CATEGORIA 1: VELOCIDADE DE TRANSAÇÕES (15 regras)
-- ============================================================================

-- V001: Alta frequência de transações em 1 hora
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V001_HIGH_FREQ_1H', 'Alta Frequência 1h', 'Mais de 10 transações em 1 hora', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V001_HIGH_FREQ_1H';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'COUNT_LAST_N_HOURS', 'GT', '10:1', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V001_HIGH_FREQ_1H';

-- V002: Alta frequência de transações em 24 horas
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V002_HIGH_FREQ_24H', 'Alta Frequência 24h', 'Mais de 50 transações em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V002_HIGH_FREQ_24H';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'COUNT_LAST_N_HOURS', 'GT', '50:24', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V002_HIGH_FREQ_24H';

-- V003: Velocidade extrema (transações em segundos)
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V003_EXTREME_VELOCITY', 'Velocidade Extrema', 'Mais de 5 transações em 1 minuto', 'AND', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V003_EXTREME_VELOCITY';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'transactionsLastMinute', 'GT', '5', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V003_EXTREME_VELOCITY';

-- V004-V015: Mais regras de velocidade
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('V004_HIGH_FREQ_7D', 'Alta Frequência 7 dias', 'Mais de 200 transações em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('V005_HIGH_FREQ_30D', 'Alta Frequência 30 dias', 'Mais de 500 transações em 30 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('V006_VELOCITY_SPIKE', 'Spike de Velocidade', 'Aumento de 500% na frequência vs média', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V007_VELOCITY_NIGHT', 'Velocidade Noturna', 'Alta frequência em horário noturno', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V008_VELOCITY_WEEKEND', 'Velocidade Fim de Semana', 'Alta frequência no fim de semana', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('V009_VELOCITY_HOLIDAY', 'Velocidade Feriado', 'Alta frequência em feriado', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('V010_VELOCITY_NEW_CARD', 'Velocidade Cartão Novo', 'Alta frequência em cartão recém-emitido', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V011_VELOCITY_NEW_ACCOUNT', 'Velocidade Conta Nova', 'Alta frequência em conta recém-aberta', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('V012_VELOCITY_DORMANT', 'Velocidade Conta Dormante', 'Alta frequência em conta anteriormente inativa', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V013_VELOCITY_INTERNATIONAL', 'Velocidade Internacional', 'Alta frequência de transações internacionais', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V014_VELOCITY_ATM', 'Velocidade ATM', 'Alta frequência de saques em ATM', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V015_VELOCITY_ECOMMERCE', 'Velocidade E-commerce', 'Alta frequência de compras online', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 2: AGREGAÇÃO DE VALORES (15 regras)
-- ============================================================================

-- V016: Soma alta em 1 hora
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V016_SUM_HIGH_1H', 'Soma Alta 1h', 'Soma de transações > R$ 10.000 em 1 hora', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V016_SUM_HIGH_1H';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'SUM_LAST_N_HOURS', 'GT', '10000:1', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V016_SUM_HIGH_1H';

-- V017: Soma alta em 24 horas
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V017_SUM_HIGH_24H', 'Soma Alta 24h', 'Soma de transações > R$ 50.000 em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V017_SUM_HIGH_24H';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'SUM_LAST_N_HOURS', 'GT', '50000:24', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V017_SUM_HIGH_24H';

-- V018-V030: Mais regras de agregação
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('V018_SUM_HIGH_7D', 'Soma Alta 7 dias', 'Soma de transações > R$ 100.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V019_SUM_HIGH_30D', 'Soma Alta 30 dias', 'Soma de transações > R$ 200.000 em 30 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V020_AVG_SPIKE', 'Spike de Média', 'Transação 10x maior que a média histórica', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('V021_AVG_HIGH_7D', 'Média Alta 7 dias', 'Média de transações > R$ 5.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('V022_MAX_SPIKE', 'Spike de Máximo', 'Transação 5x maior que o máximo histórico', 'AND', 'SUSPEITA_DE_FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('V023_SUM_CASH_HIGH', 'Soma Cash Alta', 'Soma de saques > R$ 20.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V024_SUM_INTERNATIONAL_HIGH', 'Soma Internacional Alta', 'Soma de transações internacionais > R$ 30.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('V025_SUM_HIGH_RISK_MCC', 'Soma MCC Alto Risco', 'Soma em MCCs de alto risco > R$ 10.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V026_SUM_GIFT_CARDS', 'Soma Gift Cards', 'Soma de compras de gift cards > R$ 5.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('V027_SUM_CRYPTO', 'Soma Crypto', 'Soma de compras de crypto > R$ 20.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 90, 95, 'PUBLISHED', false, NULL),
('V028_SUM_GAMBLING', 'Soma Gambling', 'Soma em casas de apostas > R$ 10.000 em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V029_SUM_NEAR_LIMIT', 'Soma Próxima do Limite', 'Soma > 80% do limite de crédito em 7 dias', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V030_SUM_STRUCTURING', 'Soma Structuring', 'Múltiplas transações somando > R$ 10.000 em 24h', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 3: CONTAGEM DISTINTA (10 regras)
-- ============================================================================

-- V031: Múltiplos merchants em 1 hora
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V031_DISTINCT_MERCHANTS_1H', 'Merchants Distintos 1h', 'Mais de 5 merchants distintos em 1 hora', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V031_DISTINCT_MERCHANTS_1H';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS', 'GT', '5:1', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V031_DISTINCT_MERCHANTS_1H';

-- V032-V040: Mais regras de contagem distinta
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('V032_DISTINCT_MERCHANTS_24H', 'Merchants Distintos 24h', 'Mais de 20 merchants distintos em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V033_DISTINCT_COUNTRIES_1H', 'Países Distintos 1h', 'Mais de 2 países distintos em 1 hora', 'AND', 'FRAUDE', 95, 98, 'PUBLISHED', false, NULL),
('V034_DISTINCT_COUNTRIES_24H', 'Países Distintos 24h', 'Mais de 5 países distintos em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('V035_DISTINCT_CITIES_1H', 'Cidades Distintas 1h', 'Mais de 3 cidades distintas em 1 hora', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V036_DISTINCT_DEVICES_24H', 'Devices Distintos 24h', 'Mais de 3 devices distintos em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL),
('V037_DISTINCT_IPS_1H', 'IPs Distintos 1h', 'Mais de 5 IPs distintos em 1 hora', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('V038_DISTINCT_MCCS_24H', 'MCCs Distintos 24h', 'Mais de 10 MCCs distintos em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('V039_DISTINCT_CURRENCIES_24H', 'Moedas Distintas 24h', 'Mais de 3 moedas distintas em 24 horas', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V040_DISTINCT_TERMINALS_1H', 'Terminais Distintos 1h', 'Mais de 5 terminais distintos em 1 hora', 'AND', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 4: PADRÕES TEMPORAIS (10 regras)
-- ============================================================================

-- V041: Transação fora do horário habitual
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES ('V041_UNUSUAL_TIME', 'Horário Incomum', 'Transação fora do horário habitual do cliente', 'AND', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'V041_UNUSUAL_TIME';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, condition_index)
SELECT rcg.id, 'transactionHour', 'NOT_IN_CUSTOMER_USUAL_HOURS', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'V041_UNUSUAL_TIME';

-- V042-V050: Mais regras de padrões temporais
INSERT INTO complex_rules (key, name, description, logic_operator, decision, severity, priority, status, enabled, created_by)
VALUES 
('V042_MADRUGADA', 'Madrugada', 'Transação entre 00h e 05h', 'AND', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('V043_MADRUGADA_HIGH_VALUE', 'Madrugada Alto Valor', 'Transação > R$ 5.000 entre 00h e 05h', 'AND', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('V044_WEEKEND_HIGH_VELOCITY', 'Fim de Semana Alta Velocidade', 'Alta frequência no fim de semana', 'AND', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('V045_HOLIDAY_HIGH_VALUE', 'Feriado Alto Valor', 'Transação > R$ 10.000 em feriado', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('V046_FIRST_OF_MONTH', 'Início do Mês', 'Alta frequência no início do mês', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('V047_END_OF_MONTH', 'Fim do Mês', 'Alta frequência no fim do mês', 'AND', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('V048_LUNCH_HOUR', 'Horário de Almoço', 'Transação de alto valor no horário de almoço', 'AND', 'SUSPEITA_DE_FRAUDE', 40, 45, 'PUBLISHED', false, NULL),
('V049_BUSINESS_HOURS', 'Horário Comercial', 'Transação pessoal em horário comercial', 'AND', 'SUSPEITA_DE_FRAUDE', 35, 40, 'PUBLISHED', false, NULL),
('V050_PATTERN_BREAK', 'Quebra de Padrão', 'Transação que quebra o padrão temporal do cliente', 'AND', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL);

-- ============================================================================
-- FIM DA MIGRATION V33
-- ============================================================================

-- Total de regras criadas: 50 regras de VELOCIDADE e AGREGAÇÃO
-- Categorias cobertas:
--   - Velocidade de Transações: 15 regras (V001-V015)
--   - Agregação de Valores: 15 regras (V016-V030)
--   - Contagem Distinta: 10 regras (V031-V040)
--   - Padrões Temporais: 10 regras (V041-V050)
