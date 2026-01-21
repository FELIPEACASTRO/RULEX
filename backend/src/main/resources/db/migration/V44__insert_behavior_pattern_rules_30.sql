-- ============================================================================
-- Migration V35: 30 Regras de COMPORTAMENTO e PADRÃO
-- Data: 2026-01-06
-- Objetivo: Implementar 30 regras de detecção de fraude baseadas em
--           análise comportamental e padrões de uso do cliente
-- ============================================================================

-- ============================================================================
-- CATEGORIA 1: MUDANÇA DE COMPORTAMENTO (15 regras)
-- ============================================================================

-- B001: Mudança abrupta de valor médio
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('B001_AVG_AMOUNT_CHANGE', 'Mudança de Valor Médio', 'Transação 5x maior que a média histórica', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'B001_AVG_AMOUNT_CHANGE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionAmount', 'GT_FIELD_MULTIPLIER', 'customerAvgAmount:5', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'B001_AVG_AMOUNT_CHANGE';

-- B002: Mudança de categoria de gasto
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('B002_CATEGORY_CHANGE', 'Mudança de Categoria', 'Transação em categoria nunca usada pelo cliente', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'B002_CATEGORY_CHANGE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'mcc', 'NOT_IN_CUSTOMER_HISTORY', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'B002_CATEGORY_CHANGE';

-- B003: Mudança de horário habitual
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('B003_TIME_CHANGE', 'Mudança de Horário', 'Transação fora do horário habitual do cliente', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'B003_TIME_CHANGE';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'transactionHour', 'NOT_IN_CUSTOMER_USUAL_HOURS', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'B003_TIME_CHANGE';

-- B004-B015: Mais regras de mudança de comportamento
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('B004_FREQUENCY_SPIKE', 'Spike de Frequência', 'Aumento de 300% na frequência de transações', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('B005_CHANNEL_CHANGE', 'Mudança de Canal', 'Primeira transação em novo canal (app/web/pos)', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('B006_MERCHANT_TYPE_CHANGE', 'Mudança de Tipo de Merchant', 'Transação em tipo de merchant incomum', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('B007_PAYMENT_METHOD_CHANGE', 'Mudança de Método de Pagamento', 'Primeira transação com novo método de pagamento', 'SUSPEITA_DE_FRAUDE', 45, 50, 'PUBLISHED', false, NULL),
('B008_CURRENCY_CHANGE', 'Mudança de Moeda', 'Primeira transação em nova moeda', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('B009_DORMANT_REACTIVATION', 'Reativação de Conta Dormante', 'Transação após 90+ dias de inatividade', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('B010_SPENDING_PATTERN_BREAK', 'Quebra de Padrão de Gasto', 'Transação que quebra o padrão de gasto', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('B011_WEEKEND_SPENDING_CHANGE', 'Mudança de Gasto no Fim de Semana', 'Gasto no fim de semana muito diferente do habitual', 'SUSPEITA_DE_FRAUDE', 50, 55, 'PUBLISHED', false, NULL),
('B012_NIGHT_SPENDING_CHANGE', 'Mudança de Gasto Noturno', 'Gasto noturno muito diferente do habitual', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL),
('B013_INTERNATIONAL_FIRST', 'Primeira Internacional', 'Primeira transação internacional do cliente', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('B014_HIGH_RISK_MCC_FIRST', 'Primeiro MCC de Alto Risco', 'Primeira transação em MCC de alto risco', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('B015_CREDIT_UTILIZATION_SPIKE', 'Spike de Utilização de Crédito', 'Aumento abrupto na utilização do crédito', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

-- ============================================================================
-- CATEGORIA 2: PADRÕES DE USO (15 regras)
-- ============================================================================

-- P001: Padrão de escada (valores crescentes)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('P001_ESCALATION_PATTERN', 'Padrão de Escada', 'Valores crescentes em transações consecutivas', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'P001_ESCALATION_PATTERN';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'escalationPattern', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'P001_ESCALATION_PATTERN';

-- P002: Padrão de round numbers (valores redondos)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('P002_ROUND_NUMBERS', 'Valores Redondos', 'Múltiplas transações com valores redondos', 'SUSPEITA_DE_FRAUDE', 60, 65, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'P002_ROUND_NUMBERS';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'roundNumberPattern', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'P002_ROUND_NUMBERS';

-- P003: Padrão de split (divisão de transação)
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES ('P003_SPLIT_PATTERN', 'Padrão de Split', 'Múltiplas transações pequenas no mesmo merchant', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE key = 'P003_SPLIT_PATTERN';

INSERT INTO rule_conditions (group_id, field_name, operator, value_single, position)
SELECT rcg.id, 'splitTransactionPattern', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.key = 'P003_SPLIT_PATTERN';

-- P004-P015: Mais regras de padrões de uso
INSERT INTO complex_rules (key, title, description, decision, severity, priority, status, enabled, created_by)
VALUES 
('P004_STRUCTURING_PATTERN', 'Padrão de Structuring', 'Transações abaixo do limite de reporte', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('P005_BURST_PATTERN', 'Padrão de Burst', 'Múltiplas transações em rajada', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('P006_SEQUENTIAL_PATTERN', 'Padrão Sequencial', 'Transações em sequência numérica', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('P007_RECURRING_AMOUNT', 'Valor Recorrente', 'Mesmo valor em múltiplas transações', 'SUSPEITA_DE_FRAUDE', 55, 60, 'PUBLISHED', false, NULL),
('P008_PING_PONG_PATTERN', 'Padrão Ping-Pong', 'Transferências de ida e volta', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('P009_LAYERING_PATTERN', 'Padrão de Layering', 'Múltiplas transferências entre contas', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('P010_SMURFING_PATTERN', 'Padrão de Smurfing', 'Múltiplos depósitos pequenos', 'SUSPEITA_DE_FRAUDE', 80, 85, 'PUBLISHED', false, NULL),
('P011_FUNNEL_PATTERN', 'Padrão de Funil', 'Múltiplas entradas para uma saída', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('P012_FAN_OUT_PATTERN', 'Padrão Fan-Out', 'Uma entrada para múltiplas saídas', 'SUSPEITA_DE_FRAUDE', 75, 80, 'PUBLISHED', false, NULL),
('P013_RAPID_SUCCESSION', 'Sucessão Rápida', 'Transações em rápida sucessão', 'SUSPEITA_DE_FRAUDE', 70, 75, 'PUBLISHED', false, NULL),
('P014_THRESHOLD_TESTING', 'Teste de Limite', 'Transações próximas ao limite', 'SUSPEITA_DE_FRAUDE', 65, 70, 'PUBLISHED', false, NULL),
('P015_ANOMALY_CLUSTER', 'Cluster de Anomalias', 'Múltiplas anomalias em curto período', 'SUSPEITA_DE_FRAUDE', 85, 90, 'PUBLISHED', false, NULL);

-- ============================================================================
-- FIM DA MIGRATION V35
-- ============================================================================

-- Total de regras criadas: 30 regras de COMPORTAMENTO e PADRÃO
-- Categorias cobertas:
--   - Mudança de Comportamento: 15 regras (B001-B015)
--   - Padrões de Uso: 15 regras (P001-P015)
