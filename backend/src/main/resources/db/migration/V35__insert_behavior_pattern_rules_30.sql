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
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('B001_AVG_AMOUNT_CHANGE', 'Mudança de Valor Médio', 'Transação 5x maior que a média histórica', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'B001_AVG_AMOUNT_CHANGE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'transactionAmount', 'GT_FIELD_MULTIPLIER', 'customerAvgAmount:5', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'B001_AVG_AMOUNT_CHANGE';

-- B002: Mudança de categoria de gasto
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('B002_CATEGORY_CHANGE', 'Mudança de Categoria', 'Transação em categoria nunca usada pelo cliente', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'B002_CATEGORY_CHANGE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'mcc', 'NOT_IN_CUSTOMER_HISTORY', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'B002_CATEGORY_CHANGE';

-- B003: Mudança de horário habitual
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('B003_TIME_CHANGE', 'Mudança de Horário', 'Transação fora do horário habitual do cliente', 'AND', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'B003_TIME_CHANGE';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'transactionHour', 'NOT_IN_CUSTOMER_USUAL_HOURS', '', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'B003_TIME_CHANGE';

-- B004-B015: Mais regras de mudança de comportamento
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES 
('B004_FREQUENCY_SPIKE', 'Spike de Frequência', 'Aumento de 300% na frequência de transações', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B005_CHANNEL_CHANGE', 'Mudança de Canal', 'Primeira transação em novo canal (app/web/pos)', 'AND', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B006_MERCHANT_TYPE_CHANGE', 'Mudança de Tipo de Merchant', 'Transação em tipo de merchant incomum', 'AND', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B007_PAYMENT_METHOD_CHANGE', 'Mudança de Método de Pagamento', 'Primeira transação com novo método de pagamento', 'AND', 'REVIEW', 45, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B008_CURRENCY_CHANGE', 'Mudança de Moeda', 'Primeira transação em nova moeda', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B009_DORMANT_REACTIVATION', 'Reativação de Conta Dormante', 'Transação após 90+ dias de inatividade', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B010_SPENDING_PATTERN_BREAK', 'Quebra de Padrão de Gasto', 'Transação que quebra o padrão de gasto', 'AND', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B011_WEEKEND_SPENDING_CHANGE', 'Mudança de Gasto no Fim de Semana', 'Gasto no fim de semana muito diferente do habitual', 'AND', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B012_NIGHT_SPENDING_CHANGE', 'Mudança de Gasto Noturno', 'Gasto noturno muito diferente do habitual', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B013_INTERNATIONAL_FIRST', 'Primeira Internacional', 'Primeira transação internacional do cliente', 'AND', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B014_HIGH_RISK_MCC_FIRST', 'Primeiro MCC de Alto Risco', 'Primeira transação em MCC de alto risco', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('B015_CREDIT_UTILIZATION_SPIKE', 'Spike de Utilização de Crédito', 'Aumento abrupto na utilização do crédito', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

-- ============================================================================
-- CATEGORIA 2: PADRÕES DE USO (15 regras)
-- ============================================================================

-- P001: Padrão de escada (valores crescentes)
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('P001_ESCALATION_PATTERN', 'Padrão de Escada', 'Valores crescentes em transações consecutivas', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'P001_ESCALATION_PATTERN';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'escalationPattern', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'P001_ESCALATION_PATTERN';

-- P002: Padrão de round numbers (valores redondos)
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('P002_ROUND_NUMBERS', 'Valores Redondos', 'Múltiplas transações com valores redondos', 'AND', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'P002_ROUND_NUMBERS';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'roundNumberPattern', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'P002_ROUND_NUMBERS';

-- P003: Padrão de split (divisão de transação)
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('P003_SPLIT_PATTERN', 'Padrão de Split', 'Múltiplas transações pequenas no mesmo merchant', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

INSERT INTO rule_condition_groups (complex_rule_id, group_index, logic_operator)
SELECT id, 0, 'AND' FROM complex_rules WHERE rule_key = 'P003_SPLIT_PATTERN';

INSERT INTO rule_conditions (condition_group_id, field_name, operator, comparison_value, condition_index)
SELECT rcg.id, 'splitTransactionPattern', 'EQ', 'Y', 0
FROM rule_condition_groups rcg
JOIN complex_rules cr ON rcg.complex_rule_id = cr.id
WHERE cr.rule_key = 'P003_SPLIT_PATTERN';

-- P004-P015: Mais regras de padrões de uso
INSERT INTO complex_rules (rule_key, name, description, logic_operator, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES 
('P004_STRUCTURING_PATTERN', 'Padrão de Structuring', 'Transações abaixo do limite de reporte', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P005_BURST_PATTERN', 'Padrão de Burst', 'Múltiplas transações em rajada', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P006_SEQUENTIAL_PATTERN', 'Padrão Sequencial', 'Transações em sequência numérica', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P007_RECURRING_AMOUNT', 'Valor Recorrente', 'Mesmo valor em múltiplas transações', 'AND', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P008_PING_PONG_PATTERN', 'Padrão Ping-Pong', 'Transferências de ida e volta', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P009_LAYERING_PATTERN', 'Padrão de Layering', 'Múltiplas transferências entre contas', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P010_SMURFING_PATTERN', 'Padrão de Smurfing', 'Múltiplos depósitos pequenos', 'AND', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P011_FUNNEL_PATTERN', 'Padrão de Funil', 'Múltiplas entradas para uma saída', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P012_FAN_OUT_PATTERN', 'Padrão Fan-Out', 'Uma entrada para múltiplas saídas', 'AND', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P013_RAPID_SUCCESSION', 'Sucessão Rápida', 'Transações em rápida sucessão', 'AND', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P014_THRESHOLD_TESTING', 'Teste de Limite', 'Transações próximas ao limite', 'AND', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35'),
('P015_ANOMALY_CLUSTER', 'Cluster de Anomalias', 'Múltiplas anomalias em curto período', 'AND', 'REVIEW', 85, 90, 'ACTIVE', false, 'SYSTEM_MIGRATION_V35');

-- ============================================================================
-- FIM DA MIGRATION V35
-- ============================================================================

-- Total de regras criadas: 30 regras de COMPORTAMENTO e PADRÃO
-- Categorias cobertas:
--   - Mudança de Comportamento: 15 regras (B001-B015)
--   - Padrões de Uso: 15 regras (P001-P015)
