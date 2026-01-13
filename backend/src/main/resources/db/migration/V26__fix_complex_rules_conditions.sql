-- V26__fix_complex_rules_conditions.sql
-- ===================================================================================
-- CORREÇÃO: Adiciona condition groups e conditions para complex rules que estão vazias
-- ===================================================================================
-- As seguintes complex rules foram criadas em V22 mas ficaram sem condições:
-- 1. ANOMALIA_DISPOSITIVO (era DEVICE_ANOMALY_COMPLEX)
-- 2. ATAQUE_VELOCIDADE_TRANSFRONTEIRICO (era CROSS_BORDER_VELOCITY_COMPLEX)
-- 3. CASCATA_MCC_ALTO_RISCO (era HIGH_RISK_MCC_CASCADE_COMPLEX)
-- 4. PADRAO_CONTA_LARANJA (era MONEY_MULE_PATTERN_COMPLEX)
-- 5. PERFIL_RISCO_PRIMEIRA_VEZ (era FIRST_TIME_HIGH_RISK_COMPLEX)
-- ===================================================================================

-- =============================================
-- 1. ANOMALIA_DISPOSITIVO - Device Anomaly
-- =============================================
-- Detecta anomalias de dispositivo/autenticação
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'ANOMALIA_DISPOSITIVO')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Anomalia de dispositivo e autenticação', true
FROM rule
WHERE NOT EXISTS (
  SELECT 1 FROM rule_condition_groups rcg 
  JOIN complex_rules cr ON rcg.complex_rule_id = cr.id 
  WHERE cr.key = 'ANOMALIA_DISPOSITIVO'
);

WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'ANOMALIA_DISPOSITIVO' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  true
FROM grp, 
(VALUES
  (0, 'eciIndicator', 'EQ', 'NUMBER', '7'),
  (1, 'tokenAssuranceLevel', 'LTE', 'NUMBER', '1'),
  (2, 'consumerAuthenticationScore', 'LT', 'NUMBER', '50'),
  (3, 'transactionAmount', 'GT', 'NUMBER', '100000')
) AS conditions(pos, field_name, op, vtype, vsingle)
WHERE grp.group_id IS NOT NULL
ON CONFLICT DO NOTHING;

-- =============================================
-- 2. ATAQUE_VELOCIDADE_TRANSFRONTEIRICO - Cross-Border Velocity
-- =============================================
-- Detecta transações em países diferentes em tempo impossível
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'ATAQUE_VELOCIDADE_TRANSFRONTEIRICO')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Velocidade transfronteiriça impossível', true
FROM rule
WHERE NOT EXISTS (
  SELECT 1 FROM rule_condition_groups rcg 
  JOIN complex_rules cr ON rcg.complex_rule_id = cr.id 
  WHERE cr.key = 'ATAQUE_VELOCIDADE_TRANSFRONTEIRICO'
);

WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'ATAQUE_VELOCIDADE_TRANSFRONTEIRICO' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  true
FROM grp, 
(VALUES
  (0, 'merchantCountryCode', 'NEQ', 'STRING', '076'),
  (1, 'velocity.distinct_countries_2h', 'GT', 'NUMBER', '1'),
  (2, 'transactionAmount', 'GT', 'NUMBER', '50000')
) AS conditions(pos, field_name, op, vtype, vsingle)
WHERE grp.group_id IS NOT NULL
ON CONFLICT DO NOTHING;

-- =============================================
-- 3. CASCATA_MCC_ALTO_RISCO - High Risk MCC Cascade
-- =============================================
-- Detecta múltiplas transações em MCCs de alto risco
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'CASCATA_MCC_ALTO_RISCO')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Cascata de MCCs de alto risco', true
FROM rule
WHERE NOT EXISTS (
  SELECT 1 FROM rule_condition_groups rcg 
  JOIN complex_rules cr ON rcg.complex_rule_id = cr.id 
  WHERE cr.key = 'CASCATA_MCC_ALTO_RISCO'
);

WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'CASCATA_MCC_ALTO_RISCO' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, value_array, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  conditions.varray,
  true
FROM grp, 
(VALUES
  (0, 'mcc', 'IN', 'ARRAY_NUMBER', NULL, ARRAY['7995', '6051', '6211', '4829', '5933', '7273']),
  (1, 'velocity.high_risk_mcc_count_1h', 'GT', 'NUMBER', '2', NULL),
  (2, 'transactionAmount', 'GT', 'NUMBER', '100000', NULL)
) AS conditions(pos, field_name, op, vtype, vsingle, varray)
WHERE grp.group_id IS NOT NULL
ON CONFLICT DO NOTHING;

-- =============================================
-- 4. PADRAO_CONTA_LARANJA - Money Mule Pattern
-- =============================================
-- Detecta padrão de conta laranja (recebe e transfere rapidamente)
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'PADRAO_CONTA_LARANJA')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Padrão de conta laranja', true
FROM rule
WHERE NOT EXISTS (
  SELECT 1 FROM rule_condition_groups rcg 
  JOIN complex_rules cr ON rcg.complex_rule_id = cr.id 
  WHERE cr.key = 'PADRAO_CONTA_LARANJA'
);

WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'PADRAO_CONTA_LARANJA' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, value_array, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  conditions.varray,
  true
FROM grp, 
(VALUES
  (0, 'mcc', 'IN', 'ARRAY_NUMBER', NULL, ARRAY['6010', '6011', '6012', '6051', '4829']),
  (1, 'velocity.msb_tx_count_24h', 'GT', 'NUMBER', '3', NULL),
  (2, 'velocity.amount_received_24h', 'GT', 'NUMBER', '500000', NULL),
  (3, 'transactionAmount', 'GT', 'NUMBER', '100000', NULL)
) AS conditions(pos, field_name, op, vtype, vsingle, varray)
WHERE grp.group_id IS NOT NULL
ON CONFLICT DO NOTHING;

-- =============================================
-- 5. PERFIL_RISCO_PRIMEIRA_VEZ - First Time High Risk
-- =============================================
-- Detecta primeira transação com perfil de alto risco
WITH rule AS (SELECT id FROM complex_rules WHERE key = 'PERFIL_RISCO_PRIMEIRA_VEZ')
INSERT INTO rule_condition_groups (id, complex_rule_id, parent_group_id, logic_operator, position, name, enabled)
SELECT gen_random_uuid(), rule.id, NULL, 'AND', 0, 'Root: Perfil de alto risco na primeira transação', true
FROM rule
WHERE NOT EXISTS (
  SELECT 1 FROM rule_condition_groups rcg 
  JOIN complex_rules cr ON rcg.complex_rule_id = cr.id 
  WHERE cr.key = 'PERFIL_RISCO_PRIMEIRA_VEZ'
);

WITH grp AS (
  SELECT g.id as group_id 
  FROM rule_condition_groups g 
  JOIN complex_rules r ON g.complex_rule_id = r.id 
  WHERE r.key = 'PERFIL_RISCO_PRIMEIRA_VEZ' AND g.parent_group_id IS NULL
  LIMIT 1
)
INSERT INTO rule_conditions (id, group_id, position, field_name, operator, value_type, value_single, enabled)
SELECT 
  gen_random_uuid(),
  grp.group_id,
  conditions.pos,
  conditions.field_name,
  conditions.op::condition_operator,
  conditions.vtype::condition_value_type,
  conditions.vsingle,
  true
FROM grp, 
(VALUES
  (0, 'customer.is_first_transaction', 'EQ', 'STRING', 'true'),
  (1, 'transactionAmount', 'GT', 'NUMBER', '200000'),
  (2, 'merchantCountryCode', 'NEQ', 'STRING', '076'),
  (3, 'eciIndicator', 'EQ', 'NUMBER', '7')
) AS conditions(pos, field_name, op, vtype, vsingle)
WHERE grp.group_id IS NOT NULL
ON CONFLICT DO NOTHING;

-- ===================================================================================
-- FIM DA MIGRAÇÃO V26
-- ===================================================================================
