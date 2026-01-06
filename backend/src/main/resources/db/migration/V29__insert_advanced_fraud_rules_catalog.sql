-- ============================================================================
-- Migration V29: Inserção de Regras Avançadas de Fraude (Catálogo Completo)
-- Data: 2026-01-06
-- Fonte: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md (60 regras totais)
-- Objetivo: Implementar 25 regras adicionais cobrindo categorias avançadas:
--           - Card Testing (CT): 6 regras adicionais
--           - KYC Fraud (KYC): 10 regras
--           - Transfer Fraud (TRF): 5 regras
--           - Bot Detection (BOT): 4 regras
-- ============================================================================

-- ============================================================================
-- CATEGORIA: CARD TESTING (CT) - Regras Adicionais
-- ============================================================================

-- CT-005: CNP - Endereço de entrega de alto risco
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'CT_005_CNP_HIGH_RISK_SHIPPING',
    'Card Testing - CNP Endereço de Alto Risco',
    'Detecta transações CNP com endereço de entrega suspeito (frete expresso, drop address, PO Box).',
    'REVIEW',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'CT_005_CNP_HIGH_RISK_SHIPPING'), 1, 'AND');

-- Condição 1: POS Entry Mode indica CNP
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_005_CNP_HIGH_RISK_SHIPPING') AND group_order = 1),
    1, 'posEntryMode', 'IN_LIST', '81,10,79', 'STRING'
);

-- Condição 2: Shipping Method indica frete expresso
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_005_CNP_HIGH_RISK_SHIPPING') AND group_order = 1),
    2, 'shippingMethod', 'IN_LIST', 'EXPRESS,OVERNIGHT,SAME_DAY', 'STRING'
);

-- Condição 3: Valor >= 300
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_005_CNP_HIGH_RISK_SHIPPING') AND group_order = 1),
    3, 'transactionAmount', 'GTE', '300', 'NUMBER'
);

-- ============================================================================

-- CT-006: CNP - Mismatch de nome (titular vs recebedor)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'CT_006_CNP_NAME_MISMATCH',
    'Card Testing - CNP Nome Divergente',
    'Detecta transações CNP onde o nome do titular do cartão difere significativamente do nome do recebedor.',
    'REVIEW',
    70,
    75,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'CT_006_CNP_NAME_MISMATCH'), 1, 'AND');

-- Condição 1: POS Entry Mode indica CNP
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_006_CNP_NAME_MISMATCH') AND group_order = 1),
    1, 'posEntryMode', 'IN_LIST', '81,10,79', 'STRING'
);

-- Condição 2: Nome do titular ≠ nome do recebedor (similaridade < 50%)
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_006_CNP_NAME_MISMATCH') AND group_order = 1),
    2, 'cardholderName', 'NAME_SIMILARITY_LT', 'shippingName:50', 'STRING'
);

-- Condição 3: Valor >= 200
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_006_CNP_NAME_MISMATCH') AND group_order = 1),
    3, 'transactionAmount', 'GTE', '200', 'NUMBER'
);

-- ============================================================================

-- CT-007: Pico de chargeback por merchant
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'CT_007_MERCHANT_CHARGEBACK_SPIKE',
    'Card Testing - Pico de Chargeback no Merchant',
    'Detecta merchants com pico anômalo de chargebacks, indicando possível comprometimento ou fraude interna.',
    'REVIEW',
    85,
    90,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'CT_007_MERCHANT_CHARGEBACK_SPIKE'), 1, 'AND');

-- Condição: Taxa de chargeback do merchant > 2% nos últimos 7 dias
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_007_MERCHANT_CHARGEBACK_SPIKE') AND group_order = 1),
    1, 'merchantId', 'CHARGEBACK_RATE_GT', '2:7', 'NUMBER'  -- Taxa > 2% em 7 dias
);

-- ============================================================================

-- CT-008: Velocidade por device (múltiplas contas/cartões)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'CT_008_DEVICE_VELOCITY_MULTI_ACCOUNTS',
    'Card Testing - Device com Múltiplas Contas',
    'Detecta mesmo device realizando compras com múltiplos cartões/contas diferentes, padrão de fraude organizada.',
    'BLOCK',
    90,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'CT_008_DEVICE_VELOCITY_MULTI_ACCOUNTS'), 1, 'AND');

-- Condição: Device ID com >= 5 PANs distintos em 1 hora
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_008_DEVICE_VELOCITY_MULTI_ACCOUNTS') AND group_order = 1),
    1, 'deviceId', 'COUNT_DISTINCT_PANS_LAST_N_HOURS', '5:1', 'NUMBER'
);

-- ============================================================================

-- CT-009: 3DS falhou + tentativa imediata sem 3DS (fallback)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'CT_009_3DS_FAIL_THEN_NO_3DS',
    'Card Testing - 3DS Falhou + Tentativa sem 3DS',
    'Detecta tentativa de 3DS que falhou seguida de tentativa imediata sem 3DS, indicando bypass fraudulento.',
    'BLOCK',
    95,
    98,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'CT_009_3DS_FAIL_THEN_NO_3DS'), 1, 'AND');

-- Condição 1: ECI Indicator < 5 (sem 3DS)
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_009_3DS_FAIL_THEN_NO_3DS') AND group_order = 1),
    1, 'eciIndicator', 'LT', '5', 'NUMBER'
);

-- Condição 2: Houve tentativa 3DS falhada nos últimos 5 minutos
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_009_3DS_FAIL_THEN_NO_3DS') AND group_order = 1),
    2, 'pan', 'HAS_FAILED_3DS_LAST_N_MINUTES', '5', 'BOOLEAN'
);

-- ============================================================================

-- CT-010: MCC de alto risco + primeira transação + ticket alto
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'CT_010_HIGH_RISK_MCC_FIRST_TXN_HIGH_VALUE',
    'Card Testing - MCC Alto Risco + Primeira Transação + Valor Alto',
    'Detecta primeira transação do cartão em MCC de alto risco com valor elevado, padrão clássico de fraude.',
    'BLOCK',
    90,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'CT_010_HIGH_RISK_MCC_FIRST_TXN_HIGH_VALUE'), 1, 'AND');

-- Condição 1: MCC em lista de alto risco
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_010_HIGH_RISK_MCC_FIRST_TXN_HIGH_VALUE') AND group_order = 1),
    1, 'mcc', 'IN_LIST', '5094,5122,5912,5962,5993,6051,7273,7995', 'NUMBER'
);

-- Condição 2: Primeira transação do PAN
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_010_HIGH_RISK_MCC_FIRST_TXN_HIGH_VALUE') AND group_order = 1),
    2, 'pan', 'COUNT_LAST_N_HOURS', '1:8760', 'NUMBER'  -- Apenas 1 transação em 365 dias
);

-- Condição 3: Valor >= 500
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_010_HIGH_RISK_MCC_FIRST_TXN_HIGH_VALUE') AND group_order = 1),
    3, 'transactionAmount', 'GTE', '500', 'NUMBER'
);

-- ============================================================================
-- CATEGORIA: KYC FRAUD (KYC) - 10 Regras
-- ============================================================================

-- KYC-001: Múltiplas contas compartilhando o mesmo device/telefone
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_001_MULTI_ACCOUNTS_SAME_DEVICE',
    'KYC Fraud - Múltiplas Contas no Mesmo Device',
    'Detecta múltiplas contas cadastradas no mesmo device/telefone, indicando fraude sintética ou mula.',
    'REVIEW',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_001_MULTI_ACCOUNTS_SAME_DEVICE'), 1, 'AND');

-- Condição: Device ID com >= 3 contas distintas
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_001_MULTI_ACCOUNTS_SAME_DEVICE') AND group_order = 1),
    1, 'deviceId', 'COUNT_DISTINCT_ACCOUNTS', '3', 'NUMBER'
);

-- ============================================================================

-- KYC-002: Email descartável/temporário
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_002_DISPOSABLE_EMAIL',
    'KYC Fraud - Email Descartável',
    'Detecta cadastros com email descartável/temporário, indicando tentativa de fraude ou abuso.',
    'REVIEW',
    70,
    75,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_002_DISPOSABLE_EMAIL'), 1, 'AND');

-- Condição: Email em lista de provedores descartáveis
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_002_DISPOSABLE_EMAIL') AND group_order = 1),
    1, 'customerEmail', 'DOMAIN_IN_LIST', 'guerrillamail.com,10minutemail.com,temp-mail.org,mailinator.com', 'STRING'
);

-- ============================================================================

-- KYC-003: VOIP/virtual phone + alto risco
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_003_VOIP_PHONE_HIGH_RISK',
    'KYC Fraud - Telefone VOIP + Alto Risco',
    'Detecta cadastros com telefone VOIP/virtual em combinação com outros sinais de risco.',
    'REVIEW',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_003_VOIP_PHONE_HIGH_RISK'), 1, 'AND');

-- Condição 1: Telefone é VOIP
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_003_VOIP_PHONE_HIGH_RISK') AND group_order = 1),
    1, 'customerPhone', 'IS_VOIP', 'true', 'BOOLEAN'
);

-- Condição 2: País de alto risco
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_003_VOIP_PHONE_HIGH_RISK') AND group_order = 1),
    2, 'customerCountry', 'IN_LIST', 'NG,GH,PH,ID,VN', 'STRING'  -- Países com alta incidência de fraude
);

-- ============================================================================

-- KYC-005: Cadastro + 1ª transação em < 10 min (cash-out)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_005_FAST_SIGNUP_TO_TRANSACTION',
    'KYC Fraud - Cadastro + Transação Imediata',
    'Detecta cadastro seguido de transação em menos de 10 minutos, padrão típico de cash-out fraudulento.',
    'BLOCK',
    90,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_005_FAST_SIGNUP_TO_TRANSACTION'), 1, 'AND');

-- Condição 1: Conta criada há menos de 10 minutos
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_005_FAST_SIGNUP_TO_TRANSACTION') AND group_order = 1),
    1, 'customerAcctNumber', 'ACCOUNT_AGE_LT_MINUTES', '10', 'NUMBER'
);

-- Condição 2: Valor >= 100
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_005_FAST_SIGNUP_TO_TRANSACTION') AND group_order = 1),
    2, 'transactionAmount', 'GTE', '100', 'NUMBER'
);

-- ============================================================================

-- KYC-007: Endereço de alto risco (PO Box / locker)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_007_HIGH_RISK_ADDRESS',
    'KYC Fraud - Endereço de Alto Risco',
    'Detecta cadastros com endereço de alto risco (PO Box, locker, redirecionador).',
    'REVIEW',
    70,
    75,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_007_HIGH_RISK_ADDRESS'), 1, 'OR');

-- Condição 1: Endereço contém "PO BOX"
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_007_HIGH_RISK_ADDRESS') AND group_order = 1),
    1, 'billingAddress', 'CONTAINS', 'PO BOX', 'STRING'
);

-- Condição 2: Endereço contém "LOCKER"
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_007_HIGH_RISK_ADDRESS') AND group_order = 1),
    2, 'billingAddress', 'CONTAINS', 'LOCKER', 'STRING'
);

-- ============================================================================

-- KYC-008: Vários perfis usando o mesmo cartão de funding
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_008_MULTI_PROFILES_SAME_CARD',
    'KYC Fraud - Múltiplos Perfis com Mesmo Cartão',
    'Detecta múltiplos perfis/contas usando o mesmo cartão de funding, indicando fraude organizada.',
    'REVIEW',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_008_MULTI_PROFILES_SAME_CARD'), 1, 'AND');

-- Condição: PAN com >= 3 contas distintas
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_008_MULTI_PROFILES_SAME_CARD') AND group_order = 1),
    1, 'pan', 'COUNT_DISTINCT_ACCOUNTS', '3', 'NUMBER'
);

-- ============================================================================

-- KYC-009: Fraude em empréstimo - renda alta + dados frágeis
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_009_LOAN_FRAUD_HIGH_INCOME_WEAK_DATA',
    'KYC Fraud - Empréstimo com Renda Alta + Dados Frágeis',
    'Detecta solicitações de empréstimo com renda declarada alta mas dados de verificação fracos.',
    'REVIEW',
    85,
    90,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_009_LOAN_FRAUD_HIGH_INCOME_WEAK_DATA'), 1, 'AND');

-- Condição 1: Renda declarada >= 10000
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_009_LOAN_FRAUD_HIGH_INCOME_WEAK_DATA') AND group_order = 1),
    1, 'declaredIncome', 'GTE', '10000', 'NUMBER'
);

-- Condição 2: Score de verificação de identidade < 50
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_009_LOAN_FRAUD_HIGH_INCOME_WEAK_DATA') AND group_order = 1),
    2, 'identityVerificationScore', 'LT', '50', 'NUMBER'
);

-- ============================================================================

-- KYC-010: Abandono quando pede MFA/KYC
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_010_ABANDONMENT_ON_VERIFICATION',
    'KYC Fraud - Abandono ao Solicitar Verificação',
    'Detecta usuários que abandonam o processo quando MFA ou verificação adicional é solicitada.',
    'REVIEW',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_010_ABANDONMENT_ON_VERIFICATION'), 1, 'AND');

-- Condição 1: MFA foi solicitado
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_010_ABANDONMENT_ON_VERIFICATION') AND group_order = 1),
    1, 'mfaRequested', 'EQ', 'true', 'BOOLEAN'
);

-- Condição 2: Usuário não completou MFA
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_010_ABANDONMENT_ON_VERIFICATION') AND group_order = 1),
    2, 'mfaCompleted', 'EQ', 'false', 'BOOLEAN'
);

-- Condição 3: Histórico de abandonos >= 2
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_010_ABANDONMENT_ON_VERIFICATION') AND group_order = 1),
    3, 'customerAcctNumber', 'COUNT_MFA_ABANDONMENTS', '2', 'NUMBER'
);

-- ============================================================================

-- KYC-004: Inconsistência de nome/documento/idade
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_004_DATA_INCONSISTENCY',
    'KYC Fraud - Inconsistência de Dados Cadastrais',
    'Detecta inconsistências entre nome, documento e idade declarados vs validados.',
    'REVIEW',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_004_DATA_INCONSISTENCY'), 1, 'OR');

-- Condição 1: Nome não confere com documento
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_004_DATA_INCONSISTENCY') AND group_order = 1),
    1, 'nameMatchScore', 'LT', '70', 'NUMBER'
);

-- Condição 2: Idade declarada difere da idade do documento
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_004_DATA_INCONSISTENCY') AND group_order = 1),
    2, 'ageDifference', 'GT', '2', 'NUMBER'
);

-- ============================================================================

-- KYC-006: Sinais de deepfake/selfie anômalo
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'KYC_006_DEEPFAKE_DETECTION',
    'KYC Fraud - Detecção de Deepfake',
    'Detecta sinais de deepfake ou selfie anômalo no processo de verificação remota.',
    'BLOCK',
    95,
    98,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'KYC_006_DEEPFAKE_DETECTION'), 1, 'OR');

-- Condição 1: Score de deepfake > 70
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_006_DEEPFAKE_DETECTION') AND group_order = 1),
    1, 'deepfakeScore', 'GT', '70', 'NUMBER'
);

-- Condição 2: Liveness check falhou
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'KYC_006_DEEPFAKE_DETECTION') AND group_order = 1),
    2, 'livenessCheckPassed', 'EQ', 'false', 'BOOLEAN'
);

-- ============================================================================
-- CATEGORIA: TRANSFER FRAUD (TRF) - 5 Regras
-- ============================================================================

-- TRF-001: Novo beneficiário + primeira transferência alta
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'TRF_001_NEW_BENEFICIARY_HIGH_VALUE',
    'Transfer Fraud - Novo Beneficiário + Valor Alto',
    'Detecta primeira transferência para novo beneficiário com valor alto, padrão de APP fraud.',
    'STEP_UP',
    85,
    90,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'TRF_001_NEW_BENEFICIARY_HIGH_VALUE'), 1, 'AND');

-- Condição 1: Beneficiário nunca visto antes
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_001_NEW_BENEFICIARY_HIGH_VALUE') AND group_order = 1),
    1, 'beneficiaryId', 'NOT_IN_HISTORICAL', 'customerAcctNumber:beneficiaryId:90', 'STRING'
);

-- Condição 2: Valor >= 1000
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_001_NEW_BENEFICIARY_HIGH_VALUE') AND group_order = 1),
    2, 'transactionAmount', 'GTE', '1000', 'NUMBER'
);

-- ============================================================================

-- TRF-003: Sequência típica de mule (entrada → saída rápida)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'TRF_003_MULE_PATTERN_FAST_OUT',
    'Transfer Fraud - Padrão de Mula (Entrada + Saída Rápida)',
    'Detecta padrão de conta mula: recebimento de valor seguido de saída rápida para terceiros.',
    'BLOCK',
    90,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'TRF_003_MULE_PATTERN_FAST_OUT'), 1, 'AND');

-- Condição 1: Recebeu transferência nas últimas 2 horas
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_003_MULE_PATTERN_FAST_OUT') AND group_order = 1),
    1, 'customerAcctNumber', 'HAS_INCOMING_TRANSFER_LAST_N_HOURS', '2', 'BOOLEAN'
);

-- Condição 2: Está realizando transferência de saída
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_003_MULE_PATTERN_FAST_OUT') AND group_order = 1),
    2, 'transactionType', 'EQ', 'TRANSFER_OUT', 'STRING'
);

-- Condição 3: Valor de saída >= 80% do valor de entrada
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_003_MULE_PATTERN_FAST_OUT') AND group_order = 1),
    3, 'transactionAmount', 'GTE_PERCENT_OF_LAST_INCOMING', '80', 'NUMBER'
);

-- ============================================================================

-- TRF-004: Estruturação (smurfing) - múltiplas saídas abaixo do limite
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'TRF_004_STRUCTURING_SMURFING',
    'Transfer Fraud - Estruturação (Smurfing)',
    'Detecta múltiplas transferências abaixo do limite de reporte para evitar detecção (smurfing).',
    'REVIEW',
    85,
    90,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'TRF_004_STRUCTURING_SMURFING'), 1, 'AND');

-- Condição 1: Contagem de transferências >= 5 em 24 horas
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_004_STRUCTURING_SMURFING') AND group_order = 1),
    1, 'customerAcctNumber', 'COUNT_LAST_N_HOURS', '5:24', 'NUMBER'
);

-- Condição 2: Cada valor individual < 3000 (abaixo do limite)
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_004_STRUCTURING_SMURFING') AND group_order = 1),
    2, 'transactionAmount', 'LT', '3000', 'NUMBER'
);

-- Condição 3: Soma total >= 10000
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_004_STRUCTURING_SMURFING') AND group_order = 1),
    3, 'transactionAmount', 'SUM_LAST_N_DAYS', '10000:1', 'NUMBER'
);

-- ============================================================================

-- TRF-005: Transferência para jurisdição de alto risco
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'TRF_005_HIGH_RISK_JURISDICTION',
    'Transfer Fraud - Jurisdição de Alto Risco',
    'Detecta transferências para países/jurisdições de alto risco (sanções, AML).',
    'REVIEW',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'TRF_005_HIGH_RISK_JURISDICTION'), 1, 'AND');

-- Condição 1: País do beneficiário em lista FATF de alto risco
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_005_HIGH_RISK_JURISDICTION') AND group_order = 1),
    1, 'beneficiaryCountry', 'IN_LIST', 'IR,KP,MM,SY,YE', 'STRING'  -- Irã, Coreia do Norte, Myanmar, Síria, Iêmen
);

-- Condição 2: Valor >= 500
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_005_HIGH_RISK_JURISDICTION') AND group_order = 1),
    2, 'transactionAmount', 'GTE', '500', 'NUMBER'
);

-- ============================================================================

-- TRF-006: Mudança de chave PIX recente + transferência
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'TRF_006_PIX_KEY_CHANGE_THEN_TRANSFER',
    'Transfer Fraud - Mudança de Chave PIX + Transferência',
    'Detecta mudança recente de chave PIX seguida de transferência, indicando possível takeover.',
    'STEP_UP',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'TRF_006_PIX_KEY_CHANGE_THEN_TRANSFER'), 1, 'AND');

-- Condição 1: Chave PIX foi alterada nos últimos 7 dias
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_006_PIX_KEY_CHANGE_THEN_TRANSFER') AND group_order = 1),
    1, 'customerAcctNumber', 'PIX_KEY_CHANGED_LAST_N_DAYS', '7', 'BOOLEAN'
);

-- Condição 2: Valor >= 500
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TRF_006_PIX_KEY_CHANGE_THEN_TRANSFER') AND group_order = 1),
    2, 'transactionAmount', 'GTE', '500', 'NUMBER'
);

-- ============================================================================
-- CATEGORIA: BOT DETECTION (BOT) - 4 Regras
-- ============================================================================

-- BOT-002: Checkout/payment attempts em alta frequência
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'BOT_002_HIGH_FREQUENCY_CHECKOUT',
    'Bot Detection - Checkout em Alta Frequência',
    'Detecta tentativas de checkout/pagamento em alta frequência, indicando script automatizado.',
    'BLOCK',
    90,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'BOT_002_HIGH_FREQUENCY_CHECKOUT'), 1, 'AND');

-- Condição: Contagem de tentativas >= 10 em 1 minuto
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'BOT_002_HIGH_FREQUENCY_CHECKOUT') AND group_order = 1),
    1, 'deviceId', 'COUNT_LAST_N_HOURS', '10:0.0167', 'NUMBER'  -- 10 em 0.0167 horas (1 minuto)
);

-- ============================================================================

-- BOT-003: User-agent impossível (inconsistência OS/browser)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'BOT_003_IMPOSSIBLE_USER_AGENT',
    'Bot Detection - User-Agent Impossível',
    'Detecta user-agent com inconsistências impossíveis (OS vs browser), indicando bot ou scraper.',
    'BLOCK',
    85,
    90,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'BOT_003_IMPOSSIBLE_USER_AGENT'), 1, 'AND');

-- Condição: User-agent em lista de combinações impossíveis
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'BOT_003_IMPOSSIBLE_USER_AGENT') AND group_order = 1),
    1, 'userAgent', 'IS_IMPOSSIBLE_COMBINATION', 'true', 'BOOLEAN'
);

-- ============================================================================

-- BOT-004: IP de reputação ruim (spam/botnet)
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'BOT_004_BAD_IP_REPUTATION',
    'Bot Detection - IP de Reputação Ruim',
    'Detecta transações originadas de IPs com reputação ruim (spam, botnet, proxy).',
    'REVIEW',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'BOT_004_BAD_IP_REPUTATION'), 1, 'AND');

-- Condição: IP Reputation Score < 30
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'BOT_004_BAD_IP_REPUTATION') AND group_order = 1),
    1, 'ipReputationScore', 'LT', '30', 'NUMBER'
);

-- ============================================================================

-- BOT-008: Múltiplos pagamentos com cartões diferentes em sequência
INSERT INTO complex_rules (
    rule_key,
    rule_name,
    description,
    decision_outcome,
    score_impact,
    priority,
    status,
    shadow_mode,
    created_by
) VALUES (
    'BOT_008_CARDING_BOT_MULTI_CARDS',
    'Bot Detection - Carding Bot (Múltiplos Cartões)',
    'Detecta bot de carding testando múltiplos cartões em sequência rápida.',
    'BLOCK',
    95,
    98,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V29'
);

INSERT INTO rule_condition_groups (complex_rule_id, group_order, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE rule_key = 'BOT_008_CARDING_BOT_MULTI_CARDS'), 1, 'AND');

-- Condição: Device ID com >= 5 PANs distintos em 5 minutos
INSERT INTO rule_conditions (condition_group_id, condition_order, field_name, operator, comparison_value, data_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'BOT_008_CARDING_BOT_MULTI_CARDS') AND group_order = 1),
    1, 'deviceId', 'COUNT_DISTINCT_PANS_LAST_N_HOURS', '5:0.083', 'NUMBER'  -- 5 PANs em 0.083 horas (5 minutos)
);

-- ============================================================================
-- FIM DA MIGRATION V29
-- ============================================================================

-- Total de regras criadas: 25
-- Categorias cobertas:
--   - Card Testing (CT): 6 regras adicionais (CT-005 a CT-010)
--   - KYC Fraud (KYC): 10 regras (KYC-001 a KYC-010)
--   - Transfer Fraud (TRF): 5 regras (TRF-001, TRF-003, TRF-004, TRF-005, TRF-006)
--   - Bot Detection (BOT): 4 regras (BOT-002, BOT-003, BOT-004, BOT-008)
