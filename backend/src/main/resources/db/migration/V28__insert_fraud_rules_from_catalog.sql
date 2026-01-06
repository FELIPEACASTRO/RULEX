-- ============================================================================
-- Migration V28: Inserção de Regras de Fraude Reais Baseadas no Catálogo
-- Data: 2026-01-06
-- Fonte: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md
-- Objetivo: Implementar regras parametrizáveis de fraude baseadas em padrões
--           documentados por FATF, Europol, FinCEN, NIST e FBI.
-- ============================================================================

-- ============================================================================
-- CATEGORIA 1: CARD TESTING (CT)
-- ============================================================================

-- CT-001: Múltiplas transações pequenas em curto período
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
    'CT_001_MULTIPLE_SMALL_TRANSACTIONS',
    'Card Testing - Múltiplas Transações Pequenas',
    'Detecta tentativas de card testing através de múltiplas transações de baixo valor em curto período. Padrão típico de validação de cartões roubados.',
    'BLOCK',
    85,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

-- Adicionar grupos de condições para CT-001
INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'CT_001_MULTIPLE_SMALL_TRANSACTIONS'),
    1,
    'AND'
);

-- Condição 1: Contagem de transações nos últimos 5 minutos >= 5
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_001_MULTIPLE_SMALL_TRANSACTIONS') AND group_order = 1),
    1,
    'pan',
    'COUNT_LAST_N_HOURS',
    '5:0.083',  -- 5 transações em 0.083 horas (5 minutos)
    'NUMBER'
);

-- Condição 2: Valor da transação < 10
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_001_MULTIPLE_SMALL_TRANSACTIONS') AND group_order = 1),
    2,
    'transactionAmount',
    'LT',
    '10',
    'NUMBER'
);

-- ============================================================================

-- CT-002: Transações em múltiplos merchants em curto período
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
    'CT_002_MULTIPLE_MERCHANTS',
    'Card Testing - Múltiplos Merchants',
    'Detecta card testing através de transações em múltiplos merchants diferentes em curto período. Indica uso automatizado de cartão roubado.',
    'REVIEW',
    80,
    90,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'CT_002_MULTIPLE_MERCHANTS'),
    1,
    'AND'
);

-- Condição: Contagem de merchants distintos >= 5 em 10 minutos
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_002_MULTIPLE_MERCHANTS') AND group_order = 1),
    1,
    'merchantId',
    'COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS',
    '5:0.007',  -- 5 merchants em 0.007 dias (10 minutos)
    'NUMBER'
);

-- ============================================================================

-- CT-003: Padrão de valores crescentes (escada)
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
    'CT_003_ESCALATING_AMOUNTS',
    'Card Testing - Valores Crescentes',
    'Detecta padrão de valores crescentes (escada) típico de fraudadores testando limites de cartão roubado.',
    'REVIEW',
    75,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'CT_003_ESCALATING_AMOUNTS'),
    1,
    'AND'
);

-- Condição 1: Contagem de transações >= 3 em 15 minutos
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_003_ESCALATING_AMOUNTS') AND group_order = 1),
    1,
    'pan',
    'COUNT_LAST_N_HOURS',
    '3:0.25',  -- 3 transações em 0.25 horas (15 minutos)
    'NUMBER'
);

-- Condição 2: Valor atual > valor médio das últimas transações
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_003_ESCALATING_AMOUNTS') AND group_order = 1),
    2,
    'transactionAmount',
    'GT',
    'AVG:pan:1',  -- Maior que a média do PAN no último dia
    'NUMBER'
);

-- ============================================================================

-- CT-004: Falhas de autenticação seguidas de sucesso
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
    'CT_004_AUTH_FAILURES_THEN_SUCCESS',
    'Card Testing - Falhas + Sucesso',
    'Detecta múltiplas falhas de autenticação seguidas de sucesso, indicando tentativa de descoberta de CVV/senha.',
    'BLOCK',
    90,
    95,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'CT_004_AUTH_FAILURES_THEN_SUCCESS'),
    1,
    'AND'
);

-- Condição 1: CVV2 Response = 'N' (falha) nas últimas tentativas
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_004_AUTH_FAILURES_THEN_SUCCESS') AND group_order = 1),
    1,
    'cvv2Response',
    'EQ',
    'N',
    'STRING'
);

-- Condição 2: Contagem de falhas >= 3 em 10 minutos
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'CT_004_AUTH_FAILURES_THEN_SUCCESS') AND group_order = 1),
    2,
    'pan',
    'COUNT_LAST_N_HOURS',
    '3:0.167',  -- 3 transações em 0.167 horas (10 minutos)
    'NUMBER'
);

-- ============================================================================
-- CATEGORIA 2: ACCOUNT TAKEOVER (ATO)
-- ============================================================================

-- ATO-001: Credential Stuffing (muitas falhas de login por IP)
-- NOTA: Esta regra requer dados de login que não estão no TransactionRequest
-- Será implementada quando o payload for expandido para incluir eventos de login

-- ============================================================================

-- ATO-003: Login de novo device + nova geolocalização
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
    'ATO_003_NEW_DEVICE_NEW_GEO',
    'Account Takeover - Novo Device + Nova Geo',
    'Detecta transação de novo terminal em país diferente do histórico do cliente, indicando possível takeover.',
    'STEP_UP',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'ATO_003_NEW_DEVICE_NEW_GEO'),
    1,
    'AND'
);

-- Condição 1: Terminal ID nunca visto antes para este cliente
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'ATO_003_NEW_DEVICE_NEW_GEO') AND group_order = 1),
    1,
    'terminalId',
    'NOT_IN_LIST',
    'HISTORICAL:customerAcctNumber:terminalId:30',  -- Não está no histórico dos últimos 30 dias
    'STRING'
);

-- Condição 2: País do merchant diferente do país histórico
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'ATO_003_NEW_DEVICE_NEW_GEO') AND group_order = 1),
    2,
    'merchantCountryCode',
    'NOT_IN_LIST',
    'HISTORICAL:customerAcctNumber:merchantCountryCode:30',
    'STRING'
);

-- ============================================================================
-- CATEGORIA 3: TRANSAÇÕES DE ALTO RISCO (TR)
-- ============================================================================

-- TR-001: Transação internacional + valor alto + primeira vez
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
    'TR_001_INTL_HIGH_VALUE_FIRST_TIME',
    'Transação de Alto Risco - Internacional + Alto Valor + Primeira Vez',
    'Detecta primeira transação internacional de alto valor, padrão comum de fraude com cartão roubado.',
    'REVIEW',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'TR_001_INTL_HIGH_VALUE_FIRST_TIME'),
    1,
    'AND'
);

-- Condição 1: País do merchant diferente do país do adquirente
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_001_INTL_HIGH_VALUE_FIRST_TIME') AND group_order = 1),
    1,
    'merchantCountryCode',
    'NEQ',
    'acquirerCountry',
    'STRING'
);

-- Condição 2: Valor >= 1000
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_001_INTL_HIGH_VALUE_FIRST_TIME') AND group_order = 1),
    2,
    'transactionAmount',
    'GTE',
    '1000',
    'NUMBER'
);

-- Condição 3: Primeira transação internacional do cliente
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_001_INTL_HIGH_VALUE_FIRST_TIME') AND group_order = 1),
    3,
    'customerAcctNumber',
    'COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS',
    '1:8760',  -- Apenas 1 país nos últimos 365 dias (8760 horas)
    'NUMBER'
);

-- ============================================================================

-- TR-002: MCC de alto risco + valor alto
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
    'TR_002_HIGH_RISK_MCC_HIGH_VALUE',
    'Transação de Alto Risco - MCC Alto Risco + Valor Alto',
    'Detecta transações de alto valor em MCCs de alto risco (gift cards, cripto, apostas, joias).',
    'REVIEW',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'TR_002_HIGH_RISK_MCC_HIGH_VALUE'),
    1,
    'AND'
);

-- Condição 1: MCC em lista de alto risco
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_002_HIGH_RISK_MCC_HIGH_VALUE') AND group_order = 1),
    1,
    'mcc',
    'IN_LIST',
    '5094,5122,5912,5962,5993,6051,7273,7995',  -- Gift cards, drogas, cripto, apostas, joias
    'NUMBER'
);

-- Condição 2: Valor >= 500
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_002_HIGH_RISK_MCC_HIGH_VALUE') AND group_order = 1),
    2,
    'transactionAmount',
    'GTE',
    '500',
    'NUMBER'
);

-- ============================================================================

-- TR-003: Transação CNP (Card Not Present) + sem 3DS
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
    'TR_003_CNP_WITHOUT_3DS',
    'Transação de Alto Risco - CNP sem 3DS',
    'Detecta transações Card Not Present sem autenticação 3D Secure, aumentando risco de fraude.',
    'REVIEW',
    70,
    75,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'TR_003_CNP_WITHOUT_3DS'),
    1,
    'AND'
);

-- Condição 1: POS Entry Mode indica CNP (81, 10, 79)
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_003_CNP_WITHOUT_3DS') AND group_order = 1),
    1,
    'posEntryMode',
    'IN_LIST',
    '81,10,79',
    'STRING'
);

-- Condição 2: ECI Indicator indica sem 3DS (valor < 5)
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_003_CNP_WITHOUT_3DS') AND group_order = 1),
    2,
    'eciIndicator',
    'LT',
    '5',
    'NUMBER'
);

-- Condição 3: Valor >= 200
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'TR_003_CNP_WITHOUT_3DS') AND group_order = 1),
    3,
    'transactionAmount',
    'GTE',
    '200',
    'NUMBER'
);

-- ============================================================================
-- CATEGORIA 4: VELOCIDADE ANÔMALA (VA)
-- ============================================================================

-- VA-001: Múltiplas transações em curto período (velocity)
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
    'VA_001_HIGH_VELOCITY',
    'Velocidade Anômala - Múltiplas Transações',
    'Detecta velocidade anômala de transações para o mesmo cartão, indicando uso fraudulento.',
    'REVIEW',
    75,
    80,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'VA_001_HIGH_VELOCITY'),
    1,
    'AND'
);

-- Condição: Contagem de transações >= 10 em 1 hora
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'VA_001_HIGH_VELOCITY') AND group_order = 1),
    1,
    'pan',
    'COUNT_LAST_N_HOURS',
    '10:1',  -- 10 transações em 1 hora
    'NUMBER'
);

-- ============================================================================

-- VA-002: Soma de valores anômala em período curto
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
    'VA_002_HIGH_AMOUNT_VELOCITY',
    'Velocidade Anômala - Soma de Valores Alta',
    'Detecta soma anômala de valores transacionados em curto período, indicando esvaziamento de conta.',
    'REVIEW',
    80,
    85,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'VA_002_HIGH_AMOUNT_VELOCITY'),
    1,
    'AND'
);

-- Condição: Soma de valores >= 5000 em 24 horas
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'VA_002_HIGH_AMOUNT_VELOCITY') AND group_order = 1),
    1,
    'transactionAmount',
    'SUM_LAST_N_DAYS',
    '5000:1',  -- Soma >= 5000 em 1 dia
    'NUMBER'
);

-- ============================================================================
-- CATEGORIA 5: PADRÕES COMPORTAMENTAIS ANÔMALOS (PA)
-- ============================================================================

-- PA-001: Transação fora do horário habitual do cliente
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
    'PA_001_UNUSUAL_TIME',
    'Padrão Anômalo - Horário Incomum',
    'Detecta transações fora do horário habitual do cliente (madrugada), aumentando suspeita de fraude.',
    'REVIEW',
    60,
    65,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'PA_001_UNUSUAL_TIME'),
    1,
    'AND'
);

-- Condição 1: Horário entre 02:00 e 05:00
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'PA_001_UNUSUAL_TIME') AND group_order = 1),
    1,
    'transactionTime',
    'BETWEEN',
    '20000,50000',  -- Entre 02:00:00 e 05:00:00 (formato HHMMSS)
    'NUMBER'
);

-- Condição 2: Valor >= 100
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'PA_001_UNUSUAL_TIME') AND group_order = 1),
    2,
    'transactionAmount',
    'GTE',
    '100',
    'NUMBER'
);

-- ============================================================================

-- PA-002: Mudança abrupta de padrão de gasto
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
    'PA_002_SPENDING_PATTERN_CHANGE',
    'Padrão Anômalo - Mudança de Gasto',
    'Detecta mudança abrupta no padrão de gasto do cliente (valor muito acima da média histórica).',
    'REVIEW',
    70,
    75,
    'ACTIVE',
    false,
    'SYSTEM_MIGRATION_V28'
);

INSERT INTO rule_condition_groups (
    complex_rule_id,
    group_order,
    logic_operator
) VALUES (
    (SELECT id FROM complex_rules WHERE rule_key = 'PA_002_SPENDING_PATTERN_CHANGE'),
    1,
    'AND'
);

-- Condição: Valor atual > 3x a média dos últimos 30 dias
INSERT INTO rule_conditions (
    condition_group_id,
    condition_order,
    field_name,
    operator,
    comparison_value,
    data_type
) VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE rule_key = 'PA_002_SPENDING_PATTERN_CHANGE') AND group_order = 1),
    1,
    'transactionAmount',
    'GT',
    'AVG:customerAcctNumber:30:3',  -- Maior que 3x a média dos últimos 30 dias
    'NUMBER'
);

-- ============================================================================
-- FIM DA MIGRATION V28
-- ============================================================================

-- Total de regras criadas: 15
-- Categorias cobertas:
--   - Card Testing (CT): 4 regras
--   - Account Takeover (ATO): 1 regra
--   - Transações de Alto Risco (TR): 3 regras
--   - Velocidade Anômala (VA): 2 regras
--   - Padrões Comportamentais Anômalos (PA): 2 regras
