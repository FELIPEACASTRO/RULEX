-- ============================================================================
-- Migration V30: Regras Avançadas de AML e Account Takeover
-- Data: 2026-01-06
-- Fonte: RULEX_REGRAS_DURAS_A_PARTIR_DAS_URLS.md
-- Objetivo: Implementar 15 regras avançadas de:
--           - Anti-Money Laundering (AML): 10 regras
--           - Account Takeover (ATO): 5 regras adicionais
-- ============================================================================

-- ============================================================================
-- CATEGORIA: ANTI-MONEY LAUNDERING (AML) - 10 Regras
-- ============================================================================

-- AML-001: Transações recorrentes para país de alto risco (FATF)
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_001_HIGH_RISK_COUNTRY_RECURRING',
    'AML - Transações Recorrentes para País de Alto Risco',
    'Detecta transações recorrentes para países na lista FATF de alto risco, indicando possível lavagem de dinheiro.',
    'SUSPEITA_DE_FRAUDE',
    85,
    90,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_001_HIGH_RISK_COUNTRY_RECURRING'), 1, 'AND');

-- Condição 1: País em lista FATF de alto risco
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_001_HIGH_RISK_COUNTRY_RECURRING') AND position = 1),
    1, 'merchantCountryCode', 'IN_LIST', 'IR,KP,MM,SY,YE,AF', 'STRING'
);

-- Condição 2: Contagem de transações >= 3 em 30 dias
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_001_HIGH_RISK_COUNTRY_RECURRING') AND position = 1),
    2, 'customerAcctNumber', 'COUNT_LAST_N_DAYS', '3:30', 'NUMBER'
);

-- ============================================================================

-- AML-002: TBML proxy - Pagamentos comerciais incoerentes
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_002_TBML_PROXY_INCOHERENT_PAYMENTS',
    'AML - TBML Proxy (Pagamentos Incoerentes)',
    'Detecta pagamentos comerciais com valores incoerentes com o perfil do cliente, indicando Trade-Based Money Laundering.',
    'SUSPEITA_DE_FRAUDE',
    80,
    85,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_002_TBML_PROXY_INCOHERENT_PAYMENTS'), 1, 'AND');

-- Condição 1: MCC comercial (atacado, importação)
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_002_TBML_PROXY_INCOHERENT_PAYMENTS') AND position = 1),
    1, 'mcc', 'IN_LIST', '5013,5021,5039,5044,5045,5046,5047,5051,5065,5072,5074,5085,5094,5099', 'NUMBER'
);

-- Condição 2: Valor > 5x a média histórica do cliente
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_002_TBML_PROXY_INCOHERENT_PAYMENTS') AND position = 1),
    2, 'transactionAmount', 'GT', 'AVG:customerAcctNumber:30:5', 'NUMBER'
);

-- ============================================================================

-- AML-003: Rapid movement across accounts (layering)
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_003_RAPID_LAYERING',
    'AML - Movimentação Rápida entre Contas (Layering)',
    'Detecta movimentação rápida de valores entre múltiplas contas, técnica de layering para dificultar rastreamento.',
    'SUSPEITA_DE_FRAUDE',
    90,
    95,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_003_RAPID_LAYERING'), 1, 'AND');

-- Condição 1: Recebeu transferência nas últimas 2 horas
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_003_RAPID_LAYERING') AND position = 1),
    1, 'customerAcctNumber', 'HAS_INCOMING_TRANSFER_LAST_N_HOURS', '2', 'BOOLEAN'
);

-- Condição 2: Está realizando transferência para terceiro
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_003_RAPID_LAYERING') AND position = 1),
    2, 'transactionType', 'EQ', 'TRANSFER_OUT', 'STRING'
);

-- Condição 3: Contagem de transferências >= 3 em 24 horas
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_003_RAPID_LAYERING') AND position = 1),
    3, 'customerAcctNumber', 'COUNT_LAST_N_HOURS', '3:24', 'NUMBER'
);

-- ============================================================================

-- AML-004: Uso intenso de múltiplos instrumentos
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_004_MULTI_INSTRUMENT_INTENSIVE_USE',
    'AML - Uso Intenso de Múltiplos Instrumentos',
    'Detecta uso intenso de múltiplos instrumentos (cartão + transferência + cash-out) para dificultar rastreamento.',
    'SUSPEITA_DE_FRAUDE',
    80,
    85,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_004_MULTI_INSTRUMENT_INTENSIVE_USE'), 1, 'AND');

-- Condição 1: Usou >= 3 tipos de instrumento em 7 dias
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_004_MULTI_INSTRUMENT_INTENSIVE_USE') AND position = 1),
    1, 'customerAcctNumber', 'COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS', '3:7', 'NUMBER'
);

-- Condição 2: Soma total >= 5000
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_004_MULTI_INSTRUMENT_INTENSIVE_USE') AND position = 1),
    2, 'transactionAmount', 'SUM_LAST_N_DAYS', '5000:7', 'NUMBER'
);

-- ============================================================================

-- AML-005: Screening sanções - Match forte
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_005_SANCTIONS_SCREENING_STRONG_MATCH',
    'AML - Screening de Sanções (Match Forte)',
    'Detecta match forte em listas de sanções (OFAC, ONU, UE) por nome, data de nascimento e país.',
    'FRAUDE',
    100,
    100,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_005_SANCTIONS_SCREENING_STRONG_MATCH'), 1, 'AND');

-- Condição: Sanctions Match Score >= 90
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_005_SANCTIONS_SCREENING_STRONG_MATCH') AND position = 1),
    1, 'sanctionsMatchScore', 'GTE', '90', 'NUMBER'
);

-- ============================================================================

-- AML-006: Structuring para evitar reporte (CTR)
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_006_STRUCTURING_CTR_AVOIDANCE',
    'AML - Estruturação para Evitar CTR',
    'Detecta estruturação de transações para evitar reporte de CTR (Currency Transaction Report).',
    'SUSPEITA_DE_FRAUDE',
    85,
    90,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_006_STRUCTURING_CTR_AVOIDANCE'), 1, 'AND');

-- Condição 1: Múltiplas transações >= 5 em 24 horas
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_006_STRUCTURING_CTR_AVOIDANCE') AND position = 1),
    1, 'customerAcctNumber', 'COUNT_LAST_N_HOURS', '5:24', 'NUMBER'
);

-- Condição 2: Cada transação < 10000 (abaixo do limite CTR)
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_006_STRUCTURING_CTR_AVOIDANCE') AND position = 1),
    2, 'transactionAmount', 'LT', '10000', 'NUMBER'
);

-- Condição 3: Soma total >= 10000
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_006_STRUCTURING_CTR_AVOIDANCE') AND position = 1),
    3, 'transactionAmount', 'SUM_LAST_N_DAYS', '10000:1', 'NUMBER'
);

-- ============================================================================

-- AML-007: Ransomware payment patterns
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_007_RANSOMWARE_PAYMENT_PATTERN',
    'AML - Padrão de Pagamento de Ransomware',
    'Detecta padrões de pagamento típicos de ransomware (cripto, valor específico, urgência).',
    'FRAUDE',
    95,
    98,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_007_RANSOMWARE_PAYMENT_PATTERN'), 1, 'AND');

-- Condição 1: MCC de criptomoeda
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_007_RANSOMWARE_PAYMENT_PATTERN') AND position = 1),
    1, 'mcc', 'EQ', '6051', 'NUMBER'
);

-- Condição 2: Valor específico (múltiplo de 0.5 BTC, por exemplo)
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_007_RANSOMWARE_PAYMENT_PATTERN') AND position = 1),
    2, 'transactionAmount', 'IS_CRYPTO_RANSOM_AMOUNT', 'true', 'BOOLEAN'
);

-- Condição 3: Primeira transação de cripto do cliente
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_007_RANSOMWARE_PAYMENT_PATTERN') AND position = 1),
    3, 'customerAcctNumber', 'COUNT_CRYPTO_TXN_LAST_N_DAYS', '1:365', 'NUMBER'
);

-- ============================================================================

-- AML-008: Conta potencial mule
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_008_POTENTIAL_MULE_ACCOUNT',
    'AML - Conta Potencial Mula',
    'Detecta conta com padrão de mula: muitos pagadores diferentes + retiradas rápidas.',
    'SUSPEITA_DE_FRAUDE',
    90,
    95,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_008_POTENTIAL_MULE_ACCOUNT'), 1, 'AND');

-- Condição 1: Recebeu de >= 5 pagadores distintos em 7 dias
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_008_POTENTIAL_MULE_ACCOUNT') AND position = 1),
    1, 'customerAcctNumber', 'COUNT_DISTINCT_PAYERS_LAST_N_DAYS', '5:7', 'NUMBER'
);

-- Condição 2: Taxa de saída >= 80% do total recebido
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_008_POTENTIAL_MULE_ACCOUNT') AND position = 1),
    2, 'customerAcctNumber', 'OUTFLOW_RATE_LAST_N_DAYS', '80:7', 'NUMBER'
);

-- ============================================================================

-- AML-009: Inconsistência de perfil (baixa renda + volume alto)
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_009_PROFILE_INCONSISTENCY_INCOME_VOLUME',
    'AML - Inconsistência de Perfil (Renda vs Volume)',
    'Detecta inconsistência entre perfil de baixa renda e volume alto de transações.',
    'SUSPEITA_DE_FRAUDE',
    80,
    85,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_009_PROFILE_INCONSISTENCY_INCOME_VOLUME'), 1, 'AND');

-- Condição 1: Renda declarada < 3000
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_009_PROFILE_INCONSISTENCY_INCOME_VOLUME') AND position = 1),
    1, 'declaredIncome', 'LT', '3000', 'NUMBER'
);

-- Condição 2: Soma de transações >= 10000 em 30 dias
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_009_PROFILE_INCONSISTENCY_INCOME_VOLUME') AND position = 1),
    2, 'transactionAmount', 'SUM_LAST_N_DAYS', '10000:30', 'NUMBER'
);

-- ============================================================================

-- AML-010: Transações com finalidade incoerente
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'AML_010_INCOHERENT_PURPOSE',
    'AML - Finalidade Incoerente',
    'Detecta transações com descrição/finalidade suspeita ou incoerente.',
    'SUSPEITA_DE_FRAUDE',
    70,
    75,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'AML_010_INCOHERENT_PURPOSE'), 1, 'AND');

-- Condição: Descrição contém palavras suspeitas
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'AML_010_INCOHERENT_PURPOSE') AND position = 1),
    1, 'transactionDescription', 'CONTAINS_SUSPICIOUS_KEYWORDS', 'true', 'BOOLEAN'
);

-- ============================================================================
-- CATEGORIA: ACCOUNT TAKEOVER (ATO) - 5 Regras Adicionais
-- ============================================================================

-- ATO-006: MFA fatigue (múltiplos prompts negados + 1 aceito)
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'ATO_006_MFA_FATIGUE_ATTACK',
    'Account Takeover - MFA Fatigue Attack',
    'Detecta ataque de MFA fatigue: múltiplos prompts negados seguidos de 1 aceito, indicando cansaço da vítima.',
    'FRAUDE',
    95,
    98,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'ATO_006_MFA_FATIGUE_ATTACK'), 1, 'AND');

-- Condição 1: Contagem de MFA negados >= 5 em 1 hora
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_006_MFA_FATIGUE_ATTACK') AND position = 1),
    1, 'customerAcctNumber', 'COUNT_MFA_DENIALS_LAST_N_HOURS', '5:1', 'NUMBER'
);

-- Condição 2: MFA foi aceito agora
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_006_MFA_FATIGUE_ATTACK') AND position = 1),
    2, 'mfaCompleted', 'EQ', 'true', 'BOOLEAN'
);

-- ============================================================================

-- ATO-007: Sessão anômala (mudança de device no meio da sessão)
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'ATO_007_SESSION_DEVICE_CHANGE',
    'Account Takeover - Mudança de Device na Sessão',
    'Detecta mudança de device ou fingerprint no meio de uma sessão ativa, indicando takeover.',
    'FRAUDE',
    90,
    95,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'ATO_007_SESSION_DEVICE_CHANGE'), 1, 'AND');

-- Condição: Device ID mudou durante a sessão
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_007_SESSION_DEVICE_CHANGE') AND position = 1),
    1, 'sessionId', 'DEVICE_CHANGED_IN_SESSION', 'true', 'BOOLEAN'
);

-- ============================================================================

-- ATO-008: Login de datacenter/proxy + ação financeira
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'ATO_008_DATACENTER_LOGIN_FINANCIAL_ACTION',
    'Account Takeover - Login de Datacenter + Ação Financeira',
    'Detecta login de datacenter/proxy seguido de ação financeira, indicando bot ou fraudador.',
    'FRAUDE',
    90,
    95,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'ATO_008_DATACENTER_LOGIN_FINANCIAL_ACTION'), 1, 'AND');

-- Condição 1: IP é de datacenter/proxy
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_008_DATACENTER_LOGIN_FINANCIAL_ACTION') AND position = 1),
    1, 'ipType', 'IN_LIST', 'DATACENTER,PROXY,VPN,TOR', 'STRING'
);

-- Condição 2: Realizando transação financeira
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_008_DATACENTER_LOGIN_FINANCIAL_ACTION') AND position = 1),
    2, 'transactionType', 'IN_LIST', 'PURCHASE,TRANSFER_OUT,WITHDRAWAL', 'STRING'
);

-- ============================================================================

-- ATO-009: Conta inativa reativada + transação imediata
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'ATO_009_DORMANT_ACCOUNT_REACTIVATION',
    'Account Takeover - Reativação de Conta Inativa',
    'Detecta conta inativa por longo período que é reativada e realiza transação imediata.',
    'SUSPEITA_DE_FRAUDE',
    85,
    90,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'ATO_009_DORMANT_ACCOUNT_REACTIVATION'), 1, 'AND');

-- Condição 1: Conta estava inativa por >= 90 dias
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_009_DORMANT_ACCOUNT_REACTIVATION') AND position = 1),
    1, 'customerAcctNumber', 'DAYS_SINCE_LAST_ACTIVITY', '90', 'NUMBER'
);

-- Condição 2: Valor >= 200
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_009_DORMANT_ACCOUNT_REACTIVATION') AND position = 1),
    2, 'transactionAmount', 'GTE', '200', 'NUMBER'
);

-- ============================================================================

-- ATO-010: Múltiplos user-agents incompatíveis
INSERT INTO complex_rules (
    key,
    title,
    description,
    decision,
    severity,
    priority,
    status,
    enabled,
    created_by
) VALUES (
    'ATO_010_INCOMPATIBLE_USER_AGENTS',
    'Account Takeover - User-Agents Incompatíveis',
    'Detecta tentativas de acesso com múltiplos user-agents incompatíveis, indicando bot ou takeover.',
    'SUSPEITA_DE_FRAUDE',
    80,
    85,
    'PUBLISHED',
    false,
    NULL
);

INSERT INTO rule_condition_groups (complex_rule_id, position, logic_operator)
VALUES ((SELECT id FROM complex_rules WHERE key = 'ATO_010_INCOMPATIBLE_USER_AGENTS'), 1, 'AND');

-- Condição: Contagem de user-agents distintos >= 3 em 1 hora
INSERT INTO rule_conditions (group_id, position, field_name, operator, value_single, value_type)
VALUES (
    (SELECT id FROM rule_condition_groups WHERE complex_rule_id = (SELECT id FROM complex_rules WHERE key = 'ATO_010_INCOMPATIBLE_USER_AGENTS') AND position = 1),
    1, 'customerAcctNumber', 'COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS', '3:1', 'NUMBER'
);

-- ============================================================================
-- FIM DA MIGRATION V30
-- ============================================================================

-- Total de regras criadas: 15
-- Categorias cobertas:
--   - Anti-Money Laundering (AML): 10 regras (AML-001 a AML-010)
--   - Account Takeover (ATO): 5 regras adicionais (ATO-006 a ATO-010)
