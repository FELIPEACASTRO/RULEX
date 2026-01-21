-- ============================================================================
-- Migration V40: 100 Regras SIMPLES de Fraude (Condição Única)
-- Data: 2026-01-06
-- Objetivo: Implementar 100 regras de detecção de fraude com condição única
--           para cobertura ampla de padrões básicos de fraude
-- ============================================================================

CREATE TEMP TABLE rule_configurations (
  rule_name VARCHAR(100) NOT NULL,
  description TEXT,
  field_name TEXT,
  operator TEXT,
  comparison_value TEXT,
  decision_outcome TEXT,
  score_impact INTEGER,
  priority INTEGER,
  status TEXT,
  shadow_mode BOOLEAN,
  created_by TEXT
);

-- ============================================================================
-- CATEGORIA 1: REGRAS DE VALOR (20 regras)
-- ============================================================================

-- S001: Transação de valor muito alto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S001_HIGH_VALUE_TRANSACTION', 'Transação com valor acima de R$ 10.000', 'transactionAmount', 'GT', '10000', 'REVIEW', 70, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S002: Transação de valor extremamente alto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S002_EXTREME_HIGH_VALUE', 'Transação com valor acima de R$ 50.000', 'transactionAmount', 'GT', '50000', 'REVIEW', 85, 90, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S003: Transação de valor crítico
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S003_CRITICAL_HIGH_VALUE', 'Transação com valor acima de R$ 100.000', 'transactionAmount', 'GT', '100000', 'BLOCK', 95, 95, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S004: Transação de valor redondo suspeito (múltiplo de 1000)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S004_ROUND_AMOUNT_1000', 'Transação com valor múltiplo de R$ 1.000', 'transactionAmount', 'MODULO_ZERO', '1000', 'REVIEW', 40, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S005: Transação de valor redondo suspeito (múltiplo de 500)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S005_ROUND_AMOUNT_500', 'Transação com valor múltiplo de R$ 500', 'transactionAmount', 'MODULO_ZERO', '500', 'REVIEW', 30, 40, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S006: Transação abaixo do limite de reporte (structuring)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S006_JUST_BELOW_CTR_LIMIT', 'Transação entre R$ 9.000 e R$ 9.999 (abaixo do limite CTR)', 'transactionAmount', 'BETWEEN', '9000,9999', 'REVIEW', 60, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S007: Micro-transação suspeita (card testing)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S007_MICRO_TRANSACTION', 'Transação com valor abaixo de R$ 1', 'transactionAmount', 'LT', '1', 'REVIEW', 50, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S008: Transação de valor zero
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S008_ZERO_AMOUNT', 'Transação com valor igual a zero', 'transactionAmount', 'EQ', '0', 'BLOCK', 90, 95, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S009: Transação de valor negativo
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S009_NEGATIVE_AMOUNT', 'Transação com valor negativo', 'transactionAmount', 'LT', '0', 'BLOCK', 100, 100, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S010: Transação com centavos específicos (99 centavos - psicológico)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S010_PSYCHOLOGICAL_PRICING', 'Transação terminando em .99 (preço psicológico)', 'transactionAmount', 'ENDS_WITH', '.99', 'APPROVE', 0, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S011: Valor acima do crédito disponível
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S011_OVER_AVAILABLE_CREDIT', 'Transação acima do crédito disponível', 'transactionAmount', 'FIELD_GT', 'availableCredit', 'BLOCK', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S012: Valor igual ao crédito disponível (bust-out)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S012_EXACT_AVAILABLE_CREDIT', 'Transação igual ao crédito disponível', 'transactionAmount', 'FIELD_EQ', 'availableCredit', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S013: Valor entre 90-99% do crédito disponível
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S013_NEAR_CREDIT_LIMIT', 'Transação entre 90-99% do crédito disponível', 'transactionAmount', 'PERCENTAGE_OF_FIELD', 'availableCredit:90:99', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S014: Valor com muitas casas decimais (fraude de arredondamento)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S014_EXCESSIVE_DECIMALS', 'Transação com mais de 2 casas decimais', 'transactionAmount', 'DECIMAL_PLACES_GT', '2', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S015: Valor em moeda estrangeira alto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S015_HIGH_FOREIGN_CURRENCY', 'Transação em moeda estrangeira acima de USD 5.000', 'transactionAmount', 'GT', '5000', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S016: Valor típico de gift card (50, 100, 200, 500)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S016_GIFT_CARD_AMOUNT', 'Transação com valor típico de gift card', 'transactionAmount', 'IN', '50,100,200,500', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S017: Valor típico de ransomware (BTC equivalente)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S017_RANSOM_AMOUNT', 'Transação com valor típico de resgate', 'transactionAmount', 'IN', '500,1000,2000,5000,10000', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S018: Valor abaixo do ticket médio (possível teste)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S018_BELOW_AVERAGE_TICKET', 'Transação abaixo de R$ 10', 'transactionAmount', 'LT', '10', 'REVIEW', 35, 40, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S019: Valor de saque alto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S019_HIGH_WITHDRAWAL', 'Saque acima de R$ 5.000', 'transactionAmount', 'GT', '5000', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S020: Valor de transferência alto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S020_HIGH_TRANSFER', 'Transferência acima de R$ 20.000', 'transactionAmount', 'GT', '20000', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- ============================================================================
-- CATEGORIA 2: REGRAS DE MCC (20 regras)
-- ============================================================================

-- S021: MCC de criptomoeda
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S021_MCC_CRYPTO', 'Transação em exchange de criptomoeda (MCC 6051)', 'mcc', 'EQ', '6051', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S022: MCC de apostas/gambling
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S022_MCC_GAMBLING', 'Transação em casa de apostas (MCC 7995)', 'mcc', 'EQ', '7995', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S023: MCC de joalheria
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S023_MCC_JEWELRY', 'Transação em joalheria (MCC 5944)', 'mcc', 'EQ', '5944', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S024: MCC de eletrônicos
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S024_MCC_ELECTRONICS', 'Transação em loja de eletrônicos (MCC 5732)', 'mcc', 'EQ', '5732', 'REVIEW', 45, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S025: MCC de gift cards
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S025_MCC_GIFT_CARDS', 'Transação em loja de gift cards (MCC 5815)', 'mcc', 'EQ', '5815', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S026: MCC de transferência de dinheiro
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S026_MCC_MONEY_TRANSFER', 'Transação em serviço de transferência (MCC 6012)', 'mcc', 'EQ', '6012', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S027: MCC de casa de câmbio
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S027_MCC_CURRENCY_EXCHANGE', 'Transação em casa de câmbio (MCC 6211)', 'mcc', 'EQ', '6211', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S028: MCC de serviços financeiros
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S028_MCC_FINANCIAL_SERVICES', 'Transação em serviços financeiros (MCC 6010-6099)', 'mcc', 'BETWEEN', '6010,6099', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S029: MCC de ATM
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S029_MCC_ATM', 'Transação em ATM (MCC 6011)', 'mcc', 'EQ', '6011', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S030: MCC de compra de veículos
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S030_MCC_VEHICLE', 'Transação em concessionária (MCC 5511)', 'mcc', 'EQ', '5511', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S031: MCC de agência de viagens
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S031_MCC_TRAVEL_AGENCY', 'Transação em agência de viagens (MCC 4722)', 'mcc', 'EQ', '4722', 'REVIEW', 45, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S032: MCC de companhia aérea
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S032_MCC_AIRLINE', 'Transação em companhia aérea (MCC 3000-3299)', 'mcc', 'BETWEEN', '3000,3299', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S033: MCC de hotel
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S033_MCC_HOTEL', 'Transação em hotel (MCC 3501-3999)', 'mcc', 'BETWEEN', '3501,3999', 'REVIEW', 35, 40, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S034: MCC de aluguel de carro
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S034_MCC_CAR_RENTAL', 'Transação em locadora de veículos (MCC 7512)', 'mcc', 'EQ', '7512', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S035: MCC de farmácia
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S035_MCC_PHARMACY', 'Transação em farmácia (MCC 5912)', 'mcc', 'EQ', '5912', 'APPROVE', 0, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S036: MCC de supermercado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S036_MCC_SUPERMARKET', 'Transação em supermercado (MCC 5411)', 'mcc', 'EQ', '5411', 'APPROVE', 0, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S037: MCC de posto de gasolina
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S037_MCC_GAS_STATION', 'Transação em posto de gasolina (MCC 5541)', 'mcc', 'EQ', '5541', 'APPROVE', 0, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S038: MCC de restaurante
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S038_MCC_RESTAURANT', 'Transação em restaurante (MCC 5812)', 'mcc', 'EQ', '5812', 'APPROVE', 0, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S039: MCC de entretenimento adulto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S039_MCC_ADULT_ENTERTAINMENT', 'Transação em entretenimento adulto (MCC 5967)', 'mcc', 'EQ', '5967', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S040: MCC de tabacaria
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S040_MCC_TOBACCO', 'Transação em tabacaria (MCC 5993)', 'mcc', 'EQ', '5993', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- ============================================================================
-- CATEGORIA 3: REGRAS DE PAÍS (20 regras)
-- ============================================================================

-- S041: País de alto risco FATF (Irã)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S041_COUNTRY_IRAN', 'Transação originada do Irã (IR)', 'merchantCountryCode', 'EQ', 'IR', 'BLOCK', 100, 100, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S042: País de alto risco FATF (Coreia do Norte)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S042_COUNTRY_NORTH_KOREA', 'Transação originada da Coreia do Norte (KP)', 'merchantCountryCode', 'EQ', 'KP', 'BLOCK', 100, 100, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S043: País de alto risco FATF (Myanmar)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S043_COUNTRY_MYANMAR', 'Transação originada de Myanmar (MM)', 'merchantCountryCode', 'EQ', 'MM', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S044: País de alto risco FATF (Síria)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S044_COUNTRY_SYRIA', 'Transação originada da Síria (SY)', 'merchantCountryCode', 'EQ', 'SY', 'BLOCK', 100, 100, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S045: País de alto risco FATF (Iêmen)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S045_COUNTRY_YEMEN', 'Transação originada do Iêmen (YE)', 'merchantCountryCode', 'EQ', 'YE', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S046: País de alto risco FATF (Afeganistão)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S046_COUNTRY_AFGHANISTAN', 'Transação originada do Afeganistão (AF)', 'merchantCountryCode', 'EQ', 'AF', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S047: País de risco médio (Rússia)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S047_COUNTRY_RUSSIA', 'Transação originada da Rússia (RU)', 'merchantCountryCode', 'EQ', 'RU', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S048: País de risco médio (Belarus)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S048_COUNTRY_BELARUS', 'Transação originada de Belarus (BY)', 'merchantCountryCode', 'EQ', 'BY', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S049: País de risco médio (Venezuela)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S049_COUNTRY_VENEZUELA', 'Transação originada da Venezuela (VE)', 'merchantCountryCode', 'EQ', 'VE', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S050: País de risco médio (Cuba)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S050_COUNTRY_CUBA', 'Transação originada de Cuba (CU)', 'merchantCountryCode', 'EQ', 'CU', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S051: País de risco médio (Nigéria)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S051_COUNTRY_NIGERIA', 'Transação originada da Nigéria (NG)', 'merchantCountryCode', 'EQ', 'NG', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S052: País de risco médio (Paquistão)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S052_COUNTRY_PAKISTAN', 'Transação originada do Paquistão (PK)', 'merchantCountryCode', 'EQ', 'PK', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S053: País de risco médio (Ucrânia)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S053_COUNTRY_UKRAINE', 'Transação originada da Ucrânia (UA)', 'merchantCountryCode', 'EQ', 'UA', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S054: País de paraíso fiscal (Ilhas Cayman)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S054_COUNTRY_CAYMAN', 'Transação originada das Ilhas Cayman (KY)', 'merchantCountryCode', 'EQ', 'KY', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S055: País de paraíso fiscal (Ilhas Virgens Britânicas)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S055_COUNTRY_BVI', 'Transação originada das Ilhas Virgens Britânicas (VG)', 'merchantCountryCode', 'EQ', 'VG', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S056: País de paraíso fiscal (Panamá)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S056_COUNTRY_PANAMA', 'Transação originada do Panamá (PA)', 'merchantCountryCode', 'EQ', 'PA', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S057: País de paraíso fiscal (Suíça)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S057_COUNTRY_SWITZERLAND', 'Transação originada da Suíça (CH)', 'merchantCountryCode', 'EQ', 'CH', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S058: País de paraíso fiscal (Luxemburgo)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S058_COUNTRY_LUXEMBOURG', 'Transação originada de Luxemburgo (LU)', 'merchantCountryCode', 'EQ', 'LU', 'REVIEW', 35, 40, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S059: Transação doméstica (Brasil)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S059_COUNTRY_BRAZIL', 'Transação doméstica (Brasil)', 'merchantCountryCode', 'EQ', 'BR', 'APPROVE', 0, 5, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S060: Transação internacional genérica
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S060_INTERNATIONAL_TRANSACTION', 'Transação internacional (não-BR)', 'merchantCountryCode', 'NEQ', 'BR', 'REVIEW', 30, 35, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- ============================================================================
-- CATEGORIA 4: REGRAS DE CVV/PIN (15 regras)
-- ============================================================================

-- S061: CVV2 não presente
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S061_CVV2_NOT_PRESENT', 'CVV2 não presente na transação', 'cvv2Present', 'EQ', 'N', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S062: CVV2 inválido
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S062_CVV2_INVALID', 'CVV2 inválido', 'cvv2Response', 'EQ', 'N', 'REVIEW', 70, 75, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S063: CVV2 não processado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S063_CVV2_NOT_PROCESSED', 'CVV2 não processado pelo emissor', 'cvv2Response', 'EQ', 'P', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S064: CVV verificação falhou
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S064_CVV_VERIFY_FAILED', 'Verificação de CVV falhou', 'cvvVerifyCode', 'EQ', 'F', 'REVIEW', 75, 80, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S065: PIN verificação falhou
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S065_PIN_VERIFY_FAILED', 'Verificação de PIN falhou', 'pinVerifyCode', 'EQ', 'F', 'REVIEW', 80, 85, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S066: PIN offline falhou
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S066_OFFLINE_PIN_FAILED', 'Verificação de PIN offline falhou', 'cvrofflinePinVerificationFailed', 'EQ', '1', 'REVIEW', 85, 90, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S067: Criptograma inválido
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S067_CRYPTOGRAM_INVALID', 'Criptograma inválido', 'cryptogramValid', 'EQ', 'N', 'BLOCK', 90, 95, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S068: ATC do cartão divergente do host
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S068_ATC_MISMATCH', 'ATC do cartão divergente do host', 'atcCard', 'FIELD_NEQ', 'atcHost', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S069: CAVV resultado falhou
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S069_CAVV_FAILED', 'CAVV resultado falhou', 'cavvResult', 'EQ', '0', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S070: ECI indica sem autenticação
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S070_ECI_NO_AUTH', 'ECI indica transação sem autenticação 3DS', 'eciIndicator', 'EQ', '7', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S071: Token assurance level baixo
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S071_LOW_TOKEN_ASSURANCE', 'Nível de garantia do token baixo', 'tokenAssuranceLevel', 'LT', '2', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S072: Consumer authentication score baixo
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S072_LOW_AUTH_SCORE', 'Score de autenticação do consumidor baixo', 'consumerAuthenticationScore', 'LT', '50', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S073: Segundo fator de autenticação não completado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S073_2FA_NOT_COMPLETED', 'Segundo fator de autenticação não completado', 'secondFactorAuthCode', 'EQ', 'N', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S074: Auth indicator indica fallback
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S074_AUTH_FALLBACK', 'Indicador de autenticação indica fallback', 'authIndicator', 'EQ', '2', 'REVIEW', 45, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S075: AVS request não enviado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S075_AVS_NOT_SENT', 'Requisição AVS não enviada', 'avsRequest', 'IS_NULL', '', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- ============================================================================
-- CATEGORIA 5: REGRAS DE TERMINAL/POS (15 regras)
-- ============================================================================

-- S076: Terminal tipo ATM
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S076_TERMINAL_ATM', 'Transação em terminal ATM', 'terminalType', 'EQ', 'ATM', 'REVIEW', 30, 35, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S077: Terminal tipo e-commerce
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S077_TERMINAL_ECOMMERCE', 'Transação em terminal e-commerce', 'terminalType', 'EQ', 'ECOM', 'REVIEW', 25, 30, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S078: Terminal tipo MOTO (mail/telephone order)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S078_TERMINAL_MOTO', 'Transação MOTO (mail/telephone order)', 'terminalType', 'EQ', 'MOTO', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S079: POS entry mode manual (digitado)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S079_POS_MANUAL_ENTRY', 'Entrada manual do cartão (digitado)', 'posEntryMode', 'EQ', '01', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S080: POS entry mode fallback
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S080_POS_FALLBACK', 'Fallback de chip para tarja magnética', 'posEntryMode', 'EQ', '80', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S081: Cliente não presente
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S081_CUSTOMER_NOT_PRESENT', 'Cliente não presente na transação', 'customerPresent', 'EQ', 'N', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S082: POS condition code recorrente
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S082_POS_RECURRING', 'Transação recorrente', 'posConditionCode', 'EQ', '08', 'APPROVE', 0, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S083: POS condition code pré-autorização
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S083_POS_PREAUTH', 'Pré-autorização', 'posConditionCode', 'EQ', '06', 'REVIEW', 30, 35, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S084: Terminal verification results com erro
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S084_TVR_ERROR', 'Terminal verification results com erro', 'terminalVerificationResults', 'CONTAINS', 'ERROR', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S085: Card verification results com erro
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S085_CVR_ERROR', 'Card verification results com erro', 'cardVerificationResults', 'CONTAINS', 'ERROR', 'REVIEW', 65, 70, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S086: POS sem capacidade de PIN
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S086_POS_NO_PIN_CAPABILITY', 'Terminal sem capacidade de PIN', 'terminalEntryCapability', 'EQ', '0', 'REVIEW', 35, 40, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S087: POS security baixo
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S087_POS_LOW_SECURITY', 'POS com nível de segurança baixo', 'posSecurity', 'LT', '2', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S088: Standin advice ativo
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S088_STANDIN_ACTIVE', 'Transação processada em standin', 'standinAdvice', 'EQ', 'Y', 'REVIEW', 45, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S089: Network ID desconhecido
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S089_UNKNOWN_NETWORK', 'Network ID desconhecido', 'networkId', 'NOT_IN', 'VISA,MASTERCARD,ELO,AMEX,HIPERCARD', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S090: ATM owner diferente do emissor
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S090_ATM_OFF_US', 'ATM de terceiro (off-us)', 'atmOwner', 'NEQ', 'ISSUER', 'REVIEW', 25, 30, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- ============================================================================
-- CATEGORIA 6: REGRAS DE CARTÃO (10 regras)
-- ============================================================================

-- S091: Cartão expirado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S091_CARD_EXPIRED', 'Cartão expirado', 'cardExpireDate', 'LT_CURRENT_DATE', '', 'BLOCK', 95, 98, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S092: Cartão próximo de expirar (30 dias)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S092_CARD_EXPIRING_SOON', 'Cartão expira em menos de 30 dias', 'cardExpireDate', 'EXPIRES_WITHIN_DAYS', '30', 'REVIEW', 30, 35, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S093: Cartão virtual
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S093_VIRTUAL_CARD', 'Cartão virtual', 'cardMediaType', 'EQ', 'VIRTUAL', 'REVIEW', 25, 30, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S094: Cartão tokenizado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S094_TOKENIZED_CARD', 'Cartão tokenizado', 'tokenizationIndicator', 'EQ', 'Y', 'APPROVE', -10, 10, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S095: Cartão não tokenizado em e-commerce
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S095_NON_TOKENIZED_ECOM', 'Cartão não tokenizado em e-commerce', 'tokenizationIndicator', 'EQ', 'N', 'REVIEW', 35, 40, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S096: BIN expandido de alto risco
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S096_HIGH_RISK_BIN', 'BIN de alto risco', 'expandedBIN', 'IN', '400000,411111,422222', 'REVIEW', 50, 55, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S097: Sequência de cartão alta (muitas reemissões)
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S097_HIGH_CARD_SEQUENCE', 'Cartão com muitas reemissões (seq > 5)', 'cardSeqNum', 'GT', '5', 'REVIEW', 40, 45, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S098: Cartão em inadimplência
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S098_CARD_DELINQUENT', 'Cartão com valor em atraso', 'cardDelinquentAmount', 'GT', '0', 'REVIEW', 60, 65, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S099: Cartão com saldo de cash alto
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S099_HIGH_CASH_BALANCE', 'Cartão com saldo de cash alto', 'cardCashBalance', 'GT', '5000', 'REVIEW', 45, 50, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

-- S100: Cartão com crédito disponível zerado
INSERT INTO rule_configurations (rule_name, description, field_name, operator, comparison_value, decision_outcome, score_impact, priority, status, shadow_mode, created_by)
VALUES ('S100_ZERO_AVAILABLE_CREDIT', 'Cartão sem crédito disponível', 'availableCredit', 'EQ', '0', 'REVIEW', 55, 60, 'ACTIVE', false, 'SYSTEM_MIGRATION_V31');

INSERT INTO public.rule_configurations
  (rule_name, description, enabled, weight, classification, rule_type, threshold, parameters, conditions_json, logic_operator, created_at, updated_at)
SELECT
  rule_name,
  description,
  true,
  score_impact,
  CASE decision_outcome
    WHEN 'BLOCK' THEN 'FRAUD'
    WHEN 'REVIEW' THEN 'SUSPICIOUS'
    ELSE 'APPROVED'
  END,
  CASE decision_outcome
    WHEN 'APPROVE' THEN 'CONTEXT'
    ELSE 'SECURITY'
  END,
  0,
  jsonb_build_object(
    'priority', priority,
    'status', status,
    'shadow_mode', shadow_mode,
    'created_by', created_by,
    'decision_outcome', decision_outcome
  )::text,
  jsonb_build_array(
    jsonb_build_object('field', field_name, 'operator', operator, 'value', comparison_value)
  ),
  'AND',
  NOW(),
  NOW()
FROM rule_configurations
ON CONFLICT (rule_name) DO UPDATE
SET description = EXCLUDED.description,
    weight = EXCLUDED.weight,
    classification = EXCLUDED.classification,
    rule_type = EXCLUDED.rule_type,
    parameters = EXCLUDED.parameters,
    conditions_json = EXCLUDED.conditions_json,
    logic_operator = EXCLUDED.logic_operator,
    updated_at = NOW();

-- ============================================================================
-- FIM DA MIGRATION V40
-- ============================================================================

-- Total de regras criadas: 100 regras SIMPLES
-- Categorias cobertas:
--   - Valor: 20 regras (S001-S020)
--   - MCC: 20 regras (S021-S040)
--   - País: 20 regras (S041-S060)
--   - CVV/PIN: 15 regras (S061-S075)
--   - Terminal/POS: 15 regras (S076-S090)
--   - Cartão: 10 regras (S091-S100)
