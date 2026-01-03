-- V25__additional_fraud_rules_200plus.sql
-- ===================================================================================
-- RULEX FRAUD DETECTION - REGRAS ADICIONAIS PARA 200+ REGRAS
-- ===================================================================================
-- Esta migração adiciona 40 novas regras para completar o conjunto de 200+ regras
-- baseadas nas tipologias documentadas em FRAUD_TYPOLOGIES.md
-- ===================================================================================
-- NOTA: rule_type deve ser: SECURITY, CONTEXT, VELOCITY ou ANOMALY
-- ===================================================================================

-- =======================
-- 1. VELOCITY RULES (Velocidade/Frequência)
-- =======================

-- 1.1 Transações Excessivas por Período
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_TX_1H',
  'Mais de 10 transações em 1 hora - padrão de fraude por velocidade',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"velocity.tx_count_1h","operator":"GT","value":"10","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_TX_24H',
  'Mais de 50 transações em 24 horas - volume anormal',
  'VELOCITY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"velocity.tx_count_24h","operator":"GT","value":"50","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_TX_1H_CRITICAL',
  'Mais de 20 transações em 1 hora - bloqueio imediato',
  'VELOCITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field":"velocity.tx_count_1h","operator":"GT","value":"20","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.2 Valor Acumulado por Período
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_AMOUNT_1H',
  'Valor acumulado superior a R$ 10.000 em 1 hora',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"velocity.amount_sum_1h","operator":"GT","value":"10000","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_AMOUNT_24H',
  'Valor acumulado superior a R$ 50.000 em 24 horas',
  'VELOCITY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"velocity.amount_sum_24h","operator":"GT","value":"50000","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_AMOUNT_7D',
  'Valor acumulado superior a R$ 200.000 em 7 dias - bloqueio',
  'VELOCITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"velocity.amount_sum_7d","operator":"GT","value":"200000","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.3 Merchants Distintos
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_MERCHANTS_1H',
  'Mais de 5 estabelecimentos diferentes em 1 hora',
  'VELOCITY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"velocity.distinct_merchants_1h","operator":"GT","value":"5","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VEL_MERCHANTS_24H',
  'Mais de 15 estabelecimentos diferentes em 24 horas',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"velocity.distinct_merchants_24h","operator":"GT","value":"15","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =======================
-- 2. SECURITY RULES (Segurança)
-- =======================

-- 2.1 Tentativas de Autenticação
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_AUTH_FAIL_3X',
  '3 falhas consecutivas de autenticação - revisão necessária',
  'SECURITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"auth.consecutive_failures","operator":"EQ","value":"3","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_AUTH_FAIL_5X',
  '5 falhas consecutivas de autenticação - bloqueio',
  'SECURITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"auth.consecutive_failures","operator":"GT","value":"4","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_AUTH_BRUTE_FORCE',
  'Mais de 10 tentativas de autenticação em 5 minutos - ataque de força bruta',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"auth.attempts_5min","operator":"GT","value":"10","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.2 CVV/PIN Incorreto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_CVV_INVALID_3X',
  'CVV incorreto 3 vezes consecutivas - bloqueio',
  'SECURITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"cvv.consecutive_failures","operator":"GT","value":"2","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_PIN_INVALID_3X',
  'PIN incorreto 3 vezes consecutivas - bloqueio do cartão',
  'SECURITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field":"pin.consecutive_failures","operator":"GT","value":"2","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.3 Cartão Comprometido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_CARD_COMPROMISED',
  'Cartão presente em lista de cartões comprometidos',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"card.in_compromised_list","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEC_BIN_BLACKLIST',
  'BIN do cartão em lista de BINs suspeitos',
  'SECURITY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"card.bin_blacklisted","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =======================
-- 3. ANOMALY RULES (Anomalias Comportamentais)
-- =======================

-- 3.1 Desvio de Valor Médio
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_HIGH_VALUE_3X',
  'Valor da transação 3x maior que a média histórica do cliente',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"anomaly.value_deviation_ratio","operator":"GT","value":"3","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_HIGH_VALUE_5X',
  'Valor da transação 5x maior que a média histórica - bloqueio',
  'ANOMALY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"anomaly.value_deviation_ratio","operator":"GT","value":"5","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.2 Horário Atípico
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_UNUSUAL_HOUR',
  'Transação em horário fora do padrão do cliente (95%)',
  'ANOMALY',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"anomaly.unusual_hour","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_LATE_NIGHT',
  'Transação entre 02:00 e 05:00 - horário de alto risco',
  'ANOMALY',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"transaction.hour","operator":"GT","value":"1","valueType":"NUMBER"},{"field":"transaction.hour","operator":"LTE","value":"5","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_LATE_NIGHT_HIGH',
  'Transação acima de R$ 1.000 entre 02:00 e 05:00 - bloqueio',
  'ANOMALY',
  85,
  85,
  true,
  'FRAUD',
  '[{"field":"transaction.hour","operator":"GT","value":"1","valueType":"NUMBER"},{"field":"transaction.hour","operator":"LTE","value":"5","valueType":"NUMBER"},{"field":"amount","operator":"GT","value":"1000","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.3 Categoria de Merchant Atípica
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_NEW_MCC',
  'Primeira compra do cliente nesta categoria de estabelecimento',
  'ANOMALY',
  55,
  60,
  true,
  'SUSPICIOUS',
  '[{"field":"customer.first_mcc_usage","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_HIGH_RISK_MCC',
  'MCC de alto risco (7995-Jogos, 5967-Marketing Direto, 5966-Telemarketing)',
  'CONTEXT',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"mcc","operator":"IN","value":"7995,5967,5966","valueType":"ARRAY_NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ANO_FIRST_HIGH_VALUE',
  'Primeira transação do cliente acima de R$ 5.000',
  'ANOMALY',
  80,
  85,
  true,
  'SUSPICIOUS',
  '[{"field":"customer.is_first_transaction","operator":"EQ","value":"true","valueType":"STRING"},{"field":"amount","operator":"GT","value":"5000","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =======================
-- 4. BLOCKLIST RULES (usando SECURITY como tipo)
-- =======================

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLK_CPF',
  'CPF presente na lista de bloqueio por fraude',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"customer.cpf_blocked","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLK_CNPJ',
  'CNPJ presente na lista de bloqueio',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"merchant.cnpj_blocked","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLK_EMAIL',
  'E-mail presente na lista de bloqueio',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"customer.email_blocked","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLK_PHONE',
  'Telefone presente na lista de bloqueio',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"customer.phone_blocked","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLK_DEVICE_ID',
  'Device fingerprint presente na lista de bloqueio',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"device.fingerprint_blocked","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLK_IP',
  'Endereço IP presente na lista de bloqueio',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"network.ip_blocked","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.2 Listas de Observação
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'WATCH_CPF',
  'CPF em lista de observação - requer revisão',
  'SECURITY',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"customer.cpf_watchlist","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'WATCH_MERCHANT',
  'Estabelecimento em lista de observação',
  'CONTEXT',
  60,
  65,
  true,
  'SUSPICIOUS',
  '[{"field":"merchant.watchlist","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =======================
-- 5. GEO RULES (usando CONTEXT como tipo)
-- =======================

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_IMPOSSIBLE_TRAVEL',
  'Velocidade de deslocamento impossível (> 500 km/h)',
  'VELOCITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field":"geo.travel_speed_kmh","operator":"GT","value":"500","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_COUNTRY_CHANGE',
  'Transações em 2 ou mais países diferentes em 24 horas',
  'VELOCITY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"geo.distinct_countries_24h","operator":"GT","value":"1","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_IP_MISMATCH',
  'País do IP diferente do país declarado na transação',
  'CONTEXT',
  65,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"geo.ip_country_mismatch","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'GEO_IP_PROXY',
  'IP identificado como proxy ou VPN',
  'CONTEXT',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"network.is_proxy_vpn","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =======================
-- 6. DEVICE RULES (usando ANOMALY como tipo)
-- =======================

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DEV_NEW_DEVICE',
  'Primeira transação deste dispositivo para o cliente',
  'ANOMALY',
  55,
  60,
  true,
  'SUSPICIOUS',
  '[{"field":"device.is_new","operator":"EQ","value":"true","valueType":"STRING"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DEV_NEW_DEVICE_HIGH',
  'Dispositivo novo com transação acima de R$ 1.000 - bloqueio',
  'ANOMALY',
  85,
  85,
  true,
  'FRAUD',
  '[{"field":"device.is_new","operator":"EQ","value":"true","valueType":"STRING"},{"field":"amount","operator":"GT","value":"1000","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DEV_MULTI_24H',
  'Mais de 3 dispositivos diferentes em 24 horas',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"device.distinct_devices_24h","operator":"GT","value":"3","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DEV_MULTI_7D',
  'Mais de 5 dispositivos diferentes em 7 dias',
  'ANOMALY',
  75,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"device.distinct_devices_7d","operator":"GT","value":"5","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DEV_SHARED_24H',
  'Dispositivo usado por mais de 2 clientes em 24 horas',
  'ANOMALY',
  70,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"device.shared_customers_24h","operator":"GT","value":"2","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DEV_SHARED_7D',
  'Dispositivo usado por mais de 5 clientes em 7 dias - bloqueio',
  'ANOMALY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"device.shared_customers_7d","operator":"GT","value":"5","valueType":"NUMBER"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- FIM DA MIGRAÇÃO V25 - 40 NOVAS REGRAS ADICIONADAS
-- Total esperado: 162 + 40 = 202 regras
-- ===================================================================================
