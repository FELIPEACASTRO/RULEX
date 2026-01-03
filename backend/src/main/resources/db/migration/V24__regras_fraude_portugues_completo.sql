-- =====================================================
-- V24__regras_fraude_portugues_completo.sql
-- =====================================================
-- Descrição: Atualização de todas as regras para Português do Brasil
--            e adição de novas regras para completar 200+ regras
-- Data: 2026-01-03
-- Autor: RULEX Team
-- =====================================================

-- =====================================================
-- PARTE 1: ATUALIZAÇÃO DAS REGRAS SIMPLES EXISTENTES
-- Tradução para Português do Brasil
-- =====================================================

-- Regra 1: CVV não corresponde
UPDATE rule_configurations SET
  rule_name = 'CVV_NAO_CORRESPONDE',
  description = 'Código de verificação do cartão (CVV/CVC) não corresponde ao registrado - indica possível uso de dados roubados'
WHERE rule_name = 'CVV_MISMATCH' OR rule_name = 'CARD_EXPIRED_OBJECTIVE';

-- Regra 2: Verificação PIN falhou
UPDATE rule_configurations SET
  rule_name = 'VERIFICACAO_PIN_FALHOU',
  description = 'Verificação do PIN offline falhou - possível cartão roubado com tentativas de PIN incorreto'
WHERE rule_name = 'PIN_VERIFICATION_FAILED';

-- Regra 3: Criptograma EMV inválido
UPDATE rule_configurations SET
  rule_name = 'CRIPTOGRAMA_EMV_INVALIDO',
  description = 'Validação do criptograma EMV falhou - indica cartão falsificado ou adulterado'
WHERE rule_name = 'CRYPTOGRAM_INVALID';

-- Regra 4: ATC divergente
UPDATE rule_configurations SET
  rule_name = 'ATC_DIVERGENTE',
  description = 'Contador de transações do cartão (ATC) difere significativamente do valor do host - possível cartão clonado ou ataque de replay'
WHERE rule_name = 'ATC_MISMATCH_HIGH' OR rule_name = 'ATC_MISMATCH';

-- Regra 5: Score de autenticação baixo
UPDATE rule_configurations SET
  rule_name = 'SCORE_AUTENTICACAO_BAIXO',
  description = 'Score de autenticação do consumidor abaixo do limite - indica autenticação fraca'
WHERE rule_name = 'LOW_AUTH_SCORE' OR rule_name = 'LOW_CONSUMER_AUTH_SCORE';

-- Regra 6: Limite de tentativas PIN excedido
UPDATE rule_configurations SET
  rule_name = 'LIMITE_TENTATIVAS_PIN_EXCEDIDO',
  description = 'Número máximo de tentativas de PIN excedido - forte indicador de fraude'
WHERE rule_name = 'PIN_TRY_LIMIT_EXCEEDED';

-- Regra 7: MCC Jogos/Apostas valor alto
UPDATE rule_configurations SET
  rule_name = 'MCC_JOGOS_APOSTAS_VALOR_ALTO',
  description = 'Transação em estabelecimento de jogos/apostas (MCC 7995, 7994, 7993) acima de R$ 500 - categoria de alto risco'
WHERE rule_name = 'MCC_GAMBLING_HIGH_VALUE';

-- Regra 8: MCC Criptomoeda/Quasi-cash
UPDATE rule_configurations SET
  rule_name = 'MCC_CRIPTOMOEDA_QUASI_CASH',
  description = 'Compra de criptomoeda ou quasi-cash (MCC 6051, 6211) - alto risco de lavagem de dinheiro'
WHERE rule_name = 'MCC_CRYPTO_QUASI_CASH';

-- Regra 9: MCC Transferência bancária
UPDATE rule_configurations SET
  rule_name = 'MCC_TRANSFERENCIA_BANCARIA',
  description = 'Transação de transferência bancária (MCC 4829) - requer atenção especial'
WHERE rule_name = 'MCC_WIRE_TRANSFER';

-- Regra 10: MCC Conteúdo adulto
UPDATE rule_configurations SET
  rule_name = 'MCC_CONTEUDO_ADULTO',
  description = 'Transação em estabelecimento de conteúdo adulto - categoria de risco elevado'
WHERE rule_name = 'MCC_ADULT_CONTENT';

-- Regra 11: MCC Casa de penhores
UPDATE rule_configurations SET
  rule_name = 'MCC_CASA_PENHORES',
  description = 'Transação em casa de penhores (MCC 5933) - frequentemente associada a fraude'
WHERE rule_name = 'MCC_PAWN_SHOP';

-- Regra 12: Transação internacional valor alto
UPDATE rule_configurations SET
  rule_name = 'TRANSACAO_INTERNACIONAL_VALOR_ALTO',
  description = 'Transação internacional acima de R$ 5.000 - requer verificação adicional'
WHERE rule_name = 'INTERNATIONAL_HIGH_VALUE';

-- Regra 13: País de alto risco
UPDATE rule_configurations SET
  rule_name = 'PAIS_ALTO_RISCO',
  description = 'Transação originada de país classificado como alto risco para fraude'
WHERE rule_name = 'HIGH_RISK_COUNTRY';

-- Regra 14: Horário suspeito (madrugada)
UPDATE rule_configurations SET
  rule_name = 'HORARIO_SUSPEITO_MADRUGADA',
  description = 'Transação realizada entre 00:00 e 06:00 - horário com maior incidência de fraude'
WHERE rule_name = 'SUSPICIOUS_TIME_NIGHT' OR rule_name = 'NIGHT_TRANSACTION';

-- Regra 15: Valor redondo suspeito
UPDATE rule_configurations SET
  rule_name = 'VALOR_REDONDO_SUSPEITO',
  description = 'Transação com valor redondo (múltiplo de 100 ou 1000) acima de R$ 1.000 - padrão comum em fraudes'
WHERE rule_name = 'ROUND_AMOUNT_SUSPICIOUS';

-- Regra 16: Primeira transação valor alto
UPDATE rule_configurations SET
  rule_name = 'PRIMEIRA_TRANSACAO_VALOR_ALTO',
  description = 'Primeira transação do cartão com valor acima de R$ 2.000 - comportamento atípico'
WHERE rule_name = 'FIRST_TRANSACTION_HIGH_VALUE';

-- Regra 17: CVV2 falhou valor alto
UPDATE rule_configurations SET
  rule_name = 'CVV2_FALHOU_VALOR_ALTO',
  description = 'Verificação CVV2 falhou em transação acima de R$ 1.000 - risco elevado'
WHERE rule_name = 'CVV2_FAILED_HIGH_AMOUNT';

-- Regra 18: ECI baixo valor alto
UPDATE rule_configurations SET
  rule_name = 'ECI_BAIXO_VALOR_ALTO',
  description = 'Transação e-commerce sem autenticação 3D Secure (ECI 7) acima de R$ 2.000 - maior risco CNP'
WHERE rule_name = 'LOW_ECI_HIGH_AMOUNT';

-- Regra 19: Token com garantia zero
UPDATE rule_configurations SET
  rule_name = 'TOKEN_GARANTIA_ZERO',
  description = 'Transação tokenizada com nível de garantia mais baixo (0) - provisionamento sem verificação forte de identidade'
WHERE rule_name = 'TOKEN_ASSURANCE_ZERO';

-- Regra 20: Velocidade transações 1 hora
UPDATE rule_configurations SET
  rule_name = 'VELOCIDADE_TRANSACOES_1_HORA',
  description = 'Mais de 5 transações do mesmo cartão em 1 hora - possível teste de cartão ou fraude em andamento'
WHERE rule_name = 'VELOCITY_1H_COUNT' OR rule_name = 'HIGH_VELOCITY_1H';

-- Regra 21: Velocidade valor 1 hora
UPDATE rule_configurations SET
  rule_name = 'VELOCIDADE_VALOR_1_HORA',
  description = 'Soma de transações do mesmo cartão excede R$ 10.000 em 1 hora - possível esvaziamento de conta'
WHERE rule_name = 'VELOCITY_1H_SUM' OR rule_name = 'HIGH_VELOCITY_SUM_1H';

-- Regra 22: Velocidade transações 24 horas
UPDATE rule_configurations SET
  rule_name = 'VELOCIDADE_TRANSACOES_24_HORAS',
  description = 'Mais de 20 transações do mesmo cartão em 24 horas - comportamento altamente suspeito'
WHERE rule_name = 'VELOCITY_24H_COUNT' OR rule_name = 'HIGH_VELOCITY_24H';

-- Regra 23: Velocidade estabelecimentos distintos
UPDATE rule_configurations SET
  rule_name = 'VELOCIDADE_ESTABELECIMENTOS_DISTINTOS',
  description = 'Transações em mais de 5 estabelecimentos diferentes em 1 hora - padrão de fraude'
WHERE rule_name = 'VELOCITY_DISTINCT_MERCHANTS' OR rule_name = 'DISTINCT_MERCHANTS_1H';

-- Regra 24: Velocidade países distintos
UPDATE rule_configurations SET
  rule_name = 'VELOCIDADE_PAISES_DISTINTOS',
  description = 'Transações em mais de 2 países diferentes em 24 horas - impossibilidade física de viagem'
WHERE rule_name = 'VELOCITY_DISTINCT_COUNTRIES' OR rule_name = 'DISTINCT_COUNTRIES_24H';

-- Regra 25: Cartão presente sem chip
UPDATE rule_configurations SET
  rule_name = 'CARTAO_PRESENTE_SEM_CHIP',
  description = 'Transação presencial sem leitura de chip em cartão com chip - possível clonagem de tarja'
WHERE rule_name = 'CARD_PRESENT_NO_CHIP' OR rule_name = 'FALLBACK_TRANSACTION';

-- Regra 26: Modo entrada manual
UPDATE rule_configurations SET
  rule_name = 'MODO_ENTRADA_MANUAL',
  description = 'Dados do cartão inseridos manualmente em transação presencial - alto risco de fraude'
WHERE rule_name = 'MANUAL_ENTRY_MODE' OR rule_name = 'KEY_ENTERED';

-- Regra 27: Transação sem cliente presente
UPDATE rule_configurations SET
  rule_name = 'TRANSACAO_SEM_CLIENTE_PRESENTE',
  description = 'Transação marcada como presencial mas cliente não presente - inconsistência suspeita'
WHERE rule_name = 'CUSTOMER_NOT_PRESENT' OR rule_name = 'NO_CUSTOMER_PRESENT';

-- Regra 28: Crédito disponível excedido
UPDATE rule_configurations SET
  rule_name = 'CREDITO_DISPONIVEL_EXCEDIDO',
  description = 'Valor da transação excede o crédito disponível do cartão - tentativa de fraude'
WHERE rule_name = 'AVAILABLE_CREDIT_EXCEEDED' OR rule_name = 'OVER_CREDIT_LIMIT';

-- Regra 29: Cartão em atraso
UPDATE rule_configurations SET
  rule_name = 'CARTAO_EM_ATRASO',
  description = 'Cartão com valor em atraso acima de R$ 500 - risco de inadimplência'
WHERE rule_name = 'CARD_DELINQUENT' OR rule_name = 'DELINQUENT_ACCOUNT';

-- Regra 30: Comerciante novo valor alto
UPDATE rule_configurations SET
  rule_name = 'COMERCIANTE_NOVO_VALOR_ALTO',
  description = 'Transação de alto valor em comerciante recém-cadastrado - risco de comerciante fraudulento'
WHERE rule_name = 'NEW_MERCHANT_HIGH_VALUE';

-- =====================================================
-- PARTE 2: ATUALIZAÇÃO DAS REGRAS COMPLEXAS EXISTENTES
-- =====================================================

-- Regra Complexa 1: Jogos internacionais à noite
UPDATE complex_rules SET
  key = 'JOGOS_INTERNACIONAIS_NOITE',
  title = 'Jogos Internacionais à Noite',
  description = 'Transação em estabelecimento de jogos/apostas, fora do Brasil, durante a madrugada - combinação de alto risco',
  reason_template = 'Transação bloqueada: Jogos/apostas internacional em horário de madrugada'
WHERE key = 'INTL_GAMBLING_NIGHT_COMPLEX';

-- Regra Complexa 2: Padrão Bust-Out
UPDATE complex_rules SET
  key = 'PADRAO_BUST_OUT',
  title = 'Padrão de Fraude Bust-Out',
  description = 'Detecção de padrão bust-out: múltiplas transações de alto valor em curto período com crédito no limite',
  reason_template = 'Transação bloqueada: Padrão de bust-out detectado'
WHERE key = 'BUST_OUT_PATTERN_COMPLEX';

-- Regra Complexa 3: Sequência de teste de cartão
UPDATE complex_rules SET
  key = 'SEQUENCIA_TESTE_CARTAO',
  title = 'Sequência de Teste de Cartão',
  description = 'Múltiplas transações de baixo valor seguidas de transação de alto valor - padrão clássico de teste de cartão roubado',
  reason_template = 'Transação bloqueada: Sequência de teste de cartão detectada'
WHERE key = 'CARD_TESTING_SEQUENCE_COMPLEX';

-- Regra Complexa 4: Padrão conta laranja
UPDATE complex_rules SET
  key = 'PADRAO_CONTA_LARANJA',
  title = 'Padrão de Conta Laranja (Money Mule)',
  description = 'Comportamento típico de conta laranja: recebimentos seguidos de saques ou transferências imediatas',
  reason_template = 'Transação bloqueada: Padrão de conta laranja detectado'
WHERE key = 'MONEY_MULE_PATTERN_COMPLEX';

-- Regra Complexa 5: Anomalia de dispositivo
UPDATE complex_rules SET
  key = 'ANOMALIA_DISPOSITIVO',
  title = 'Anomalia de Dispositivo/Fingerprint',
  description = 'Incompatibilidade entre fingerprint do dispositivo e padrão histórico do cliente',
  reason_template = 'Transação bloqueada: Anomalia de dispositivo detectada'
WHERE key = 'DEVICE_ANOMALY_COMPLEX';

-- Regra Complexa 6: Ataque de velocidade transfronteiriço
UPDATE complex_rules SET
  key = 'ATAQUE_VELOCIDADE_TRANSFRONTEIRICO',
  title = 'Ataque de Velocidade Transfronteiriço',
  description = 'Transações em múltiplos países em intervalo de tempo impossível para viagem física',
  reason_template = 'Transação bloqueada: Impossibilidade geográfica detectada'
WHERE key = 'CROSS_BORDER_VELOCITY_COMPLEX';

-- Regra Complexa 7: Cascata MCC alto risco
UPDATE complex_rules SET
  key = 'CASCATA_MCC_ALTO_RISCO',
  title = 'Cascata de MCC de Alto Risco',
  description = 'Múltiplas transações em MCCs de alto risco em sequência - padrão de lavagem de dinheiro',
  reason_template = 'Transação bloqueada: Cascata de MCCs de alto risco'
WHERE key = 'HIGH_RISK_MCC_CASCADE_COMPLEX';

-- Regra Complexa 8: Perfil de risco primeira vez
UPDATE complex_rules SET
  key = 'PERFIL_RISCO_PRIMEIRA_VEZ',
  title = 'Perfil de Alto Risco - Primeira Transação',
  description = 'Primeira transação do cartão com múltiplos indicadores de risco combinados',
  reason_template = 'Transação bloqueada: Perfil de alto risco em primeira transação'
WHERE key = 'FIRST_TIME_HIGH_RISK_COMPLEX';

-- =====================================================
-- PARTE 3: NOVAS REGRAS SIMPLES (31-100)
-- Categoria: SEGURANÇA E AUTENTICAÇÃO
-- =====================================================

-- Regra 31: Falha de autenticação 3DS
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'FALHA_AUTENTICACAO_3DS',
  'Autenticação 3D Secure falhou ou foi recusada pelo emissor - transação de alto risco',
  'SECURITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "threeDSecureStatus", "operator": "IN", "value": ["FAILED", "REJECTED", "UNAVAILABLE"]}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 32: Cartão bloqueado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARTAO_BLOQUEADO',
  'Tentativa de transação com cartão marcado como bloqueado no sistema',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "cardStatus", "operator": "EQ", "value": "BLOCKED"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 33: Cartão reportado como perdido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARTAO_PERDIDO',
  'Tentativa de transação com cartão reportado como perdido',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "cardStatus", "operator": "EQ", "value": "LOST"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 34: Cartão reportado como roubado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARTAO_ROUBADO',
  'Tentativa de transação com cartão reportado como roubado',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "cardStatus", "operator": "EQ", "value": "STOLEN"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 35: Cartão vencido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARTAO_VENCIDO',
  'Data de validade do cartão anterior à data da transação - inconsistência objetiva',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "cardExpired", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 36: BIN inválido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BIN_INVALIDO',
  'BIN do cartão não encontrado nas tabelas de BINs válidos - possível cartão fabricado',
  'SECURITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field": "binValid", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 37: Número do cartão em lista negra
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARTAO_LISTA_NEGRA',
  'Número do cartão (PAN) encontrado em lista negra de fraudes confirmadas',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "panInBlocklist", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 38: CPF em lista negra
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CPF_LISTA_NEGRA',
  'CPF do titular encontrado em lista negra de fraudadores conhecidos',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "cpfInBlocklist", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 39: Comerciante em lista negra
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'COMERCIANTE_LISTA_NEGRA',
  'Estabelecimento comercial encontrado em lista negra por fraudes anteriores',
  'SECURITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field": "merchantInBlocklist", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 40: IP em lista negra
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'IP_LISTA_NEGRA',
  'Endereço IP da transação encontrado em lista negra de IPs maliciosos',
  'SECURITY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "ipInBlocklist", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: VALOR E LIMITES
-- =====================================================

-- Regra 41: Valor muito alto (acima de R$ 50.000)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VALOR_MUITO_ALTO',
  'Transação com valor acima de R$ 50.000 - requer aprovação especial',
  'CONTEXT',
  90,
  90,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionAmount", "operator": "GT", "value": "50000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 42: Valor extremamente alto (acima de R$ 100.000)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VALOR_EXTREMAMENTE_ALTO',
  'Transação com valor acima de R$ 100.000 - alto risco, requer verificação manual',
  'CONTEXT',
  95,
  95,
  true,
  'FRAUD',
  '[{"field": "transactionAmount", "operator": "GT", "value": "100000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 43: Valor abaixo do mínimo
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VALOR_ABAIXO_MINIMO',
  'Transação com valor abaixo de R$ 1 - possível teste de cartão',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionAmount", "operator": "LT", "value": "1"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 44: Valor exato suspeito (R$ 1,00)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VALOR_EXATO_UM_REAL',
  'Transação de exatamente R$ 1,00 - padrão comum de teste de cartão roubado',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionAmount", "operator": "EQ", "value": "1"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 45: Múltiplo exato de R$ 1.000
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLO_MIL_REAIS',
  'Transação com valor múltiplo exato de R$ 1.000 acima de R$ 5.000 - padrão de lavagem',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionAmount", "operator": "GT", "value": "5000"}, {"field": "transactionAmount", "operator": "MOD_EQ", "value": "1000,0"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 46: Valor logo abaixo do limite de reporte (R$ 9.999)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VALOR_ABAIXO_LIMITE_REPORTE',
  'Transação entre R$ 9.000 e R$ 9.999 - possível structuring para evitar reporte ao COAF',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionAmount", "operator": "BETWEEN", "value": "9000,9999"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 47: Saque alto em espécie
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SAQUE_ALTO_ESPECIE',
  'Saque em espécie acima de R$ 5.000 - requer verificação adicional',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionType", "operator": "EQ", "value": "CASH_WITHDRAWAL"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 48: Compra parcelada valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PARCELADO_VALOR_ALTO',
  'Compra parcelada acima de R$ 10.000 - verificar capacidade de pagamento',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "installments", "operator": "GT", "value": "1"}, {"field": "transactionAmount", "operator": "GT", "value": "10000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 49: Muitas parcelas
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUITAS_PARCELAS',
  'Transação com mais de 12 parcelas - risco de inadimplência',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "installments", "operator": "GT", "value": "12"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 50: Valor inconsistente com MCC
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VALOR_INCONSISTENTE_MCC',
  'Valor da transação incompatível com o tipo de estabelecimento (ex: R$ 50.000 em lanchonete)',
  'CONTEXT',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "IN", "value": ["5812", "5813", "5814"]}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: GEOLOCALIZAÇÃO
-- =====================================================

-- Regra 51: País de origem diferente do país do cartão
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PAIS_ORIGEM_DIFERENTE',
  'País de origem da transação diferente do país de emissão do cartão',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "merchantCountryCode", "operator": "FIELD_NEQ", "value": "cardCountryCode"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 52: Transação de país sancionado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PAIS_SANCIONADO',
  'Transação originada de país sob sanções internacionais (OFAC, ONU)',
  'CONTEXT',
  100,
  100,
  true,
  'FRAUD',
  '[{"field": "merchantCountryCode", "operator": "IN", "value": ["PRK", "IRN", "SYR", "CUB", "RUS"]}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 53: Distância geográfica impossível
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DISTANCIA_IMPOSSIVEL',
  'Distância entre transações consecutivas impossível de percorrer no tempo decorrido',
  'CONTEXT',
  95,
  95,
  true,
  'FRAUD',
  '[{"field": "geoVelocityKmH", "operator": "GT", "value": "1000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 54: IP de país diferente do comerciante
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'IP_PAIS_DIFERENTE',
  'Endereço IP da transação de país diferente do país do comerciante - possível uso de VPN',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "ipCountryCode", "operator": "FIELD_NEQ", "value": "merchantCountryCode"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 55: IP de proxy/VPN detectado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'IP_PROXY_VPN',
  'Endereço IP identificado como proxy, VPN ou Tor - tentativa de ocultar localização real',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "ipIsProxy", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 56: Transação fora do polígono habitual
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'FORA_POLIGONO_HABITUAL',
  'Transação realizada fora da área geográfica habitual do cliente',
  'CONTEXT',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "inUsualGeoArea", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 57: CEP do comerciante inválido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CEP_COMERCIANTE_INVALIDO',
  'CEP do estabelecimento comercial não existe ou é inválido',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "merchantPostalCodeValid", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 58: Região de alto índice de fraude
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'REGIAO_ALTO_INDICE_FRAUDE',
  'Transação originada de região com alto índice histórico de fraudes',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "regionFraudRisk", "operator": "EQ", "value": "HIGH"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: VELOCIDADE E FREQUÊNCIA
-- =====================================================

-- Regra 59: Burst de transações em 5 minutos
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BURST_5_MINUTOS',
  'Mais de 3 transações do mesmo cartão em 5 minutos - possível ataque automatizado',
  'VELOCITY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "velocity", "operator": "VELOCITY_COUNT_GT", "value": "PAN,5,3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 60: Burst de transações em 1 minuto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BURST_1_MINUTO',
  'Mais de 2 transações do mesmo cartão em 1 minuto - ataque de força bruta',
  'VELOCITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field": "velocity", "operator": "VELOCITY_COUNT_GT", "value": "PAN,1,2"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 61: Volume diário excedido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VOLUME_DIARIO_EXCEDIDO',
  'Soma de transações do cartão excede R$ 50.000 em 24 horas',
  'VELOCITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "velocity", "operator": "VELOCITY_SUM_GT", "value": "PAN,1440,50000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 62: Volume semanal excedido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VOLUME_SEMANAL_EXCEDIDO',
  'Soma de transações do cartão excede R$ 100.000 em 7 dias',
  'VELOCITY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "velocity", "operator": "VELOCITY_SUM_GT", "value": "PAN,10080,100000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 63: Muitos MCCs diferentes em 1 hora
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUITOS_MCCS_1_HORA',
  'Transações em mais de 5 tipos de estabelecimento diferentes em 1 hora',
  'VELOCITY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "velocity", "operator": "VELOCITY_DISTINCT_GT", "value": "PAN,60,MCC,5"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 64: Muitas cidades diferentes em 24 horas
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUITAS_CIDADES_24_HORAS',
  'Transações em mais de 3 cidades diferentes em 24 horas',
  'VELOCITY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "velocity", "operator": "VELOCITY_DISTINCT_GT", "value": "PAN,1440,CITY,3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 65: Aumento súbito de valor médio
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'AUMENTO_VALOR_MEDIO',
  'Valor médio das transações nas últimas 24h é 5x maior que a média histórica',
  'VELOCITY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "avgAmountRatio24h", "operator": "GT", "value": "5"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 66: Primeira transação internacional
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PRIMEIRA_TRANSACAO_INTERNACIONAL',
  'Primeira transação internacional do cartão - verificar com titular',
  'VELOCITY',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "isFirstInternationalTransaction", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 67: Transação após longo período inativo
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARTAO_INATIVO_REATIVADO',
  'Transação após mais de 90 dias sem uso do cartão',
  'VELOCITY',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "daysSinceLastTransaction", "operator": "GT", "value": "90"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 68: Padrão de teste de cartão
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PADRAO_TESTE_CARTAO',
  'Sequência de transações de baixo valor seguida de alto valor - teste de cartão clássico',
  'VELOCITY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "cardTestingPattern", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: MCC E TIPO DE ESTABELECIMENTO
-- =====================================================

-- Regra 69: MCC de alto risco - Joias
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_JOIAS_VALOR_ALTO',
  'Compra em joalheria (MCC 5944) acima de R$ 10.000 - item de fácil revenda',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "5944"}, {"field": "transactionAmount", "operator": "GT", "value": "10000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 70: MCC de alto risco - Eletrônicos
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_ELETRONICOS_VALOR_ALTO',
  'Compra em loja de eletrônicos (MCC 5732) acima de R$ 15.000 - item de fácil revenda',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "5732"}, {"field": "transactionAmount", "operator": "GT", "value": "15000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 71: MCC de alto risco - Gift Cards
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_GIFT_CARDS',
  'Compra de gift cards/cartões pré-pagos (MCC 5815) - frequentemente usado para lavagem',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "5815"}, {"field": "transactionAmount", "operator": "GT", "value": "1000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 72: MCC de alto risco - Casas de câmbio
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_CASA_CAMBIO',
  'Transação em casa de câmbio (MCC 6012) - risco de lavagem de dinheiro',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "6012"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 73: MCC de alto risco - Serviços financeiros
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_SERVICOS_FINANCEIROS',
  'Transação em serviços financeiros não bancários (MCC 6211) acima de R$ 5.000',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "6211"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 74: MCC de alto risco - Loterias
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_LOTERIAS',
  'Transação em loteria (MCC 7800) acima de R$ 500',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "7800"}, {"field": "transactionAmount", "operator": "GT", "value": "500"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 75: MCC de alto risco - Telecomunicações
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_TELECOM_VALOR_ALTO',
  'Recarga de celular ou serviço de telecom (MCC 4812, 4814) acima de R$ 1.000',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "IN", "value": ["4812", "4814"]}, {"field": "transactionAmount", "operator": "GT", "value": "1000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 76: MCC de alto risco - Viagens
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_VIAGENS_VALOR_ALTO',
  'Compra de passagem aérea (MCC 4511) acima de R$ 20.000 - verificar com titular',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "4511"}, {"field": "transactionAmount", "operator": "GT", "value": "20000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 77: MCC de alto risco - Hotéis
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_HOTEIS_VALOR_ALTO',
  'Reserva de hotel (MCC 7011) acima de R$ 15.000 - verificar legitimidade',
  'CONTEXT',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "7011"}, {"field": "transactionAmount", "operator": "GT", "value": "15000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 78: MCC de alto risco - Aluguel de carros
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MCC_ALUGUEL_CARROS_VALOR_ALTO',
  'Aluguel de veículos (MCC 7512) acima de R$ 10.000',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "mcc", "operator": "EQ", "value": "7512"}, {"field": "transactionAmount", "operator": "GT", "value": "10000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: HORÁRIO E TEMPO
-- =====================================================

-- Regra 79: Transação em feriado nacional
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'TRANSACAO_FERIADO',
  'Transação de alto valor em feriado nacional - verificar legitimidade',
  'CONTEXT',
  50,
  50,
  true,
  'SUSPICIOUS',
  '[{"field": "isHoliday", "operator": "EQ", "value": "true"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 80: Transação em final de semana madrugada
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'FIM_DE_SEMANA_MADRUGADA',
  'Transação entre 00:00 e 06:00 em sábado ou domingo - horário de maior risco',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "isWeekend", "operator": "EQ", "value": "true"}, {"field": "transactionTime", "operator": "TIME_BETWEEN", "value": "000000,060000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 81: Transação fora do horário comercial
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'FORA_HORARIO_COMERCIAL',
  'Transação presencial fora do horário comercial (antes das 8h ou após 22h)',
  'CONTEXT',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "IN", "value": ["05", "07", "90"]}, {"field": "transactionTime", "operator": "NOT_BETWEEN", "value": "080000,220000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 82: Múltiplas transações mesmo minuto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLAS_MESMO_MINUTO',
  'Mais de 1 transação do mesmo cartão no mesmo minuto - ataque automatizado',
  'VELOCITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field": "transactionsInSameMinute", "operator": "GT", "value": "1"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: DISPOSITIVO E CANAL
-- =====================================================

-- Regra 83: Dispositivo novo
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DISPOSITIVO_NOVO',
  'Transação de dispositivo nunca usado antes pelo cliente',
  'CONTEXT',
  50,
  50,
  true,
  'SUSPICIOUS',
  '[{"field": "isNewDevice", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 84: Dispositivo novo + valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DISPOSITIVO_NOVO_VALOR_ALTO',
  'Transação de dispositivo novo com valor acima de R$ 5.000 - alto risco',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "isNewDevice", "operator": "EQ", "value": "true"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 85: Múltiplos cartões mesmo dispositivo
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLOS_CARTOES_DISPOSITIVO',
  'Mais de 3 cartões diferentes usados no mesmo dispositivo em 24h',
  'VELOCITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "distinctCardsOnDevice24h", "operator": "GT", "value": "3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 86: Emulador detectado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'EMULADOR_DETECTADO',
  'Transação originada de emulador de dispositivo móvel',
  'SECURITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field": "isEmulator", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 87: Root/Jailbreak detectado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ROOT_JAILBREAK_DETECTADO',
  'Dispositivo com root (Android) ou jailbreak (iOS) detectado',
  'SECURITY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "isRooted", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 88: App não oficial
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'APP_NAO_OFICIAL',
  'Transação originada de aplicativo não oficial ou modificado',
  'SECURITY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "isOfficialApp", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 89: Browser suspeito
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BROWSER_SUSPEITO',
  'Transação web de navegador com fingerprint suspeito ou inconsistente',
  'SECURITY',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "browserSuspicious", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 90: User-Agent inconsistente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'USER_AGENT_INCONSISTENTE',
  'User-Agent do navegador inconsistente com características do dispositivo',
  'SECURITY',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "userAgentConsistent", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: E-COMMERCE (CNP)
-- =====================================================

-- Regra 91: E-commerce sem 3DS
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ECOMMERCE_SEM_3DS',
  'Transação e-commerce sem autenticação 3D Secure',
  'SECURITY',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "EQ", "value": "81"}, {"field": "threeDSecureStatus", "operator": "IN", "value": ["NOT_ENROLLED", "UNAVAILABLE"]}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 92: E-commerce sem 3DS valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ECOMMERCE_SEM_3DS_VALOR_ALTO',
  'Transação e-commerce sem 3DS acima de R$ 3.000 - alto risco CNP',
  'SECURITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "EQ", "value": "81"}, {"field": "threeDSecureStatus", "operator": "NEQ", "value": "AUTHENTICATED"}, {"field": "transactionAmount", "operator": "GT", "value": "3000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 93: Endereço de entrega diferente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ENDERECO_ENTREGA_DIFERENTE',
  'Endereço de entrega diferente do endereço de cobrança do cartão',
  'CONTEXT',
  50,
  50,
  true,
  'SUSPICIOUS',
  '[{"field": "shippingAddressMatchesBilling", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 94: Entrega expressa + valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ENTREGA_EXPRESSA_VALOR_ALTO',
  'Entrega expressa solicitada para compra acima de R$ 5.000 - fraudadores querem receber rápido',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "shippingMethod", "operator": "EQ", "value": "EXPRESS"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 95: Email temporário
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'EMAIL_TEMPORARIO',
  'Email do comprador é de serviço de email temporário/descartável',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "emailIsTemporary", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 96: Email recém-criado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'EMAIL_RECEM_CRIADO',
  'Email do comprador foi criado há menos de 30 dias',
  'CONTEXT',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "emailAgeDays", "operator": "LT", "value": "30"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 97: Telefone inválido
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'TELEFONE_INVALIDO',
  'Número de telefone fornecido é inválido ou não existe',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "phoneValid", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 98: CPF e nome não correspondem
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CPF_NOME_NAO_CORRESPONDEM',
  'CPF informado não corresponde ao nome do titular do cartão',
  'SECURITY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "cpfNameMatch", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 99: Primeira compra no site
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PRIMEIRA_COMPRA_SITE',
  'Primeira compra do cliente neste estabelecimento com valor acima de R$ 2.000',
  'CONTEXT',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "isFirstPurchaseAtMerchant", "operator": "EQ", "value": "true"}, {"field": "transactionAmount", "operator": "GT", "value": "2000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 100: Múltiplas tentativas de pagamento
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLAS_TENTATIVAS_PAGAMENTO',
  'Mais de 3 tentativas de pagamento para a mesma compra - possível teste de cartão',
  'VELOCITY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "paymentAttempts", "operator": "GT", "value": "3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- PARTE 4: NOVAS REGRAS SIMPLES (101-150)
-- Categoria: COMPORTAMENTO ANÔMALO
-- =====================================================

-- Regra 101: Desvio do padrão de gastos
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'DESVIO_PADRAO_GASTOS',
  'Valor da transação está 3 desvios padrão acima da média histórica do cliente',
  'ANOMALY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "amountStdDeviation", "operator": "GT", "value": "3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 102: Categoria de compra incomum
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CATEGORIA_INCOMUM',
  'Compra em categoria de estabelecimento nunca utilizada pelo cliente',
  'ANOMALY',
  50,
  50,
  true,
  'SUSPICIOUS',
  '[{"field": "isUnusualMccCategory", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 103: Horário de compra incomum
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'HORARIO_INCOMUM',
  'Transação em horário fora do padrão habitual do cliente',
  'ANOMALY',
  45,
  45,
  true,
  'SUSPICIOUS',
  '[{"field": "isUnusualTimeOfDay", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 104: Localização incomum
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'LOCALIZACAO_INCOMUM',
  'Transação em localização fora do padrão habitual do cliente',
  'ANOMALY',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "isUnusualLocation", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 105: Frequência de compras anormal
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'FREQUENCIA_ANORMAL',
  'Frequência de transações muito acima do padrão histórico do cliente',
  'ANOMALY',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "frequencyRatio", "operator": "GT", "value": "3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 106: Mudança súbita de comportamento
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUDANCA_SUBITA_COMPORTAMENTO',
  'Múltiplos indicadores de mudança de comportamento detectados simultaneamente',
  'ANOMALY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "behaviorChangeScore", "operator": "GT", "value": "70"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: PIX E TRANSFERÊNCIAS (BCB)
-- =====================================================

-- Regra 107: PIX valor alto primeira vez
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIX_VALOR_ALTO_PRIMEIRA_VEZ',
  'Primeira transferência PIX do cliente com valor acima de R$ 5.000',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionType", "operator": "EQ", "value": "PIX"}, {"field": "isFirstPixTransaction", "operator": "EQ", "value": "true"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 108: PIX para chave aleatória valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIX_CHAVE_ALEATORIA_VALOR_ALTO',
  'PIX para chave aleatória (EVP) acima de R$ 3.000 - maior risco de fraude',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionType", "operator": "EQ", "value": "PIX"}, {"field": "pixKeyType", "operator": "EQ", "value": "EVP"}, {"field": "transactionAmount", "operator": "GT", "value": "3000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 109: PIX noturno valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIX_NOTURNO_VALOR_ALTO',
  'PIX entre 20:00 e 06:00 com valor acima de R$ 2.000 - horário de maior risco',
  'CONTEXT',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionType", "operator": "EQ", "value": "PIX"}, {"field": "transactionTime", "operator": "NOT_BETWEEN", "value": "060000,200000"}, {"field": "transactionAmount", "operator": "GT", "value": "2000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 110: PIX múltiplos destinatários
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIX_MULTIPLOS_DESTINATARIOS',
  'PIX para mais de 5 destinatários diferentes em 1 hora - possível esvaziamento',
  'VELOCITY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionType", "operator": "EQ", "value": "PIX"}, {"field": "velocity", "operator": "VELOCITY_DISTINCT_GT", "value": "PAYER,60,PAYEE,5"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 111: PIX destinatário em lista de suspeitos
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIX_DESTINATARIO_SUSPEITO',
  'PIX para conta/chave marcada como suspeita no sistema MED do BCB',
  'SECURITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field": "transactionType", "operator": "EQ", "value": "PIX"}, {"field": "payeeInMedList", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 112: PIX sequencial mesmo valor
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PIX_SEQUENCIAL_MESMO_VALOR',
  'Múltiplos PIX de mesmo valor em sequência - padrão de structuring',
  'VELOCITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "transactionType", "operator": "EQ", "value": "PIX"}, {"field": "sameAmountPixCount1h", "operator": "GT", "value": "3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: CONTA LARANJA (MONEY MULE)
-- =====================================================

-- Regra 113: Padrão recebimento e saque imediato
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'RECEBIMENTO_SAQUE_IMEDIATO',
  'Saque ou transferência logo após recebimento de crédito - padrão de conta laranja',
  'ANOMALY',
  85,
  85,
  true,
  'SUSPICIOUS',
  '[{"field": "minutesSinceLastCredit", "operator": "LT", "value": "30"}, {"field": "transactionType", "operator": "IN", "value": ["WITHDRAWAL", "TRANSFER", "PIX"]}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 114: Conta nova com alto volume
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CONTA_NOVA_ALTO_VOLUME',
  'Conta com menos de 30 dias movimentando mais de R$ 50.000',
  'ANOMALY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "accountAgeDays", "operator": "LT", "value": "30"}, {"field": "totalVolume30d", "operator": "GT", "value": "50000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 115: Múltiplos remetentes
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLOS_REMETENTES',
  'Recebimento de mais de 10 remetentes diferentes em 24h - possível conta laranja',
  'VELOCITY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "distinctPayersLast24h", "operator": "GT", "value": "10"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 116: Saldo zerado frequentemente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SALDO_ZERADO_FREQUENTE',
  'Conta com saldo zerado mais de 5 vezes nos últimos 30 dias',
  'ANOMALY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "zeroBalanceCount30d", "operator": "GT", "value": "5"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: ACCOUNT TAKEOVER (ATO)
-- =====================================================

-- Regra 117: Mudança de senha recente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUDANCA_SENHA_RECENTE',
  'Transação de alto valor logo após mudança de senha da conta',
  'SECURITY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "hoursSincePasswordChange", "operator": "LT", "value": "24"}, {"field": "transactionAmount", "operator": "GT", "value": "1000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 118: Mudança de email recente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUDANCA_EMAIL_RECENTE',
  'Transação de alto valor logo após mudança de email da conta',
  'SECURITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "hoursSinceEmailChange", "operator": "LT", "value": "48"}, {"field": "transactionAmount", "operator": "GT", "value": "1000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 119: Mudança de telefone recente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MUDANCA_TELEFONE_RECENTE',
  'Transação de alto valor logo após mudança de telefone da conta',
  'SECURITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "hoursSincePhoneChange", "operator": "LT", "value": "48"}, {"field": "transactionAmount", "operator": "GT", "value": "1000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 120: Novo beneficiário + valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'NOVO_BENEFICIARIO_VALOR_ALTO',
  'Transferência para beneficiário recém-cadastrado com valor acima de R$ 5.000',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "isNewBeneficiary", "operator": "EQ", "value": "true"}, {"field": "transactionAmount", "operator": "GT", "value": "5000"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 121: Login de novo dispositivo + transação
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'LOGIN_NOVO_DISPOSITIVO_TRANSACAO',
  'Transação logo após login de dispositivo nunca usado antes',
  'SECURITY',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "minutesSinceNewDeviceLogin", "operator": "LT", "value": "60"}, {"field": "transactionAmount", "operator": "GT", "value": "500"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 122: Múltiplas falhas de login
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLAS_FALHAS_LOGIN',
  'Transação após mais de 3 tentativas de login falhas nas últimas 24h',
  'SECURITY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "failedLoginAttempts24h", "operator": "GT", "value": "3"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: FRAUDE DE COMERCIANTE
-- =====================================================

-- Regra 123: Comerciante com alta taxa de chargeback
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'COMERCIANTE_ALTO_CHARGEBACK',
  'Estabelecimento com taxa de chargeback acima de 2%',
  'CONTEXT',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "merchantChargebackRate", "operator": "GT", "value": "2"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 124: Comerciante recém-cadastrado
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'COMERCIANTE_RECEM_CADASTRADO',
  'Transação em estabelecimento cadastrado há menos de 30 dias',
  'CONTEXT',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "merchantAgeDays", "operator": "LT", "value": "30"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 125: Comerciante com pico de vendas
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'COMERCIANTE_PICO_VENDAS',
  'Estabelecimento com volume de vendas 5x acima da média histórica',
  'ANOMALY',
  65,
  65,
  true,
  'SUSPICIOUS',
  '[{"field": "merchantSalesRatio", "operator": "GT", "value": "5"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 126: Terminal suspeito
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'TERMINAL_SUSPEITO',
  'Terminal POS marcado como suspeito por atividade anormal',
  'SECURITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field": "terminalSuspicious", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- =====================================================
-- Categoria: CARTÃO PRESENTE (CP)
-- =====================================================

-- Regra 127: Fallback para tarja magnética
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'FALLBACK_TARJA_MAGNETICA',
  'Transação com fallback para tarja magnética em cartão com chip',
  'SECURITY',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "EQ", "value": "80"}, {"field": "cardHasChip", "operator": "EQ", "value": "true"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 128: Contactless valor alto
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CONTACTLESS_VALOR_ALTO',
  'Transação contactless acima do limite sem PIN (R$ 200)',
  'SECURITY',
  60,
  60,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "IN", "value": ["07", "91"]}, {"field": "transactionAmount", "operator": "GT", "value": "200"}, {"field": "pinVerified", "operator": "EQ", "value": "false"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 129: Múltiplas transações contactless
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'MULTIPLAS_CONTACTLESS',
  'Mais de 5 transações contactless do mesmo cartão em 1 hora',
  'VELOCITY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "IN", "value": ["07", "91"]}, {"field": "velocity", "operator": "VELOCITY_COUNT_GT", "value": "PAN,60,5"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Regra 130: Transação presencial país diferente
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'PRESENCIAL_PAIS_DIFERENTE',
  'Transação presencial em país diferente do país de residência do titular',
  'CONTEXT',
  55,
  55,
  true,
  'SUSPICIOUS',
  '[{"field": "posEntryMode", "operator": "IN", "value": ["05", "07", "90", "91"]}, {"field": "merchantCountryCode", "operator": "FIELD_NEQ", "value": "cardholderCountryCode"}]',
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

