-- ============================================================================
-- Migration V39: Correção de Campos e Operadores Inválidos
-- Data: 2026-01-06
-- Objetivo: Corrigir regras que usam campos inexistentes no payload ou
--           operadores inexistentes no enum ConditionOperator
-- ============================================================================

-- ============================================================================
-- PARTE 1: Correção de Operadores (aliases para operadores existentes)
-- Nota: Usamos cast para TEXT para evitar erro de enum inválido quando
--       o valor não existe no enum (o que é esperado nesta migração de correção)
-- ============================================================================

-- Corrigir IN_LIST -> IN
UPDATE rule_conditions SET operator = 'IN' WHERE operator::text = 'IN_LIST';

-- Corrigir NOT_IN_LIST -> NOT_IN
UPDATE rule_conditions SET operator = 'NOT_IN' WHERE operator::text = 'NOT_IN_LIST';

-- Corrigir EQ_FIELD -> FIELD_EQ
UPDATE rule_conditions SET operator = 'FIELD_EQ' WHERE operator::text = 'EQ_FIELD';

-- Corrigir NEQ_FIELD -> FIELD_NEQ
UPDATE rule_conditions SET operator = 'FIELD_NEQ' WHERE operator::text = 'NEQ_FIELD';

-- Corrigir GT_FIELD -> FIELD_GT
UPDATE rule_conditions SET operator = 'FIELD_GT' WHERE operator::text = 'GT_FIELD';

-- Corrigir MODULO_ZERO -> MOD_EQ (com value 0)
UPDATE rule_conditions SET operator = 'MOD_EQ', value_single = '0' WHERE operator::text = 'MODULO_ZERO';

-- ============================================================================
-- PARTE 2: Correção de Campos (mapear para campos existentes no payload)
-- ============================================================================

-- Corrigir merchantCountry -> merchantCountryCode
UPDATE rule_conditions SET field_name = 'merchantCountryCode' WHERE field_name = 'merchantCountry';

-- Corrigir transactionHour -> usar expressão derivada de transactionTime
-- (transactionTime é um Integer no formato HHMMSS, então hora = transactionTime / 10000)
UPDATE rule_conditions
SET field_name = 'transactionTime',
    value_expression = 'HOUR_FROM_HHMMSS'
WHERE field_name = 'transactionHour';

-- ============================================================================
-- PARTE 3: Desativar regras que usam campos que precisam ser enriquecidos
-- (Esses campos precisam vir de um serviço de enriquecimento externo)
-- ============================================================================

-- Campos que precisam de enriquecimento externo:
-- accountAgeInDays, customerChargebackCount, customerRefundCount30Days,
-- deviceAccountCount, deviceFraudHistory, deviceId, devicePromoUseCount,
-- geoLocation, impossibleTravel, isFirstTransaction, passwordChangedWithinDays,
-- previousCvvFailures, promoCodeUsed, shippingAddress, transactionsLastMinute

-- Desativar regras que usam campos de enriquecimento (shadow_mode = true)
UPDATE complex_rules
SET shadow_mode = true,
    description = CONCAT(description, ' [SHADOW: Requer campo de enriquecimento]')
WHERE id IN (
    SELECT DISTINCT cr.id
    FROM complex_rules cr
    JOIN rule_condition_groups rcg ON rcg.complex_rule_id = cr.id
    JOIN rule_conditions rc ON rc.group_id = rcg.id
    WHERE rc.field_name IN (
        'accountAgeInDays',
        'customerChargebackCount',
        'customerRefundCount30Days',
        'deviceAccountCount',
        'deviceFraudHistory',
        'deviceId',
        'devicePromoUseCount',
        'geoLocation',
        'impossibleTravel',
        'isFirstTransaction',
        'passwordChangedWithinDays',
        'previousCvvFailures',
        'promoCodeUsed',
        'shippingAddress',
        'transactionsLastMinute',
        'escalationPattern',
        'roundNumberPattern',
        'splitTransactionPattern'
    )
);

-- ============================================================================
-- PARTE 4: Adicionar comentário de auditoria
-- ============================================================================

COMMENT ON TABLE complex_rules IS 'Regras complexas de fraude. Auditado em 2026-01-06 via Triple Check V39.';

-- ============================================================================
-- FIM DA MIGRATION V39
-- ============================================================================
