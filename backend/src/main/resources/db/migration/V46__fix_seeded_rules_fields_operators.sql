-- ============================================================================
-- Migration V46: Correção pós-seed de campos e operadores
-- Data: 2026-01-20
-- Objetivo: Aplicar as correções de operadores/campos às regras semeadas após V39
-- ============================================================================

-- ============================================================================
-- PARTE 1: Correção de Operadores (aliases para operadores existentes)
-- Nota: Usamos cast para TEXT para evitar erro de enum inválido
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

-- Desativar regras que usam campos de enriquecimento (enabled = false)
-- Nota: Usamos enabled em vez de shadow_mode pois shadow_mode não existe em complex_rules
UPDATE complex_rules 
SET enabled = false, 
    description = CONCAT(description, ' [DISABLED: Requer campo de enriquecimento]')
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
-- FIM DA MIGRATION V46
-- ============================================================================
