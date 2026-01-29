-- ============================================================================
-- Migration V49: Fix velocity/aggregation conditions with operator in field_name
-- Data: 2026-01-29
-- Objetivo: Corrigir rule_conditions onde field_name contém operador agregado
-- ============================================================================

-- Mapear field_name inválido para operador correto e usar chave padrão (PAN)
UPDATE rule_conditions
SET operator = field_name::condition_operator,
    field_name = 'pan'
WHERE field_name IN (
    'COUNT_LAST_N_HOURS',
    'COUNT_LAST_N_DAYS',
    'COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS',
    'COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS',
    'COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS',
    'COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS',
    'COUNT_DISTINCT_PANS_LAST_N_HOURS',
    'COUNT_DISTINCT_ACCOUNTS',
    'SUM_LAST_N_HOURS',
    'SUM_LAST_N_DAYS',
    'AVG_LAST_N_DAYS',
    'MIN_AMOUNT_LAST_N_DAYS',
    'MAX_AMOUNT_LAST_N_DAYS',
    'VELOCITY_SPIKE',
    'AMOUNT_SPIKE',
    'TIME_SINCE_LAST_LT',
    'DISTANCE_FROM_LAST_GT'
  )
  AND operator IN ('GT', 'GTE', 'LT', 'LTE', 'EQ', 'NEQ');

-- Restaurar status para regras arquivadas por correção automática
UPDATE complex_rules cr
SET status = 'PUBLISHED',
    enabled = FALSE
WHERE cr.status = 'ARCHIVED'
  AND cr.id IN (
    SELECT DISTINCT rcg.complex_rule_id
    FROM rule_condition_groups rcg
    JOIN rule_conditions rc ON rc.group_id = rcg.id
    WHERE rc.operator IN (
      'COUNT_LAST_N_HOURS',
      'COUNT_LAST_N_DAYS',
      'COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS',
      'COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS',
      'COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS',
      'COUNT_DISTINCT_COUNTRIES_LAST_N_DAYS',
      'COUNT_DISTINCT_PANS_LAST_N_HOURS',
      'COUNT_DISTINCT_ACCOUNTS',
      'SUM_LAST_N_HOURS',
      'SUM_LAST_N_DAYS',
      'AVG_LAST_N_DAYS',
      'MIN_AMOUNT_LAST_N_DAYS',
      'MAX_AMOUNT_LAST_N_DAYS',
      'VELOCITY_SPIKE',
      'AMOUNT_SPIKE',
      'TIME_SINCE_LAST_LT',
      'DISTANCE_FROM_LAST_GT'
    )
  );

-- ============================================================================
-- FIM DA MIGRATION V49
-- ============================================================================
