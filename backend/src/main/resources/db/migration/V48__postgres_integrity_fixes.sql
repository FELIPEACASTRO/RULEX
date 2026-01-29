-- ============================================================================
-- Migration V48: PostgreSQL Integrity Fixes
-- Data: 2026-01-29
-- Objetivo: Corrigir inconsistências de schema e regras complexas inválidas
-- ============================================================================

-- 1) Alias para compatibilidade: access_log -> access_logs
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.views
    WHERE table_schema = 'public'
      AND table_name = 'access_log'
  ) THEN
    EXECUTE 'CREATE VIEW access_log AS SELECT * FROM access_logs';
  END IF;
END $$;

-- 2) Garantir integridade de grupos de condições
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM information_schema.table_constraints
    WHERE table_schema = 'public'
      AND table_name = 'rule_condition_groups'
      AND constraint_name = 'chk_condition_groups_has_parent'
  ) THEN
    ALTER TABLE rule_condition_groups
      ADD CONSTRAINT chk_condition_groups_has_parent
      CHECK (rule_version_id IS NOT NULL OR complex_rule_id IS NOT NULL);
  END IF;
END $$;

-- 3) Desativar regras complexas com field_name inválido (contém operador)
UPDATE complex_rules cr
SET enabled = FALSE,
    status = CASE WHEN cr.status = 'ARCHIVED' THEN cr.status ELSE 'ARCHIVED' END
WHERE cr.id IN (
  SELECT DISTINCT rcg.complex_rule_id
  FROM rule_condition_groups rcg
  JOIN rule_conditions rc ON rc.group_id = rcg.id
  WHERE rc.field_name IN (
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
-- FIM DA MIGRATION V48
-- ============================================================================
