-- ============================================================================
-- Migration V38: Sincronizar enum rule_status entre sistemas
-- Data: 2026-01-06
-- Objetivo: Unificar os valores de status entre homolog (rules) e complex_rules
-- ============================================================================

-- ============================================================================
-- PARTE 1: Adicionar valores faltantes ao enum rule_status (usado em rules/homolog)
-- ============================================================================

-- Adicionar ARCHIVED ao enum rule_status (se não existir)
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_enum 
        WHERE enumlabel = 'ARCHIVED' 
        AND enumtypid = (SELECT oid FROM pg_type WHERE typname = 'rule_status')
    ) THEN
        ALTER TYPE rule_status ADD VALUE 'ARCHIVED';
    END IF;
END $$;

-- Adicionar TESTING ao enum rule_status (se não existir)
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_enum 
        WHERE enumlabel = 'TESTING' 
        AND enumtypid = (SELECT oid FROM pg_type WHERE typname = 'rule_status')
    ) THEN
        ALTER TYPE rule_status ADD VALUE 'TESTING';
    END IF;
END $$;

-- ============================================================================
-- PARTE 2: Atualizar constraint em complex_rules para incluir DEPRECATED
-- ============================================================================

-- Remover constraint antiga
ALTER TABLE complex_rules DROP CONSTRAINT IF EXISTS chk_complex_rules_status;

-- Adicionar constraint atualizada com todos os valores
ALTER TABLE complex_rules 
ADD CONSTRAINT chk_complex_rules_status 
CHECK (status IN ('DRAFT', 'PUBLISHED', 'DEPRECATED', 'ARCHIVED', 'TESTING'));

-- ============================================================================
-- COMENTÁRIOS
-- ============================================================================

COMMENT ON TYPE rule_status IS 'Status de regras: DRAFT (rascunho), PUBLISHED (ativa), DEPRECATED (descontinuada), ARCHIVED (arquivada), TESTING (em teste)';

-- ============================================================================
-- FIM DA MIGRATION V38
-- ============================================================================
