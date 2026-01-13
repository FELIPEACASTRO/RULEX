-- V12__complex_rules_crud.sql
-- Tabela principal para regras complexas (CRUD)

-- =========================================
-- TABELA: complex_rules
-- Armazena metadados das regras complexas
-- =========================================

CREATE TABLE IF NOT EXISTS complex_rules (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  key             VARCHAR(100) NOT NULL UNIQUE,
  title           VARCHAR(255) NOT NULL,
  description     TEXT,
  version         INT NOT NULL DEFAULT 1,
  status          VARCHAR(20) NOT NULL DEFAULT 'DRAFT',
  priority        INT NOT NULL DEFAULT 0,
  severity        INT NOT NULL DEFAULT 0,
  decision        VARCHAR(50) NOT NULL DEFAULT 'APROVADO',
  reason_template TEXT,
  enabled         BOOLEAN NOT NULL DEFAULT FALSE,
  created_by      UUID,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT now(),
  
  -- Constraints
  CONSTRAINT chk_complex_rules_status CHECK (status IN ('DRAFT', 'PUBLISHED', 'ARCHIVED', 'TESTING')),
  CONSTRAINT chk_complex_rules_decision CHECK (decision IN ('APROVADO', 'SUSPEITA_DE_FRAUDE', 'FRAUDE')),
  CONSTRAINT chk_complex_rules_priority CHECK (priority >= 0 AND priority <= 1000),
  CONSTRAINT chk_complex_rules_severity CHECK (severity >= 0 AND severity <= 100)
);

-- Índices para performance
CREATE INDEX IF NOT EXISTS idx_complex_rules_key ON complex_rules(key);
CREATE INDEX IF NOT EXISTS idx_complex_rules_status ON complex_rules(status);
CREATE INDEX IF NOT EXISTS idx_complex_rules_enabled ON complex_rules(enabled);
CREATE INDEX IF NOT EXISTS idx_complex_rules_priority ON complex_rules(priority DESC);

-- =========================================
-- Atualizar FK em rule_condition_groups para aceitar complex_rules
-- =========================================

-- Adicionar coluna complex_rule_id se não existir
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM information_schema.columns 
    WHERE table_name = 'rule_condition_groups' 
    AND column_name = 'complex_rule_id'
  ) THEN
    ALTER TABLE rule_condition_groups 
    ADD COLUMN complex_rule_id UUID REFERENCES complex_rules(id) ON DELETE CASCADE;
    
    CREATE INDEX idx_condition_groups_complex_rule ON rule_condition_groups(complex_rule_id);
  END IF;
END $$;

-- Tornar rule_version_id nullable para permitir uso com complex_rules
ALTER TABLE rule_condition_groups 
ALTER COLUMN rule_version_id DROP NOT NULL;

-- Adicionar constraint para garantir que pelo menos uma FK está preenchida
-- (Comentado por enquanto para não quebrar dados existentes)
-- ALTER TABLE rule_condition_groups 
-- ADD CONSTRAINT chk_condition_groups_has_parent 
-- CHECK (rule_version_id IS NOT NULL OR complex_rule_id IS NOT NULL);

-- =========================================
-- Comentários
-- =========================================

COMMENT ON TABLE complex_rules IS 'Tabela principal para regras complexas com suporte a grupos aninhados';
COMMENT ON COLUMN complex_rules.key IS 'Chave única da regra (identificador legível)';
COMMENT ON COLUMN complex_rules.status IS 'Status: DRAFT, PUBLISHED, ARCHIVED, TESTING';
COMMENT ON COLUMN complex_rules.decision IS 'Decisão quando a regra é acionada: APROVADO, SUSPEITA_DE_FRAUDE, FRAUDE';
COMMENT ON COLUMN complex_rules.priority IS 'Prioridade de execução (0-1000, maior = mais prioritário)';
COMMENT ON COLUMN complex_rules.severity IS 'Severidade do alerta (0-100)';
