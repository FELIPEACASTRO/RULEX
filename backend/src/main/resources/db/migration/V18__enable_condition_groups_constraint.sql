-- V18__enable_condition_groups_constraint.sql
-- Ativa a constraint CHECK em rule_condition_groups que garante
-- que cada grupo tenha pelo menos uma FK (rule_version_id OU complex_rule_id)

-- =========================================
-- PASSO 1: Identificar e corrigir dados órfãos
-- =========================================

-- Criar tabela temporária para backup de grupos órfãos (se houver)
CREATE TEMP TABLE orphan_condition_groups_backup AS
SELECT * FROM rule_condition_groups
WHERE rule_version_id IS NULL AND complex_rule_id IS NULL;

-- Log de quantos registros órfãos existem
DO $$
DECLARE
  orphan_count INTEGER;
BEGIN
  SELECT COUNT(*) INTO orphan_count FROM orphan_condition_groups_backup;
  IF orphan_count > 0 THEN
    RAISE NOTICE 'Encontrados % grupos de condições órfãos. Serão removidos.', orphan_count;
  ELSE
    RAISE NOTICE 'Nenhum grupo de condições órfão encontrado. Constraint pode ser ativada com segurança.';
  END IF;
END $$;

-- =========================================
-- PASSO 2: Remover grupos órfãos (cascata para condições)
-- =========================================

-- Primeiro, remover as condições associadas aos grupos órfãos
DELETE FROM rule_conditions
WHERE group_id IN (
  SELECT id FROM rule_condition_groups
  WHERE rule_version_id IS NULL AND complex_rule_id IS NULL
);

-- Depois, remover os grupos órfãos
DELETE FROM rule_condition_groups
WHERE rule_version_id IS NULL AND complex_rule_id IS NULL;

-- =========================================
-- PASSO 3: Ativar a constraint CHECK
-- =========================================

-- Adicionar constraint que garante pelo menos uma FK preenchida
ALTER TABLE rule_condition_groups
ADD CONSTRAINT chk_condition_groups_has_parent
CHECK (rule_version_id IS NOT NULL OR complex_rule_id IS NOT NULL);

-- =========================================
-- PASSO 4: Adicionar constraint de exclusividade (opcional mas recomendado)
-- Garante que um grupo pertence a apenas UM pai (não ambos)
-- =========================================

-- Comentado por enquanto - pode ser ativado se necessário
-- ALTER TABLE rule_condition_groups
-- ADD CONSTRAINT chk_condition_groups_single_parent
-- CHECK (
--   (rule_version_id IS NOT NULL AND complex_rule_id IS NULL) OR
--   (rule_version_id IS NULL AND complex_rule_id IS NOT NULL)
-- );

-- =========================================
-- Comentários
-- =========================================

COMMENT ON CONSTRAINT chk_condition_groups_has_parent ON rule_condition_groups 
IS 'Garante que cada grupo de condições pertence a uma rule_version OU complex_rule';
