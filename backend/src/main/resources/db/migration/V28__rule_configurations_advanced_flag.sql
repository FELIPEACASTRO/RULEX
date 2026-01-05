-- V28: Marca regras do ruleset "advanced" no banco (sem listas hardcoded no código)
-- Data: 2026-01-05

ALTER TABLE rule_configurations
ADD COLUMN IF NOT EXISTS advanced BOOLEAN NOT NULL DEFAULT FALSE;

CREATE INDEX IF NOT EXISTS idx_rule_configurations_advanced_enabled
ON rule_configurations (advanced, enabled);
