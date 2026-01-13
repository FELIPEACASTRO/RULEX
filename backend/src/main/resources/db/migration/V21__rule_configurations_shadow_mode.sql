-- V21: Align shadow-mode columns with the actual rule table used by the engine.
-- The engine uses `rule_configurations` (see RuleConfiguration entity).

ALTER TABLE rule_configurations
  ADD COLUMN IF NOT EXISTS shadow_mode VARCHAR(20) NOT NULL DEFAULT 'DISABLED';

ALTER TABLE rule_configurations
  ADD COLUMN IF NOT EXISTS canary_percentage INT NOT NULL DEFAULT 0;
