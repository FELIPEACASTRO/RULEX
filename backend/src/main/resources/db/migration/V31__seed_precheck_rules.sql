-- V31__seed_precheck_rules.sql
-- Seed DB-driven equivalents for previously hardcoded precheck logic
-- (blocklist + travel anomalies). These are represented as generic conditions_json rules.

-- Blocklist (BloomFilter-backed) rules
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLOCKLIST_PAN',
  'PAN encontrado em blacklist (BloomFilter/DB) - bloquear como fraude',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"isBlacklistedPan","operator":"IS_TRUE","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLOCKLIST_MERCHANT',
  'MerchantId encontrado em blacklist (BloomFilter/DB) - bloquear como fraude',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"isBlacklistedMerchantId","operator":"IS_TRUE","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLOCKLIST_CUSTOMER',
  'CustomerId encontrado em blacklist (BloomFilter/DB) - bloquear como fraude',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"isBlacklistedCustomerId","operator":"IS_TRUE","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'BLOCKLIST_DEVICE',
  'DeviceId (terminalId) encontrado em blacklist (BloomFilter/DB) - bloquear como fraude',
  'SECURITY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"isBlacklistedDeviceId","operator":"IS_TRUE","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- Travel anomaly rules (ImpossibleTravelService-backed)
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'IMPOSSIBLE_TRAVEL',
  'Viagem impossível detectada (geo) - bloquear como fraude',
  'ANOMALY',
  100,
  100,
  true,
  'FRAUD',
  '[{"field":"isImpossibleTravel","operator":"IS_TRUE","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SUSPICIOUS_TRAVEL',
  'Viagem suspeita detectada (geo) - sinalizar como suspeita',
  'ANOMALY',
  100,
  60,
  true,
  'SUSPICIOUS',
  '[{"field":"isSuspiciousTravel","operator":"IS_TRUE","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;
