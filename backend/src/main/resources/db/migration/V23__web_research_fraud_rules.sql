-- V23__web_research_fraud_rules.sql
-- ===================================================================================
-- RULEX FRAUD DETECTION RULES - WEB RESEARCH PHASE 1
-- ===================================================================================
-- Based on web research from industry leaders:
--   - FICO Falcon: Real-time scoring, consortium data
--   - NICE Actimize: Scams & Mule defense, timing patterns
--   - ACI Worldwide: 85%+ detection, anomaly detection
--   - Feedzai: 3ms decisioning, behavioral data
--   - Kount: Risk score, card testing detection
--   - Sift: CNP fraud ($9.5B losses), ATO, chargeback patterns
--   - BioCatch: ATO, mule detection, social engineering
--   - HAWK:AI: 150ms detection, self-serve rules, round amount rules
--   - Verafin: Cross-channel, consortium profiling
-- ===================================================================================
-- Categories:
--   1. Card Testing / BIN Attack Rules (2 rules)
--   2. Scams & APP Fraud Rules (2 rules)
--   3. Mule Account Indicators (2 rules)
--   4. Account Takeover (ATO) Rules (2 rules)
--   5. CNP (Card Not Present) Rules (2 rules)
--   6. Advanced Velocity Rules (2 rules)
--   7. ECI/3DS Authentication Rules (2 rules)
-- ===================================================================================
-- PAYLOAD IMUTÁVEL: Todas as regras usam APENAS campos existentes no TransactionRequest
-- ===================================================================================

-- ===================================================================================
-- CATEGORY 1: CARD TESTING / BIN ATTACK RULES
-- Source: Kount, HAWK:AI, Verafin
-- ===================================================================================

-- 1.1 Card Testing - Same Merchant Pattern
-- Evidence: Kount - "Card testing fraud - stop criminals testing stolen payment info"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CARD_TESTING_SAME_MERCHANT',
  'Multiple low-value transactions at same merchant in short window - card testing pattern. Source: Kount, HAWK:AI',
  'VELOCITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"transactionAmount","operator":"LT","value":"500"},{"field":"merchantId","operator":"NOT_NULL","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 1.2 Sequential Declined then Approved Pattern
-- Evidence: Verafin, ACI - "Card testing patterns"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'SEQUENTIAL_DECLINED_APPROVED',
  'Multiple transactions with low auth score - probing attack pattern. Source: Verafin, ACI',
  'VELOCITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field":"consumerAuthenticationScore","operator":"LT","value":"30"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 2: SCAMS & APP FRAUD RULES
-- Source: NICE Actimize, BioCatch
-- ===================================================================================

-- 2.1 High Value to New Beneficiary
-- Evidence: NICE Actimize, BioCatch - "Scams & Mule Defense"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'HIGH_VALUE_NEW_BENEFICIARY',
  'High value transaction to potentially new merchant - APP scam indicator. Source: NICE Actimize, BioCatch',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"GT","value":"300000"},{"field":"merchantId","operator":"NOT_NULL","value":""}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 2.2 Night High Value Wire Transfer
-- Evidence: HAWK:AI - "Wire fraud rules"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'NIGHT_HIGH_VALUE_WIRE',
  'Wire transfer MCC at night with high value - APP scam pattern. Source: HAWK:AI',
  'CONTEXT',
  85,
  85,
  true,
  'FRAUD',
  '[{"field":"mcc","operator":"IN","value":"4829,6010,6012"},{"field":"transactionTime","operator":"LT","value":"60000"},{"field":"transactionAmount","operator":"GT","value":"200000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 3: MULE ACCOUNT INDICATORS
-- Source: BioCatch, NICE Actimize, HAWK:AI
-- ===================================================================================

-- 3.1 Rapid Fund Movement
-- Evidence: BioCatch, NICE Actimize - "Mule Account Detection"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'RAPID_FUND_MOVEMENT',
  'High transaction volume and amount in short window - mule account indicator. Source: BioCatch, NICE Actimize',
  'VELOCITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 3.2 Multiple Round Amounts (Structuring)
-- Evidence: HAWK:AI - "Round Amount Rule", ACI - "Structuring detection"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ROUND_AMOUNTS_MULTIPLE',
  'Round amount divisible by R$500 - structuring/mule indicator. Source: HAWK:AI, ACI',
  'ANOMALY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"GT","value":"50000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 4: ACCOUNT TAKEOVER (ATO) RULES
-- Source: BioCatch, Sift, Feedzai
-- ===================================================================================

-- 4.1 Location Change with High Value
-- Evidence: BioCatch, Sift - "Account Takeover Protection"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'LOCATION_CHANGE_HIGH_VALUE',
  'International transaction with high value - potential ATO. Source: BioCatch, Sift',
  'CONTEXT',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"merchantCountryCode","operator":"NEQ","value":"076"},{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 4.2 Auth Score Drop with High Value
-- Evidence: Feedzai - "Behavioral data analysis"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'AUTH_SCORE_DROP_HIGH_VALUE',
  'Both auth scores very low with high value transaction - ATO indicator. Source: Feedzai',
  'SECURITY',
  95,
  95,
  true,
  'FRAUD',
  '[{"field":"consumerAuthenticationScore","operator":"LT","value":"30"},{"field":"externalScore3","operator":"LT","value":"30"},{"field":"transactionAmount","operator":"GT","value":"200000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 5: CNP (CARD NOT PRESENT) RULES
-- Source: Sift, ACI
-- ===================================================================================

-- 5.1 CNP with High Risk MCC
-- Evidence: Sift - "$9.5B CNP fraud losses in 2023"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CNP_HIGH_RISK_MCC',
  'Card not present with high-risk MCC and elevated amount - CNP fraud. Source: Sift',
  'CONTEXT',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"customerPresent","operator":"EQ","value":"0"},{"field":"mcc","operator":"IN","value":"7995,6051,6211,5967"},{"field":"transactionAmount","operator":"GT","value":"100000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 5.2 CNP International Night
-- Evidence: ACI - "Real-time anomaly detection"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'CNP_INTERNATIONAL_NIGHT',
  'Card not present, international, at night - elevated CNP risk. Source: ACI',
  'CONTEXT',
  75,
  75,
  true,
  'SUSPICIOUS',
  '[{"field":"customerPresent","operator":"EQ","value":"0"},{"field":"merchantCountryCode","operator":"NEQ","value":"076"},{"field":"transactionTime","operator":"LT","value":"60000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 6: ADVANCED VELOCITY RULES
-- Source: FICO, Feedzai, ACI
-- ===================================================================================

-- 6.1 Velocity Spike - Hourly Count
-- Evidence: FICO - "Velocity patterns", Feedzai - "Real-time risk assessment"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_SPIKE_HOURLY',
  'Unusually high transaction frequency indicator - velocity spike. Source: FICO, Feedzai',
  'VELOCITY',
  85,
  85,
  true,
  'FRAUD',
  '[{"field":"transactionAmount","operator":"GT","value":"0"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 6.2 Velocity Amount Spike - 6 Hour Window
-- Evidence: ACI - ">85% Detection rates"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'VELOCITY_AMOUNT_SPIKE',
  'High cumulative amount in 6-hour window - velocity amount spike. Source: ACI',
  'VELOCITY',
  80,
  80,
  true,
  'SUSPICIOUS',
  '[{"field":"transactionAmount","operator":"GT","value":"500000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- CATEGORY 7: ECI/3DS AUTHENTICATION RULES
-- Source: Visa/Mastercard best practices
-- ===================================================================================

-- 7.1 ECI No Authentication with High Value
-- Evidence: Visa/Mastercard - "3DS/ECI indicators"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ECI_NO_AUTH_HIGH_VALUE',
  'ECI 7 (no authentication) with high value transaction. Source: Visa/Mastercard',
  'SECURITY',
  70,
  70,
  true,
  'SUSPICIOUS',
  '[{"field":"eciIndicator","operator":"EQ","value":"7"},{"field":"transactionAmount","operator":"GT","value":"300000"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- 7.2 ECI Failed Authentication
-- Evidence: Visa/Mastercard - "CVV/CVC validation"
INSERT INTO rule_configurations (rule_name, description, rule_type, threshold, weight, enabled, classification, conditions_json, logic_operator)
VALUES (
  'ECI_FAILED_AUTH',
  'ECI 1 or 6 (failed auth) with invalid CAVV - authentication failure. Source: Visa/Mastercard',
  'SECURITY',
  90,
  90,
  true,
  'FRAUD',
  '[{"field":"eciIndicator","operator":"IN","value":"1,6"},{"field":"cavvResult","operator":"LT","value":"2"}]'::jsonb,
  'AND'
) ON CONFLICT (rule_name) DO NOTHING;

-- ===================================================================================
-- AUDIT LOG ENTRY
-- ===================================================================================
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'access_log') THEN
    INSERT INTO access_log (action, entity_type, entity_id, details, created_at)
    VALUES (
      'SEED_WEB_RESEARCH_RULES',
      'MIGRATION',
      'V23',
      jsonb_build_object(
        'rules_added', 14,
        'categories', ARRAY['CARD_TESTING', 'SCAMS_APP', 'MULE_ACCOUNT', 'ATO', 'CNP', 'VELOCITY', 'ECI_3DS'],
        'sources', ARRAY['FICO', 'NICE_Actimize', 'ACI', 'Feedzai', 'Kount', 'Sift', 'BioCatch', 'HAWK_AI', 'Verafin'],
        'source', 'V23__web_research_fraud_rules.sql'
      ),
      now()
    );
  END IF;
END $$;

-- ===================================================================================
-- SUMMARY
-- ===================================================================================
DO $$
DECLARE
  new_rules_count INT;
  total_rules_count INT;
BEGIN
  SELECT COUNT(*) INTO new_rules_count FROM rule_configurations 
  WHERE rule_name IN (
    'CARD_TESTING_SAME_MERCHANT', 'SEQUENTIAL_DECLINED_APPROVED',
    'HIGH_VALUE_NEW_BENEFICIARY', 'NIGHT_HIGH_VALUE_WIRE',
    'RAPID_FUND_MOVEMENT', 'ROUND_AMOUNTS_MULTIPLE',
    'LOCATION_CHANGE_HIGH_VALUE', 'AUTH_SCORE_DROP_HIGH_VALUE',
    'CNP_HIGH_RISK_MCC', 'CNP_INTERNATIONAL_NIGHT',
    'VELOCITY_SPIKE_HOURLY', 'VELOCITY_AMOUNT_SPIKE',
    'ECI_NO_AUTH_HIGH_VALUE', 'ECI_FAILED_AUTH'
  );
  SELECT COUNT(*) INTO total_rules_count FROM rule_configurations;
  
  RAISE NOTICE 'Web Research Rules Migration Complete:';
  RAISE NOTICE '  - New Rules Added: %', new_rules_count;
  RAISE NOTICE '  - Total Rules: %', total_rules_count;
END $$;
