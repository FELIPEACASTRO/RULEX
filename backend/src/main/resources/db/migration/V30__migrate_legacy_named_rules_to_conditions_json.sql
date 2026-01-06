-- Migrar regras legadas (antes avaliadas por switch(rule_name)) para condições configuráveis em DB.
-- Objetivo: remover lógica hardcoded por nome e manter o mesmo comportamento para regras existentes.

-- LOW_AUTHENTICATION_SCORE: consumerAuthenticationScore < threshold
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','consumerAuthenticationScore','operator','LT','value', threshold::text)
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'LOW_AUTHENTICATION_SCORE'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- LOW_EXTERNAL_SCORE: externalScore3 < threshold
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','externalScore3','operator','LT','value', threshold::text)
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'LOW_EXTERNAL_SCORE'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- INVALID_CAVV: cavvResult != 0
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','cavvResult','operator','NE','value','0')
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'INVALID_CAVV'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- INVALID_CRYPTOGRAM: cryptogramValid != 'V' (legacy treated null as invalid)
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','cryptogramValid','operator','IS_NULL'),
        jsonb_build_object('field','cryptogramValid','operator','NE','value','V')
    ),
    logic_operator = 'OR'
WHERE rule_name = 'INVALID_CRYPTOGRAM'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- CVV_MISMATCH: cvv2Response == 'N'
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','cvv2Response','operator','EQ','value','N')
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'CVV_MISMATCH'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- HIGH_TRANSACTION_AMOUNT: transactionAmount > threshold
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','transactionAmount','operator','GT','value', threshold::text)
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'HIGH_TRANSACTION_AMOUNT'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- HIGH_RISK_MCC: mccIsHighRisk == true (computed field backed by DB mcc_category)
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','mccIsHighRisk','operator','IS_TRUE')
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'HIGH_RISK_MCC'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- INTERNATIONAL_TRANSACTION: merchantCountryCode != '076' AND not null
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','merchantCountryCode','operator','IS_NOT_NULL'),
        jsonb_build_object('field','merchantCountryCode','operator','NE','value','076')
    ),
    logic_operator = 'AND'
WHERE rule_name = 'INTERNATIONAL_TRANSACTION'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- CARD_NOT_PRESENT: customerPresent != 'Y' (legacy treated null as not present)
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','customerPresent','operator','IS_NULL'),
        jsonb_build_object('field','customerPresent','operator','NE','value','Y')
    ),
    logic_operator = 'OR'
WHERE rule_name = 'CARD_NOT_PRESENT'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- PIN_VERIFICATION_FAILED: pinVerifyCode == 'I'
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','pinVerifyCode','operator','EQ','value','I')
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'PIN_VERIFICATION_FAILED'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- CVV_PIN_LIMIT_EXCEEDED: cvvPinTryLimitExceeded == 1
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','cvvPinTryLimitExceeded','operator','EQ','value','1')
    ),
    logic_operator = COALESCE(logic_operator, 'AND')
WHERE rule_name = 'CVV_PIN_LIMIT_EXCEEDED'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);

-- OFFLINE_PIN_FAILED: cvrofflinePinVerificationPerformed == 1 AND cvrofflinePinVerificationFailed == 1
UPDATE rule_configurations
SET conditions_json = jsonb_build_array(
        jsonb_build_object('field','cvrofflinePinVerificationPerformed','operator','EQ','value','1'),
        jsonb_build_object('field','cvrofflinePinVerificationFailed','operator','EQ','value','1')
    ),
    logic_operator = 'AND'
WHERE rule_name = 'OFFLINE_PIN_FAILED'
  AND (conditions_json IS NULL OR jsonb_array_length(conditions_json) = 0);
