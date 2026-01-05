-- V29: Marcar regras avançadas já existentes (seed) como advanced=true
-- Data: 2026-01-05
-- Obs: Mantém comportamento atual do /analyze-advanced, mas move a definição do ruleset para o DB.

UPDATE rule_configurations
SET advanced = TRUE
WHERE rule_name IN (
  'EMV_SECURITY_CHECK',
  'TERMINAL_VERIFICATION_FAILED',
  'EXPIRED_CARD',
  'SUSPICIOUS_TRANSACTION_TYPE',
  'UNUSUAL_CARD_MEDIA',
  'SUSPICIOUS_TERMINAL',
  'ECOMMERCE_NO_AVS',
  'POS_SECURITY_MISSING',
  'CARD_CAPTURE_FRAUD',
  'PIN_CVV_LIMIT_EXCEEDED',
  'OFFLINE_PIN_FAILED',
  'MISSING_CVV2_HIGH_RISK'
);
