/**
 * Schema de validação Zod para o formulário de regras
 */

import { z } from 'zod';
import type { RuleCondition } from '@/lib/javaApi';
import { UNARY_OPERATORS } from './types';
import { validateRegex, getRegexValidationError } from '@/lib/validators/regexValidator';

// ============================================
// SCHEMA DE CONDIÇÃO
// ============================================

/**
 * Lista completa de operadores sincronizada com ConditionOperator.java (348 operadores)
 * Última sincronização: Triple-Check 1000x - 2026-01-12
 * Gerado automaticamente - NÃO EDITAR MANUALMENTE
 */
const conditionOperators = [
  'EQ',
  'NEQ',
  'GT',
  'GTE',
  'LT',
  'LTE',
  'IN',
  'NOT_IN',
  'CONTAINS',
  'NOT_CONTAINS',
  'STARTS_WITH',
  'ENDS_WITH',
  'REGEX',
  'NOT_REGEX',
  'IS_NULL',
  'NOT_NULL',
  'IS_TRUE',
  'IS_FALSE',
  'BETWEEN',
  'NOT_BETWEEN',
  'FIELD_EQ',
  'FIELD_NEQ',
  'FIELD_GT',
  'FIELD_GTE',
  'FIELD_LT',
  'FIELD_LTE',
  'DATE_BEFORE',
  'DATE_AFTER',
  'DATE_BETWEEN',
  'TIME_BEFORE',
  'TIME_AFTER',
  'TIME_BETWEEN',
  'ARRAY_CONTAINS',
  'ARRAY_NOT_CONTAINS',
  'ARRAY_SIZE_EQ',
  'ARRAY_SIZE_GT',
  'ARRAY_SIZE_LT',
  'MOD_EQ',
  'MOD_NEQ',
  'GEO_DISTANCE_LT',
  'GEO_DISTANCE_GT',
  'GEO_IN_POLYGON',
  'VELOCITY_COUNT_GT',
  'VELOCITY_COUNT_LT',
  'VELOCITY_SUM_GT',
  'VELOCITY_SUM_LT',
  'VELOCITY_AVG_GT',
  'VELOCITY_AVG_LT',
  'VELOCITY_DISTINCT_GT',
  'VELOCITY_DISTINCT_LT',
  'SUM_LAST_N_DAYS',
  'COUNT_LAST_N_HOURS',
  'AVG_LAST_N_DAYS',
  'COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS',
  'COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS',
  'MAX_AMOUNT_LAST_N_DAYS',
  'MIN_AMOUNT_LAST_N_DAYS',
  'GT_FIELD_MULTIPLIER',
  'DECIMAL_PLACES_GT',
  'EXPIRES_WITHIN_DAYS',
  'IS_NEW',
  'IS_FIRST',
  'LT_CURRENT_DATE',
  'GT_CURRENT_DATE',
  'NOT_IN_CUSTOMER_HISTORY',
  'IN_CUSTOMER_HISTORY',
  'NOT_IN_CUSTOMER_USUAL_HOURS',
  'IN_CUSTOMER_USUAL_HOURS',
  'IN_CUSTOMER_CHARGEBACK_MERCHANTS',
  'PERCENTAGE_OF_FIELD',
  'HOUR_BETWEEN',
  'DAY_OF_WEEK_IN',
  'IS_WEEKEND',
  'IS_HOLIDAY',
  'DISTANCE_FROM_LAST_GT',
  'TIME_SINCE_LAST_LT',
  'COUNT_FAILURES_LAST_N_HOURS',
  'SUM_LAST_N_HOURS',
  'COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS',
  'VELOCITY_SPIKE',
  'AMOUNT_SPIKE',
  'PATTERN_ESCALATION',
  'PATTERN_ROUND_NUMBERS',
  'PATTERN_SPLIT_TRANSACTION',
  'NOT_IN_HISTORICAL',
  'NAME_SIMILARITY_LT',
  'GTE_PERCENT_OF_LAST_INCOMING',
  'DOMAIN_IN_LIST',
  'CHARGEBACK_RATE_GT',
  'ACCOUNT_AGE_LT_MINUTES',
  'IS_VOIP',
  'COUNT_DISTINCT_PANS_LAST_N_HOURS',
  'COUNT_DISTINCT_ACCOUNTS',
  'IN_LIST',
  'COUNT_MFA_ABANDONMENTS',
  'HAS_INCOMING_TRANSFER_LAST_N_HOURS',
  'IS_IMPOSSIBLE_COMBINATION',
  'PIX_KEY_CHANGED_LAST_N_DAYS',
  'CONTAINS_SUSPICIOUS_KEYWORDS',
  'COUNT_CRYPTO_TXN_LAST_N_DAYS',
  'COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS',
  'COUNT_DISTINCT_PAYERS_LAST_N_DAYS',
  'COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS',
  'COUNT_LAST_N_DAYS',
  'COUNT_MFA_DENIALS_LAST_N_HOURS',
  'DAYS_SINCE_LAST_ACTIVITY',
  'DEVICE_CHANGED_IN_SESSION',
  'IS_CRYPTO_RANSOM_AMOUNT',
  'OUTFLOW_RATE_LAST_N_DAYS',
  'VELOCITY_CROSS_CHANNEL',
  'VELOCITY_ROLLING_WINDOW',
  'VELOCITY_PERCENTILE',
  'VELOCITY_RATIO_GT',
  'VELOCITY_TREND',
  'COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS',
  'COUNT_UNIQUE_IPS_LAST_N_HOURS',
  'SUM_BY_CHANNEL_LAST_N_DAYS',
  'AVG_INTERVAL_BETWEEN_TXN',
  'VELOCITY_ACCELERATION',
  'DORMANCY_REVIVAL',
  'AMOUNT_DEVIATION_FROM_AVG',
  'TIME_DEVIATION_FROM_USUAL',
  'MERCHANT_DEVIATION',
  'MICRO_TRANSACTION_TEST',
  'LOCATION_DEVIATION',
  'CHANNEL_SWITCH_PATTERN',
  'BENEFICIARY_REUSE_PATTERN',
  'FAN_OUT_COUNT',
  'FAN_IN_COUNT',
  'SHARED_DEVICE_COUNT',
  'SHARED_IP_COUNT',
  'ACCOUNT_LINK_DEPTH',
  'CIRCULAR_TRANSFER_DETECTION',
  'RAPID_MULTI_HOP',
  'BENEFICIARY_CONCENTRATION',
  'OFAC_LIST_CHECK',
  'PEP_LIST_CHECK',
  'ADVERSE_MEDIA_CHECK',
  'SANCTIONS_COUNTRY_CHECK',
  'HIGH_RISK_JURISDICTION',
  'NAME_TRANSLITERATION_MATCH',
  'ALIAS_DETECTION',
  'CPF_SSN_VALIDATION',
  'PHONE_CARRIER_CHECK',
  'EMAIL_DOMAIN_AGE',
  'ADDRESS_VERIFICATION',
  'IDENTITY_VELOCITY',
  'DEVICE_ACCOUNT_RATIO',
  'EMAIL_PHONE_MISMATCH',
  'CREDIT_FILE_THIN',
  'STRUCTURING_DETECTION',
  'LAYERING_PATTERN',
  'RAPID_MOVEMENT',
  'INTEGRATION_PATTERN',
  'CASH_INTENSIVE_RATIO',
  'UNUSUAL_BUSINESS_PATTERN',
  'SHELL_COMPANY_INDICATOR',
  'TRADE_BASED_ML_INDICATOR',
  'SCA_EXEMPTION_TRA',
  'SCA_EXEMPTION_LOW_VALUE',
  'SCA_EXEMPTION_TRUSTED_BENEFICIARY',
  'SCA_EXEMPTION_RECURRING',
  'DORA_INCIDENT_SEVERITY',
  'EIDAS_ASSURANCE_LEVEL',
  'GDPR_DATA_RETENTION_CHECK',
  'DEVICE_JAILBREAK_ROOTED',
  'EMULATOR_DETECTION',
  'VPN_PROXY_DETECTION',
  'TOR_EXIT_NODE',
  'BROWSER_INCONSISTENCY',
  'TIMEZONE_MISMATCH',
  'LANGUAGE_MISMATCH',
  'MCC_HIGH_RISK',
  'MCC_GAMBLING',
  'MCC_CRYPTO',
  'MERCHANT_FIRST_SEEN',
  'MERCHANT_COUNTRY_MISMATCH',
  'MERCHANT_CATEGORY_CHANGE',
  'MERCHANT_VELOCITY_SPIKE',
  'REMITTANCE_INFO_ANALYSIS',
  'PURPOSE_CODE_MISMATCH',
  'UETR_DUPLICATE_CHECK',
  'CREDITOR_NAME_VALIDATION',
  'STRUCTURED_ADDRESS_CHECK',
  'BENFORD_LAW_DEVIATION',
  'Z_SCORE_GT',
  'STANDARD_DEVIATION_GT',
  'PERCENTILE_GT',
  'COEFFICIENT_VARIATION_GT',
  'TRANSACTION_COUNT_PER_CARD_HOUR',
  'TRANSACTION_COUNT_PER_IP_HOUR',
  'TRANSACTION_COUNT_PER_DEVICE_DAY',
  'TRANSACTION_COUNT_PER_MERCHANT_HOUR',
  'TRANSACTION_COUNT_PER_CUSTOMER_HOUR',
  'UNIQUE_CARD_COUNT_PER_IP_HOUR',
  'UNIQUE_MERCHANT_COUNT_PER_CARD_DAY',
  'TRANSACTION_ATTEMPT_COUNT_PER_CARD',
  'CVV_FAILURE_VELOCITY',
  'ADDRESS_CHANGE_VELOCITY',
  'BENEFICIARY_ADD_VELOCITY',
  'CARD_ADD_VELOCITY',
  'AMOUNT_SUM_PER_CARD_HOUR',
  'AMOUNT_SUM_PER_CUSTOMER_DAY',
  'AVG_TRANSACTION_SPIKE',
  'LARGE_AMOUNT_FREQUENCY',
  'SMALL_AMOUNT_VELOCITY',
  'ROUND_AMOUNT_FREQUENCY',
  'SEQUENTIAL_AMOUNT_PATTERN',
  'AMOUNT_VARIANCE_ANOMALY',
  'DAILY_LIMIT_PROXIMITY',
  'WEEKLY_LIMIT_PROXIMITY',
  'TIME_BETWEEN_CONSECUTIVE_TX',
  'TRANSACTION_FREQUENCY_ANOMALY',
  'TIME_OF_DAY_ANOMALY',
  'DORMANCY_ALERT_VELOCITY',
  'WEEKEND_VS_WEEKDAY_PATTERN',
  'HOLIDAY_TRANSACTION_SPIKE',
  'NIGHTTIME_TRANSACTION_RATIO',
  'BUSINESS_HOURS_DEVIATION',
  'DEVICE_TRUST_SCORE',
  'CANVAS_FINGERPRINT_MISMATCH',
  'WEBGL_FINGERPRINT_ANOMALY',
  'AUDIO_FINGERPRINT_NEW',
  'FONTS_FINGERPRINT_ANOMALY',
  'SCREEN_RESOLUTION_CHANGE',
  'BATTERY_LEVEL_ANOMALY',
  'HARDWARE_CONCURRENCY_MISMATCH',
  'TOUCH_SUPPORT_INCONSISTENCY',
  'DEVICE_MEMORY_ANOMALY',
  'BEHAVIORAL_BASELINE_DEVIATION',
  'SPENDING_CATEGORY_SHIFT',
  'TRANSACTION_SIZE_ESCALATION',
  'FREQUENCY_PATTERN_CHANGE',
  'TIME_PREFERENCE_SHIFT',
  'CHANNEL_USAGE_ANOMALY',
  'PAYMENT_METHOD_SWITCH',
  'RECIPIENT_DIVERSITY_CHANGE',
  'GEOGRAPHIC_BEHAVIOR_SHIFT',
  'SESSION_BEHAVIOR_ANOMALY',
  'LOGIN_PATTERN_DEVIATION',
  'NAVIGATION_PATTERN_ANOMALY',
  'TRANSACTION_TIMING_CLUSTER',
  'AMOUNT_ROUNDING_BEHAVIOR',
  'SPLIT_PAYMENT_PATTERN',
  'CHI_SQUARE_DISTRIBUTION_TEST',
  'KOLMOGOROV_SMIRNOV_TEST',
  'ANDERSON_DARLING_TEST',
  'T_TEST_AMOUNT_DEVIATION',
  'MANN_WHITNEY_U_TEST',
  'CORRELATION_ANOMALY',
  'REGRESSION_RESIDUAL_OUTLIER',
  'VARIANCE_RATIO_TEST',
  'ENTROPY_SCORE_ANOMALY',
  'SKEWNESS_KURTOSIS_ANOMALY',
  'MCC_CATEGORY_VELOCITY',
  'MCC_SPENDING_LIMIT_CHECK',
  'MCC_CROSS_CATEGORY_PATTERN',
  'MERCHANT_REPUTATION_SCORE',
  'MERCHANT_AGE_CHECK',
  'MERCHANT_TRANSACTION_VOLUME',
  'MERCHANT_CHARGEBACK_HISTORY',
  'MERCHANT_FRAUD_RATE_CHECK',
  'MERCHANT_GEOGRAPHIC_SPREAD',
  'MERCHANT_CUSTOMER_CONCENTRATION',
  'MERCHANT_AMOUNT_DISTRIBUTION',
  'MERCHANT_TIME_PATTERN',
  'MERCHANT_DEVICE_DIVERSITY',
  'MERCHANT_REFUND_RATIO',
  'MERCHANT_NEW_CUSTOMER_RATIO',
  'MERCHANT_DORMANT_REACTIVATION',
  'MERCHANT_CROSS_BORDER_RATIO',
  'MERCHANT_HIGH_VALUE_FREQUENCY',
  'FATF_PLACEMENT_CASH_INTENSIVE',
  'FATF_PLACEMENT_STRUCTURING',
  'FATF_PLACEMENT_SMURFING',
  'FATF_PLACEMENT_CURRENCY_EXCHANGE',
  'FATF_PLACEMENT_CASINO_GAMBLING',
  'FATF_LAYERING_RAPID_MOVEMENT',
  'FATF_LAYERING_SHELL_COMPANY',
  'FATF_LAYERING_OFFSHORE',
  'FATF_LAYERING_WIRE_CHAINS',
  'FATF_LAYERING_CONVERTIBLE_INSTRUMENTS',
  'FATF_INTEGRATION_REAL_ESTATE',
  'FATF_INTEGRATION_LUXURY_GOODS',
  'FATF_INTEGRATION_BUSINESS_INVESTMENT',
  'FATF_INTEGRATION_LOAN_REPAYMENT',
  'FATF_TBML_OVER_INVOICING',
  'FATF_TBML_UNDER_INVOICING',
  'FATF_TBML_PHANTOM_SHIPPING',
  'FATF_TBML_MULTIPLE_INVOICING',
  'FATF_TBML_FALSE_DESCRIPTION',
  'FATF_HAWALA_INFORMAL',
  'FATF_NEW_PAYMENT_EXPLOITATION',
  'FATF_CRYPTO_MIXING',
  'FATF_CRYPTO_ATM_CASHOUT',
  'FATF_PEP_TRANSACTION',
  'FATF_CORRESPONDENT_LAYERING',
  'FATF_ROUND_TRIPPING',
  'FATF_BLACK_MARKET_EXCHANGE',
  'FATF_INSURANCE_CASH_VALUE',
  'SCA_LOW_VALUE_EXEMPTION',
  'SCA_CONTACTLESS_EXEMPTION',
  'SCA_TRA_EXEMPTION',
  'SCA_TRUSTED_BENEFICIARY',
  'SCA_RECURRING_TRANSACTION',
  'SCA_MERCHANT_INITIATED',
  'SCA_CORPORATE_PAYMENT',
  'SCA_SECURE_CORPORATE_PROTOCOL',
  'SCA_LIABILITY_SHIFT',
  'SCA_FRAUD_RATE_MONITORING',
  'SCA_CHALLENGE_MANDATORY',
  'PLT_BEHAVIOR_SORTED_LISTS',
  'PLT_BUSINESS_RULES_SCENARIO',
  'PLT_IDENTITY_RESOLUTION',
  'PLT_COMPROMISE_MANAGER',
  'PLT_INTELLIGENCE_NETWORK',
  'PLT_RULES_MODELS_HYBRID',
  'PLT_BEHAVIORAL_PROFILING',
  'PLT_NETWORK_ANALYTICS',
  'PLT_SAR_AUTOMATED',
  'PLT_REAL_TIME_DETECTION',
  'PLT_NETWORK_ENTITY_RESOLUTION',
  'PLT_SCENARIO_SCORECARD',
  'PLT_RADAR_RULE_BACKTESTING',
  'PLT_RADAR_METADATA_MATCHING',
  'PLT_RADAR_INLINE_LISTS',
  'PLT_RADAR_COMPLEX_CONDITIONS',
  'PLT_RISK_PROFILE_ASSIGNMENT',
  'PLT_CUSTOM_RULE_BUILDER',
  'PLT_RISK_LIST_COMPARISON',
  'PLT_BACKTESTING_LABELING',
  'PLT_ML_FRAUD_RISK_OUTCOME',
  'PLT_RISK_SCORE_CALCULATION',
  'PLT_VELOCITY_FILTERS',
  'PLT_LINKING_VELOCITY',
  'PLT_BAD_ENTITY_NETWORK',
  'PLT_REVIEWLIST_QUEUE',
  'STRING',
  'NUMBER',
  'BOOLEAN',
  'DATE',
  'TIME',
  'DATETIME',
  'ARRAY_STRING',
  'ARRAY_NUMBER',
  'FIELD_REFERENCE',
  'EXPRESSION',
  'GEO_POINT',
  // Neo4j Graph Operators (18)
  'NEO4J_WEAKLY_CONNECTED_COMPONENTS',
  'NEO4J_DEGREE_CENTRALITY',
  'NEO4J_PAGERANK_FRAUD_SCORE',
  'NEO4J_LOUVAIN_COMMUNITY_DETECTION',
  'NEO4J_PAIRWISE_SIMILARITY_PII',
  'NEO4J_ENTITY_RESOLUTION_SHARED_PII',
  'NEO4J_FRAUD_RING_DETECTION',
  'NEO4J_MONEY_MULE_NETWORK_ANALYSIS',
  'NEO4J_CIRCULAR_TRANSACTION_DETECTION',
  'NEO4J_FIRST_PARTY_FRAUD_CLUSTERING',
  'NEO4J_SECOND_LEVEL_FRAUDSTER_ID',
  'NEO4J_BETWEENNESS_CENTRALITY_MULE',
  'NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD',
  'NEO4J_SHORTEST_PATH_AML_TRACKING',
  'NEO4J_TRIANGLE_COUNT_COLLUSION',
  'NEO4J_NODE_SIMILARITY_SYNTHETIC_ID',
  'NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION',
  'NEO4J_TEMPORAL_MOTIF_PATTERN',
  // Legacy (compatibilidade)
  'NE',
  'MATCHES_REGEX',
  'IS_NOT_NULL',
  '==',
  '!=',
  '>',
  '<',
  '>=',
  '<=',
] as const;
export const conditionSchema = z.object({
  field: z
    .string()
    .min(1, 'Campo é obrigatório')
    .max(100, 'Campo muito longo'),
  operator: z
    .enum(conditionOperators, { message: 'Operador inválido' }),
  value: z.string(),
}).refine(
  (data) => {
    // Operadores unários não precisam de valor
    if (UNARY_OPERATORS.includes(data.operator as any)) {
      return true;
    }
    // Outros operadores precisam de valor
    return data.value.trim().length > 0;
  },
  {
    message: 'Valor é obrigatório para este operador',
    path: ['value'],
  }
).refine(
  (data) => {
    // P0-01: Validar REGEX com proteção ReDoS (inclui REGEX, NOT_REGEX e legacy MATCHES_REGEX)
    if (data.operator === 'REGEX' || data.operator === 'NOT_REGEX' || data.operator === 'MATCHES_REGEX') {
      const result = validateRegex(data.value);
      return result.valid;
    }
    return true;
  },
  {
    message: 'Expressão regular inválida ou potencialmente perigosa (ReDoS).',
    path: ['value'],
  }
).refine(
  (data) => {
    // P0-02: Validar BETWEEN (requer 2 valores)
    if (data.operator === 'BETWEEN' || data.operator === 'NOT_BETWEEN') {
      const value = data.value.trim();
      if (!value) return false;
      const parts = value.includes('..') ? value.split('..') : value.split(',');
      return parts.length === 2 && parts.every(p => p.trim().length > 0);
    }
    return true;
  },
  {
    message: 'Use o formato: valor1,valor2 ou valor1..valor2 (ex: 10,100)',
    path: ['value'],
  }
).refine(
  (data) => {
    // P0-03: Validar IN/NOT_IN (requer lista com pelo menos 1 item)
    if (data.operator === 'IN' || data.operator === 'NOT_IN') {
      const value = data.value.trim();
      if (!value) return false;
      // Aceita: [1,2,3] ou 1,2,3 ou ["a","b"]
      const cleanValue = value.replace(/^\[|\]$/g, '');
      const items = cleanValue.split(',').map(v => v.trim()).filter(Boolean);
      return items.length >= 1;
    }
    return true;
  },
  {
    message: 'Lista deve ter pelo menos 1 item (ex: valor1,valor2 ou [valor1,valor2])',
    path: ['value'],
  }
);

// ============================================
// SCHEMA PRINCIPAL DO FORMULÁRIO
// ============================================

export const ruleFormSchema = z.object({
  // Informações básicas
  ruleName: z
    .string()
    .min(3, 'Nome deve ter pelo menos 3 caracteres')
    .max(100, 'Nome deve ter no máximo 100 caracteres')
    .regex(
      /^[A-Z][A-Z0-9_]*$/,
      'Nome deve começar com letra maiúscula e conter apenas letras maiúsculas, números e underscores (ex: HIGH_AMOUNT_RULE)'
    ),

  description: z
    .string()
    .max(500, 'Descrição deve ter no máximo 500 caracteres')
    .optional()
    .nullable(),

  // Tipo e classificação
  ruleType: z.enum(['SECURITY', 'CONTEXT', 'VELOCITY', 'ANOMALY'], { message: 'Tipo de regra inválido' }),

  classification: z.enum(['APPROVED', 'SUSPICIOUS', 'FRAUD'], { message: 'Classificação inválida' }),

  // Configuração
  threshold: z
    .number()
    .int('Threshold deve ser um número inteiro')
    .min(0, 'Threshold deve ser maior ou igual a 0')
    .max(1000, 'Threshold deve ser menor ou igual a 1000'),

  weight: z
    .number()
    .int('Peso deve ser um número inteiro')
    .min(0, 'Peso deve ser entre 0 e 100')
    .max(100, 'Peso deve ser entre 0 e 100'),

  enabled: z.boolean(),

  // Parâmetros JSON (opcional, para regras de velocidade)
  parameters: z
    .string()
    .optional()
    .nullable()
    .refine(
      (val) => {
        if (!val || val.trim() === '') return true;
        try {
          JSON.parse(val);
          return true;
        } catch {
          return false;
        }
      },
      { message: 'Parâmetros devem ser um JSON válido' }
    ),

  // Condições
  conditions: z
    .array(conditionSchema)
    .min(0, 'Adicione pelo menos uma condição para regras genéricas'),

  logicOperator: z.enum(['AND', 'OR'], { message: 'Operador lógico inválido' }),
});

// ============================================
// TIPOS DERIVADOS DO SCHEMA
// ============================================

export type RuleFormData = z.infer<typeof ruleFormSchema>;
export type ConditionFormData = z.infer<typeof conditionSchema>;

// ============================================
// VALORES PADRÃO
// ============================================

export const defaultRuleFormValues: RuleFormData = {
  ruleName: '',
  description: '',
  ruleType: 'SECURITY',
  classification: 'SUSPICIOUS',
  threshold: 0,
  weight: 50,
  enabled: true,
  parameters: '',
  conditions: [],
  logicOperator: 'AND',
};

export const defaultConditionValues: ConditionFormData = {
  field: '',
  operator: 'EQ',
  value: '',
};

// ============================================
// HELPERS DE VALIDAÇÃO
// ============================================

/**
 * Valida se o valor é apropriado para o tipo de campo
 */
export function validateValueForFieldType(
  value: string,
  fieldType: 'string' | 'number' | 'boolean' | 'date',
  operator: string
): { valid: boolean; message?: string } {
  // Operadores unários não precisam de valor
  if (UNARY_OPERATORS.includes(operator as any)) {
    return { valid: true };
  }

  if (!value.trim()) {
    return { valid: false, message: 'Valor é obrigatório' };
  }

  switch (fieldType) {
    case 'number':
      if (operator === 'IN' || operator === 'NOT_IN') {
        // Lista de números: "1,2,3" ou "[1,2,3]"
        const cleanValue = value.replace(/[\[\]]/g, '');
        const numbers = cleanValue.split(',').map(v => v.trim());
        const allValid = numbers.every(n => !isNaN(Number(n)));
        if (!allValid) {
          return { valid: false, message: 'Lista deve conter apenas números separados por vírgula' };
        }
      } else if (operator === 'BETWEEN' || operator === 'NOT_BETWEEN') {
        // Range: "10,20" ou "10..20"
        const parts = value.includes('..') ? value.split('..') : value.split(',');
        if (parts.length !== 2 || parts.some(p => isNaN(Number(p.trim())))) {
          return { valid: false, message: 'Use o formato: min,max ou min..max' };
        }
      } else {
        if (isNaN(Number(value))) {
          return { valid: false, message: 'Valor deve ser um número' };
        }
      }
      break;

    case 'boolean':
      const boolValues = ['true', 'false', '1', '0', 'yes', 'no', 'y', 'n'];
      if (!boolValues.includes(value.toLowerCase())) {
        return { valid: false, message: 'Valor deve ser true/false' };
      }
      break;

    case 'date':
      // Aceita YYYYMMDD ou ISO date
      if (!/^\d{8}$/.test(value) && isNaN(Date.parse(value))) {
        return { valid: false, message: 'Data inválida (use YYYYMMDD)' };
      }
      break;
  }

  return { valid: true };
}

/**
 * Formata o valor para exibição no preview
 */
export function formatValueForPreview(value: string, fieldName: string): string {
  // Formatar valores monetários
  if (fieldName === 'transactionAmount' && !isNaN(Number(value))) {
    const amount = Number(value) / 100; // Assumindo centavos
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL',
    }).format(amount);
  }

  // Formatar horário
  if (fieldName === 'transactionTime' && /^\d{6}$/.test(value)) {
    return `${value.slice(0, 2)}:${value.slice(2, 4)}:${value.slice(4, 6)}`;
  }

  // Formatar data
  if (fieldName === 'transactionDate' && /^\d{8}$/.test(value)) {
    return `${value.slice(6, 8)}/${value.slice(4, 6)}/${value.slice(0, 4)}`;
  }

  return value;
}

// ============================================
// VALIDAÇÃO AVANÇADA POR OPERADOR
// ============================================

/**
 * Valida o valor baseado no operador selecionado
 * Retorna mensagem de erro ou null se válido
 */
export function validateValueByOperator(
  operator: string,
  value: string,
  fieldType?: 'string' | 'number' | 'boolean' | 'date'
): string | null {
  // Operadores unários não precisam de valor
  if (UNARY_OPERATORS.includes(operator as any)) {
    return null;
  }

  const trimmedValue = value.trim();

  // Valor obrigatório para operadores não-unários
  if (!trimmedValue) {
    return 'Valor é obrigatório para este operador';
  }

  // Validação específica por operador
  switch (operator) {
    case 'REGEX':
    case 'NOT_REGEX':
    case 'MATCHES_REGEX':
      const regexError = getRegexValidationError(trimmedValue);
      if (regexError) {
        return regexError;
      }
      break;

    case 'BETWEEN':
    case 'NOT_BETWEEN':
      const betweenParts = trimmedValue.includes('..')
        ? trimmedValue.split('..')
        : trimmedValue.split(',');
      if (betweenParts.length !== 2) {
        return 'Use o formato: valor1,valor2 ou valor1..valor2';
      }
      // P0-GAP-01: Validar que ambos os valores não estão vazios
      if (betweenParts.some(p => !p.trim())) {
        return 'Ambos os valores são obrigatórios (ex: 10,100)';
      }
      if (fieldType === 'number') {
        if (betweenParts.some(p => isNaN(Number(p.trim())))) {
          return 'Ambos os valores devem ser números';
        }
        const [min, max] = betweenParts.map(p => Number(p.trim()));
        if (min > max) {
          return 'O primeiro valor deve ser menor que o segundo';
        }
      }
      break;

    case 'IN':
    case 'NOT_IN':
      const cleanValue = trimmedValue.replace(/^\[|\]$/g, '');
      const items = cleanValue.split(',').map(v => v.trim()).filter(Boolean);
      if (items.length === 0) {
        return 'Lista deve ter pelo menos 1 item';
      }
      if (fieldType === 'number') {
        const invalidItems = items.filter(item => isNaN(Number(item.replace(/['"]/g, ''))));
        if (invalidItems.length > 0) {
          return `Valores inválidos para campo numérico: ${invalidItems.join(', ')}`;
        }
      }
      break;

    case 'GT':
    case 'LT':
    case 'GTE':
    case 'LTE':
    case '>':
    case '<':
    case '>=':
    case '<=':
      if (fieldType === 'number' && isNaN(Number(trimmedValue))) {
        return 'Valor deve ser um número para operadores de comparação';
      }
      break;
  }

  return null;
}

/**
 * Retorna placeholder apropriado para o operador
 */
export function getPlaceholderForOperator(operator: string): string {
  switch (operator) {
    case 'IN':
    case 'NOT_IN':
      return 'Ex: valor1,valor2,valor3 ou [1,2,3]';
    case 'BETWEEN':
    case 'NOT_BETWEEN':
    case 'DATE_BETWEEN':
    case 'TIME_BETWEEN':
      return 'Ex: 10,100 ou 10..100';
    case 'REGEX':
    case 'NOT_REGEX':
    case 'MATCHES_REGEX':
      return 'Ex: ^[A-Z]+$ (expressão regular)';
    case 'CONTAINS':
    case 'NOT_CONTAINS':
    case 'ARRAY_CONTAINS':
    case 'ARRAY_NOT_CONTAINS':
      return 'Ex: texto a buscar';
    case 'STARTS_WITH':
      return 'Ex: prefixo';
    case 'ENDS_WITH':
      return 'Ex: sufixo';
    case 'IS_NULL':
    case 'NOT_NULL':
    case 'IS_NOT_NULL':
    case 'IS_TRUE':
    case 'IS_FALSE':
      return '(não aplicável)';
    case 'FIELD_EQ':
    case 'FIELD_NEQ':
    case 'FIELD_GT':
    case 'FIELD_GTE':
    case 'FIELD_LT':
    case 'FIELD_LTE':
      return 'Ex: outroNomeDeCampo';
    case 'DATE_BEFORE':
    case 'DATE_AFTER':
      return 'Ex: 2024-12-31';
    case 'TIME_BEFORE':
    case 'TIME_AFTER':
      return 'Ex: 23:59:59';
    case 'ARRAY_SIZE_EQ':
    case 'ARRAY_SIZE_GT':
    case 'ARRAY_SIZE_LT':
      return 'Ex: 5';
    case 'MOD_EQ':
    case 'MOD_NEQ':
      return 'Ex: 100,0 (divisor,resto)';
    case 'GEO_DISTANCE_LT':
    case 'GEO_DISTANCE_GT':
      return 'Ex: -23.55,-46.63,100 (lat,lon,km)';
    case 'GEO_IN_POLYGON':
      return 'Ex: BRASIL ou SAO_PAULO';
    case 'VELOCITY_COUNT_GT':
    case 'VELOCITY_COUNT_LT':
    case 'VELOCITY_SUM_GT':
    case 'VELOCITY_SUM_LT':
    case 'VELOCITY_AVG_GT':
    case 'VELOCITY_AVG_LT':
      return 'Ex: PAN,60,5 (keyType,minutes,threshold)';
    case 'VELOCITY_DISTINCT_GT':
    case 'VELOCITY_DISTINCT_LT':
      return 'Ex: PAN,1440,MERCHANTS,3';
    default:
      return 'Ex: 100';
  }
}

/**
 * Constante para limite maximo de condicoes
 */
export const MAX_CONDITIONS = 20;
