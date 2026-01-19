/**
 * Tipos para o ComplexRuleBuilder
 * Alinhados com o backend ComplexRuleDTO, ConditionGroupDTO, ConditionDTO
 */

// ============================================
// OPERADORES LÓGICOS
// ============================================

export type LogicOperator = 'AND' | 'OR' | 'NOT' | 'XOR' | 'NAND' | 'NOR';

export const LOGIC_OPERATORS: { value: LogicOperator; label: string; description: string; color: string }[] = [
  { value: 'AND', label: 'E', description: 'Todas as condições devem ser verdadeiras', color: 'bg-blue-500' },
  { value: 'OR', label: 'OU', description: 'Pelo menos uma condição deve ser verdadeira', color: 'bg-green-500' },
  { value: 'NOT', label: 'NÃO', description: 'Inverte o resultado do grupo', color: 'bg-red-500' },
  { value: 'XOR', label: 'XOR', description: 'Exatamente uma condição deve ser verdadeira', color: 'bg-purple-500' },
  { value: 'NAND', label: 'NAND', description: 'Pelo menos uma condição deve ser falsa', color: 'bg-orange-500' },
  { value: 'NOR', label: 'NOR', description: 'Todas as condições devem ser falsas', color: 'bg-pink-500' },
];

// ============================================
// OPERADORES DE COMPARAÇÃO
// ============================================

// ComparisonOperator - Todos os 489+ operadores suportados
// Usando string para permitir extensibilidade com os 489 operadores do backend
export type ComparisonOperator = string;

export interface OperatorInfo {
  value: ComparisonOperator;
  label: string;
  description: string;
  category: 'basic' | 'list' | 'range' | 'string' | 'null' | 'boolean' | 'field' | 'date' | 'array' | 'math' | 'geo' | 'velocity' | 'advanced' | 'graph' | 'regulatory' | 'platform';
  requiresValue: boolean;
  requiresSecondValue?: boolean;
  requiresFieldRef?: boolean;
  applicableTypes: ValueType[];
}

export const COMPARISON_OPERATORS: OperatorInfo[] = [
  // Básicos
  { value: 'EQ', label: '=', description: 'Igual a', category: 'basic', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE', 'ARRAY_STRING', 'ARRAY_NUMBER', 'GEO_POINT', 'GEO_POLYGON'] },
  { value: 'NEQ', label: '≠', description: 'Diferente de', category: 'basic', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE', 'ARRAY_STRING', 'ARRAY_NUMBER', 'GEO_POINT', 'GEO_POLYGON'] },
  { value: 'GT', label: '>', description: 'Maior que', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'GTE', label: '≥', description: 'Maior ou igual a', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'LT', label: '<', description: 'Menor que', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'LTE', label: '≤', description: 'Menor ou igual a', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE'] },

  // Listas
  { value: 'IN', label: 'está em', description: 'Valor está na lista', category: 'list', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'EXPRESSION', 'FIELD_REFERENCE', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'NOT_IN', label: 'não está em', description: 'Valor não está na lista', category: 'list', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'EXPRESSION', 'FIELD_REFERENCE', 'DATE', 'TIME', 'DATETIME'] },

  // Range
  { value: 'BETWEEN', label: 'entre', description: 'Valor está entre dois valores', category: 'range', requiresValue: true, requiresSecondValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'NOT_BETWEEN', label: 'não entre', description: 'Valor não está entre dois valores', category: 'range', requiresValue: true, requiresSecondValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME', 'EXPRESSION', 'FIELD_REFERENCE'] },

  // Strings
  { value: 'CONTAINS', label: 'contém', description: 'Texto contém', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'NOT_CONTAINS', label: 'não contém', description: 'Texto não contém', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'STARTS_WITH', label: 'começa com', description: 'Texto começa com', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'ENDS_WITH', label: 'termina com', description: 'Texto termina com', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'REGEX', label: 'regex', description: 'Corresponde à expressão regular', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'NOT_REGEX', label: 'não regex', description: 'Não corresponde à expressão regular', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },
  { value: 'MATCHES_REGEX', label: 'regex (legacy)', description: 'Corresponde à regex (use REGEX)', category: 'string', requiresValue: true, applicableTypes: ['STRING', 'EXPRESSION', 'FIELD_REFERENCE'] },

  // Nulos
  { value: 'IS_NULL', label: 'é nulo', description: 'Campo é nulo', category: 'null', requiresValue: false, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'ARRAY_STRING', 'ARRAY_NUMBER', 'EXPRESSION', 'FIELD_REFERENCE', 'GEO_POINT', 'GEO_POLYGON'] },
  { value: 'NOT_NULL', label: 'não é nulo', description: 'Campo não é nulo', category: 'null', requiresValue: false, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'ARRAY_STRING', 'ARRAY_NUMBER', 'EXPRESSION', 'FIELD_REFERENCE', 'GEO_POINT', 'GEO_POLYGON'] },
  { value: 'IS_NOT_NULL', label: 'não é nulo (legacy)', description: 'Campo não é nulo (use NOT_NULL)', category: 'null', requiresValue: false, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'ARRAY_STRING', 'ARRAY_NUMBER', 'EXPRESSION', 'FIELD_REFERENCE', 'GEO_POINT', 'GEO_POLYGON'] },

  // Booleanos
  { value: 'IS_TRUE', label: 'é verdadeiro', description: 'Campo é verdadeiro', category: 'boolean', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'IS_FALSE', label: 'é falso', description: 'Campo é falso', category: 'boolean', requiresValue: false, applicableTypes: ['BOOLEAN'] },

  // Comparação entre campos
  { value: 'FIELD_EQ', label: '= campo', description: 'Igual a outro campo', category: 'field', requiresValue: false, requiresFieldRef: true, applicableTypes: ['STRING', 'NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'FIELD_NEQ', label: '≠ campo', description: 'Diferente de outro campo', category: 'field', requiresValue: false, requiresFieldRef: true, applicableTypes: ['STRING', 'NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'FIELD_GT', label: '> campo', description: 'Maior que outro campo', category: 'field', requiresValue: false, requiresFieldRef: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'FIELD_GTE', label: '≥ campo', description: 'Maior ou igual a outro campo', category: 'field', requiresValue: false, requiresFieldRef: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'FIELD_LT', label: '< campo', description: 'Menor que outro campo', category: 'field', requiresValue: false, requiresFieldRef: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'FIELD_LTE', label: '≤ campo', description: 'Menor ou igual a outro campo', category: 'field', requiresValue: false, requiresFieldRef: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },

  // Data/Hora
  { value: 'DATE_BEFORE', label: 'data antes de', description: 'Data anterior a', category: 'date', requiresValue: true, applicableTypes: ['DATE', 'DATETIME'] },
  { value: 'DATE_AFTER', label: 'data após', description: 'Data posterior a', category: 'date', requiresValue: true, applicableTypes: ['DATE', 'DATETIME'] },
  { value: 'DATE_BETWEEN', label: 'data entre', description: 'Data entre duas datas', category: 'date', requiresValue: true, requiresSecondValue: true, applicableTypes: ['DATE', 'DATETIME'] },
  { value: 'TIME_BEFORE', label: 'hora antes de', description: 'Hora anterior a', category: 'date', requiresValue: true, applicableTypes: ['TIME', 'DATETIME'] },
  { value: 'TIME_AFTER', label: 'hora após', description: 'Hora posterior a', category: 'date', requiresValue: true, applicableTypes: ['TIME', 'DATETIME'] },
  { value: 'TIME_BETWEEN', label: 'hora entre', description: 'Hora entre dois horários', category: 'date', requiresValue: true, requiresSecondValue: true, applicableTypes: ['TIME', 'DATETIME'] },

  // Arrays
  { value: 'ARRAY_CONTAINS', label: 'array contém', description: 'Array contém o valor', category: 'array', requiresValue: true, applicableTypes: ['ARRAY_STRING', 'ARRAY_NUMBER'] },
  { value: 'ARRAY_NOT_CONTAINS', label: 'array não contém', description: 'Array não contém o valor', category: 'array', requiresValue: true, applicableTypes: ['ARRAY_STRING', 'ARRAY_NUMBER'] },
  { value: 'ARRAY_SIZE_EQ', label: 'tamanho =', description: 'Tamanho do array igual a', category: 'array', requiresValue: true, applicableTypes: ['ARRAY_STRING', 'ARRAY_NUMBER'] },
  { value: 'ARRAY_SIZE_GT', label: 'tamanho >', description: 'Tamanho do array maior que', category: 'array', requiresValue: true, applicableTypes: ['ARRAY_STRING', 'ARRAY_NUMBER'] },
  { value: 'ARRAY_SIZE_LT', label: 'tamanho <', description: 'Tamanho do array menor que', category: 'array', requiresValue: true, applicableTypes: ['ARRAY_STRING', 'ARRAY_NUMBER'] },

  // Matemáticos
  { value: 'MOD_EQ', label: 'módulo =', description: 'Resto da divisão igual a', category: 'math', requiresValue: true, requiresSecondValue: true, applicableTypes: ['NUMBER'] },
  { value: 'MOD_NEQ', label: 'módulo ≠', description: 'Resto da divisão diferente de', category: 'math', requiresValue: true, requiresSecondValue: true, applicableTypes: ['NUMBER'] },

  // Geolocalização
  { value: 'GEO_DISTANCE_LT', label: 'distância <', description: 'Distância menor que (km). Formato: lat,lon,distKm', category: 'geo', requiresValue: true, applicableTypes: ['STRING', 'GEO_POINT'] },
  { value: 'GEO_DISTANCE_GT', label: 'distância >', description: 'Distância maior que (km). Formato: lat,lon,distKm', category: 'geo', requiresValue: true, applicableTypes: ['STRING', 'GEO_POINT'] },
  { value: 'GEO_IN_POLYGON', label: 'no polígono', description: 'Dentro do polígono. Valor: nome do polígono', category: 'geo', requiresValue: true, applicableTypes: ['STRING', 'GEO_POINT', 'GEO_POLYGON'] },

  // Velocity (agregações temporais)
  { value: 'VELOCITY_COUNT_GT', label: 'contagem >', description: 'Contagem de transações maior que. Formato: keyType,windowMinutes,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_COUNT_LT', label: 'contagem <', description: 'Contagem de transações menor que. Formato: keyType,windowMinutes,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_SUM_GT', label: 'soma >', description: 'Soma de valores maior que. Formato: keyType,windowMinutes,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_SUM_LT', label: 'soma <', description: 'Soma de valores menor que. Formato: keyType,windowMinutes,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_AVG_GT', label: 'média >', description: 'Média de valores maior que. Formato: keyType,windowMinutes,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_AVG_LT', label: 'média <', description: 'Média de valores menor que. Formato: keyType,windowMinutes,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_DISTINCT_GT', label: 'distintos >', description: 'Valores distintos maior que. Formato: keyType,windowMinutes,distinctType,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_DISTINCT_LT', label: 'distintos <', description: 'Valores distintos menor que. Formato: keyType,windowMinutes,distinctType,threshold', category: 'velocity', requiresValue: true, applicableTypes: ['NUMBER'] },

  // ============================================
  // OPERADORES AVANÇADOS (GERADOS AUTOMATICAMENTE)
  // ============================================
  { value: 'ACCOUNT_AGE_LT_MINUTES', label: 'Account age lt minutes', description: 'Operador ACCOUNT_AGE_LT_MINUTES', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ACCOUNT_LINK_DEPTH', label: 'Account link depth', description: 'Operador ACCOUNT_LINK_DEPTH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ACCOUNT_TAKEOVER_PATTERN', label: 'Account takeover pattern', description: 'Operador ACCOUNT_TAKEOVER_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ADAPTIVE_BEHAVIORAL_ANALYTICS', label: 'Adaptive behavioral analytics', description: 'Operador ADAPTIVE_BEHAVIORAL_ANALYTICS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ADAPTIVE_PARAMETRIC_THRESHOLD', label: 'Adaptive parametric threshold', description: 'Operador ADAPTIVE_PARAMETRIC_THRESHOLD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ADDRESS_CHANGE_VELOCITY', label: 'Address change velocity', description: 'Operador ADDRESS_CHANGE_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ADDRESS_VERIFICATION', label: 'Address verification', description: 'Operador ADDRESS_VERIFICATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ADVERSE_MEDIA_CHECK', label: 'Adverse media check', description: 'Operador ADVERSE_MEDIA_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ALIAS_DETECTION', label: 'Alias detection', description: 'Operador ALIAS_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AMOUNT_DEVIATION_FROM_AVG', label: 'Amount deviation from avg', description: 'Operador AMOUNT_DEVIATION_FROM_AVG', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AMOUNT_ROUNDING_BEHAVIOR', label: 'Amount rounding behavior', description: 'Operador AMOUNT_ROUNDING_BEHAVIOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AMOUNT_SPIKE', label: 'Amount spike', description: 'Operador AMOUNT_SPIKE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AMOUNT_SUM_PER_CARD_HOUR', label: 'Amount sum per card hour', description: 'Operador AMOUNT_SUM_PER_CARD_HOUR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AMOUNT_SUM_PER_CUSTOMER_DAY', label: 'Amount sum per customer day', description: 'Operador AMOUNT_SUM_PER_CUSTOMER_DAY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AMOUNT_VARIANCE_ANOMALY', label: 'Amount variance anomaly', description: 'Operador AMOUNT_VARIANCE_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ANDERSON_DARLING_TEST', label: 'Anderson darling test', description: 'Operador ANDERSON_DARLING_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ANTI_DETECT_BROWSER_DETECTION', label: 'Anti detect browser detection', description: 'Operador ANTI_DETECT_BROWSER_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'APP_FRAUD_DETECTION', label: 'App fraud detection', description: 'Operador APP_FRAUD_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'APRIORI_ASSOCIATION', label: 'Apriori association', description: 'Operador APRIORI_ASSOCIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AUDIO_FINGERPRINT_NEW', label: 'Audio fingerprint new', description: 'Operador AUDIO_FINGERPRINT_NEW', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AVG_INTERVAL_BETWEEN_TXN', label: 'Avg interval between txn', description: 'Operador AVG_INTERVAL_BETWEEN_TXN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AVG_LAST_N_DAYS', label: 'Avg last n days', description: 'Operador AVG_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'AVG_TRANSACTION_SPIKE', label: 'Avg transaction spike', description: 'Operador AVG_TRANSACTION_SPIKE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BATTERY_LEVEL_ANOMALY', label: 'Battery level anomaly', description: 'Operador BATTERY_LEVEL_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BEHAVIORAL_BASELINE_DEVIATION', label: 'Behavioral baseline deviation', description: 'Operador BEHAVIORAL_BASELINE_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BENEFICIARY_ADD_VELOCITY', label: 'Beneficiary add velocity', description: 'Operador BENEFICIARY_ADD_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BENEFICIARY_CONCENTRATION', label: 'Beneficiary concentration', description: 'Operador BENEFICIARY_CONCENTRATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BENEFICIARY_REUSE_PATTERN', label: 'Beneficiary reuse pattern', description: 'Operador BENEFICIARY_REUSE_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BENFORD_LAW_DEVIATION', label: 'Benford law deviation', description: 'Operador BENFORD_LAW_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BIOMETRIC_KEYSTROKE_DYNAMICS', label: 'Biometric keystroke dynamics', description: 'Operador BIOMETRIC_KEYSTROKE_DYNAMICS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BIOMETRIC_MOUSE_MOVEMENT', label: 'Biometric mouse movement', description: 'Operador BIOMETRIC_MOUSE_MOVEMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BIOMETRIC_SCROLL_VELOCITY', label: 'Biometric scroll velocity', description: 'Operador BIOMETRIC_SCROLL_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BROWSER_INCONSISTENCY', label: 'Browser inconsistency', description: 'Operador BROWSER_INCONSISTENCY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_BUCKET_CLASSIFICATION', label: 'Bsl bucket classification', description: 'Operador BSL_BUCKET_CLASSIFICATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_BUSINESS_INDICATOR', label: 'Bsl business indicator', description: 'Operador BSL_BUSINESS_INDICATOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_BUSINESS_INDICATOR_COMPONENT', label: 'Bsl business indicator component', description: 'Operador BSL_BUSINESS_INDICATOR_COMPONENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_CONTROL_DEFICIENCY', label: 'Bsl control deficiency', description: 'Operador BSL_CONTROL_DEFICIENCY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_INTERNAL_LOSS_MULTIPLIER', label: 'Bsl internal loss multiplier', description: 'Operador BSL_INTERNAL_LOSS_MULTIPLIER', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_KRI_MONITORING', label: 'Bsl kri monitoring', description: 'Operador BSL_KRI_MONITORING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_LOSS_DATA_COLLECTION', label: 'Bsl loss data collection', description: 'Operador BSL_LOSS_DATA_COLLECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_LOSS_EVENT_REPORTING', label: 'Bsl loss event reporting', description: 'Operador BSL_LOSS_EVENT_REPORTING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_LOSS_EXCLUSION_APPROVAL', label: 'Bsl loss exclusion approval', description: 'Operador BSL_LOSS_EXCLUSION_APPROVAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_LOSS_THRESHOLD_SETTING', label: 'Bsl loss threshold setting', description: 'Operador BSL_LOSS_THRESHOLD_SETTING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_MARGINAL_COEFFICIENT', label: 'Bsl marginal coefficient', description: 'Operador BSL_MARGINAL_COEFFICIENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_RETENTION_PERIOD', label: 'Bsl retention period', description: 'Operador BSL_RETENTION_PERIOD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_RISK_GOVERNANCE', label: 'Bsl risk governance', description: 'Operador BSL_RISK_GOVERNANCE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BSL_SCENARIO_ANALYSIS', label: 'Bsl scenario analysis', description: 'Operador BSL_SCENARIO_ANALYSIS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BUSINESS_HOURS_DEVIATION', label: 'Business hours deviation', description: 'Operador BUSINESS_HOURS_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'BUST_OUT_PATTERN_DETECTION', label: 'Bust out pattern detection', description: 'Operador BUST_OUT_PATTERN_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CANVAS_FINGERPRINT_MISMATCH', label: 'Canvas fingerprint mismatch', description: 'Operador CANVAS_FINGERPRINT_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CARD_ADD_VELOCITY', label: 'Card add velocity', description: 'Operador CARD_ADD_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CARD_TESTING_RING_DETECTION', label: 'Card testing ring detection', description: 'Operador CARD_TESTING_RING_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CASH_INTENSIVE_RATIO', label: 'Cash intensive ratio', description: 'Operador CASH_INTENSIVE_RATIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CHANNEL_SWITCH_PATTERN', label: 'Channel switch pattern', description: 'Operador CHANNEL_SWITCH_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CHANNEL_USAGE_ANOMALY', label: 'Channel usage anomaly', description: 'Operador CHANNEL_USAGE_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CHARGEBACK_RATE_GT', label: 'Chargeback rate gt', description: 'Operador CHARGEBACK_RATE_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CHI_SQUARE_DISTRIBUTION_TEST', label: 'Chi square distribution test', description: 'Operador CHI_SQUARE_DISTRIBUTION_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CIRCULAR_PAYMENT_DETECTION', label: 'Circular payment detection', description: 'Operador CIRCULAR_PAYMENT_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CIRCULAR_TRANSFER_DETECTION', label: 'Circular transfer detection', description: 'Operador CIRCULAR_TRANSFER_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COEFFICIENT_VARIATION_GT', label: 'Coefficient variation gt', description: 'Operador COEFFICIENT_VARIATION_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CONSORTIUM_NEGATIVE_FILE_CHECK', label: 'Consortium negative file check', description: 'Operador CONSORTIUM_NEGATIVE_FILE_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CONTAINS_SUSPICIOUS_KEYWORDS', label: 'Contains suspicious keywords', description: 'Operador CONTAINS_SUSPICIOUS_KEYWORDS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CORRELATION_ANOMALY', label: 'Correlation anomaly', description: 'Operador CORRELATION_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CORRESPONDENT_ANOMALY', label: 'Correspondent anomaly', description: 'Operador CORRESPONDENT_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_CRYPTO_TXN_LAST_N_DAYS', label: 'Count crypto txn last n days', description: 'Operador COUNT_CRYPTO_TXN_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_ACCOUNTS', label: 'Count distinct accounts', description: 'Operador COUNT_DISTINCT_ACCOUNTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS', label: 'Count distinct countries last n hours', description: 'Operador COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS', label: 'Count distinct instruments last n days', description: 'Operador COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS', label: 'Count distinct merchants last n days', description: 'Operador COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS', label: 'Count distinct merchants last n hours', description: 'Operador COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_PANS_LAST_N_HOURS', label: 'Count distinct pans last n hours', description: 'Operador COUNT_DISTINCT_PANS_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_PAYERS_LAST_N_DAYS', label: 'Count distinct payers last n days', description: 'Operador COUNT_DISTINCT_PAYERS_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS', label: 'Count distinct user agents last n hours', description: 'Operador COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_FAILURES_LAST_N_HOURS', label: 'Count failures last n hours', description: 'Operador COUNT_FAILURES_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_LAST_N_DAYS', label: 'Count last n days', description: 'Operador COUNT_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_LAST_N_HOURS', label: 'Count last n hours', description: 'Operador COUNT_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_MFA_ABANDONMENTS', label: 'Count mfa abandonments', description: 'Operador COUNT_MFA_ABANDONMENTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_MFA_DENIALS_LAST_N_HOURS', label: 'Count mfa denials last n hours', description: 'Operador COUNT_MFA_DENIALS_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS', label: 'Count unique beneficiaries last n days', description: 'Operador COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'COUNT_UNIQUE_IPS_LAST_N_HOURS', label: 'Count unique ips last n hours', description: 'Operador COUNT_UNIQUE_IPS_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CPF_SSN_VALIDATION', label: 'Cpf ssn validation', description: 'Operador CPF_SSN_VALIDATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CREDITOR_NAME_VALIDATION', label: 'Creditor name validation', description: 'Operador CREDITOR_NAME_VALIDATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CREDIT_FILE_THIN', label: 'Credit file thin', description: 'Operador CREDIT_FILE_THIN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CROSS_BORDER_VELOCITY', label: 'Cross border velocity', description: 'Operador CROSS_BORDER_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CRYPTO_PUMP_DUMP_DETECTION', label: 'Crypto pump dump detection', description: 'Operador CRYPTO_PUMP_DUMP_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'CVV_FAILURE_VELOCITY', label: 'Cvv failure velocity', description: 'Operador CVV_FAILURE_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DAILY_LIMIT_PROXIMITY', label: 'Daily limit proximity', description: 'Operador DAILY_LIMIT_PROXIMITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DAYS_SINCE_LAST_ACTIVITY', label: 'Days since last activity', description: 'Operador DAYS_SINCE_LAST_ACTIVITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DAY_OF_WEEK_IN', label: 'Day of week in', description: 'Operador DAY_OF_WEEK_IN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DECIMAL_PLACES_GT', label: 'Decimal places gt', description: 'Operador DECIMAL_PLACES_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DEVICE_ACCOUNT_RATIO', label: 'Device account ratio', description: 'Operador DEVICE_ACCOUNT_RATIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DEVICE_CHANGED_IN_SESSION', label: 'Device changed in session', description: 'Operador DEVICE_CHANGED_IN_SESSION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DEVICE_FINGERPRINT_CONSISTENCY_CHECK', label: 'Device fingerprint consistency check', description: 'Operador DEVICE_FINGERPRINT_CONSISTENCY_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DEVICE_JAILBREAK_ROOTED', label: 'Device jailbreak rooted', description: 'Operador DEVICE_JAILBREAK_ROOTED', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DEVICE_MEMORY_ANOMALY', label: 'Device memory anomaly', description: 'Operador DEVICE_MEMORY_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DEVICE_TRUST_SCORE', label: 'Device trust score', description: 'Operador DEVICE_TRUST_SCORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DISTANCE_FROM_LAST_GT', label: 'Distance from last gt', description: 'Operador DISTANCE_FROM_LAST_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DOCUMENT_FORGERY_DETECTION', label: 'Document forgery detection', description: 'Operador DOCUMENT_FORGERY_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DOMAIN_IN_LIST', label: 'Domain in list', description: 'Operador DOMAIN_IN_LIST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DORA_INCIDENT_SEVERITY', label: 'Dora incident severity', description: 'Operador DORA_INCIDENT_SEVERITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DORMANCY_ALERT_VELOCITY', label: 'Dormancy alert velocity', description: 'Operador DORMANCY_ALERT_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'DORMANCY_REVIVAL', label: 'Dormancy revival', description: 'Operador DORMANCY_REVIVAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ECBSV_SSN_VALIDATION', label: 'Ecbsv ssn validation', description: 'Operador ECBSV_SSN_VALIDATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ECLAT_ITEMSET', label: 'Eclat itemset', description: 'Operador ECLAT_ITEMSET', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'EIDAS_ASSURANCE_LEVEL', label: 'Eidas assurance level', description: 'Operador EIDAS_ASSURANCE_LEVEL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'EMAIL_DOMAIN_AGE', label: 'Email domain age', description: 'Operador EMAIL_DOMAIN_AGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'EMAIL_PHONE_MISMATCH', label: 'Email phone mismatch', description: 'Operador EMAIL_PHONE_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'EMULATOR_DETECTION', label: 'Emulator detection', description: 'Operador EMULATOR_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ENTROPY_SCORE_ANOMALY', label: 'Entropy score anomaly', description: 'Operador ENTROPY_SCORE_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'EXPIRES_WITHIN_DAYS', label: 'Expires within days', description: 'Operador EXPIRES_WITHIN_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FACE_TO_ID_PHOTO_MATCHING', label: 'Face to id photo matching', description: 'Operador FACE_TO_ID_PHOTO_MATCHING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FAN_IN_COUNT', label: 'Fan in count', description: 'Operador FAN_IN_COUNT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FAN_OUT_COUNT', label: 'Fan out count', description: 'Operador FAN_OUT_COUNT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_BLACK_MARKET_EXCHANGE', label: 'Fatf black market exchange', description: 'Operador FATF_BLACK_MARKET_EXCHANGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_CORRESPONDENT_LAYERING', label: 'Fatf correspondent layering', description: 'Operador FATF_CORRESPONDENT_LAYERING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_CRYPTO_ATM_CASHOUT', label: 'Fatf crypto atm cashout', description: 'Operador FATF_CRYPTO_ATM_CASHOUT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_CRYPTO_MIXING', label: 'Fatf crypto mixing', description: 'Operador FATF_CRYPTO_MIXING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_HAWALA_INFORMAL', label: 'Fatf hawala informal', description: 'Operador FATF_HAWALA_INFORMAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_INSURANCE_CASH_VALUE', label: 'Fatf insurance cash value', description: 'Operador FATF_INSURANCE_CASH_VALUE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_INTEGRATION_BUSINESS_INVESTMENT', label: 'Fatf integration business investment', description: 'Operador FATF_INTEGRATION_BUSINESS_INVESTMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_INTEGRATION_LOAN_REPAYMENT', label: 'Fatf integration loan repayment', description: 'Operador FATF_INTEGRATION_LOAN_REPAYMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_INTEGRATION_LUXURY_GOODS', label: 'Fatf integration luxury goods', description: 'Operador FATF_INTEGRATION_LUXURY_GOODS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_INTEGRATION_REAL_ESTATE', label: 'Fatf integration real estate', description: 'Operador FATF_INTEGRATION_REAL_ESTATE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_LAYERING_CONVERTIBLE_INSTRUMENTS', label: 'Fatf layering convertible instruments', description: 'Operador FATF_LAYERING_CONVERTIBLE_INSTRUMENTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_LAYERING_OFFSHORE', label: 'Fatf layering offshore', description: 'Operador FATF_LAYERING_OFFSHORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_LAYERING_RAPID_MOVEMENT', label: 'Fatf layering rapid movement', description: 'Operador FATF_LAYERING_RAPID_MOVEMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_LAYERING_SHELL_COMPANY', label: 'Fatf layering shell company', description: 'Operador FATF_LAYERING_SHELL_COMPANY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_LAYERING_WIRE_CHAINS', label: 'Fatf layering wire chains', description: 'Operador FATF_LAYERING_WIRE_CHAINS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_NEW_PAYMENT_EXPLOITATION', label: 'Fatf new payment exploitation', description: 'Operador FATF_NEW_PAYMENT_EXPLOITATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_PEP_TRANSACTION', label: 'Fatf pep transaction', description: 'Operador FATF_PEP_TRANSACTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_PLACEMENT_CASH_INTENSIVE', label: 'Fatf placement cash intensive', description: 'Operador FATF_PLACEMENT_CASH_INTENSIVE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_PLACEMENT_CASINO_GAMBLING', label: 'Fatf placement casino gambling', description: 'Operador FATF_PLACEMENT_CASINO_GAMBLING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_PLACEMENT_CURRENCY_EXCHANGE', label: 'Fatf placement currency exchange', description: 'Operador FATF_PLACEMENT_CURRENCY_EXCHANGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_PLACEMENT_SMURFING', label: 'Fatf placement smurfing', description: 'Operador FATF_PLACEMENT_SMURFING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_PLACEMENT_STRUCTURING', label: 'Fatf placement structuring', description: 'Operador FATF_PLACEMENT_STRUCTURING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_ROUND_TRIPPING', label: 'Fatf round tripping', description: 'Operador FATF_ROUND_TRIPPING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_TBML_FALSE_DESCRIPTION', label: 'Fatf tbml false description', description: 'Operador FATF_TBML_FALSE_DESCRIPTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_TBML_MULTIPLE_INVOICING', label: 'Fatf tbml multiple invoicing', description: 'Operador FATF_TBML_MULTIPLE_INVOICING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_TBML_OVER_INVOICING', label: 'Fatf tbml over invoicing', description: 'Operador FATF_TBML_OVER_INVOICING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_TBML_PHANTOM_SHIPPING', label: 'Fatf tbml phantom shipping', description: 'Operador FATF_TBML_PHANTOM_SHIPPING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FATF_TBML_UNDER_INVOICING', label: 'Fatf tbml under invoicing', description: 'Operador FATF_TBML_UNDER_INVOICING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FONTS_FINGERPRINT_ANOMALY', label: 'Fonts fingerprint anomaly', description: 'Operador FONTS_FINGERPRINT_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FPGROWTH_FREQUENT_PATTERNS', label: 'Fpgrowth frequent patterns', description: 'Operador FPGROWTH_FREQUENT_PATTERNS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FREQUENCY_PATTERN_CHANGE', label: 'Frequency pattern change', description: 'Operador FREQUENCY_PATTERN_CHANGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FUZZY_ADAPTIVE_THRESHOLD', label: 'Fuzzy adaptive threshold', description: 'Operador FUZZY_ADAPTIVE_THRESHOLD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'FUZZY_MEMBERSHIP', label: 'Fuzzy membership', description: 'Operador FUZZY_MEMBERSHIP', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'GDPR_DATA_RETENTION_CHECK', label: 'Gdpr data retention check', description: 'Operador GDPR_DATA_RETENTION_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'GEOGRAPHIC_BEHAVIOR_SHIFT', label: 'Geographic behavior shift', description: 'Operador GEOGRAPHIC_BEHAVIOR_SHIFT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'GTE_PERCENT_OF_LAST_INCOMING', label: 'Gte percent of last incoming', description: 'Operador GTE_PERCENT_OF_LAST_INCOMING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'GT_CURRENT_DATE', label: 'Gt current date', description: 'Operador GT_CURRENT_DATE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'GT_FIELD_MULTIPLIER', label: 'Gt field multiplier', description: 'Operador GT_FIELD_MULTIPLIER', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HARDWARE_CONCURRENCY_MISMATCH', label: 'Hardware concurrency mismatch', description: 'Operador HARDWARE_CONCURRENCY_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HAS_FAILED_3DS_LAST_N_MINUTES', label: 'Has failed 3ds last n minutes', description: 'Operador HAS_FAILED_3DS_LAST_N_MINUTES', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HAS_INCOMING_TRANSFER_LAST_N_HOURS', label: 'Has incoming transfer last n hours', description: 'Operador HAS_INCOMING_TRANSFER_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HIGH_RISK_CORRIDOR_CHECK', label: 'High risk corridor check', description: 'Operador HIGH_RISK_CORRIDOR_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HIGH_RISK_JURISDICTION', label: 'High risk jurisdiction', description: 'Operador HIGH_RISK_JURISDICTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HOLIDAY_TRANSACTION_SPIKE', label: 'Holiday transaction spike', description: 'Operador HOLIDAY_TRANSACTION_SPIKE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'HOUR_BETWEEN', label: 'Hour between', description: 'Operador HOUR_BETWEEN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IDENTITY_VELOCITY', label: 'Identity velocity', description: 'Operador IDENTITY_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'INJECTION_ATTACK_DETECTION', label: 'Injection attack detection', description: 'Operador INJECTION_ATTACK_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'INTEGRATION_PATTERN', label: 'Integration pattern', description: 'Operador INTEGRATION_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'INVESTMENT_SCAM_PATTERN', label: 'Investment scam pattern', description: 'Operador INVESTMENT_SCAM_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IN_CUSTOMER_CHARGEBACK_MERCHANTS', label: 'In customer chargeback merchants', description: 'Operador IN_CUSTOMER_CHARGEBACK_MERCHANTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IN_CUSTOMER_HISTORY', label: 'In customer history', description: 'Operador IN_CUSTOMER_HISTORY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IN_CUSTOMER_USUAL_HOURS', label: 'In customer usual hours', description: 'Operador IN_CUSTOMER_USUAL_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IN_LIST', label: 'In list', description: 'Operador IN_LIST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_CRYPTO_RANSOM_AMOUNT', label: 'Is crypto ransom amount', description: 'Operador IS_CRYPTO_RANSOM_AMOUNT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_FIRST', label: 'Is first', description: 'Operador IS_FIRST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_HOLIDAY', label: 'Is holiday', description: 'Operador IS_HOLIDAY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_IMPOSSIBLE_COMBINATION', label: 'Is impossible combination', description: 'Operador IS_IMPOSSIBLE_COMBINATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_NEW', label: 'Is new', description: 'Operador IS_NEW', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_VOIP', label: 'Is voip', description: 'Operador IS_VOIP', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'IS_WEEKEND', label: 'Is weekend', description: 'Operador IS_WEEKEND', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'KOLMOGOROV_SMIRNOV_TEST', label: 'Kolmogorov smirnov test', description: 'Operador KOLMOGOROV_SMIRNOV_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LANGUAGE_MISMATCH', label: 'Language mismatch', description: 'Operador LANGUAGE_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LARGE_AMOUNT_FREQUENCY', label: 'Large amount frequency', description: 'Operador LARGE_AMOUNT_FREQUENCY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LAYERED_TRANSFER_PATTERN', label: 'Layered transfer pattern', description: 'Operador LAYERED_TRANSFER_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LAYERING_PATTERN', label: 'Layering pattern', description: 'Operador LAYERING_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LIVENESS_DETECTION_FACIAL', label: 'Liveness detection facial', description: 'Operador LIVENESS_DETECTION_FACIAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LIVENESS_DETECTION_VOICE', label: 'Liveness detection voice', description: 'Operador LIVENESS_DETECTION_VOICE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_ADVERSARIAL_ATTACK_RESISTANCE', label: 'Llm adversarial attack resistance', description: 'Operador LLM_ADVERSARIAL_ATTACK_RESISTANCE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_ANOMALY_EXPLANATION_GENERATION', label: 'Llm anomaly explanation generation', description: 'Operador LLM_ANOMALY_EXPLANATION_GENERATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_CHATBOT_FRAUD_DETECTION', label: 'Llm chatbot fraud detection', description: 'Operador LLM_CHATBOT_FRAUD_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_DEEPFAKE_VOICE_DETECTION', label: 'Llm deepfake voice detection', description: 'Operador LLM_DEEPFAKE_VOICE_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_EMAIL_PHISHING_ANALYSIS', label: 'Llm email phishing analysis', description: 'Operador LLM_EMAIL_PHISHING_ANALYSIS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_FRAUD_ALERT_PRIORITIZATION', label: 'Llm fraud alert prioritization', description: 'Operador LLM_FRAUD_ALERT_PRIORITIZATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_FRAUD_PATTERN_AUTODISCOVERY', label: 'Llm fraud pattern autodiscovery', description: 'Operador LLM_FRAUD_PATTERN_AUTODISCOVERY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_GENERATIVE_RULE_SYNTHESIS', label: 'Llm generative rule synthesis', description: 'Operador LLM_GENERATIVE_RULE_SYNTHESIS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_MULTI_MODAL_FRAUD_DETECTION', label: 'Llm multi modal fraud detection', description: 'Operador LLM_MULTI_MODAL_FRAUD_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_SOCIAL_ENGINEERING_CLASSIFICATION', label: 'Llm social engineering classification', description: 'Operador LLM_SOCIAL_ENGINEERING_CLASSIFICATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_SYNTHETIC_IMAGE_DETECTION', label: 'Llm synthetic image detection', description: 'Operador LLM_SYNTHETIC_IMAGE_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LLM_TRANSACTION_DESCRIPTION_ANALYSIS', label: 'Llm transaction description analysis', description: 'Operador LLM_TRANSACTION_DESCRIPTION_ANALYSIS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LOCATION_DEVIATION', label: 'Location deviation', description: 'Operador LOCATION_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LOGIN_PATTERN_DEVIATION', label: 'Login pattern deviation', description: 'Operador LOGIN_PATTERN_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'LT_CURRENT_DATE', label: 'Lt current date', description: 'Operador LT_CURRENT_DATE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MANN_WHITNEY_U_TEST', label: 'Mann whitney u test', description: 'Operador MANN_WHITNEY_U_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MAX_AMOUNT_LAST_N_DAYS', label: 'Max amount last n days', description: 'Operador MAX_AMOUNT_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MCC_CATEGORY_VELOCITY', label: 'Mcc category velocity', description: 'Operador MCC_CATEGORY_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MCC_CROSS_CATEGORY_PATTERN', label: 'Mcc cross category pattern', description: 'Operador MCC_CROSS_CATEGORY_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MCC_CRYPTO', label: 'Mcc crypto', description: 'Operador MCC_CRYPTO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MCC_GAMBLING', label: 'Mcc gambling', description: 'Operador MCC_GAMBLING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MCC_HIGH_RISK', label: 'Mcc high risk', description: 'Operador MCC_HIGH_RISK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MCC_SPENDING_LIMIT_CHECK', label: 'Mcc spending limit check', description: 'Operador MCC_SPENDING_LIMIT_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_AGE_CHECK', label: 'Merchant age check', description: 'Operador MERCHANT_AGE_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_AMOUNT_DISTRIBUTION', label: 'Merchant amount distribution', description: 'Operador MERCHANT_AMOUNT_DISTRIBUTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_CATEGORY_CHANGE', label: 'Merchant category change', description: 'Operador MERCHANT_CATEGORY_CHANGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_CHARGEBACK_HISTORY', label: 'Merchant chargeback history', description: 'Operador MERCHANT_CHARGEBACK_HISTORY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_COUNTRY_MISMATCH', label: 'Merchant country mismatch', description: 'Operador MERCHANT_COUNTRY_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_CROSS_BORDER_RATIO', label: 'Merchant cross border ratio', description: 'Operador MERCHANT_CROSS_BORDER_RATIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_CUSTOMER_CONCENTRATION', label: 'Merchant customer concentration', description: 'Operador MERCHANT_CUSTOMER_CONCENTRATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_DEVIATION', label: 'Merchant deviation', description: 'Operador MERCHANT_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_DEVICE_DIVERSITY', label: 'Merchant device diversity', description: 'Operador MERCHANT_DEVICE_DIVERSITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_DORMANT_REACTIVATION', label: 'Merchant dormant reactivation', description: 'Operador MERCHANT_DORMANT_REACTIVATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_FIRST_SEEN', label: 'Merchant first seen', description: 'Operador MERCHANT_FIRST_SEEN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_FRAUD_RATE_CHECK', label: 'Merchant fraud rate check', description: 'Operador MERCHANT_FRAUD_RATE_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_GEOGRAPHIC_SPREAD', label: 'Merchant geographic spread', description: 'Operador MERCHANT_GEOGRAPHIC_SPREAD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_HIGH_VALUE_FREQUENCY', label: 'Merchant high value frequency', description: 'Operador MERCHANT_HIGH_VALUE_FREQUENCY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_NEW_CUSTOMER_RATIO', label: 'Merchant new customer ratio', description: 'Operador MERCHANT_NEW_CUSTOMER_RATIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_REFUND_RATIO', label: 'Merchant refund ratio', description: 'Operador MERCHANT_REFUND_RATIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_REPUTATION_SCORE', label: 'Merchant reputation score', description: 'Operador MERCHANT_REPUTATION_SCORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_TIME_PATTERN', label: 'Merchant time pattern', description: 'Operador MERCHANT_TIME_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_TRANSACTION_VOLUME', label: 'Merchant transaction volume', description: 'Operador MERCHANT_TRANSACTION_VOLUME', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MERCHANT_VELOCITY_SPIKE', label: 'Merchant velocity spike', description: 'Operador MERCHANT_VELOCITY_SPIKE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MICRO_DEPOSIT_VELOCITY', label: 'Micro deposit velocity', description: 'Operador MICRO_DEPOSIT_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MICRO_TRANSACTION_TEST', label: 'Micro transaction test', description: 'Operador MICRO_TRANSACTION_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MIN_AMOUNT_LAST_N_DAYS', label: 'Min amount last n days', description: 'Operador MIN_AMOUNT_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'MULTI_LAYERED_SYNTHETIC_ID_CONTROLS', label: 'Multi layered synthetic id controls', description: 'Operador MULTI_LAYERED_SYNTHETIC_ID_CONTROLS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NAME_SIMILARITY_LT', label: 'Name similarity lt', description: 'Operador NAME_SIMILARITY_LT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NAME_TRANSLITERATION_MATCH', label: 'Name transliteration match', description: 'Operador NAME_TRANSLITERATION_MATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NAVIGATION_PATTERN_ANOMALY', label: 'Navigation pattern anomaly', description: 'Operador NAVIGATION_PATTERN_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_BETWEENNESS_CENTRALITY_MULE', label: 'Neo4j betweenness centrality mule', description: 'Operador NEO4J_BETWEENNESS_CENTRALITY_MULE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_CIRCULAR_TRANSACTION_DETECTION', label: 'Neo4j circular transaction detection', description: 'Operador NEO4J_CIRCULAR_TRANSACTION_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_DEGREE_CENTRALITY', label: 'Neo4j degree centrality', description: 'Operador NEO4J_DEGREE_CENTRALITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_ENTITY_RESOLUTION_SHARED_PII', label: 'Neo4j entity resolution shared pii', description: 'Operador NEO4J_ENTITY_RESOLUTION_SHARED_PII', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_FIRST_PARTY_FRAUD_CLUSTERING', label: 'Neo4j first party fraud clustering', description: 'Operador NEO4J_FIRST_PARTY_FRAUD_CLUSTERING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_FRAUD_RING_DETECTION', label: 'Neo4j fraud ring detection', description: 'Operador NEO4J_FRAUD_RING_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION', label: 'Neo4j graph embedding fraud prediction', description: 'Operador NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD', label: 'Neo4j label propagation fraud spread', description: 'Operador NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_LOUVAIN_COMMUNITY_DETECTION', label: 'Neo4j louvain community detection', description: 'Operador NEO4J_LOUVAIN_COMMUNITY_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_MONEY_MULE_NETWORK_ANALYSIS', label: 'Neo4j money mule network analysis', description: 'Operador NEO4J_MONEY_MULE_NETWORK_ANALYSIS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_NODE_SIMILARITY_SYNTHETIC_ID', label: 'Neo4j node similarity synthetic id', description: 'Operador NEO4J_NODE_SIMILARITY_SYNTHETIC_ID', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_PAGERANK_FRAUD_SCORE', label: 'Neo4j pagerank fraud score', description: 'Operador NEO4J_PAGERANK_FRAUD_SCORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_PAIRWISE_SIMILARITY_PII', label: 'Neo4j pairwise similarity pii', description: 'Operador NEO4J_PAIRWISE_SIMILARITY_PII', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_SECOND_LEVEL_FRAUDSTER_ID', label: 'Neo4j second level fraudster id', description: 'Operador NEO4J_SECOND_LEVEL_FRAUDSTER_ID', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_SHORTEST_PATH_AML_TRACKING', label: 'Neo4j shortest path aml tracking', description: 'Operador NEO4J_SHORTEST_PATH_AML_TRACKING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_TEMPORAL_MOTIF_PATTERN', label: 'Neo4j temporal motif pattern', description: 'Operador NEO4J_TEMPORAL_MOTIF_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_TRIANGLE_COUNT_COLLUSION', label: 'Neo4j triangle count collusion', description: 'Operador NEO4J_TRIANGLE_COUNT_COLLUSION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NEO4J_WEAKLY_CONNECTED_COMPONENTS', label: 'Neo4j weakly connected components', description: 'Operador NEO4J_WEAKLY_CONNECTED_COMPONENTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NESTED_CORRESPONDENT_CHECK', label: 'Nested correspondent check', description: 'Operador NESTED_CORRESPONDENT_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NIGHTTIME_TRANSACTION_RATIO', label: 'Nighttime transaction ratio', description: 'Operador NIGHTTIME_TRANSACTION_RATIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NOT_IN_CUSTOMER_HISTORY', label: 'Not in customer history', description: 'Operador NOT_IN_CUSTOMER_HISTORY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NOT_IN_CUSTOMER_USUAL_HOURS', label: 'Not in customer usual hours', description: 'Operador NOT_IN_CUSTOMER_USUAL_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'NOT_IN_HISTORICAL', label: 'Not in historical', description: 'Operador NOT_IN_HISTORICAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'OFAC_LIST_CHECK', label: 'Ofac list check', description: 'Operador OFAC_LIST_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'OUTFLOW_RATE_LAST_N_DAYS', label: 'Outflow rate last n days', description: 'Operador OUTFLOW_RATE_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PACS008_FIELD_VALIDATION', label: 'Pacs008 field validation', description: 'Operador PACS008_FIELD_VALIDATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PATTERN_ESCALATION', label: 'Pattern escalation', description: 'Operador PATTERN_ESCALATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PATTERN_ROUND_NUMBERS', label: 'Pattern round numbers', description: 'Operador PATTERN_ROUND_NUMBERS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PATTERN_SPLIT_TRANSACTION', label: 'Pattern split transaction', description: 'Operador PATTERN_SPLIT_TRANSACTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PAYMENT_METHOD_SWITCH', label: 'Payment method switch', description: 'Operador PAYMENT_METHOD_SWITCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PEER_GROUP_DEVIATION_SCORE', label: 'Peer group deviation score', description: 'Operador PEER_GROUP_DEVIATION_SCORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PEP_LIST_CHECK', label: 'Pep list check', description: 'Operador PEP_LIST_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PERCENTAGE_OF_FIELD', label: 'Percentage of field', description: 'Operador PERCENTAGE_OF_FIELD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PERCENTILE_GT', label: 'Percentile gt', description: 'Operador PERCENTILE_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PHONE_CARRIER_CHECK', label: 'Phone carrier check', description: 'Operador PHONE_CARRIER_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PIX_KEY_CHANGED_LAST_N_DAYS', label: 'Pix key changed last n days', description: 'Operador PIX_KEY_CHANGED_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_BACKTESTING_LABELING', label: 'Plt backtesting labeling', description: 'Operador PLT_BACKTESTING_LABELING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_BAD_ENTITY_NETWORK', label: 'Plt bad entity network', description: 'Operador PLT_BAD_ENTITY_NETWORK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_BEHAVIORAL_PROFILING', label: 'Plt behavioral profiling', description: 'Operador PLT_BEHAVIORAL_PROFILING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_BEHAVIOR_SORTED_LISTS', label: 'Plt behavior sorted lists', description: 'Operador PLT_BEHAVIOR_SORTED_LISTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_BUSINESS_RULES_SCENARIO', label: 'Plt business rules scenario', description: 'Operador PLT_BUSINESS_RULES_SCENARIO', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_COMPROMISE_MANAGER', label: 'Plt compromise manager', description: 'Operador PLT_COMPROMISE_MANAGER', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_CONSORTIUM_DATA_CHECK', label: 'Plt consortium data check', description: 'Operador PLT_CONSORTIUM_DATA_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_CUSTOM_RULE_BUILDER', label: 'Plt custom rule builder', description: 'Operador PLT_CUSTOM_RULE_BUILDER', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_DS2_RULE_ENGINE', label: 'Plt ds2 rule engine', description: 'Operador PLT_DS2_RULE_ENGINE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_IDENTITY_RESOLUTION', label: 'Plt identity resolution', description: 'Operador PLT_IDENTITY_RESOLUTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_INTELLIGENCE_NETWORK', label: 'Plt intelligence network', description: 'Operador PLT_INTELLIGENCE_NETWORK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_LINKING_VELOCITY', label: 'Plt linking velocity', description: 'Operador PLT_LINKING_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_ML_FRAUD_RISK_OUTCOME', label: 'Plt ml fraud risk outcome', description: 'Operador PLT_ML_FRAUD_RISK_OUTCOME', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_NETWORK_ANALYTICS', label: 'Plt network analytics', description: 'Operador PLT_NETWORK_ANALYTICS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_NETWORK_ENTITY_RESOLUTION', label: 'Plt network entity resolution', description: 'Operador PLT_NETWORK_ENTITY_RESOLUTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RADAR_COMPLEX_CONDITIONS', label: 'Plt radar complex conditions', description: 'Operador PLT_RADAR_COMPLEX_CONDITIONS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RADAR_INLINE_LISTS', label: 'Plt radar inline lists', description: 'Operador PLT_RADAR_INLINE_LISTS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RADAR_METADATA_MATCHING', label: 'Plt radar metadata matching', description: 'Operador PLT_RADAR_METADATA_MATCHING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RADAR_RULE_BACKTESTING', label: 'Plt radar rule backtesting', description: 'Operador PLT_RADAR_RULE_BACKTESTING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_REAL_TIME_DETECTION', label: 'Plt real time detection', description: 'Operador PLT_REAL_TIME_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_REVIEWLIST_QUEUE', label: 'Plt reviewlist queue', description: 'Operador PLT_REVIEWLIST_QUEUE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RISK_LIST_COMPARISON', label: 'Plt risk list comparison', description: 'Operador PLT_RISK_LIST_COMPARISON', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RISK_PROFILE_ASSIGNMENT', label: 'Plt risk profile assignment', description: 'Operador PLT_RISK_PROFILE_ASSIGNMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RISK_SCORE_CALCULATION', label: 'Plt risk score calculation', description: 'Operador PLT_RISK_SCORE_CALCULATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_RULES_MODELS_HYBRID', label: 'Plt rules models hybrid', description: 'Operador PLT_RULES_MODELS_HYBRID', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_SAR_AUTOMATED', label: 'Plt sar automated', description: 'Operador PLT_SAR_AUTOMATED', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_SCENARIO_SCORECARD', label: 'Plt scenario scorecard', description: 'Operador PLT_SCENARIO_SCORECARD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PLT_VELOCITY_FILTERS', label: 'Plt velocity filters', description: 'Operador PLT_VELOCITY_FILTERS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PSD3_COP_NAME_MATCH', label: 'Psd3 cop name match', description: 'Operador PSD3_COP_NAME_MATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'PURPOSE_CODE_MISMATCH', label: 'Purpose code mismatch', description: 'Operador PURPOSE_CODE_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'RAPID_MOVEMENT', label: 'Rapid movement', description: 'Operador RAPID_MOVEMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'RAPID_MULTI_HOP', label: 'Rapid multi hop', description: 'Operador RAPID_MULTI_HOP', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'RAPID_SUCCESSION_PATTERN', label: 'Rapid succession pattern', description: 'Operador RAPID_SUCCESSION_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'REAL_TIME_RISK_SCORING', label: 'Real time risk scoring', description: 'Operador REAL_TIME_RISK_SCORING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'RECIPIENT_DIVERSITY_CHANGE', label: 'Recipient diversity change', description: 'Operador RECIPIENT_DIVERSITY_CHANGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'REGRESSION_RESIDUAL_OUTLIER', label: 'Regression residual outlier', description: 'Operador REGRESSION_RESIDUAL_OUTLIER', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'REMITTANCE_INFO_ANALYSIS', label: 'Remittance info analysis', description: 'Operador REMITTANCE_INFO_ANALYSIS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ROMANCE_SCAM_INDICATOR', label: 'Romance scam indicator', description: 'Operador ROMANCE_SCAM_INDICATOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ROUND_AMOUNT_FREQUENCY', label: 'Round amount frequency', description: 'Operador ROUND_AMOUNT_FREQUENCY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'ROUND_TRIP_DETECTION', label: 'Round trip detection', description: 'Operador ROUND_TRIP_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SANCTIONS_COUNTRY_CHECK', label: 'Sanctions country check', description: 'Operador SANCTIONS_COUNTRY_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_CHALLENGE_MANDATORY', label: 'Sca challenge mandatory', description: 'Operador SCA_CHALLENGE_MANDATORY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_CONTACTLESS_EXEMPTION', label: 'Sca contactless exemption', description: 'Operador SCA_CONTACTLESS_EXEMPTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_CORPORATE_PAYMENT', label: 'Sca corporate payment', description: 'Operador SCA_CORPORATE_PAYMENT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_DYNAMIC_3DS_ROUTING', label: 'Sca dynamic 3ds routing', description: 'Operador SCA_DYNAMIC_3DS_ROUTING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_EXEMPTION_LOW_VALUE', label: 'Sca exemption low value', description: 'Operador SCA_EXEMPTION_LOW_VALUE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_EXEMPTION_RECURRING', label: 'Sca exemption recurring', description: 'Operador SCA_EXEMPTION_RECURRING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_EXEMPTION_TRA', label: 'Sca exemption tra', description: 'Operador SCA_EXEMPTION_TRA', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_EXEMPTION_TRUSTED_BENEFICIARY', label: 'Sca exemption trusted beneficiary', description: 'Operador SCA_EXEMPTION_TRUSTED_BENEFICIARY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_FRAUD_RATE_MONITORING', label: 'Sca fraud rate monitoring', description: 'Operador SCA_FRAUD_RATE_MONITORING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_LIABILITY_SHIFT', label: 'Sca liability shift', description: 'Operador SCA_LIABILITY_SHIFT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_LOW_VALUE_EXEMPTION', label: 'Sca low value exemption', description: 'Operador SCA_LOW_VALUE_EXEMPTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_MERCHANT_INITIATED', label: 'Sca merchant initiated', description: 'Operador SCA_MERCHANT_INITIATED', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_RECURRING_TRANSACTION', label: 'Sca recurring transaction', description: 'Operador SCA_RECURRING_TRANSACTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_SECURE_CORPORATE_PROTOCOL', label: 'Sca secure corporate protocol', description: 'Operador SCA_SECURE_CORPORATE_PROTOCOL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_TRA_EXEMPTION', label: 'Sca tra exemption', description: 'Operador SCA_TRA_EXEMPTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCA_TRUSTED_BENEFICIARY', label: 'Sca trusted beneficiary', description: 'Operador SCA_TRUSTED_BENEFICIARY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SCREEN_RESOLUTION_CHANGE', label: 'Screen resolution change', description: 'Operador SCREEN_RESOLUTION_CHANGE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SEGMENT_OF_ONE_PROFILING', label: 'Segment of one profiling', description: 'Operador SEGMENT_OF_ONE_PROFILING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SEQUENTIAL_AMOUNT_PATTERN', label: 'Sequential amount pattern', description: 'Operador SEQUENTIAL_AMOUNT_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SESSION_BEHAVIOR_ANOMALY', label: 'Session behavior anomaly', description: 'Operador SESSION_BEHAVIOR_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SHARED_DEVICE_COUNT', label: 'Shared device count', description: 'Operador SHARED_DEVICE_COUNT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SHARED_IP_COUNT', label: 'Shared ip count', description: 'Operador SHARED_IP_COUNT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SHELL_BANK_INDICATOR', label: 'Shell bank indicator', description: 'Operador SHELL_BANK_INDICATOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SHELL_COMPANY_INDICATOR', label: 'Shell company indicator', description: 'Operador SHELL_COMPANY_INDICATOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SKEWNESS_KURTOSIS_ANOMALY', label: 'Skewness kurtosis anomaly', description: 'Operador SKEWNESS_KURTOSIS_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SMALL_AMOUNT_VELOCITY', label: 'Small amount velocity', description: 'Operador SMALL_AMOUNT_VELOCITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SPENDING_CATEGORY_SHIFT', label: 'Spending category shift', description: 'Operador SPENDING_CATEGORY_SHIFT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SPLIT_PAYMENT_PATTERN', label: 'Split payment pattern', description: 'Operador SPLIT_PAYMENT_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SPLIT_TRANSACTION_DETECTION', label: 'Split transaction detection', description: 'Operador SPLIT_TRANSACTION_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STANDARD_DEVIATION_GT', label: 'Standard deviation gt', description: 'Operador STANDARD_DEVIATION_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_ANOVA_F_TEST', label: 'Stat anova f test', description: 'Operador STAT_ANOVA_F_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_BOOTSTRAP_CONFIDENCE_INTERVAL', label: 'Stat bootstrap confidence interval', description: 'Operador STAT_BOOTSTRAP_CONFIDENCE_INTERVAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_DBSCAN_NOISE_DETECTION', label: 'Stat dbscan noise detection', description: 'Operador STAT_DBSCAN_NOISE_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_DIXON_Q_TEST', label: 'Stat dixon q test', description: 'Operador STAT_DIXON_Q_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_GMM_PROBABILITY', label: 'Stat gmm probability', description: 'Operador STAT_GMM_PROBABILITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_GRUBBS_TEST', label: 'Stat grubbs test', description: 'Operador STAT_GRUBBS_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_ISOLATION_FOREST_SCORE', label: 'Stat isolation forest score', description: 'Operador STAT_ISOLATION_FOREST_SCORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_KMEANS_CLUSTER_DISTANCE', label: 'Stat kmeans cluster distance', description: 'Operador STAT_KMEANS_CLUSTER_DISTANCE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_KRUSKAL_WALLIS_TEST', label: 'Stat kruskal wallis test', description: 'Operador STAT_KRUSKAL_WALLIS_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_LEVENE_TEST', label: 'Stat levene test', description: 'Operador STAT_LEVENE_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_LOCAL_OUTLIER_FACTOR', label: 'Stat local outlier factor', description: 'Operador STAT_LOCAL_OUTLIER_FACTOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_MAHALANOBIS_DISTANCE', label: 'Stat mahalanobis distance', description: 'Operador STAT_MAHALANOBIS_DISTANCE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_ONE_CLASS_SVM_BOUNDARY', label: 'Stat one class svm boundary', description: 'Operador STAT_ONE_CLASS_SVM_BOUNDARY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_SHAPIRO_WILK_TEST', label: 'Stat shapiro wilk test', description: 'Operador STAT_SHAPIRO_WILK_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STAT_WELCH_T_TEST', label: 'Stat welch t test', description: 'Operador STAT_WELCH_T_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STRUCTURED_ADDRESS_CHECK', label: 'Structured address check', description: 'Operador STRUCTURED_ADDRESS_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'STRUCTURING_DETECTION', label: 'Structuring detection', description: 'Operador STRUCTURING_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SUM_BY_CHANNEL_LAST_N_DAYS', label: 'Sum by channel last n days', description: 'Operador SUM_BY_CHANNEL_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SUM_LAST_N_DAYS', label: 'Sum last n days', description: 'Operador SUM_LAST_N_DAYS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SUM_LAST_N_HOURS', label: 'Sum last n hours', description: 'Operador SUM_LAST_N_HOURS', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SYNTHETIC_FRAUD_SCORE', label: 'Synthetic fraud score', description: 'Operador SYNTHETIC_FRAUD_SCORE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SYNTHETIC_IDENTITY_RING', label: 'Synthetic identity ring', description: 'Operador SYNTHETIC_IDENTITY_RING', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'SYNTHETIC_ID_LABEL_CORRECTION', label: 'Synthetic id label correction', description: 'Operador SYNTHETIC_ID_LABEL_CORRECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TIMEZONE_MISMATCH', label: 'Timezone mismatch', description: 'Operador TIMEZONE_MISMATCH', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TIME_BETWEEN_CONSECUTIVE_TX', label: 'Time between consecutive tx', description: 'Operador TIME_BETWEEN_CONSECUTIVE_TX', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TIME_DEVIATION_FROM_USUAL', label: 'Time deviation from usual', description: 'Operador TIME_DEVIATION_FROM_USUAL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TIME_OF_DAY_ANOMALY', label: 'Time of day anomaly', description: 'Operador TIME_OF_DAY_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TIME_PREFERENCE_SHIFT', label: 'Time preference shift', description: 'Operador TIME_PREFERENCE_SHIFT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TIME_SINCE_LAST_LT', label: 'Time since last lt', description: 'Operador TIME_SINCE_LAST_LT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TOR_EXIT_NODE', label: 'Tor exit node', description: 'Operador TOR_EXIT_NODE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TOUCH_SUPPORT_INCONSISTENCY', label: 'Touch support inconsistency', description: 'Operador TOUCH_SUPPORT_INCONSISTENCY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRADE_BASED_ML_INDICATOR', label: 'Trade based ml indicator', description: 'Operador TRADE_BASED_ML_INDICATOR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_ATTEMPT_COUNT_PER_CARD', label: 'Transaction attempt count per card', description: 'Operador TRANSACTION_ATTEMPT_COUNT_PER_CARD', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_COUNT_PER_CARD_HOUR', label: 'Transaction count per card hour', description: 'Operador TRANSACTION_COUNT_PER_CARD_HOUR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_COUNT_PER_CUSTOMER_HOUR', label: 'Transaction count per customer hour', description: 'Operador TRANSACTION_COUNT_PER_CUSTOMER_HOUR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_COUNT_PER_DEVICE_DAY', label: 'Transaction count per device day', description: 'Operador TRANSACTION_COUNT_PER_DEVICE_DAY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_COUNT_PER_IP_HOUR', label: 'Transaction count per ip hour', description: 'Operador TRANSACTION_COUNT_PER_IP_HOUR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_COUNT_PER_MERCHANT_HOUR', label: 'Transaction count per merchant hour', description: 'Operador TRANSACTION_COUNT_PER_MERCHANT_HOUR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_FREQUENCY_ANOMALY', label: 'Transaction frequency anomaly', description: 'Operador TRANSACTION_FREQUENCY_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_SIZE_ESCALATION', label: 'Transaction size escalation', description: 'Operador TRANSACTION_SIZE_ESCALATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'TRANSACTION_TIMING_CLUSTER', label: 'Transaction timing cluster', description: 'Operador TRANSACTION_TIMING_CLUSTER', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'T_TEST_AMOUNT_DEVIATION', label: 'T test amount deviation', description: 'Operador T_TEST_AMOUNT_DEVIATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'UETR_DUPLICATE_CHECK', label: 'Uetr duplicate check', description: 'Operador UETR_DUPLICATE_CHECK', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'UNIQUE_CARD_COUNT_PER_IP_HOUR', label: 'Unique card count per ip hour', description: 'Operador UNIQUE_CARD_COUNT_PER_IP_HOUR', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'UNIQUE_MERCHANT_COUNT_PER_CARD_DAY', label: 'Unique merchant count per card day', description: 'Operador UNIQUE_MERCHANT_COUNT_PER_CARD_DAY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'UNUSUAL_BUSINESS_PATTERN', label: 'Unusual business pattern', description: 'Operador UNUSUAL_BUSINESS_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VARIANCE_RATIO_TEST', label: 'Variance ratio test', description: 'Operador VARIANCE_RATIO_TEST', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_ACCELERATION', label: 'Velocity acceleration', description: 'Operador VELOCITY_ACCELERATION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_CROSS_CHANNEL', label: 'Velocity cross channel', description: 'Operador VELOCITY_CROSS_CHANNEL', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_PERCENTILE', label: 'Velocity percentile', description: 'Operador VELOCITY_PERCENTILE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_RATIO_GT', label: 'Velocity ratio gt', description: 'Operador VELOCITY_RATIO_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_ROLLING_WINDOW', label: 'Velocity rolling window', description: 'Operador VELOCITY_ROLLING_WINDOW', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_SPIKE', label: 'Velocity spike', description: 'Operador VELOCITY_SPIKE', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VELOCITY_TREND', label: 'Velocity trend', description: 'Operador VELOCITY_TREND', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'VPN_PROXY_DETECTION', label: 'Vpn proxy detection', description: 'Operador VPN_PROXY_DETECTION', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'WEBGL_FINGERPRINT_ANOMALY', label: 'Webgl fingerprint anomaly', description: 'Operador WEBGL_FINGERPRINT_ANOMALY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'WEEKEND_VS_WEEKDAY_PATTERN', label: 'Weekend vs weekday pattern', description: 'Operador WEEKEND_VS_WEEKDAY_PATTERN', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'WEEKLY_LIMIT_PROXIMITY', label: 'Weekly limit proximity', description: 'Operador WEEKLY_LIMIT_PROXIMITY', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },
  { value: 'Z_SCORE_GT', label: 'Z score gt', description: 'Operador Z_SCORE_GT', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN'] },

  // ============================================
  // OPERADORES DB SYNC (60 novos)
  // ============================================
  { value: 'ACCOUNT_AGE_LT_DAYS', label: 'Account age less than days', description: 'Idade da conta menor que N dias', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'ADDRESS_MISMATCH', label: 'Address mismatch', description: 'Incompatibilidade de endereço', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'AMOUNT_ANOMALY', label: 'Amount anomaly', description: 'Anomalia de valor', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'CAPTCHA_FAILED', label: 'Captcha failed', description: 'Falha no captcha', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'CARD_CAPTURE_FRAUD', label: 'Card capture fraud', description: 'Fraude de captura de cartão', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'CLICK_VELOCITY_GT', label: 'Click velocity greater than', description: 'Velocidade de clique maior que', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'CONTEXT', label: 'Context', description: 'Operador de contexto', category: 'advanced', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'DEVICE_FINGERPRINT_MISMATCH', label: 'Device fingerprint mismatch', description: 'Incompatibilidade de fingerprint', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'ECOMMERCE_NO_AVS', label: 'E-commerce no AVS', description: 'E-commerce sem AVS', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'EMAIL_DOMAIN_AGE_LT_DAYS', label: 'Email domain age less than days', description: 'Idade do domínio do email menor que N dias', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'EMV_SECURITY_CHECK', label: 'EMV security check', description: 'Verificação de segurança EMV', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'EXPIRED_CARD', label: 'Expired card', description: 'Cartão expirado', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'FRAUD', label: 'Fraud', description: 'Indicador de fraude', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'HAS_FAILED_3DS_LAST_N_MINUTES', label: 'Has failed 3DS last N minutes', description: 'Houve falha 3DS nos últimos N minutos', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'IMPOSSIBLE_TRAVEL', label: 'Impossible travel', description: 'Viagem impossível', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'IS_NEW_DEVICE', label: 'Is new device', description: 'É novo dispositivo', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'IS_NEW_LOCATION', label: 'Is new location', description: 'É nova localização', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'MOUSE_MOVEMENT_ANOMALY', label: 'Mouse movement anomaly', description: 'Anomalia de movimento do mouse', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_BETWEENNESS_CENTRALITY_MULE', label: 'Neo4j betweenness centrality mule', description: 'Centralidade de intermediação para mules', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_CIRCULAR_TRANSACTION_DETECTION', label: 'Neo4j circular transaction detection', description: 'Detecção de transação circular', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_DEGREE_CENTRALITY', label: 'Neo4j degree centrality', description: 'Centralidade de grau', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_ENTITY_RESOLUTION_SHARED_PII', label: 'Neo4j entity resolution shared PII', description: 'Resolução de entidade por PII compartilhado', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_FIRST_PARTY_FRAUD_CLUSTERING', label: 'Neo4j first party fraud clustering', description: 'Clustering de fraude de primeira parte', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_FRAUD_RING_DETECTION', label: 'Neo4j fraud ring detection', description: 'Detecção de anel de fraude', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION', label: 'Neo4j graph embedding fraud prediction', description: 'Predição de fraude via embedding de grafo', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD', label: 'Neo4j label propagation fraud spread', description: 'Propagação de label de fraude', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_LOUVAIN_COMMUNITY_DETECTION', label: 'Neo4j Louvain community detection', description: 'Detecção de comunidade Louvain', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_MONEY_MULE_NETWORK_ANALYSIS', label: 'Neo4j money mule network analysis', description: 'Análise de rede de money mules', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_NODE_SIMILARITY_SYNTHETIC_ID', label: 'Neo4j node similarity synthetic ID', description: 'Similaridade de nó para ID sintético', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_PAGERANK_FRAUD_SCORE', label: 'Neo4j PageRank fraud score', description: 'Score de fraude via PageRank', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_PAIRWISE_SIMILARITY_PII', label: 'Neo4j pairwise similarity PII', description: 'Similaridade de PII entre pares', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_SECOND_LEVEL_FRAUDSTER_ID', label: 'Neo4j second level fraudster ID', description: 'Identificação de fraudador de segundo nível', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_SHORTEST_PATH_AML_TRACKING', label: 'Neo4j shortest path AML tracking', description: 'Rastreamento AML via caminho mais curto', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_TEMPORAL_MOTIF_PATTERN', label: 'Neo4j temporal motif pattern', description: 'Padrão de motif temporal', category: 'graph', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'NEO4J_TRIANGLE_COUNT_COLLUSION', label: 'Neo4j triangle count collusion', description: 'Contagem de triângulos para colusão', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NEO4J_WEAKLY_CONNECTED_COMPONENTS', label: 'Neo4j weakly connected components', description: 'Componentes fracamente conectados', category: 'graph', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'NOT_IN_LIST', label: 'Not in list', description: 'Não está na lista', category: 'advanced', requiresValue: true, applicableTypes: ['STRING', 'NUMBER'] },
  { value: 'OFFLINE_PIN_FAILED', label: 'Offline PIN failed', description: 'Falha no PIN offline', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'PACS008_FIELD_VALIDATION', label: 'PACS008 field validation', description: 'Validação campos PACS008', category: 'regulatory', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'PHONE_COUNTRY_MISMATCH', label: 'Phone country mismatch', description: 'Incompatibilidade de país do telefone', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'PIG_BUTCHERING_INDICATOR', label: 'Pig butchering indicator', description: 'Indicador de pig butchering scam', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'PIN_CVV_LIMIT_EXCEEDED', label: 'PIN/CVV limit exceeded', description: 'Limite de PIN/CVV excedido', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'PLT_DS2_RULE_ENGINE', label: 'PLT DS2 rule engine', description: 'SAS DS2 rule engine', category: 'platform', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'POS_SECURITY_MISSING', label: 'POS security missing', description: 'Segurança POS ausente', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'PSD3_COP_NAME_MATCH', label: 'PSD3 CoP name match', description: 'Match de nome CoP PSD3', category: 'regulatory', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'ROUND_AMOUNT', label: 'Round amount', description: 'Valor redondo', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'SCA_DYNAMIC_3DS_ROUTING', label: 'SCA dynamic 3DS routing', description: 'Dynamic 3DS exemption routing', category: 'regulatory', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'SECURITY', label: 'Security', description: 'Verificação de segurança', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'SESSION_DURATION_LT', label: 'Session duration less than', description: 'Duração da sessão menor que', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'SUSPICIOUS', label: 'Suspicious', description: 'Indicador de suspeita', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'SUSPICIOUS_TERMINAL', label: 'Suspicious terminal', description: 'Terminal suspeito', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'TERMINAL_VERIFICATION_FAILED', label: 'Terminal verification failed', description: 'Falha na verificação do terminal', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'TIME_ANOMALY', label: 'Time anomaly', description: 'Anomalia de tempo', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'TRANSFER_AMOUNT_GT', label: 'Transfer amount greater than', description: 'Valor de transferência maior que', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'TRANSFER_VELOCITY_GT', label: 'Transfer velocity greater than', description: 'Velocidade de transferência maior que', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'TYPING_SPEED_ANOMALY', label: 'Typing speed anomaly', description: 'Anomalia de velocidade de digitação', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'UNUSUAL_CARD_MEDIA', label: 'Unusual card media', description: 'Mídia de cartão incomum', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'USER_AGENT_SUSPICIOUS', label: 'User agent suspicious', description: 'User agent suspeito', category: 'advanced', requiresValue: false, applicableTypes: ['BOOLEAN'] },
  { value: 'VELOCITY', label: 'Velocity', description: 'Verificação de velocidade', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
  { value: 'VELOCITY_ANOMALY', label: 'Velocity anomaly', description: 'Anomalia de velocidade', category: 'advanced', requiresValue: true, applicableTypes: ['NUMBER'] },
];

// ============================================
// TIPOS DE VALOR
// ============================================

export type ValueType =
  | 'STRING'
  | 'NUMBER'
  | 'BOOLEAN'
  | 'DATE'
  | 'TIME'
  | 'DATETIME'
  | 'ARRAY_STRING'
  | 'ARRAY_NUMBER'
  | 'FIELD_REFERENCE'
  | 'EXPRESSION'
  | 'GEO_POINT'
  | 'GEO_POLYGON';

export const VALUE_TYPES: { value: ValueType; label: string }[] = [
  { value: 'STRING', label: 'Texto' },
  { value: 'NUMBER', label: 'Número' },
  { value: 'BOOLEAN', label: 'Booleano' },
  { value: 'DATE', label: 'Data' },
  { value: 'TIME', label: 'Hora' },
  { value: 'DATETIME', label: 'Data e Hora' },
  { value: 'ARRAY_STRING', label: 'Lista de Textos' },
  { value: 'ARRAY_NUMBER', label: 'Lista de Números' },
  { value: 'FIELD_REFERENCE', label: 'Referência a Campo' },
  { value: 'EXPRESSION', label: 'Expressão' },
  { value: 'GEO_POINT', label: 'Ponto Geográfico' },
  { value: 'GEO_POLYGON', label: 'Polígono Geográfico' },
];

// ============================================
// CONDIÇÃO
// ============================================

export interface Condition {
  id: string;
  fieldName: string;
  fieldPath?: string;
  operator: ComparisonOperator;
  valueType: ValueType;
  valueSingle?: string;
  valueArray?: string[];
  valueMin?: string;
  valueMax?: string;
  valueFieldRef?: string;
  valueExpression?: string;
  caseSensitive?: boolean;
  negate?: boolean;
  enabled?: boolean;
}

// ============================================
// GRUPO DE CONDIÇÕES
// ============================================

export interface ConditionGroup {
  id: string;
  logicOperator: LogicOperator;
  name?: string;
  description?: string;
  position?: number;
  enabled?: boolean;
  conditions: Condition[];
  children: ConditionGroup[];
}

// ============================================
// STATUS E DECISÃO
// ============================================

export type RuleStatus = 'DRAFT' | 'PUBLISHED' | 'ARCHIVED' | 'TESTING';
export type DecisionType = 'APROVADO' | 'SUSPEITA_DE_FRAUDE' | 'FRAUDE';

export const RULE_STATUSES: { value: RuleStatus; label: string; color: string }[] = [
  { value: 'DRAFT', label: 'Rascunho', color: 'bg-gray-500' },
  { value: 'PUBLISHED', label: 'Publicada', color: 'bg-green-500' },
  { value: 'TESTING', label: 'Em Teste', color: 'bg-yellow-500' },
  { value: 'ARCHIVED', label: 'Arquivada', color: 'bg-red-500' },
];

export const DECISION_TYPES: { value: DecisionType; label: string; color: string }[] = [
  { value: 'APROVADO', label: 'Aprovado', color: 'bg-green-500' },
  { value: 'SUSPEITA_DE_FRAUDE', label: 'Suspeita de Fraude', color: 'bg-yellow-500' },
  { value: 'FRAUDE', label: 'Fraude', color: 'bg-red-500' },
];

// ============================================
// EXPRESSÃO CALCULADA
// ============================================

export interface Expression {
  id: string;
  name: string;
  expression: string;
  resultType: ValueType;
  description?: string;
}

// ============================================
// VARIÁVEL DE CONTEXTO
// ============================================

export interface ContextVariable {
  id: string;
  name: string;
  source: 'REQUEST' | 'SESSION' | 'COMPUTED' | 'EXTERNAL';
  path?: string;
  defaultValue?: string;
  description?: string;
}

// ============================================
// AÇÃO
// ============================================

export type ActionType = 'LOG' | 'ALERT' | 'BLOCK' | 'FLAG' | 'NOTIFY' | 'CUSTOM';

export interface RuleAction {
  id: string;
  type: ActionType;
  config: Record<string, unknown>;
  enabled?: boolean;
}

// ============================================
// REGRA COMPLEXA COMPLETA
// ============================================

export interface ComplexRule {
  id?: string;
  key: string;
  title: string;
  description?: string;
  version?: number;
  status: RuleStatus;
  priority: number;
  severity: number;
  decision: DecisionType;
  reasonTemplate?: string;
  enabled: boolean;
  rootConditionGroup: ConditionGroup;
  expressions?: Expression[];
  contextVariables?: ContextVariable[];
  actions?: RuleAction[];
  tags?: string[];
  fieldsUsed?: string[];
  createdBy?: string;
  createdAt?: string;
  updatedAt?: string;
}

// ============================================
// HELPERS
// ============================================

export function createEmptyCondition(): Condition {
  return {
    id: crypto.randomUUID(),
    fieldName: '',
    operator: 'EQ',
    valueType: 'STRING',
    valueSingle: '',
    enabled: true,
  };
}

export function createEmptyGroup(logicOperator: LogicOperator = 'AND'): ConditionGroup {
  return {
    id: crypto.randomUUID(),
    logicOperator,
    conditions: [],
    children: [],
    enabled: true,
  };
}

export function createEmptyRule(): ComplexRule {
  return {
    key: '',
    title: '',
    description: '',
    status: 'DRAFT',
    priority: 50,
    severity: 50,
    decision: 'SUSPEITA_DE_FRAUDE',
    enabled: true,
    rootConditionGroup: createEmptyGroup('AND'),
  };
}

export function getOperatorInfo(operator: ComparisonOperator): OperatorInfo | undefined {
  return COMPARISON_OPERATORS.find(op => op.value === operator);
}

export function getOperatorsForType(valueType: ValueType): OperatorInfo[] {
  return COMPARISON_OPERATORS.filter(op => op.applicableTypes.includes(valueType));
}
