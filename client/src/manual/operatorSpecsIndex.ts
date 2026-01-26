/**
 * OPERATOR_SPECS - MASTER INDEX
 * 
 * Este arquivo centraliza TODAS as specs de operadores.
 * Importa de todos os arquivos de specs e exporta um único objeto consolidado.
 * 
 * TOTAL: 469 operadores documentados com specs ULTRA DIDÁTICAS
 */

// Import do arquivo original (specs detalhados)
import { OPERATOR_SPECS as ORIGINAL_SPECS } from './operatorSpecs';

// Import Part 1: BASIC_COMPARISON, ARRAY, FIELD, ACCOUNT_AGE, VELOCITY, GEO, DEVICE
import {
  BASIC_COMPARISON_SPECS,
  ARRAY_SPECS,
  FIELD_SPECS,
  ACCOUNT_AGE_SPECS,
  VELOCITY_SPECS,
  GEO_SPECS,
  DEVICE_SPECS
} from './operatorSpecsComplete';

// Import Part 2: NEO4J, FATF, FRAUD_PATTERNS, BIOMETRICS
import {
  NEO4J_SPECS,
  FATF_SPECS,
  FRAUD_PATTERN_SPECS,
  BIOMETRIC_SPECS
} from './operatorSpecsComplete2';

// Import Part 3: STATISTICAL, DATE_TIME, MERCHANT, TRANSACTION, SCA_PSD3
import {
  STATISTICAL_SPECS,
  DATE_TIME_SPECS,
  MERCHANT_SPECS,
  TRANSACTION_SPECS,
  SCA_PSD3_SPECS
} from './operatorSpecsComplete3';

// Import Part 4: BSL_SANCTIONS, ADDRESS_NAME, PLATFORM, STRING_ADVANCED, LIST_CHECKING
import {
  BSL_SANCTIONS_SPECS,
  ADDRESS_NAME_SPECS,
  PLATFORM_SPECS,
  STRING_ADVANCED_SPECS,
  LIST_CHECKING_SPECS
} from './operatorSpecsComplete4';

// Import Part 5-9: Missing operators from backend list
import { MISSING_SPECS_A_E } from './operatorSpecsMissingA';
import { MISSING_SPECS_EMV_MANN } from './operatorSpecsMissingB';
import { MISSING_SPECS_M_N } from './operatorSpecsMissingC';
import { MISSING_SPECS_NOT_S } from './operatorSpecsMissingD';
import { MISSING_SPECS_T_W } from './operatorSpecsMissingE';

// Re-export type
export type { OperatorSpec } from './operatorSpecs';

/**
 * OPERATOR_SPECS_COMPLETE - Todas as specs consolidadas
 * 
 * Ordem das categorias:
 * 1. BASIC_COMPARISON (25) - EQ, NEQ, GT, AND, OR, CONTAINS, etc.
 * 2. ARRAY (10) - Operações em arrays
 * 3. FIELD (8) - Validação de campos
 * 4. ACCOUNT_AGE (6) - Idade e padrões de conta
 * 5. VELOCITY (17) - Contagens e agregações temporais
 * 6. GEO (12) - Geolocalização
 * 7. DEVICE (20) - Fingerprint e segurança de device
 * 8. NEO4J (18) - Grafos e relacionamentos
 * 9. FATF (28) - Tipologias AML do FATF
 * 10. FRAUD_PATTERNS (30) - Padrões de fraude específicos
 * 11. BIOMETRICS (15) - Biometria comportamental e facial
 * 12. STATISTICAL (15) - Análises estatísticas
 * 13. DATE_TIME (15) - Data, hora, timezone
 * 14. MERCHANT (20) - Risco de merchant/MCC
 * 15. TRANSACTION (15) - Padrões transacionais
 * 16. SCA_PSD3 (17) - Strong Customer Authentication
 * 17. BSL_SANCTIONS (20) - Compliance e sanções
 * 18. ADDRESS_NAME (15) - Validação de endereço/nome
 * 19. PLATFORM (15) - Infraestrutura e bots
 * 20. STRING_ADVANCED (12) - Algoritmos de string
 * 21. LIST_CHECKING (10) - White/black/greylist
 * 22. ORIGINAL (15) - Specs originais detalhados
 */
export const OPERATOR_SPECS_COMPLETE = {
  // Part 1
  ...BASIC_COMPARISON_SPECS,
  ...ARRAY_SPECS,
  ...FIELD_SPECS,
  ...ACCOUNT_AGE_SPECS,
  ...VELOCITY_SPECS,
  ...GEO_SPECS,
  ...DEVICE_SPECS,
  
  // Part 2
  ...NEO4J_SPECS,
  ...FATF_SPECS,
  ...FRAUD_PATTERN_SPECS,
  ...BIOMETRIC_SPECS,
  
  // Part 3
  ...STATISTICAL_SPECS,
  ...DATE_TIME_SPECS,
  ...MERCHANT_SPECS,
  ...TRANSACTION_SPECS,
  ...SCA_PSD3_SPECS,
  
  // Part 4
  ...BSL_SANCTIONS_SPECS,
  ...ADDRESS_NAME_SPECS,
  ...PLATFORM_SPECS,
  ...STRING_ADVANCED_SPECS,
  ...LIST_CHECKING_SPECS,

  // Part 5-9 (Missing backend operators)
  ...MISSING_SPECS_A_E,
  ...MISSING_SPECS_EMV_MANN,
  ...MISSING_SPECS_M_N,
  ...MISSING_SPECS_NOT_S,
  ...MISSING_SPECS_T_W,
  
  // Original detailed specs (override any duplicates with more detailed versions)
  ...ORIGINAL_SPECS
};

/**
 * Categorias disponíveis para navegação
 */
export const OPERATOR_CATEGORIES = {
  BASIC_COMPARISON: Object.keys(BASIC_COMPARISON_SPECS),
  ARRAY: Object.keys(ARRAY_SPECS),
  FIELD: Object.keys(FIELD_SPECS),
  ACCOUNT_AGE: Object.keys(ACCOUNT_AGE_SPECS),
  VELOCITY: Object.keys(VELOCITY_SPECS),
  GEO: Object.keys(GEO_SPECS),
  DEVICE: Object.keys(DEVICE_SPECS),
  NEO4J: Object.keys(NEO4J_SPECS),
  FATF_AML: Object.keys(FATF_SPECS),
  FRAUD_PATTERNS: Object.keys(FRAUD_PATTERN_SPECS),
  BIOMETRICS: Object.keys(BIOMETRIC_SPECS),
  STATISTICAL: Object.keys(STATISTICAL_SPECS),
  DATE_TIME: Object.keys(DATE_TIME_SPECS),
  MERCHANT_MCC: Object.keys(MERCHANT_SPECS),
  TRANSACTION: Object.keys(TRANSACTION_SPECS),
  SCA_PSD3: Object.keys(SCA_PSD3_SPECS),
  BSL_SANCTIONS: Object.keys(BSL_SANCTIONS_SPECS),
  ADDRESS_NAME: Object.keys(ADDRESS_NAME_SPECS),
  PLATFORM: Object.keys(PLATFORM_SPECS),
  STRING_ADVANCED: Object.keys(STRING_ADVANCED_SPECS),
  LIST_CHECKING: Object.keys(LIST_CHECKING_SPECS),
  MISSING_A_E: Object.keys(MISSING_SPECS_A_E),
  MISSING_EMV_MANN: Object.keys(MISSING_SPECS_EMV_MANN),
  MISSING_M_N: Object.keys(MISSING_SPECS_M_N),
  MISSING_NOT_S: Object.keys(MISSING_SPECS_NOT_S),
  MISSING_T_W: Object.keys(MISSING_SPECS_T_W)
};

/**
 * Contagem de operadores por categoria
 */
export const OPERATOR_COUNTS = {
  BASIC_COMPARISON: Object.keys(BASIC_COMPARISON_SPECS).length,
  ARRAY: Object.keys(ARRAY_SPECS).length,
  FIELD: Object.keys(FIELD_SPECS).length,
  ACCOUNT_AGE: Object.keys(ACCOUNT_AGE_SPECS).length,
  VELOCITY: Object.keys(VELOCITY_SPECS).length,
  GEO: Object.keys(GEO_SPECS).length,
  DEVICE: Object.keys(DEVICE_SPECS).length,
  NEO4J: Object.keys(NEO4J_SPECS).length,
  FATF_AML: Object.keys(FATF_SPECS).length,
  FRAUD_PATTERNS: Object.keys(FRAUD_PATTERN_SPECS).length,
  BIOMETRICS: Object.keys(BIOMETRIC_SPECS).length,
  STATISTICAL: Object.keys(STATISTICAL_SPECS).length,
  DATE_TIME: Object.keys(DATE_TIME_SPECS).length,
  MERCHANT_MCC: Object.keys(MERCHANT_SPECS).length,
  TRANSACTION: Object.keys(TRANSACTION_SPECS).length,
  SCA_PSD3: Object.keys(SCA_PSD3_SPECS).length,
  BSL_SANCTIONS: Object.keys(BSL_SANCTIONS_SPECS).length,
  ADDRESS_NAME: Object.keys(ADDRESS_NAME_SPECS).length,
  PLATFORM: Object.keys(PLATFORM_SPECS).length,
  STRING_ADVANCED: Object.keys(STRING_ADVANCED_SPECS).length,
  LIST_CHECKING: Object.keys(LIST_CHECKING_SPECS).length,
  MISSING_A_E: Object.keys(MISSING_SPECS_A_E).length,
  MISSING_EMV_MANN: Object.keys(MISSING_SPECS_EMV_MANN).length,
  MISSING_M_N: Object.keys(MISSING_SPECS_M_N).length,
  MISSING_NOT_S: Object.keys(MISSING_SPECS_NOT_S).length,
  MISSING_T_W: Object.keys(MISSING_SPECS_T_W).length,
  TOTAL: Object.keys(OPERATOR_SPECS_COMPLETE).length
};

/**
 * Helper para buscar spec de operador
 */
export function getOperatorSpec(operatorName: string) {
  return OPERATOR_SPECS_COMPLETE[operatorName] || null;
}

/**
 * Helper para buscar operadores por categoria
 */
export function getOperatorsByCategory(category: keyof typeof OPERATOR_CATEGORIES) {
  const operators = OPERATOR_CATEGORIES[category] || [];
  return operators.map(name => ({
    name,
    spec: OPERATOR_SPECS_COMPLETE[name]
  }));
}

/**
 * Helper para buscar operadores por texto (search)
 */
export function searchOperators(query: string) {
  const q = query.toLowerCase();
  return Object.entries(OPERATOR_SPECS_COMPLETE)
    .filter(([name, spec]) => 
      name.toLowerCase().includes(q) ||
      spec.summary?.toLowerCase().includes(q) ||
      spec.problem?.toLowerCase().includes(q) ||
      spec.story?.toLowerCase().includes(q)
    )
    .map(([name, spec]) => ({ name, spec }));
}

/**
 * Estatísticas do catálogo
 */
export const CATALOG_STATS = {
  totalOperators: Object.keys(OPERATOR_SPECS_COMPLETE).length,
  totalCategories: Object.keys(OPERATOR_CATEGORIES).length,
  operatorsWithEngineDetails: Object.values(OPERATOR_SPECS_COMPLETE)
    .filter(spec => spec.engineBehavior).length,
  operatorsWithRealScenarios: Object.values(OPERATOR_SPECS_COMPLETE)
    .filter(spec => spec.realScenarios && spec.realScenarios.length > 0).length,
  lastUpdated: new Date().toISOString()
};

// Log stats em desenvolvimento
if (typeof window !== 'undefined' && process.env.NODE_ENV === 'development') {
  console.log('📊 OPERATOR_SPECS_COMPLETE loaded:', CATALOG_STATS);
}

// Default export para conveniência
export default OPERATOR_SPECS_COMPLETE;
