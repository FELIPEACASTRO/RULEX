/**
 * Documentação de comportamento NULL para operadores RULEX
 * GAP-002: Documentar semântica NULL no Frontend
 * 
 * Este arquivo define como cada operador se comporta quando o campo tem valor NULL/undefined
 */

export type NullBehavior = 
  | 'returns_false'    // NULL sempre retorna false
  | 'returns_true'     // NULL sempre retorna true  
  | 'checks_null'      // Operador verifica especificamente NULL
  | 'context_dependent' // Depende do contexto/configuração
  | 'not_applicable';  // Operador não recebe campo (ex: agregações)

export interface OperatorNullBehavior {
  operator: string;
  nullBehavior: NullBehavior;
  description: string;
}

/**
 * Mapeamento de comportamento NULL por operador
 * 
 * Regras gerais:
 * - Operadores de comparação (GT, LT, etc): NULL → false
 * - Operadores de string (CONTAINS, STARTS_WITH): NULL → false
 * - Operadores de verificação de NULL (IS_NULL, NOT_NULL): verificam NULL
 * - Operadores de lista (IN): NULL IN [] → false
 * - Operadores de lista negativa (NOT_IN): NULL NOT_IN [] → true
 */
export const OPERATOR_NULL_BEHAVIORS: Record<string, NullBehavior> = {
  // Comparação Básica - NULL retorna false
  'EQ': 'returns_false',
  'NEQ': 'returns_false',
  'GT': 'returns_false',
  'GTE': 'returns_false',
  'LT': 'returns_false',
  'LTE': 'returns_false',
  
  // Listas
  'IN': 'returns_false',      // NULL IN [valores] → false
  'NOT_IN': 'returns_true',   // NULL NOT_IN [valores] → true
  
  // Strings - NULL retorna false
  'CONTAINS': 'returns_false',
  'NOT_CONTAINS': 'returns_true',
  'STARTS_WITH': 'returns_false',
  'ENDS_WITH': 'returns_false',
  'REGEX': 'returns_false',
  'NOT_REGEX': 'returns_true',
  
  // Verificação de NULL - comportamento específico
  'IS_NULL': 'checks_null',   // NULL → true
  'NOT_NULL': 'checks_null',  // NULL → false
  'IS_TRUE': 'returns_false',
  'IS_FALSE': 'returns_false',
  
  // Range - NULL retorna false
  'BETWEEN': 'returns_false',
  'NOT_BETWEEN': 'returns_true',
  
  // Comparação entre campos - NULL retorna false
  'FIELD_EQ': 'returns_false',
  'FIELD_NEQ': 'returns_false',
  'FIELD_GT': 'returns_false',
  'FIELD_GTE': 'returns_false',
  'FIELD_LT': 'returns_false',
  'FIELD_LTE': 'returns_false',
  
  // Data/Hora - NULL retorna false
  'DATE_BEFORE': 'returns_false',
  'DATE_AFTER': 'returns_false',
  'DATE_BETWEEN': 'returns_false',
  'TIME_BEFORE': 'returns_false',
  'TIME_AFTER': 'returns_false',
  'TIME_BETWEEN': 'returns_false',
  
  // Arrays - NULL retorna false
  'ARRAY_CONTAINS': 'returns_false',
  'ARRAY_NOT_CONTAINS': 'returns_true',
  'ARRAY_SIZE_EQ': 'returns_false',
  'ARRAY_SIZE_GT': 'returns_false',
  'ARRAY_SIZE_LT': 'returns_false',
  
  // Matemáticos - NULL retorna false
  'MOD_EQ': 'returns_false',
  'MOD_NEQ': 'returns_false',
  
  // Geolocalização - NULL retorna false
  'GEO_DISTANCE_LT': 'returns_false',
  'GEO_DISTANCE_GT': 'returns_false',
  'GEO_IN_POLYGON': 'returns_false',
  
  // Velocity - Depende do contexto (usa Redis)
  'VELOCITY_COUNT_GT': 'context_dependent',
  'VELOCITY_COUNT_LT': 'context_dependent',
  'VELOCITY_SUM_GT': 'context_dependent',
  'VELOCITY_SUM_LT': 'context_dependent',
  'VELOCITY_AVG_GT': 'context_dependent',
  'VELOCITY_AVG_LT': 'context_dependent',
  'VELOCITY_DISTINCT_GT': 'context_dependent',
  'VELOCITY_DISTINCT_LT': 'context_dependent',
  
  // Neo4j - Depende do contexto (usa grafo)
  'NEO4J_WEAKLY_CONNECTED_COMPONENTS': 'context_dependent',
  'NEO4J_DEGREE_CENTRALITY': 'context_dependent',
  'NEO4J_PAGERANK_FRAUD_SCORE': 'context_dependent',
  'NEO4J_LOUVAIN_COMMUNITY_DETECTION': 'context_dependent',
  'NEO4J_PAIRWISE_SIMILARITY_PII': 'context_dependent',
  'NEO4J_ENTITY_RESOLUTION_SHARED_PII': 'context_dependent',
  'NEO4J_FRAUD_RING_DETECTION': 'context_dependent',
  'NEO4J_MONEY_MULE_NETWORK_ANALYSIS': 'context_dependent',
  'NEO4J_CIRCULAR_TRANSACTION_DETECTION': 'context_dependent',
  'NEO4J_FIRST_PARTY_FRAUD_CLUSTERING': 'context_dependent',
  'NEO4J_SECOND_LEVEL_FRAUDSTER_ID': 'context_dependent',
  'NEO4J_BETWEENNESS_CENTRALITY_MULE': 'context_dependent',
  'NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD': 'context_dependent',
  'NEO4J_SHORTEST_PATH_AML_TRACKING': 'context_dependent',
  'NEO4J_TRIANGLE_COUNT_COLLUSION': 'context_dependent',
  'NEO4J_NODE_SIMILARITY_SYNTHETIC_ID': 'context_dependent',
  'NEO4J_GRAPH_EMBEDDING_FRAUD_PREDICTION': 'context_dependent',
  'NEO4J_TEMPORAL_MOTIF_PATTERN': 'context_dependent',
};

/**
 * Obtém o comportamento NULL de um operador
 * @param operator Nome do operador
 * @returns Comportamento NULL ou 'context_dependent' se não mapeado
 */
export function getNullBehavior(operator: string): NullBehavior {
  return OPERATOR_NULL_BEHAVIORS[operator] || 'context_dependent';
}

/**
 * Verifica se um operador retorna false com NULL
 */
export function returnsfalseWithNull(operator: string): boolean {
  const behavior = getNullBehavior(operator);
  return behavior === 'returns_false';
}

/**
 * Verifica se um operador retorna true com NULL
 */
export function returnsTrueWithNull(operator: string): boolean {
  const behavior = getNullBehavior(operator);
  return behavior === 'returns_true';
}

/**
 * Verifica se um operador é específico para verificar NULL
 */
export function checksNull(operator: string): boolean {
  const behavior = getNullBehavior(operator);
  return behavior === 'checks_null';
}
