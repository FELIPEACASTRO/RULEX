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

export type ComparisonOperator =
  // Básicos
  | 'EQ' | 'NEQ' | 'GT' | 'GTE' | 'LT' | 'LTE'
  // Listas
  | 'IN' | 'NOT_IN'
  // Range
  | 'BETWEEN' | 'NOT_BETWEEN'
  // Strings (REGEX alinhado com backend, MATCHES_REGEX mantido para compatibilidade)
  | 'CONTAINS' | 'NOT_CONTAINS' | 'STARTS_WITH' | 'ENDS_WITH' | 'REGEX' | 'NOT_REGEX' | 'MATCHES_REGEX'
  // Nulos (NOT_NULL alinhado com backend, IS_NOT_NULL mantido para compatibilidade)
  | 'IS_NULL' | 'NOT_NULL' | 'IS_NOT_NULL'
  // Booleanos
  | 'IS_TRUE' | 'IS_FALSE'
  // Comparação entre campos
  | 'FIELD_EQ' | 'FIELD_NEQ' | 'FIELD_GT' | 'FIELD_GTE' | 'FIELD_LT' | 'FIELD_LTE'
  // Data/Hora
  | 'DATE_BEFORE' | 'DATE_AFTER' | 'DATE_BETWEEN'
  | 'TIME_BEFORE' | 'TIME_AFTER' | 'TIME_BETWEEN'
  // Arrays
  | 'ARRAY_CONTAINS' | 'ARRAY_NOT_CONTAINS' | 'ARRAY_SIZE_EQ' | 'ARRAY_SIZE_GT' | 'ARRAY_SIZE_LT'
  // Matemáticos
  | 'MOD_EQ' | 'MOD_NEQ'
  // Geolocalização
  | 'GEO_DISTANCE_LT' | 'GEO_DISTANCE_GT' | 'GEO_IN_POLYGON';

export interface OperatorInfo {
  value: ComparisonOperator;
  label: string;
  description: string;
  category: 'basic' | 'list' | 'range' | 'string' | 'null' | 'boolean' | 'field' | 'date' | 'array' | 'math' | 'geo';
  requiresValue: boolean;
  requiresSecondValue?: boolean;
  requiresFieldRef?: boolean;
  applicableTypes: ValueType[];
}

export const COMPARISON_OPERATORS: OperatorInfo[] = [
  // Básicos
  { value: 'EQ', label: '=', description: 'Igual a', category: 'basic', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'NEQ', label: '≠', description: 'Diferente de', category: 'basic', requiresValue: true, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'GT', label: '>', description: 'Maior que', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'GTE', label: '≥', description: 'Maior ou igual a', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'LT', label: '<', description: 'Menor que', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'LTE', label: '≤', description: 'Menor ou igual a', category: 'basic', requiresValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },

  // Listas
  { value: 'IN', label: 'está em', description: 'Valor está na lista', category: 'list', requiresValue: true, applicableTypes: ['STRING', 'NUMBER'] },
  { value: 'NOT_IN', label: 'não está em', description: 'Valor não está na lista', category: 'list', requiresValue: true, applicableTypes: ['STRING', 'NUMBER'] },

  // Range
  { value: 'BETWEEN', label: 'entre', description: 'Valor está entre dois valores', category: 'range', requiresValue: true, requiresSecondValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },
  { value: 'NOT_BETWEEN', label: 'não entre', description: 'Valor não está entre dois valores', category: 'range', requiresValue: true, requiresSecondValue: true, applicableTypes: ['NUMBER', 'DATE', 'TIME', 'DATETIME'] },

  // Strings
  { value: 'CONTAINS', label: 'contém', description: 'Texto contém', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'NOT_CONTAINS', label: 'não contém', description: 'Texto não contém', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'STARTS_WITH', label: 'começa com', description: 'Texto começa com', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'ENDS_WITH', label: 'termina com', description: 'Texto termina com', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'REGEX', label: 'regex', description: 'Corresponde à expressão regular', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'NOT_REGEX', label: 'não regex', description: 'Não corresponde à expressão regular', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'MATCHES_REGEX', label: 'regex (legacy)', description: 'Corresponde à regex (use REGEX)', category: 'string', requiresValue: true, applicableTypes: ['STRING'] },

  // Nulos
  { value: 'IS_NULL', label: 'é nulo', description: 'Campo é nulo', category: 'null', requiresValue: false, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'ARRAY_STRING', 'ARRAY_NUMBER'] },
  { value: 'NOT_NULL', label: 'não é nulo', description: 'Campo não é nulo', category: 'null', requiresValue: false, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'ARRAY_STRING', 'ARRAY_NUMBER'] },
  { value: 'IS_NOT_NULL', label: 'não é nulo (legacy)', description: 'Campo não é nulo (use NOT_NULL)', category: 'null', requiresValue: false, applicableTypes: ['STRING', 'NUMBER', 'BOOLEAN', 'DATE', 'TIME', 'DATETIME', 'ARRAY_STRING', 'ARRAY_NUMBER'] },

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
  { value: 'GEO_DISTANCE_LT', label: 'distância <', description: 'Distância menor que (km). Formato: lat,lon,distKm', category: 'geo', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'GEO_DISTANCE_GT', label: 'distância >', description: 'Distância maior que (km). Formato: lat,lon,distKm', category: 'geo', requiresValue: true, applicableTypes: ['STRING'] },
  { value: 'GEO_IN_POLYGON', label: 'no polígono', description: 'Dentro do polígono. Valor: nome do polígono', category: 'geo', requiresValue: true, applicableTypes: ['STRING'] },
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
