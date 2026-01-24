/**
 * Tipos para o RuleFormDialog
 */

import type { RuleConfiguration, RuleCondition, FieldDictionaryItem } from '@/lib/javaApi';

// ============================================
// TIPOS DE CLASSIFICAÇÃO E TIPO DE REGRA
// ============================================

export type RuleClassification = RuleConfiguration['classification'];
export type RuleType = RuleConfiguration['ruleType'];
export type LogicOperator = RuleConfiguration['logicOperator'];
export type ConditionOperator = RuleCondition['operator'];

// ============================================
// PROPS DO COMPONENTE
// ============================================

export interface RuleFormDialogProps {
  /** Controla se o dialog está aberto */
  open: boolean;
  /** Callback para mudança de estado do dialog */
  onOpenChange: (open: boolean) => void;
  /** Regra existente para edição (null para criação) */
  rule?: RuleConfiguration | null;
  /** Callback de sucesso após salvar */
  onSuccess?: (rule: RuleConfiguration) => void;
  /** Callback de erro */
  onError?: (error: Error) => void;
  /** Modo do formulário */
  mode?: 'create' | 'edit';
  /** Título customizado */
  title?: string;
  /** Descrição customizada */
  description?: string;
}

// ============================================
// TIPOS INTERNOS
// ============================================

export interface ConditionFieldOption {
  value: string;
  label: string;
  type: 'string' | 'number' | 'boolean' | 'date';
  description?: string;
  category?: string;
  allowedOperators?: ConditionOperator[];
}

export interface OperatorOption {
  value: ConditionOperator;
  label: string;
  description?: string;
  requiresValue?: boolean;
}

// ============================================
// CONSTANTES
// ============================================

export const RULE_TYPES: { value: RuleType; label: string; description: string }[] = [
  { value: 'SECURITY', label: 'Segurança', description: 'Regras de verificação de segurança e autenticação' },
  { value: 'CONTEXT', label: 'Contexto', description: 'Regras baseadas no contexto da transação' },
  { value: 'VELOCITY', label: 'Velocidade', description: 'Regras de frequência e velocidade de transações' },
  { value: 'ANOMALY', label: 'Anomalia', description: 'Regras de detecção de padrões anômalos' },
];

export const CLASSIFICATIONS: { value: RuleClassification; label: string; color: string; icon: string }[] = [
  { value: 'APPROVED', label: 'Aprovada', color: 'bg-green-100 text-green-800', icon: '✅' },
  { value: 'SUSPICIOUS', label: 'Suspeita', color: 'bg-amber-100 text-amber-800', icon: '⚠️' },
  { value: 'FRAUD', label: 'Fraude', color: 'bg-red-100 text-red-800', icon: '🚫' },
];

export const LOGIC_OPERATORS: { value: LogicOperator; label: string; description: string }[] = [
  { value: 'AND', label: 'E (AND)', description: 'Todas as condições devem ser verdadeiras' },
  { value: 'OR', label: 'OU (OR)', description: 'Pelo menos uma condição deve ser verdadeira' },
];

export const OPERATORS: OperatorOption[] = [
  // ========== Comparação Básica ==========
  { value: 'EQ', label: '== (Igual)', description: 'Valor igual a' },
  { value: 'NEQ', label: '!= (Diferente)', description: 'Valor diferente de' },
  { value: 'GT', label: '> (Maior)', description: 'Valor maior que' },
  { value: 'LT', label: '< (Menor)', description: 'Valor menor que' },
  { value: 'GTE', label: '>= (Maior ou igual)', description: 'Valor maior ou igual a' },
  { value: 'LTE', label: '<= (Menor ou igual)', description: 'Valor menor ou igual a' },
  // ========== Listas ==========
  { value: 'IN', label: 'IN (Na lista)', description: 'Valor está na lista' },
  { value: 'NOT_IN', label: 'NOT IN (Fora da lista)', description: 'Valor não está na lista' },
  // ========== Range ==========
  { value: 'BETWEEN', label: 'BETWEEN (Entre)', description: 'Valor entre dois limites (ex: 10,100)' },
  { value: 'NOT_BETWEEN', label: 'NOT BETWEEN', description: 'Valor fora do intervalo' },
  // ========== String ==========
  { value: 'CONTAINS', label: 'CONTAINS (Contém)', description: 'Texto contém substring' },
  { value: 'NOT_CONTAINS', label: 'NOT CONTAINS', description: 'Texto não contém substring' },
  { value: 'STARTS_WITH', label: 'STARTS WITH', description: 'Texto começa com' },
  { value: 'ENDS_WITH', label: 'ENDS WITH', description: 'Texto termina com' },
  { value: 'REGEX', label: 'REGEX', description: 'Corresponde à expressão regular' },
  { value: 'NOT_REGEX', label: 'NOT REGEX', description: 'Não corresponde à expressão regular' },
  // ========== Null/Boolean ==========
  { value: 'IS_NULL', label: 'IS NULL', description: 'Valor é nulo', requiresValue: false },
  { value: 'NOT_NULL', label: 'NOT NULL', description: 'Valor não é nulo', requiresValue: false },
  { value: 'IS_TRUE', label: 'IS TRUE', description: 'Valor é verdadeiro', requiresValue: false },
  { value: 'IS_FALSE', label: 'IS FALSE', description: 'Valor é falso', requiresValue: false },
  // ========== Comparação entre Campos ==========
  { value: 'FIELD_EQ', label: 'FIELD == (Igual a campo)', description: 'Igual a outro campo' },
  { value: 'FIELD_NEQ', label: 'FIELD != (Diferente de campo)', description: 'Diferente de outro campo' },
  { value: 'FIELD_GT', label: 'FIELD > (Maior que campo)', description: 'Maior que outro campo' },
  { value: 'FIELD_GTE', label: 'FIELD >= (Maior ou igual a campo)', description: 'Maior ou igual a outro campo' },
  { value: 'FIELD_LT', label: 'FIELD < (Menor que campo)', description: 'Menor que outro campo' },
  { value: 'FIELD_LTE', label: 'FIELD <= (Menor ou igual a campo)', description: 'Menor ou igual a outro campo' },
  // ========== Data/Hora ==========
  { value: 'DATE_BEFORE', label: 'DATE BEFORE', description: 'Data anterior a (YYYY-MM-DD)' },
  { value: 'DATE_AFTER', label: 'DATE AFTER', description: 'Data posterior a (YYYY-MM-DD)' },
  { value: 'DATE_BETWEEN', label: 'DATE BETWEEN', description: 'Data entre (YYYY-MM-DD,YYYY-MM-DD)' },
  { value: 'TIME_BEFORE', label: 'TIME BEFORE', description: 'Hora anterior a (HH:MM:SS)' },
  { value: 'TIME_AFTER', label: 'TIME AFTER', description: 'Hora posterior a (HH:MM:SS)' },
  { value: 'TIME_BETWEEN', label: 'TIME BETWEEN', description: 'Hora entre (HH:MM:SS,HH:MM:SS)' },
  // ========== Array ==========
  { value: 'ARRAY_CONTAINS', label: 'ARRAY CONTAINS', description: 'Array contém o valor' },
  { value: 'ARRAY_NOT_CONTAINS', label: 'ARRAY NOT CONTAINS', description: 'Array não contém o valor' },
  { value: 'ARRAY_SIZE_EQ', label: 'ARRAY SIZE ==', description: 'Tamanho do array igual a' },
  { value: 'ARRAY_SIZE_GT', label: 'ARRAY SIZE >', description: 'Tamanho do array maior que' },
  { value: 'ARRAY_SIZE_LT', label: 'ARRAY SIZE <', description: 'Tamanho do array menor que' },
  // ========== Matemáticos ==========
  { value: 'MOD_EQ', label: 'MOD == (Módulo igual)', description: 'Resto da divisão igual (divisor,resto)' },
  { value: 'MOD_NEQ', label: 'MOD != (Módulo diferente)', description: 'Resto da divisão diferente' },
  // ========== Geolocalização ==========
  { value: 'GEO_DISTANCE_LT', label: 'GEO DISTANCE <', description: 'Distância menor que (lat,lon,km)' },
  { value: 'GEO_DISTANCE_GT', label: 'GEO DISTANCE >', description: 'Distância maior que (lat,lon,km)' },
  { value: 'GEO_IN_POLYGON', label: 'GEO IN POLYGON', description: 'Dentro do polígono (nome)' },
  // ========== Velocity (Agregações Temporais) ==========
  { value: 'VELOCITY_COUNT_GT', label: 'VELOCITY COUNT >', description: 'Contagem > (keyType,minutes,threshold)' },
  { value: 'VELOCITY_COUNT_LT', label: 'VELOCITY COUNT <', description: 'Contagem < (keyType,minutes,threshold)' },
  { value: 'VELOCITY_SUM_GT', label: 'VELOCITY SUM >', description: 'Soma > (keyType,minutes,threshold)' },
  { value: 'VELOCITY_SUM_LT', label: 'VELOCITY SUM <', description: 'Soma < (keyType,minutes,threshold)' },
  { value: 'VELOCITY_AVG_GT', label: 'VELOCITY AVG >', description: 'Média > (keyType,minutes,threshold)' },
  { value: 'VELOCITY_AVG_LT', label: 'VELOCITY AVG <', description: 'Média < (keyType,minutes,threshold)' },
  { value: 'VELOCITY_DISTINCT_GT', label: 'VELOCITY DISTINCT >', description: 'Distintos > (keyType,minutes,type,threshold)' },
  { value: 'VELOCITY_DISTINCT_LT', label: 'VELOCITY DISTINCT <', description: 'Distintos < (keyType,minutes,type,threshold)' },
];

// Operadores que não requerem valor
export const UNARY_OPERATORS: ConditionOperator[] = ['IS_NULL', 'NOT_NULL', 'IS_TRUE', 'IS_FALSE'];

// Operadores que requerem referência a outro campo
export const FIELD_REF_OPERATORS: ConditionOperator[] = ['FIELD_EQ', 'FIELD_NEQ', 'FIELD_GT', 'FIELD_GTE', 'FIELD_LT', 'FIELD_LTE'];

// Mapeamento de tipo de campo para operadores permitidos (alinhado com backend)
export const OPERATORS_BY_TYPE: Record<string, ConditionOperator[]> = {
  number: [
    'EQ', 'NEQ', 'GT', 'LT', 'GTE', 'LTE', 'IN', 'NOT_IN', 'BETWEEN', 'NOT_BETWEEN',
    'FIELD_EQ', 'FIELD_NEQ', 'FIELD_GT', 'FIELD_GTE', 'FIELD_LT', 'FIELD_LTE',
    'MOD_EQ', 'MOD_NEQ',
    'VELOCITY_COUNT_GT', 'VELOCITY_COUNT_LT', 'VELOCITY_SUM_GT', 'VELOCITY_SUM_LT',
    'VELOCITY_AVG_GT', 'VELOCITY_AVG_LT', 'VELOCITY_DISTINCT_GT', 'VELOCITY_DISTINCT_LT',
    'IS_NULL', 'NOT_NULL'
  ],
  string: [
    'EQ', 'NEQ', 'IN', 'NOT_IN', 'CONTAINS', 'NOT_CONTAINS', 'STARTS_WITH', 'ENDS_WITH',
    'REGEX', 'NOT_REGEX', 'FIELD_EQ', 'FIELD_NEQ',
    'GEO_DISTANCE_LT', 'GEO_DISTANCE_GT', 'GEO_IN_POLYGON',
    'IS_NULL', 'NOT_NULL'
  ],
  boolean: ['IS_TRUE', 'IS_FALSE', 'IS_NULL', 'NOT_NULL'],
  date: [
    'EQ', 'NEQ', 'GT', 'LT', 'GTE', 'LTE', 'BETWEEN', 'NOT_BETWEEN',
    'DATE_BEFORE', 'DATE_AFTER', 'DATE_BETWEEN',
    'FIELD_EQ', 'FIELD_NEQ', 'FIELD_GT', 'FIELD_GTE', 'FIELD_LT', 'FIELD_LTE',
    'IS_NULL', 'NOT_NULL'
  ],
  time: [
    'TIME_BEFORE', 'TIME_AFTER', 'TIME_BETWEEN',
    'IS_NULL', 'NOT_NULL'
  ],
  array: [
    'ARRAY_CONTAINS', 'ARRAY_NOT_CONTAINS', 'ARRAY_SIZE_EQ', 'ARRAY_SIZE_GT', 'ARRAY_SIZE_LT',
    'IS_NULL', 'NOT_NULL'
  ],
};

// Campos fallback quando a API não está disponível
export const FALLBACK_FIELDS: ConditionFieldOption[] = [
  // Identificação
  { value: 'customerIdFromHeader', label: 'ID do Cliente', type: 'string', category: 'Identificação' },
  { value: 'merchantId', label: 'ID do Merchant', type: 'string', category: 'Identificação' },
  { value: 'pan', label: 'PAN (Cartão)', type: 'string', category: 'Identificação' },
  { value: 'externalTransactionId', label: 'ID Externo', type: 'string', category: 'Identificação' },

  // Valores/Datas
  { value: 'transactionAmount', label: 'Valor da Transação', type: 'number', category: 'Valores' },
  { value: 'transactionDate', label: 'Data da Transação', type: 'number', category: 'Valores', description: 'Formato YYYYMMDD' },
  { value: 'transactionTime', label: 'Hora da Transação', type: 'number', category: 'Valores', description: 'Formato HHMMSS' },
  { value: 'transactionCurrencyCode', label: 'Código da Moeda', type: 'number', category: 'Valores' },

  // Localização
  { value: 'merchantCountryCode', label: 'País do Merchant', type: 'string', category: 'Localização' },
  { value: 'merchantCity', label: 'Cidade do Merchant', type: 'string', category: 'Localização' },
  { value: 'merchantState', label: 'Estado do Merchant', type: 'string', category: 'Localização' },
  { value: 'merchantPostalCode', label: 'CEP do Merchant', type: 'string', category: 'Localização' },

  // Segurança
  { value: 'consumerAuthenticationScore', label: 'Score de Autenticação', type: 'number', category: 'Segurança', description: '0-999' },
  { value: 'externalScore3', label: 'Score Externo', type: 'number', category: 'Segurança', description: '0-999' },
  { value: 'cavvResult', label: 'Resultado CAVV', type: 'number', category: 'Segurança', description: '3D Secure' },
  { value: 'cryptogramValid', label: 'Criptograma Válido', type: 'string', category: 'Segurança' },
  { value: 'cvv2Response', label: 'Resposta CVV2', type: 'string', category: 'Segurança' },
  { value: 'eciIndicator', label: 'Indicador ECI', type: 'number', category: 'Segurança' },

  // Categoria
  { value: 'mcc', label: 'MCC (Categoria)', type: 'number', category: 'Categoria', description: 'Merchant Category Code' },
  { value: 'posEntryMode', label: 'Modo de Entrada', type: 'string', category: 'Categoria' },
  { value: 'customerPresent', label: 'Cliente Presente', type: 'string', category: 'Categoria' },
];
