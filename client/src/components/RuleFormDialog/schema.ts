/**
 * Schema de validação Zod para o formulário de regras
 */

import { z } from 'zod';
import type { RuleCondition } from '@/lib/javaApi';
import { UNARY_OPERATORS } from './types';

// ============================================
// SCHEMA DE CONDIÇÃO
// ============================================

const conditionOperators = [
  'EQ', 'NE', 'GT', 'LT', 'GTE', 'LTE',
  'IN', 'NOT_IN', 'BETWEEN', 'NOT_BETWEEN',
  'CONTAINS', 'NOT_CONTAINS', 'STARTS_WITH', 'ENDS_WITH', 'MATCHES_REGEX',
  'IS_NULL', 'IS_NOT_NULL', 'IS_TRUE', 'IS_FALSE',
  // Legado
  '==', '!=', '>', '<', '>=', '<=',
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
    // P0-01: Validar REGEX
    if (data.operator === 'MATCHES_REGEX') {
      try {
        new RegExp(data.value);
        return true;
      } catch {
        return false;
      }
    }
    return true;
  },
  {
    message: 'Expressão regular inválida. Verifique a sintaxe.',
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
    case 'MATCHES_REGEX':
      try {
        new RegExp(trimmedValue);
      } catch (e) {
        return `Expressão regular inválida: ${e instanceof Error ? e.message : 'erro de sintaxe'}`;
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
      return 'Ex: 10,100 ou 10..100';
    case 'MATCHES_REGEX':
      return 'Ex: regex';
    case 'CONTAINS':
    case 'NOT_CONTAINS':
      return 'Ex: texto a buscar';
    case 'STARTS_WITH':
      return 'Ex: prefixo';
    case 'ENDS_WITH':
      return 'Ex: sufixo';
    case 'IS_NULL':
    case 'IS_NOT_NULL':
    case 'IS_TRUE':
    case 'IS_FALSE':
      return '(nao aplicavel)';
    default:
      return 'Ex: 100';
  }
}

/**
 * Constante para limite maximo de condicoes
 */
export const MAX_CONDITIONS = 20;
