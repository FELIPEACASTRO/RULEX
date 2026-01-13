/**
 * Testes unitários para CADA operador suportado pelo sistema
 * Gap P1-02: Cobertura completa dos 52 operadores
 */

import { describe, expect, it } from 'vitest';
import { validateValueByOperator, conditionSchema } from './schema';

// Lista completa de todos os operadores suportados
const ALL_OPERATORS = {
  // Básicos (6)
  basic: ['EQ', 'NEQ', 'GT', 'LT', 'GTE', 'LTE'],
  // Listas (4)
  lists: ['IN', 'NOT_IN', 'BETWEEN', 'NOT_BETWEEN'],
  // Strings (6)
  strings: ['CONTAINS', 'NOT_CONTAINS', 'STARTS_WITH', 'ENDS_WITH', 'REGEX', 'NOT_REGEX'],
  // Nulos/Booleanos (4)
  unary: ['IS_NULL', 'NOT_NULL', 'IS_TRUE', 'IS_FALSE'],
  // Comparação entre campos (6)
  fieldComparison: ['FIELD_EQ', 'FIELD_NEQ', 'FIELD_GT', 'FIELD_GTE', 'FIELD_LT', 'FIELD_LTE'],
  // Data/Hora (6)
  dateTime: ['DATE_BEFORE', 'DATE_AFTER', 'DATE_BETWEEN', 'TIME_BEFORE', 'TIME_AFTER', 'TIME_BETWEEN'],
  // Array (5)
  array: ['ARRAY_CONTAINS', 'ARRAY_NOT_CONTAINS', 'ARRAY_SIZE_EQ', 'ARRAY_SIZE_GT', 'ARRAY_SIZE_LT'],
  // Matemáticos (2)
  math: ['MOD_EQ', 'MOD_NEQ'],
  // Geolocalização (3)
  geo: ['GEO_DISTANCE_LT', 'GEO_DISTANCE_GT', 'GEO_IN_POLYGON'],
  // Velocity (8)
  velocity: [
    'VELOCITY_COUNT_GT', 'VELOCITY_COUNT_LT',
    'VELOCITY_SUM_GT', 'VELOCITY_SUM_LT',
    'VELOCITY_AVG_GT', 'VELOCITY_AVG_LT',
    'VELOCITY_DISTINCT_GT', 'VELOCITY_DISTINCT_LT'
  ],
  // Legacy (6)
  legacy: ['NE', 'MATCHES_REGEX', 'IS_NOT_NULL', '==', '!=', '>', '<', '>=', '<='],
} as const;

// ============================================
// OPERADORES BÁSICOS (EQ, NEQ, GT, LT, GTE, LTE)
// ============================================
describe('Operadores Básicos', () => {
  describe('EQ (Equal)', () => {
    it('aceita valor string', () => {
      const error = validateValueByOperator('EQ', 'test');
      expect(error).toBeNull();
    });

    it('aceita valor numérico', () => {
      const error = validateValueByOperator('EQ', '100');
      expect(error).toBeNull();
    });

    it('rejeita valor vazio', () => {
      const error = validateValueByOperator('EQ', '');
      expect(error).not.toBeNull();
    });

    it('aceita valor com espaços (trim)', () => {
      const error = validateValueByOperator('EQ', '  test  ');
      expect(error).toBeNull();
    });
  });

  describe('NEQ (Not Equal)', () => {
    it('aceita valor string', () => {
      const error = validateValueByOperator('NEQ', 'test');
      expect(error).toBeNull();
    });

    it('aceita valor numérico', () => {
      const error = validateValueByOperator('NEQ', '100');
      expect(error).toBeNull();
    });

    it('rejeita valor vazio', () => {
      const error = validateValueByOperator('NEQ', '');
      expect(error).not.toBeNull();
    });
  });

  describe('GT (Greater Than)', () => {
    it('aceita número positivo', () => {
      const error = validateValueByOperator('GT', '100');
      expect(error).toBeNull();
    });

    it('aceita número negativo', () => {
      const error = validateValueByOperator('GT', '-100');
      expect(error).toBeNull();
    });

    it('aceita zero', () => {
      const error = validateValueByOperator('GT', '0');
      expect(error).toBeNull();
    });

    it('rejeita texto para campo number', () => {
      const error = validateValueByOperator('GT', 'abc', 'number');
      expect(error).not.toBeNull();
    });
  });

  describe('LT (Less Than)', () => {
    it('aceita número positivo', () => {
      const error = validateValueByOperator('LT', '50');
      expect(error).toBeNull();
    });

    it('aceita número negativo', () => {
      const error = validateValueByOperator('LT', '-50');
      expect(error).toBeNull();
    });

    it('rejeita texto para campo number', () => {
      const error = validateValueByOperator('LT', 'xyz', 'number');
      expect(error).not.toBeNull();
    });
  });

  describe('GTE (Greater Than or Equal)', () => {
    it('aceita número', () => {
      const error = validateValueByOperator('GTE', '100');
      expect(error).toBeNull();
    });

    it('rejeita texto para campo number', () => {
      const error = validateValueByOperator('GTE', 'not-a-number', 'number');
      expect(error).not.toBeNull();
    });
  });

  describe('LTE (Less Than or Equal)', () => {
    it('aceita número', () => {
      const error = validateValueByOperator('LTE', '200');
      expect(error).toBeNull();
    });

    it('rejeita texto para campo number', () => {
      const error = validateValueByOperator('LTE', 'invalid', 'number');
      expect(error).not.toBeNull();
    });
  });
});

// ============================================
// OPERADORES DE LISTA (IN, NOT_IN, BETWEEN, NOT_BETWEEN)
// ============================================
describe('Operadores de Lista', () => {
  describe('IN', () => {
    it('aceita lista simples', () => {
      const error = validateValueByOperator('IN', '1,2,3');
      expect(error).toBeNull();
    });

    it('aceita lista com colchetes', () => {
      const error = validateValueByOperator('IN', '[1,2,3]');
      expect(error).toBeNull();
    });

    it('aceita lista de strings', () => {
      const error = validateValueByOperator('IN', 'a,b,c');
      expect(error).toBeNull();
    });

    it('aceita lista com um item', () => {
      const error = validateValueByOperator('IN', '1');
      expect(error).toBeNull();
    });

    it('rejeita lista vazia', () => {
      const error = validateValueByOperator('IN', '');
      expect(error).not.toBeNull();
    });

    it('valida tipos numéricos', () => {
      const error = validateValueByOperator('IN', 'a,b', 'number');
      expect(error).not.toBeNull();
    });
  });

  describe('NOT_IN', () => {
    it('aceita lista simples', () => {
      const error = validateValueByOperator('NOT_IN', '1,2,3');
      expect(error).toBeNull();
    });

    it('aceita lista com colchetes', () => {
      const error = validateValueByOperator('NOT_IN', '[a,b,c]');
      expect(error).toBeNull();
    });

    it('rejeita lista vazia', () => {
      const error = validateValueByOperator('NOT_IN', '');
      expect(error).not.toBeNull();
    });
  });

  describe('BETWEEN', () => {
    it('aceita formato vírgula', () => {
      const error = validateValueByOperator('BETWEEN', '10,100');
      expect(error).toBeNull();
    });

    it('aceita formato dois pontos', () => {
      const error = validateValueByOperator('BETWEEN', '10..100');
      expect(error).toBeNull();
    });

    it('aceita números negativos', () => {
      const error = validateValueByOperator('BETWEEN', '-100,100');
      expect(error).toBeNull();
    });

    it('rejeita valor único', () => {
      const error = validateValueByOperator('BETWEEN', '100');
      expect(error).not.toBeNull();
    });

    it('rejeita três valores', () => {
      const error = validateValueByOperator('BETWEEN', '10,50,100');
      expect(error).not.toBeNull();
    });

    it('rejeita valores vazios', () => {
      const error = validateValueByOperator('BETWEEN', ',');
      expect(error).not.toBeNull();
    });

    it('rejeita ordem invertida para números', () => {
      const error = validateValueByOperator('BETWEEN', '100,10', 'number');
      expect(error).not.toBeNull();
    });

    it('rejeita texto para campo number', () => {
      const error = validateValueByOperator('BETWEEN', 'a,b', 'number');
      expect(error).not.toBeNull();
    });
  });

  describe('NOT_BETWEEN', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('NOT_BETWEEN', '10,100');
      expect(error).toBeNull();
    });

    it('aceita formato dois pontos', () => {
      const error = validateValueByOperator('NOT_BETWEEN', '10..100');
      expect(error).toBeNull();
    });

    it('rejeita valor único', () => {
      const error = validateValueByOperator('NOT_BETWEEN', '50');
      expect(error).not.toBeNull();
    });
  });
});

// ============================================
// OPERADORES DE STRING
// ============================================
describe('Operadores de String', () => {
  describe('CONTAINS', () => {
    it('aceita substring', () => {
      const error = validateValueByOperator('CONTAINS', 'test');
      expect(error).toBeNull();
    });

    it('aceita caracteres especiais', () => {
      const error = validateValueByOperator('CONTAINS', '@#$');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('CONTAINS', '');
      expect(error).not.toBeNull();
    });
  });

  describe('NOT_CONTAINS', () => {
    it('aceita substring', () => {
      const error = validateValueByOperator('NOT_CONTAINS', 'test');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('NOT_CONTAINS', '');
      expect(error).not.toBeNull();
    });
  });

  describe('STARTS_WITH', () => {
    it('aceita prefixo', () => {
      const error = validateValueByOperator('STARTS_WITH', 'PREFIX');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('STARTS_WITH', '');
      expect(error).not.toBeNull();
    });
  });

  describe('ENDS_WITH', () => {
    it('aceita sufixo', () => {
      const error = validateValueByOperator('ENDS_WITH', 'SUFFIX');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('ENDS_WITH', '');
      expect(error).not.toBeNull();
    });
  });

  describe('REGEX', () => {
    it('aceita regex válida simples', () => {
      const error = validateValueByOperator('REGEX', '^test$');
      expect(error).toBeNull();
    });

    it('aceita regex com classes de caracteres', () => {
      const error = validateValueByOperator('REGEX', '[A-Z0-9]+');
      expect(error).toBeNull();
    });

    it('aceita regex match-all', () => {
      const error = validateValueByOperator('REGEX', '.*');
      expect(error).toBeNull();
    });

    it('rejeita regex inválida (colchete aberto)', () => {
      const error = validateValueByOperator('REGEX', '[');
      expect(error).not.toBeNull();
    });

    it('rejeita regex inválida (parêntese)', () => {
      const error = validateValueByOperator('REGEX', '(');
      expect(error).not.toBeNull();
    });

    it('rejeita regex ReDoS (nested quantifiers)', () => {
      const error = validateValueByOperator('REGEX', '(a+)+');
      expect(error).not.toBeNull();
    });

    it('rejeita regex muito longa', () => {
      const error = validateValueByOperator('REGEX', 'a'.repeat(1000));
      expect(error).not.toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('REGEX', '');
      expect(error).not.toBeNull();
    });
  });

  describe('NOT_REGEX', () => {
    it('aceita regex válida', () => {
      const error = validateValueByOperator('NOT_REGEX', '^[0-9]+$');
      expect(error).toBeNull();
    });

    it('rejeita regex inválida', () => {
      const error = validateValueByOperator('NOT_REGEX', '[invalid');
      expect(error).not.toBeNull();
    });

    it('rejeita regex ReDoS', () => {
      const error = validateValueByOperator('NOT_REGEX', '(x+)+y');
      expect(error).not.toBeNull();
    });
  });
});

// ============================================
// OPERADORES UNÁRIOS (IS_NULL, NOT_NULL, IS_TRUE, IS_FALSE)
// ============================================
describe('Operadores Unários', () => {
  describe('IS_NULL', () => {
    it('ignora valor (unário)', () => {
      const error = validateValueByOperator('IS_NULL', 'qualquer');
      expect(error).toBeNull();
    });

    it('aceita sem valor', () => {
      const error = validateValueByOperator('IS_NULL', '');
      expect(error).toBeNull();
    });
  });

  describe('NOT_NULL', () => {
    it('ignora valor (unário)', () => {
      const error = validateValueByOperator('NOT_NULL', 'qualquer');
      expect(error).toBeNull();
    });

    it('aceita sem valor', () => {
      const error = validateValueByOperator('NOT_NULL', '');
      expect(error).toBeNull();
    });
  });

  describe('IS_TRUE', () => {
    it('ignora valor (unário)', () => {
      const error = validateValueByOperator('IS_TRUE', 'qualquer');
      expect(error).toBeNull();
    });

    it('aceita sem valor', () => {
      const error = validateValueByOperator('IS_TRUE', '');
      expect(error).toBeNull();
    });
  });

  describe('IS_FALSE', () => {
    it('ignora valor (unário)', () => {
      const error = validateValueByOperator('IS_FALSE', 'qualquer');
      expect(error).toBeNull();
    });

    it('aceita sem valor', () => {
      const error = validateValueByOperator('IS_FALSE', '');
      expect(error).toBeNull();
    });
  });

  describe('IS_NOT_NULL (legacy)', () => {
    it('ignora valor (unário)', () => {
      const error = validateValueByOperator('IS_NOT_NULL', 'qualquer');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES DE COMPARAÇÃO ENTRE CAMPOS
// ============================================
describe('Operadores de Comparação entre Campos', () => {
  describe('FIELD_EQ', () => {
    it('aceita nome de campo', () => {
      const error = validateValueByOperator('FIELD_EQ', 'outroNomeDeCampo');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('FIELD_EQ', '');
      expect(error).not.toBeNull();
    });
  });

  describe('FIELD_NEQ', () => {
    it('aceita nome de campo', () => {
      const error = validateValueByOperator('FIELD_NEQ', 'fieldName');
      expect(error).toBeNull();
    });
  });

  describe('FIELD_GT', () => {
    it('aceita nome de campo', () => {
      const error = validateValueByOperator('FIELD_GT', 'amount1');
      expect(error).toBeNull();
    });
  });

  describe('FIELD_GTE', () => {
    it('aceita nome de campo', () => {
      const error = validateValueByOperator('FIELD_GTE', 'threshold');
      expect(error).toBeNull();
    });
  });

  describe('FIELD_LT', () => {
    it('aceita nome de campo', () => {
      const error = validateValueByOperator('FIELD_LT', 'limit');
      expect(error).toBeNull();
    });
  });

  describe('FIELD_LTE', () => {
    it('aceita nome de campo', () => {
      const error = validateValueByOperator('FIELD_LTE', 'maxValue');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES DE DATA/HORA
// ============================================
describe('Operadores de Data/Hora', () => {
  describe('DATE_BEFORE', () => {
    it('aceita data ISO', () => {
      const error = validateValueByOperator('DATE_BEFORE', '2024-12-31');
      expect(error).toBeNull();
    });

    it('aceita data YYYYMMDD', () => {
      const error = validateValueByOperator('DATE_BEFORE', '20241231');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('DATE_BEFORE', '');
      expect(error).not.toBeNull();
    });
  });

  describe('DATE_AFTER', () => {
    it('aceita data ISO', () => {
      const error = validateValueByOperator('DATE_AFTER', '2024-01-01');
      expect(error).toBeNull();
    });

    it('aceita data YYYYMMDD', () => {
      const error = validateValueByOperator('DATE_AFTER', '20240101');
      expect(error).toBeNull();
    });
  });

  describe('DATE_BETWEEN', () => {
    it('aceita range de datas', () => {
      const error = validateValueByOperator('DATE_BETWEEN', '2024-01-01,2024-12-31');
      expect(error).toBeNull();
    });

    it('aceita formato dois pontos', () => {
      const error = validateValueByOperator('DATE_BETWEEN', '20240101..20241231');
      expect(error).toBeNull();
    });
  });

  describe('TIME_BEFORE', () => {
    it('aceita horário', () => {
      const error = validateValueByOperator('TIME_BEFORE', '23:59:59');
      expect(error).toBeNull();
    });

    it('aceita formato HHMMSS', () => {
      const error = validateValueByOperator('TIME_BEFORE', '235959');
      expect(error).toBeNull();
    });
  });

  describe('TIME_AFTER', () => {
    it('aceita horário', () => {
      const error = validateValueByOperator('TIME_AFTER', '00:00:01');
      expect(error).toBeNull();
    });
  });

  describe('TIME_BETWEEN', () => {
    it('aceita range de horários', () => {
      const error = validateValueByOperator('TIME_BETWEEN', '08:00:00,18:00:00');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES DE ARRAY
// ============================================
describe('Operadores de Array', () => {
  describe('ARRAY_CONTAINS', () => {
    it('aceita valor', () => {
      const error = validateValueByOperator('ARRAY_CONTAINS', 'item');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('ARRAY_CONTAINS', '');
      expect(error).not.toBeNull();
    });
  });

  describe('ARRAY_NOT_CONTAINS', () => {
    it('aceita valor', () => {
      const error = validateValueByOperator('ARRAY_NOT_CONTAINS', 'item');
      expect(error).toBeNull();
    });
  });

  describe('ARRAY_SIZE_EQ', () => {
    it('aceita número', () => {
      const error = validateValueByOperator('ARRAY_SIZE_EQ', '5');
      expect(error).toBeNull();
    });

    it('aceita zero', () => {
      const error = validateValueByOperator('ARRAY_SIZE_EQ', '0');
      expect(error).toBeNull();
    });
  });

  describe('ARRAY_SIZE_GT', () => {
    it('aceita número', () => {
      const error = validateValueByOperator('ARRAY_SIZE_GT', '10');
      expect(error).toBeNull();
    });
  });

  describe('ARRAY_SIZE_LT', () => {
    it('aceita número', () => {
      const error = validateValueByOperator('ARRAY_SIZE_LT', '100');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES MATEMÁTICOS
// ============================================
describe('Operadores Matemáticos', () => {
  describe('MOD_EQ', () => {
    it('aceita formato divisor,resto', () => {
      const error = validateValueByOperator('MOD_EQ', '100,0');
      expect(error).toBeNull();
    });

    it('aceita diferentes valores', () => {
      const error = validateValueByOperator('MOD_EQ', '7,3');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('MOD_EQ', '');
      expect(error).not.toBeNull();
    });
  });

  describe('MOD_NEQ', () => {
    it('aceita formato divisor,resto', () => {
      const error = validateValueByOperator('MOD_NEQ', '2,1');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES GEO
// ============================================
describe('Operadores Geo', () => {
  describe('GEO_DISTANCE_LT', () => {
    it('aceita formato lat,lon,km', () => {
      const error = validateValueByOperator('GEO_DISTANCE_LT', '-23.55,-46.63,100');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('GEO_DISTANCE_LT', '');
      expect(error).not.toBeNull();
    });
  });

  describe('GEO_DISTANCE_GT', () => {
    it('aceita formato lat,lon,km', () => {
      const error = validateValueByOperator('GEO_DISTANCE_GT', '-22.90,-43.17,50');
      expect(error).toBeNull();
    });
  });

  describe('GEO_IN_POLYGON', () => {
    it('aceita nome de região', () => {
      const error = validateValueByOperator('GEO_IN_POLYGON', 'BRASIL');
      expect(error).toBeNull();
    });

    it('aceita nome de estado', () => {
      const error = validateValueByOperator('GEO_IN_POLYGON', 'SAO_PAULO');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES VELOCITY
// ============================================
describe('Operadores Velocity', () => {
  describe('VELOCITY_COUNT_GT', () => {
    it('aceita formato keyType,minutes,threshold', () => {
      const error = validateValueByOperator('VELOCITY_COUNT_GT', 'PAN,60,5');
      expect(error).toBeNull();
    });

    it('aceita diferentes key types', () => {
      const error = validateValueByOperator('VELOCITY_COUNT_GT', 'CUSTOMER,1440,10');
      expect(error).toBeNull();
    });

    it('rejeita vazio', () => {
      const error = validateValueByOperator('VELOCITY_COUNT_GT', '');
      expect(error).not.toBeNull();
    });
  });

  describe('VELOCITY_COUNT_LT', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('VELOCITY_COUNT_LT', 'DEVICE,30,3');
      expect(error).toBeNull();
    });
  });

  describe('VELOCITY_SUM_GT', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('VELOCITY_SUM_GT', 'PAN,60,10000');
      expect(error).toBeNull();
    });
  });

  describe('VELOCITY_SUM_LT', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('VELOCITY_SUM_LT', 'CUSTOMER,1440,50000');
      expect(error).toBeNull();
    });
  });

  describe('VELOCITY_AVG_GT', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('VELOCITY_AVG_GT', 'PAN,60,500');
      expect(error).toBeNull();
    });
  });

  describe('VELOCITY_AVG_LT', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('VELOCITY_AVG_LT', 'MERCHANT,120,100');
      expect(error).toBeNull();
    });
  });

  describe('VELOCITY_DISTINCT_GT', () => {
    it('aceita formato keyType,minutes,distinctField,threshold', () => {
      const error = validateValueByOperator('VELOCITY_DISTINCT_GT', 'PAN,1440,MERCHANTS,3');
      expect(error).toBeNull();
    });
  });

  describe('VELOCITY_DISTINCT_LT', () => {
    it('aceita formato válido', () => {
      const error = validateValueByOperator('VELOCITY_DISTINCT_LT', 'CUSTOMER,60,DEVICES,5');
      expect(error).toBeNull();
    });
  });
});

// ============================================
// OPERADORES LEGACY
// ============================================
describe('Operadores Legacy', () => {
  describe('NE (legacy para NEQ)', () => {
    it('aceita valor', () => {
      const error = validateValueByOperator('NE', 'test');
      expect(error).toBeNull();
    });
  });

  describe('MATCHES_REGEX (legacy para REGEX)', () => {
    it('aceita regex válida', () => {
      const error = validateValueByOperator('MATCHES_REGEX', '^[0-9]+$');
      expect(error).toBeNull();
    });

    it('rejeita regex inválida', () => {
      const error = validateValueByOperator('MATCHES_REGEX', '[invalid');
      expect(error).not.toBeNull();
    });

    it('rejeita regex ReDoS', () => {
      const error = validateValueByOperator('MATCHES_REGEX', '(a+)+');
      expect(error).not.toBeNull();
    });
  });

  describe('Operadores simbólicos', () => {
    it('== aceita valor', () => {
      const error = validateValueByOperator('==', '100');
      expect(error).toBeNull();
    });

    it('!= aceita valor', () => {
      const error = validateValueByOperator('!=', '100');
      expect(error).toBeNull();
    });

    it('> aceita número', () => {
      const error = validateValueByOperator('>', '50');
      expect(error).toBeNull();
    });

    it('< aceita número', () => {
      const error = validateValueByOperator('<', '50');
      expect(error).toBeNull();
    });

    it('>= aceita número', () => {
      const error = validateValueByOperator('>=', '100');
      expect(error).toBeNull();
    });

    it('<= aceita número', () => {
      const error = validateValueByOperator('<=', '100');
      expect(error).toBeNull();
    });

    it('> rejeita texto para campo number', () => {
      const error = validateValueByOperator('>', 'abc', 'number');
      expect(error).not.toBeNull();
    });
  });
});

// ============================================
// TESTES DE SCHEMA DE CONDIÇÃO COMPLETO
// ============================================
describe('Condition Schema - Validação Completa', () => {
  it('aceita condição válida com EQ', () => {
    const result = conditionSchema.safeParse({
      field: 'transactionAmount',
      operator: 'EQ',
      value: '100',
    });
    expect(result.success).toBe(true);
  });

  it('aceita condição válida com BETWEEN', () => {
    const result = conditionSchema.safeParse({
      field: 'transactionAmount',
      operator: 'BETWEEN',
      value: '100,500',
    });
    expect(result.success).toBe(true);
  });

  it('aceita condição válida com IN', () => {
    const result = conditionSchema.safeParse({
      field: 'mcc',
      operator: 'IN',
      value: '5411,5812,5912',
    });
    expect(result.success).toBe(true);
  });

  it('aceita condição válida com REGEX', () => {
    const result = conditionSchema.safeParse({
      field: 'merchantName',
      operator: 'REGEX',
      value: '^[A-Z]+$',
    });
    expect(result.success).toBe(true);
  });

  it('aceita condição unária IS_NULL sem valor', () => {
    const result = conditionSchema.safeParse({
      field: 'optionalField',
      operator: 'IS_NULL',
      value: '',
    });
    expect(result.success).toBe(true);
  });

  it('rejeita campo vazio', () => {
    const result = conditionSchema.safeParse({
      field: '',
      operator: 'EQ',
      value: '100',
    });
    expect(result.success).toBe(false);
  });

  it('rejeita operador inválido', () => {
    const result = conditionSchema.safeParse({
      field: 'test',
      operator: 'INVALID_OP',
      value: '100',
    });
    expect(result.success).toBe(false);
  });

  it('rejeita valor vazio para operador não-unário', () => {
    const result = conditionSchema.safeParse({
      field: 'test',
      operator: 'EQ',
      value: '',
    });
    expect(result.success).toBe(false);
  });

  it('rejeita BETWEEN com formato inválido', () => {
    const result = conditionSchema.safeParse({
      field: 'amount',
      operator: 'BETWEEN',
      value: '100', // falta segundo valor
    });
    expect(result.success).toBe(false);
  });

  it('rejeita IN com lista vazia', () => {
    const result = conditionSchema.safeParse({
      field: 'category',
      operator: 'IN',
      value: '',
    });
    expect(result.success).toBe(false);
  });

  it('rejeita REGEX inválida', () => {
    const result = conditionSchema.safeParse({
      field: 'pattern',
      operator: 'REGEX',
      value: '[invalid',
    });
    expect(result.success).toBe(false);
  });
});

// ============================================
// CONTAGEM DE OPERADORES
// ============================================
describe('Cobertura de Operadores', () => {
  const allOperators = [
    ...ALL_OPERATORS.basic,
    ...ALL_OPERATORS.lists,
    ...ALL_OPERATORS.strings,
    ...ALL_OPERATORS.unary,
    ...ALL_OPERATORS.fieldComparison,
    ...ALL_OPERATORS.dateTime,
    ...ALL_OPERATORS.array,
    ...ALL_OPERATORS.math,
    ...ALL_OPERATORS.geo,
    ...ALL_OPERATORS.velocity,
  ];

  it('deve ter pelo menos 50 operadores principais', () => {
    expect(allOperators.length).toBeGreaterThanOrEqual(50);
  });

  it('total de operadores incluindo legacy deve ser 52+', () => {
    const total = allOperators.length + ALL_OPERATORS.legacy.length;
    expect(total).toBeGreaterThanOrEqual(52);
  });

  // Função auxiliar para obter valor de teste apropriado para cada operador
  const getTestValue = (op: string): string => {
    if (op.includes('NULL') || op.includes('TRUE') || op.includes('FALSE')) return '';
    if (op === 'BETWEEN' || op === 'NOT_BETWEEN') return '10,100';
    if (op === 'DATE_BETWEEN') return '2024-01-01,2024-12-31';
    if (op === 'TIME_BETWEEN') return '08:00:00,18:00:00';
    if (op === 'DATE_BEFORE' || op === 'DATE_AFTER') return '2024-12-31';
    if (op === 'TIME_BEFORE' || op === 'TIME_AFTER') return '23:59:59';
    if (op === 'MOD_EQ' || op === 'MOD_NEQ') return '100,0';
    if (op === 'GEO_DISTANCE_LT' || op === 'GEO_DISTANCE_GT') return '-23.55,-46.63,100';
    if (op === 'GEO_IN_POLYGON') return 'BRASIL';
    if (op === 'VELOCITY_DISTINCT_GT' || op === 'VELOCITY_DISTINCT_LT') return 'PAN,1440,MERCHANTS,3';
    if (op.startsWith('VELOCITY_')) return 'PAN,60,5';
    if (op === 'REGEX' || op === 'NOT_REGEX' || op === 'MATCHES_REGEX') return '[A-Z]+';
    return 'testValue';
  };

  // Teste dinâmico para garantir que todos os operadores são aceitos pelo schema
  allOperators.forEach(operator => {
    it(`operador ${operator} deve ser aceito pelo schema`, () => {
      const result = conditionSchema.safeParse({
        field: 'testField',
        operator: operator,
        value: getTestValue(operator),
      });
      // Se falhar, mostrar detalhes
      if (!result.success) {
        console.log(`Operador ${operator} falhou:`, result.error.issues);
      }
      expect(result.success).toBe(true);
    });
  });
});
