/**
 * Testes adversariais para validação do schema de regras
 * Red Team - Auditoria 10.000x
 */

import { describe, expect, it } from 'vitest';
import {
  ruleFormSchema,
  validateValueByOperator,
  validateValueForFieldType,
  conditionSchema,
  MAX_CONDITIONS,
} from './schema';

describe('Schema de Regras - Testes Adversariais', () => {
  // ============================================
  // STR - Strings Maliciosas
  // ============================================
  describe('STR - Strings Maliciosas', () => {
    it('STR-01: ruleName vazio deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: '',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.issues.some(i => i.path.includes('ruleName'))).toBe(true);
      }
    });

    it('STR-02: ruleName com apenas whitespace deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: '   ',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-03: ruleName com unicode invisível deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: '\u200B\u200B\u200B',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-04: ruleName muito longo deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'A'.repeat(101),
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-05: ruleName lowercase deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'test_rule',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-06: ruleName começando com número deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: '123_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-07: ruleName com hífen deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'RULE-NAME',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-08: ruleName com espaço deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'RULE NAME',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('STR-11: description muito longa deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        description: 'A'.repeat(501),
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });
  });

  // ============================================
  // NUM - Números Extremos
  // ============================================
  describe('NUM - Números Extremos', () => {
    it('NUM-01: threshold negativo deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: -1,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('NUM-02: threshold > 1000 deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 1001,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('NUM-05: threshold decimal deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 1.5,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('NUM-07: weight negativo deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: -1,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('NUM-08: weight > 100 deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 101,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });
  });

  // ============================================
  // REG - Regex Maliciosas
  // ============================================
  describe('REG - Regex Maliciosas', () => {
    it('REG-01: regex inválida deve retornar erro', () => {
      const error = validateValueByOperator('MATCHES_REGEX', '[');
      expect(error).not.toBeNull();
      expect(error).toContain('inválida');
    });

    it('REG-02: regex ReDoS deve ser REJEITADA (com proteção ReDoS)', () => {
      // NOTA: Agora há proteção contra ReDoS via regexValidator
      const error = validateValueByOperator('MATCHES_REGEX', '(a+)+');
      // Deve falhar pois é regex perigosa (catastrophic backtracking)
      expect(error).not.toBeNull();
      expect(error).toContain('perigosa');
    });

    it('REG-03: regex match-all deve ser aceita', () => {
      const error = validateValueByOperator('MATCHES_REGEX', '.*');
      expect(error).toBeNull();
    });

    it('REG-06: regex com null byte deve ser tratada', () => {
      const error = validateValueByOperator('MATCHES_REGEX', '\\x00');
      // Deve passar pois é regex válida
      expect(error).toBeNull();
    });
  });

  // ============================================
  // BET - Operador BETWEEN
  // ============================================
  describe('BET - Operador BETWEEN', () => {
    it('BET-01: BETWEEN invertido deve falhar para números', () => {
      const error = validateValueByOperator('BETWEEN', '100,10', 'number');
      expect(error).not.toBeNull();
      expect(error).toContain('primeiro valor deve ser menor');
    });

    it('BET-02: BETWEEN com 1 valor deve falhar', () => {
      const error = validateValueByOperator('BETWEEN', '10');
      expect(error).not.toBeNull();
      expect(error).toContain('valor1,valor2');
    });

    it('BET-03: BETWEEN com 3 valores deve falhar', () => {
      const error = validateValueByOperator('BETWEEN', '10,20,30');
      expect(error).not.toBeNull();
    });

    it('BET-04: BETWEEN não numérico para campo number deve falhar', () => {
      const error = validateValueByOperator('BETWEEN', 'abc,def', 'number');
      expect(error).not.toBeNull();
      expect(error).toContain('números');
    });

    it('BET-05: BETWEEN formato alternativo (..) deve aceitar', () => {
      const error = validateValueByOperator('BETWEEN', '10..20', 'number');
      expect(error).toBeNull();
    });

    it('BET-06: BETWEEN com negativo deve aceitar', () => {
      const error = validateValueByOperator('BETWEEN', '-10,10', 'number');
      expect(error).toBeNull();
    });

    it('BET-07: BETWEEN com espaço deve aceitar (trim)', () => {
      const error = validateValueByOperator('BETWEEN', '10, 20', 'number');
      expect(error).toBeNull();
    });

    it('BET-08: BETWEEN vazio deve falhar', () => {
      const error = validateValueByOperator('BETWEEN', ',');
      expect(error).not.toBeNull();
    });
  });

  // ============================================
  // IN - Operador IN/NOT_IN
  // ============================================
  describe('IN - Operador IN/NOT_IN', () => {
    it('IN-01: IN vazio deve falhar', () => {
      const error = validateValueByOperator('IN', '');
      expect(error).not.toBeNull();
    });

    it('IN-02: IN com 1 item deve aceitar', () => {
      const error = validateValueByOperator('IN', '1');
      expect(error).toBeNull();
    });

    it('IN-04: IN com vazio no meio deve filtrar', () => {
      const error = validateValueByOperator('IN', '1,,2');
      // Deve aceitar pois filtra vazios
      expect(error).toBeNull();
    });

    it('IN-05: IN formato array deve aceitar', () => {
      const error = validateValueByOperator('IN', '[1,2,3]');
      expect(error).toBeNull();
    });

    it('IN-06: IN strings deve aceitar', () => {
      const error = validateValueByOperator('IN', "['a','b']");
      expect(error).toBeNull();
    });

    it('IN-09: IN não numérico para campo number deve falhar', () => {
      const error = validateValueByOperator('IN', 'a,b,c', 'number');
      expect(error).not.toBeNull();
      expect(error).toContain('numérico');
    });
  });

  // ============================================
  // UNA - Operadores Unários
  // ============================================
  describe('UNA - Operadores Unários', () => {
    it('UNA-01: IS_NULL com valor deve ignorar', () => {
      const error = validateValueByOperator('IS_NULL', 'qualquer');
      expect(error).toBeNull();
    });

    it('UNA-02: IS_NOT_NULL com valor deve ignorar', () => {
      const error = validateValueByOperator('IS_NOT_NULL', 'qualquer');
      expect(error).toBeNull();
    });

    it('UNA-03: IS_TRUE com valor deve ignorar', () => {
      const error = validateValueByOperator('IS_TRUE', 'qualquer');
      expect(error).toBeNull();
    });

    it('UNA-04: IS_FALSE com valor deve ignorar', () => {
      const error = validateValueByOperator('IS_FALSE', 'qualquer');
      expect(error).toBeNull();
    });

    it('UNA-05: IS_NULL sem valor deve aceitar', () => {
      const error = validateValueByOperator('IS_NULL', '');
      expect(error).toBeNull();
    });
  });

  // ============================================
  // FLD - Campos e Tipos
  // ============================================
  describe('FLD - Campos e Tipos', () => {
    it('FLD-01: campo vazio deve falhar', () => {
      const result = conditionSchema.safeParse({
        field: '',
        operator: 'EQ',
        value: '10',
      });
      expect(result.success).toBe(false);
    });

    it('FLD-03: valor não numérico para comparação numérica deve falhar', () => {
      const error = validateValueByOperator('GT', 'abc', 'number');
      expect(error).not.toBeNull();
      expect(error).toContain('número');
    });
  });

  // ============================================
  // JSON - Parameters
  // ============================================
  describe('JSON - Parameters', () => {
    it('JSON-01: JSON inválido deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        parameters: '{',
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });

    it('JSON-02: null deve aceitar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        parameters: 'null',
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(true);
    });

    it('JSON-03: array vazio deve aceitar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        parameters: '[]',
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(true);
    });

    it('JSON-04: objeto vazio deve aceitar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        parameters: '{}',
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(true);
    });

    it('JSON-05: aspas simples deve falhar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        parameters: "{'a':1}",
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(false);
    });
  });

  // ============================================
  // Casos Válidos (Sanity Check)
  // ============================================
  describe('Casos Válidos - Sanity Check', () => {
    it('Regra válida completa deve passar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'HIGH_AMOUNT_RULE',
        description: 'Regra para valores altos',
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 100,
        weight: 50,
        enabled: true,
        parameters: '{"key": "value"}',
        conditions: [
          { field: 'transactionAmount', operator: 'GT', value: '10000' },
        ],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(true);
    });

    it('Regra mínima válida deve passar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'MIN_RULE',
        ruleType: 'SECURITY',
        classification: 'APPROVED',
        threshold: 0,
        weight: 0,
        enabled: false,
        conditions: [],
        logicOperator: 'OR',
      });
      expect(result.success).toBe(true);
    });
  });

  // ============================================
  // Limites
  // ============================================
  describe('Limites', () => {
    it('MAX_CONDITIONS deve ser 20', () => {
      expect(MAX_CONDITIONS).toBe(20);
    });

    it('ruleName no limite (100 chars) deve passar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'A'.repeat(100),
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(true);
    });

    it('description no limite (500 chars) deve passar', () => {
      const result = ruleFormSchema.safeParse({
        ruleName: 'VALID_RULE',
        description: 'A'.repeat(500),
        ruleType: 'SECURITY',
        classification: 'SUSPICIOUS',
        threshold: 0,
        weight: 50,
        enabled: true,
        conditions: [],
        logicOperator: 'AND',
      });
      expect(result.success).toBe(true);
    });
  });
});

// ============================================
// Testes adicionais para funções auxiliares
// ============================================
import { formatValueForPreview, getPlaceholderForOperator } from './schema';

describe('formatValueForPreview', () => {
  it('formata valor monetário para transactionAmount', () => {
    const result = formatValueForPreview('100000', 'transactionAmount');
    expect(result).toMatch(/R\$/);
    expect(result).toContain('1.000');
  });

  it('formata horário para transactionTime', () => {
    const result = formatValueForPreview('143025', 'transactionTime');
    expect(result).toBe('14:30:25');
  });

  it('formata data para transactionDate', () => {
    const result = formatValueForPreview('20241231', 'transactionDate');
    expect(result).toBe('31/12/2024');
  });

  it('retorna valor original para campos não especiais', () => {
    const result = formatValueForPreview('test', 'otherField');
    expect(result).toBe('test');
  });

  it('retorna valor original para transactionTime com formato inválido', () => {
    const result = formatValueForPreview('invalid', 'transactionTime');
    expect(result).toBe('invalid');
  });

  it('retorna valor original para transactionDate com formato inválido', () => {
    const result = formatValueForPreview('invalid', 'transactionDate');
    expect(result).toBe('invalid');
  });
});

describe('getPlaceholderForOperator', () => {
  it('retorna placeholder para IN', () => {
    const result = getPlaceholderForOperator('IN');
    expect(result).toContain('valor1,valor2');
  });

  it('retorna placeholder para NOT_IN', () => {
    const result = getPlaceholderForOperator('NOT_IN');
    expect(result).toContain('valor1,valor2');
  });

  it('retorna placeholder para BETWEEN', () => {
    const result = getPlaceholderForOperator('BETWEEN');
    expect(result).toContain('10,100');
  });

  it('retorna placeholder para NOT_BETWEEN', () => {
    const result = getPlaceholderForOperator('NOT_BETWEEN');
    expect(result).toContain('10,100');
  });

  it('retorna placeholder para MATCHES_REGEX', () => {
    const result = getPlaceholderForOperator('MATCHES_REGEX');
    expect(result).toContain('regular'); // Expressão regular
  });

  it('retorna placeholder para REGEX', () => {
    const result = getPlaceholderForOperator('REGEX');
    expect(result).toContain('regular'); // Expressão regular
  });

  it('retorna placeholder para CONTAINS', () => {
    const result = getPlaceholderForOperator('CONTAINS');
    expect(result).toContain('texto');
  });

  it('retorna placeholder para NOT_CONTAINS', () => {
    const result = getPlaceholderForOperator('NOT_CONTAINS');
    expect(result).toContain('texto');
  });

  it('retorna placeholder para STARTS_WITH', () => {
    const result = getPlaceholderForOperator('STARTS_WITH');
    expect(result).toContain('prefixo');
  });

  it('retorna placeholder para ENDS_WITH', () => {
    const result = getPlaceholderForOperator('ENDS_WITH');
    expect(result).toContain('sufixo');
  });

  it('retorna placeholder para IS_NULL', () => {
    const result = getPlaceholderForOperator('IS_NULL');
    expect(result).toContain('aplicável'); // Com acento
  });

  it('retorna placeholder para IS_NOT_NULL', () => {
    const result = getPlaceholderForOperator('IS_NOT_NULL');
    expect(result).toContain('aplicável'); // Com acento
  });

  it('retorna placeholder para NOT_NULL', () => {
    const result = getPlaceholderForOperator('NOT_NULL');
    expect(result).toContain('aplicável'); // Com acento
  });

  it('retorna placeholder para IS_TRUE', () => {
    const result = getPlaceholderForOperator('IS_TRUE');
    expect(result).toContain('aplicável'); // Com acento
  });

  it('retorna placeholder para IS_FALSE', () => {
    const result = getPlaceholderForOperator('IS_FALSE');
    expect(result).toContain('aplicável'); // Com acento
  });

  it('retorna placeholder padrão para operador desconhecido', () => {
    const result = getPlaceholderForOperator('EQ');
    expect(result).toContain('100');
  });
});

describe('validateValueForFieldType - cobertura adicional', () => {
  it('valida campo number com operador IN', () => {
    const result = validateValueForFieldType('1,2,3', 'number', 'IN');
    expect(result.valid).toBe(true);
  });

  it('falha para campo number com operador IN e valores inválidos', () => {
    const result = validateValueForFieldType('a,b,c', 'number', 'IN');
    expect(result.valid).toBe(false);
  });

  it('valida campo number com operador BETWEEN', () => {
    const result = validateValueForFieldType('10,20', 'number', 'BETWEEN');
    expect(result.valid).toBe(true);
  });

  it('falha para campo number com operador BETWEEN e valores inválidos', () => {
    const result = validateValueForFieldType('a,b', 'number', 'BETWEEN');
    expect(result.valid).toBe(false);
  });

  it('valida campo boolean', () => {
    const result = validateValueForFieldType('true', 'boolean', 'EQ');
    expect(result.valid).toBe(true);
  });

  it('valida campo boolean com yes', () => {
    const result = validateValueForFieldType('yes', 'boolean', 'EQ');
    expect(result.valid).toBe(true);
  });

  it('falha para campo boolean com valor inválido', () => {
    const result = validateValueForFieldType('maybe', 'boolean', 'EQ');
    expect(result.valid).toBe(false);
  });

  it('valida campo date com formato YYYYMMDD', () => {
    const result = validateValueForFieldType('20241231', 'date', 'EQ');
    expect(result.valid).toBe(true);
  });

  it('valida campo date com formato ISO', () => {
    const result = validateValueForFieldType('2024-12-31', 'date', 'EQ');
    expect(result.valid).toBe(true);
  });

  it('falha para campo date com formato inválido', () => {
    const result = validateValueForFieldType('invalid-date', 'date', 'EQ');
    expect(result.valid).toBe(false);
  });

  it('valida campo number simples', () => {
    const result = validateValueForFieldType('123', 'number', 'EQ');
    expect(result.valid).toBe(true);
  });

  it('falha para campo number com valor não numérico', () => {
    const result = validateValueForFieldType('abc', 'number', 'EQ');
    expect(result.valid).toBe(false);
  });
});
