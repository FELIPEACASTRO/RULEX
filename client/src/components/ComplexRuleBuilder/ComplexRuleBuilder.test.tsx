/**
 * Testes para o ComplexRuleBuilder - Types e Helpers
 */

import { describe, it, expect } from 'vitest';
import {
  createEmptyRule,
  createEmptyGroup,
  createEmptyCondition,
  getOperatorInfo,
  getOperatorsForType,
  LOGIC_OPERATORS,
  COMPARISON_OPERATORS,
  VALUE_TYPES,
  RULE_STATUSES,
  DECISION_TYPES,
  type ComplexRule,
  type ConditionGroup,
  type Condition,
  type LogicOperator,
  type ComparisonOperator,
  type ValueType,
} from './types';

describe('ComplexRuleBuilder Types', () => {
  describe('createEmptyRule', () => {
    it('creates a valid empty rule', () => {
      const rule = createEmptyRule();
      expect(rule.key).toBe('');
      expect(rule.title).toBe('');
      expect(rule.status).toBe('DRAFT');
      expect(rule.decision).toBe('SUSPEITA_DE_FRAUDE');
      expect(rule.enabled).toBe(true);
      expect(rule.rootConditionGroup).toBeDefined();
      expect(rule.rootConditionGroup.logicOperator).toBe('AND');
    });
  });

  describe('createEmptyGroup', () => {
    it('creates a valid empty group with default AND operator', () => {
      const group = createEmptyGroup();
      expect(group.logicOperator).toBe('AND');
      expect(group.conditions).toEqual([]);
      expect(group.children).toEqual([]);
      expect(group.enabled).toBe(true);
      expect(group.id).toBeDefined();
    });

    it('creates a group with specified operator', () => {
      const group = createEmptyGroup('OR');
      expect(group.logicOperator).toBe('OR');
    });

    it('creates a group with XOR operator', () => {
      const group = createEmptyGroup('XOR');
      expect(group.logicOperator).toBe('XOR');
    });
  });

  describe('createEmptyCondition', () => {
    it('creates a valid empty condition', () => {
      const condition = createEmptyCondition();
      expect(condition.fieldName).toBe('');
      expect(condition.operator).toBe('EQ');
      expect(condition.valueType).toBe('STRING');
      expect(condition.valueSingle).toBe('');
      expect(condition.enabled).toBe(true);
      expect(condition.id).toBeDefined();
    });
  });

  describe('getOperatorInfo', () => {
    it('returns info for EQ operator', () => {
      const info = getOperatorInfo('EQ');
      expect(info).toBeDefined();
      expect(info?.label).toBe('=');
      expect(info?.requiresValue).toBe(true);
    });

    it('returns info for IS_NULL operator', () => {
      const info = getOperatorInfo('IS_NULL');
      expect(info).toBeDefined();
      expect(info?.requiresValue).toBe(false);
    });

    it('returns info for BETWEEN operator', () => {
      const info = getOperatorInfo('BETWEEN');
      expect(info).toBeDefined();
      expect(info?.requiresSecondValue).toBe(true);
    });

    it('returns info for FIELD_EQ operator', () => {
      const info = getOperatorInfo('FIELD_EQ');
      expect(info).toBeDefined();
      expect(info?.requiresFieldRef).toBe(true);
    });

    it('returns undefined for unknown operator', () => {
      const info = getOperatorInfo('UNKNOWN' as any);
      expect(info).toBeUndefined();
    });
  });

  describe('getOperatorsForType', () => {
    it('returns operators for STRING type', () => {
      const ops = getOperatorsForType('STRING');
      expect(ops.length).toBeGreaterThan(0);
      expect(ops.some(op => op.value === 'CONTAINS')).toBe(true);
      expect(ops.some(op => op.value === 'STARTS_WITH')).toBe(true);
    });

    it('returns operators for NUMBER type', () => {
      const ops = getOperatorsForType('NUMBER');
      expect(ops.length).toBeGreaterThan(0);
      expect(ops.some(op => op.value === 'GT')).toBe(true);
      expect(ops.some(op => op.value === 'BETWEEN')).toBe(true);
    });

    it('returns operators for BOOLEAN type', () => {
      const ops = getOperatorsForType('BOOLEAN');
      expect(ops.length).toBeGreaterThan(0);
      expect(ops.some(op => op.value === 'IS_TRUE')).toBe(true);
      expect(ops.some(op => op.value === 'IS_FALSE')).toBe(true);
    });

    it('returns operators for DATE type', () => {
      const ops = getOperatorsForType('DATE');
      expect(ops.length).toBeGreaterThan(0);
      expect(ops.some(op => op.value === 'DATE_BEFORE')).toBe(true);
    });
  });

  describe('Constants', () => {
    it('LOGIC_OPERATORS has all expected operators', () => {
      expect(LOGIC_OPERATORS.length).toBe(6);
      expect(LOGIC_OPERATORS.map(op => op.value)).toContain('AND');
      expect(LOGIC_OPERATORS.map(op => op.value)).toContain('OR');
      expect(LOGIC_OPERATORS.map(op => op.value)).toContain('NOT');
      expect(LOGIC_OPERATORS.map(op => op.value)).toContain('XOR');
      expect(LOGIC_OPERATORS.map(op => op.value)).toContain('NAND');
      expect(LOGIC_OPERATORS.map(op => op.value)).toContain('NOR');
    });

    it('COMPARISON_OPERATORS has many operators', () => {
      expect(COMPARISON_OPERATORS.length).toBeGreaterThan(30);
    });

    it('VALUE_TYPES has all expected types', () => {
      expect(VALUE_TYPES.length).toBe(12); // Inclui GEO_POINT e GEO_POLYGON
      expect(VALUE_TYPES.map(t => t.value)).toContain('STRING');
      expect(VALUE_TYPES.map(t => t.value)).toContain('NUMBER');
      expect(VALUE_TYPES.map(t => t.value)).toContain('BOOLEAN');
      expect(VALUE_TYPES.map(t => t.value)).toContain('GEO_POINT');
      expect(VALUE_TYPES.map(t => t.value)).toContain('GEO_POLYGON');
    });

    it('RULE_STATUSES has all expected statuses', () => {
      expect(RULE_STATUSES.length).toBe(5);
      expect(RULE_STATUSES.map(s => s.value)).toContain('DRAFT');
      expect(RULE_STATUSES.map(s => s.value)).toContain('PUBLISHED');
      expect(RULE_STATUSES.map(s => s.value)).toContain('DEPRECATED');
    });

    it('DECISION_TYPES has all expected decisions', () => {
      expect(DECISION_TYPES.length).toBe(3);
      expect(DECISION_TYPES.map(d => d.value)).toContain('APROVADO');
      expect(DECISION_TYPES.map(d => d.value)).toContain('FRAUDE');
    });
  });
});

describe('ComplexRuleBuilder Helpers - Additional Tests', () => {
  describe('createEmptyRule - detailed', () => {
    it('has correct priority default', () => {
      const rule = createEmptyRule();
      expect(rule.priority).toBe(50);
    });

    it('has correct severity default', () => {
      const rule = createEmptyRule();
      expect(rule.severity).toBe(50);
    });

    it('rootConditionGroup has empty conditions', () => {
      const rule = createEmptyRule();
      expect(rule.rootConditionGroup.conditions).toEqual([]);
    });

    it('rootConditionGroup has empty children', () => {
      const rule = createEmptyRule();
      expect(rule.rootConditionGroup.children).toEqual([]);
    });
  });

  describe('createEmptyGroup - all operators', () => {
    it('creates NOT group', () => {
      const group = createEmptyGroup('NOT');
      expect(group.logicOperator).toBe('NOT');
    });

    it('creates NAND group', () => {
      const group = createEmptyGroup('NAND');
      expect(group.logicOperator).toBe('NAND');
    });

    it('creates NOR group', () => {
      const group = createEmptyGroup('NOR');
      expect(group.logicOperator).toBe('NOR');
    });

    it('generates unique IDs', () => {
      const group1 = createEmptyGroup();
      const group2 = createEmptyGroup();
      expect(group1.id).not.toBe(group2.id);
    });
  });

  describe('createEmptyCondition - detailed', () => {
    it('generates unique IDs', () => {
      const cond1 = createEmptyCondition();
      const cond2 = createEmptyCondition();
      expect(cond1.id).not.toBe(cond2.id);
    });

    it('has no negate by default', () => {
      const cond = createEmptyCondition();
      expect(cond.negate).toBeUndefined();
    });
  });

  describe('getOperatorInfo - more operators', () => {
    it('returns info for GT operator', () => {
      const info = getOperatorInfo('GT');
      expect(info?.label).toBe('>');
    });

    it('returns info for IN operator', () => {
      const info = getOperatorInfo('IN');
      expect(info?.category).toBe('list');
    });

    it('returns info for CONTAINS operator', () => {
      const info = getOperatorInfo('CONTAINS');
      expect(info?.category).toBe('string');
    });

    it('returns info for DATE_BEFORE operator', () => {
      const info = getOperatorInfo('DATE_BEFORE');
      expect(info?.category).toBe('date');
    });

    it('returns info for ARRAY_CONTAINS operator', () => {
      const info = getOperatorInfo('ARRAY_CONTAINS');
      expect(info?.category).toBe('array');
    });

    it('returns info for MOD_EQ operator', () => {
      const info = getOperatorInfo('MOD_EQ');
      expect(info?.category).toBe('math');
    });
  });

  describe('getOperatorsForType - more types', () => {
    it('returns operators for ARRAY_STRING type', () => {
      const ops = getOperatorsForType('ARRAY_STRING');
      expect(ops.some(op => op.value === 'ARRAY_CONTAINS')).toBe(true);
    });

    it('returns operators for TIME type', () => {
      const ops = getOperatorsForType('TIME');
      expect(ops.some(op => op.value === 'TIME_BEFORE')).toBe(true);
    });

    it('returns operators for DATETIME type', () => {
      const ops = getOperatorsForType('DATETIME');
      expect(ops.length).toBeGreaterThan(0);
    });
  });

  describe('LOGIC_OPERATORS details', () => {
    it('each operator has a label', () => {
      LOGIC_OPERATORS.forEach(op => {
        expect(op.label).toBeDefined();
        expect(op.label.length).toBeGreaterThan(0);
      });
    });

    it('each operator has a description', () => {
      LOGIC_OPERATORS.forEach(op => {
        expect(op.description).toBeDefined();
      });
    });

    it('each operator has a color', () => {
      LOGIC_OPERATORS.forEach(op => {
        expect(op.color).toMatch(/^bg-/);
      });
    });
  });

  describe('COMPARISON_OPERATORS details', () => {
    it('each operator has applicableTypes', () => {
      COMPARISON_OPERATORS.forEach(op => {
        expect(op.applicableTypes).toBeDefined();
        expect(op.applicableTypes.length).toBeGreaterThan(0);
      });
    });

    it('each operator has a category', () => {
      COMPARISON_OPERATORS.forEach(op => {
        expect(op.category).toBeDefined();
      });
    });
  });

  describe('VALUE_TYPES details', () => {
    it('each type has a label', () => {
      VALUE_TYPES.forEach(type => {
        expect(type.label).toBeDefined();
        expect(type.label.length).toBeGreaterThan(0);
      });
    });
  });

  describe('RULE_STATUSES details', () => {
    it('each status has a color', () => {
      RULE_STATUSES.forEach(status => {
        expect(status.color).toMatch(/^bg-/);
      });
    });
  });

  describe('DECISION_TYPES details', () => {
    it('each decision has a color', () => {
      DECISION_TYPES.forEach(decision => {
        expect(decision.color).toMatch(/^bg-/);
      });
    });
  });

  describe('Type exports', () => {
    it('ComplexRule type can be used', () => {
      const rule: ComplexRule = createEmptyRule();
      expect(rule.key).toBeDefined();
    });

    it('ConditionGroup type can be used', () => {
      const group: ConditionGroup = createEmptyGroup();
      expect(group.logicOperator).toBeDefined();
    });

    it('Condition type can be used', () => {
      const condition: Condition = createEmptyCondition();
      expect(condition.operator).toBeDefined();
    });

    it('LogicOperator type values are valid', () => {
      const operators: LogicOperator[] = ['AND', 'OR', 'NOT', 'XOR', 'NAND', 'NOR'];
      operators.forEach(op => {
        const group = createEmptyGroup(op);
        expect(group.logicOperator).toBe(op);
      });
    });

    it('ComparisonOperator type values are valid', () => {
      const operators: ComparisonOperator[] = ['EQ', 'NEQ', 'GT', 'LT', 'IN', 'BETWEEN'];
      operators.forEach(op => {
        const info = getOperatorInfo(op);
        expect(info).toBeDefined();
      });
    });

    it('ValueType type values are valid', () => {
      const types: ValueType[] = ['STRING', 'NUMBER', 'BOOLEAN', 'DATE'];
      types.forEach(type => {
        const ops = getOperatorsForType(type);
        expect(ops.length).toBeGreaterThan(0);
      });
    });
  });

  describe('Edge cases', () => {
    it('getOperatorsForType returns empty for invalid type', () => {
      const ops = getOperatorsForType('INVALID' as ValueType);
      expect(ops).toEqual([]);
    });

    it('createEmptyRule generates unique IDs for nested structures', () => {
      const rule1 = createEmptyRule();
      const rule2 = createEmptyRule();
      expect(rule1.rootConditionGroup.id).not.toBe(rule2.rootConditionGroup.id);
    });

    it('COMPARISON_OPERATORS covers all categories', () => {
      const categories = new Set(COMPARISON_OPERATORS.map(op => op.category));
      expect(categories.has('basic')).toBe(true);
      expect(categories.has('list')).toBe(true);
      expect(categories.has('range')).toBe(true);
      expect(categories.has('string')).toBe(true);
      expect(categories.has('null')).toBe(true);
      expect(categories.has('boolean')).toBe(true);
      expect(categories.has('field')).toBe(true);
      expect(categories.has('date')).toBe(true);
      expect(categories.has('array')).toBe(true);
      expect(categories.has('math')).toBe(true);
    });
  });
});
