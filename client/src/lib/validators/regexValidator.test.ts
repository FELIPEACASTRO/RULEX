/**
 * Testes para o validador de regex com proteção ReDoS
 */

import { describe, it, expect } from 'vitest';
import { 
  validateRegex, 
  getRegexValidationError, 
  isRegexSafe,
  MAX_PATTERN_LENGTH,
  MAX_CAPTURE_GROUPS 
} from './regexValidator';

describe('regexValidator', () => {
  describe('validateRegex', () => {
    describe('Patterns válidos', () => {
      it('deve aceitar pattern simples', () => {
        const result = validateRegex('^[a-z]+$');
        expect(result.valid).toBe(true);
        expect(result.errorMessage).toBeNull();
      });

      it('deve aceitar pattern de email', () => {
        const result = validateRegex('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+$');
        expect(result.valid).toBe(true);
      });

      it('deve aceitar pattern de telefone', () => {
        const result = validateRegex('^\\+?[0-9]{10,14}$');
        expect(result.valid).toBe(true);
      });

      it('deve aceitar pattern de CPF', () => {
        const result = validateRegex('^\\d{3}\\.\\d{3}\\.\\d{3}-\\d{2}$');
        expect(result.valid).toBe(true);
      });

      it('deve aceitar pattern de CEP', () => {
        const result = validateRegex('^\\d{5}-?\\d{3}$');
        expect(result.valid).toBe(true);
      });
    });

    describe('Patterns inválidos', () => {
      it('deve rejeitar pattern vazio', () => {
        const result = validateRegex('');
        expect(result.valid).toBe(false);
        expect(result.errorMessage).toContain('vazio');
      });

      it('deve rejeitar pattern nulo/undefined', () => {
        const result = validateRegex(null as unknown as string);
        expect(result.valid).toBe(false);
      });

      it('deve rejeitar pattern muito longo', () => {
        const longPattern = 'a'.repeat(MAX_PATTERN_LENGTH + 1);
        const result = validateRegex(longPattern);
        expect(result.valid).toBe(false);
        expect(result.errorMessage).toContain('muito longo');
      });

      it('deve rejeitar sintaxe inválida', () => {
        const result = validateRegex('[a-z');
        expect(result.valid).toBe(false);
        expect(result.errorMessage).toContain('Sintaxe');
      });
    });

    describe('Proteção ReDoS', () => {
      it('deve rejeitar (a+)+', () => {
        const result = validateRegex('(a+)+');
        expect(result.valid).toBe(false);
        expect(result.errorMessage).toContain('perigosa');
      });

      it('deve rejeitar (a*)*', () => {
        const result = validateRegex('(a*)*');
        expect(result.valid).toBe(false);
      });

      it('deve rejeitar (.*)+', () => {
        const result = validateRegex('(.*)+');
        expect(result.valid).toBe(false);
      });

      it('deve rejeitar (.+)+', () => {
        const result = validateRegex('(.+)+');
        expect(result.valid).toBe(false);
      });

      it('deve rejeitar ([a-zA-Z]+)*', () => {
        const result = validateRegex('([a-zA-Z]+)*');
        expect(result.valid).toBe(false);
      });

      it('deve rejeitar (a+)+$', () => {
        const result = validateRegex('(a+)+$');
        expect(result.valid).toBe(false);
      });

      it('deve rejeitar quantificadores aninhados dinâmicos', () => {
        const result = validateRegex('(x+y+)+');
        expect(result.valid).toBe(false);
      });
    });

    describe('Complexidade', () => {
      it('deve calcular complexidade para pattern simples', () => {
        const result = validateRegex('^[a-z]+$');
        expect(result.complexity).toBeGreaterThan(0);
        expect(result.complexity).toBeLessThan(50);
      });

      it('deve rejeitar pattern muito complexo', () => {
        const complex = '((a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)+)+';
        const result = validateRegex(complex);
        expect(result.valid).toBe(false);
        expect(result.errorMessage).toContain('complexo');
      });
    });

    describe('Grupos de captura', () => {
      it('deve aceitar número razoável de grupos', () => {
        const result = validateRegex('(a)(b)(c)');
        expect(result.valid).toBe(true);
      });

      it('deve contar grupos corretamente', () => {
        const result = validateRegex('(?:a)(b)(?:c)(d)');
        expect(result.valid).toBe(true);
      });
    });
  });

  describe('getRegexValidationError', () => {
    it('deve retornar null para pattern válido', () => {
      expect(getRegexValidationError('^[a-z]+$')).toBeNull();
    });

    it('deve retornar mensagem para pattern inválido', () => {
      expect(getRegexValidationError('[a-z')).not.toBeNull();
    });

    it('deve retornar mensagem para pattern perigoso', () => {
      expect(getRegexValidationError('(a+)+')).not.toBeNull();
    });
  });

  describe('isRegexSafe', () => {
    it('deve retornar true para pattern seguro', () => {
      expect(isRegexSafe('^[a-z]+$')).toBe(true);
    });

    it('deve retornar false para pattern perigoso', () => {
      expect(isRegexSafe('(a+)+')).toBe(false);
    });

    it('deve retornar false para sintaxe inválida', () => {
      expect(isRegexSafe('[a-z')).toBe(false);
    });
  });

  describe('Casos de uso reais', () => {
    it('deve validar pattern de cartão de crédito', () => {
      const result = validateRegex('^\\d{4}[- ]?\\d{4}[- ]?\\d{4}[- ]?\\d{4}$');
      expect(result.valid).toBe(true);
    });

    it('deve validar pattern de data YYYYMMDD simplificado', () => {
      const result = validateRegex('^\\d{8}$');
      expect(result.valid).toBe(true);
    });

    it('deve validar pattern de hora HHMMSS simplificado', () => {
      const result = validateRegex('^\\d{6}$');
      expect(result.valid).toBe(true);
    });

    it('deve validar pattern de MCC', () => {
      const result = validateRegex('^\\d{4}$');
      expect(result.valid).toBe(true);
    });

    it('deve validar pattern de país ISO', () => {
      const result = validateRegex('^[A-Z]{2,3}$');
      expect(result.valid).toBe(true);
    });
  });
});
