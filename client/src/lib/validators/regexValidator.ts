/**
 * Validador de expressões regulares com proteção contra ReDoS.
 * Espelha a lógica do backend RegexValidator.java
 */

// ============================================
// CONFIGURAÇÕES
// ============================================

/** Tamanho máximo do pattern em caracteres */
export const MAX_PATTERN_LENGTH = 500;

/** Máximo de grupos de captura permitidos */
export const MAX_CAPTURE_GROUPS = 10;

// ============================================
// PADRÕES PERIGOSOS
// ============================================

/**
 * Padrões conhecidos por causar catastrophic backtracking.
 */
const DANGEROUS_PATTERNS = new Set([
  '(a+)+',
  '(a*)*',
  '(a+)*',
  '(.*a){x}',
  '(a|a)+',
  '(a|aa)+',
  '([a-zA-Z]+)*',
  '(a+)+$',
  '^(a+)+$',
  '(x+x+)+y',
  '(?=.*a)(?=.*b)(?=.*c)',
  '(.*?)+',
]);

/**
 * Substrings que indicam padrões potencialmente perigosos.
 */
const DANGEROUS_SUBSTRINGS = [
  '(.*)+',
  '(.+)+',
  '(.*)*',
  '(.+)*',
  '([^)]+)+',
  '([^]]+)+',
  '(\\s*)+',
  '(\\S*)+',
  '(\\w*)+',
  '(\\d*)+',
  '(.*)\\1',
  '(?:.*)+',
  '(?:.+)+',
];

// ============================================
// RESULTADO DA VALIDAÇÃO
// ============================================

export interface RegexValidationResult {
  valid: boolean;
  errorMessage: string | null;
  complexity: number;
  warnings: string[];
}

// ============================================
// FUNÇÕES DE VALIDAÇÃO
// ============================================

/**
 * Valida um pattern de regex.
 */
export function validateRegex(pattern: string): RegexValidationResult {
  const warnings: string[] = [];

  // 1. Verificar se está vazio
  if (!pattern || pattern.trim() === '') {
    return {
      valid: false,
      errorMessage: 'Pattern não pode ser vazio',
      complexity: 0,
      warnings,
    };
  }

  // 2. Verificar tamanho
  if (pattern.length > MAX_PATTERN_LENGTH) {
    return {
      valid: false,
      errorMessage: `Pattern muito longo (${pattern.length} caracteres). Máximo: ${MAX_PATTERN_LENGTH}`,
      complexity: 0,
      warnings,
    };
  }

  // 3. Verificar padrões perigosos conhecidos
  const dangerousMatch = findDangerousPattern(pattern);
  if (dangerousMatch) {
    return {
      valid: false,
      errorMessage: `Pattern contém construção perigosa que pode causar ReDoS: '${dangerousMatch}'`,
      complexity: 0,
      warnings,
    };
  }

  // 4. Verificar sintaxe
  try {
    new RegExp(pattern);
  } catch (e) {
    return {
      valid: false,
      errorMessage: `Sintaxe de regex inválida: ${e instanceof Error ? e.message : 'erro desconhecido'}`,
      complexity: 0,
      warnings,
    };
  }

  // 5. Calcular complexidade
  const complexity = calculateComplexity(pattern);
  if (complexity > 100) {
    return {
      valid: false,
      errorMessage: `Pattern muito complexo (score: ${complexity}). Simplifique a expressão.`,
      complexity,
      warnings,
    };
  }

  // 6. Verificar grupos de captura
  const captureGroups = countCaptureGroups(pattern);
  if (captureGroups > MAX_CAPTURE_GROUPS) {
    return {
      valid: false,
      errorMessage: `Muitos grupos de captura (${captureGroups}). Máximo: ${MAX_CAPTURE_GROUPS}`,
      complexity,
      warnings,
    };
  }

  // Adicionar warnings para complexidade moderada
  if (complexity > 50) {
    warnings.push('Pattern moderadamente complexo. Considere simplificar se possível.');
  }

  return {
    valid: true,
    errorMessage: null,
    complexity,
    warnings,
  };
}

/**
 * Procura padrões perigosos no pattern.
 */
function findDangerousPattern(pattern: string): string | null {
  // Verificar padrões exatos (converter Set para Array para compatibilidade)
  const dangerousPatternsArray = Array.from(DANGEROUS_PATTERNS);
  for (const dangerous of dangerousPatternsArray) {
    if (pattern.includes(dangerous)) {
      return dangerous;
    }
  }

  // Verificar substrings perigosas
  for (const substring of DANGEROUS_SUBSTRINGS) {
    if (pattern.includes(substring)) {
      return substring;
    }
  }

  // Verificar padrões dinâmicos perigosos: (X+)+ onde X é qualquer coisa
  if (/\([^)]*[+*]\)[+*]/.test(pattern)) {
    return 'quantificador aninhado detectado';
  }

  return null;
}

/**
 * Calcula um score de complexidade do pattern.
 */
function calculateComplexity(pattern: string): number {
  let score = 0;

  // Cada caractere base
  score += pattern.length;

  // Quantificadores
  score += (pattern.match(/\+/g) || []).length * 5;
  score += (pattern.match(/\*/g) || []).length * 5;
  score += (pattern.match(/\?/g) || []).length * 2;

  // Grupos
  score += (pattern.match(/\(/g) || []).length * 10;

  // Alternação
  score += (pattern.match(/\|/g) || []).length * 8;

  // Lookahead/lookbehind
  score += (pattern.match(/\(\?=/g) || []).length * 15;
  score += (pattern.match(/\(\?!/g) || []).length * 15;
  score += (pattern.match(/\(\?<=/g) || []).length * 15;
  score += (pattern.match(/\(\?<!/g) || []).length * 15;

  // Backreferences
  for (let i = 1; i <= 9; i++) {
    score += (pattern.match(new RegExp(`\\\\${i}`, 'g')) || []).length * 20;
  }

  // Character classes
  score += (pattern.match(/\[/g) || []).length * 3;

  // Quantificadores com limites {n,m}
  score += (pattern.match(/\{/g) || []).length * 5;

  return score;
}

/**
 * Conta grupos de captura.
 */
function countCaptureGroups(pattern: string): number {
  let count = 0;
  for (let i = 0; i < pattern.length; i++) {
    if (pattern[i] === '(' && (i + 1 >= pattern.length || pattern[i + 1] !== '?')) {
      count++;
    }
  }
  return count;
}

/**
 * Valida e retorna mensagem de erro amigável para uso em formulários.
 */
export function getRegexValidationError(pattern: string): string | null {
  const result = validateRegex(pattern);
  return result.errorMessage;
}

/**
 * Verifica se um pattern é seguro para uso.
 */
export function isRegexSafe(pattern: string): boolean {
  return validateRegex(pattern).valid;
}
