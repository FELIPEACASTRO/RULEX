package com.rulex.util;

import java.util.Set;
import java.util.concurrent.*;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import lombok.extern.slf4j.Slf4j;

/**
 * Validador de expressões regulares com proteção contra ReDoS.
 *
 * <p>Implementa múltiplas camadas de proteção: 1. Limite de tamanho do pattern 2. Denylist de
 * construções perigosas (catastrophic backtracking) 3. Timeout na execução 4. Limite de
 * complexidade (grupos aninhados)
 */
@Slf4j
public final class RegexValidator {

  private RegexValidator() {
    // Utility class
  }

  // ========== CONFIGURAÇÕES ==========

  /** Tamanho máximo do pattern em caracteres */
  public static final int MAX_PATTERN_LENGTH = 500;

  /** Tamanho máximo do input para matching */
  public static final int MAX_INPUT_LENGTH = 10000;

  /** Timeout para execução de regex em milissegundos */
  public static final long REGEX_TIMEOUT_MS = 1000;

  /** Máximo de grupos de captura permitidos */
  public static final int MAX_CAPTURE_GROUPS = 10;

  /** Máximo de quantificadores aninhados */
  public static final int MAX_NESTED_QUANTIFIERS = 3;

  // ========== DENYLIST DE PADRÕES PERIGOSOS ==========

  /**
   * Padrões conhecidos por causar catastrophic backtracking. Estes padrões podem causar tempo
   * exponencial de execução.
   */
  private static final Set<String> DANGEROUS_PATTERNS =
      Set.of(
          // Quantificadores aninhados com backtracking
          "(a+)+",
          "(a*)*",
          "(a+)*",
          "(.*a){x}",
          // Alternação com sobreposição
          "(a|a)+",
          "(a|aa)+",
          // Evil regex patterns
          "([a-zA-Z]+)*",
          "(a+)+$",
          "^(a+)+$",
          "(x+x+)+y",
          // Lookahead/lookbehind com quantificadores
          "(?=.*a)(?=.*b)(?=.*c)",
          // Backtracking em grupos
          "(.*?)+");

  /** Substrings que indicam padrões potencialmente perigosos. */
  private static final Set<String> DANGEROUS_SUBSTRINGS =
      Set.of(
          "(.*)+",
          "(.+)+",
          "(.*)*",
          "(.+)*",
          "([^)]+)+",
          "([^]]+)+",
          "(\\s*)+",
          "(\\S*)+",
          "(\\w*)+",
          "(\\d*)+",
          "(.*)\\1", // Backreference com quantificador
          "(?:.*)+",
          "(?:.+)+");

  // ========== RESULTADO DA VALIDAÇÃO ==========

  /** Resultado da validação de regex. */
  public record ValidationResult(
      boolean valid, String errorMessage, String sanitizedPattern, int complexity) {
    public static ValidationResult success(String pattern, int complexity) {
      return new ValidationResult(true, null, pattern, complexity);
    }

    public static ValidationResult failure(String errorMessage) {
      return new ValidationResult(false, errorMessage, null, 0);
    }
  }

  // ========== MÉTODOS PÚBLICOS ==========

  /**
   * Valida um pattern de regex.
   *
   * @param pattern O pattern a validar
   * @return Resultado da validação
   */
  public static ValidationResult validate(String pattern) {
    if (pattern == null || pattern.isEmpty()) {
      return ValidationResult.failure("Pattern não pode ser vazio");
    }

    // 1. Verificar tamanho
    if (pattern.length() > MAX_PATTERN_LENGTH) {
      return ValidationResult.failure(
          String.format(
              "Pattern muito longo (%d caracteres). Máximo: %d",
              pattern.length(), MAX_PATTERN_LENGTH));
    }

    // 2. Verificar padrões perigosos conhecidos
    String dangerousMatch = findDangerousPattern(pattern);
    if (dangerousMatch != null) {
      return ValidationResult.failure(
          String.format(
              "Pattern contém construção perigosa que pode causar ReDoS: '%s'", dangerousMatch));
    }

    // 3. Verificar sintaxe
    try {
      Pattern.compile(pattern);
    } catch (PatternSyntaxException e) {
      return ValidationResult.failure(
          String.format("Sintaxe de regex inválida: %s", e.getDescription()));
    }

    // 4. Calcular complexidade
    int complexity = calculateComplexity(pattern);
    if (complexity > 100) {
      return ValidationResult.failure(
          String.format(
              "Pattern muito complexo (score: %d). Simplifique a expressão.", complexity));
    }

    // 5. Verificar grupos de captura
    int captureGroups = countCaptureGroups(pattern);
    if (captureGroups > MAX_CAPTURE_GROUPS) {
      return ValidationResult.failure(
          String.format(
              "Muitos grupos de captura (%d). Máximo: %d", captureGroups, MAX_CAPTURE_GROUPS));
    }

    // 6. Verificar quantificadores aninhados
    int nestedQuantifiers = countNestedQuantifiers(pattern);
    if (nestedQuantifiers > MAX_NESTED_QUANTIFIERS) {
      return ValidationResult.failure(
          String.format(
              "Muitos quantificadores aninhados (%d). Máximo: %d",
              nestedQuantifiers, MAX_NESTED_QUANTIFIERS));
    }

    return ValidationResult.success(pattern, complexity);
  }

  /**
   * Executa um match de regex com timeout.
   *
   * @param pattern O pattern compilado
   * @param input O input a testar
   * @param timeoutMs Timeout em milissegundos
   * @return true se match, false caso contrário
   * @throws TimeoutException se exceder o timeout
   */
  public static boolean matchWithTimeout(Pattern pattern, String input, long timeoutMs)
      throws TimeoutException {

    if (input == null) {
      return false;
    }

    // Truncar input se muito longo
    String safeInput =
        input.length() > MAX_INPUT_LENGTH ? input.substring(0, MAX_INPUT_LENGTH) : input;

    ExecutorService executor = Executors.newSingleThreadExecutor();
    Future<Boolean> future = executor.submit(() -> pattern.matcher(safeInput).matches());

    try {
      return future.get(timeoutMs, TimeUnit.MILLISECONDS);
    } catch (java.util.concurrent.TimeoutException e) {
      future.cancel(true);
      throw new TimeoutException(
          String.format("Regex timeout após %dms. Pattern pode ser muito complexo.", timeoutMs));
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new TimeoutException("Execução de regex interrompida");
    } catch (ExecutionException e) {
      log.error("Erro ao executar regex: {}", e.getMessage());
      return false;
    } finally {
      executor.shutdownNow();
    }
  }

  /** Executa um match de regex com timeout padrão. */
  public static boolean matchWithTimeout(Pattern pattern, String input) throws TimeoutException {
    return matchWithTimeout(pattern, input, REGEX_TIMEOUT_MS);
  }

  /**
   * Valida e compila um pattern de forma segura.
   *
   * @param pattern O pattern a compilar
   * @return Pattern compilado ou null se inválido
   */
  public static Pattern safeCompile(String pattern) {
    ValidationResult result = validate(pattern);
    if (!result.valid()) {
      log.warn("Pattern inválido: {} - {}", pattern, result.errorMessage());
      return null;
    }
    return Pattern.compile(pattern);
  }

  // ========== MÉTODOS PRIVADOS ==========

  /** Procura padrões perigosos no pattern. */
  private static String findDangerousPattern(String pattern) {
    // Verificar padrões exatos
    for (String dangerous : DANGEROUS_PATTERNS) {
      if (pattern.contains(dangerous)) {
        return dangerous;
      }
    }

    // Verificar substrings perigosas
    for (String substring : DANGEROUS_SUBSTRINGS) {
      if (pattern.contains(substring)) {
        return substring;
      }
    }

    // Verificar padrões dinâmicos perigosos
    // (X+)+ onde X é qualquer coisa
    if (pattern.matches(".*\\([^)]*[+*]\\)[+*].*")) {
      return "quantificador aninhado detectado";
    }

    // Verificar alternação com sobreposição potencial
    if (hasOverlappingAlternation(pattern)) {
      return "alternação com sobreposição potencial";
    }

    return null;
  }

  /** Verifica se há alternação com sobreposição. */
  private static boolean hasOverlappingAlternation(String pattern) {
    // Padrão simplificado: (a|ab) ou (ab|a) onde um é prefixo do outro
    // Esta é uma verificação heurística, não completa
    if (!pattern.contains("|")) {
      return false;
    }

    // Verificar padrões como (a|a+) ou (a+|a)
    return pattern.matches(".*\\([^|)]+\\|\\1[+*]\\).*")
        || pattern.matches(".*\\([^|)]+[+*]\\|\\1\\).*");
  }

  /** Calcula um score de complexidade do pattern. */
  private static int calculateComplexity(String pattern) {
    int score = 0;

    // Cada caractere base
    score += pattern.length();

    // Quantificadores
    score += countOccurrences(pattern, '+') * 5;
    score += countOccurrences(pattern, '*') * 5;
    score += countOccurrences(pattern, '?') * 2;

    // Grupos
    score += countOccurrences(pattern, '(') * 10;

    // Alternação
    score += countOccurrences(pattern, '|') * 8;

    // Lookahead/lookbehind
    score += countOccurrences(pattern, "(?=") * 15;
    score += countOccurrences(pattern, "(?!") * 15;
    score += countOccurrences(pattern, "(?<=") * 15;
    score += countOccurrences(pattern, "(?<!") * 15;

    // Backreferences
    for (int i = 1; i <= 9; i++) {
      score += countOccurrences(pattern, "\\" + i) * 20;
    }

    // Character classes
    score += countOccurrences(pattern, '[') * 3;

    // Quantificadores com limites {n,m}
    score += countOccurrences(pattern, '{') * 5;

    return score;
  }

  /** Conta grupos de captura. */
  private static int countCaptureGroups(String pattern) {
    int count = 0;
    int i = 0;
    while (i < pattern.length()) {
      if (pattern.charAt(i) == '(' && (i + 1 >= pattern.length() || pattern.charAt(i + 1) != '?')) {
        count++;
      }
      i++;
    }
    return count;
  }

  /** Conta quantificadores aninhados. */
  private static int countNestedQuantifiers(String pattern) {
    int maxNesting = 0;
    int currentNesting = 0;
    boolean inQuantifier = false;

    for (int i = 0; i < pattern.length(); i++) {
      char c = pattern.charAt(i);

      if (c == '(') {
        if (inQuantifier) {
          currentNesting++;
          maxNesting = Math.max(maxNesting, currentNesting);
        }
      } else if (c == ')') {
        if (i + 1 < pattern.length()) {
          char next = pattern.charAt(i + 1);
          if (next == '+' || next == '*' || next == '?' || next == '{') {
            inQuantifier = true;
          }
        }
        if (currentNesting > 0) {
          currentNesting--;
        }
      }
    }

    return maxNesting;
  }

  /** Conta ocorrências de um caractere. */
  private static int countOccurrences(String str, char c) {
    int count = 0;
    for (int i = 0; i < str.length(); i++) {
      if (str.charAt(i) == c) {
        count++;
      }
    }
    return count;
  }

  /** Conta ocorrências de uma substring. */
  private static int countOccurrences(String str, String sub) {
    int count = 0;
    int idx = 0;
    while ((idx = str.indexOf(sub, idx)) != -1) {
      count++;
      idx += sub.length();
    }
    return count;
  }

  /** Exceção para timeout de regex. */
  public static class TimeoutException extends Exception {
    private static final long serialVersionUID = 1L;

    public TimeoutException(String message) {
      super(message);
    }
  }
}
