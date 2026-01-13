package com.rulex.util;

import java.util.regex.Pattern;
import lombok.extern.slf4j.Slf4j;

/**
 * Parser seguro para valueSingle com validação de entrada.
 *
 * <p>Resolve vulnerabilidade ReDoS (CVSS 7.5) identificada no Triple-Check validando formatos antes
 * de processamento.
 *
 * <p>Formatos suportados:
 *
 * <ul>
 *   <li>Simples: "30" (um valor numérico)
 *   <li>Threshold/Window: "10|30" (threshold|days)
 *   <li>Colon format: "3:24" (count:hours)
 *   <li>Key/Threshold/Window: "PAN|10|30" (keyType|threshold|days)
 *   <li>Operator format: "30|GT" (threshold|operator)
 * </ul>
 */
@Slf4j
public final class ValueSingleParser {

  private ValueSingleParser() {
    // Utility class
  }

  // Pattern seguro - sem grupos capturando ou backreferences que causam ReDoS
  // Permite apenas caracteres alfanuméricos, separadores (|, :, .) e espaços
  private static final Pattern SAFE_VALUE_PATTERN =
      Pattern.compile("^[a-zA-Z0-9_|:.\\-\\s]{1,100}$");

  // Pattern para validar números
  private static final Pattern NUMERIC_PATTERN = Pattern.compile("^-?\\d+(\\.\\d+)?$");

  // Pattern para validar identificadores (ex: PAN, CUSTOMER_ID)
  private static final Pattern IDENTIFIER_PATTERN = Pattern.compile("^[A-Z][A-Z0-9_]{0,30}$");

  // Separadores suportados
  public static final String PIPE_SEPARATOR = "\\|";
  public static final String COLON_SEPARATOR = ":";
  public static final String DOT_SEPARATOR = "\\.";

  /**
   * Valida se o valor é seguro para processamento.
   *
   * @param value Valor a validar
   * @return true se é seguro, false caso contrário
   */
  public static boolean isSafe(String value) {
    if (value == null || value.isBlank()) {
      return false;
    }
    return SAFE_VALUE_PATTERN.matcher(value.trim()).matches();
  }

  /**
   * Sanitiza o valor removendo caracteres potencialmente perigosos.
   *
   * @param value Valor original
   * @return Valor sanitizado ou null se inválido
   */
  public static String sanitize(String value) {
    if (value == null) {
      return null;
    }

    String trimmed = value.trim();
    if (trimmed.isEmpty() || trimmed.length() > 100) {
      log.warn("Valor inválido: vazio ou muito longo (max 100 chars)");
      return null;
    }

    if (!SAFE_VALUE_PATTERN.matcher(trimmed).matches()) {
      log.warn("Valor contém caracteres inválidos: {}", maskValue(trimmed));
      return null;
    }

    return trimmed;
  }

  /**
   * Parse de valor único (sem separadores).
   *
   * @param value Valor
   * @param defaultValue Valor default se inválido
   * @return Valor inteiro ou default
   */
  public static int parseInt(String value, int defaultValue) {
    String sanitized = sanitize(value);
    if (sanitized == null) {
      return defaultValue;
    }

    try {
      return Integer.parseInt(sanitized);
    } catch (NumberFormatException e) {
      log.debug("Valor não é inteiro: {}", maskValue(sanitized));
      return defaultValue;
    }
  }

  /** Parse de valor double. */
  public static double parseDouble(String value, double defaultValue) {
    String sanitized = sanitize(value);
    if (sanitized == null) {
      return defaultValue;
    }

    try {
      return Double.parseDouble(sanitized);
    } catch (NumberFormatException e) {
      log.debug("Valor não é double: {}", maskValue(sanitized));
      return defaultValue;
    }
  }

  /**
   * Parse de formato pipe: "threshold|window" ou "keyType|threshold|window".
   *
   * @param value Valor no formato pipe
   * @return Array de strings ou null se inválido
   */
  public static String[] parsePipeFormat(String value) {
    String sanitized = sanitize(value);
    if (sanitized == null) {
      return null;
    }

    String[] parts = sanitized.split(PIPE_SEPARATOR, 4);
    if (parts.length == 0 || parts.length > 3) {
      log.debug("Formato pipe inválido: esperado 1-3 partes, encontrado {}", parts.length);
      return null;
    }

    return parts;
  }

  /**
   * Parse de formato colon: "count:hours".
   *
   * @param value Valor no formato colon
   * @return Array de strings ou null se inválido
   */
  public static String[] parseColonFormat(String value) {
    String sanitized = sanitize(value);
    if (sanitized == null) {
      return null;
    }

    String[] parts = sanitized.split(COLON_SEPARATOR, 3);
    if (parts.length == 0 || parts.length > 2) {
      log.debug("Formato colon inválido: esperado 1-2 partes, encontrado {}", parts.length);
      return null;
    }

    return parts;
  }

  /** Resultado de parse para formato threshold|window. */
  public record ThresholdWindow(int threshold, int window) {}

  /**
   * Parse específico para threshold|window (ex: "10|30").
   *
   * @param value Valor
   * @param defaultThreshold Threshold default
   * @param defaultWindow Window default
   * @return ThresholdWindow com valores parsed
   */
  public static ThresholdWindow parseThresholdWindow(
      String value, int defaultThreshold, int defaultWindow) {

    String[] parts = parsePipeFormat(value);
    if (parts == null || parts.length == 0) {
      return new ThresholdWindow(defaultThreshold, defaultWindow);
    }

    int threshold = parseInt(parts[0], defaultThreshold);
    int window = parts.length > 1 ? parseInt(parts[1], defaultWindow) : defaultWindow;

    return new ThresholdWindow(threshold, window);
  }

  /** Resultado de parse para keyType|threshold|window. */
  public record KeyThresholdWindow(String keyType, int threshold, int window) {}

  /**
   * Parse específico para keyType|threshold|window (ex: "PAN|10|30").
   *
   * @param value Valor
   * @param defaultKeyType KeyType default
   * @param defaultThreshold Threshold default
   * @param defaultWindow Window default
   * @return KeyThresholdWindow com valores parsed
   */
  public static KeyThresholdWindow parseKeyThresholdWindow(
      String value, String defaultKeyType, int defaultThreshold, int defaultWindow) {

    String[] parts = parsePipeFormat(value);
    if (parts == null || parts.length == 0) {
      return new KeyThresholdWindow(defaultKeyType, defaultThreshold, defaultWindow);
    }

    if (parts.length == 3) {
      // keyType|threshold|window
      String keyType = IDENTIFIER_PATTERN.matcher(parts[0]).matches() ? parts[0] : defaultKeyType;
      int threshold = parseInt(parts[1], defaultThreshold);
      int window = parseInt(parts[2], defaultWindow);
      return new KeyThresholdWindow(keyType, threshold, window);
    } else if (parts.length == 2) {
      // threshold|window (usa default keyType)
      int threshold = parseInt(parts[0], defaultThreshold);
      int window = parseInt(parts[1], defaultWindow);
      return new KeyThresholdWindow(defaultKeyType, threshold, window);
    } else {
      // apenas threshold
      int threshold = parseInt(parts[0], defaultThreshold);
      return new KeyThresholdWindow(defaultKeyType, threshold, defaultWindow);
    }
  }

  /** Resultado de parse para threshold|operator. */
  public record ThresholdOperator(int threshold, String operator) {}

  /** Parse específico para threshold|operator (ex: "30|GT"). */
  public static ThresholdOperator parseThresholdOperator(
      String value, int defaultThreshold, String defaultOperator) {

    String[] parts = parsePipeFormat(value);
    if (parts == null || parts.length == 0) {
      return new ThresholdOperator(defaultThreshold, defaultOperator);
    }

    int threshold = parseInt(parts[0], defaultThreshold);
    String operator =
        parts.length > 1 && IDENTIFIER_PATTERN.matcher(parts[1]).matches()
            ? parts[1].toUpperCase()
            : defaultOperator;

    return new ThresholdOperator(threshold, operator);
  }

  /** Mascara valor para logging seguro. */
  private static String maskValue(String value) {
    if (value == null || value.length() < 4) {
      return "****";
    }
    return value.substring(0, 2) + "***" + value.substring(value.length() - 1);
  }
}
