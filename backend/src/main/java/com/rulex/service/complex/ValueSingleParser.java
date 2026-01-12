package com.rulex.service.complex;

import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;
import lombok.Builder;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Parser unificado e seguro para os formatos de valueSingle.
 *
 * <p>Formatos suportados:
 *
 * <ul>
 *   <li>PIPE: "field|nDays|threshold|op" ou "threshold|hours"
 *   <li>COMMA: "value1,value2,value3"
 *   <li>COLON: "threshold:hours"
 *   <li>SIMPLE: "threshold"
 *   <li>MIN_MAX: usa valueMin + valueMax (não valueSingle)
 * </ul>
 *
 * <p>Inclui validação de segurança contra ReDoS e injection.
 */
@Component
@Slf4j
public class ValueSingleParser {

  /** Padrão seguro para valueSingle - previne ReDoS e injection */
  private static final Pattern SAFE_VALUE_PATTERN = Pattern.compile("^[\\w\\-:|,\\.\\s]{0,500}$");

  /** Padrões pré-compilados para split (evita recompilação) */
  private static final Pattern PIPE_PATTERN = Pattern.compile("\\|");

  private static final Pattern COMMA_PATTERN = Pattern.compile(",");
  private static final Pattern COLON_PATTERN = Pattern.compile(":");

  /** Formato do valueSingle */
  public enum ValueFormat {
    PIPE, // field|nDays|threshold|op
    COMMA, // value1,value2,value3
    COLON, // threshold:hours
    SIMPLE, // threshold
    MIN_MAX, // usa valueMin + valueMax
    EMPTY // null ou vazio
  }

  /** Resultado do parsing */
  @Data
  @Builder
  public static class ParsedValue {
    private final ValueFormat format;
    private final String[] parts;
    private final String raw;
    private final boolean valid;
    private final String errorMessage;

    /**
     * Obtém o threshold como inteiro.
     *
     * @param defaultValue valor padrão se parsing falhar
     * @return threshold ou defaultValue
     */
    public int getThresholdInt(int defaultValue) {
      if (!valid || parts == null || parts.length == 0) {
        return defaultValue;
      }
      return switch (format) {
        case PIPE -> parseIntSafe(parts[0], defaultValue);
        case COLON -> parseIntSafe(parts[0], defaultValue);
        case SIMPLE -> parseIntSafe(raw, defaultValue);
        default -> defaultValue;
      };
    }

    /**
     * Obtém o threshold como double.
     *
     * @param defaultValue valor padrão se parsing falhar
     * @return threshold ou defaultValue
     */
    public double getThresholdDouble(double defaultValue) {
      if (!valid || parts == null || parts.length == 0) {
        return defaultValue;
      }
      return switch (format) {
        case PIPE -> parseDoubleSafe(parts[0], defaultValue);
        case COLON -> parseDoubleSafe(parts[0], defaultValue);
        case SIMPLE -> parseDoubleSafe(raw, defaultValue);
        default -> defaultValue;
      };
    }

    /**
     * Obtém o valor de tempo (horas ou dias).
     *
     * @param defaultValue valor padrão
     * @return time value ou defaultValue
     */
    public int getTimeValue(int defaultValue) {
      if (!valid || parts == null) {
        return defaultValue;
      }
      return switch (format) {
        case PIPE -> parts.length > 1 ? parseIntSafe(parts[1], defaultValue) : defaultValue;
        case COLON -> parts.length > 1 ? parseIntSafe(parts[1], defaultValue) : defaultValue;
        default -> defaultValue;
      };
    }

    /**
     * Obtém o operador de comparação.
     *
     * @param defaultValue operador padrão
     * @return operador ou defaultValue
     */
    public String getOperator(String defaultValue) {
      if (!valid || format != ValueFormat.PIPE || parts == null) {
        return defaultValue;
      }
      return parts.length > 2 ? parts[2] : defaultValue;
    }

    /**
     * Obtém como lista de strings.
     *
     * @return lista ou lista vazia
     */
    public List<String> getList() {
      if (!valid || format != ValueFormat.COMMA || parts == null) {
        return List.of();
      }
      return Arrays.asList(parts);
    }

    /**
     * Obtém a parte no índice especificado.
     *
     * @param index índice
     * @param defaultValue valor padrão
     * @return parte ou defaultValue
     */
    public String getPart(int index, String defaultValue) {
      if (!valid || parts == null || index < 0 || index >= parts.length) {
        return defaultValue;
      }
      return parts[index];
    }

    private static int parseIntSafe(String value, int defaultValue) {
      if (value == null || value.isBlank()) {
        return defaultValue;
      }
      try {
        return Integer.parseInt(value.trim());
      } catch (NumberFormatException e) {
        return defaultValue;
      }
    }

    private static double parseDoubleSafe(String value, double defaultValue) {
      if (value == null || value.isBlank()) {
        return defaultValue;
      }
      try {
        return Double.parseDouble(value.trim());
      } catch (NumberFormatException e) {
        return defaultValue;
      }
    }
  }

  /**
   * Valida e faz parse do valueSingle.
   *
   * @param valueSingle string a ser parseada
   * @return ParsedValue com resultado do parsing
   */
  public ParsedValue parse(String valueSingle) {
    // Null ou vazio
    if (valueSingle == null || valueSingle.isBlank()) {
      return ParsedValue.builder()
          .format(ValueFormat.EMPTY)
          .parts(new String[0])
          .raw("")
          .valid(true)
          .build();
    }

    String trimmed = valueSingle.trim();

    // Validação de segurança contra ReDoS e injection
    if (!SAFE_VALUE_PATTERN.matcher(trimmed).matches()) {
      log.warn("valueSingle rejeitado por segurança: contém caracteres inválidos");
      return ParsedValue.builder()
          .format(ValueFormat.SIMPLE)
          .parts(new String[0])
          .raw(trimmed)
          .valid(false)
          .errorMessage("Invalid characters in valueSingle")
          .build();
    }

    // Detectar formato
    if (trimmed.contains("|")) {
      String[] parts = PIPE_PATTERN.split(trimmed);
      return ParsedValue.builder()
          .format(ValueFormat.PIPE)
          .parts(trimArrayParts(parts))
          .raw(trimmed)
          .valid(true)
          .build();
    }

    // Vírgula para listas (mas não para números decimais como "1,5")
    if (trimmed.contains(",") && !isDecimalNumber(trimmed)) {
      String[] parts = COMMA_PATTERN.split(trimmed);
      return ParsedValue.builder()
          .format(ValueFormat.COMMA)
          .parts(trimArrayParts(parts))
          .raw(trimmed)
          .valid(true)
          .build();
    }

    if (trimmed.contains(":")) {
      String[] parts = COLON_PATTERN.split(trimmed);
      return ParsedValue.builder()
          .format(ValueFormat.COLON)
          .parts(trimArrayParts(parts))
          .raw(trimmed)
          .valid(true)
          .build();
    }

    // Formato simples
    return ParsedValue.builder()
        .format(ValueFormat.SIMPLE)
        .parts(new String[] {trimmed})
        .raw(trimmed)
        .valid(true)
        .build();
  }

  /**
   * Valida se o valueSingle é seguro para uso.
   *
   * @param valueSingle string a validar
   * @return true se seguro
   */
  public boolean isValid(String valueSingle) {
    if (valueSingle == null || valueSingle.isBlank()) {
      return true;
    }
    return SAFE_VALUE_PATTERN.matcher(valueSingle.trim()).matches();
  }

  /**
   * Faz parse de inteiro de forma segura.
   *
   * @param value string a converter
   * @param defaultValue valor padrão
   * @return inteiro ou defaultValue
   */
  public int parseIntSafe(String value, int defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException e) {
      log.debug("Falha ao converter '{}' para int, usando default: {}", value, defaultValue);
      return defaultValue;
    }
  }

  /**
   * Faz parse de double de forma segura.
   *
   * @param value string a converter
   * @param defaultValue valor padrão
   * @return double ou defaultValue
   */
  public double parseDoubleSafe(String value, double defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Double.parseDouble(value.trim());
    } catch (NumberFormatException e) {
      log.debug("Falha ao converter '{}' para double, usando default: {}", value, defaultValue);
      return defaultValue;
    }
  }

  private String[] trimArrayParts(String[] parts) {
    for (int i = 0; i < parts.length; i++) {
      parts[i] = parts[i].trim();
    }
    return parts;
  }

  private boolean isDecimalNumber(String value) {
    // Verifica se é um número decimal (ex: "1,5" ou "1.5")
    return value.matches("^-?\\d+[,.]\\d+$");
  }
}
