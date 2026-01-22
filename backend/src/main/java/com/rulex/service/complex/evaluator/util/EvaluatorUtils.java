package com.rulex.service.complex.evaluator.util;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.RuleEvaluationException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeParseException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import lombok.extern.slf4j.Slf4j;

/**
 * Classe utilitária centralizada para operações comuns dos Evaluators.
 *
 * <p>GAP-7 FIX: Elimina duplicação de código nos evaluators.
 *
 * <p>Fornece métodos para:
 *
 * <ul>
 *   <li>Parsing seguro de valores (Long, Integer, Double, BigDecimal)
 *   <li>Extração de valores do contexto e payload
 *   <li>Conversão de tipos
 *   <li>Tratamento de exceções com logging adequado
 * </ul>
 *
 * @version 1.0.0
 * @since 1.3.1
 */
@Slf4j
public final class EvaluatorUtils {

  private EvaluatorUtils() {
    // Utility class - não instanciar
  }

  // ========== PARSING SEGURO ==========

  /**
   * Parse seguro de Long com valor default.
   *
   * @param value valor a ser parseado
   * @param defaultValue valor default se parsing falhar
   * @return valor parseado ou default
   */
  public static long parseLongSafe(String value, long defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Long.parseLong(value.trim());
    } catch (NumberFormatException e) {
      log.debug("Falha ao parsear Long '{}', usando default {}", value, defaultValue);
      return defaultValue;
    }
  }

  /** Parse seguro de Integer com valor default. */
  public static int parseIntSafe(String value, int defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException e) {
      log.debug("Falha ao parsear Integer '{}', usando default {}", value, defaultValue);
      return defaultValue;
    }
  }

  /** Parse seguro de Double com valor default. */
  public static double parseDoubleSafe(String value, double defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return Double.parseDouble(value.trim());
    } catch (NumberFormatException e) {
      log.debug("Falha ao parsear Double '{}', usando default {}", value, defaultValue);
      return defaultValue;
    }
  }

  /** Parse seguro de BigDecimal com valor default. */
  public static BigDecimal parseBigDecimalSafe(String value, BigDecimal defaultValue) {
    if (value == null || value.isBlank()) {
      return defaultValue;
    }
    try {
      return new BigDecimal(value.trim());
    } catch (NumberFormatException e) {
      log.debug("Falha ao parsear BigDecimal '{}', usando default {}", value, defaultValue);
      return defaultValue;
    }
  }

  /** Parse seguro de BigDecimal a partir de Object. */
  public static BigDecimal toBigDecimalSafe(Object value, BigDecimal defaultValue) {
    if (value == null) {
      return defaultValue;
    }
    if (value instanceof BigDecimal bd) {
      return bd;
    }
    if (value instanceof Number n) {
      return BigDecimal.valueOf(n.doubleValue());
    }
    return parseBigDecimalSafe(String.valueOf(value), defaultValue);
  }

  // ========== EXTRAÇÃO DE VALORES ==========

  /**
   * Extrai valor do campo do contexto de forma segura.
   *
   * @param fieldName nome do campo
   * @param context contexto de avaliação
   * @return Optional com o valor ou empty
   */
  public static Optional<Object> getFieldValue(String fieldName, EvaluationContext context) {
    if (fieldName == null || context == null) {
      return Optional.empty();
    }

    // Tenta primeiro no payload
    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return Optional.ofNullable(payload.get(fieldName));
    }

    // Tenta no TransactionRequest via reflection
    TransactionRequest request = context.getTransactionRequest();
    if (request != null) {
      try {
        var field = request.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return Optional.ofNullable(field.get(request));
      } catch (NoSuchFieldException | IllegalAccessException e) {
        log.trace("Campo '{}' não encontrado no TransactionRequest", fieldName);
      }
    }

    return Optional.empty();
  }

  /** Extrai valor como String de forma segura. */
  public static String getStringValue(
      String fieldName, EvaluationContext context, String defaultValue) {
    return getFieldValue(fieldName, context).map(String::valueOf).orElse(defaultValue);
  }

  /** Extrai valor como Long de forma segura. */
  public static long getLongValue(String fieldName, EvaluationContext context, long defaultValue) {
    return getFieldValue(fieldName, context)
        .map(
            v -> {
              if (v instanceof Number n) return n.longValue();
              return parseLongSafe(String.valueOf(v), defaultValue);
            })
        .orElse(defaultValue);
  }

  /** Extrai valor como Integer de forma segura. */
  public static int getIntValue(String fieldName, EvaluationContext context, int defaultValue) {
    return getFieldValue(fieldName, context)
        .map(
            v -> {
              if (v instanceof Number n) return n.intValue();
              return parseIntSafe(String.valueOf(v), defaultValue);
            })
        .orElse(defaultValue);
  }

  /** Extrai valor como Double de forma segura. */
  public static double getDoubleValue(
      String fieldName, EvaluationContext context, double defaultValue) {
    return getFieldValue(fieldName, context)
        .map(
            v -> {
              if (v instanceof Number n) return n.doubleValue();
              return parseDoubleSafe(String.valueOf(v), defaultValue);
            })
        .orElse(defaultValue);
  }

  /** Extrai valor como BigDecimal de forma segura. */
  public static BigDecimal getBigDecimalValue(
      String fieldName, EvaluationContext context, BigDecimal defaultValue) {
    return getFieldValue(fieldName, context)
        .map(v -> toBigDecimalSafe(v, defaultValue))
        .orElse(defaultValue);
  }

  /** Extrai valor como Boolean de forma segura. */
  public static boolean getBooleanValue(
      String fieldName, EvaluationContext context, boolean defaultValue) {
    return getFieldValue(fieldName, context).map(EvaluatorUtils::toBoolean).orElse(defaultValue);
  }

  // ========== CONVERSÃO DE TIPOS ==========

  /** Converte Object para Boolean de forma segura. */
  public static boolean toBoolean(Object value) {
    if (value == null) return false;
    if (value instanceof Boolean b) return b;
    String str = String.valueOf(value).toLowerCase().trim();
    return "true".equals(str)
        || "1".equals(str)
        || "yes".equals(str)
        || "y".equals(str)
        || "sim".equals(str);
  }

  /** Converte Object para Long de forma segura. */
  public static long toLong(Object value, long defaultValue) {
    if (value == null) return defaultValue;
    if (value instanceof Number n) return n.longValue();
    return parseLongSafe(String.valueOf(value), defaultValue);
  }

  /** Converte Object para Double de forma segura. */
  public static double toDouble(Object value, double defaultValue) {
    if (value == null) return defaultValue;
    if (value instanceof Number n) return n.doubleValue();
    return parseDoubleSafe(String.valueOf(value), defaultValue);
  }

  /** Converte Object para String de forma segura. */
  public static String toString(Object value, String defaultValue) {
    if (value == null) return defaultValue;
    return String.valueOf(value);
  }

  // ========== COMPARAÇÕES ==========

  /**
   * Compara dois valores numéricos.
   *
   * @return negativo se a < b, zero se a == b, positivo se a > b
   */
  public static int compareNumeric(Object a, Object b) {
    BigDecimal bdA = toBigDecimalSafe(a, BigDecimal.ZERO);
    BigDecimal bdB = toBigDecimalSafe(b, BigDecimal.ZERO);
    return bdA.compareTo(bdB);
  }

  /** Verifica se valor está entre min e max (inclusive). */
  public static boolean isBetween(Object value, Object min, Object max) {
    BigDecimal bdValue = toBigDecimalSafe(value, null);
    BigDecimal bdMin = toBigDecimalSafe(min, null);
    BigDecimal bdMax = toBigDecimalSafe(max, null);

    if (bdValue == null || bdMin == null || bdMax == null) {
      return false;
    }

    return bdValue.compareTo(bdMin) >= 0 && bdValue.compareTo(bdMax) <= 0;
  }

  // ========== VALIDAÇÕES ==========

  /** Verifica se valor é nulo ou vazio. */
  public static boolean isNullOrEmpty(Object value) {
    if (value == null) return true;
    if (value instanceof String s) return s.isBlank();
    if (value instanceof Collection<?> c) return c.isEmpty();
    if (value instanceof Map<?, ?> m) return m.isEmpty();
    if (value.getClass().isArray()) return java.lang.reflect.Array.getLength(value) == 0;
    return false;
  }

  /** Verifica se valor não é nulo nem vazio. */
  public static boolean isNotNullOrEmpty(Object value) {
    return !isNullOrEmpty(value);
  }

  // ========== TRATAMENTO DE EXCEÇÕES ==========

  /**
   * Executa operação com tratamento de exceção e logging.
   *
   * <p>GAP-3 FIX: Substitui catch (Exception e) { return false; }
   *
   * @param operation operação a executar
   * @param operatorName nome do operador para logging
   * @param defaultValue valor default em caso de erro
   * @return resultado da operação ou default
   */
  public static boolean evaluateSafely(
      Supplier<Boolean> operation, String operatorName, boolean defaultValue) {
    try {
      Boolean result = operation.get();
      return result != null ? result : defaultValue;
    } catch (Exception e) {
      log.warn(
          "Erro ao avaliar operador {}: {} - {}",
          operatorName,
          e.getClass().getSimpleName(),
          e.getMessage());
      log.debug("Stack trace para {}", operatorName, e);
      return defaultValue;
    }
  }

  /**
   * Executa operação com tratamento de exceção, lançando RuleEvaluationException se falhar.
   *
   * <p>GAP-3 FIX: Para operadores que devem falhar em caso de erro.
   *
   * @param operation operação a executar
   * @param operatorName nome do operador
   * @param condition condição sendo avaliada
   * @return resultado da operação
   * @throws RuleEvaluationException se operação falhar
   */
  public static boolean evaluateOrThrow(
      Supplier<Boolean> operation, String operatorName, RuleCondition condition) {
    try {
      Boolean result = operation.get();
      if (result == null) {
        throw new RuleEvaluationException(
            operatorName,
            String.format("Operador retornou null para campo %s", condition.getFieldName()));
      }
      return result;
    } catch (RuleEvaluationException e) {
      throw e;
    } catch (Exception e) {
      log.error("Erro crítico ao avaliar operador {}: {}", operatorName, e.getMessage(), e);
      throw new RuleEvaluationException(
          operatorName,
          String.format(
              "Falha ao avaliar no campo %s: %s", condition.getFieldName(), e.getMessage()),
          e);
    }
  }

  // ========== PARSING DE CONDIÇÕES ==========

  /**
   * Extrai threshold e window de valueSingle no formato "threshold:window" ou "threshold".
   *
   * @param valueSingle valor da condição
   * @param defaultThreshold threshold default
   * @param defaultWindow window default
   * @return array com [threshold, window]
   */
  public static double[] parseThresholdAndWindow(
      String valueSingle, double defaultThreshold, double defaultWindow) {
    if (valueSingle == null || valueSingle.isBlank()) {
      return new double[] {defaultThreshold, defaultWindow};
    }

    String[] parts = valueSingle.split(":");
    double threshold = parseDoubleSafe(parts[0], defaultThreshold);
    double window = parts.length > 1 ? parseDoubleSafe(parts[1], defaultWindow) : defaultWindow;

    return new double[] {threshold, window};
  }

  /** Extrai lista de valores de valueSingle separados por vírgula. */
  public static List<String> parseValueList(String valueSingle) {
    if (valueSingle == null || valueSingle.isBlank()) {
      return List.of();
    }
    return List.of(valueSingle.split(",")).stream()
        .map(String::trim)
        .filter(s -> !s.isEmpty())
        .toList();
  }

  // ========== DATE/TIME ==========

  /** Parse seguro de data. */
  public static Optional<LocalDate> parseDate(String value) {
    if (value == null || value.isBlank()) {
      return Optional.empty();
    }
    try {
      return Optional.of(LocalDate.parse(value.trim()));
    } catch (DateTimeParseException e) {
      log.debug("Falha ao parsear data '{}'", value);
      return Optional.empty();
    }
  }

  /** Parse seguro de hora. */
  public static Optional<LocalTime> parseTime(String value) {
    if (value == null || value.isBlank()) {
      return Optional.empty();
    }
    try {
      return Optional.of(LocalTime.parse(value.trim()));
    } catch (DateTimeParseException e) {
      log.debug("Falha ao parsear hora '{}'", value);
      return Optional.empty();
    }
  }

  /** Parse seguro de datetime. */
  public static Optional<LocalDateTime> parseDateTime(String value) {
    if (value == null || value.isBlank()) {
      return Optional.empty();
    }
    try {
      return Optional.of(LocalDateTime.parse(value.trim()));
    } catch (DateTimeParseException e) {
      log.debug("Falha ao parsear datetime '{}'", value);
      return Optional.empty();
    }
  }

  /** Extrai hora do dia de um timestamp. */
  public static int getHourOfDay(Object timestamp) {
    if (timestamp == null) return -1;

    if (timestamp instanceof OffsetDateTime odt) {
      return odt.getHour();
    }
    if (timestamp instanceof LocalDateTime ldt) {
      return ldt.getHour();
    }
    if (timestamp instanceof LocalTime lt) {
      return lt.getHour();
    }

    // Tenta parsear string
    String str = String.valueOf(timestamp);
    return parseDateTime(str).map(LocalDateTime::getHour).orElse(-1);
  }

  /** Extrai dia da semana (1=Segunda, 7=Domingo). */
  public static int getDayOfWeek(Object timestamp) {
    if (timestamp == null) return -1;

    if (timestamp instanceof OffsetDateTime odt) {
      return odt.getDayOfWeek().getValue();
    }
    if (timestamp instanceof LocalDateTime ldt) {
      return ldt.getDayOfWeek().getValue();
    }
    if (timestamp instanceof LocalDate ld) {
      return ld.getDayOfWeek().getValue();
    }

    String str = String.valueOf(timestamp);
    return parseDateTime(str).map(dt -> dt.getDayOfWeek().getValue()).orElse(-1);
  }

  // ========== LOGGING HELPERS ==========

  /** Log de debug para avaliação de operador. */
  public static void logEvaluation(
      String operatorName,
      String fieldName,
      Object fieldValue,
      Object conditionValue,
      boolean result) {
    if (log.isDebugEnabled()) {
      log.debug(
          "Operador {}: field={}, value={}, condition={}, result={}",
          operatorName,
          fieldName,
          fieldValue,
          conditionValue,
          result);
    }
  }

  /** Log de trace para detalhes de avaliação. */
  public static void logEvaluationTrace(String operatorName, String details) {
    if (log.isTraceEnabled()) {
      log.trace("Operador {} - {}", operatorName, details);
    }
  }
}
