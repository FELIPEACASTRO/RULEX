package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.DateTimeParser;
import com.rulex.util.RegexValidator;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class BasicOperatorEvaluator {

  private BasicOperatorEvaluator() {}

  public static boolean evaluateEquals(Object fieldValue, String expected, Boolean caseSensitive) {
    if (fieldValue == null && expected == null) return true;
    if (fieldValue == null || expected == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.equalsIgnoreCase(expected);
    }
    return fieldStr.equals(expected);
  }

  public static int compareValues(Object fieldValue, String expected) {
    if (fieldValue == null || expected == null) return 0;

    try {
      BigDecimal fieldNum = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal expectedNum = new BigDecimal(expected);
      return fieldNum.compareTo(expectedNum);
    } catch (NumberFormatException e) {
      return String.valueOf(fieldValue).compareTo(expected);
    }
  }

  public static boolean evaluateIn(Object fieldValue, List<String> values, Boolean caseSensitive) {
    if (fieldValue == null || values == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    for (String value : values) {
      if (Boolean.FALSE.equals(caseSensitive)) {
        if (fieldStr.equalsIgnoreCase(value)) return true;
      } else {
        if (fieldStr.equals(value)) return true;
      }
    }
    return false;
  }

  public static boolean evaluateInList(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null) return false;

    List<String> values;
    if (condition.getValueArray() != null && !condition.getValueArray().isEmpty()) {
      values = condition.getValueArray();
    } else if (condition.getValueSingle() != null && !condition.getValueSingle().isEmpty()) {
      values = java.util.Arrays.asList(condition.getValueSingle().split("\\|"));
    } else {
      return false;
    }

    String fieldStr = String.valueOf(fieldValue);
    Boolean caseSensitive = condition.getCaseSensitive();

    for (String value : values) {
      String trimmedValue = value.trim();
      if (Boolean.FALSE.equals(caseSensitive)) {
        if (fieldStr.equalsIgnoreCase(trimmedValue)) return true;
      } else {
        if (fieldStr.equalsIgnoreCase(trimmedValue)) return true;
      }
    }
    return false;
  }

  public static boolean evaluateContains(
      Object fieldValue, String substring, Boolean caseSensitive) {
    if (fieldValue == null || substring == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().contains(substring.toLowerCase());
    }
    return fieldStr.contains(substring);
  }

  public static boolean evaluateStartsWith(
      Object fieldValue, String prefix, Boolean caseSensitive) {
    if (fieldValue == null || prefix == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().startsWith(prefix.toLowerCase());
    }
    return fieldStr.startsWith(prefix);
  }

  public static boolean evaluateEndsWith(Object fieldValue, String suffix, Boolean caseSensitive) {
    if (fieldValue == null || suffix == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().endsWith(suffix.toLowerCase());
    }
    return fieldStr.endsWith(suffix);
  }

  public static boolean evaluateRegex(Object fieldValue, String pattern) {
    if (fieldValue == null || pattern == null) return false;

    RegexValidator.ValidationResult validation = RegexValidator.validate(pattern);
    if (!validation.valid()) {
      log.warn("Regex rejeitada por segurança: {} - {}", pattern, validation.errorMessage());
      return false;
    }

    try {
      return RegexValidator.safeMatches(pattern, String.valueOf(fieldValue));
    } catch (Exception e) {
      log.warn("Erro ao avaliar regex '{}': {}", pattern, e.getMessage());
      return false;
    }
  }

  public static boolean evaluateBetween(Object fieldValue, String min, String max) {
    if (fieldValue == null || min == null || max == null) return false;

    try {
      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal minVal = new BigDecimal(min);
      BigDecimal maxVal = new BigDecimal(max);
      return value.compareTo(minVal) >= 0 && value.compareTo(maxVal) <= 0;
    } catch (NumberFormatException e) {
      return false;
    }
  }

  public static boolean evaluateFieldComparison(
      Object fieldValue, String otherFieldName, EvaluationContext context, int comparison) {
    Object otherValue = FieldValueExtractor.getFieldValue(otherFieldName, null, context);
    int result = compareValues(fieldValue, String.valueOf(otherValue));

    return switch (comparison) {
      case 0 -> result == 0;
      case 1 -> result > 0;
      case 2 -> result >= 0;
      case -1 -> result < 0;
      case -2 -> result <= 0;
      default -> false;
    };
  }

  public static boolean evaluateDateBefore(Object fieldValue, String dateStr) {
    try {
      LocalDate fieldDate = DateTimeParser.parseDate(fieldValue);
      LocalDate compareDate = LocalDate.parse(dateStr);
      return fieldDate != null && fieldDate.isBefore(compareDate);
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateDateAfter(Object fieldValue, String dateStr) {
    try {
      LocalDate fieldDate = DateTimeParser.parseDate(fieldValue);
      LocalDate compareDate = LocalDate.parse(dateStr);
      return fieldDate != null && fieldDate.isAfter(compareDate);
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateDateBetween(Object fieldValue, String minDate, String maxDate) {
    try {
      LocalDate fieldDate = DateTimeParser.parseDate(fieldValue);
      LocalDate min = LocalDate.parse(minDate);
      LocalDate max = LocalDate.parse(maxDate);
      return fieldDate != null && !fieldDate.isBefore(min) && !fieldDate.isAfter(max);
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateTimeBefore(Object fieldValue, String timeStr) {
    try {
      LocalTime fieldTime = DateTimeParser.parseTime(fieldValue);
      LocalTime compareTime = LocalTime.parse(timeStr);
      return fieldTime != null && fieldTime.isBefore(compareTime);
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateTimeAfter(Object fieldValue, String timeStr) {
    try {
      LocalTime fieldTime = DateTimeParser.parseTime(fieldValue);
      LocalTime compareTime = LocalTime.parse(timeStr);
      return fieldTime != null && fieldTime.isAfter(compareTime);
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateTimeBetween(Object fieldValue, String minTime, String maxTime) {
    try {
      LocalTime fieldTime = DateTimeParser.parseTime(fieldValue);
      LocalTime min = LocalTime.parse(minTime);
      LocalTime max = LocalTime.parse(maxTime);
      return fieldTime != null && !fieldTime.isBefore(min) && !fieldTime.isAfter(max);
    } catch (Exception e) {
      return false;
    }
  }

  @SuppressWarnings("unchecked")
  public static boolean evaluateArrayContains(Object fieldValue, String element) {
    if (fieldValue == null) return false;

    if (fieldValue instanceof List) {
      return ((List<Object>) fieldValue)
          .stream().anyMatch(item -> String.valueOf(item).equals(element));
    }
    return false;
  }

  @SuppressWarnings("unchecked")
  public static boolean evaluateArraySize(Object fieldValue, String expectedSize, int comparison) {
    if (fieldValue == null || expectedSize == null) return false;

    int size = 0;
    if (fieldValue instanceof List) {
      size = ((List<Object>) fieldValue).size();
    } else if (fieldValue.getClass().isArray()) {
      size = java.lang.reflect.Array.getLength(fieldValue);
    } else {
      return false;
    }

    int expected = Integer.parseInt(expectedSize);
    return switch (comparison) {
      case 0 -> size == expected;
      case 1 -> size > expected;
      case -1 -> size < expected;
      default -> false;
    };
  }

  public static boolean evaluateModulo(
      Object fieldValue, String divisor, String remainder, boolean equals) {
    try {
      long value = Long.parseLong(String.valueOf(fieldValue));
      long div = Long.parseLong(divisor);
      long rem = Long.parseLong(remainder);
      boolean result = (value % div) == rem;
      return equals ? result : !result;
    } catch (Exception e) {
      return false;
    }
  }
}
