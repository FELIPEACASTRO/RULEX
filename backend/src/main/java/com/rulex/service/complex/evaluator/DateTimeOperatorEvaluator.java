package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de data e hora.
 *
 * <p>Operadores suportados:
 *
 * <ul>
 *   <li>DATE_BEFORE, DATE_AFTER - comparação de datas
 *   <li>DATE_BETWEEN - intervalo de datas
 *   <li>TIME_BETWEEN - intervalo de horários
 *   <li>HOUR_BETWEEN - intervalo de horas
 *   <li>DAY_OF_WEEK_IN - dias da semana
 *   <li>IS_WEEKEND - fim de semana
 *   <li>IS_BUSINESS_HOURS - horário comercial
 *   <li>DATE_DIFF_DAYS_LT/GT - diferença em dias
 * </ul>
 */
@Component
@Slf4j
public class DateTimeOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          ConditionOperator.DATE_BEFORE,
          ConditionOperator.DATE_AFTER,
          ConditionOperator.DATE_BETWEEN,
          ConditionOperator.TIME_BEFORE,
          ConditionOperator.TIME_AFTER,
          ConditionOperator.TIME_BETWEEN,
          ConditionOperator.HOUR_BETWEEN,
          ConditionOperator.DAY_OF_WEEK_IN,
          ConditionOperator.IS_WEEKEND);

  private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");
  private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");
  private static final DateTimeFormatter DATETIME_FORMATTER =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    String fieldName = condition.getFieldName();
    Object fieldValue = getFieldValue(context, fieldName);

    log.debug("DateTimeOperatorEvaluator: op={}, field={}, value={}", op, fieldName, fieldValue);

    return switch (op) {
      case DATE_BEFORE -> evaluateDateBefore(fieldValue, condition.getValueSingle());
      case DATE_AFTER -> evaluateDateAfter(fieldValue, condition.getValueSingle());
      case DATE_BETWEEN ->
          evaluateDateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case TIME_BEFORE -> evaluateTimeBefore(fieldValue, condition.getValueSingle());
      case TIME_AFTER -> evaluateTimeAfter(fieldValue, condition.getValueSingle());
      case TIME_BETWEEN ->
          evaluateTimeBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case HOUR_BETWEEN ->
          evaluateHourBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case DAY_OF_WEEK_IN -> evaluateDayOfWeekIn(fieldValue, condition.getValueArray());
      case IS_WEEKEND -> evaluateIsWeekend(fieldValue);
      default -> false;
    };
  }

  private boolean evaluateTimeBefore(Object fieldValue, String expected) {
    LocalTime fieldTime = parseTime(fieldValue);
    LocalTime expectedTime = parseTime(expected);

    if (fieldTime == null || expectedTime == null) return false;
    return fieldTime.isBefore(expectedTime);
  }

  private boolean evaluateTimeAfter(Object fieldValue, String expected) {
    LocalTime fieldTime = parseTime(fieldValue);
    LocalTime expectedTime = parseTime(expected);

    if (fieldTime == null || expectedTime == null) return false;
    return fieldTime.isAfter(expectedTime);
  }

  private Object getFieldValue(EvaluationContext context, String fieldName) {
    if (context == null || fieldName == null) {
      return null;
    }

    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return payload.get(fieldName);
    }

    if (context.getTransactionRequest() != null) {
      try {
        var request = context.getTransactionRequest();
        var field = request.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(request);
      } catch (Exception e) {
        log.trace("Campo {} não encontrado no TransactionRequest", fieldName);
      }
    }

    return null;
  }

  private boolean evaluateDateBefore(Object fieldValue, String expected) {
    LocalDate fieldDate = parseDate(fieldValue);
    LocalDate expectedDate = parseDate(expected);

    if (fieldDate == null || expectedDate == null) return false;
    return fieldDate.isBefore(expectedDate);
  }

  private boolean evaluateDateAfter(Object fieldValue, String expected) {
    LocalDate fieldDate = parseDate(fieldValue);
    LocalDate expectedDate = parseDate(expected);

    if (fieldDate == null || expectedDate == null) return false;
    return fieldDate.isAfter(expectedDate);
  }

  private boolean evaluateDateBetween(Object fieldValue, String min, String max) {
    LocalDate fieldDate = parseDate(fieldValue);
    LocalDate minDate = parseDate(min);
    LocalDate maxDate = parseDate(max);

    if (fieldDate == null || minDate == null || maxDate == null) return false;
    return !fieldDate.isBefore(minDate) && !fieldDate.isAfter(maxDate);
  }

  private boolean evaluateTimeBetween(Object fieldValue, String min, String max) {
    LocalTime fieldTime = parseTime(fieldValue);
    LocalTime minTime = parseTime(min);
    LocalTime maxTime = parseTime(max);

    if (fieldTime == null || minTime == null || maxTime == null) return false;

    // Suporta intervalos que cruzam meia-noite (ex: 22:00 - 06:00)
    if (minTime.isAfter(maxTime)) {
      return !fieldTime.isBefore(minTime) || !fieldTime.isAfter(maxTime);
    }
    return !fieldTime.isBefore(minTime) && !fieldTime.isAfter(maxTime);
  }

  private boolean evaluateHourBetween(Object fieldValue, String min, String max) {
    LocalTime fieldTime = parseTime(fieldValue);
    if (fieldTime == null) return false;

    try {
      int minHour = Integer.parseInt(min);
      int maxHour = Integer.parseInt(max);
      int fieldHour = fieldTime.getHour();

      if (minHour > maxHour) {
        return fieldHour >= minHour || fieldHour <= maxHour;
      }
      return fieldHour >= minHour && fieldHour <= maxHour;
    } catch (NumberFormatException e) {
      log.warn("Erro ao parsear horas: min={}, max={}", min, max);
      return false;
    }
  }

  private boolean evaluateDayOfWeekIn(Object fieldValue, List<String> days) {
    LocalDate fieldDate = parseDate(fieldValue);
    if (fieldDate == null || days == null || days.isEmpty()) return false;

    DayOfWeek dayOfWeek = fieldDate.getDayOfWeek();

    for (String day : days) {
      try {
        DayOfWeek expected = DayOfWeek.valueOf(day.toUpperCase());
        if (dayOfWeek == expected) return true;
      } catch (IllegalArgumentException e) {
        // Tentar por número (1=Monday, 7=Sunday)
        try {
          int dayNum = Integer.parseInt(day);
          if (dayOfWeek.getValue() == dayNum) return true;
        } catch (NumberFormatException ignored) {
          log.warn("Dia da semana inválido: {}", day);
        }
      }
    }
    return false;
  }

  private boolean evaluateIsWeekend(Object fieldValue) {
    LocalDate fieldDate = parseDate(fieldValue);
    if (fieldDate == null) return false;

    DayOfWeek day = fieldDate.getDayOfWeek();
    return day == DayOfWeek.SATURDAY || day == DayOfWeek.SUNDAY;
  }

  private boolean evaluateIsBusinessHours(Object fieldValue) {
    LocalTime fieldTime = parseTime(fieldValue);
    if (fieldTime == null) return false;

    // Horário comercial: 09:00 - 18:00
    LocalTime start = LocalTime.of(9, 0);
    LocalTime end = LocalTime.of(18, 0);

    return !fieldTime.isBefore(start) && !fieldTime.isAfter(end);
  }

  private boolean evaluateDateDiffDays(
      Object fieldValue, String compareDate, Integer threshold, boolean lessThan) {
    LocalDate fieldDate = parseDate(fieldValue);
    LocalDate otherDate = parseDate(compareDate);

    if (fieldDate == null || otherDate == null || threshold == null) return false;

    long daysDiff = Math.abs(java.time.temporal.ChronoUnit.DAYS.between(fieldDate, otherDate));

    return lessThan ? daysDiff < threshold : daysDiff > threshold;
  }

  private LocalDate parseDate(Object value) {
    if (value == null) return null;

    if (value instanceof LocalDate ld) return ld;
    if (value instanceof LocalDateTime ldt) return ldt.toLocalDate();

    String str = String.valueOf(value);

    // Tentar formato YYYYMMDD (comum em transações)
    if (str.matches("\\d{8}")) {
      try {
        int year = Integer.parseInt(str.substring(0, 4));
        int month = Integer.parseInt(str.substring(4, 6));
        int day = Integer.parseInt(str.substring(6, 8));
        return LocalDate.of(year, month, day);
      } catch (Exception e) {
        log.trace("Erro ao parsear data YYYYMMDD: {}", str);
      }
    }

    // Tentar formato ISO
    try {
      return LocalDate.parse(str, DATE_FORMATTER);
    } catch (DateTimeParseException e) {
      try {
        return LocalDateTime.parse(str, DATETIME_FORMATTER).toLocalDate();
      } catch (DateTimeParseException e2) {
        log.trace("Erro ao parsear data: {}", str);
        return null;
      }
    }
  }

  private LocalTime parseTime(Object value) {
    if (value == null) return null;

    if (value instanceof LocalTime lt) return lt;
    if (value instanceof LocalDateTime ldt) return ldt.toLocalTime();

    String str = String.valueOf(value);

    // Tentar formato HHMMSS (comum em transações)
    if (str.matches("\\d{6}")) {
      try {
        int hour = Integer.parseInt(str.substring(0, 2));
        int minute = Integer.parseInt(str.substring(2, 4));
        int second = Integer.parseInt(str.substring(4, 6));
        return LocalTime.of(hour, minute, second);
      } catch (Exception e) {
        log.trace("Erro ao parsear hora HHMMSS: {}", str);
      }
    }

    // Tentar formato ISO
    try {
      return LocalTime.parse(str, TIME_FORMATTER);
    } catch (DateTimeParseException e) {
      try {
        return LocalTime.parse(str);
      } catch (DateTimeParseException e2) {
        log.trace("Erro ao parsear hora: {}", str);
        return null;
      }
    }
  }

  @Override
  public String getCategory() {
    return "DATETIME";
  }
}
