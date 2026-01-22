package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.OperatorDataService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.DateTimeParser;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class TimeDateEvaluator {

  private TimeDateEvaluator() {}

  public static boolean evaluateIsWeekend(EvaluationContext context) {
    try {
      Object dateValue = FieldValueExtractor.getFieldValue("transactionDate", null, context);
      if (dateValue == null) {
        return false;
      }
      LocalDate date = DateTimeParser.parseDate(dateValue);
      if (date == null) {
        return false;
      }
      java.time.DayOfWeek dow = date.getDayOfWeek();
      return dow == java.time.DayOfWeek.SATURDAY || dow == java.time.DayOfWeek.SUNDAY;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_WEEKEND: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateIsHoliday(
      EvaluationContext context, OperatorDataService operatorDataService) {
    try {
      Object dateValue = FieldValueExtractor.getFieldValue("transactionDate", null, context);
      LocalDate transactionDate;

      if (dateValue instanceof LocalDate) {
        transactionDate = (LocalDate) dateValue;
      } else if (dateValue instanceof LocalDateTime) {
        transactionDate = ((LocalDateTime) dateValue).toLocalDate();
      } else if (dateValue instanceof String) {
        transactionDate = LocalDate.parse((String) dateValue);
      } else {
        transactionDate = LocalDate.now();
      }

      Object countryValue = FieldValueExtractor.getFieldValue("merchantCountryCode", null, context);
      String countryCode = countryValue != null ? countryValue.toString() : "BRA";

      return operatorDataService.isHoliday(transactionDate, countryCode);
    } catch (Exception e) {
      log.warn("Erro ao verificar feriado: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateHourBetween(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      int startHour = Integer.parseInt(parts[0].trim());
      int endHour = Integer.parseInt(parts[1].trim());

      Object timeValue = FieldValueExtractor.getFieldValue("transactionTime", null, context);
      if (timeValue == null) {
        return false;
      }

      int hour;
      if (timeValue instanceof Integer) {
        hour = ((Integer) timeValue) / 10000;
      } else {
        LocalTime time = DateTimeParser.parseTime(timeValue);
        if (time == null) return false;
        hour = time.getHour();
      }

      if (startHour <= endHour) {
        return hour >= startHour && hour <= endHour;
      }
      return hour >= startHour || hour <= endHour;
    } catch (Exception e) {
      log.error("Erro ao avaliar HOUR_BETWEEN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDayOfWeekIn(RuleCondition condition, EvaluationContext context) {
    try {
      Object dateValue = FieldValueExtractor.getFieldValue("transactionDate", null, context);
      if (dateValue == null) {
        return false;
      }
      LocalDate date = DateTimeParser.parseDate(dateValue);
      if (date == null) {
        return false;
      }

      int dayOfWeek = date.getDayOfWeek().getValue();
      String[] days = condition.getValueSingle().split(",");
      for (String day : days) {
        if (Integer.parseInt(day.trim()) == dayOfWeek) {
          return true;
        }
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DAY_OF_WEEK_IN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateGtCurrentDate(Object fieldValue) {
    try {
      LocalDate date = DateTimeParser.parseDate(fieldValue);
      return date != null && date.isAfter(LocalDate.now());
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateLtCurrentDate(Object fieldValue) {
    try {
      LocalDate date = DateTimeParser.parseDate(fieldValue);
      return date != null && date.isBefore(LocalDate.now());
    } catch (Exception e) {
      return false;
    }
  }

  public static boolean evaluateExpiresWithinDays(Object fieldValue, RuleCondition condition) {
    try {
      int days = Integer.parseInt(condition.getValueSingle().trim());
      LocalDate expiryDate = DateTimeParser.parseDate(fieldValue);
      if (expiryDate == null) {
        return false;
      }
      LocalDate threshold = LocalDate.now().plusDays(days);
      return !expiryDate.isAfter(threshold);
    } catch (Exception e) {
      log.error("Erro ao avaliar EXPIRES_WITHIN_DAYS: {}", e.getMessage());
      return false;
    }
  }
}
