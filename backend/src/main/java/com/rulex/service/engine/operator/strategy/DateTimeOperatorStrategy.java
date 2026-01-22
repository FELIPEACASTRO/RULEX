package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de data e tempo.
 * 
 * <p>Operadores suportados:
 * <ul>
 *   <li>DATE_BEFORE - data anterior a</li>
 *   <li>DATE_AFTER - data posterior a</li>
 *   <li>DATE_BETWEEN - data entre duas datas</li>
 *   <li>TIME_BEFORE - hora anterior a</li>
 *   <li>TIME_AFTER - hora posterior a</li>
 *   <li>TIME_BETWEEN - hora entre dois horários</li>
 * </ul>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class DateTimeOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.DATE_BEFORE,
        ConditionOperator.DATE_AFTER,
        ConditionOperator.DATE_BETWEEN,
        ConditionOperator.TIME_BEFORE,
        ConditionOperator.TIME_AFTER,
        ConditionOperator.TIME_BETWEEN
    );

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE;
    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ISO_LOCAL_TIME;
    private static final DateTimeFormatter DATETIME_FORMATTER = DateTimeFormatter.ISO_LOCAL_DATE_TIME;

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "DATETIME";
    }

    @Override
    public int priority() {
        return 40;
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();

        if (fieldValue == null) {
            return false;
        }

        return switch (operator) {
            case DATE_BEFORE -> evaluateDateBefore(fieldValue, condition);
            case DATE_AFTER -> evaluateDateAfter(fieldValue, condition);
            case DATE_BETWEEN -> evaluateDateBetween(fieldValue, condition);
            case TIME_BEFORE -> evaluateTimeBefore(fieldValue, condition);
            case TIME_AFTER -> evaluateTimeAfter(fieldValue, condition);
            case TIME_BETWEEN -> evaluateTimeBetween(fieldValue, condition);
            default -> {
                log.warn("Unexpected operator in DateTimeOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    private boolean evaluateDateBefore(Object fieldValue, RuleCondition condition) {
        LocalDate fieldDate = parseDate(fieldValue);
        LocalDate targetDate = parseDate(condition.getValueSingle());
        
        if (fieldDate == null || targetDate == null) {
            return false;
        }
        
        return fieldDate.isBefore(targetDate);
    }

    private boolean evaluateDateAfter(Object fieldValue, RuleCondition condition) {
        LocalDate fieldDate = parseDate(fieldValue);
        LocalDate targetDate = parseDate(condition.getValueSingle());
        
        if (fieldDate == null || targetDate == null) {
            return false;
        }
        
        return fieldDate.isAfter(targetDate);
    }

    private boolean evaluateDateBetween(Object fieldValue, RuleCondition condition) {
        LocalDate fieldDate = parseDate(fieldValue);
        
        if (fieldDate == null) {
            return false;
        }

        LocalDate minDate = parseDate(condition.getValueMin());
        LocalDate maxDate = parseDate(condition.getValueMax());

        // Fallback para valueSingle no formato "min,max"
        if ((minDate == null || maxDate == null) && condition.getValueSingle() != null) {
            String[] parts = condition.getValueSingle().split(",");
            if (parts.length >= 2) {
                minDate = parseDate(parts[0].trim());
                maxDate = parseDate(parts[1].trim());
            }
        }

        if (minDate == null || maxDate == null) {
            log.warn("DATE_BETWEEN requires min and max dates");
            return false;
        }

        return !fieldDate.isBefore(minDate) && !fieldDate.isAfter(maxDate);
    }

    private boolean evaluateTimeBefore(Object fieldValue, RuleCondition condition) {
        LocalTime fieldTime = parseTime(fieldValue);
        LocalTime targetTime = parseTime(condition.getValueSingle());
        
        if (fieldTime == null || targetTime == null) {
            return false;
        }
        
        return fieldTime.isBefore(targetTime);
    }

    private boolean evaluateTimeAfter(Object fieldValue, RuleCondition condition) {
        LocalTime fieldTime = parseTime(fieldValue);
        LocalTime targetTime = parseTime(condition.getValueSingle());
        
        if (fieldTime == null || targetTime == null) {
            return false;
        }
        
        return fieldTime.isAfter(targetTime);
    }

    private boolean evaluateTimeBetween(Object fieldValue, RuleCondition condition) {
        LocalTime fieldTime = parseTime(fieldValue);
        
        if (fieldTime == null) {
            return false;
        }

        LocalTime minTime = parseTime(condition.getValueMin());
        LocalTime maxTime = parseTime(condition.getValueMax());

        // Fallback para valueSingle no formato "min,max"
        if ((minTime == null || maxTime == null) && condition.getValueSingle() != null) {
            String[] parts = condition.getValueSingle().split(",");
            if (parts.length >= 2) {
                minTime = parseTime(parts[0].trim());
                maxTime = parseTime(parts[1].trim());
            }
        }

        if (minTime == null || maxTime == null) {
            log.warn("TIME_BETWEEN requires min and max times");
            return false;
        }

        // Suporta intervalos que cruzam meia-noite (ex: 22:00 - 06:00)
        if (minTime.isAfter(maxTime)) {
            return !fieldTime.isBefore(minTime) || !fieldTime.isAfter(maxTime);
        }

        return !fieldTime.isBefore(minTime) && !fieldTime.isAfter(maxTime);
    }

    /**
     * Parse de data de vários formatos.
     */
    private LocalDate parseDate(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof LocalDate ld) {
            return ld;
        }

        if (value instanceof LocalDateTime ldt) {
            return ldt.toLocalDate();
        }

        if (value instanceof OffsetDateTime odt) {
            return odt.toLocalDate();
        }

        if (value instanceof Integer intVal) {
            // Formato YYYYMMDD
            try {
                String dateStr = String.valueOf(intVal);
                if (dateStr.length() == 8) {
                    return LocalDate.parse(dateStr, DateTimeFormatter.BASIC_ISO_DATE);
                }
            } catch (DateTimeParseException e) {
                // Tentar outros formatos
            }
        }

        String strValue = String.valueOf(value).trim();
        
        // Tentar vários formatos
        try {
            return LocalDate.parse(strValue, DATE_FORMATTER);
        } catch (DateTimeParseException e1) {
            try {
                return LocalDate.parse(strValue, DateTimeFormatter.BASIC_ISO_DATE);
            } catch (DateTimeParseException e2) {
                try {
                    return LocalDateTime.parse(strValue, DATETIME_FORMATTER).toLocalDate();
                } catch (DateTimeParseException e3) {
                    log.debug("Failed to parse date: {}", strValue);
                    return null;
                }
            }
        }
    }

    /**
     * Parse de hora de vários formatos.
     */
    private LocalTime parseTime(Object value) {
        if (value == null) {
            return null;
        }

        if (value instanceof LocalTime lt) {
            return lt;
        }

        if (value instanceof LocalDateTime ldt) {
            return ldt.toLocalTime();
        }

        if (value instanceof OffsetDateTime odt) {
            return odt.toLocalTime();
        }

        if (value instanceof Integer intVal) {
            // Formato HHmmss
            try {
                String timeStr = String.format("%06d", intVal);
                return LocalTime.parse(timeStr, DateTimeFormatter.ofPattern("HHmmss"));
            } catch (DateTimeParseException e) {
                // Tentar outros formatos
            }
        }

        String strValue = String.valueOf(value).trim();
        
        try {
            return LocalTime.parse(strValue, TIME_FORMATTER);
        } catch (DateTimeParseException e1) {
            try {
                return LocalTime.parse(strValue, DateTimeFormatter.ofPattern("HHmmss"));
            } catch (DateTimeParseException e2) {
                try {
                    return LocalTime.parse(strValue, DateTimeFormatter.ofPattern("HH:mm"));
                } catch (DateTimeParseException e3) {
                    log.debug("Failed to parse time: {}", strValue);
                    return null;
                }
            }
        }
    }
}
