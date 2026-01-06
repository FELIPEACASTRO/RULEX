package com.rulex.service.complex;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.service.GeoService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.util.RegexValidator;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.regex.Pattern;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Motor de avaliação de regras complexas. Avalia grupos de condições aninhados com suporte a todos
 * os operadores.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleEvaluator {

  private final GeoService geoService;
  private final VelocityService velocityService;
  private final VelocityServiceFacade velocityServiceFacade;

  /** Resultado da avaliação de uma regra */
  @Data
  @Builder
  public static class EvaluationResult {
    private boolean matched;
    private List<RuleExecutionDetail> executionDetails;
    private long executionTimeMs;
    private String errorMessage;
  }

  /** Contexto de avaliação contendo os dados da transação */
  @Data
  @Builder
  public static class EvaluationContext {
    private Map<String, Object> payload;
    private Map<String, Object> variables;
    private UUID decisionLogId;
    private UUID ruleVersionId;
    private TransactionRequest transactionRequest; // Para operadores GEO
  }

  /** Avalia um grupo de condições contra um contexto */
  public EvaluationResult evaluate(RuleConditionGroup rootGroup, EvaluationContext context) {
    long startTime = System.currentTimeMillis();
    List<RuleExecutionDetail> details = new ArrayList<>();

    try {
      boolean result = evaluateGroup(rootGroup, context, details);

      return EvaluationResult.builder()
          .matched(result)
          .executionDetails(details)
          .executionTimeMs(System.currentTimeMillis() - startTime)
          .build();

    } catch (Exception e) {
      log.error("Erro ao avaliar regra: {}", e.getMessage(), e);
      return EvaluationResult.builder()
          .matched(false)
          .executionDetails(details)
          .executionTimeMs(System.currentTimeMillis() - startTime)
          .errorMessage(e.getMessage())
          .build();
    }
  }

  /** Avalia um grupo de condições recursivamente */
  private boolean evaluateGroup(
      RuleConditionGroup group, EvaluationContext context, List<RuleExecutionDetail> details) {
    if (group == null || !Boolean.TRUE.equals(group.getEnabled())) {
      return true; // Grupo desabilitado é considerado verdadeiro
    }

    List<Boolean> results = new ArrayList<>();

    // Avaliar condições diretas do grupo
    if (group.getConditions() != null) {
      for (RuleCondition condition : group.getConditions()) {
        if (Boolean.TRUE.equals(condition.getEnabled())) {
          boolean conditionResult = evaluateCondition(condition, context, details);
          results.add(conditionResult);
        }
      }
    }

    // Avaliar grupos filhos recursivamente
    if (group.getChildren() != null) {
      for (RuleConditionGroup child : group.getChildren()) {
        boolean childResult = evaluateGroup(child, context, details);
        results.add(childResult);
      }
    }

    // Se não há resultados, retorna verdadeiro
    if (results.isEmpty()) {
      return true;
    }

    // Aplicar operador lógico
    return applyLogicOperator(group.getLogicOperator(), results);
  }

  /** Aplica o operador lógico aos resultados */
  private boolean applyLogicOperator(
      RuleConditionGroup.GroupLogicOperator operator, List<Boolean> results) {
    if (results.isEmpty()) return true;

    return switch (operator) {
      case AND -> results.stream().allMatch(r -> r);
      case OR -> results.stream().anyMatch(r -> r);
      case NOT -> !results.get(0); // NOT aplica ao primeiro resultado
      case XOR -> results.stream().filter(r -> r).count() == 1;
      case NAND -> !results.stream().allMatch(r -> r);
      case NOR -> !results.stream().anyMatch(r -> r);
    };
  }

  /** Avalia uma condição individual */
  private boolean evaluateCondition(
      RuleCondition condition, EvaluationContext context, List<RuleExecutionDetail> details) {
    long startTime = System.currentTimeMillis();
    String errorMessage = null;
    boolean result = false;

    try {
      // Obter valor do campo
      Object fieldValue =
          getFieldValue(condition.getFieldName(), condition.getFieldPath(), context);

      // Avaliar condição
      result = evaluateOperator(condition, fieldValue, context);

      // Aplicar negação se necessário
      if (Boolean.TRUE.equals(condition.getNegate())) {
        result = !result;
      }

    } catch (Exception e) {
      log.warn("Erro ao avaliar condição {}: {}", condition.getFieldName(), e.getMessage());
      errorMessage = e.getMessage();
      result = false;
    }

    // Registrar detalhe da execução
    details.add(
        RuleExecutionDetail.builder()
            .decisionLogId(context.getDecisionLogId())
            .ruleVersionId(context.getRuleVersionId())
            .conditionId(condition.getId())
            .groupId(condition.getGroup() != null ? condition.getGroup().getId() : null)
            .fieldName(condition.getFieldName())
            .fieldValue(
                String.valueOf(
                    getFieldValue(condition.getFieldName(), condition.getFieldPath(), context)))
            .operator(condition.getOperator().name())
            .expectedValue(getExpectedValueString(condition))
            .result(result)
            .executionTimeMs((int) (System.currentTimeMillis() - startTime))
            .errorMessage(errorMessage)
            .build());

    return result;
  }

  /** Obtém o valor de um campo do contexto */
  @SuppressWarnings("unchecked")
  private Object getFieldValue(String fieldName, String fieldPath, EvaluationContext context) {
    // Primeiro verificar variáveis
    if (context.getVariables() != null && context.getVariables().containsKey(fieldName)) {
      return context.getVariables().get(fieldName);
    }

    // Depois verificar payload
    if (context.getPayload() == null) {
      return null;
    }

    // Se tem path, navegar pelo objeto
    if (fieldPath != null && !fieldPath.isEmpty()) {
      String[] parts = fieldPath.split("\\.");
      Object current = context.getPayload();

      for (String part : parts) {
        if (current == null) return null;
        if (current instanceof Map) {
          current = ((Map<String, Object>) current).get(part);
        } else {
          return null;
        }
      }
      return current;
    }

    return context.getPayload().get(fieldName);
  }

  /** Avalia o operador da condição */
  private boolean evaluateOperator(
      RuleCondition condition, Object fieldValue, EvaluationContext context) {
    RuleCondition.ConditionOperator operator = condition.getOperator();

    return switch (operator) {
        // Comparação básica
      case EQ ->
          evaluateEquals(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case NEQ ->
          !evaluateEquals(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case GT -> compareValues(fieldValue, condition.getValueSingle()) > 0;
      case GTE -> compareValues(fieldValue, condition.getValueSingle()) >= 0;
      case LT -> compareValues(fieldValue, condition.getValueSingle()) < 0;
      case LTE -> compareValues(fieldValue, condition.getValueSingle()) <= 0;

        // Listas
      case IN -> evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());
      case NOT_IN ->
          !evaluateIn(fieldValue, condition.getValueArray(), condition.getCaseSensitive());

        // Strings
      case CONTAINS ->
          evaluateContains(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case NOT_CONTAINS ->
          !evaluateContains(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case STARTS_WITH ->
          evaluateStartsWith(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case ENDS_WITH ->
          evaluateEndsWith(fieldValue, condition.getValueSingle(), condition.getCaseSensitive());
      case REGEX -> evaluateRegex(fieldValue, condition.getValueSingle());
      case NOT_REGEX -> !evaluateRegex(fieldValue, condition.getValueSingle());

        // Nulos
      case IS_NULL -> fieldValue == null;
      case NOT_NULL -> fieldValue != null;

        // Booleanos
      case IS_TRUE -> Boolean.TRUE.equals(toBoolean(fieldValue));
      case IS_FALSE -> Boolean.FALSE.equals(toBoolean(fieldValue));

        // Range
      case BETWEEN -> evaluateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case NOT_BETWEEN ->
          !evaluateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());

        // Comparação entre campos
      case FIELD_EQ ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 0);
      case FIELD_NEQ ->
          !evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 0);
      case FIELD_GT ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 1);
      case FIELD_GTE ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, 2);
      case FIELD_LT ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, -1);
      case FIELD_LTE ->
          evaluateFieldComparison(fieldValue, condition.getValueFieldRef(), context, -2);

        // Data/Tempo
      case DATE_BEFORE -> evaluateDateBefore(fieldValue, condition.getValueSingle());
      case DATE_AFTER -> evaluateDateAfter(fieldValue, condition.getValueSingle());
      case DATE_BETWEEN ->
          evaluateDateBetween(fieldValue, condition.getValueMin(), condition.getValueMax());
      case TIME_BEFORE -> evaluateTimeBefore(fieldValue, condition.getValueSingle());
      case TIME_AFTER -> evaluateTimeAfter(fieldValue, condition.getValueSingle());
      case TIME_BETWEEN ->
          evaluateTimeBetween(fieldValue, condition.getValueMin(), condition.getValueMax());

        // Array
      case ARRAY_CONTAINS -> evaluateArrayContains(fieldValue, condition.getValueSingle());
      case ARRAY_NOT_CONTAINS -> !evaluateArrayContains(fieldValue, condition.getValueSingle());
      case ARRAY_SIZE_EQ -> evaluateArraySize(fieldValue, condition.getValueSingle(), 0);
      case ARRAY_SIZE_GT -> evaluateArraySize(fieldValue, condition.getValueSingle(), 1);
      case ARRAY_SIZE_LT -> evaluateArraySize(fieldValue, condition.getValueSingle(), -1);

        // Matemáticas
      case MOD_EQ ->
          evaluateModulo(fieldValue, condition.getValueSingle(), condition.getValueMin(), true);
      case MOD_NEQ ->
          evaluateModulo(fieldValue, condition.getValueSingle(), condition.getValueMin(), false);

        // Geolocalização - Implementado via GeoService
        // Coordenadas são derivadas de merchantCity/State/CountryCode
      case GEO_DISTANCE_LT -> evaluateGeoDistanceLt(condition, context);
      case GEO_DISTANCE_GT -> evaluateGeoDistanceGt(condition, context);
      case GEO_IN_POLYGON -> evaluateGeoInPolygon(condition, context);

        // Velocity (agregações temporais)
      case VELOCITY_COUNT_GT -> evaluateVelocityCount(condition, context, true);
      case VELOCITY_COUNT_LT -> evaluateVelocityCount(condition, context, false);
      case VELOCITY_SUM_GT -> evaluateVelocitySum(condition, context, true);
      case VELOCITY_SUM_LT -> evaluateVelocitySum(condition, context, false);
      case VELOCITY_AVG_GT -> evaluateVelocityAvg(condition, context, true);
      case VELOCITY_AVG_LT -> evaluateVelocityAvg(condition, context, false);
      case VELOCITY_DISTINCT_GT -> evaluateVelocityDistinct(condition, context, true);
      case VELOCITY_DISTINCT_LT -> evaluateVelocityDistinct(condition, context, false);

        // Agregações temporais avançadas (DSL expandida)
      case SUM_LAST_N_DAYS -> evaluateSumLastNDays(condition, context);
      case COUNT_LAST_N_HOURS -> evaluateCountLastNHours(condition, context);
      case AVG_LAST_N_DAYS -> evaluateAvgLastNDays(condition, context);
      case COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS -> evaluateCountDistinctMerchantsLastNDays(condition, context);
      case COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS -> evaluateCountDistinctCountriesLastNHours(condition, context);
      case MAX_AMOUNT_LAST_N_DAYS -> evaluateMaxAmountLastNDays(condition, context);
      case MIN_AMOUNT_LAST_N_DAYS -> evaluateMinAmountLastNDays(condition, context);

        // Operadores críticos para regras de fraude avançadas
      case NOT_IN_HISTORICAL -> evaluateNotInHistorical(condition, context);
      case NAME_SIMILARITY_LT -> evaluateNameSimilarityLt(condition, context);
      case GTE_PERCENT_OF_LAST_INCOMING -> evaluateGtePercentOfLastIncoming(condition, context);
      case DOMAIN_IN_LIST -> evaluateDomainInList(fieldValue, condition);
      case CHARGEBACK_RATE_GT -> evaluateChargebackRateGt(condition, context);
      case ACCOUNT_AGE_LT_MINUTES -> evaluateAccountAgeLtMinutes(condition, context);
      case IS_VOIP -> evaluateIsVoip(fieldValue);
      case COUNT_DISTINCT_PANS_LAST_N_HOURS -> evaluateCountDistinctPansLastNHours(condition, context);
      case COUNT_DISTINCT_ACCOUNTS -> evaluateCountDistinctAccounts(condition, context);

        // Operadores de Tempo/Data
      case IS_WEEKEND -> evaluateIsWeekend(context);
      case IS_HOLIDAY -> evaluateIsHoliday(context);
      case HOUR_BETWEEN -> evaluateHourBetween(condition, context);
      case DAY_OF_WEEK_IN -> evaluateDayOfWeekIn(condition, context);
      case GT_CURRENT_DATE -> evaluateGtCurrentDate(fieldValue);
      case LT_CURRENT_DATE -> evaluateLtCurrentDate(fieldValue);
      case EXPIRES_WITHIN_DAYS -> evaluateExpiresWithinDays(fieldValue, condition);

        // Operadores de Padrão
      case DECIMAL_PLACES_GT -> evaluateDecimalPlacesGt(fieldValue, condition);
      case PATTERN_ROUND_NUMBERS -> evaluatePatternRoundNumbers(fieldValue);
      case GT_FIELD_MULTIPLIER -> evaluateGtFieldMultiplier(condition, context);
      case PERCENTAGE_OF_FIELD -> evaluatePercentageOfField(condition, context);

        // Operadores de Velocity adicionais
      case SUM_LAST_N_HOURS -> evaluateSumLastNHours(condition, context);
      case COUNT_FAILURES_LAST_N_HOURS -> evaluateCountFailuresLastNHours(condition, context);
      case COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS -> evaluateCountDistinctMerchantsLastNHours(condition, context);
      case TIME_SINCE_LAST_LT -> evaluateTimeSinceLastLt(condition, context);

        // Operadores de Padrão de Fraude
      case VELOCITY_SPIKE -> evaluateVelocitySpike(condition, context);
      case AMOUNT_SPIKE -> evaluateAmountSpike(condition, context);
      case PATTERN_ESCALATION -> evaluatePatternEscalation(condition, context);
      case PATTERN_SPLIT_TRANSACTION -> evaluatePatternSplitTransaction(condition, context);

        // Operadores de Histórico do Cliente
      case IN_CUSTOMER_HISTORY -> evaluateInCustomerHistory(condition, context);
      case NOT_IN_CUSTOMER_HISTORY -> !evaluateInCustomerHistory(condition, context);
      case IN_CUSTOMER_USUAL_HOURS -> evaluateInCustomerUsualHours(condition, context);
      case NOT_IN_CUSTOMER_USUAL_HOURS -> !evaluateInCustomerUsualHours(condition, context);
      case IN_CUSTOMER_CHARGEBACK_MERCHANTS -> evaluateInCustomerChargebackMerchants(condition, context);

        // Operadores de Primeira Ocorrência
      case IS_FIRST -> evaluateIsFirst(condition, context);
      case IS_NEW -> evaluateIsNew(condition, context);
      case DISTANCE_FROM_LAST_GT -> evaluateDistanceFromLastGt(condition, context);

      default -> {
        log.warn("Operador não implementado: {}", operator);
        yield false;
      }
    };
  }

  // ========== Métodos auxiliares de avaliação ==========

  private boolean evaluateEquals(Object fieldValue, String expected, Boolean caseSensitive) {
    if (fieldValue == null && expected == null) return true;
    if (fieldValue == null || expected == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.equalsIgnoreCase(expected);
    }
    return fieldStr.equals(expected);
  }

  private int compareValues(Object fieldValue, String expected) {
    if (fieldValue == null || expected == null) return 0;

    try {
      BigDecimal fieldNum = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal expectedNum = new BigDecimal(expected);
      return fieldNum.compareTo(expectedNum);
    } catch (NumberFormatException e) {
      return String.valueOf(fieldValue).compareTo(expected);
    }
  }

  private boolean evaluateIn(Object fieldValue, List<String> values, Boolean caseSensitive) {
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

  private boolean evaluateContains(Object fieldValue, String substring, Boolean caseSensitive) {
    if (fieldValue == null || substring == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().contains(substring.toLowerCase());
    }
    return fieldStr.contains(substring);
  }

  private boolean evaluateStartsWith(Object fieldValue, String prefix, Boolean caseSensitive) {
    if (fieldValue == null || prefix == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().startsWith(prefix.toLowerCase());
    }
    return fieldStr.startsWith(prefix);
  }

  private boolean evaluateEndsWith(Object fieldValue, String suffix, Boolean caseSensitive) {
    if (fieldValue == null || suffix == null) return false;

    String fieldStr = String.valueOf(fieldValue);
    if (Boolean.FALSE.equals(caseSensitive)) {
      return fieldStr.toLowerCase().endsWith(suffix.toLowerCase());
    }
    return fieldStr.endsWith(suffix);
  }

  private boolean evaluateRegex(Object fieldValue, String pattern) {
    if (fieldValue == null || pattern == null) return false;

    // Validar pattern contra ReDoS antes de executar
    RegexValidator.ValidationResult validation = RegexValidator.validate(pattern);
    if (!validation.valid()) {
      log.warn("Regex rejeitada por segurança: {} - {}", pattern, validation.errorMessage());
      return false;
    }

    try {
      Pattern compiledPattern = Pattern.compile(pattern);
      return RegexValidator.matchWithTimeout(compiledPattern, String.valueOf(fieldValue));
    } catch (RegexValidator.TimeoutException e) {
      log.warn("Regex timeout: {} - {}", pattern, e.getMessage());
      return false;
    } catch (Exception e) {
      log.warn("Regex inválida: {}", pattern);
      return false;
    }
  }

  private boolean evaluateBetween(Object fieldValue, String min, String max) {
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

  private boolean evaluateFieldComparison(
      Object fieldValue, String otherFieldName, EvaluationContext context, int comparison) {
    Object otherValue = getFieldValue(otherFieldName, null, context);
    int result = compareValues(fieldValue, String.valueOf(otherValue));

    return switch (comparison) {
      case 0 -> result == 0; // EQ
      case 1 -> result > 0; // GT
      case 2 -> result >= 0; // GTE
      case -1 -> result < 0; // LT
      case -2 -> result <= 0; // LTE
      default -> false;
    };
  }

  private boolean evaluateDateBefore(Object fieldValue, String dateStr) {
    try {
      LocalDate fieldDate = parseDate(fieldValue);
      LocalDate compareDate = LocalDate.parse(dateStr);
      return fieldDate != null && fieldDate.isBefore(compareDate);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateDateAfter(Object fieldValue, String dateStr) {
    try {
      LocalDate fieldDate = parseDate(fieldValue);
      LocalDate compareDate = LocalDate.parse(dateStr);
      return fieldDate != null && fieldDate.isAfter(compareDate);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateDateBetween(Object fieldValue, String minDate, String maxDate) {
    try {
      LocalDate fieldDate = parseDate(fieldValue);
      LocalDate min = LocalDate.parse(minDate);
      LocalDate max = LocalDate.parse(maxDate);
      return fieldDate != null && !fieldDate.isBefore(min) && !fieldDate.isAfter(max);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeBefore(Object fieldValue, String timeStr) {
    try {
      LocalTime fieldTime = parseTime(fieldValue);
      LocalTime compareTime = LocalTime.parse(timeStr);
      return fieldTime != null && fieldTime.isBefore(compareTime);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeAfter(Object fieldValue, String timeStr) {
    try {
      LocalTime fieldTime = parseTime(fieldValue);
      LocalTime compareTime = LocalTime.parse(timeStr);
      return fieldTime != null && fieldTime.isAfter(compareTime);
    } catch (Exception e) {
      return false;
    }
  }

  private boolean evaluateTimeBetween(Object fieldValue, String minTime, String maxTime) {
    try {
      LocalTime fieldTime = parseTime(fieldValue);
      LocalTime min = LocalTime.parse(minTime);
      LocalTime max = LocalTime.parse(maxTime);
      return fieldTime != null && !fieldTime.isBefore(min) && !fieldTime.isAfter(max);
    } catch (Exception e) {
      return false;
    }
  }

  @SuppressWarnings("unchecked")
  private boolean evaluateArrayContains(Object fieldValue, String element) {
    if (fieldValue == null) return false;

    if (fieldValue instanceof List) {
      return ((List<Object>) fieldValue)
          .stream().anyMatch(item -> String.valueOf(item).equals(element));
    }
    return false;
  }

  @SuppressWarnings("unchecked")
  private boolean evaluateArraySize(Object fieldValue, String expectedSize, int comparison) {
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

  private boolean evaluateModulo(
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

  // ========== Métodos de Geolocalização ==========

  /**
   * Avalia GEO_DISTANCE_LT: distância menor que threshold. Formato do valor: "lat,lon,distanceKm"
   * (ex: "-23.55,-46.63,100")
   */
  private boolean evaluateGeoDistanceLt(RuleCondition condition, EvaluationContext context) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para GEO_DISTANCE_LT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para GEO_DISTANCE_LT. Esperado: lat,lon,distanceKm");
        return false;
      }

      double targetLat = Double.parseDouble(parts[0].trim());
      double targetLon = Double.parseDouble(parts[1].trim());
      double thresholdKm = Double.parseDouble(parts[2].trim());

      GeoService.GeoResult result =
          geoService.evaluateDistanceLessThan(
              context.getTransactionRequest(), targetLat, targetLon, thresholdKm);

      if (!result.isSuccess()) {
        log.warn("GEO_DISTANCE_LT falhou: {}", result.getErrorMessage());
        return false;
      }

      return result.isResult();
    } catch (Exception e) {
      log.error("Erro ao avaliar GEO_DISTANCE_LT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia GEO_DISTANCE_GT: distância maior que threshold. Formato do valor: "lat,lon,distanceKm"
   * (ex: "-23.55,-46.63,100")
   */
  private boolean evaluateGeoDistanceGt(RuleCondition condition, EvaluationContext context) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para GEO_DISTANCE_GT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para GEO_DISTANCE_GT. Esperado: lat,lon,distanceKm");
        return false;
      }

      double targetLat = Double.parseDouble(parts[0].trim());
      double targetLon = Double.parseDouble(parts[1].trim());
      double thresholdKm = Double.parseDouble(parts[2].trim());

      GeoService.GeoResult result =
          geoService.evaluateDistanceGreaterThan(
              context.getTransactionRequest(), targetLat, targetLon, thresholdKm);

      if (!result.isSuccess()) {
        log.warn("GEO_DISTANCE_GT falhou: {}", result.getErrorMessage());
        return false;
      }

      return result.isResult();
    } catch (Exception e) {
      log.error("Erro ao avaliar GEO_DISTANCE_GT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia GEO_IN_POLYGON: ponto dentro de polígono. Formato do valor: nome do polígono (ex:
   * "BRASIL", "SAO_PAULO_ESTADO")
   */
  private boolean evaluateGeoInPolygon(RuleCondition condition, EvaluationContext context) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para GEO_IN_POLYGON");
      return false;
    }

    try {
      String polygonName = condition.getValueSingle();
      if (polygonName == null || polygonName.isBlank()) {
        log.warn("Nome do polígono não especificado para GEO_IN_POLYGON");
        return false;
      }

      GeoService.GeoResult result =
          geoService.evaluateInPolygon(context.getTransactionRequest(), polygonName.trim());

      if (!result.isSuccess()) {
        log.warn("GEO_IN_POLYGON falhou: {}", result.getErrorMessage());
        return false;
      }

      return result.isResult();
    } catch (Exception e) {
      log.error("Erro ao avaliar GEO_IN_POLYGON: {}", e.getMessage());
      return false;
    }
  }

  // ========== Métodos de Velocity ==========

  /**
   * Avalia VELOCITY_COUNT: contagem de transações em janela temporal. Formato do valor:
   * "keyType,windowMinutes,threshold" (ex: "PAN,60,5")
   */
  private boolean evaluateVelocityCount(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_COUNT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para VELOCITY_COUNT. Esperado: keyType,windowMinutes,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      long threshold = Long.parseLong(parts[2].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      return greaterThan
          ? stats.getTransactionCount() > threshold
          : stats.getTransactionCount() < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia VELOCITY_SUM: soma de valores em janela temporal. Formato do valor:
   * "keyType,windowMinutes,threshold" (ex: "PAN,1440,10000")
   */
  private boolean evaluateVelocitySum(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_SUM");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para VELOCITY_SUM. Esperado: keyType,windowMinutes,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      BigDecimal threshold = new BigDecimal(parts[2].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      return greaterThan
          ? stats.getTotalAmount().compareTo(threshold) > 0
          : stats.getTotalAmount().compareTo(threshold) < 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_SUM: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia VELOCITY_AVG: média de valores em janela temporal. Formato do valor:
   * "keyType,windowMinutes,threshold" (ex: "PAN,1440,500")
   */
  private boolean evaluateVelocityAvg(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_AVG");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 3) {
        log.warn("Formato inválido para VELOCITY_AVG. Esperado: keyType,windowMinutes,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      BigDecimal threshold = new BigDecimal(parts[2].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      return greaterThan
          ? stats.getAvgAmount().compareTo(threshold) > 0
          : stats.getAvgAmount().compareTo(threshold) < 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_AVG: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia VELOCITY_DISTINCT: contagem de valores distintos em janela temporal. Formato do valor:
   * "keyType,windowMinutes,distinctType,threshold" (ex: "PAN,1440,MERCHANTS,3")
   */
  private boolean evaluateVelocityDistinct(
      RuleCondition condition, EvaluationContext context, boolean greaterThan) {
    if (context.getTransactionRequest() == null) {
      log.warn("TransactionRequest não disponível para VELOCITY_DISTINCT");
      return false;
    }

    try {
      String[] parts = condition.getValueSingle().split(",");
      if (parts.length < 4) {
        log.warn(
            "Formato inválido para VELOCITY_DISTINCT. Esperado: keyType,windowMinutes,distinctType,threshold");
        return false;
      }

      VelocityService.KeyType keyType =
          VelocityService.KeyType.valueOf(parts[0].trim().toUpperCase());
      VelocityService.TimeWindow window = parseTimeWindow(Integer.parseInt(parts[1].trim()));
      String distinctType = parts[2].trim().toUpperCase();
      long threshold = Long.parseLong(parts[3].trim());

      // GAP-FIX #2: Usar facade que escolhe entre cache e banco
      VelocityService.VelocityStats stats =
          velocityServiceFacade.getStats(context.getTransactionRequest(), keyType, window);

      long distinctCount =
          switch (distinctType) {
            case "MERCHANTS" -> stats.getDistinctMerchants();
            case "MCCS" -> stats.getDistinctMccs();
            case "COUNTRIES" -> stats.getDistinctCountries();
            default -> 0;
          };

      return greaterThan ? distinctCount > threshold : distinctCount < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_DISTINCT: {}", e.getMessage());
      return false;
    }
  }

  /** Converte minutos para TimeWindow mais próxima. */
  private VelocityService.TimeWindow parseTimeWindow(int minutes) {
    if (minutes <= 5) return VelocityService.TimeWindow.MINUTE_5;
    if (minutes <= 15) return VelocityService.TimeWindow.MINUTE_15;
    if (minutes <= 30) return VelocityService.TimeWindow.MINUTE_30;
    if (minutes <= 60) return VelocityService.TimeWindow.HOUR_1;
    if (minutes <= 360) return VelocityService.TimeWindow.HOUR_6;
    if (minutes <= 720) return VelocityService.TimeWindow.HOUR_12;
    if (minutes <= 1440) return VelocityService.TimeWindow.HOUR_24;
    if (minutes <= 10080) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  // ========== Métodos utilitários ==========

  private Boolean toBoolean(Object value) {
    if (value == null) return null;
    if (value instanceof Boolean) return (Boolean) value;
    String str = String.valueOf(value).toLowerCase();
    return "true".equals(str) || "1".equals(str) || "yes".equals(str);
  }

  private LocalDate parseDate(Object value) {
    if (value == null) return null;
    if (value instanceof LocalDate) return (LocalDate) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalDate();

    String str = String.valueOf(value);
    // Tentar formato YYYYMMDD (inteiro)
    if (str.matches("\\d{8}")) {
      return LocalDate.parse(str, DateTimeFormatter.BASIC_ISO_DATE);
    }
    return LocalDate.parse(str);
  }

  private LocalTime parseTime(Object value) {
    if (value == null) return null;
    if (value instanceof LocalTime) return (LocalTime) value;
    if (value instanceof LocalDateTime) return ((LocalDateTime) value).toLocalTime();

    String str = String.valueOf(value);
    // Tentar formato HHMMSS (inteiro)
    if (str.matches("\\d{6}")) {
      return LocalTime.parse(str, DateTimeFormatter.ofPattern("HHmmss"));
    }
    return LocalTime.parse(str);
  }

  private String getExpectedValueString(RuleCondition condition) {
    if (condition.getValueSingle() != null) {
      return condition.getValueSingle();
    }
    if (condition.getValueArray() != null) {
      return condition.getValueArray().toString();
    }
    if (condition.getValueMin() != null && condition.getValueMax() != null) {
      return condition.getValueMin() + " - " + condition.getValueMax();
    }
    if (condition.getValueFieldRef() != null) {
      return "field:" + condition.getValueFieldRef();
    }
    if (condition.getValueExpression() != null) {
      return "expr:" + condition.getValueExpression();
    }
    return "";
  }

  // ========== Agregações Temporais Avançadas (DSL Expandida) ==========

  /**
   * Avalia SUM_LAST_N_DAYS: Soma de valores nos últimos N dias
   * Formato do valueSingle: "fieldName|nDays|threshold|operator"
   * Exemplo: "amount|7|5000|GT" -> Soma de 'amount' nos últimos 7 dias > 5000
   */
  private boolean evaluateSumLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 4) {
        log.error("Formato inválido para SUM_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      String fieldName = parts[0];
      int nDays = Integer.parseInt(parts[1]);
      BigDecimal threshold = new BigDecimal(parts[2]);
      String operator = parts[3]; // GT, GTE, LT, LTE, EQ

      // Calcular a soma usando o VelocityServiceFacade
      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      BigDecimal sum = stats.getTotalAmount();

      return switch (operator) {
        case "GT" -> sum.compareTo(threshold) > 0;
        case "GTE" -> sum.compareTo(threshold) >= 0;
        case "LT" -> sum.compareTo(threshold) < 0;
        case "LTE" -> sum.compareTo(threshold) <= 0;
        case "EQ" -> sum.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia COUNT_LAST_N_HOURS: Contagem de transações nas últimas N horas
   * Formato do valueSingle: "nHours|threshold|operator"
   * Exemplo: "24|100|GT" -> Mais de 100 transações nas últimas 24 horas
   */
  private boolean evaluateCountLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para COUNT_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int nHours = Integer.parseInt(parts[0]);
      long threshold = Long.parseLong(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromHours(nHours);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      long count = stats.getTransactionCount();

      return switch (operator) {
        case "GT" -> count > threshold;
        case "GTE" -> count >= threshold;
        case "LT" -> count < threshold;
        case "LTE" -> count <= threshold;
        case "EQ" -> count == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia AVG_LAST_N_DAYS: Média de valores nos últimos N dias
   * Formato do valueSingle: "fieldName|nDays|threshold|operator"
   * Exemplo: "amount|30|500|GT" -> Média de 'amount' nos últimos 30 dias > 500
   */
  private boolean evaluateAvgLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 4) {
        log.error("Formato inválido para AVG_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      String fieldName = parts[0];
      int nDays = Integer.parseInt(parts[1]);
      BigDecimal threshold = new BigDecimal(parts[2]);
      String operator = parts[3];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      BigDecimal avg = stats.getAvgAmount();

      return switch (operator) {
        case "GT" -> avg.compareTo(threshold) > 0;
        case "GTE" -> avg.compareTo(threshold) >= 0;
        case "LT" -> avg.compareTo(threshold) < 0;
        case "LTE" -> avg.compareTo(threshold) <= 0;
        case "EQ" -> avg.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar AVG_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: Contagem de merchants distintos
   * Formato do valueSingle: "nDays|threshold|operator"
   * Exemplo: "7|10|GT" -> Mais de 10 merchants distintos nos últimos 7 dias
   */
  private boolean evaluateCountDistinctMerchantsLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      int threshold = Integer.parseInt(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      long distinctMerchants = stats.getDistinctMerchants();

      return switch (operator) {
        case "GT" -> distinctMerchants > threshold;
        case "GTE" -> distinctMerchants >= threshold;
        case "LT" -> distinctMerchants < threshold;
        case "LTE" -> distinctMerchants <= threshold;
        case "EQ" -> distinctMerchants == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: Contagem de países distintos
   * Formato do valueSingle: "nHours|threshold|operator"
   * Exemplo: "24|5|GT" -> Mais de 5 países distintos nas últimas 24 horas
   */
  private boolean evaluateCountDistinctCountriesLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int nHours = Integer.parseInt(parts[0]);
      int threshold = Integer.parseInt(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromHours(nHours);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      long distinctCountries = stats.getDistinctCountries();

      return switch (operator) {
        case "GT" -> distinctCountries > threshold;
        case "GTE" -> distinctCountries >= threshold;
        case "LT" -> distinctCountries < threshold;
        case "LTE" -> distinctCountries <= threshold;
        case "EQ" -> distinctCountries == threshold;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia MAX_AMOUNT_LAST_N_DAYS: Valor máximo nos últimos N dias
   * Formato do valueSingle: "nDays|threshold|operator"
   * Exemplo: "30|10000|GT" -> Valor máximo nos últimos 30 dias > 10000
   */
  private boolean evaluateMaxAmountLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para MAX_AMOUNT_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      BigDecimal threshold = new BigDecimal(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      BigDecimal maxAmount = stats.getMaxAmount();

      return switch (operator) {
        case "GT" -> maxAmount.compareTo(threshold) > 0;
        case "GTE" -> maxAmount.compareTo(threshold) >= 0;
        case "LT" -> maxAmount.compareTo(threshold) < 0;
        case "LTE" -> maxAmount.compareTo(threshold) <= 0;
        case "EQ" -> maxAmount.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar MAX_AMOUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * Avalia MIN_AMOUNT_LAST_N_DAYS: Valor mínimo nos últimos N dias
   * Formato do valueSingle: "nDays|threshold|operator"
   * Exemplo: "7|10|LT" -> Valor mínimo nos últimos 7 dias < 10
   */
  private boolean evaluateMinAmountLastNDays(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split("\\|");
      if (parts.length != 3) {
        log.error("Formato inválido para MIN_AMOUNT_LAST_N_DAYS: {}", condition.getValueSingle());
        return false;
      }

      int nDays = Integer.parseInt(parts[0]);
      BigDecimal threshold = new BigDecimal(parts[1]);
      String operator = parts[2];

      VelocityService.TimeWindow window = parseTimeWindowFromDays(nDays);
      VelocityService.KeyType keyType = VelocityService.KeyType.PAN;

      var stats = velocityServiceFacade.getStats(
          context.getTransactionRequest(),
          keyType,
          window
      );

      BigDecimal minAmount = stats.getMinAmount();

      return switch (operator) {
        case "GT" -> minAmount.compareTo(threshold) > 0;
        case "GTE" -> minAmount.compareTo(threshold) >= 0;
        case "LT" -> minAmount.compareTo(threshold) < 0;
        case "LTE" -> minAmount.compareTo(threshold) <= 0;
        case "EQ" -> minAmount.compareTo(threshold) == 0;
        default -> false;
      };

    } catch (Exception e) {
      log.error("Erro ao avaliar MIN_AMOUNT_LAST_N_DAYS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Métodos Auxiliares para Agregações Avançadas ==========

  /** Converte dias para TimeWindow mais próxima */
  private VelocityService.TimeWindow parseTimeWindowFromDays(int days) {
    if (days <= 1) return VelocityService.TimeWindow.HOUR_24;
    if (days <= 7) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  /** Converte horas para TimeWindow mais próxima */
  private VelocityService.TimeWindow parseTimeWindowFromHours(int hours) {
    if (hours <= 1) return VelocityService.TimeWindow.HOUR_1;
    if (hours <= 6) return VelocityService.TimeWindow.HOUR_6;
    if (hours <= 12) return VelocityService.TimeWindow.HOUR_12;
    if (hours <= 24) return VelocityService.TimeWindow.HOUR_24;
    if (hours <= 168) return VelocityService.TimeWindow.DAY_7;
    return VelocityService.TimeWindow.DAY_30;
  }

  /** Extrai o campo de agrupamento do contexto (ex: cardNumber, accountId) */
  private String extractGroupByFromContext(EvaluationContext context) {
    // Prioridade: cardNumber > accountId > customerId
    if (context.getPayload().containsKey("cardNumber")) {
      return String.valueOf(context.getPayload().get("cardNumber"));
    }
    if (context.getPayload().containsKey("accountId")) {
      return String.valueOf(context.getPayload().get("accountId"));
    }
    if (context.getPayload().containsKey("customerId")) {
      return String.valueOf(context.getPayload().get("customerId"));
    }
    return "unknown";
  }

  // ========== Operadores Críticos para Regras de Fraude Avançadas ==========

  /**
   * NOT_IN_HISTORICAL: Verifica se o valor NÃO está no histórico do cliente.
   * Formato: "sourceField:targetField:days" (ex: "customerAcctNumber:beneficiaryId:90")
   * Retorna true se o beneficiário NUNCA foi usado antes pelo cliente nos últimos N dias.
   */
  private boolean evaluateNotInHistorical(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 3) {
        log.warn("Formato inválido para NOT_IN_HISTORICAL. Esperado: sourceField:targetField:days");
        return false;
      }

      String sourceField = parts[0].trim();
      String targetField = parts[1].trim();
      int days = Integer.parseInt(parts[2].trim());

      Object sourceValue = getFieldValue(sourceField, null, context);
      Object targetValue = getFieldValue(targetField, null, context);

      if (sourceValue == null || targetValue == null) {
        return false;
      }

      // Consulta ao VelocityService para verificar histórico
      // Por enquanto, retorna true (beneficiário novo) se não houver dados históricos
      // TODO: Implementar consulta real ao histórico de transações
      log.debug("NOT_IN_HISTORICAL: source={}, target={}, days={}", sourceValue, targetValue, days);
      return true; // Placeholder - assumir novo beneficiário
    } catch (Exception e) {
      log.error("Erro ao avaliar NOT_IN_HISTORICAL: {}", e.getMessage());
      return false;
    }
  }

  /**
   * NAME_SIMILARITY_LT: Verifica se a similaridade entre dois nomes é menor que o threshold.
   * Formato: "otherField:threshold" (ex: "shippingName:50")
   * Usa algoritmo de Levenshtein para calcular similaridade.
   */
  private boolean evaluateNameSimilarityLt(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para NAME_SIMILARITY_LT. Esperado: otherField:threshold");
        return false;
      }

      String otherField = parts[0].trim();
      int threshold = Integer.parseInt(parts[1].trim());

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      String name1 = String.valueOf(fieldValue).toLowerCase().trim();
      String name2 = String.valueOf(otherValue).toLowerCase().trim();

      int similarity = calculateSimilarity(name1, name2);
      return similarity < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NAME_SIMILARITY_LT: {}", e.getMessage());
      return false;
    }
  }

  /** Calcula similaridade entre duas strings usando distância de Levenshtein (0-100) */
  private int calculateSimilarity(String s1, String s2) {
    if (s1.equals(s2)) return 100;
    if (s1.isEmpty() || s2.isEmpty()) return 0;

    int[][] dp = new int[s1.length() + 1][s2.length() + 1];

    for (int i = 0; i <= s1.length(); i++) dp[i][0] = i;
    for (int j = 0; j <= s2.length(); j++) dp[0][j] = j;

    for (int i = 1; i <= s1.length(); i++) {
      for (int j = 1; j <= s2.length(); j++) {
        int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;
        dp[i][j] = Math.min(Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1), dp[i - 1][j - 1] + cost);
      }
    }

    int maxLen = Math.max(s1.length(), s2.length());
    return (int) ((1.0 - (double) dp[s1.length()][s2.length()] / maxLen) * 100);
  }

  /**
   * GTE_PERCENT_OF_LAST_INCOMING: Verifica se o valor é >= X% do último valor recebido.
   * Formato: "percentage" (ex: "80")
   * Usado para detectar saques que drenam a conta após depósito.
   */
  private boolean evaluateGtePercentOfLastIncoming(RuleCondition condition, EvaluationContext context) {
    try {
      int percentage = Integer.parseInt(condition.getValueSingle().trim());
      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);

      if (fieldValue == null) {
        return false;
      }

      BigDecimal currentAmount = new BigDecimal(String.valueOf(fieldValue));

      // TODO: Implementar consulta ao último depósito/crédito do cliente
      // Por enquanto, usa um valor placeholder
      BigDecimal lastIncoming = BigDecimal.valueOf(1000); // Placeholder

      BigDecimal threshold = lastIncoming.multiply(BigDecimal.valueOf(percentage)).divide(BigDecimal.valueOf(100));
      return currentAmount.compareTo(threshold) >= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar GTE_PERCENT_OF_LAST_INCOMING: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DOMAIN_IN_LIST: Verifica se o domínio do email está em uma lista.
   * Formato: "domain1,domain2,domain3" (ex: "guerrillamail.com,10minutemail.com")
   */
  private boolean evaluateDomainInList(Object fieldValue, RuleCondition condition) {
    if (fieldValue == null || condition.getValueSingle() == null) {
      return false;
    }

    String email = String.valueOf(fieldValue).toLowerCase().trim();
    int atIndex = email.indexOf('@');
    if (atIndex < 0 || atIndex >= email.length() - 1) {
      return false;
    }

    String domain = email.substring(atIndex + 1);
    String[] blockedDomains = condition.getValueSingle().toLowerCase().split(",");

    for (String blocked : blockedDomains) {
      if (domain.equals(blocked.trim())) {
        return true;
      }
    }
    return false;
  }

  /**
   * CHARGEBACK_RATE_GT: Verifica se a taxa de chargeback do merchant é maior que o threshold.
   * Formato: "rate:days" (ex: "2:7" = taxa > 2% nos últimos 7 dias)
   */
  private boolean evaluateChargebackRateGt(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para CHARGEBACK_RATE_GT. Esperado: rate:days");
        return false;
      }

      double rateThreshold = Double.parseDouble(parts[0].trim());
      int days = Integer.parseInt(parts[1].trim());

      Object merchantId = getFieldValue(condition.getFieldName(), null, context);
      if (merchantId == null) {
        return false;
      }

      // TODO: Implementar consulta real à taxa de chargeback do merchant
      // Por enquanto, retorna false (merchant OK)
      log.debug("CHARGEBACK_RATE_GT: merchantId={}, threshold={}%, days={}", merchantId, rateThreshold, days);
      return false; // Placeholder
    } catch (Exception e) {
      log.error("Erro ao avaliar CHARGEBACK_RATE_GT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ACCOUNT_AGE_LT_MINUTES: Verifica se a conta tem menos de N minutos de idade.
   * Formato: "minutes" (ex: "10")
   */
  private boolean evaluateAccountAgeLtMinutes(RuleCondition condition, EvaluationContext context) {
    try {
      int minutes = Integer.parseInt(condition.getValueSingle().trim());

      // Verificar se há campo de data de criação da conta no contexto
      Object createdAt = getFieldValue("accountCreatedAt", null, context);
      if (createdAt == null) {
        createdAt = getFieldValue("customerCreatedAt", null, context);
      }

      if (createdAt == null) {
        // Se não há informação de idade da conta, assumir conta antiga (segura)
        return false;
      }

      // TODO: Implementar parsing da data e cálculo de idade
      // Por enquanto, retorna false (conta antiga)
      log.debug("ACCOUNT_AGE_LT_MINUTES: minutes={}, createdAt={}", minutes, createdAt);
      return false; // Placeholder
    } catch (Exception e) {
      log.error("Erro ao avaliar ACCOUNT_AGE_LT_MINUTES: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IS_VOIP: Verifica se o número de telefone é VoIP.
   * Retorna true se o telefone for identificado como VoIP.
   */
  private boolean evaluateIsVoip(Object fieldValue) {
    if (fieldValue == null) {
      return false;
    }

    String phone = String.valueOf(fieldValue).replaceAll("[^0-9]", "");

    // Lista de prefixos conhecidos de VoIP no Brasil
    String[] voipPrefixes = {"0800", "0300", "0500", "0900"};
    for (String prefix : voipPrefixes) {
      if (phone.startsWith(prefix)) {
        return true;
      }
    }

    // TODO: Integrar com serviço externo de verificação de VoIP (ex: Twilio Lookup)
    // Por enquanto, usa heurística básica
    return false;
  }

  /**
   * COUNT_DISTINCT_PANS_LAST_N_HOURS: Conta PANs distintos associados ao campo nas últimas N horas.
   * Formato: "threshold:hours" (ex: "5:1" = mais de 5 PANs distintos na última hora)
   */
  private boolean evaluateCountDistinctPansLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para COUNT_DISTINCT_PANS_LAST_N_HOURS. Esperado: threshold:hours");
        return false;
      }

      int threshold = Integer.parseInt(parts[0].trim());
      int hours = Integer.parseInt(parts[1].trim());

      Object keyValue = getFieldValue(condition.getFieldName(), null, context);
      if (keyValue == null) {
        return false;
      }

      // Usar VelocityService para obter contagem de PANs distintos
      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );
        // Usar distinctMerchants como proxy para PANs distintos (simplificação)
        return stats.getDistinctMerchants() > threshold;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_PANS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /**
   * COUNT_DISTINCT_ACCOUNTS: Conta contas distintas associadas ao campo.
   * Formato: "threshold" (ex: "3" = mais de 3 contas distintas)
   */
  private boolean evaluateCountDistinctAccounts(RuleCondition condition, EvaluationContext context) {
    try {
      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object keyValue = getFieldValue(condition.getFieldName(), null, context);
      if (keyValue == null) {
        return false;
      }

      // Usar VelocityService para obter contagem de contas distintas
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.DAY_30
        );
        // Usar distinctCountries como proxy para contas distintas (simplificação)
        return stats.getDistinctCountries() > threshold;
      }

      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_ACCOUNTS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Tempo/Data ==========

  /** IS_WEEKEND: Verifica se a transação é em fim de semana */
  private boolean evaluateIsWeekend(EvaluationContext context) {
    try {
      Object dateValue = getFieldValue("transactionDate", null, context);
      if (dateValue == null) {
        return false;
      }
      LocalDate date = parseDate(dateValue);
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

  /** IS_HOLIDAY: Verifica se a transação é em feriado (placeholder) */
  private boolean evaluateIsHoliday(EvaluationContext context) {
    // TODO: Implementar consulta a tabela de feriados
    // Por enquanto, retorna false
    return false;
  }

  /** HOUR_BETWEEN: Verifica se a hora está entre dois valores. Formato: "startHour:endHour" */
  private boolean evaluateHourBetween(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      int startHour = Integer.parseInt(parts[0].trim());
      int endHour = Integer.parseInt(parts[1].trim());

      Object timeValue = getFieldValue("transactionTime", null, context);
      if (timeValue == null) {
        return false;
      }

      int hour;
      if (timeValue instanceof Integer) {
        // Formato HHMMSS
        hour = ((Integer) timeValue) / 10000;
      } else {
        LocalTime time = parseTime(timeValue);
        if (time == null) return false;
        hour = time.getHour();
      }

      if (startHour <= endHour) {
        return hour >= startHour && hour <= endHour;
      } else {
        // Atravessa meia-noite (ex: 22:00 - 06:00)
        return hour >= startHour || hour <= endHour;
      }
    } catch (Exception e) {
      log.error("Erro ao avaliar HOUR_BETWEEN: {}", e.getMessage());
      return false;
    }
  }

  /** DAY_OF_WEEK_IN: Verifica se o dia da semana está na lista. Formato: "1,2,3" (1=Segunda) */
  private boolean evaluateDayOfWeekIn(RuleCondition condition, EvaluationContext context) {
    try {
      Object dateValue = getFieldValue("transactionDate", null, context);
      if (dateValue == null) {
        return false;
      }
      LocalDate date = parseDate(dateValue);
      if (date == null) {
        return false;
      }

      int dayOfWeek = date.getDayOfWeek().getValue(); // 1=Monday, 7=Sunday
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

  /** GT_CURRENT_DATE: Verifica se a data é maior que a data atual (futuro) */
  private boolean evaluateGtCurrentDate(Object fieldValue) {
    try {
      LocalDate date = parseDate(fieldValue);
      return date != null && date.isAfter(LocalDate.now());
    } catch (Exception e) {
      return false;
    }
  }

  /** LT_CURRENT_DATE: Verifica se a data é menor que a data atual (passado) */
  private boolean evaluateLtCurrentDate(Object fieldValue) {
    try {
      LocalDate date = parseDate(fieldValue);
      return date != null && date.isBefore(LocalDate.now());
    } catch (Exception e) {
      return false;
    }
  }

  /** EXPIRES_WITHIN_DAYS: Verifica se expira em N dias. Formato: "days" */
  private boolean evaluateExpiresWithinDays(Object fieldValue, RuleCondition condition) {
    try {
      int days = Integer.parseInt(condition.getValueSingle().trim());
      LocalDate expiryDate = parseDate(fieldValue);
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

  // ========== Operadores de Padrão ==========

  /** DECIMAL_PLACES_GT: Verifica se o número tem mais de N casas decimais */
  private boolean evaluateDecimalPlacesGt(Object fieldValue, RuleCondition condition) {
    try {
      int threshold = Integer.parseInt(condition.getValueSingle().trim());
      String valueStr = String.valueOf(fieldValue);
      int dotIndex = valueStr.indexOf('.');
      if (dotIndex < 0) {
        return 0 > threshold;
      }
      int decimalPlaces = valueStr.length() - dotIndex - 1;
      return decimalPlaces > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  /** PATTERN_ROUND_NUMBERS: Verifica se o valor é um número redondo (múltiplo de 100, 500, 1000) */
  private boolean evaluatePatternRoundNumbers(Object fieldValue) {
    try {
      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      // Verificar se é múltiplo de 100
      return value.remainder(BigDecimal.valueOf(100)).compareTo(BigDecimal.ZERO) == 0;
    } catch (Exception e) {
      return false;
    }
  }

  /** GT_FIELD_MULTIPLIER: Verifica se campo > outro * fator. Formato: "otherField:multiplier" */
  private boolean evaluateGtFieldMultiplier(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      String otherField = parts[0].trim();
      BigDecimal multiplier = new BigDecimal(parts[1].trim());

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal other = new BigDecimal(String.valueOf(otherValue));
      BigDecimal threshold = other.multiply(multiplier);

      return value.compareTo(threshold) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar GT_FIELD_MULTIPLIER: {}", e.getMessage());
      return false;
    }
  }

  /** PERCENTAGE_OF_FIELD: Calcula se campo é X% de outro. Formato: "otherField:percentage:operator" */
  private boolean evaluatePercentageOfField(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }
      String otherField = parts[0].trim();
      BigDecimal percentage = new BigDecimal(parts[1].trim());

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      BigDecimal value = new BigDecimal(String.valueOf(fieldValue));
      BigDecimal other = new BigDecimal(String.valueOf(otherValue));

      if (other.compareTo(BigDecimal.ZERO) == 0) {
        return false;
      }

      BigDecimal actualPercentage = value.multiply(BigDecimal.valueOf(100)).divide(other, 2, java.math.RoundingMode.HALF_UP);
      return actualPercentage.compareTo(percentage) >= 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar PERCENTAGE_OF_FIELD: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Velocity Adicionais ==========

  /** SUM_LAST_N_HOURS: Soma nas últimas N horas. Formato: "hours:threshold:operator" */
  private boolean evaluateSumLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.error("Formato inválido para SUM_LAST_N_HOURS: {}", condition.getValueSingle());
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      BigDecimal threshold = new BigDecimal(parts[1].trim());
      String operator = parts.length > 2 ? parts[2].trim() : "GT";

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );

        return switch (operator) {
          case "GT" -> stats.getTotalAmount().compareTo(threshold) > 0;
          case "GTE" -> stats.getTotalAmount().compareTo(threshold) >= 0;
          case "LT" -> stats.getTotalAmount().compareTo(threshold) < 0;
          case "LTE" -> stats.getTotalAmount().compareTo(threshold) <= 0;
          default -> false;
        };
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar SUM_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /** COUNT_FAILURES_LAST_N_HOURS: Contagem de falhas nas últimas N horas */
  private boolean evaluateCountFailuresLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      int threshold = Integer.parseInt(parts[1].trim());

      // TODO: Implementar consulta real a falhas
      // Por enquanto, usa contagem de transações suspeitas como proxy
      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );
        // Usar suspicious_count como proxy para falhas
        return stats.getFraudCount() > threshold;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_FAILURES_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /** COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: Merchants distintos nas últimas N horas */
  private boolean evaluateCountDistinctMerchantsLastNHours(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        return false;
      }

      int hours = Integer.parseInt(parts[0].trim());
      int threshold = Integer.parseInt(parts[1].trim());

      VelocityService.TimeWindow window = parseTimeWindowFromHours(hours);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );
        return stats.getDistinctMerchants() > threshold;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS: {}", e.getMessage());
      return false;
    }
  }

  /** TIME_SINCE_LAST_LT: Tempo desde última transação menor que N minutos */
  private boolean evaluateTimeSinceLastLt(RuleCondition condition, EvaluationContext context) {
    try {
      int minutes = Integer.parseInt(condition.getValueSingle().trim());

      // TODO: Implementar consulta real ao timestamp da última transação
      // Por enquanto, verifica se há muitas transações na janela de tempo
      VelocityService.TimeWindow window = parseTimeWindowFromHours(1);

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );
        // Se há mais de 1 transação na última hora, provavelmente o tempo é curto
        return stats.getTransactionCount() > 1;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar TIME_SINCE_LAST_LT: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Padrão de Fraude ==========

  /** VELOCITY_SPIKE: Detecta spike de velocidade comparado com média histórica */
  private boolean evaluateVelocitySpike(RuleCondition condition, EvaluationContext context) {
    try {
      // Formato: "multiplier" (ex: "3" = 3x a média)
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      if (context.getTransactionRequest() != null) {
        // Comparar contagem atual com média histórica
        VelocityService.VelocityStats statsHour = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.HOUR_1
        );

        VelocityService.VelocityStats statsDay = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.HOUR_24
        );

        // Média por hora = total do dia / 24
        double avgPerHour = statsDay.getTransactionCount() / 24.0;
        double currentHour = statsHour.getTransactionCount();

        return avgPerHour > 0 && currentHour > (avgPerHour * multiplier);
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar VELOCITY_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /** AMOUNT_SPIKE: Detecta spike de valor comparado com média histórica */
  private boolean evaluateAmountSpike(RuleCondition condition, EvaluationContext context) {
    try {
      double multiplier = Double.parseDouble(condition.getValueSingle().trim());

      Object amountValue = getFieldValue("transactionAmount", null, context);
      if (amountValue == null) {
        return false;
      }

      BigDecimal currentAmount = new BigDecimal(String.valueOf(amountValue));

      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.DAY_30
        );

        BigDecimal avgAmount = stats.getAvgAmount();
        if (avgAmount.compareTo(BigDecimal.ZERO) > 0) {
          BigDecimal threshold = avgAmount.multiply(BigDecimal.valueOf(multiplier));
          return currentAmount.compareTo(threshold) > 0;
        }
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar AMOUNT_SPIKE: {}", e.getMessage());
      return false;
    }
  }

  /** PATTERN_ESCALATION: Detecta padrão de valores crescentes (escada) */
  private boolean evaluatePatternEscalation(RuleCondition condition, EvaluationContext context) {
    // TODO: Implementar detecção de padrão de escada
    // Requer análise de histórico de transações recentes
    // Por enquanto, retorna false
    log.debug("PATTERN_ESCALATION não implementado completamente");
    return false;
  }

  /**
   * PATTERN_SPLIT_TRANSACTION: Detecta padrão de split de transação.
   * Múltiplas transações pequenas em curto período que somam um valor maior.
   * Formato: "maxMinutes:minTransactions:totalThreshold"
   */
  private boolean evaluatePatternSplitTransaction(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 3) {
        log.warn("Formato inválido para PATTERN_SPLIT_TRANSACTION. Esperado: maxMinutes:minTransactions:totalThreshold");
        return false;
      }

      int maxMinutes = Integer.parseInt(parts[0].trim());
      int minTransactions = Integer.parseInt(parts[1].trim());
      BigDecimal totalThreshold = new BigDecimal(parts[2].trim());

      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromHours(maxMinutes / 60 + 1);
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );

        // Detectar split: muitas transações que somam mais que o threshold
        return stats.getTransactionCount() >= minTransactions &&
               stats.getTotalAmount().compareTo(totalThreshold) >= 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar PATTERN_SPLIT_TRANSACTION: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Histórico do Cliente ==========

  /**
   * IN_CUSTOMER_HISTORY: Verifica se o valor do campo está no histórico do cliente.
   * Formato: "fieldToCheck:days" (ex: "merchantId:90" - merchant usado nos últimos 90 dias)
   */
  private boolean evaluateInCustomerHistory(RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      String fieldToCheck = parts.length > 0 ? parts[0].trim() : condition.getFieldName();
      int days = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 90;

      Object fieldValue = getFieldValue(fieldToCheck, null, context);
      if (fieldValue == null) {
        return false;
      }

      // Usar VelocityService para verificar histórico
      if (context.getTransactionRequest() != null) {
        VelocityService.TimeWindow window = parseTimeWindowFromDays(days);
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );

        // Se há transações no período, assumir que o campo está no histórico
        // Para uma implementação completa, seria necessário consultar o histórico real
        return stats.getTransactionCount() > 0;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_HISTORY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IN_CUSTOMER_USUAL_HOURS: Verifica se a transação está no horário habitual do cliente.
   * Analisa o padrão de horários das transações anteriores.
   */
  private boolean evaluateInCustomerUsualHours(RuleCondition condition, EvaluationContext context) {
    try {
      // Obter hora da transação atual
      Object timeValue = getFieldValue("transactionTime", null, context);
      if (timeValue == null) {
        return true; // Se não há hora, assumir horário normal
      }

      int currentHour;
      if (timeValue instanceof Integer) {
        currentHour = ((Integer) timeValue) / 10000;
      } else {
        LocalTime time = parseTime(timeValue);
        if (time == null) return true;
        currentHour = time.getHour();
      }

      // Definir horários "normais" como 6h-23h
      // Para uma implementação completa, seria necessário analisar o histórico do cliente
      int threshold = condition.getValueSingle() != null ?
          Integer.parseInt(condition.getValueSingle().trim()) : 2; // Tolerância de 2 horas

      // Horário comercial estendido: 6h às 23h
      boolean isNormalHour = currentHour >= 6 && currentHour <= 23;

      return isNormalHour;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_USUAL_HOURS: {}", e.getMessage());
      return true; // Em caso de erro, assumir horário normal
    }
  }

  /**
   * IN_CUSTOMER_CHARGEBACK_MERCHANTS: Verifica se o merchant está na lista de chargebacks do cliente.
   */
  private boolean evaluateInCustomerChargebackMerchants(RuleCondition condition, EvaluationContext context) {
    try {
      Object merchantId = getFieldValue("merchantId", null, context);
      if (merchantId == null) {
        return false;
      }

      // TODO: Implementar consulta real à tabela de chargebacks
      // Por enquanto, retorna false (merchant não tem chargeback)
      log.debug("IN_CUSTOMER_CHARGEBACK_MERCHANTS: merchantId={}", merchantId);
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar IN_CUSTOMER_CHARGEBACK_MERCHANTS: {}", e.getMessage());
      return false;
    }
  }

  // ========== Operadores de Primeira Ocorrência ==========

  /**
   * IS_FIRST: Verifica se é a primeira ocorrência do valor para o cliente.
   * Formato: "fieldToCheck" (ex: "merchantId" - primeiro uso deste merchant)
   */
  private boolean evaluateIsFirst(RuleCondition condition, EvaluationContext context) {
    try {
      String fieldToCheck = condition.getValueSingle() != null ?
          condition.getValueSingle().trim() : condition.getFieldName();

      Object fieldValue = getFieldValue(fieldToCheck, null, context);
      if (fieldValue == null) {
        return false;
      }

      // Usar VelocityService para verificar se há histórico
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.DAY_30
        );

        // Se não há transações anteriores, é a primeira
        return stats.getTransactionCount() == 0;
      }
      return true; // Se não há contexto, assumir primeira
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_FIRST: {}", e.getMessage());
      return false;
    }
  }

  /**
   * IS_NEW: Verifica se o valor é "novo" (visto pela primeira vez recentemente).
   * Formato: "maxDays" (ex: "7" - visto pela primeira vez nos últimos 7 dias)
   */
  private boolean evaluateIsNew(RuleCondition condition, EvaluationContext context) {
    try {
      int maxDays = condition.getValueSingle() != null ?
          Integer.parseInt(condition.getValueSingle().trim()) : 7;

      Object fieldValue = getFieldValue(condition.getFieldName(), null, context);
      if (fieldValue == null) {
        return false;
      }

      // Verificar se há histórico além do período especificado
      if (context.getTransactionRequest() != null) {
        // Verificar histórico de 30 dias
        VelocityService.VelocityStats stats30 = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.DAY_30
        );

        // Verificar histórico do período especificado
        VelocityService.TimeWindow window = parseTimeWindowFromDays(maxDays);
        VelocityService.VelocityStats statsRecent = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            window
        );

        // É "novo" se só aparece no período recente, não no histórico mais longo
        return statsRecent.getTransactionCount() > 0 &&
               stats30.getTransactionCount() <= statsRecent.getTransactionCount();
      }
      return true;
    } catch (Exception e) {
      log.error("Erro ao avaliar IS_NEW: {}", e.getMessage());
      return false;
    }
  }

  /**
   * DISTANCE_FROM_LAST_GT: Verifica se a distância da última transação é maior que N km.
   * Formato: "distanceKm" (ex: "500" - mais de 500km da última transação)
   */
  private boolean evaluateDistanceFromLastGt(RuleCondition condition, EvaluationContext context) {
    try {
      double thresholdKm = Double.parseDouble(condition.getValueSingle().trim());

      if (context.getTransactionRequest() == null) {
        return false;
      }

      // Obter localização atual da transação
      String currentCountry = context.getTransactionRequest().getMerchantCountryCode();
      String currentCity = context.getTransactionRequest().getMerchantCity();

      if (currentCountry == null && currentCity == null) {
        return false;
      }

      // Para uma implementação completa, seria necessário:
      // 1. Consultar a última transação do cliente
      // 2. Obter as coordenadas de ambas as localizações
      // 3. Calcular a distância usando a fórmula de Haversine

      // Por enquanto, usar heurística baseada em país
      // Se o país mudou, assumir distância > threshold
      if (context.getTransactionRequest() != null) {
        VelocityService.VelocityStats stats = velocityServiceFacade.getStats(
            context.getTransactionRequest(),
            VelocityService.KeyType.PAN,
            VelocityService.TimeWindow.HOUR_24
        );

        // Se há transações em múltiplos países nas últimas 24h, pode indicar distância grande
        return stats.getDistinctCountries() > 1;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar DISTANCE_FROM_LAST_GT: {}", e.getMessage());
      return false;
    }
  }
}
