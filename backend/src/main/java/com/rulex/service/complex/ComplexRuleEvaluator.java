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
}
