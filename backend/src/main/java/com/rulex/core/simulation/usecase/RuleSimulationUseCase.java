package com.rulex.core.simulation.usecase;

import com.rulex.core.simulation.port.RuleSimulationInputPort;
import com.rulex.core.simulation.port.RuleSimulationRepositoryPort;
import com.rulex.core.simulation.port.RuleSimulationSerializerPort;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.util.RegexValidator;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Builder;
import lombok.Data;

/** Caso de uso para simulação e teste de regras. */
public class RuleSimulationUseCase implements RuleSimulationInputPort {

  private final RuleSimulationRepositoryPort repositoryPort;
  private final RuleSimulationSerializerPort serializerPort;

  public RuleSimulationUseCase(
      RuleSimulationRepositoryPort repositoryPort, RuleSimulationSerializerPort serializerPort) {
    this.repositoryPort = repositoryPort;
    this.serializerPort = serializerPort;
  }

  /** Simula uma regra contra um payload de teste. */
  @Override
  public SimulationResult simulateRule(RuleConfigurationDTO rule, TransactionRequest testPayload) {
    long startTime = System.currentTimeMillis();
    List<ConditionResult> conditionResults = new ArrayList<>();
    boolean allConditionsMet = true;
    boolean anyConditionMet = false;

    List<RuleConditionDTO> conditions = rule.getConditions();
    if (conditions == null || conditions.isEmpty()) {
      return SimulationResult.builder()
          .ruleName(rule.getRuleName())
          .triggered(false)
          .reason("Regra sem condições definidas")
          .conditionResults(conditionResults)
          .processingTimeMs(System.currentTimeMillis() - startTime)
          .build();
    }

    for (RuleConditionDTO condition : conditions) {
      ConditionResult result = evaluateCondition(condition, testPayload);
      conditionResults.add(result);

      if (result.isMet()) {
        anyConditionMet = true;
      } else {
        allConditionsMet = false;
      }
    }

    boolean triggered;
    String logicOperator = rule.getLogicOperator() != null ? rule.getLogicOperator() : "AND";
    if ("OR".equalsIgnoreCase(logicOperator)) {
      triggered = anyConditionMet;
    } else {
      triggered = allConditionsMet;
    }

    String reason =
        triggered
            ? String.format(
                "Regra disparada com classificação %s e peso %d",
                rule.getClassification(), rule.getWeight())
            : "Condições não atendidas";

    return SimulationResult.builder()
        .ruleName(rule.getRuleName())
        .triggered(triggered)
        .classification(triggered ? rule.getClassification() : null)
        .weight(triggered ? rule.getWeight() : 0)
        .reason(reason)
        .conditionResults(conditionResults)
        .logicOperator(logicOperator)
        .processingTimeMs(System.currentTimeMillis() - startTime)
        .build();
  }

  /** Faz backtesting de uma regra contra transações históricas. */
  @Override
  public BacktestResult backtestRule(
      Long ruleId, LocalDateTime startDate, LocalDateTime endDate, int sampleSize) {

    RuleConfiguration rule =
        repositoryPort
            .findRuleById(ruleId)
            .orElseThrow(() -> new IllegalArgumentException("Regra não encontrada: " + ruleId));

    RuleConfigurationDTO ruleDto = convertToDTO(rule);

    // Buscar transações do período (limitado por sample size)
    List<TransactionRequest> transactions =
        getHistoricalTransactions(startDate, endDate, sampleSize);

    long totalEvaluated = 0;
    long totalTriggered = 0;
    long wouldApprove = 0;
    long wouldSuspect = 0;
    long wouldBlock = 0;
    BigDecimal totalAmountAffected = BigDecimal.ZERO;
    List<SimulationResult> sampleResults = new ArrayList<>();

    for (TransactionRequest tx : transactions) {
      totalEvaluated++;
      SimulationResult result = simulateRule(ruleDto, tx);

      if (result.isTriggered()) {
        totalTriggered++;
        if (tx.getTransactionAmount() != null) {
          totalAmountAffected = totalAmountAffected.add(tx.getTransactionAmount());
        }

        switch (result.getClassification()) {
          case "APPROVED" -> wouldApprove++;
          case "SUSPICIOUS" -> wouldSuspect++;
          case "FRAUD" -> wouldBlock++;
          default -> {}
        }
      }

      // Guardar amostra dos primeiros 10 resultados
      if (sampleResults.size() < 10) {
        sampleResults.add(result);
      }
    }

    double triggerRate = totalEvaluated > 0 ? (double) totalTriggered / totalEvaluated * 100 : 0;

    return BacktestResult.builder()
        .ruleId(ruleId)
        .ruleName(rule.getRuleName())
        .startDate(startDate)
        .endDate(endDate)
        .totalEvaluated(totalEvaluated)
        .totalTriggered(totalTriggered)
        .triggerRate(triggerRate)
        .wouldApprove(wouldApprove)
        .wouldSuspect(wouldSuspect)
        .wouldBlock(wouldBlock)
        .totalAmountAffected(totalAmountAffected)
        .sampleResults(sampleResults)
        .build();
  }

  /** Compara duas versões de uma regra. */
  @Override
  public ComparisonResult compareRules(
      RuleConfigurationDTO ruleA,
      RuleConfigurationDTO ruleB,
      List<TransactionRequest> testPayloads) {

    int triggeredByA = 0;
    int triggeredByB = 0;
    int triggeredByBoth = 0;
    int triggeredByNeither = 0;
    List<ComparisonDetail> details = new ArrayList<>();

    for (TransactionRequest payload : testPayloads) {
      SimulationResult resultA = simulateRule(ruleA, payload);
      SimulationResult resultB = simulateRule(ruleB, payload);

      if (resultA.isTriggered() && resultB.isTriggered()) {
        triggeredByBoth++;
      } else if (resultA.isTriggered()) {
        triggeredByA++;
      } else if (resultB.isTriggered()) {
        triggeredByB++;
      } else {
        triggeredByNeither++;
      }

      if (details.size() < 20) {
        details.add(
            ComparisonDetail.builder()
                .transactionId(payload.getExternalTransactionId())
                .triggeredByA(resultA.isTriggered())
                .triggeredByB(resultB.isTriggered())
                .build());
      }
    }

    return ComparisonResult.builder()
        .ruleAName(ruleA.getRuleName())
        .ruleBName(ruleB.getRuleName())
        .totalPayloads(testPayloads.size())
        .triggeredByA(triggeredByA)
        .triggeredByB(triggeredByB)
        .triggeredByBoth(triggeredByBoth)
        .triggeredByNeither(triggeredByNeither)
        .details(details)
        .build();
  }

  /** Avalia uma condição individual. */
  private ConditionResult evaluateCondition(
      RuleConditionDTO condition, TransactionRequest payload) {
    String field = condition.getField();
    String operatorRaw = condition.getOperator();
    String operator = normalizeOperator(operatorRaw);
    String expectedValue = condition.getValue();

    Object actualValue = getFieldValue(payload, field);
    boolean met =
        switch (operator) {
          case "GT_FIELD", "EQ_FIELD", "NE_FIELD", "GTE_FIELD", "LT_FIELD", "LTE_FIELD" -> {
            Object otherValue = getFieldValue(payload, expectedValue);
            yield compareFieldValues(actualValue, otherValue, operator);
          }
          case "PERCENTAGE_OF_FIELD" ->
              evaluatePercentageOfField(actualValue, payload, expectedValue);
          case "MODULO_ZERO" -> evaluateModuloZero(actualValue, expectedValue);
          case "DECIMAL_PLACES_GT" -> evaluateDecimalPlacesGt(actualValue, expectedValue);
          default -> evaluateOperator(actualValue, operator, expectedValue);
        };

    return ConditionResult.builder()
        .field(field)
        .operator(operator)
        .expectedValue(expectedValue)
        .actualValue(actualValue != null ? actualValue.toString() : "null")
        .met(met)
        .build();
  }

  /** Obtém o valor de um campo do payload. */
  private Object getFieldValue(TransactionRequest payload, String field) {
    if (field == null || payload == null) {
      return null;
    }

    try {
      Map<String, Object> map = serializerPort.toPayloadMap(payload);
      return map.get(field);
    } catch (Exception e) {
      return null;
    }
  }

  /** Avalia um operador de comparação. */
  private boolean evaluateOperator(Object actual, String operator, String expected) {
    if (operator == null) {
      return false;
    }

    // Operadores unários
    switch (operator.toUpperCase()) {
      case "IS_NULL":
        return actual == null;
      case "IS_NOT_NULL":
        return actual != null;
      case "IS_TRUE":
        return Boolean.TRUE.equals(actual) || "true".equalsIgnoreCase(String.valueOf(actual));
      case "IS_FALSE":
        return Boolean.FALSE.equals(actual) || "false".equalsIgnoreCase(String.valueOf(actual));
      default:
        break;
    }

    if (actual == null) {
      return false;
    }

    String actualStr = String.valueOf(actual);

    switch (operator.toUpperCase()) {
      case "EQ", "==":
        return actualStr.equals(expected);
      case "NE", "!=":
        return !actualStr.equals(expected);
      case "GT", ">":
        return compareNumeric(actual, expected) > 0;
      case "LT", "<":
        return compareNumeric(actual, expected) < 0;
      case "GTE", ">=":
        return compareNumeric(actual, expected) >= 0;
      case "LTE", "<=":
        return compareNumeric(actual, expected) <= 0;
      case "IN":
        return isInList(actualStr, expected);
      case "NOT_IN":
        return !isInList(actualStr, expected);
      case "CONTAINS":
        return actualStr.contains(expected);
      case "NOT_CONTAINS":
        return !actualStr.contains(expected);
      case "STARTS_WITH":
        return actualStr.startsWith(expected);
      case "ENDS_WITH":
        return actualStr.endsWith(expected);
      case "MATCHES_REGEX":
        // Usa RegexValidator para proteção contra ReDoS
        return RegexValidator.safeMatches(expected, actualStr);
      case "BETWEEN":
        return isBetween(actual, expected);
      case "NOT_BETWEEN":
        return !isBetween(actual, expected);
      default:
        return false;
    }
  }

  private String normalizeOperator(String raw) {
    if (raw == null) {
      return "";
    }
    return switch (raw.trim()) {
      case "==" -> "EQ";
      case "!=" -> "NE";
      case ">" -> "GT";
      case "<" -> "LT";
      case ">=" -> "GTE";
      case "<=" -> "LTE";
      case "IN_LIST" -> "IN";
      case "NOT_IN_LIST" -> "NOT_IN";
      case "NEQ_FIELD" -> "NE_FIELD";
      default -> raw.trim().toUpperCase();
    };
  }

  private boolean compareFieldValues(Object leftValue, Object rightValue, String operator) {
    if (leftValue == null || rightValue == null) {
      return false;
    }

    if (leftValue instanceof Number
        || leftValue instanceof java.math.BigDecimal
        || rightValue instanceof Number
        || rightValue instanceof java.math.BigDecimal) {
      java.math.BigDecimal left = toBigDecimal(leftValue);
      java.math.BigDecimal right = toBigDecimal(rightValue);
      if (left == null || right == null) {
        return false;
      }
      return switch (operator) {
        case "GT_FIELD" -> left.compareTo(right) > 0;
        case "GTE_FIELD" -> left.compareTo(right) >= 0;
        case "LT_FIELD" -> left.compareTo(right) < 0;
        case "LTE_FIELD" -> left.compareTo(right) <= 0;
        case "EQ_FIELD" -> left.compareTo(right) == 0;
        case "NE_FIELD" -> left.compareTo(right) != 0;
        default -> false;
      };
    }

    String left = String.valueOf(leftValue);
    String right = String.valueOf(rightValue);
    return switch (operator) {
      case "EQ_FIELD" -> left.equals(right);
      case "NE_FIELD" -> !left.equals(right);
      case "GT_FIELD" -> left.compareTo(right) > 0;
      case "GTE_FIELD" -> left.compareTo(right) >= 0;
      case "LT_FIELD" -> left.compareTo(right) < 0;
      case "LTE_FIELD" -> left.compareTo(right) <= 0;
      default -> false;
    };
  }

  private java.math.BigDecimal toBigDecimal(Object value) {
    if (value == null) {
      return null;
    }
    if (value instanceof java.math.BigDecimal bd) {
      return bd;
    }
    try {
      return new java.math.BigDecimal(String.valueOf(value));
    } catch (Exception e) {
      return null;
    }
  }

  private boolean evaluatePercentageOfField(
      Object leftValue, TransactionRequest payload, String rawValue) {
    if (leftValue == null || rawValue == null || rawValue.isBlank()) {
      return false;
    }
    String[] parts = rawValue.split(":");
    if (parts.length < 2) {
      return false;
    }

    String otherField = parts[0].trim();
    java.math.BigDecimal min = parseBigDecimalSafe(parts[1]);
    java.math.BigDecimal max = parts.length >= 3 ? parseBigDecimalSafe(parts[2]) : null;
    if (min == null) {
      return false;
    }

    Object otherValue = getFieldValue(payload, otherField);
    java.math.BigDecimal left = toBigDecimal(leftValue);
    java.math.BigDecimal right = toBigDecimal(otherValue);
    if (left == null || right == null || right.compareTo(java.math.BigDecimal.ZERO) == 0) {
      return false;
    }

    java.math.BigDecimal pct =
        left.multiply(java.math.BigDecimal.valueOf(100))
            .divide(right, 6, java.math.RoundingMode.HALF_UP);
    if (max == null) {
      return pct.compareTo(min) >= 0;
    }
    return pct.compareTo(min) >= 0 && pct.compareTo(max) <= 0;
  }

  private java.math.BigDecimal parseBigDecimalSafe(String value) {
    try {
      return new java.math.BigDecimal(value.trim());
    } catch (Exception e) {
      return null;
    }
  }

  private boolean evaluateModuloZero(Object leftValue, String rawValue) {
    java.math.BigDecimal left = toBigDecimal(leftValue);
    java.math.BigDecimal divisor = parseBigDecimalSafe(rawValue == null ? "" : rawValue);
    if (left == null || divisor == null || divisor.compareTo(java.math.BigDecimal.ZERO) == 0) {
      return false;
    }
    return left.remainder(divisor).compareTo(java.math.BigDecimal.ZERO) == 0;
  }

  private boolean evaluateDecimalPlacesGt(Object leftValue, String rawValue) {
    java.math.BigDecimal left = toBigDecimal(leftValue);
    if (left == null) {
      return false;
    }
    int threshold = 0;
    try {
      threshold = Integer.parseInt(rawValue.trim());
    } catch (Exception e) {
      return false;
    }
    String normalized = left.stripTrailingZeros().toPlainString();
    int idx = normalized.indexOf('.');
    int decimals = idx >= 0 ? normalized.length() - idx - 1 : 0;
    return decimals > threshold;
  }

  private int compareNumeric(Object actual, String expected) {
    try {
      double actualNum = Double.parseDouble(String.valueOf(actual));
      double expectedNum = Double.parseDouble(expected);
      return Double.compare(actualNum, expectedNum);
    } catch (NumberFormatException e) {
      return String.valueOf(actual).compareTo(expected);
    }
  }

  private boolean isInList(String value, String listStr) {
    if (listStr == null || listStr.isBlank()) {
      return false;
    }
    // Suporta formatos: "a,b,c" ou "[a,b,c]" ou "['a','b','c']"
    String cleaned = listStr.replaceAll("[\\[\\]'\"\\s]", "");
    String[] items = cleaned.split(",");
    for (String item : items) {
      if (item.trim().equals(value)) {
        return true;
      }
    }
    return false;
  }

  private boolean isBetween(Object actual, String rangeStr) {
    try {
      double actualNum = Double.parseDouble(String.valueOf(actual));
      // Suporta formatos: "10,20" ou "10..20"
      String[] parts = rangeStr.split("[,.]");
      if (parts.length >= 2) {
        double min = Double.parseDouble(parts[0].trim());
        double max = Double.parseDouble(parts[parts.length - 1].trim());
        return actualNum >= min && actualNum <= max;
      }
    } catch (NumberFormatException ignored) {
      // ignore
    }
    return false;
  }

  private List<TransactionRequest> getHistoricalTransactions(
      LocalDateTime startDate, LocalDateTime endDate, int limit) {
    try {
      var pageable =
          org.springframework.data.domain.PageRequest.of(
              0, limit, org.springframework.data.domain.Sort.by("createdAt").descending());

      var page = repositoryPort.findTransactionsByFilters(startDate, endDate, pageable);

      return repositoryPort.toList(page).stream()
          .map(this::convertToTransactionRequest)
          .collect(java.util.stream.Collectors.toList());
    } catch (Exception e) {
      return new ArrayList<>();
    }
  }

  private TransactionRequest convertToTransactionRequest(com.rulex.entity.Transaction tx) {
    TransactionRequest request = new TransactionRequest();

    request.setExternalTransactionId(tx.getExternalTransactionId());
    request.setCustomerIdFromHeader(tx.getCustomerIdFromHeader());
    request.setCustomerAcctNumber(tx.getCustomerAcctNumber());
    request.setPan(tx.getPan());
    request.setMerchantId(tx.getMerchantId());
    request.setMerchantName(tx.getMerchantName());

    request.setTransactionAmount(tx.getTransactionAmount());
    request.setTransactionDate(tx.getTransactionDate());
    request.setTransactionTime(tx.getTransactionTime());
    request.setGmtOffset(tx.getGmtOffset());
    request.setTransactionCurrencyCode(tx.getTransactionCurrencyCode());
    request.setTransactionCurrencyConversionRate(tx.getTransactionCurrencyConversionRate());

    request.setMerchantCountryCode(tx.getMerchantCountryCode());
    request.setMcc(tx.getMcc());

    if (tx.getPosEntryMode() != null) {
      request.setPosEntryMode(tx.getPosEntryMode());
    }

    return request;
  }

  private RuleConfigurationDTO convertToDTO(RuleConfiguration rule) {
    List<RuleConditionDTO> conditions = new ArrayList<>();
    try {
      if (rule.getConditionsJson() != null && !rule.getConditionsJson().isBlank()) {
        conditions = serializerPort.readConditions(rule.getConditionsJson());
      }
    } catch (Exception ignored) {
      // ignore
    }

    return RuleConfigurationDTO.builder()
        .id(rule.getId())
        .ruleName(rule.getRuleName())
        .description(rule.getDescription())
        .ruleType(rule.getRuleType().name())
        .threshold(rule.getThreshold())
        .weight(rule.getWeight())
        .enabled(rule.getEnabled())
        .classification(rule.getClassification().name())
        .parameters(rule.getParameters())
        .conditions(conditions)
        .logicOperator(rule.getLogicOperator() != null ? rule.getLogicOperator().name() : "AND")
        .version(rule.getVersion())
        .build();
  }

  // DTOs de resultado

  @Data
  @Builder
  public static class SimulationResult {
    private String ruleName;
    private boolean triggered;
    private String classification;
    private int weight;
    private String reason;
    private List<ConditionResult> conditionResults;
    private String logicOperator;
    private long processingTimeMs;
  }

  @Data
  @Builder
  public static class ConditionResult {
    private String field;
    private String operator;
    private String expectedValue;
    private String actualValue;
    private boolean met;
  }

  @Data
  @Builder
  public static class BacktestResult {
    private Long ruleId;
    private String ruleName;
    private LocalDateTime startDate;
    private LocalDateTime endDate;
    private long totalEvaluated;
    private long totalTriggered;
    private double triggerRate;
    private long wouldApprove;
    private long wouldSuspect;
    private long wouldBlock;
    private BigDecimal totalAmountAffected;
    private List<SimulationResult> sampleResults;
  }

  @Data
  @Builder
  public static class ComparisonResult {
    private String ruleAName;
    private String ruleBName;
    private int totalPayloads;
    private int triggeredByA;
    private int triggeredByB;
    private int triggeredByBoth;
    private int triggeredByNeither;
    private List<ComparisonDetail> details;
  }

  @Data
  @Builder
  public static class ComparisonDetail {
    private String transactionId;
    private boolean triggeredByA;
    private boolean triggeredByB;
  }
}
