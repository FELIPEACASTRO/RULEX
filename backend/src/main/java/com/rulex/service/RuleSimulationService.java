package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço para simulação e teste de regras.
 * Permite testar regras antes de ativá-las e fazer backtesting.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class RuleSimulationService {

  private final RuleConfigurationRepository ruleConfigRepository;
  private final TransactionRepository transactionRepository;
  private final ObjectMapper objectMapper;

  /**
   * Simula uma regra contra um payload de teste.
   */
  public SimulationResult simulateRule(RuleConfigurationDTO rule, TransactionRequest testPayload) {
    log.info("Simulando regra: {} contra payload de teste", rule.getRuleName());

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

    String reason = triggered
        ? String.format("Regra disparada com classificação %s e peso %d", rule.getClassification(), rule.getWeight())
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

  /**
   * Faz backtesting de uma regra contra transações históricas.
   */
  public BacktestResult backtestRule(Long ruleId, LocalDateTime startDate, LocalDateTime endDate, int sampleSize) {
    log.info("Iniciando backtest da regra {} de {} a {} com amostra de {}", ruleId, startDate, endDate, sampleSize);

    RuleConfiguration rule = ruleConfigRepository.findById(ruleId)
        .orElseThrow(() -> new IllegalArgumentException("Regra não encontrada: " + ruleId));

    RuleConfigurationDTO ruleDto = convertToDTO(rule);

    // Buscar transações do período (limitado por sample size)
    List<TransactionRequest> transactions = getHistoricalTransactions(startDate, endDate, sampleSize);

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

  /**
   * Compara duas versões de uma regra.
   */
  public ComparisonResult compareRules(RuleConfigurationDTO ruleA, RuleConfigurationDTO ruleB,
                                        List<TransactionRequest> testPayloads) {
    log.info("Comparando regras: {} vs {}", ruleA.getRuleName(), ruleB.getRuleName());

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
        details.add(ComparisonDetail.builder()
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

  /**
   * Avalia uma condição individual.
   */
  private ConditionResult evaluateCondition(RuleConditionDTO condition, TransactionRequest payload) {
    String field = condition.getField();
    String operator = condition.getOperator();
    String expectedValue = condition.getValue();

    Object actualValue = getFieldValue(payload, field);
    boolean met = evaluateOperator(actualValue, operator, expectedValue);

    return ConditionResult.builder()
        .field(field)
        .operator(operator)
        .expectedValue(expectedValue)
        .actualValue(actualValue != null ? actualValue.toString() : "null")
        .met(met)
        .build();
  }

  /**
   * Obtém o valor de um campo do payload.
   */
  private Object getFieldValue(TransactionRequest payload, String field) {
    if (field == null || payload == null) {
      return null;
    }

    try {
      // Converter para Map para acesso dinâmico
      @SuppressWarnings("unchecked")
      Map<String, Object> map = objectMapper.convertValue(payload, Map.class);
      return map.get(field);
    } catch (Exception e) {
      log.warn("Erro ao obter campo {}: {}", field, e.getMessage());
      return null;
    }
  }

  /**
   * Avalia um operador de comparação.
   */
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
        return Pattern.matches(expected, actualStr);
      case "BETWEEN":
        return isBetween(actual, expected);
      case "NOT_BETWEEN":
        return !isBetween(actual, expected);
      default:
        log.warn("Operador desconhecido: {}", operator);
        return false;
    }
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
    } catch (NumberFormatException e) {
      log.warn("Erro ao parsear range: {}", rangeStr);
    }
    return false;
  }

  private List<TransactionRequest> getHistoricalTransactions(LocalDateTime startDate, LocalDateTime endDate, int limit) {
    // Simplificado - em produção, buscaria do repositório
    return new ArrayList<>();
  }

  private RuleConfigurationDTO convertToDTO(RuleConfiguration rule) {
    List<RuleConditionDTO> conditions = new ArrayList<>();
    try {
      if (rule.getConditionsJson() != null && !rule.getConditionsJson().isBlank()) {
        conditions = objectMapper.readValue(
            rule.getConditionsJson(),
            objectMapper.getTypeFactory().constructCollectionType(List.class, RuleConditionDTO.class));
      }
    } catch (Exception e) {
      log.warn("Erro ao parsear condições: {}", e.getMessage());
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
