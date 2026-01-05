package com.rulex.service;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision.TransactionClassification;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionRepository;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;

/**
 * Serviço que executa regras de fraude configuradas no banco de dados. Substitui a lógica hardcoded
 * do AdvancedRuleEngineService.
 *
 * <p>GAP-FIX: Elimina a fragmentação de regras (hardcoded vs banco vs especificadas) criando uma
 * única fonte da verdade para as regras de fraude.
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DatabaseRuleExecutorService {

  private final RuleConfigurationRepository ruleConfigRepository;
  private final TransactionRepository transactionRepository;
  private final AuditService auditService;
  private final ObjectMapper objectMapper;
  private final Clock clock;

  // Lista das 28 regras que foram migradas do AdvancedRuleEngineService
  private static final Set<String> ADVANCED_RULE_NAMES =
      Set.of(
          "EMV_SECURITY_CHECK",
          "TERMINAL_VERIFICATION_FAILED",
          "EXPIRED_CARD",
          "SUSPICIOUS_TRANSACTION_TYPE",
          "UNUSUAL_CARD_MEDIA",
          "SUSPICIOUS_TERMINAL",
          "ECOMMERCE_NO_AVS",
          "POS_SECURITY_MISSING",
          "CARD_CAPTURE_FRAUD",
          "PIN_CVV_LIMIT_EXCEEDED",
          "OFFLINE_PIN_FAILED",
          "MISSING_CVV2_HIGH_RISK",
          "CUSTOM_INDICATOR_FRAUD",
          "PROCESSING_LAG_ANOMALY",
          "TIMEZONE_NORMALIZED_CHECK",
          "DUPLICATE_TRANSACTION",
          "SUSPICIOUS_MERCHANT_POSTAL",
          "SUSPICIOUS_TOKEN",
          "UNEXPECTED_CURRENCY",
          "ANOMALOUS_CONVERSION_RATE",
          "INCOHERENT_AUTH_SEQUENCE",
          "INCOHERENT_CONTEXT",
          "CONTRADICTORY_AUTHORIZATION",
          "SUSPICIOUS_ACQUIRER",
          "ACQUIRER_COUNTRY_MISMATCH",
          "COMBINED_SCORE_CHECK",
          "VELOCITY_CHECK_CONSOLIDATED",
          "CUSTOM_INDICATORS_COMPREHENSIVE");

  public enum RuleResult {
    APPROVED,
    SUSPICIOUS,
    FRAUD
  }

  public record RuleExecution(RuleResult result, List<TriggeredRuleDTO> triggeredRules) {}

  /**
   * Executa todas as regras avançadas configuradas no banco de dados.
   *
   * @param transaction A transação a ser avaliada
   * @return Resultado da execução com a classificação mais severa e regras acionadas
   */
  public RuleExecution executeAdvancedRules(TransactionRequest transaction) {
    log.info(
        "Executando regras avançadas do banco de dados para transação: {}",
        transaction.getExternalTransactionId());

    List<RuleConfiguration> rules = getEnabledAdvancedRules();
    List<TriggeredRuleDTO> triggeredRules = new ArrayList<>();
    RuleResult mostSevere = RuleResult.APPROVED;

    for (RuleConfiguration rule : rules) {
      try {
        RuleResult result = evaluateRule(rule, transaction);

        if (result != RuleResult.APPROVED) {
          triggeredRules.add(
              TriggeredRuleDTO.builder()
                  .name(rule.getRuleName())
                  .detail(rule.getDescription())
                  .build());

          auditService.logRule(rule.getRuleName(), transaction, result.name());

          if (result == RuleResult.FRAUD) {
            mostSevere = RuleResult.FRAUD;
          } else if (result == RuleResult.SUSPICIOUS && mostSevere != RuleResult.FRAUD) {
            mostSevere = RuleResult.SUSPICIOUS;
          }
        }
      } catch (Exception e) {
        log.error("Erro ao avaliar regra {}: {}", rule.getRuleName(), e.getMessage());
      }
    }

    log.info(
        "Execução de regras avançadas concluída. Resultado: {}, Regras acionadas: {}",
        mostSevere,
        triggeredRules.size());

    return new RuleExecution(mostSevere, triggeredRules);
  }

  /** Obtém todas as regras avançadas habilitadas do banco de dados. */
  @Cacheable(value = "advancedRules", key = "'enabled'")
  public List<RuleConfiguration> getEnabledAdvancedRules() {
    List<RuleConfiguration> allEnabled = ruleConfigRepository.findByEnabled(true);
    return allEnabled.stream().filter(r -> ADVANCED_RULE_NAMES.contains(r.getRuleName())).toList();
  }

  /** Avalia uma regra específica contra uma transação. */
  private RuleResult evaluateRule(RuleConfiguration rule, TransactionRequest transaction) {
    log.debug("Avaliando regra: {}", rule.getRuleName());

    // Primeiro, tenta avaliar usando as condições JSON configuradas
    List<RuleConditionDTO> conditions = parseConditions(rule.getConditionsJson());

    if (!conditions.isEmpty()) {
      return evaluateConditions(rule, conditions, transaction);
    }

    // Fallback: avaliação baseada no tipo de regra e parâmetros
    return evaluateByRuleType(rule, transaction);
  }

  /** Avalia condições configuradas em JSON. */
  private RuleResult evaluateConditions(
      RuleConfiguration rule, List<RuleConditionDTO> conditions, TransactionRequest transaction) {

    RuleConfiguration.LogicOperator operator = rule.getLogicOperator();
    if (operator == null) {
      operator = RuleConfiguration.LogicOperator.AND;
    }

    boolean allMatch = true;
    boolean anyMatch = false;

    for (RuleConditionDTO condition : conditions) {
      boolean matches = evaluateCondition(condition, transaction);

      if (matches) {
        anyMatch = true;
      } else {
        allMatch = false;
      }
    }

    boolean triggered = (operator == RuleConfiguration.LogicOperator.AND) ? allMatch : anyMatch;

    if (triggered) {
      return mapClassificationToResult(rule.getClassification());
    }

    return RuleResult.APPROVED;
  }

  /** Avalia uma condição individual. */
  private boolean evaluateCondition(RuleConditionDTO condition, TransactionRequest transaction) {
    String field = condition.getField();
    String operator = condition.getOperator();
    String expectedValue = condition.getValue();

    Object actualValue = getFieldValue(transaction, field);

    if (actualValue == null && !"IS_NULL".equals(operator) && !"IS_NOT_NULL".equals(operator)) {
      return false;
    }

    return switch (operator) {
      case "EQUALS" -> String.valueOf(actualValue).equals(expectedValue);
      case "NOT_EQUALS" -> !String.valueOf(actualValue).equals(expectedValue);
      case "GREATER_THAN" -> compareNumeric(actualValue, expectedValue) > 0;
      case "LESS_THAN" -> compareNumeric(actualValue, expectedValue) < 0;
      case "GREATER_THAN_OR_EQUALS" -> compareNumeric(actualValue, expectedValue) >= 0;
      case "LESS_THAN_OR_EQUALS" -> compareNumeric(actualValue, expectedValue) <= 0;
      case "CONTAINS" -> String.valueOf(actualValue).contains(expectedValue);
      case "NOT_CONTAINS" -> !String.valueOf(actualValue).contains(expectedValue);
      case "IN" -> isIn(actualValue, expectedValue);
      case "NOT_IN" -> !isIn(actualValue, expectedValue);
      case "IS_NULL" -> actualValue == null;
      case "IS_NOT_NULL" -> actualValue != null;
      case "STARTS_WITH" -> String.valueOf(actualValue).startsWith(expectedValue);
      case "ENDS_WITH" -> String.valueOf(actualValue).endsWith(expectedValue);
      case "REGEX" -> matchesRegex(actualValue, expectedValue);
      default -> {
        log.warn("Operador desconhecido: {}", operator);
        yield false;
      }
    };
  }

  /** Avalia regras baseadas no tipo (para regras sem condições JSON). */
  private RuleResult evaluateByRuleType(RuleConfiguration rule, TransactionRequest transaction) {
    String ruleName = rule.getRuleName();

    // Regras de duplicação (verificar primeiro, antes de velocidade)
    if (ruleName.contains("DUPLICATE")) {
      return evaluateDuplicateRule(rule, transaction);
    }

    // Regras de velocidade
    if ("VELOCITY".equals(rule.getRuleType().name()) || ruleName.contains("VELOCITY")) {
      return evaluateVelocityRule(rule, transaction);
    }

    // Regras baseadas em threshold simples
    if (rule.getThreshold() != null && rule.getThreshold() > 0) {
      return evaluateThresholdRule(rule, transaction);
    }

    return RuleResult.APPROVED;
  }

  /** Avalia regras de velocidade. */
  private RuleResult evaluateVelocityRule(RuleConfiguration rule, TransactionRequest transaction) {
    String customerId = transaction.getCustomerIdFromHeader();
    if (customerId == null) {
      return RuleResult.APPROVED;
    }

    // Contagem em 5 minutos
    long count5min =
        Optional.ofNullable(
                transactionRepository.countTransactionsByCustomerSince(
                    customerId, LocalDateTime.now(clock).minusMinutes(5)))
            .orElse(0L);

    if (count5min >= 3) {
      return RuleResult.FRAUD;
    }

    // Contagem em 1 hora
    long count1hour =
        Optional.ofNullable(
                transactionRepository.countTransactionsByCustomerSince(
                    customerId, LocalDateTime.now(clock).minusHours(1)))
            .orElse(0L);

    if (count1hour >= 10) {
      return RuleResult.SUSPICIOUS;
    }

    // Contagem diária
    long countDaily =
        transactionRepository.countDailyTransactions(customerId, transaction.getTransactionDate());

    if (countDaily >= 50) {
      return RuleResult.SUSPICIOUS;
    }

    return RuleResult.APPROVED;
  }

  /** Avalia regras de duplicação. */
  private RuleResult evaluateDuplicateRule(RuleConfiguration rule, TransactionRequest transaction) {
    long duplicateCount =
        transactionRepository.countDuplicateTransactions(
            transaction.getExternalTransactionId(), transaction.getTransactionDate());

    if (duplicateCount > 0) {
      return RuleResult.FRAUD;
    }

    return RuleResult.APPROVED;
  }

  /** Avalia regras baseadas em threshold. */
  private RuleResult evaluateThresholdRule(RuleConfiguration rule, TransactionRequest transaction) {
    BigDecimal amount = transaction.getTransactionAmount();
    if (amount == null) {
      return RuleResult.APPROVED;
    }

    BigDecimal threshold = new BigDecimal(rule.getThreshold());

    if (amount.compareTo(threshold) > 0) {
      return mapClassificationToResult(rule.getClassification());
    }

    return RuleResult.APPROVED;
  }

  // ==================== MÉTODOS AUXILIARES ====================

  private List<RuleConditionDTO> parseConditions(String conditionsJson) {
    if (conditionsJson == null || conditionsJson.isBlank() || "[]".equals(conditionsJson)) {
      return List.of();
    }
    try {
      return objectMapper.readValue(conditionsJson, new TypeReference<List<RuleConditionDTO>>() {});
    } catch (Exception e) {
      log.warn("Erro ao parsear condições JSON: {}", e.getMessage());
      return List.of();
    }
  }

  private Object getFieldValue(TransactionRequest transaction, String fieldName) {
    if (fieldName == null || fieldName.isBlank()) {
      return null;
    }

    // Remove prefixo JSON path se presente
    if (fieldName.startsWith("$.")) {
      fieldName = fieldName.substring(2);
    }
    if (fieldName.startsWith("transaction.")) {
      fieldName = fieldName.substring(12);
    }

    try {
      PropertyDescriptor pd = BeanUtils.getPropertyDescriptor(TransactionRequest.class, fieldName);
      if (pd != null && pd.getReadMethod() != null) {
        return pd.getReadMethod().invoke(transaction);
      }
    } catch (Exception e) {
      log.debug("Campo não encontrado: {}", fieldName);
    }
    return null;
  }

  private int compareNumeric(Object actual, String expected) {
    try {
      BigDecimal actualNum = new BigDecimal(String.valueOf(actual));
      BigDecimal expectedNum = new BigDecimal(expected);
      return actualNum.compareTo(expectedNum);
    } catch (NumberFormatException e) {
      return String.valueOf(actual).compareTo(expected);
    }
  }

  private boolean isIn(Object actual, String expectedList) {
    String[] values = expectedList.split(",");
    String actualStr = String.valueOf(actual);
    for (String value : values) {
      if (actualStr.equals(value.trim())) {
        return true;
      }
    }
    return false;
  }

  private boolean matchesRegex(Object actual, String pattern) {
    try {
      Pattern regex = Pattern.compile(pattern);
      Matcher matcher = regex.matcher(String.valueOf(actual));
      return matcher.matches();
    } catch (Exception e) {
      log.warn("Regex inválido: {}", pattern);
      return false;
    }
  }

  private RuleResult mapClassificationToResult(TransactionClassification classification) {
    if (classification == null) {
      return RuleResult.SUSPICIOUS;
    }
    return switch (classification) {
      case FRAUD -> RuleResult.FRAUD;
      case SUSPICIOUS -> RuleResult.SUSPICIOUS;
      case APPROVED -> RuleResult.APPROVED;
      default -> RuleResult.SUSPICIOUS;
    };
  }
}
