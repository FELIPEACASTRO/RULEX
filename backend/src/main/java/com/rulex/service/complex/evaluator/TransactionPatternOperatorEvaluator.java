package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de padrões de transação. Detecta padrões suspeitos como splitting,
 * layering, structuring, etc.
 */
@Component
@Slf4j
public class TransactionPatternOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          // Contagem de transações
          ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR,
          ConditionOperator.TRANSACTION_COUNT_PER_CUSTOMER_HOUR,
          ConditionOperator.TRANSACTION_COUNT_PER_DEVICE_DAY,
          ConditionOperator.TRANSACTION_COUNT_PER_IP_HOUR,
          ConditionOperator.TRANSACTION_COUNT_PER_MERCHANT_HOUR,
          ConditionOperator.TRANSACTION_FREQUENCY_ANOMALY,
          ConditionOperator.TRANSACTION_SIZE_ESCALATION,
          ConditionOperator.TRANSACTION_TIMING_CLUSTER,
          ConditionOperator.TRANSACTION_ATTEMPT_COUNT_PER_CARD,

          // Padrões
          ConditionOperator.PATTERN_ESCALATION,
          ConditionOperator.PATTERN_ROUND_NUMBERS,
          ConditionOperator.PATTERN_SPLIT_TRANSACTION,
          ConditionOperator.BUST_OUT_PATTERN_DETECTION,
          ConditionOperator.CARD_TESTING_RING_DETECTION,
          ConditionOperator.CIRCULAR_PAYMENT_DETECTION,
          ConditionOperator.RAPID_SUCCESSION_PATTERN,
          ConditionOperator.ROUND_TRIP_DETECTION,

          // Splitting e structuring
          ConditionOperator.SPLIT_PAYMENT_PATTERN,
          ConditionOperator.STRUCTURING_DETECTION,

          // Circular e layering
          ConditionOperator.CIRCULAR_TRANSFER_DETECTION,
          ConditionOperator.LAYERED_TRANSFER_PATTERN,

          // Outros padrões
          ConditionOperator.RAPID_MOVEMENT,
          ConditionOperator.RAPID_MULTI_HOP,
          ConditionOperator.SEQUENTIAL_AMOUNT_PATTERN,
          ConditionOperator.MICRO_TRANSACTION_TEST,

          // Novos operadores de padrão
          ConditionOperator.SPLIT_TRANSACTION_DETECTION,
          ConditionOperator.MICRO_DEPOSIT_VELOCITY);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    log.debug("TransactionPatternOperatorEvaluator: op={}", op);

    return switch (op) {
      case TRANSACTION_COUNT_PER_CARD_HOUR ->
          evaluateCount(context, "txCountPerCardHour", condition, 10);
      case TRANSACTION_COUNT_PER_CUSTOMER_HOUR ->
          evaluateCount(context, "txCountPerCustomerHour", condition, 15);
      case TRANSACTION_COUNT_PER_DEVICE_DAY ->
          evaluateCount(context, "txCountPerDeviceDay", condition, 50);
      case TRANSACTION_COUNT_PER_IP_HOUR ->
          evaluateCount(context, "txCountPerIpHour", condition, 20);
      case TRANSACTION_COUNT_PER_MERCHANT_HOUR ->
          evaluateCount(context, "txCountPerMerchantHour", condition, 100);
      case TRANSACTION_FREQUENCY_ANOMALY -> evaluateBoolean(context, "txFrequencyAnomaly");
      case TRANSACTION_SIZE_ESCALATION -> evaluateBoolean(context, "txSizeEscalation");
      case TRANSACTION_TIMING_CLUSTER -> evaluateBoolean(context, "txTimingCluster");
      case TRANSACTION_ATTEMPT_COUNT_PER_CARD ->
          evaluateCount(context, "txAttemptCountPerCard", condition, 5);
      case PATTERN_ESCALATION -> evaluateBoolean(context, "patternEscalation");
      case PATTERN_ROUND_NUMBERS -> evaluateBoolean(context, "patternRoundNumbers");
      case PATTERN_SPLIT_TRANSACTION -> evaluateBoolean(context, "patternSplitTransaction");
      case BUST_OUT_PATTERN_DETECTION -> evaluateBoolean(context, "bustOutPatternDetected");
      case CARD_TESTING_RING_DETECTION -> evaluateBoolean(context, "cardTestingRingDetected");
      case CIRCULAR_PAYMENT_DETECTION -> evaluateBoolean(context, "circularPaymentDetected");
      case RAPID_SUCCESSION_PATTERN -> evaluateBoolean(context, "rapidSuccessionPattern");
      case ROUND_TRIP_DETECTION -> evaluateBoolean(context, "roundTripDetected");
      case SPLIT_PAYMENT_PATTERN -> evaluateBoolean(context, "splitPaymentPattern");
      case STRUCTURING_DETECTION -> evaluateBoolean(context, "structuringDetected");
      case CIRCULAR_TRANSFER_DETECTION -> evaluateBoolean(context, "circularTransferDetected");
      case LAYERED_TRANSFER_PATTERN -> evaluateBoolean(context, "layeredTransferPattern");
      case RAPID_MOVEMENT -> evaluateBoolean(context, "rapidMovementDetected");
      case RAPID_MULTI_HOP -> evaluateBoolean(context, "rapidMultiHopDetected");
      case SEQUENTIAL_AMOUNT_PATTERN -> evaluateBoolean(context, "sequentialAmountPattern");
      case MICRO_TRANSACTION_TEST -> evaluateBoolean(context, "microTransactionTest");
      case SPLIT_TRANSACTION_DETECTION -> evaluateBoolean(context, "splitTransactionDetected");
      case MICRO_DEPOSIT_VELOCITY -> evaluateCount(context, "microDepositVelocity", condition, 5);
      default -> false;
    };
  }

  private boolean evaluateBoolean(EvaluationContext context, String key) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;
    Object value = payload.get(key);
    if (value == null) return false;
    return Boolean.parseBoolean(String.valueOf(value));
  }

  private boolean evaluateCount(
      EvaluationContext context, String key, RuleCondition condition, int defaultThreshold) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;
    Object value = payload.get(key);
    if (value == null) return false;
    int count = parseIntSafe(String.valueOf(value), 0);
    int threshold = parseIntSafe(condition.getValueSingle(), defaultThreshold);
    return count > threshold;
  }

  private int parseIntSafe(String value, int defaultValue) {
    try {
      return Integer.parseInt(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  @Override
  public String getCategory() {
    return "TRANSACTION_PATTERN";
  }
}
