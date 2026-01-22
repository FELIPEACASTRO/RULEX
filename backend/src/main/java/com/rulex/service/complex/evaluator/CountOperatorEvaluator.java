package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityService.AggregationType;
import com.rulex.service.VelocityService.KeyType;
import com.rulex.service.VelocityService.TimeWindow;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de contagem.
 *
 * <p>Operadores suportados:
 *
 * <ul>
 *   <li>COUNT_LAST_N_DAYS - contagem nos últimos N dias
 *   <li>COUNT_DISTINCT_ACCOUNTS - contagem de contas distintas
 *   <li>COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS - instrumentos distintos
 *   <li>COUNT_DISTINCT_PANS_LAST_N_HOURS - PANs distintos
 *   <li>COUNT_DISTINCT_PAYERS_LAST_N_DAYS - pagadores distintos
 *   <li>COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS - user agents distintos
 *   <li>COUNT_CRYPTO_TXN_LAST_N_DAYS - transações crypto
 *   <li>COUNT_MFA_ABANDONMENTS - abandonos de MFA
 *   <li>COUNT_MFA_DENIALS_LAST_N_HOURS - negações de MFA
 *   <li>COUNT_FAILURES_LAST_N_HOURS - falhas recentes
 *   <li>COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS - beneficiários únicos
 *   <li>COUNT_UNIQUE_IPS_LAST_N_HOURS - IPs únicos
 *   <li>COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS - merchants distintos por hora
 * </ul>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class CountOperatorEvaluator implements OperatorEvaluator {

  private final VelocityService velocityService;

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          ConditionOperator.COUNT_LAST_N_DAYS,
          ConditionOperator.COUNT_DISTINCT_ACCOUNTS,
          ConditionOperator.COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS,
          ConditionOperator.COUNT_DISTINCT_PANS_LAST_N_HOURS,
          ConditionOperator.COUNT_DISTINCT_PAYERS_LAST_N_DAYS,
          ConditionOperator.COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS,
          ConditionOperator.COUNT_CRYPTO_TXN_LAST_N_DAYS,
          ConditionOperator.COUNT_MFA_ABANDONMENTS,
          ConditionOperator.COUNT_MFA_DENIALS_LAST_N_HOURS,
          ConditionOperator.COUNT_FAILURES_LAST_N_HOURS,
          ConditionOperator.COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS,
          ConditionOperator.COUNT_UNIQUE_IPS_LAST_N_HOURS,
          ConditionOperator.COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();

    log.debug("CountOperatorEvaluator: op={}, field={}", op, condition.getFieldName());

    return switch (op) {
      case COUNT_LAST_N_DAYS -> evaluateCountLastNDays(condition, context);
      case COUNT_DISTINCT_ACCOUNTS -> evaluateCountDistinctAccounts(condition, context);
      case COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS ->
          evaluateCountDistinctInstruments(condition, context);
      case COUNT_DISTINCT_PANS_LAST_N_HOURS -> evaluateCountDistinctPans(condition, context);
      case COUNT_DISTINCT_PAYERS_LAST_N_DAYS -> evaluateCountDistinctPayers(condition, context);
      case COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS ->
          evaluateCountDistinctUserAgents(condition, context);
      case COUNT_CRYPTO_TXN_LAST_N_DAYS -> evaluateCountCryptoTxn(condition, context);
      case COUNT_MFA_ABANDONMENTS -> evaluateCountMfaAbandonments(condition, context);
      case COUNT_MFA_DENIALS_LAST_N_HOURS -> evaluateCountMfaDenials(condition, context);
      case COUNT_FAILURES_LAST_N_HOURS -> evaluateCountFailures(condition, context);
      case COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS ->
          evaluateCountUniqueBeneficiaries(condition, context);
      case COUNT_UNIQUE_IPS_LAST_N_HOURS -> evaluateCountUniqueIps(condition, context);
      case COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS ->
          evaluateCountDistinctMerchantsHours(condition, context);
      default -> false;
    };
  }

  /** COUNT_LAST_N_DAYS - contagem de transações nos últimos N dias. */
  private boolean evaluateCountLastNDays(RuleCondition condition, EvaluationContext context) {
    TransactionRequest request = context.getTransactionRequest();
    if (request == null) {
      return evaluateFromPayload(condition, context, "transactionCountLastNDays");
    }

    int days = parseIntSafe(condition.getValueMin(), 7);
    KeyType keyType = resolveKeyType(condition.getFieldName());
    TimeWindow window = resolveTimeWindowFromDays(days);

    BigDecimal count =
        velocityService.getAggregation(request, keyType, window, AggregationType.COUNT);
    long threshold = parseLongSafe(condition.getValueSingle(), 0);

    log.debug("COUNT_LAST_N_DAYS: days={}, count={}, threshold={}", days, count, threshold);
    return count.longValue() > threshold;
  }

  /** COUNT_DISTINCT_ACCOUNTS - contagem de contas distintas. */
  private boolean evaluateCountDistinctAccounts(
      RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "distinctAccountCount");
  }

  /** COUNT_DISTINCT_INSTRUMENTS_LAST_N_DAYS - instrumentos de pagamento distintos. */
  private boolean evaluateCountDistinctInstruments(
      RuleCondition condition, EvaluationContext context) {
    TransactionRequest request = context.getTransactionRequest();
    if (request == null) {
      return evaluateFromPayload(condition, context, "distinctInstrumentCount");
    }

    int days = parseIntSafe(condition.getValueMin(), 7);
    TimeWindow window = resolveTimeWindowFromDays(days);

    BigDecimal count =
        velocityService.getAggregation(
            request, KeyType.CUSTOMER_ID, window, AggregationType.DISTINCT_MERCHANTS);
    long threshold = parseLongSafe(condition.getValueSingle(), 0);

    return count.longValue() > threshold;
  }

  /** COUNT_DISTINCT_PANS_LAST_N_HOURS - PANs distintos nas últimas N horas. */
  private boolean evaluateCountDistinctPans(RuleCondition condition, EvaluationContext context) {
    TransactionRequest request = context.getTransactionRequest();
    if (request == null) {
      return evaluateFromPayload(condition, context, "distinctPanCount");
    }

    int hours = parseIntSafe(condition.getValueMin(), 24);
    TimeWindow window = resolveTimeWindowFromHours(hours);

    BigDecimal count =
        velocityService.getAggregation(
            request, KeyType.CUSTOMER_ID, window, AggregationType.DISTINCT_MERCHANTS);
    long threshold = parseLongSafe(condition.getValueSingle(), 0);

    return count.longValue() > threshold;
  }

  /** COUNT_DISTINCT_PAYERS_LAST_N_DAYS - pagadores distintos nos últimos N dias. */
  private boolean evaluateCountDistinctPayers(RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "distinctPayerCount");
  }

  /** COUNT_DISTINCT_USER_AGENTS_LAST_N_HOURS - user agents distintos. */
  private boolean evaluateCountDistinctUserAgents(
      RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "distinctUserAgentCount");
  }

  /** COUNT_CRYPTO_TXN_LAST_N_DAYS - transações de criptomoeda. */
  private boolean evaluateCountCryptoTxn(RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "cryptoTransactionCount");
  }

  /** COUNT_MFA_ABANDONMENTS - abandonos de autenticação multifator. */
  private boolean evaluateCountMfaAbandonments(RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "mfaAbandonmentCount");
  }

  /** COUNT_MFA_DENIALS_LAST_N_HOURS - negações de MFA nas últimas N horas. */
  private boolean evaluateCountMfaDenials(RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "mfaDenialCount");
  }

  /** COUNT_FAILURES_LAST_N_HOURS - falhas nas últimas N horas. */
  private boolean evaluateCountFailures(RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "failureCount");
  }

  /** COUNT_UNIQUE_BENEFICIARIES_LAST_N_DAYS - beneficiários únicos. */
  private boolean evaluateCountUniqueBeneficiaries(
      RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "uniqueBeneficiaryCount");
  }

  /** COUNT_UNIQUE_IPS_LAST_N_HOURS - IPs únicos nas últimas N horas. */
  private boolean evaluateCountUniqueIps(RuleCondition condition, EvaluationContext context) {
    return evaluateFromPayload(condition, context, "uniqueIpCount");
  }

  /** COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS - merchants distintos por hora. */
  private boolean evaluateCountDistinctMerchantsHours(
      RuleCondition condition, EvaluationContext context) {
    TransactionRequest request = context.getTransactionRequest();
    if (request == null) {
      return evaluateFromPayload(condition, context, "distinctMerchantCountHours");
    }

    int hours = parseIntSafe(condition.getValueMin(), 24);
    TimeWindow window = resolveTimeWindowFromHours(hours);

    BigDecimal count =
        velocityService.getAggregation(
            request, KeyType.PAN, window, AggregationType.DISTINCT_MERCHANTS);
    long threshold = parseLongSafe(condition.getValueSingle(), 0);

    return count.longValue() > threshold;
  }

  // Métodos auxiliares

  private boolean evaluateFromPayload(
      RuleCondition condition, EvaluationContext context, String defaultKey) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) {
      return false;
    }

    String key = condition.getFieldName() != null ? condition.getFieldName() : defaultKey;
    Object value = payload.get(key);

    // Tentar variações do nome
    if (value == null) {
      value = payload.get(defaultKey);
    }
    if (value == null) {
      value = payload.get(toSnakeCase(defaultKey));
    }

    if (value == null) {
      log.debug("Valor não encontrado para key={}", key);
      return false;
    }

    long count = parseLongSafe(String.valueOf(value), 0);
    long threshold = parseLongSafe(condition.getValueSingle(), 0);

    log.debug("evaluateFromPayload: key={}, count={}, threshold={}", key, count, threshold);
    return count > threshold;
  }

  private KeyType resolveKeyType(String fieldName) {
    if (fieldName == null) return KeyType.PAN;
    return switch (fieldName.toLowerCase()) {
      case "pan", "cardnumber", "card_number" -> KeyType.PAN;
      case "customerid", "customer_id", "customer" -> KeyType.CUSTOMER_ID;
      case "merchantid", "merchant_id", "merchant" -> KeyType.MERCHANT_ID;
      default -> KeyType.PAN;
    };
  }

  private TimeWindow resolveTimeWindowFromHours(int hours) {
    return switch (hours) {
      case 1 -> TimeWindow.HOUR_1;
      case 6 -> TimeWindow.HOUR_6;
      case 12 -> TimeWindow.HOUR_12;
      case 24 -> TimeWindow.HOUR_24;
      default ->
          hours <= 1
              ? TimeWindow.HOUR_1
              : hours <= 6
                  ? TimeWindow.HOUR_6
                  : hours <= 12 ? TimeWindow.HOUR_12 : TimeWindow.HOUR_24;
    };
  }

  private TimeWindow resolveTimeWindowFromDays(int days) {
    return switch (days) {
      case 1 -> TimeWindow.HOUR_24;
      case 7 -> TimeWindow.DAY_7;
      case 30 -> TimeWindow.DAY_30;
      default -> days <= 1 ? TimeWindow.HOUR_24 : days <= 7 ? TimeWindow.DAY_7 : TimeWindow.DAY_30;
    };
  }

  private int parseIntSafe(String value, int defaultValue) {
    try {
      return Integer.parseInt(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  private long parseLongSafe(String value, long defaultValue) {
    try {
      return Long.parseLong(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  private String toSnakeCase(String camelCase) {
    return camelCase.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase();
  }

  @Override
  public String getCategory() {
    return "COUNT";
  }
}
