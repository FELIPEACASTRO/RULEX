package com.rulex.service.complex.evaluation;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class MerchantAdvancedEvaluator {

  private MerchantAdvancedEvaluator() {}

  /**
   * MCC_CATEGORY_VELOCITY: Velocidade por categoria MCC. Formato valueSingle:
   * "mccCategory|threshold" (ex: "gambling|5")
   */
  public static boolean evaluateMccCategoryVelocity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      String mccCategory = parts[0].trim();
      int threshold = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : 5;

      Object velocityObj = payload.get("mcc_velocity_" + mccCategory.toLowerCase());
      if (velocityObj == null) {
        velocityObj = payload.get("mccVelocity_" + mccCategory);
      }

      if (velocityObj == null) return false;
      int velocity = velocityObj instanceof Number ? ((Number) velocityObj).intValue() : 0;
      return velocity > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_CATEGORY_VELOCITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MCC_SPENDING_LIMIT_CHECK: Verificação de limite por MCC. Formato valueSingle: "limitAmount"
   * (ex: "5000")
   */
  public static boolean evaluateMccSpendingLimitCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      TransactionRequest tx = context.getTransactionRequest();
      if (tx == null) return false;

      BigDecimal limit = new BigDecimal(condition.getValueSingle().trim());

      Object spentObj = payload != null ? payload.get("mcc_spending_total") : null;
      if (spentObj == null && payload != null) {
        spentObj = payload.get("mccSpendingTotal");
      }

      BigDecimal spent = BigDecimal.ZERO;
      if (spentObj instanceof Number) {
        spent = BigDecimal.valueOf(((Number) spentObj).doubleValue());
      }

      BigDecimal total = spent.add(tx.getTransactionAmount());
      return total.compareTo(limit) > 0;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_SPENDING_LIMIT_CHECK: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MCC_CROSS_CATEGORY_PATTERN: Detecta padrão cross-category MCC. Formato valueSingle:
   * "categoryCount" (ex: "4" = 4+ categorias diferentes)
   */
  public static boolean evaluateMccCrossCategoryPattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("distinct_mcc_categories");
      if (countObj == null) {
        countObj = payload.get("distinctMccCategories");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_CROSS_CATEGORY_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_REPUTATION_SCORE: Score de reputação do merchant. Formato valueSingle: "minScore" (ex:
   * "70" = score mínimo)
   */
  public static boolean evaluateMerchantReputationScore(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double minScore = Double.parseDouble(condition.getValueSingle().trim());

      Object scoreObj = payload.get("merchant_reputation_score");
      if (scoreObj == null) {
        scoreObj = payload.get("merchantReputationScore");
      }

      if (scoreObj == null) return true; // No score = suspicious
      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 0.0;
      return score < minScore;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_REPUTATION_SCORE: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_AGE_CHECK: Verificação de idade do merchant. Formato valueSingle: "minAgeDays" (ex:
   * "30" = mínimo 30 dias)
   */
  public static boolean evaluateMerchantAgeCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int minAge = Integer.parseInt(condition.getValueSingle().trim());

      Object ageObj = payload.get("merchant_age_days");
      if (ageObj == null) {
        ageObj = payload.get("merchantAgeDays");
      }

      if (ageObj == null) return true; // No age = new merchant = suspicious
      int age = ageObj instanceof Number ? ((Number) ageObj).intValue() : 0;
      return age < minAge;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_AGE_CHECK: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_TRANSACTION_VOLUME: Volume de transações do merchant. Formato valueSingle:
   * "minVolume|maxVolume" (ex: "100|10000")
   */
  public static boolean evaluateMerchantTransactionVolume(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int minVolume = Integer.parseInt(parts[0].trim());
      int maxVolume = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : Integer.MAX_VALUE;

      Object volumeObj = payload.get("merchant_transaction_volume");
      if (volumeObj == null) {
        volumeObj = payload.get("merchantTransactionVolume");
      }

      if (volumeObj == null) return false;
      int volume = volumeObj instanceof Number ? ((Number) volumeObj).intValue() : 0;
      return volume < minVolume || volume > maxVolume;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_TRANSACTION_VOLUME: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_CHARGEBACK_HISTORY: Histórico de chargeback do merchant. Formato valueSingle:
   * "maxChargebackRate" (ex: "0.02" = 2%)
   */
  public static boolean evaluateMerchantChargebackHistory(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRate = Double.parseDouble(condition.getValueSingle().trim());

      Object rateObj = payload.get("merchant_chargeback_rate");
      if (rateObj == null) {
        rateObj = payload.get("merchantChargebackRate");
      }

      if (rateObj == null) return false;
      double rate = rateObj instanceof Number ? ((Number) rateObj).doubleValue() : 0.0;
      return rate > maxRate;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CHARGEBACK_HISTORY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_FRAUD_RATE_CHECK: Verificação de taxa de fraude do merchant. Formato valueSingle:
   * "maxFraudRate" (ex: "0.01" = 1%)
   */
  public static boolean evaluateMerchantFraudRateCheck(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRate = Double.parseDouble(condition.getValueSingle().trim());

      Object rateObj = payload.get("merchant_fraud_rate");
      if (rateObj == null) {
        rateObj = payload.get("merchantFraudRate");
      }

      if (rateObj == null) return false;
      double rate = rateObj instanceof Number ? ((Number) rateObj).doubleValue() : 0.0;
      return rate > maxRate;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_FRAUD_RATE_CHECK: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_GEOGRAPHIC_SPREAD: Dispersão geográfica do merchant. Formato valueSingle:
   * "maxCountries" (ex: "10")
   */
  public static boolean evaluateMerchantGeographicSpread(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int maxCountries = Integer.parseInt(condition.getValueSingle().trim());

      Object countObj = payload.get("merchant_country_count");
      if (countObj == null) {
        countObj = payload.get("merchantCountryCount");
      }

      if (countObj == null) return false;
      int count = countObj instanceof Number ? ((Number) countObj).intValue() : 0;
      return count > maxCountries;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_GEOGRAPHIC_SPREAD: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_CUSTOMER_CONCENTRATION: Concentração de clientes do merchant. Formato valueSingle:
   * "concentrationThreshold" (ex: "0.5" = 50%)
   */
  public static boolean evaluateMerchantCustomerConcentration(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object concObj = payload.get("merchant_customer_concentration");
      if (concObj == null) {
        concObj = payload.get("merchantCustomerConcentration");
      }

      if (concObj == null) return false;
      double concentration = concObj instanceof Number ? ((Number) concObj).doubleValue() : 0.0;
      return concentration > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CUSTOMER_CONCENTRATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_AMOUNT_DISTRIBUTION: Distribuição de valores do merchant. Formato valueSingle:
   * "stdDevThreshold" (ex: "1000")
   */
  public static boolean evaluateMerchantAmountDistribution(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double threshold = Double.parseDouble(condition.getValueSingle().trim());

      Object stdObj = payload.get("merchant_amount_stddev");
      if (stdObj == null) {
        stdObj = payload.get("merchantAmountStddev");
      }

      if (stdObj == null) return false;
      double stdDev = stdObj instanceof Number ? ((Number) stdObj).doubleValue() : 0.0;
      return stdDev > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_AMOUNT_DISTRIBUTION: {}", e.getMessage());
      return false;
    }
  }

  /** MERCHANT_TIME_PATTERN: Padrão temporal do merchant. */
  public static boolean evaluateMerchantTimePattern(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("merchant_time_pattern_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("merchantTimePatternAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_TIME_PATTERN: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_DEVICE_DIVERSITY: Diversidade de dispositivos do merchant. Formato valueSingle:
   * "minDiversity|maxDiversity" (ex: "10|1000")
   */
  public static boolean evaluateMerchantDeviceDiversity(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int minDiversity = Integer.parseInt(parts[0].trim());
      int maxDiversity = parts.length > 1 ? Integer.parseInt(parts[1].trim()) : Integer.MAX_VALUE;

      Object diversityObj = payload.get("merchant_device_diversity");
      if (diversityObj == null) {
        diversityObj = payload.get("merchantDeviceDiversity");
      }

      if (diversityObj == null) return false;
      int diversity = diversityObj instanceof Number ? ((Number) diversityObj).intValue() : 0;
      return diversity < minDiversity || diversity > maxDiversity;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_DEVICE_DIVERSITY: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_REFUND_RATIO: Razão de reembolso do merchant. Formato valueSingle: "maxRefundRatio"
   * (ex: "0.1" = 10%)
   */
  public static boolean evaluateMerchantRefundRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRatio = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("merchant_refund_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("merchantRefundRatio");
      }

      if (ratioObj == null) return false;
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > maxRatio;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_REFUND_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_NEW_CUSTOMER_RATIO: Razão de novos clientes do merchant. Formato valueSingle:
   * "maxNewCustomerRatio" (ex: "0.8" = 80%)
   */
  public static boolean evaluateMerchantNewCustomerRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRatio = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("merchant_new_customer_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("merchantNewCustomerRatio");
      }

      if (ratioObj == null) return false;
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > maxRatio;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_NEW_CUSTOMER_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_DORMANT_REACTIVATION: Reativação de merchant dormente. Formato valueSingle:
   * "dormantDays" (ex: "90" = 90 dias de inatividade)
   */
  public static boolean evaluateMerchantDormantReactivation(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int dormantThreshold = Integer.parseInt(condition.getValueSingle().trim());

      Object daysObj = payload.get("merchant_days_since_last_tx");
      if (daysObj == null) {
        daysObj = payload.get("merchantDaysSinceLastTx");
      }

      if (daysObj == null) return false;
      int daysSinceLastTx = daysObj instanceof Number ? ((Number) daysObj).intValue() : 0;
      return daysSinceLastTx > dormantThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_DORMANT_REACTIVATION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_CROSS_BORDER_RATIO: Razão cross-border do merchant. Formato valueSingle:
   * "maxCrossBorderRatio" (ex: "0.3" = 30%)
   */
  public static boolean evaluateMerchantCrossBorderRatio(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double maxRatio = Double.parseDouble(condition.getValueSingle().trim());

      Object ratioObj = payload.get("merchant_cross_border_ratio");
      if (ratioObj == null) {
        ratioObj = payload.get("merchantCrossBorderRatio");
      }

      if (ratioObj == null) return false;
      double ratio = ratioObj instanceof Number ? ((Number) ratioObj).doubleValue() : 0.0;
      return ratio > maxRatio;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CROSS_BORDER_RATIO: {}", e.getMessage());
      return false;
    }
  }

  /**
   * MERCHANT_HIGH_VALUE_FREQUENCY: Frequência de alto valor do merchant. Formato valueSingle:
   * "threshold|percentage" (ex: "1000|0.5" = 50% acima de 1000)
   */
  public static boolean evaluateMerchantHighValueFrequency(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      double amountThreshold = Double.parseDouble(parts[0].trim());
      double percentageThreshold = parts.length > 1 ? Double.parseDouble(parts[1].trim()) : 0.5;

      Object percentageObj = payload.get("merchant_high_value_percentage");
      if (percentageObj == null) {
        percentageObj = payload.get("merchantHighValuePercentage");
      }

      if (percentageObj == null) return false;
      double percentage =
          percentageObj instanceof Number ? ((Number) percentageObj).doubleValue() : 0.0;
      return percentage > percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_HIGH_VALUE_FREQUENCY: {}", e.getMessage());
      return false;
    }
  }
}
