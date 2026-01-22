package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import com.rulex.service.complex.parsing.BooleanParser;
import com.rulex.service.complex.parsing.TimeWindowParser;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class MerchantMccEvaluator {

  private MerchantMccEvaluator() {}

  public static boolean evaluateMccHighRisk(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String mcc = String.valueOf(fieldValue).trim();
      if (mcc.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (mcc.equals(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_HIGH_RISK: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMccGambling(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String mcc = String.valueOf(fieldValue).trim();
      if (mcc.isBlank()) return false;
      Set<String> gambling = Set.of("7995", "7800", "7801", "7802");
      return gambling.contains(mcc);
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_GAMBLING: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMccCrypto(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String mcc = String.valueOf(fieldValue).trim();
      if (mcc.isBlank()) return false;
      Set<String> crypto = Set.of("6051", "4829");
      return crypto.contains(mcc);
    } catch (Exception e) {
      log.error("Erro ao avaliar MCC_CRYPTO: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMerchantFirstSeen(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      int thresholdDays = Integer.parseInt(condition.getValueSingle().trim());

      Object daysObj = context.getPayload().get("merchant_first_seen_days_ago");
      if (daysObj == null) daysObj = context.getPayload().get("merchantFirstSeenDaysAgo");
      if (daysObj == null) {
        Object isNewObj = context.getPayload().get("merchant_is_new");
        if (isNewObj == null) isNewObj = context.getPayload().get("merchantIsNew");
        if (isNewObj != null) return Boolean.TRUE.equals(BooleanParser.toBoolean(isNewObj));
        return false;
      }

      int daysAgo = daysObj instanceof Number ? ((Number) daysObj).intValue() : Integer.MAX_VALUE;
      return daysAgo <= thresholdDays;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_FIRST_SEEN: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMerchantCountryMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("merchant_country_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("merchantCountryMismatch");
      if (mismatchObj != null) return Boolean.TRUE.equals(BooleanParser.toBoolean(mismatchObj));

      Object merchantCountry = context.getPayload().get("merchant_country");
      if (merchantCountry == null) merchantCountry = context.getPayload().get("merchantCountry");
      Object customerCountry = context.getPayload().get("customer_country");
      if (customerCountry == null) customerCountry = context.getPayload().get("customerCountry");
      if (merchantCountry == null || customerCountry == null) return false;
      return !String.valueOf(merchantCountry).equalsIgnoreCase(String.valueOf(customerCountry));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_COUNTRY_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMerchantCategoryChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object changedObj = context.getPayload().get("merchant_category_changed");
      if (changedObj == null) changedObj = context.getPayload().get("merchantCategoryChanged");
      return Boolean.TRUE.equals(BooleanParser.toBoolean(changedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_CATEGORY_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateMerchantVelocitySpike(
      RuleCondition condition,
      EvaluationContext context,
      VelocityServiceFacade velocityServiceFacade) {
    try {
      if (context.getTransactionRequest() == null) return false;
      String[] parts =
          (condition.getValueSingle() == null ? "" : condition.getValueSingle()).split("\\|");
      long threshold =
          parts.length >= 1 && !parts[0].isBlank() ? Long.parseLong(parts[0].trim()) : 20L;
      int hours = parts.length >= 2 && !parts[1].isBlank() ? Integer.parseInt(parts[1].trim()) : 1;
      VelocityService.TimeWindow window = TimeWindowParser.fromHours(hours);
      var stats =
          velocityServiceFacade.getStats(
              context.getTransactionRequest(), VelocityService.KeyType.MERCHANT_ID, window);
      return stats.getTransactionCount() >= threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar MERCHANT_VELOCITY_SPIKE: {}", e.getMessage());
      return false;
    }
  }
}
