package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class GraphNetworkEvaluator {

  private GraphNetworkEvaluator() {}

  /**
   * FAN_OUT_COUNT: Conta destinatários únicos de um remetente. Formato valueSingle:
   * "threshold|hours" (ex: "10|24")
   */
  public static boolean evaluateFanOutCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object fanOutObj = payload.get("fan_out_count");
      if (fanOutObj == null) fanOutObj = payload.get("fanOutCount");

      int fanOut = fanOutObj instanceof Number ? ((Number) fanOutObj).intValue() : 0;
      return fanOut > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar FAN_OUT_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * FAN_IN_COUNT: Conta remetentes únicos para um destinatário. Formato valueSingle:
   * "threshold|hours" (ex: "10|24")
   */
  public static boolean evaluateFanInCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int threshold = Integer.parseInt(parts[0].trim());

      Object fanInObj = payload.get("fan_in_count");
      if (fanInObj == null) fanInObj = payload.get("fanInCount");

      int fanIn = fanInObj instanceof Number ? ((Number) fanInObj).intValue() : 0;
      return fanIn > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar FAN_IN_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * SHARED_DEVICE_COUNT: Conta contas usando o mesmo dispositivo. Formato valueSingle: "threshold"
   * (ex: "3")
   */
  public static boolean evaluateSharedDeviceCount(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object sharedObj = payload.get("shared_device_accounts");
      if (sharedObj == null) sharedObj = payload.get("sharedDeviceAccounts");

      int sharedCount = sharedObj instanceof Number ? ((Number) sharedObj).intValue() : 0;
      return sharedCount > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SHARED_DEVICE_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /** SHARED_IP_COUNT: Conta contas usando o mesmo IP. Formato valueSingle: "threshold" (ex: "5") */
  public static boolean evaluateSharedIpCount(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int threshold = Integer.parseInt(condition.getValueSingle().trim());

      Object sharedObj = payload.get("shared_ip_accounts");
      if (sharedObj == null) sharedObj = payload.get("sharedIpAccounts");

      int sharedCount = sharedObj instanceof Number ? ((Number) sharedObj).intValue() : 0;
      return sharedCount > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar SHARED_IP_COUNT: {}", e.getMessage());
      return false;
    }
  }

  /**
   * ACCOUNT_LINK_DEPTH: Profundidade de links entre contas. Formato valueSingle: "maxDepth" (ex:
   * "3" = 3 saltos)
   */
  public static boolean evaluateAccountLinkDepth(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      int maxDepth = Integer.parseInt(condition.getValueSingle().trim());

      Object depthObj = payload.get("account_link_depth");
      if (depthObj == null) depthObj = payload.get("accountLinkDepth");

      int depth = depthObj instanceof Number ? ((Number) depthObj).intValue() : 0;
      return depth >= maxDepth;
    } catch (Exception e) {
      log.error("Erro ao avaliar ACCOUNT_LINK_DEPTH: {}", e.getMessage());
      return false;
    }
  }

  /**
   * CIRCULAR_TRANSFER_DETECTION: Detecta transferências circulares. Formato valueSingle: "true"
   * (verifica se há ciclo)
   */
  public static boolean evaluateCircularTransferDetection(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object circularObj = payload.get("circular_transfer_detected");
      if (circularObj == null) circularObj = payload.get("circularTransferDetected");

      return Boolean.TRUE.equals(circularObj)
          || "true".equalsIgnoreCase(String.valueOf(circularObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CIRCULAR_TRANSFER_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  /**
   * RAPID_MULTI_HOP: Detecta múltiplos saltos rápidos. Formato valueSingle: "hops|minutes" (ex:
   * "3|10")
   */
  public static boolean evaluateRapidMultiHop(RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      String[] parts = condition.getValueSingle().split("\\|");
      int hopThreshold = Integer.parseInt(parts[0].trim());

      Object hopCountObj = payload.get("rapid_hop_count");
      if (hopCountObj == null) hopCountObj = payload.get("rapidHopCount");

      int hopCount = hopCountObj instanceof Number ? ((Number) hopCountObj).intValue() : 0;
      return hopCount >= hopThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar RAPID_MULTI_HOP: {}", e.getMessage());
      return false;
    }
  }

  /**
   * BENEFICIARY_CONCENTRATION: Concentração de valores em poucos beneficiários. Formato
   * valueSingle: "percentageThreshold" (ex: "80" = 80% para top 3)
   */
  public static boolean evaluateBeneficiaryConcentration(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double percentageThreshold = Double.parseDouble(condition.getValueSingle().trim());

      Object concentrationObj = payload.get("beneficiary_concentration_pct");
      if (concentrationObj == null) concentrationObj = payload.get("beneficiaryConcentrationPct");

      double concentration =
          concentrationObj instanceof Number ? ((Number) concentrationObj).doubleValue() : 0.0;
      return concentration > percentageThreshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BENEFICIARY_CONCENTRATION: {}", e.getMessage());
      return false;
    }
  }
}