package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.parsing.BooleanParser;
import java.util.Arrays;
import java.util.List;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class DeviceRiskEvaluator {

  private DeviceRiskEvaluator() {}

  public static boolean evaluateDeviceJailbreakRooted(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object rootedObj = context.getPayload().get("device_rooted");
      if (rootedObj == null) rootedObj = context.getPayload().get("deviceRooted");
      if (rootedObj == null) rootedObj = context.getPayload().get("jailbroken");
      return Boolean.TRUE.equals(BooleanParser.toBoolean(rootedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_JAILBREAK_ROOTED: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateEmulatorDetection(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object emuObj = context.getPayload().get("emulator_detected");
      if (emuObj == null) emuObj = context.getPayload().get("emulatorDetected");
      return Boolean.TRUE.equals(BooleanParser.toBoolean(emuObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar EMULATOR_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateVpnProxyDetection(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object vpnObj = context.getPayload().get("vpn_or_proxy");
      if (vpnObj == null) vpnObj = context.getPayload().get("vpnOrProxy");
      if (vpnObj == null) vpnObj = context.getPayload().get("proxy_detected");
      return Boolean.TRUE.equals(BooleanParser.toBoolean(vpnObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar VPN_PROXY_DETECTION: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTorExitNode(Object fieldValue, RuleCondition condition) {
    try {
      if (fieldValue == null) return false;
      String ip = String.valueOf(fieldValue).trim();
      if (ip.isBlank()) return false;

      List<String> list = condition.getValueArray();
      if (list == null || list.isEmpty()) {
        String csv = condition.getValueSingle();
        if (csv == null || csv.isBlank()) return false;
        list = Arrays.stream(csv.split(",")).map(String::trim).filter(s -> !s.isBlank()).toList();
      }

      for (String entry : list) {
        if (ip.equalsIgnoreCase(String.valueOf(entry).trim())) return true;
      }
      return false;
    } catch (Exception e) {
      log.error("Erro ao avaliar TOR_EXIT_NODE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateBrowserInconsistency(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      double threshold = Double.parseDouble(condition.getValueSingle().trim());
      Object scoreObj = context.getPayload().get("browser_inconsistency_score");
      if (scoreObj == null) scoreObj = context.getPayload().get("browserInconsistencyScore");
      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 0.0;
      return score > threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar BROWSER_INCONSISTENCY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTimezoneMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("timezone_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("timezoneMismatch");
      return Boolean.TRUE.equals(BooleanParser.toBoolean(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TIMEZONE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateLanguageMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      if (context.getPayload() == null) return false;
      Object mismatchObj = context.getPayload().get("language_mismatch");
      if (mismatchObj == null) mismatchObj = context.getPayload().get("languageMismatch");
      if (mismatchObj != null) return Boolean.TRUE.equals(BooleanParser.toBoolean(mismatchObj));

      Object userLang = context.getPayload().get("user_language");
      if (userLang == null) userLang = context.getPayload().get("userLanguage");
      Object deviceLang = context.getPayload().get("device_language");
      if (deviceLang == null) deviceLang = context.getPayload().get("deviceLanguage");
      if (userLang == null || deviceLang == null) return false;
      return !String.valueOf(userLang).equalsIgnoreCase(String.valueOf(deviceLang));
    } catch (Exception e) {
      log.error("Erro ao avaliar LANGUAGE_MISMATCH: {}", e.getMessage());
      return false;
    }
  }
}
