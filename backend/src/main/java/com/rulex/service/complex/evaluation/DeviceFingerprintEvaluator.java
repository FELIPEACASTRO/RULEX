package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class DeviceFingerprintEvaluator {

  private DeviceFingerprintEvaluator() {}

  public static boolean evaluateDeviceTrustScore(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      double minScore = Double.parseDouble(condition.getValueSingle().trim());

      Object scoreObj = payload.get("device_trust_score");
      if (scoreObj == null) {
        scoreObj = payload.get("deviceReputationScore");
      }

      double score = scoreObj instanceof Number ? ((Number) scoreObj).doubleValue() : 100.0;
      return score < minScore;
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_TRUST_SCORE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateCanvasFingerprintMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object mismatchObj = payload.get("canvas_fingerprint_mismatch");
      if (mismatchObj == null) {
        mismatchObj = payload.get("canvasMismatch");
      }

      return Boolean.TRUE.equals(mismatchObj)
          || "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar CANVAS_FINGERPRINT_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateWebglFingerprintAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("webgl_fingerprint_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("webglAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar WEBGL_FINGERPRINT_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateAudioFingerprintNew(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object isNewObj = payload.get("audio_fingerprint_new");
      if (isNewObj == null) {
        isNewObj = payload.get("audioFingerprintIsNew");
      }

      return Boolean.TRUE.equals(isNewObj) || "true".equalsIgnoreCase(String.valueOf(isNewObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar AUDIO_FINGERPRINT_NEW: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateFontsFingerprintAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("fonts_fingerprint_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("fontsAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar FONTS_FINGERPRINT_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateScreenResolutionChange(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object changedObj = payload.get("screen_resolution_changed");
      if (changedObj == null) {
        changedObj = payload.get("screenResolutionChanged");
      }

      return Boolean.TRUE.equals(changedObj) || "true".equalsIgnoreCase(String.valueOf(changedObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar SCREEN_RESOLUTION_CHANGE: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateBatteryLevelAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("battery_level_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("batteryAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar BATTERY_LEVEL_ANOMALY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateHardwareConcurrencyMismatch(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object mismatchObj = payload.get("hardware_concurrency_mismatch");
      if (mismatchObj == null) {
        mismatchObj = payload.get("hardwareConcurrencyMismatch");
      }

      return Boolean.TRUE.equals(mismatchObj)
          || "true".equalsIgnoreCase(String.valueOf(mismatchObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar HARDWARE_CONCURRENCY_MISMATCH: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateTouchSupportInconsistency(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object inconsistencyObj = payload.get("touch_support_inconsistency");
      if (inconsistencyObj == null) {
        inconsistencyObj = payload.get("touchSupportInconsistency");
      }

      return Boolean.TRUE.equals(inconsistencyObj)
          || "true".equalsIgnoreCase(String.valueOf(inconsistencyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar TOUCH_SUPPORT_INCONSISTENCY: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateDeviceMemoryAnomaly(
      RuleCondition condition, EvaluationContext context) {
    try {
      Map<String, Object> payload = context.getPayload();
      if (payload == null) return false;

      Object anomalyObj = payload.get("device_memory_anomaly");
      if (anomalyObj == null) {
        anomalyObj = payload.get("deviceMemoryAnomaly");
      }

      return Boolean.TRUE.equals(anomalyObj) || "true".equalsIgnoreCase(String.valueOf(anomalyObj));
    } catch (Exception e) {
      log.error("Erro ao avaliar DEVICE_MEMORY_ANOMALY: {}", e.getMessage());
      return false;
    }
  }
}
