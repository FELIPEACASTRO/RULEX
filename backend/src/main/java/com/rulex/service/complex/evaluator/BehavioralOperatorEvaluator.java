package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/** Avaliador para operadores comportamentais e biométricos. */
@Component
@Slf4j
public class BehavioralOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          // Comportamentais
          ConditionOperator.ADAPTIVE_BEHAVIORAL_ANALYTICS,
          ConditionOperator.ADAPTIVE_PARAMETRIC_THRESHOLD,
          ConditionOperator.BEHAVIORAL_BASELINE_DEVIATION,
          ConditionOperator.BUSINESS_HOURS_DEVIATION,
          ConditionOperator.CHANNEL_SWITCH_PATTERN,
          ConditionOperator.CHANNEL_USAGE_ANOMALY,
          ConditionOperator.FREQUENCY_PATTERN_CHANGE,
          ConditionOperator.GEOGRAPHIC_BEHAVIOR_SHIFT,
          ConditionOperator.LOGIN_PATTERN_DEVIATION,
          ConditionOperator.NAVIGATION_PATTERN_ANOMALY,
          ConditionOperator.SESSION_BEHAVIOR_ANOMALY,
          ConditionOperator.SPENDING_CATEGORY_SHIFT,
          ConditionOperator.TIME_PREFERENCE_SHIFT,
          ConditionOperator.UNUSUAL_BUSINESS_PATTERN,

          // Biométricos
          ConditionOperator.BIOMETRIC_KEYSTROKE_DYNAMICS,
          ConditionOperator.BIOMETRIC_MOUSE_MOVEMENT,
          ConditionOperator.BIOMETRIC_SCROLL_VELOCITY,
          ConditionOperator.FACE_TO_ID_PHOTO_MATCHING,
          ConditionOperator.LIVENESS_DETECTION_FACIAL,
          ConditionOperator.LIVENESS_DETECTION_VOICE,

          // Fingerprint
          ConditionOperator.AUDIO_FINGERPRINT_NEW,
          ConditionOperator.CANVAS_FINGERPRINT_MISMATCH,
          ConditionOperator.FONTS_FINGERPRINT_ANOMALY,
          ConditionOperator.WEBGL_FINGERPRINT_ANOMALY,

          // Device
          ConditionOperator.BATTERY_LEVEL_ANOMALY,
          ConditionOperator.HARDWARE_CONCURRENCY_MISMATCH,
          ConditionOperator.SCREEN_RESOLUTION_CHANGE,
          ConditionOperator.TOUCH_SUPPORT_INCONSISTENCY);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    log.debug("BehavioralOperatorEvaluator: op={}", op);

    return switch (op) {
      case ADAPTIVE_BEHAVIORAL_ANALYTICS ->
          evaluateScore(context, "adaptiveBehavioralScore", condition, 0.7);
      case ADAPTIVE_PARAMETRIC_THRESHOLD -> evaluateBoolean(context, "adaptiveThresholdExceeded");
      case BEHAVIORAL_BASELINE_DEVIATION ->
          evaluateScore(context, "behavioralBaselineDeviation", condition, 2.0);
      case BUSINESS_HOURS_DEVIATION -> evaluateBoolean(context, "businessHoursDeviation");
      case CHANNEL_SWITCH_PATTERN -> evaluateBoolean(context, "channelSwitchPattern");
      case CHANNEL_USAGE_ANOMALY -> evaluateBoolean(context, "channelUsageAnomaly");
      case FREQUENCY_PATTERN_CHANGE -> evaluateBoolean(context, "frequencyPatternChange");
      case GEOGRAPHIC_BEHAVIOR_SHIFT -> evaluateBoolean(context, "geographicBehaviorShift");
      case LOGIN_PATTERN_DEVIATION -> evaluateBoolean(context, "loginPatternDeviation");
      case NAVIGATION_PATTERN_ANOMALY -> evaluateBoolean(context, "navigationPatternAnomaly");
      case SESSION_BEHAVIOR_ANOMALY -> evaluateBoolean(context, "sessionBehaviorAnomaly");
      case SPENDING_CATEGORY_SHIFT -> evaluateBoolean(context, "spendingCategoryShift");
      case TIME_PREFERENCE_SHIFT -> evaluateBoolean(context, "timePreferenceShift");
      case UNUSUAL_BUSINESS_PATTERN -> evaluateBoolean(context, "unusualBusinessPattern");
      case BIOMETRIC_KEYSTROKE_DYNAMICS -> evaluateBoolean(context, "biometricKeystrokeMismatch");
      case BIOMETRIC_MOUSE_MOVEMENT -> evaluateBoolean(context, "biometricMouseMismatch");
      case BIOMETRIC_SCROLL_VELOCITY -> evaluateBoolean(context, "biometricScrollMismatch");
      case FACE_TO_ID_PHOTO_MATCHING ->
          evaluateScoreLessThan(context, "faceToIdPhotoMatch", condition, 0.8);
      case LIVENESS_DETECTION_FACIAL -> evaluateBoolean(context, "livenessFacialFailed");
      case LIVENESS_DETECTION_VOICE -> evaluateBoolean(context, "livenessVoiceFailed");
      case AUDIO_FINGERPRINT_NEW -> evaluateBoolean(context, "audioFingerprintNew");
      case CANVAS_FINGERPRINT_MISMATCH -> evaluateBoolean(context, "canvasFingerprintMismatch");
      case FONTS_FINGERPRINT_ANOMALY -> evaluateBoolean(context, "fontsFingerprintAnomaly");
      case WEBGL_FINGERPRINT_ANOMALY -> evaluateBoolean(context, "webglFingerprintAnomaly");
      case BATTERY_LEVEL_ANOMALY -> evaluateBoolean(context, "batteryLevelAnomaly");
      case HARDWARE_CONCURRENCY_MISMATCH -> evaluateBoolean(context, "hardwareConcurrencyMismatch");
      case SCREEN_RESOLUTION_CHANGE -> evaluateBoolean(context, "screenResolutionChange");
      case TOUCH_SUPPORT_INCONSISTENCY -> evaluateBoolean(context, "touchSupportInconsistency");
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

  private boolean evaluateScore(
      EvaluationContext context, String key, RuleCondition condition, double defaultThreshold) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;
    Object value = payload.get(key);
    if (value == null) return false;

    BigDecimal score = parseBigDecimal(String.valueOf(value), BigDecimal.ZERO);
    BigDecimal threshold =
        parseBigDecimal(condition.getValueSingle(), new BigDecimal(defaultThreshold));
    return score.compareTo(threshold) > 0;
  }

  private boolean evaluateScoreLessThan(
      EvaluationContext context, String key, RuleCondition condition, double defaultThreshold) {
    Map<String, Object> payload = context.getPayload();
    if (payload == null) return false;
    Object value = payload.get(key);
    if (value == null) return false;

    if (value instanceof Boolean) {
      return !(Boolean) value;
    }

    BigDecimal score = parseBigDecimal(String.valueOf(value), BigDecimal.ONE);
    BigDecimal threshold =
        parseBigDecimal(condition.getValueSingle(), new BigDecimal(defaultThreshold));
    return score.compareTo(threshold) < 0;
  }

  private BigDecimal parseBigDecimal(String value, BigDecimal defaultValue) {
    try {
      return new BigDecimal(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  @Override
  public String getCategory() {
    return "BEHAVIORAL";
  }
}
