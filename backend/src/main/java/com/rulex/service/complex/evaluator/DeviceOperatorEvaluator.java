package com.rulex.service.complex.evaluator;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de dispositivo.
 *
 * <p>Operadores suportados:
 *
 * <ul>
 *   <li>DEVICE_CHANGED_IN_SESSION - mudança de dispositivo na sessão
 *   <li>DEVICE_JAILBREAK_ROOTED - dispositivo com jailbreak/root
 *   <li>EMULATOR_DETECTION - detecção de emulador
 *   <li>VPN_PROXY_DETECTION - detecção de VPN/Proxy
 *   <li>TOR_EXIT_NODE - nó de saída Tor
 *   <li>BROWSER_INCONSISTENCY - inconsistência de browser
 *   <li>DEVICE_TRUST_SCORE - score de confiança do dispositivo
 * </ul>
 */
@Component
@Slf4j
public class DeviceOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          ConditionOperator.DEVICE_CHANGED_IN_SESSION,
          ConditionOperator.DEVICE_JAILBREAK_ROOTED,
          ConditionOperator.EMULATOR_DETECTION,
          ConditionOperator.VPN_PROXY_DETECTION,
          ConditionOperator.TOR_EXIT_NODE,
          ConditionOperator.BROWSER_INCONSISTENCY,
          ConditionOperator.DEVICE_TRUST_SCORE,
          ConditionOperator.DEVICE_MEMORY_ANOMALY,
          ConditionOperator.SHARED_DEVICE_COUNT,
          ConditionOperator.DEVICE_ACCOUNT_RATIO,
          ConditionOperator.DEVICE_FINGERPRINT_CONSISTENCY_CHECK,
          ConditionOperator.ANTI_DETECT_BROWSER_DETECTION);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    TransactionRequest request = context.getTransactionRequest();
    Map<String, Object> payload = context.getPayload();

    log.debug("DeviceOperatorEvaluator: op={}", op);

    try {
      return switch (op) {
        case DEVICE_CHANGED_IN_SESSION -> evaluateDeviceChanged(request, payload, condition);
        case DEVICE_JAILBREAK_ROOTED -> evaluateJailbreakRooted(request, payload);
        case EMULATOR_DETECTION -> evaluateEmulator(request, payload);
        case VPN_PROXY_DETECTION -> evaluateVpnProxy(request, payload);
        case TOR_EXIT_NODE -> evaluateTorExitNode(request, payload);
        case BROWSER_INCONSISTENCY -> evaluateBrowserInconsistency(request, payload);
        case DEVICE_TRUST_SCORE -> evaluateDeviceTrustScore(request, payload, condition);
        case DEVICE_MEMORY_ANOMALY -> evaluateMemoryAnomaly(request, payload);
        case SHARED_DEVICE_COUNT -> evaluateSharedDeviceCount(request, payload, condition);
        case DEVICE_ACCOUNT_RATIO -> evaluateDeviceAccountRatio(request, payload, condition);
        case DEVICE_FINGERPRINT_CONSISTENCY_CHECK ->
            evaluateFingerprintConsistency(request, payload);
        case ANTI_DETECT_BROWSER_DETECTION -> evaluateAntiDetectBrowser(request, payload);
        default -> false;
      };
    } catch (Exception e) {
      log.error("Erro ao avaliar operador de dispositivo {}: {}", op, e.getMessage());
      return false;
    }
  }

  private boolean evaluateDeviceChanged(
      TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
    // Verificar se o dispositivo mudou durante a sessão
    Object currentDevice = getDeviceId(request, payload);
    Object previousDevice = payload != null ? payload.get("previousDeviceId") : null;

    if (currentDevice == null || previousDevice == null) {
      return false;
    }

    boolean changed = !currentDevice.equals(previousDevice);
    log.debug(
        "DEVICE_CHANGED_IN_SESSION: current={}, previous={}, changed={}",
        currentDevice,
        previousDevice,
        changed);
    return changed;
  }

  private boolean evaluateJailbreakRooted(TransactionRequest request, Map<String, Object> payload) {
    // Verificar flags de jailbreak/root no payload
    Boolean isJailbroken = getBooleanFromPayload(payload, "isJailbroken");
    Boolean isRooted = getBooleanFromPayload(payload, "isRooted");
    Boolean deviceCompromised = getBooleanFromPayload(payload, "deviceCompromised");

    boolean result =
        Boolean.TRUE.equals(isJailbroken)
            || Boolean.TRUE.equals(isRooted)
            || Boolean.TRUE.equals(deviceCompromised);
    log.debug(
        "DEVICE_JAILBREAK_ROOTED: jailbroken={}, rooted={}, compromised={}",
        isJailbroken,
        isRooted,
        deviceCompromised);
    return result;
  }

  private boolean evaluateEmulator(TransactionRequest request, Map<String, Object> payload) {
    Boolean isEmulator = getBooleanFromPayload(payload, "isEmulator");
    Boolean isVirtualMachine = getBooleanFromPayload(payload, "isVirtualMachine");

    boolean result = Boolean.TRUE.equals(isEmulator) || Boolean.TRUE.equals(isVirtualMachine);
    log.debug("EMULATOR_DETECTION: emulator={}, vm={}", isEmulator, isVirtualMachine);
    return result;
  }

  private boolean evaluateVpnProxy(TransactionRequest request, Map<String, Object> payload) {
    Boolean isVpn = getBooleanFromPayload(payload, "isVpn");
    Boolean isProxy = getBooleanFromPayload(payload, "isProxy");
    Boolean isDatacenter = getBooleanFromPayload(payload, "isDatacenter");

    boolean result =
        Boolean.TRUE.equals(isVpn)
            || Boolean.TRUE.equals(isProxy)
            || Boolean.TRUE.equals(isDatacenter);
    log.debug("VPN_PROXY_DETECTION: vpn={}, proxy={}, datacenter={}", isVpn, isProxy, isDatacenter);
    return result;
  }

  private boolean evaluateTorExitNode(TransactionRequest request, Map<String, Object> payload) {
    Boolean isTor = getBooleanFromPayload(payload, "isTor");
    Boolean isTorExitNode = getBooleanFromPayload(payload, "isTorExitNode");

    boolean result = Boolean.TRUE.equals(isTor) || Boolean.TRUE.equals(isTorExitNode);
    log.debug("TOR_EXIT_NODE: tor={}, exitNode={}", isTor, isTorExitNode);
    return result;
  }

  private boolean evaluateBrowserInconsistency(
      TransactionRequest request, Map<String, Object> payload) {
    // Verificar inconsistências entre user agent, headers e fingerprint
    Boolean hasInconsistency = getBooleanFromPayload(payload, "browserInconsistency");
    Integer inconsistencyScore = getIntFromPayload(payload, "browserInconsistencyScore");

    if (Boolean.TRUE.equals(hasInconsistency)) {
      return true;
    }

    // Score > 50 indica inconsistência significativa
    if (inconsistencyScore != null && inconsistencyScore > 50) {
      log.debug("BROWSER_INCONSISTENCY: score={}", inconsistencyScore);
      return true;
    }

    return false;
  }

  private boolean evaluateDeviceTrustScore(
      TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
    Integer trustScore = getIntFromPayload(payload, "deviceTrustScore");
    if (trustScore == null) {
      trustScore = getIntFromPayload(payload, "trustScore");
    }

    if (trustScore == null) {
      return false;
    }

    int threshold = parseIntSafe(condition.getValueSingle(), 50);
    log.debug("DEVICE_TRUST_SCORE: score={}, threshold={}", trustScore, threshold);
    return trustScore < threshold; // Score baixo = dispositivo não confiável
  }

  private boolean evaluateMemoryAnomaly(TransactionRequest request, Map<String, Object> payload) {
    Boolean hasMemoryAnomaly = getBooleanFromPayload(payload, "memoryAnomaly");
    Boolean hasHookingDetected = getBooleanFromPayload(payload, "hookingDetected");

    boolean result =
        Boolean.TRUE.equals(hasMemoryAnomaly) || Boolean.TRUE.equals(hasHookingDetected);
    log.debug(
        "DEVICE_MEMORY_ANOMALY: memoryAnomaly={}, hooking={}",
        hasMemoryAnomaly,
        hasHookingDetected);
    return result;
  }

  private boolean evaluateSharedDeviceCount(
      TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
    Integer sharedCount = getIntFromPayload(payload, "sharedDeviceCount");
    if (sharedCount == null) {
      sharedCount = getIntFromPayload(payload, "accountsOnDevice");
    }

    if (sharedCount == null) {
      return false;
    }

    int threshold = parseIntSafe(condition.getValueSingle(), 3);
    log.debug("SHARED_DEVICE_COUNT: count={}, threshold={}", sharedCount, threshold);
    return sharedCount > threshold;
  }

  private boolean evaluateDeviceAccountRatio(
      TransactionRequest request, Map<String, Object> payload, RuleCondition condition) {
    Double ratio = getDoubleFromPayload(payload, "deviceAccountRatio");
    if (ratio == null) {
      return false;
    }

    double threshold = parseDoubleSafe(condition.getValueSingle(), 2.0);
    log.debug("DEVICE_ACCOUNT_RATIO: ratio={}, threshold={}", ratio, threshold);
    return ratio > threshold;
  }

  private boolean evaluateFingerprintConsistency(
      TransactionRequest request, Map<String, Object> payload) {
    Boolean isConsistent = getBooleanFromPayload(payload, "fingerprintConsistent");
    Integer consistencyScore = getIntFromPayload(payload, "fingerprintConsistencyScore");

    if (Boolean.FALSE.equals(isConsistent)) {
      return true; // Inconsistente = suspeito
    }

    // Score < 70 indica inconsistência
    if (consistencyScore != null && consistencyScore < 70) {
      log.debug("FINGERPRINT_CONSISTENCY: score={}", consistencyScore);
      return true;
    }

    return false;
  }

  private boolean evaluateAntiDetectBrowser(
      TransactionRequest request, Map<String, Object> payload) {
    Boolean isAntiDetect = getBooleanFromPayload(payload, "antiDetectBrowser");
    Boolean hasCanvasManipulation = getBooleanFromPayload(payload, "canvasManipulation");
    Boolean hasWebGLManipulation = getBooleanFromPayload(payload, "webglManipulation");

    boolean result =
        Boolean.TRUE.equals(isAntiDetect)
            || Boolean.TRUE.equals(hasCanvasManipulation)
            || Boolean.TRUE.equals(hasWebGLManipulation);
    log.debug(
        "ANTI_DETECT_BROWSER: antiDetect={}, canvas={}, webgl={}",
        isAntiDetect,
        hasCanvasManipulation,
        hasWebGLManipulation);
    return result;
  }

  private Object getDeviceId(TransactionRequest request, Map<String, Object> payload) {
    if (payload != null) {
      Object deviceId = payload.get("deviceId");
      if (deviceId != null) return deviceId;

      deviceId = payload.get("deviceFingerprint");
      if (deviceId != null) return deviceId;
    }

    // TransactionRequest não tem deviceFingerprint, usar payload apenas
    return null;
  }

  private Boolean getBooleanFromPayload(Map<String, Object> payload, String key) {
    if (payload == null) return null;
    Object value = payload.get(key);
    if (value instanceof Boolean) return (Boolean) value;
    if (value instanceof String) return Boolean.parseBoolean((String) value);
    return null;
  }

  private Integer getIntFromPayload(Map<String, Object> payload, String key) {
    if (payload == null) return null;
    Object value = payload.get(key);
    if (value instanceof Number) return ((Number) value).intValue();
    if (value instanceof String) {
      try {
        return Integer.parseInt((String) value);
      } catch (NumberFormatException e) {
        return null;
      }
    }
    return null;
  }

  private Double getDoubleFromPayload(Map<String, Object> payload, String key) {
    if (payload == null) return null;
    Object value = payload.get(key);
    if (value instanceof Number) return ((Number) value).doubleValue();
    if (value instanceof String) {
      try {
        return Double.parseDouble((String) value);
      } catch (NumberFormatException e) {
        return null;
      }
    }
    return null;
  }

  private int parseIntSafe(String value, int defaultValue) {
    if (value == null || value.isEmpty()) return defaultValue;
    try {
      return Integer.parseInt(value);
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }

  private double parseDoubleSafe(String value, double defaultValue) {
    if (value == null || value.isEmpty()) return defaultValue;
    try {
      return Double.parseDouble(value);
    } catch (NumberFormatException e) {
      return defaultValue;
    }
  }

  @Override
  public String getCategory() {
    return "DEVICE";
  }
}
