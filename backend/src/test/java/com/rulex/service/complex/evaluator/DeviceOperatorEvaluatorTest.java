package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Testes unitários para DeviceOperatorEvaluator.
 *
 * <p>GAP-2 FIX: Cobertura de testes para evaluators.
 *
 * @version 1.0.0
 */
@DisplayName("DeviceOperatorEvaluator Tests")
class DeviceOperatorEvaluatorTest {

  private DeviceOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new DeviceOperatorEvaluator();
  }

  // ========== HELPER METHODS ==========

  private RuleCondition createCondition(
      String fieldName, ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(fieldName);
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  // ========== SUPPORTED OPERATORS ==========

  @Test
  @DisplayName("Deve suportar operadores de device")
  void shouldSupportDeviceOperators() {
    var supported = evaluator.getSupportedOperators();

    assertThat(supported)
        .contains(
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
  }

  @Test
  @DisplayName("Deve retornar categoria correta")
  void shouldReturnCorrectCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("DEVICE");
  }

  // ========== DEVICE_CHANGED_IN_SESSION ==========

  @Nested
  @DisplayName("Operador DEVICE_CHANGED_IN_SESSION")
  class DeviceChangedInSessionTests {

    @Test
    @DisplayName("Deve detectar mudança de device na sessão")
    void shouldDetectDeviceChangeInSession() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_CHANGED_IN_SESSION, null);
      EvaluationContext context = createContext(Map.of("deviceChangedInSession", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando device não mudou")
    void shouldReturnFalseWhenDeviceNotChanged() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_CHANGED_IN_SESSION, null);
      EvaluationContext context = createContext(Map.of("deviceChangedInSession", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== DEVICE_JAILBREAK_ROOTED ==========

  @Nested
  @DisplayName("Operador DEVICE_JAILBREAK_ROOTED")
  class DeviceJailbreakRootedTests {

    @Test
    @DisplayName("Deve detectar device rooted/jailbroken")
    void shouldDetectRootedDevice() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_JAILBREAK_ROOTED, null);
      EvaluationContext context = createContext(Map.of("isRooted", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para device não rooted")
    void shouldReturnFalseForNonRootedDevice() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_JAILBREAK_ROOTED, null);
      EvaluationContext context = createContext(Map.of("isRooted", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== EMULATOR_DETECTION ==========

  @Nested
  @DisplayName("Operador EMULATOR_DETECTION")
  class EmulatorDetectionTests {

    @Test
    @DisplayName("Deve detectar emulador")
    void shouldDetectEmulator() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.EMULATOR_DETECTION, null);
      EvaluationContext context = createContext(Map.of("isEmulator", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para device real")
    void shouldReturnFalseForRealDevice() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.EMULATOR_DETECTION, null);
      EvaluationContext context = createContext(Map.of("isEmulator", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== VPN_PROXY_DETECTION ==========

  @Nested
  @DisplayName("Operador VPN_PROXY_DETECTION")
  class VpnProxyDetectionTests {

    @Test
    @DisplayName("Deve detectar uso de VPN")
    void shouldDetectVpnUsage() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.VPN_PROXY_DETECTION, null);
      EvaluationContext context = createContext(Map.of("isVpn", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve detectar uso de proxy")
    void shouldDetectProxyUsage() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.VPN_PROXY_DETECTION, null);
      EvaluationContext context = createContext(Map.of("isProxy", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando não usa VPN/proxy")
    void shouldReturnFalseWhenNotUsingVpnProxy() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.VPN_PROXY_DETECTION, null);
      EvaluationContext context = createContext(Map.of("isVpn", false, "isProxy", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== TOR_EXIT_NODE ==========

  @Nested
  @DisplayName("Operador TOR_EXIT_NODE")
  class TorExitNodeTests {

    @Test
    @DisplayName("Deve detectar uso de Tor")
    void shouldDetectTorUsage() {
      RuleCondition condition = createCondition("device", ConditionOperator.TOR_EXIT_NODE, null);
      EvaluationContext context = createContext(Map.of("isTor", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando não usa Tor")
    void shouldReturnFalseWhenNotUsingTor() {
      RuleCondition condition = createCondition("device", ConditionOperator.TOR_EXIT_NODE, null);
      EvaluationContext context = createContext(Map.of("isTor", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== DEVICE_TRUST_SCORE ==========

  @Nested
  @DisplayName("Operador DEVICE_TRUST_SCORE")
  class DeviceTrustScoreTests {

    @Test
    @DisplayName("Deve retornar true quando trust score é baixo")
    void shouldReturnTrueWhenTrustScoreLow() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_TRUST_SCORE, "50");
      EvaluationContext context = createContext(Map.of("deviceTrustScore", 30));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando trust score é alto")
    void shouldReturnFalseWhenTrustScoreHigh() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_TRUST_SCORE, "50");
      EvaluationContext context = createContext(Map.of("deviceTrustScore", 80));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== SHARED_DEVICE_COUNT ==========

  @Nested
  @DisplayName("Operador SHARED_DEVICE_COUNT")
  class SharedDeviceCountTests {

    @Test
    @DisplayName("Deve detectar device compartilhado por múltiplas contas")
    void shouldDetectSharedDevice() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.SHARED_DEVICE_COUNT, "3");
      EvaluationContext context = createContext(Map.of("sharedDeviceCount", 5));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false quando device não é compartilhado")
    void shouldReturnFalseWhenDeviceNotShared() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.SHARED_DEVICE_COUNT, "3");
      EvaluationContext context = createContext(Map.of("sharedDeviceCount", 1));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== DEVICE_ACCOUNT_RATIO ==========

  @Nested
  @DisplayName("Operador DEVICE_ACCOUNT_RATIO")
  class DeviceAccountRatioTests {

    @Test
    @DisplayName("Deve detectar ratio alto de contas por device")
    void shouldDetectHighAccountRatio() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_ACCOUNT_RATIO, "2");
      EvaluationContext context = createContext(Map.of("deviceAccountRatio", 5.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para ratio normal")
    void shouldReturnFalseForNormalRatio() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.DEVICE_ACCOUNT_RATIO, "2");
      EvaluationContext context = createContext(Map.of("deviceAccountRatio", 1.0));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== ANTI_DETECT_BROWSER_DETECTION ==========

  @Nested
  @DisplayName("Operador ANTI_DETECT_BROWSER_DETECTION")
  class AntiDetectBrowserDetectionTests {

    @Test
    @DisplayName("Deve detectar anti-detect browser")
    void shouldDetectAntiDetectBrowser() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.ANTI_DETECT_BROWSER_DETECTION, null);
      EvaluationContext context = createContext(Map.of("antiDetectBrowser", true));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve retornar false para browser normal")
    void shouldReturnFalseForNormalBrowser() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.ANTI_DETECT_BROWSER_DETECTION, null);
      EvaluationContext context = createContext(Map.of("antiDetectBrowser", false));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }
  }

  // ========== EDGE CASES ==========

  @Nested
  @DisplayName("Casos de Borda")
  class EdgeCaseTests {

    @Test
    @DisplayName("Deve tratar campo ausente graciosamente")
    void shouldHandleMissingFieldGracefully() {
      RuleCondition condition =
          createCondition("nonExistent", ConditionOperator.EMULATOR_DETECTION, null);
      EvaluationContext context = createContext(Map.of());

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve tratar valor nulo graciosamente")
    void shouldHandleNullValueGracefully() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.EMULATOR_DETECTION, null);
      java.util.HashMap<String, Object> payload = new java.util.HashMap<>();
      payload.put("isEmulator", null);
      EvaluationContext context = createContext(payload);

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isFalse();
    }

    @Test
    @DisplayName("Deve funcionar com valores como strings")
    void shouldWorkWithValuesAsStrings() {
      RuleCondition condition =
          createCondition("device", ConditionOperator.EMULATOR_DETECTION, null);
      EvaluationContext context = createContext(Map.of("isEmulator", "true"));

      boolean result = evaluator.evaluate(condition, context);

      assertThat(result).isTrue();
    }

    @Test
    @DisplayName("Deve detectar múltiplos indicadores de risco")
    void shouldDetectMultipleRiskIndicators() {
      EvaluationContext context =
          createContext(
              Map.of(
                  "isVpn", true,
                  "isEmulator", true,
                  "isRooted", true));

      RuleCondition vpnCondition =
          createCondition("device", ConditionOperator.VPN_PROXY_DETECTION, null);
      RuleCondition emulatorCondition =
          createCondition("device", ConditionOperator.EMULATOR_DETECTION, null);
      RuleCondition rootedCondition =
          createCondition("device", ConditionOperator.DEVICE_JAILBREAK_ROOTED, null);

      assertThat(evaluator.evaluate(vpnCondition, context)).isTrue();
      assertThat(evaluator.evaluate(emulatorCondition, context)).isTrue();
      assertThat(evaluator.evaluate(rootedCondition, context)).isTrue();
    }
  }
}
