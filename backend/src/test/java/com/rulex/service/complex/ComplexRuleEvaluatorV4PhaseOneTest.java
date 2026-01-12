package com.rulex.service.complex;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleConditionGroup.GroupLogicOperator;
import com.rulex.service.GeoService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para operadores V4.0 Phase 1: Velocity + Device (40 operadores) Categorias L, M, N, O
 *
 * <p>Foco em: Device Fingerprint (Categoria O) - operadores baseados em payload
 */
@DisplayName("ComplexRuleEvaluator V4 Phase 1 Tests")
class ComplexRuleEvaluatorV4PhaseOneTest {

  private GeoService geoService;
  private VelocityService velocityService;
  private VelocityServiceFacade velocityServiceFacade;
  private ComplexRuleEvaluator evaluator;

  private TransactionRequest transactionRequest;
  private Map<String, Object> payload;

  @BeforeEach
  void setUp() {
    geoService = Mockito.mock(GeoService.class);
    velocityService = Mockito.mock(VelocityService.class);
    velocityServiceFacade = Mockito.mock(VelocityServiceFacade.class);
    evaluator = new ComplexRuleEvaluator(geoService, velocityService, velocityServiceFacade);

    transactionRequest = new TransactionRequest();
    transactionRequest.setTransactionAmount(BigDecimal.valueOf(1000));
    transactionRequest.setPan("4111111111111111");
    transactionRequest.setMerchantId("MERCH-001");

    payload = new HashMap<>();
  }

  private ComplexRuleEvaluator.EvaluationContext createContext() {
    return ComplexRuleEvaluator.EvaluationContext.builder()
        .transactionRequest(transactionRequest)
        .payload(payload)
        .build();
  }

  private RuleCondition createCondition(ConditionOperator operator, String valueSingle) {
    return RuleCondition.builder()
        .operator(operator)
        .fieldName("test_field")
        .valueSingle(valueSingle)
        .build();
  }

  private RuleConditionGroup createGroup(GroupLogicOperator logic, RuleCondition... conditions) {
    return RuleConditionGroup.builder()
        .logicOperator(logic)
        .conditions(List.of(conditions))
        .build();
  }

  // ========== CATEGORIA L: Transaction Count Velocity (using VelocityServiceFacade) ==========

  @Nested
  @DisplayName("CATEGORIA L: Transaction Count Velocity")
  class CategoryLTests {

    @Test
    @DisplayName(
        "TRANSACTION_COUNT_PER_CARD_HOUR - Deve retornar true quando contagem excede threshold")
    void transactionCountPerCardHour_exceedsThreshold_returnsTrue() {
      VelocityService.VelocityStats stats = mock(VelocityService.VelocityStats.class);
      when(stats.getTransactionCount()).thenReturn(10L);
      when(velocityServiceFacade.getStats(
              any(TransactionRequest.class), eq(VelocityService.KeyType.PAN), any()))
          .thenReturn(stats);

      RuleCondition condition =
          createCondition(ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR, "5");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName(
        "TRANSACTION_COUNT_PER_CARD_HOUR - Deve retornar false quando contagem abaixo do threshold")
    void transactionCountPerCardHour_belowThreshold_returnsFalse() {
      VelocityService.VelocityStats stats = mock(VelocityService.VelocityStats.class);
      when(stats.getTransactionCount()).thenReturn(3L);
      when(velocityServiceFacade.getStats(
              any(TransactionRequest.class), eq(VelocityService.KeyType.PAN), any()))
          .thenReturn(stats);

      RuleCondition condition =
          createCondition(ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR, "5");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isFalse();
    }
  }

  // ========== CATEGORIA M: Amount Velocity (using VelocityServiceFacade) ==========

  @Nested
  @DisplayName("CATEGORIA M: Amount Velocity")
  class CategoryMTests {

    @Test
    @DisplayName("AMOUNT_SUM_PER_CARD_HOUR - Deve verificar soma de valores por cartão")
    void amountSumPerCardHour_exceedsThreshold_returnsTrue() {
      VelocityService.VelocityStats stats = mock(VelocityService.VelocityStats.class);
      when(stats.getTotalAmount()).thenReturn(BigDecimal.valueOf(15000));
      when(velocityServiceFacade.getStats(
              any(TransactionRequest.class), eq(VelocityService.KeyType.PAN), any()))
          .thenReturn(stats);

      RuleCondition condition =
          createCondition(ConditionOperator.AMOUNT_SUM_PER_CARD_HOUR, "10000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("AMOUNT_VARIANCE_ANOMALY - Deve detectar anomalia de variância")
    void amountVarianceAnomaly_exceedsThreshold_returnsTrue() {
      VelocityService.VelocityStats stats = mock(VelocityService.VelocityStats.class);
      when(stats.getAvgAmount()).thenReturn(BigDecimal.valueOf(500));
      when(stats.getStdDevAmount()).thenReturn(BigDecimal.valueOf(100));
      when(velocityServiceFacade.getStats(
              any(TransactionRequest.class), eq(VelocityService.KeyType.PAN), any()))
          .thenReturn(stats);

      transactionRequest.setTransactionAmount(BigDecimal.valueOf(1500)); // 10 std devs away

      RuleCondition condition = createCondition(ConditionOperator.AMOUNT_VARIANCE_ANOMALY, "3.0");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }
  }

  // ========== CATEGORIA O: Device Fingerprint (payload-based - most stable tests) ==========

  @Nested
  @DisplayName("CATEGORIA O: Device Fingerprint")
  class CategoryOTests {

    @Test
    @DisplayName("DEVICE_TRUST_SCORE - Deve detectar score de confiança baixo")
    void deviceTrustScore_belowThreshold_returnsTrue() {
      payload.put("device_trust_score", 25.0);
      RuleCondition condition = createCondition(ConditionOperator.DEVICE_TRUST_SCORE, "50");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("DEVICE_TRUST_SCORE - Deve retornar false quando score é alto")
    void deviceTrustScore_highScore_returnsFalse() {
      payload.put("device_trust_score", 85.0);
      RuleCondition condition = createCondition(ConditionOperator.DEVICE_TRUST_SCORE, "50");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("CANVAS_FINGERPRINT_MISMATCH - Deve detectar incompatibilidade de canvas")
    void canvasFingerprintMismatch_detected_returnsTrue() {
      payload.put("canvas_fingerprint_mismatch", true);
      RuleCondition condition =
          createCondition(ConditionOperator.CANVAS_FINGERPRINT_MISMATCH, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("WEBGL_FINGERPRINT_ANOMALY - Deve detectar anomalia WebGL")
    void webglFingerprintAnomaly_detected_returnsTrue() {
      payload.put("webgl_fingerprint_anomaly", true);
      RuleCondition condition =
          createCondition(ConditionOperator.WEBGL_FINGERPRINT_ANOMALY, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("AUDIO_FINGERPRINT_NEW - Deve detectar fingerprint de áudio novo")
    void audioFingerprintNew_detected_returnsTrue() {
      payload.put("audio_fingerprint_new", true);
      RuleCondition condition = createCondition(ConditionOperator.AUDIO_FINGERPRINT_NEW, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("FONTS_FINGERPRINT_ANOMALY - Deve detectar anomalia de fontes")
    void fontsFingerprintAnomaly_detected_returnsTrue() {
      payload.put("fonts_fingerprint_anomaly", true);
      RuleCondition condition =
          createCondition(ConditionOperator.FONTS_FINGERPRINT_ANOMALY, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("SCREEN_RESOLUTION_CHANGE - Deve detectar mudança de resolução")
    void screenResolutionChange_detected_returnsTrue() {
      payload.put("screen_resolution_changed", true);
      RuleCondition condition = createCondition(ConditionOperator.SCREEN_RESOLUTION_CHANGE, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("BATTERY_LEVEL_ANOMALY - Deve detectar anomalia de bateria")
    void batteryLevelAnomaly_detected_returnsTrue() {
      payload.put("battery_level_anomaly", true);
      RuleCondition condition = createCondition(ConditionOperator.BATTERY_LEVEL_ANOMALY, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("HARDWARE_CONCURRENCY_MISMATCH - Deve detectar incompatibilidade de hardware")
    void hardwareConcurrencyMismatch_detected_returnsTrue() {
      payload.put("hardware_concurrency_mismatch", true);
      RuleCondition condition =
          createCondition(ConditionOperator.HARDWARE_CONCURRENCY_MISMATCH, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("TOUCH_SUPPORT_INCONSISTENCY - Deve detectar inconsistência de touch")
    void touchSupportInconsistency_detected_returnsTrue() {
      payload.put("touch_support_inconsistency", true);
      RuleCondition condition =
          createCondition(ConditionOperator.TOUCH_SUPPORT_INCONSISTENCY, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("DEVICE_MEMORY_ANOMALY - Deve detectar anomalia de memória")
    void deviceMemoryAnomaly_detected_returnsTrue() {
      payload.put("device_memory_anomaly", true);
      RuleCondition condition = createCondition(ConditionOperator.DEVICE_MEMORY_ANOMALY, "true");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isTrue();
    }
  }

  // ========== TESTES DE EDGE CASES ==========

  @Nested
  @DisplayName("Edge Cases")
  class EdgeCaseTests {

    @Test
    @DisplayName("Deve retornar false quando payload é null para operador de device")
    void nullPayload_returnsFalse() {
      ComplexRuleEvaluator.EvaluationContext context =
          ComplexRuleEvaluator.EvaluationContext.builder()
              .transactionRequest(transactionRequest)
              .payload(null)
              .build();

      RuleCondition condition = createCondition(ConditionOperator.DEVICE_TRUST_SCORE, "50");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar false quando TransactionRequest é null para operador de velocidade")
    void nullTransactionRequest_returnsFalse() {
      ComplexRuleEvaluator.EvaluationContext context =
          ComplexRuleEvaluator.EvaluationContext.builder()
              .transactionRequest(null)
              .payload(payload)
              .build();

      RuleCondition condition =
          createCondition(ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR, "5");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("Deve retornar false quando valueSingle é inválido")
    void invalidValueSingle_returnsFalse() {
      RuleCondition condition =
          createCondition(ConditionOperator.TRANSACTION_COUNT_PER_CARD_HOUR, "invalid");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName(
        "Deve usar fallback key deviceReputationScore quando device_trust_score não existe")
    void fallbackKey_deviceReputationScore_works() {
      payload.put("deviceReputationScore", 25.0);
      RuleCondition condition = createCondition(ConditionOperator.DEVICE_TRUST_SCORE, "50");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());
      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Operador com payload vazio deve retornar valor padrão")
    void emptyPayload_returnsDefault() {
      // With empty payload, device_trust_score defaults to 100.0
      // So checking for < 50 should return false
      RuleCondition condition = createCondition(ConditionOperator.DEVICE_TRUST_SCORE, "50");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, createContext());
      assertThat(result.isMatched()).isFalse();
    }
  }
}
