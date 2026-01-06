package com.rulex.application.usecase;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.rulex.application.port.in.AnalyzeTransactionUseCase.AnalysisResult;
import com.rulex.application.port.out.*;
import com.rulex.domain.exception.TamperDetectedException;
import com.rulex.domain.model.*;
import java.math.BigDecimal;
import java.security.MessageDigest;
import java.time.Instant;
import java.util.HexFormat;
import java.util.*;
import org.junit.jupiter.api.*;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
@DisplayName("AnalyzeTransactionUseCaseImpl Tests")
class AnalyzeTransactionUseCaseImplTest {

  @Mock private RulePersistencePort rulePersistencePort;
  @Mock private TransactionPersistencePort transactionPersistencePort;
  @Mock private DecisionPersistencePort decisionPersistencePort;
  @Mock private RuleCachePort ruleCachePort;
  @Mock private MetricsPort metricsPort;
  @Mock private AuditEventPort auditEventPort;

  @InjectMocks private AnalyzeTransactionUseCaseImpl useCase;

  private TransactionData sampleTransaction;
  private Map<String, Object> samplePayload;

  @BeforeEach
  void setUp() {
    samplePayload = new HashMap<>();
    samplePayload.put("amount", 1000);
    samplePayload.put("merchant", "Store A");
    samplePayload.put("userId", "user123");
    samplePayload.put("customerId", "cust-001");

    sampleTransaction =
        TransactionData.builder()
            .transactionId("txn-001")
            .customerId("cust-001")
            .amount(BigDecimal.valueOf(1000))
            .timestamp(Instant.now())
            .data(samplePayload)
            .build();

        // Default mock behavior: cache miss, no existing decision/hash
        lenient()
          .when(ruleCachePort.get(eq(RuleCachePort.ACTIVE_RULES_KEY), any()))
          .thenReturn(Optional.empty());
        lenient()
          .when(ruleCachePort.get(eq(RuleCachePort.SHADOW_RULES_KEY), any()))
          .thenReturn(Optional.empty());
        lenient()
          .when(transactionPersistencePort.findPayloadHashByExternalId(anyString()))
          .thenReturn(Optional.empty());
        lenient()
          .when(decisionPersistencePort.findByExternalTransactionId(anyString()))
          .thenReturn(Optional.empty());
  }

  @Nested
  @DisplayName("Happy Path Scenarios")
  class HappyPath {

    @Test
    @DisplayName("Should analyze transaction with no rules and return APPROVED")
    void shouldAnalyzeWithNoRulesAndApprove() {
      // Arrange
      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      AnalysisResult result = useCase.analyze(sampleTransaction);

      // Assert
      assertThat(result).isNotNull();
      assertThat(result.classification()).isEqualTo(Classification.APPROVED);
      assertThat(result.totalScore()).isZero();
      assertThat(result.triggeredRules()).isEmpty();
      assertThat(result.processingTimeMs()).isGreaterThanOrEqualTo(0);

      verify(transactionPersistencePort).save(eq(sampleTransaction), anyString());
      verify(metricsPort).recordEvaluation(anyLong(), eq("APPROVED"), eq(0), eq(0), eq(0));
      verify(auditEventPort)
          .logTransactionAnalyzed(eq("txn-001"), eq("APPROVED"), eq(0), anyLong());
      verify(ruleCachePort).put(eq(RuleCachePort.ACTIVE_RULES_KEY), anyList());
    }

    @Test
    @DisplayName("Should analyze transaction with active rules and calculate score")
    void shouldAnalyzeWithActiveRules() {
      // Arrange
      Rule rule1 =
          createRule("High Amount", Rule.RuleType.CONTEXT, 40, Classification.SUSPICIOUS);
      Rule rule2 =
          createRule("Suspicious Merchant", Rule.RuleType.SECURITY, 25, Classification.SUSPICIOUS);

      when(rulePersistencePort.findAllActiveOrderedByPriority())
          .thenReturn(List.of(rule1, rule2));
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      AnalysisResult result = useCase.analyze(sampleTransaction);

      // Assert
      assertThat(result.classification()).isIn(Classification.SUSPICIOUS, Classification.FRAUD);
      assertThat(result.totalScore()).isGreaterThan(0);

      verify(transactionPersistencePort).save(eq(sampleTransaction), anyString());
      verify(metricsPort).recordEvaluation(anyLong(), anyString(), anyInt(), eq(2), anyInt());
    }

    @Test
    @DisplayName("Should evaluate shadow rules separately and record metrics")
    void shouldEvaluateShadowRules() {
      // Arrange
      Rule activeRule =
          createRule("Active Rule", Rule.RuleType.CONTEXT, 30, Classification.SUSPICIOUS);
      Rule shadowRule = createShadowRule("Shadow Rule", Rule.RuleType.ANOMALY, 50);

      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of(activeRule));
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of(shadowRule));

      // Act
      AnalysisResult result = useCase.analyze(sampleTransaction);

      // Assert
      assertThat(result).isNotNull();

      // Verify shadow rule metrics recorded (may or may not be triggered)
      verify(metricsPort).recordEvaluation(anyLong(), anyString(), anyInt(), eq(1), anyInt());
    }
  }

  @Nested
  @DisplayName("Cache Behavior")
  class CacheBehavior {

    @Test
    @DisplayName("Should use cached active rules when available")
    void shouldUseCachedActiveRules() {
      // Arrange
      Rule cachedRule =
          createRule("Cached Rule", Rule.RuleType.VELOCITY, 20, Classification.APPROVED);

      when(ruleCachePort.get(eq(RuleCachePort.ACTIVE_RULES_KEY), any()))
          .thenReturn(Optional.of(List.of(cachedRule)));
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      useCase.analyze(sampleTransaction);

      // Assert
      verify(metricsPort).recordCacheAccess(true);
      verify(rulePersistencePort, never()).findAllActiveOrderedByPriority();
      verify(ruleCachePort, never()).put(eq(RuleCachePort.ACTIVE_RULES_KEY), anyList());
    }

    @Test
    @DisplayName("Should load from persistence on cache miss and populate cache")
    void shouldLoadFromPersistenceOnCacheMiss() {
      // Arrange
      Rule dbRule = createRule("DB Rule", Rule.RuleType.CONTEXT, 10, Classification.APPROVED);

      when(ruleCachePort.get(eq(RuleCachePort.ACTIVE_RULES_KEY), any()))
          .thenReturn(Optional.empty());
      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of(dbRule));
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      useCase.analyze(sampleTransaction);

      // Assert
      verify(metricsPort).recordCacheAccess(false);
      verify(rulePersistencePort).findAllActiveOrderedByPriority();
      verify(ruleCachePort).put(eq(RuleCachePort.ACTIVE_RULES_KEY), eq(List.of(dbRule)));
      verify(metricsPort).recordCacheSize(1);
    }

    @Test
    @DisplayName("Should use cached shadow rules when available")
    void shouldUseCachedShadowRules() {
      // Arrange
      Rule shadowRule = createShadowRule("Cached Shadow", Rule.RuleType.ANOMALY, 40);

      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(ruleCachePort.get(eq(RuleCachePort.SHADOW_RULES_KEY), any()))
          .thenReturn(Optional.of(List.of(shadowRule)));

      // Act
      AnalysisResult result = useCase.analyze(sampleTransaction);

      // Assert
      assertThat(result).isNotNull();

      verify(rulePersistencePort, never()).findShadowRules();
      verify(ruleCachePort, never()).put(eq(RuleCachePort.SHADOW_RULES_KEY), anyList());
    }
  }

  @Nested
  @DisplayName("Idempotency and Anti-Tamper")
  class IdempotencyAndAntiTamper {

    @Test
    @DisplayName("Should return existing decision for duplicate transaction (idempotency)")
    void shouldReturnExistingDecisionForDuplicate() {
      // Arrange
      String payloadHash = calculatePayloadHash(samplePayload);
      Decision existingDecision =
          Decision.builder()
              .externalTransactionId("txn-001")
              .classification(Classification.SUSPICIOUS)
              .riskScore(45)
              .payloadRawHash(payloadHash)
              .build();

      when(transactionPersistencePort.findPayloadHashByExternalId("txn-001"))
          .thenReturn(Optional.of(payloadHash));
      when(decisionPersistencePort.findByExternalTransactionId("txn-001"))
          .thenReturn(Optional.of(existingDecision));

      // Act
      AnalysisResult result = useCase.analyze(sampleTransaction);

      // Assert
      assertThat(result.classification()).isEqualTo(Classification.SUSPICIOUS);
      assertThat(result.totalScore()).isEqualTo(45);

      // Should not evaluate rules again
      verify(rulePersistencePort, never()).findAllActiveOrderedByPriority();
      verify(transactionPersistencePort, never()).save(any(TransactionData.class), anyString());
    }

    @Test
    @DisplayName("Should detect tampering when payload hash differs")
    void shouldDetectTamperingWhenHashDiffers() {
      // Arrange
      String oldHash = "old-hash-value";

      when(transactionPersistencePort.findPayloadHashByExternalId("txn-001"))
          .thenReturn(Optional.of(oldHash));

      // Act & Assert
      assertThatThrownBy(() -> useCase.analyze(sampleTransaction))
          .isInstanceOf(TamperDetectedException.class)
          .hasMessageContaining("Tentativa de adulteração detectada");

      verify(metricsPort).incrementErrorCount("TAMPER_DETECTED");
      verify(auditEventPort).logTamperDetected(eq("txn-001"), eq(oldHash), anyString());
    }

    @Test
    @DisplayName("Should allow first transaction with no existing hash")
    void shouldAllowFirstTransactionWithNoDecision() {
      // Arrange
      when(transactionPersistencePort.findPayloadHashByExternalId("txn-001"))
          .thenReturn(Optional.empty());
      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      AnalysisResult result = useCase.analyze(sampleTransaction);

      // Assert
      assertThat(result).isNotNull();
      assertThat(result.classification()).isEqualTo(Classification.APPROVED);

      verify(transactionPersistencePort).save(eq(sampleTransaction), anyString());
    }
  }

  @Nested
  @DisplayName("Metrics and Audit")
  class MetricsAndAudit {

    @Test
    @DisplayName("Should record processing metrics with correct values")
    void shouldRecordProcessingMetrics() {
      // Arrange
      Rule rule = createRule("Test Rule", Rule.RuleType.CONTEXT, 35, Classification.SUSPICIOUS);

      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of(rule));
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      useCase.analyze(sampleTransaction);

      // Assert
      verify(metricsPort).recordEvaluation(anyLong(), anyString(), anyInt(), eq(1), anyInt());
    }

    @Test
    @DisplayName("Should log audit event with transaction details")
    void shouldLogAuditEvent() {
      // Arrange
      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      useCase.analyze(sampleTransaction);

      // Assert
      verify(auditEventPort)
          .logTransactionAnalyzed(eq("txn-001"), eq("APPROVED"), eq(0), anyLong());
    }

    @Test
    @DisplayName("Should record shadow rule metrics when triggered")
    void shouldRecordShadowRuleMetrics() {
      // Arrange
      Rule shadowRule = createShadowRule("Shadow Test", Rule.RuleType.ANOMALY, 50);

      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of(shadowRule));

      // Act
      useCase.analyze(sampleTransaction);

      // Assert - may or may not trigger based on conditions
      verify(metricsPort).recordEvaluation(anyLong(), anyString(), anyInt(), eq(0), anyInt());
    }
  }

  @Nested
  @DisplayName("Error Handling")
  class ErrorHandling {

    @Test
    @DisplayName("Should handle rule evaluation errors and record metrics")
    void shouldHandleRuleEvaluationErrors() {
      // Arrange
      when(rulePersistencePort.findAllActiveOrderedByPriority())
          .thenThrow(new RuntimeException("Database error"));

      // Act & Assert
      assertThatThrownBy(() -> useCase.analyze(sampleTransaction))
          .isInstanceOf(RuntimeException.class)
          .hasMessageContaining("Erro ao analisar transação");

      verify(metricsPort).incrementErrorCount("ANALYSIS_ERROR");
    }

    @Test
    @DisplayName("Should propagate TamperDetectedException without wrapping")
    void shouldPropagateTamperDetectedException() {
      // Arrange
      String oldHash = "different-hash";

      when(transactionPersistencePort.findPayloadHashByExternalId("txn-001"))
          .thenReturn(Optional.of(oldHash));

      // Act & Assert
      assertThatThrownBy(() -> useCase.analyze(sampleTransaction))
          .isInstanceOf(TamperDetectedException.class);

      verify(metricsPort).incrementErrorCount("TAMPER_DETECTED");
    }

    @Test
    @DisplayName("Should handle null payload gracefully")
    void shouldHandleNullPayload() {
      // Arrange
      TransactionData nullPayloadTransaction =
          TransactionData.builder()
              .transactionId("txn-002")
              .customerId("cust-002")
              .amount(BigDecimal.ZERO)
              .timestamp(Instant.now())
              .data(null)
              .build();
      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      AnalysisResult result = useCase.analyze(nullPayloadTransaction);

      // Assert
      assertThat(result).isNotNull();
      assertThat(result.classification()).isEqualTo(Classification.APPROVED);
    }
  }

  @Nested
  @DisplayName("Decision Persistence")
  class DecisionPersistence {

    @Test
    @DisplayName("Should not persist decision in this use case")
    void shouldNotPersistDecision() {
      // Arrange
      when(rulePersistencePort.findAllActiveOrderedByPriority()).thenReturn(List.of());
      when(rulePersistencePort.findShadowRules()).thenReturn(List.of());

      // Act
      useCase.analyze(sampleTransaction);

      // Assert
      verify(decisionPersistencePort, never()).save(any(Decision.class), any());
    }
  }

  private String calculatePayloadHash(Map<String, Object> payload) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      String json = String.valueOf(payload);
      byte[] hash = digest.digest(json.getBytes());
      return HexFormat.of().formatHex(hash);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  // Helper methods to create rules
  private Rule createRule(
      String name, Rule.RuleType type, Integer weight, Classification classification) {
    RuleCondition condition = new RuleCondition("amount", "GT", "500");
    return Rule.builder()
        .id(UUID.randomUUID())
        .name(name)
        .type(type)
        .weight(weight)
        .enabled(true)
        .classification(classification)
        .logicOperator(Rule.LogicOperator.AND)
        .conditions(List.of(condition))
        .shadowMode(Rule.ShadowMode.DISABLED)
        .canaryPercentage(0)
        .build();
  }

  private Rule createShadowRule(String name, Rule.RuleType type, Integer weight) {
    RuleCondition condition = new RuleCondition("amount", "GT", "500");
    return Rule.builder()
        .id(UUID.randomUUID())
        .name(name)
        .type(type)
        .weight(weight)
        .enabled(true)
        .classification(Classification.SUSPICIOUS)
        .logicOperator(Rule.LogicOperator.AND)
        .conditions(List.of(condition))
        .shadowMode(Rule.ShadowMode.SHADOW)
        .canaryPercentage(0)
        .build();
  }
}
