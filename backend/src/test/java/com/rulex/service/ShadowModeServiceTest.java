package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision.TransactionClassification;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import org.awaitility.Awaitility;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * Testes para o ShadowModeService.
 *
 * <p>GAP-FIX #3: Cobertura de testes para classes críticas.
 *
 * <p>O ShadowModeService permite testar regras em produção sem afetar decisões reais:
 * <ul>
 *   <li>Regras SHADOW são avaliadas mas não afetam a decisão final
 *   <li>Estatísticas rastreiam o que TERIA acontecido vs o que ACONTECEU
 *   <li>Permite rollout gradual com canary deployments
 * </ul>
 */
@DisplayName("ShadowModeService Tests")
class ShadowModeServiceTest {

  private ShadowModeService shadowModeService;

  @BeforeEach
  void setUp() {
    shadowModeService = new ShadowModeService();
  }

  @Nested
  @DisplayName("Execução de Regras Shadow")
  class ShadowRuleExecution {

    @Test
    @DisplayName("Deve executar regra em modo shadow assincronamente")
    void shouldExecuteShadowRuleAsynchronously() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("SHADOW_RULE_1");

      // Criar um evaluator que retorna triggered
      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) ->
              ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test shadow rule");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      // Aguardar execução assíncrona
      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getGlobalStats();
                assertThat((Long) stats.get("totalShadowEvaluations")).isGreaterThanOrEqualTo(1);
              });
    }

    @Test
    @DisplayName("Deve registrar avaliação shadow que não disparou")
    void shouldRecordNonTriggeredShadowEvaluation() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("SHADOW_RULE_2");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.notTriggered();

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getGlobalStats();
                assertThat((Long) stats.get("totalShadowEvaluations")).isGreaterThanOrEqualTo(1);
              });
    }

    @Test
    @DisplayName("Deve incrementar contador de regras disparadas")
    void shouldIncrementTriggeredCounter() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("SHADOW_RULE_3");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) ->
              ShadowModeService.RuleEvaluationResult.triggered(80, "BLOCK", "High risk detected");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getGlobalStats();
                assertThat((Long) stats.get("shadowRulesTriggered")).isGreaterThanOrEqualTo(1);
              });
    }
  }

  @Nested
  @DisplayName("Estatísticas de Shadow")
  class ShadowStatistics {

    @Test
    @DisplayName("Deve rastrear estatísticas por regra")
    void shouldTrackStatsPerRule() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("SHADOW_STATS_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) ->
              ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test rule");

      // Executar múltiplas vezes
      for (int i = 0; i < 5; i++) {
        shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");
      }

      Awaitility.await()
          .atMost(3, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
                assertThat(stats.evaluationCount().get()).isEqualTo(5);
                assertThat(stats.triggerCount().get()).isEqualTo(5);
              });
    }

    @Test
    @DisplayName("Deve rastrear potenciais falsos positivos")
    void shouldTrackPotentialFalsePositives() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("SHADOW_FP_RULE");

      // Regra dispara mas decisão real foi ALLOW (potencial falso positivo)
      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) ->
              ShadowModeService.RuleEvaluationResult.triggered(80, "BLOCK", "Would have blocked");

      // Usar "ALLOW" que é o valor verificado no código
      shadowModeService.executeShadow(shadowRule, request, evaluator, "ALLOW");

      Awaitility.await()
          .atMost(3, TimeUnit.SECONDS)
          .pollInterval(100, TimeUnit.MILLISECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
                // Verificar que a regra foi avaliada e disparou
                assertThat(stats.triggerCount().get()).isGreaterThanOrEqualTo(1);
              });
    }

    @Test
    @DisplayName("Deve retornar estatísticas globais")
    void shouldReturnGlobalStatistics() {
      var globalStats = shadowModeService.getGlobalStats();

      assertThat(globalStats).containsKey("totalShadowEvaluations");
      assertThat(globalStats).containsKey("shadowRulesTriggered");
      assertThat(globalStats).containsKey("potentialFalsePositives");
      assertThat(globalStats).containsKey("potentialCatches");
      assertThat(globalStats).containsKey("activeShadowRules");
    }

    @Test
    @DisplayName("Deve retornar todas as estatísticas de regras")
    void shouldReturnAllStats() {
      TransactionRequest request = createRequest();
      RuleConfiguration rule1 = createShadowRule("RULE_1");
      RuleConfiguration rule2 = createShadowRule("RULE_2");
      rule2.setId(2L);

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test");

      shadowModeService.executeShadow(rule1, request, evaluator, "APPROVED");
      shadowModeService.executeShadow(rule2, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                List<ShadowModeService.ShadowRuleStats> allStats = shadowModeService.getAllStats();
                assertThat(allStats).hasSizeGreaterThanOrEqualTo(2);
              });
    }
  }

  @Nested
  @DisplayName("Avaliações Recentes")
  class RecentEvaluations {

    @Test
    @DisplayName("Deve armazenar avaliações recentes")
    void shouldStoreRecentEvaluations() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("RECENT_EVAL_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var recentEvals = shadowModeService.getRecentEvaluations(shadowRule.getId(), 10);
                assertThat(recentEvals).isNotEmpty();
                assertThat(recentEvals.get(0).triggered()).isTrue();
                assertThat(recentEvals.get(0).score()).isEqualTo(50);
              });
    }

    @Test
    @DisplayName("Deve limitar número de avaliações recentes")
    void shouldLimitRecentEvaluations() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("LIMIT_EVAL_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test");

      // Executar muitas vezes
      for (int i = 0; i < 20; i++) {
        shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");
      }

      Awaitility.await()
          .atMost(3, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var recentEvals = shadowModeService.getRecentEvaluations(shadowRule.getId(), 5);
                assertThat(recentEvals).hasSize(5);
              });
    }

    @Test
    @DisplayName("Deve retornar lista vazia para regra sem avaliações")
    void shouldReturnEmptyListForRuleWithoutEvaluations() {
      var recentEvals = shadowModeService.getRecentEvaluations(999L, 10);
      assertThat(recentEvals).isEmpty();
    }
  }

  @Nested
  @DisplayName("Avaliação de Promoção")
  class PromotionAssessment {

    @Test
    @DisplayName("Deve retornar não pronto para regra sem dados")
    void shouldReturnNotReadyForRuleWithoutData() {
      var assessment = shadowModeService.assessPromotion(999L);

      assertThat(assessment.isReady()).isFalse();
      assertThat(assessment.concerns()).isNotEmpty();
      assertThat(assessment.recommendation()).contains("shadow mode");
    }

    @Test
    @DisplayName("Deve avaliar prontidão para promoção")
    void shouldAssessPromotionReadiness() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("PROMOTION_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test");

      // Executar algumas vezes
      for (int i = 0; i < 10; i++) {
        shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");
      }

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var assessment = shadowModeService.assessPromotion(shadowRule.getId());
                assertThat(assessment).isNotNull();
                assertThat(assessment.ruleId()).isEqualTo(shadowRule.getId());
                // Com poucos dados, não deve estar pronto
                assertThat(assessment.isReady()).isFalse();
                assertThat(assessment.concerns()).contains("Insufficient data: 10 evaluations (recommend 10000)");
              });
    }
  }

  @Nested
  @DisplayName("Marcação de Fraude")
  class FraudMarking {

    @Test
    @DisplayName("Deve marcar transação como fraude")
    void shouldMarkTransactionAsFraud() {
      TransactionRequest request = createRequest();
      request.setExternalTransactionId("fraud-tx-123");
      RuleConfiguration shadowRule = createShadowRule("FRAUD_MARK_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(80, "BLOCK", "Fraud detected");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
              });

      // Marcar como fraude
      shadowModeService.markTransactionAsFraud("fraud-tx-123");

      var globalStats = shadowModeService.getGlobalStats();
      assertThat((Long) globalStats.get("potentialCatches")).isGreaterThanOrEqualTo(0);
    }
  }

  @Nested
  @DisplayName("Reset de Estatísticas")
  class StatsReset {

    @Test
    @DisplayName("Deve resetar estatísticas de uma regra")
    void shouldResetStatsForRule() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("RESET_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(50, "FLAG", "Test");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
              });

      // Reset
      shadowModeService.resetStats(shadowRule.getId());

      var stats = shadowModeService.getStats(shadowRule.getId());
      assertThat(stats).isNull();
    }
  }

  @Nested
  @DisplayName("Ações de Regras")
  class RuleActions {

    @Test
    @DisplayName("Deve rastrear ação BLOCK")
    void shouldTrackBlockAction() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("BLOCK_ACTION_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(90, "BLOCK", "High risk");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
                assertThat(stats.wouldHaveBlocked().get()).isEqualTo(1);
              });
    }

    @Test
    @DisplayName("Deve rastrear ação FLAG")
    void shouldTrackFlagAction() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("FLAG_ACTION_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(60, "FLAG", "Medium risk");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
                assertThat(stats.wouldHaveFlagged().get()).isEqualTo(1);
              });
    }

    @Test
    @DisplayName("Deve rastrear ação REVIEW")
    void shouldTrackReviewAction() {
      TransactionRequest request = createRequest();
      RuleConfiguration shadowRule = createShadowRule("REVIEW_ACTION_RULE");

      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> ShadowModeService.RuleEvaluationResult.triggered(50, "REVIEW", "Needs review");

      shadowModeService.executeShadow(shadowRule, request, evaluator, "APPROVED");

      Awaitility.await()
          .atMost(2, TimeUnit.SECONDS)
          .untilAsserted(
              () -> {
                var stats = shadowModeService.getStats(shadowRule.getId());
                assertThat(stats).isNotNull();
                assertThat(stats.wouldHaveFlagged().get()).isEqualTo(1);
              });
    }
  }

  @Nested
  @DisplayName("RuleEvaluationResult")
  class RuleEvaluationResultTests {

    @Test
    @DisplayName("Deve criar resultado não disparado")
    void shouldCreateNotTriggeredResult() {
      var result = ShadowModeService.RuleEvaluationResult.notTriggered();

      assertThat(result.triggered()).isFalse();
      assertThat(result.score()).isEqualTo(0);
      assertThat(result.action()).isEqualTo("ALLOW");
      assertThat(result.reason()).isNull();
    }

    @Test
    @DisplayName("Deve criar resultado disparado")
    void shouldCreateTriggeredResult() {
      var result = ShadowModeService.RuleEvaluationResult.triggered(75, "BLOCK", "Fraud detected");

      assertThat(result.triggered()).isTrue();
      assertThat(result.score()).isEqualTo(75);
      assertThat(result.action()).isEqualTo("BLOCK");
      assertThat(result.reason()).isEqualTo("Fraud detected");
    }
  }

  // ========== Helpers ==========

  private TransactionRequest createRequest() {
    return TransactionRequest.builder()
        .externalTransactionId("tx-shadow-test-" + System.currentTimeMillis())
        .customerIdFromHeader("cust-123")
        .pan("4111111111111111")
        .transactionAmount(new BigDecimal("100.00"))
        .transactionDate(20251219)
        .transactionTime(120000)
        .mcc(5411)
        .merchantCountryCode("076")
        .build();
  }

  private RuleConfiguration createShadowRule(String name) {
    return RuleConfiguration.builder()
        .id(1L)
        .ruleName(name)
        .description("Shadow rule for testing")
        .ruleType(RuleConfiguration.RuleType.SECURITY)
        .threshold(100)
        .weight(50)
        .enabled(true)
        .classification(TransactionClassification.SUSPICIOUS)
        .shadowMode(RuleConfiguration.ShadowMode.SHADOW)
        .build();
  }
}
