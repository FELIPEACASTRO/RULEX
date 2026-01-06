package com.rulex.domain.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.rulex.domain.model.Classification;
import com.rulex.domain.model.Decision;
import com.rulex.domain.model.Decision.TriggeredRule;
import com.rulex.domain.model.Rule;
import com.rulex.domain.model.RuleCondition;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("RuleEvaluatorService - Domain Service")
class RuleEvaluatorServiceTest {

  private ConditionEvaluator mockConditionEvaluator;
  private RuleEvaluatorService service;

  @BeforeEach
  void setUp() {
    mockConditionEvaluator = mock(ConditionEvaluator.class);
    service = new RuleEvaluatorService(mockConditionEvaluator);
  }

  @Nested
  @DisplayName("evaluate() - Full Decision Pipeline")
  class EvaluateDecision {

    @Test
    @DisplayName("Should return APPROVED when no rules triggered")
    void testEvaluate_NoRulesTriggered() {
      Rule rule = createRule("RULE_1", Rule.RuleType.SECURITY, 10, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(rule);
      Map<String, Object> payload = Map.of("amount", 50);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(false, "amount GT 100 → FALSE"));

      Decision decision = service.evaluate(rules, payload, "TXN-001");

      assertThat(decision.getClassification()).isEqualTo(Classification.APPROVED);
      assertThat(decision.getRiskScore()).isZero();
      assertThat(decision.getTriggeredRules()).isEmpty();
      assertThat(decision.getReason()).contains("Nenhuma regra crítica");
      assertThat(decision.getExternalTransactionId()).isEqualTo("TXN-001");
      assertThat(decision.getProcessingTimeMs()).isGreaterThanOrEqualTo(0);
    }

    @Test
    @DisplayName("Should return SUSPICIOUS when suspicious rule triggered")
    void testEvaluate_SuspiciousTriggered() {
      Rule rule =
          createRule("SUSPICIOUS_AMOUNT", Rule.RuleType.CONTEXT, 30, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(rule);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 1000 → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-002");

      assertThat(decision.getClassification()).isEqualTo(Classification.SUSPICIOUS);
      assertThat(decision.getRiskScore()).isEqualTo(30);
      assertThat(decision.getTriggeredRules()).hasSize(1);
      assertThat(decision.getReason()).contains("suspeita").contains("SUSPICIOUS_AMOUNT");
    }

    @Test
    @DisplayName("Should return FRAUD when fraud rule triggered")
    void testEvaluate_FraudTriggered() {
      Rule rule = createRule("HIGH_RISK", Rule.RuleType.ANOMALY, 100, Classification.FRAUD, true);
      List<Rule> rules = List.of(rule);
      Map<String, Object> payload = Map.of("country", "BLACKLISTED");

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(
              new ConditionEvaluator.EvaluationResult(true, "country IN blacklist → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-003");

      assertThat(decision.getClassification()).isEqualTo(Classification.FRAUD);
      assertThat(decision.getRiskScore()).isEqualTo(100);
      assertThat(decision.getTriggeredRules()).hasSize(1);
      assertThat(decision.getReason()).contains("fraude").contains("HIGH_RISK");
    }

    @Test
    @DisplayName("Should accumulate scores from multiple triggered rules")
    void testEvaluate_MultipleRulesAccumulateScore() {
      Rule rule1 = createRule("RULE_1", Rule.RuleType.SECURITY, 20, Classification.SUSPICIOUS, true);
      Rule rule2 = createRule("RULE_2", Rule.RuleType.VELOCITY, 30, Classification.SUSPICIOUS, true);
      Rule rule3 = createRule("RULE_3", Rule.RuleType.CONTEXT, 15, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(rule1, rule2, rule3);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-004");

      assertThat(decision.getRiskScore()).isEqualTo(65); // 20 + 30 + 15
      assertThat(decision.getTriggeredRules()).hasSize(3);
    }

    @Test
    @DisplayName("Should cap total score at 100")
    void testEvaluate_ScoreCappedAt100() {
      Rule rule1 = createRule("RULE_1", Rule.RuleType.SECURITY, 60, Classification.SUSPICIOUS, true);
      Rule rule2 = createRule("RULE_2", Rule.RuleType.VELOCITY, 70, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(rule1, rule2);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-005");

      assertThat(decision.getRiskScore()).isEqualTo(100); // Capped at 100 (not 130)
    }

    @Test
    @DisplayName("Should use most severe classification when multiple rules trigger")
    void testEvaluate_MostSevereClassification() {
      Rule rule1 =
          createRule("SUSPICIOUS_1", Rule.RuleType.CONTEXT, 20, Classification.SUSPICIOUS, true);
      Rule rule2 =
          createRule("SUSPICIOUS_2", Rule.RuleType.VELOCITY, 30, Classification.SUSPICIOUS, true);
      Rule rule3 = createRule("FRAUD", Rule.RuleType.ANOMALY, 100, Classification.FRAUD, true);
      List<Rule> rules = List.of(rule1, rule2, rule3);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-006");

      assertThat(decision.getClassification()).isEqualTo(Classification.FRAUD);
    }

    @Test
    @DisplayName("Should short-circuit on FRAUD classification")
    void testEvaluate_ShortCircuitOnFraud() {
      Rule rule1 = createRule("FRAUD_RULE", Rule.RuleType.ANOMALY, 100, Classification.FRAUD, true);
      Rule rule2 =
          createRule("SUSPICIOUS_RULE", Rule.RuleType.VELOCITY, 50, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(rule1, rule2);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-007");

      assertThat(decision.getClassification()).isEqualTo(Classification.FRAUD);
      assertThat(decision.getTriggeredRules()).hasSize(1); // Only first FRAUD rule
      assertThat(decision.getTriggeredRules().get(0).getRuleName()).isEqualTo("FRAUD_RULE");
    }

    @Test
    @DisplayName("Should skip inactive rules")
    void testEvaluate_SkipInactiveRules() {
      Rule activeRule =
          createRule("ACTIVE", Rule.RuleType.SECURITY, 50, Classification.SUSPICIOUS, true);
      Rule inactiveRule =
          createRule("INACTIVE", Rule.RuleType.ANOMALY, 100, Classification.FRAUD, false);
      List<Rule> rules = List.of(activeRule, inactiveRule);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-008");

      assertThat(decision.getTriggeredRules()).hasSize(1);
      assertThat(decision.getTriggeredRules().get(0).getRuleName()).isEqualTo("ACTIVE");
      assertThat(decision.getClassification()).isEqualTo(Classification.SUSPICIOUS);
    }

    @Test
    @DisplayName("Should return APPROVED when rule list is empty")
    void testEvaluate_EmptyRuleList() {
      Decision decision = service.evaluate(List.of(), Map.of("amount", 100), "TXN-009");

      assertThat(decision.getClassification()).isEqualTo(Classification.APPROVED);
      assertThat(decision.getRiskScore()).isZero();
      assertThat(decision.getTriggeredRules()).isEmpty();
    }

    @Test
    @DisplayName("Should return APPROVED when rule list is null")
    void testEvaluate_NullRuleList() {
      Decision decision = service.evaluate(null, Map.of("amount", 100), "TXN-010");

      assertThat(decision.getClassification()).isEqualTo(Classification.APPROVED);
      assertThat(decision.getRiskScore()).isZero();
      assertThat(decision.getTriggeredRules()).isEmpty();
    }

    @Test
    @DisplayName("Should include rule details in TriggeredRule")
    void testEvaluate_TriggeredRuleDetails() {
      Rule rule =
          createRule("HIGH_AMOUNT", Rule.RuleType.VELOCITY, 45, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(rule);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 1000 → TRUE"));

      Decision decision = service.evaluate(rules, payload, "TXN-011");

      TriggeredRule triggered = decision.getTriggeredRules().get(0);
      assertThat(triggered.getRuleName()).isEqualTo("HIGH_AMOUNT");
      assertThat(triggered.getRuleType()).isEqualTo("VELOCITY");
      assertThat(triggered.getWeight()).isEqualTo(45);
      assertThat(triggered.getContribution()).isEqualTo(45);
      assertThat(triggered.getDetail()).contains("amount GT 1000");
      assertThat(triggered.getClassification()).isEqualTo(Classification.SUSPICIOUS);
    }
  }

  @Nested
  @DisplayName("evaluateRule() - Single Rule Evaluation")
  class EvaluateRule {

    @Test
    @DisplayName("Should evaluate rule with AND logic - all conditions must pass")
    void testEvaluateRule_AndLogic_AllPass() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "100");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      Rule rule =
          createRuleWithConditions(
              "RULE_AND", Rule.LogicOperator.AND, List.of(cond1, cond2), true);

      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond1), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 100 → TRUE"));
      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond2), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "country EQ BRAZIL → TRUE"));

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 200, "country", "BRAZIL"));

      assertThat(result.triggered()).isTrue();
      assertThat(result.explanation()).contains("amount GT 100").contains("country EQ BRAZIL");
    }

    @Test
    @DisplayName("Should evaluate rule with AND logic - one failure means rule fails")
    void testEvaluateRule_AndLogic_OneFailure() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "100");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      Rule rule =
          createRuleWithConditions(
              "RULE_AND", Rule.LogicOperator.AND, List.of(cond1, cond2), true);

      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond1), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 100 → TRUE"));
      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond2), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(false, "country EQ BRAZIL → FALSE"));

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 200, "country", "USA"));

      assertThat(result.triggered()).isFalse();
    }

    @Test
    @DisplayName("Should short-circuit AND logic on first failure")
    void testEvaluateRule_AndLogic_ShortCircuit() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "100");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      RuleCondition cond3 = new RuleCondition("status", "EQ", "ACTIVE");
      Rule rule =
          createRuleWithConditions(
              "RULE_AND", Rule.LogicOperator.AND, List.of(cond1, cond2, cond3), true);

      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond1), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 100 → TRUE"));
      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond2), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(false, "country EQ BRAZIL → FALSE"));

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 200, "country", "USA"));

      assertThat(result.triggered()).isFalse();
      verify(mockConditionEvaluator, times(2)).evaluateWithExplanation(any(), any());
      verify(mockConditionEvaluator, never()).evaluateWithExplanation(eq(cond3), any());
    }

    @Test
    @DisplayName("Should evaluate rule with OR logic - one success is enough")
    void testEvaluateRule_OrLogic_OneSuccess() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "10000");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      Rule rule =
          createRuleWithConditions(
              "RULE_OR", Rule.LogicOperator.OR, List.of(cond1, cond2), true);

      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond1), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(false, "amount GT 10000 → FALSE"));
      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond2), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "country EQ BRAZIL → TRUE"));

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 500, "country", "BRAZIL"));

      assertThat(result.triggered()).isTrue();
    }

    @Test
    @DisplayName("Should short-circuit OR logic on first success")
    void testEvaluateRule_OrLogic_ShortCircuit() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "100");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      RuleCondition cond3 = new RuleCondition("status", "EQ", "ACTIVE");
      Rule rule =
          createRuleWithConditions(
              "RULE_OR", Rule.LogicOperator.OR, List.of(cond1, cond2, cond3), true);

      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond1), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 100 → TRUE"));

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 200));

      assertThat(result.triggered()).isTrue();
      verify(mockConditionEvaluator, times(1)).evaluateWithExplanation(any(), any());
      verify(mockConditionEvaluator, never()).evaluateWithExplanation(eq(cond2), any());
      verify(mockConditionEvaluator, never()).evaluateWithExplanation(eq(cond3), any());
    }

    @Test
    @DisplayName("Should default to AND logic when logic operator is null")
    void testEvaluateRule_NullLogicOperator_DefaultsToAnd() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "100");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      Rule rule = createRuleWithConditions("RULE_NULL_OP", null, List.of(cond1, cond2), true);

      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond1), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 100 → TRUE"));
      when(mockConditionEvaluator.evaluateWithExplanation(eq(cond2), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "country EQ BRAZIL → TRUE"));

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 200, "country", "BRAZIL"));

      assertThat(result.triggered()).isTrue();
    }

    @Test
    @DisplayName("Should return not triggered when rule is null")
    void testEvaluateRule_NullRule() {
      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(null, Map.of("amount", 100));

      assertThat(result.triggered()).isFalse();
      assertThat(result.explanation()).contains("sem condições");
    }

    @Test
    @DisplayName("Should return not triggered when conditions are empty")
    void testEvaluateRule_EmptyConditions() {
      Rule rule = createRuleWithConditions("RULE_EMPTY", Rule.LogicOperator.AND, List.of(), true);

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 100));

      assertThat(result.triggered()).isFalse();
      assertThat(result.explanation()).contains("sem condições");
    }

    @Test
    @DisplayName("Should return not triggered when conditions are null")
    void testEvaluateRule_NullConditions() {
      Rule rule = createRuleWithConditions("RULE_NULL", Rule.LogicOperator.AND, null, true);

      RuleEvaluatorService.RuleEvaluationResult result =
          service.evaluateRule(rule, Map.of("amount", 100));

      assertThat(result.triggered()).isFalse();
      assertThat(result.explanation()).contains("sem condições");
    }
  }

  @Nested
  @DisplayName("evaluateShadow() - Shadow Mode Evaluation")
  class EvaluateShadow {

    @Test
    @DisplayName("Should evaluate shadow rules without affecting production")
    void testEvaluateShadow_ShadowRules() {
      Rule shadowRule =
          createShadowRule("SHADOW_1", Rule.RuleType.SECURITY, 50, Classification.SUSPICIOUS, true);
      List<Rule> shadowRules = List.of(shadowRule);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "amount GT 1000 → TRUE"));

      List<RuleEvaluatorService.ShadowResult> results =
          service.evaluateShadow(shadowRules, payload);

      assertThat(results).hasSize(1);
      RuleEvaluatorService.ShadowResult result = results.get(0);
      assertThat(result.ruleName()).isEqualTo("SHADOW_1");
      assertThat(result.triggered()).isTrue();
      assertThat(result.score()).isEqualTo(50);
      assertThat(result.classification()).isEqualTo(Classification.SUSPICIOUS);
    }

    @Test
    @DisplayName("Should skip non-shadow rules in shadow evaluation")
    void testEvaluateShadow_SkipProductionRules() {
      Rule productionRule =
          createRule("PRODUCTION", Rule.RuleType.SECURITY, 50, Classification.SUSPICIOUS, true);
      Rule shadowRule =
          createShadowRule("SHADOW", Rule.RuleType.CONTEXT, 50, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(productionRule, shadowRule);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      List<RuleEvaluatorService.ShadowResult> results = service.evaluateShadow(rules, payload);

      assertThat(results).hasSize(1);
      assertThat(results.get(0).ruleName()).isEqualTo("SHADOW");
    }

    @Test
    @DisplayName("Should evaluate canary rules as shadow")
    void testEvaluateShadow_CanaryRules() {
      Rule canaryRule =
          createCanaryRule("CANARY_1", Rule.RuleType.VELOCITY, 30, Classification.SUSPICIOUS);
      List<Rule> rules = List.of(canaryRule);
      Map<String, Object> payload = Map.of("amount", 5000);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      List<RuleEvaluatorService.ShadowResult> results = service.evaluateShadow(rules, payload);

      assertThat(results).hasSize(1);
      assertThat(results.get(0).ruleName()).isEqualTo("CANARY_1");
    }

    @Test
    @DisplayName("Should return empty list when no shadow rules present")
    void testEvaluateShadow_NoShadowRules() {
      Rule productionRule =
          createRule("PRODUCTION", Rule.RuleType.SECURITY, 50, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(productionRule);

      List<RuleEvaluatorService.ShadowResult> results =
          service.evaluateShadow(rules, Map.of("amount", 5000));

      assertThat(results).isEmpty();
    }

    @Test
    @DisplayName("Should handle null weight in shadow result")
    void testEvaluateShadow_NullWeight() {
      Rule shadowRule =
          createShadowRule("SHADOW", Rule.RuleType.ANOMALY, null, Classification.SUSPICIOUS, true);
      List<Rule> rules = List.of(shadowRule);

      when(mockConditionEvaluator.evaluateWithExplanation(any(), any()))
          .thenReturn(new ConditionEvaluator.EvaluationResult(true, "condition → TRUE"));

      List<RuleEvaluatorService.ShadowResult> results =
          service.evaluateShadow(rules, Map.of("amount", 5000));

      assertThat(results.get(0).score()).isZero();
    }
  }

  @Nested
  @DisplayName("Integration with Real ConditionEvaluator")
  class RealIntegration {

    private RuleEvaluatorService realService;

    @BeforeEach
    void setUp() {
      realService = new RuleEvaluatorService(); // Uses real ConditionEvaluator
    }

    @Test
    @DisplayName("Should evaluate real rule with real conditions")
    void testRealEvaluation_SimpleRule() {
      RuleCondition condition = new RuleCondition("amount", "GT", "1000");
      Rule rule =
          Rule.builder()
              .name("HIGH_AMOUNT")
              .type(Rule.RuleType.VELOCITY)
              .weight(50)
              .enabled(true)
              .classification(Classification.SUSPICIOUS)
              .logicOperator(Rule.LogicOperator.AND)
              .conditions(List.of(condition))
              .shadowMode(Rule.ShadowMode.DISABLED)
              .canaryPercentage(0)
              .build();

      Map<String, Object> payload = Map.of("amount", new BigDecimal("5000"));

      Decision decision = realService.evaluate(List.of(rule), payload, "TXN-REAL-001");

      assertThat(decision.getClassification()).isEqualTo(Classification.SUSPICIOUS);
      assertThat(decision.getRiskScore()).isEqualTo(50);
      assertThat(decision.getTriggeredRules()).hasSize(1);
    }

    @Test
    @DisplayName("Should evaluate complex rule with multiple conditions")
    void testRealEvaluation_ComplexRule() {
      RuleCondition cond1 = new RuleCondition("amount", "GT", "1000");
      RuleCondition cond2 = new RuleCondition("country", "EQ", "BRAZIL");
      RuleCondition cond3 = new RuleCondition("mcc", "IN", "6010,6011");
      Rule rule =
          Rule.builder()
              .name("COMPLEX_RULE")
              .type(Rule.RuleType.SECURITY)
              .weight(75)
              .enabled(true)
              .classification(Classification.FRAUD)
              .logicOperator(Rule.LogicOperator.AND)
              .conditions(List.of(cond1, cond2, cond3))
              .shadowMode(Rule.ShadowMode.DISABLED)
              .canaryPercentage(0)
              .build();

      Map<String, Object> payload =
          Map.of(
              "amount", new BigDecimal("5000"),
              "country", "BRAZIL",
              "mcc", "6011");

      Decision decision = realService.evaluate(List.of(rule), payload, "TXN-REAL-002");

      assertThat(decision.getClassification()).isEqualTo(Classification.FRAUD);
      assertThat(decision.getRiskScore()).isEqualTo(75);
    }
  }

  // ========== Helper Methods ==========

  private Rule createRule(
      String name, Rule.RuleType type, Integer weight, Classification classification, boolean active) {
    RuleCondition dummyCondition = new RuleCondition("dummy", "EQ", "value");
    return Rule.builder()
        .id(UUID.randomUUID())
        .name(name)
        .type(type)
        .weight(weight)
        .enabled(active)
        .classification(classification)
        .logicOperator(Rule.LogicOperator.AND)
        .conditions(List.of(dummyCondition))
        .shadowMode(Rule.ShadowMode.DISABLED)
        .canaryPercentage(0)
        .build();
  }

  private Rule createRuleWithConditions(
      String name, Rule.LogicOperator operator, List<RuleCondition> conditions, boolean active) {
    return Rule.builder()
        .id(UUID.randomUUID())
        .name(name)
        .type(Rule.RuleType.SECURITY)
        .weight(50)
        .enabled(active)
        .classification(Classification.SUSPICIOUS)
        .logicOperator(operator)
        .conditions(conditions)
        .shadowMode(Rule.ShadowMode.DISABLED)
        .canaryPercentage(0)
        .build();
  }

  private Rule createShadowRule(
      String name, Rule.RuleType type, Integer weight, Classification classification, boolean active) {
    RuleCondition dummyCondition = new RuleCondition("dummy", "EQ", "value");
    return Rule.builder()
        .id(UUID.randomUUID())
        .name(name)
        .type(type)
        .weight(weight)
        .enabled(active)
        .classification(classification)
        .logicOperator(Rule.LogicOperator.AND)
        .conditions(List.of(dummyCondition))
        .shadowMode(Rule.ShadowMode.SHADOW)
        .canaryPercentage(0)
        .build();
  }

  private Rule createCanaryRule(
      String name, Rule.RuleType type, Integer weight, Classification classification) {
    RuleCondition dummyCondition = new RuleCondition("dummy", "EQ", "value");
    return Rule.builder()
        .id(UUID.randomUUID())
        .name(name)
        .type(type)
        .weight(weight)
        .enabled(true)
        .classification(classification)
        .logicOperator(Rule.LogicOperator.AND)
        .conditions(List.of(dummyCondition))
        .shadowMode(Rule.ShadowMode.CANARY)
        .canaryPercentage(10)
        .build();
  }
}
