package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("RegulatoryOperatorEvaluator Tests")
class RegulatoryOperatorEvaluatorTest {

  private RegulatoryOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new RegulatoryOperatorEvaluator();
  }

  private RuleCondition createCondition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload, TransactionRequest request) {
    return EvaluationContext.builder().payload(payload).transactionRequest(request).build();
  }

  @Test
  @DisplayName("deve suportar operadores regulatórios")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.SCA_EXEMPTION_TRA, ConditionOperator.GDPR_DATA_RETENTION_CHECK);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("REGULATORY");
  }

  @Nested
  @DisplayName("SCA_EXEMPTION_TRA")
  class ScaTraTests {

    @Test
    void shouldReturnTrueWhenRiskBelowThreshold() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_TRA, "0.5");
      EvaluationContext context = createContext(Map.of("riskScore", 0.3), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenRiskAboveThreshold() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_TRA, "0.5");
      EvaluationContext context = createContext(Map.of("riskScore", 0.8), null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("SCA_EXEMPTION_LOW_VALUE")
  class LowValueTests {

    @Test
    void shouldReturnTrueWhenAmountBelowLimit() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_LOW_VALUE, null);
      TransactionRequest request = new TransactionRequest();
      request.setTransactionAmount(new BigDecimal("10"));
      EvaluationContext context = createContext(new HashMap<>(), request);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenAmountAboveLimit() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_LOW_VALUE, null);
      TransactionRequest request = new TransactionRequest();
      request.setTransactionAmount(new BigDecimal("40"));
      EvaluationContext context = createContext(new HashMap<>(), request);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    void shouldReturnFalseWhenRequestMissing() {
      RuleCondition condition = createCondition(ConditionOperator.SCA_EXEMPTION_LOW_VALUE, null);
      EvaluationContext context = createContext(new HashMap<>(), null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("PSD3_COP_NAME_MATCH")
  class CopNameMatchTests {

    @Test
    void shouldReturnTrueWhenScoreHigh() {
      RuleCondition condition = createCondition(ConditionOperator.PSD3_COP_NAME_MATCH, null);
      EvaluationContext context = createContext(Map.of("copMatchScore", 95), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnTrueWhenNamesMatch() {
      RuleCondition condition = createCondition(ConditionOperator.PSD3_COP_NAME_MATCH, null);
      EvaluationContext context = createContext(Map.of("payeeName", "Alice", "accountHolderName", "alice"), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenScoreLow() {
      RuleCondition condition = createCondition(ConditionOperator.PSD3_COP_NAME_MATCH, null);
      EvaluationContext context = createContext(Map.of("copMatchScore", 40), null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("GDPR_DATA_RETENTION_CHECK")
  class GdprRetentionTests {

    @Test
    void shouldReturnTrueWhenWithinRetention() {
      RuleCondition condition = createCondition(ConditionOperator.GDPR_DATA_RETENTION_CHECK, "30");
      EvaluationContext context = createContext(Map.of("dataAgeDays", 20, "retentionPeriodDays", 30), null);

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenExceededRetention() {
      RuleCondition condition = createCondition(ConditionOperator.GDPR_DATA_RETENTION_CHECK, "30");
      EvaluationContext context = createContext(Map.of("dataAgeDays", 40, "retentionPeriodDays", 30), null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }
}
