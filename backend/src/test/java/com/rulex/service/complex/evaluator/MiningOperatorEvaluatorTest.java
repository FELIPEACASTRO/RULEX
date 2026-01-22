package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@DisplayName("MiningOperatorEvaluator Tests")
class MiningOperatorEvaluatorTest {

  private MiningOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new MiningOperatorEvaluator();
  }

  private RuleCondition createCondition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores de mining/fuzzy")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.APRIORI_ASSOCIATION, ConditionOperator.FUZZY_MEMBERSHIP);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("MINING_FUZZY");
  }

  @Nested
  @DisplayName("Operadores numéricos")
  class NumericOperators {

    @ParameterizedTest
    @CsvSource({
      "APRIORI_ASSOCIATION,0.2,0.1,true",
      "APRIORI_ASSOCIATION,0.05,0.1,false",
      "FPGROWTH_FREQUENT_PATTERNS,6,5,true",
      "FPGROWTH_FREQUENT_PATTERNS,2,5,false",
      "ECLAT_ITEMSET,4,3,true",
      "ECLAT_ITEMSET,1,3,false",
      "FUZZY_MEMBERSHIP,0.7,0.5,true",
      "FUZZY_MEMBERSHIP,0.3,0.5,false"
    })
    void shouldEvaluateNumericOperators(
        ConditionOperator operator, String value, String threshold, boolean expected) {
      RuleCondition condition = createCondition(operator, threshold);
      EvaluationContext context = createContext(Map.of("field", value));

      assertThat(evaluator.evaluate(condition, context)).isEqualTo(expected);
    }

    @Test
    @DisplayName("FUZZY_ADAPTIVE_THRESHOLD deve ajustar threshold com riskLevel HIGH")
    void shouldAdjustAdaptiveThresholdForHighRisk() {
      RuleCondition condition = createCondition(ConditionOperator.FUZZY_ADAPTIVE_THRESHOLD, "0.7");
      EvaluationContext context = createContext(Map.of("field", 0.6, "riskLevel", "HIGH"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    @DisplayName("FUZZY_ADAPTIVE_THRESHOLD deve retornar false em risco baixo")
    void shouldAdjustAdaptiveThresholdForLowRisk() {
      RuleCondition condition = createCondition(ConditionOperator.FUZZY_ADAPTIVE_THRESHOLD, "0.7");
      EvaluationContext context = createContext(Map.of("field", 0.6, "riskLevel", "LOW"));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("Indicador Pig Butchering")
  class PigButcheringTests {

    @Test
    void shouldReturnTrueWhenIndicatorTrue() {
      RuleCondition condition = createCondition(ConditionOperator.PIG_BUTCHERING_INDICATOR, null);
      EvaluationContext context = createContext(Map.of("field", "PIG_BUTCHERING"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnTrueWhenRomanceAndInvestmentSignals() {
      RuleCondition condition = createCondition(ConditionOperator.PIG_BUTCHERING_INDICATOR, null);
      EvaluationContext context =
          createContext(
              Map.of(
                  "romanceScamIndicator",
                  true,
                  "investmentScamIndicator",
                  true,
                  "cryptoTransaction",
                  true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("Casos de borda")
  class EdgeCases {

    @Test
    void shouldHandleNullPayload() {
      RuleCondition condition = createCondition(ConditionOperator.APRIORI_ASSOCIATION, "0.1");
      EvaluationContext context = createContext(null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    void shouldHandleMissingField() {
      RuleCondition condition = createCondition(ConditionOperator.FPGROWTH_FREQUENT_PATTERNS, "5");
      EvaluationContext context = createContext(new HashMap<>());

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }
}
