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

/**
 * Testes unitários para StatisticalOperatorEvaluator.
 */
@DisplayName("StatisticalOperatorEvaluator Tests")
class StatisticalOperatorEvaluatorTest {

  private StatisticalOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new StatisticalOperatorEvaluator();
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
  @DisplayName("deve suportar operadores estatísticos")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.Z_SCORE_GT,
            ConditionOperator.STAT_ANOVA_F_TEST,
            ConditionOperator.BENFORD_LAW_DEVIATION);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("STATISTICAL");
  }

  @Nested
  @DisplayName("Operadores baseados em p-value")
  class PValueOperators {

    @ParameterizedTest(name = "{0} com p-value={1} e alpha={2} deve ser {3}")
    @CsvSource({
      "STAT_ANOVA_F_TEST,0.01,0.05,true",
      "STAT_ANOVA_F_TEST,0.10,0.05,false",
      "STAT_WELCH_T_TEST,0.03,0.05,true",
      "STAT_WELCH_T_TEST,0.20,0.05,false",
      "STAT_SHAPIRO_WILK_TEST,0.01,0.05,true",
      "STAT_SHAPIRO_WILK_TEST,0.20,0.05,false",
      "STAT_LEVENE_TEST,0.02,0.05,true",
      "STAT_LEVENE_TEST,0.20,0.05,false",
      "STAT_KRUSKAL_WALLIS_TEST,0.03,0.05,true",
      "STAT_KRUSKAL_WALLIS_TEST,0.20,0.05,false"
    })
    void shouldEvaluatePValueOperators(
        ConditionOperator operator,
        String pValue,
        String alpha,
        boolean expected) {
      RuleCondition condition = createCondition(operator, alpha);
      EvaluationContext context = createContext(Map.of("fTestPValue", pValue, "welchPValue", pValue, "swPValue", pValue, "leveneTestPValue", pValue, "kwPValue", pValue));

      assertThat(evaluator.evaluate(condition, context)).isEqualTo(expected);
    }
  }

  @Nested
  @DisplayName("Operadores de score/limiar")
  class ScoreOperators {

    @Test
    @DisplayName("Z_SCORE_GT deve retornar true acima do threshold")
    void shouldEvaluateZScore() {
      RuleCondition condition = createCondition(ConditionOperator.Z_SCORE_GT, "2.0");
      EvaluationContext context = createContext(Map.of("zScore", 2.5));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    @DisplayName("BENFORD_LAW_DEVIATION deve retornar false abaixo do threshold")
    void shouldEvaluateBenfordDeviation() {
      RuleCondition condition = createCondition(ConditionOperator.BENFORD_LAW_DEVIATION, "0.2");
      EvaluationContext context = createContext(Map.of("benfordDeviation", 0.1));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    @DisplayName("STANDARD_DEVIATION_GT deve retornar true acima do threshold")
    void shouldEvaluateStandardDeviation() {
      RuleCondition condition = createCondition(ConditionOperator.STANDARD_DEVIATION_GT, "2");
      EvaluationContext context = createContext(Map.of("standardDeviation", 3));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    @DisplayName("KURTOSIS_GT deve retornar true acima do threshold")
    void shouldEvaluateKurtosis() {
      RuleCondition condition = createCondition(ConditionOperator.KURTOSIS_GT, "3");
      EvaluationContext context = createContext(Map.of("kurtosis", 4));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    @DisplayName("SKEWNESS_GT deve retornar false abaixo do threshold")
    void shouldEvaluateSkewness() {
      RuleCondition condition = createCondition(ConditionOperator.SKEWNESS_GT, "2");
      EvaluationContext context = createContext(Map.of("skewness", 1));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("Casos de borda")
  class EdgeCases {

    @Test
    @DisplayName("deve retornar false quando payload é nulo")
    void shouldHandleNullPayload() {
      RuleCondition condition = createCondition(ConditionOperator.Z_SCORE_GT, "2.0");
      EvaluationContext context = createContext(null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    @DisplayName("deve retornar false quando chave não existe")
    void shouldHandleMissingKey() {
      RuleCondition condition = createCondition(ConditionOperator.Z_SCORE_GT, "2.0");
      EvaluationContext context = createContext(new HashMap<>());

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }
}
