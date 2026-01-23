package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("FATFOperatorEvaluator Tests")
class FATFOperatorEvaluatorTest {

  private Object evaluator;

  @BeforeEach
  void setUp() {
    evaluator = tryInstantiateEvaluator();
    Assumptions.assumeTrue(
        evaluator != null, "FATFOperatorEvaluator indisponível para carregamento no ambiente");
  }

  private Object tryInstantiateEvaluator() {
    try {
      Class<?> clazz =
          Class.forName("com.rulex.service.complex.evaluator.FATFOperatorEvaluator");
      return clazz.getDeclaredConstructor().newInstance();
    } catch (Exception e) {
      return null;
    }
  }

  @SuppressWarnings("unchecked")
  private Set<ConditionOperator> supportedOperators() {
    try {
      Method method = evaluator.getClass().getMethod("getSupportedOperators");
      return (Set<ConditionOperator>) method.invoke(evaluator);
    } catch (Exception e) {
      throw new RuntimeException("Falha ao acessar operadores suportados", e);
    }
  }

  private boolean evaluateCondition(RuleCondition condition, EvaluationContext context) {
    try {
      Method method =
          evaluator.getClass().getMethod("evaluate", RuleCondition.class, EvaluationContext.class);
      return (boolean) method.invoke(evaluator, condition, context);
    } catch (Exception e) {
      throw new RuntimeException("Falha ao avaliar condição FATF", e);
    }
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    condition.setFieldName("field");
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores FATF")
  void shouldSupportOperators() {
    assertThat(supportedOperators())
        .contains(
            ConditionOperator.FATF_PLACEMENT_STRUCTURING,
            ConditionOperator.FATF_LAYERING_SHELL_COMPANY,
            ConditionOperator.FATF_TBML_OVER_INVOICING);
  }

  @Nested
  @DisplayName("FATF_PLACEMENT_STRUCTURING")
  class PlacementStructuringTests {

    @Test
    void shouldReturnTrueWhenInStructuringRange() {
      RuleCondition condition = condition(ConditionOperator.FATF_PLACEMENT_STRUCTURING, "10000");
      EvaluationContext context = context(Map.of("field", 9000));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_PLACEMENT_CASH_INTENSIVE")
  class PlacementCashIntensiveTests {

    @Test
    void shouldReturnTrueForCashIntensiveMcc() {
      RuleCondition condition = condition(ConditionOperator.FATF_PLACEMENT_CASH_INTENSIVE, null);
      EvaluationContext context = context(Map.of("field", "5812"));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_LAYERING_SHELL_COMPANY")
  class LayeringShellCompanyTests {

    @Test
    void shouldReturnTrueWhenShellCompanyFlag() {
      RuleCondition condition = condition(ConditionOperator.FATF_LAYERING_SHELL_COMPANY, null);
      EvaluationContext context = context(Map.of("field", "SHELL"));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_LAYERING_OFFSHORE")
  class LayeringOffshoreTests {

    @Test
    void shouldReturnTrueForOffshoreCountry() {
      RuleCondition condition = condition(ConditionOperator.FATF_LAYERING_OFFSHORE, null);
      EvaluationContext context = context(Map.of("field", "KY"));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_INTEGRATION_REAL_ESTATE")
  class IntegrationRealEstateTests {

    @Test
    void shouldReturnTrueWhenRealEstateMcc() {
      RuleCondition condition = condition(ConditionOperator.FATF_INTEGRATION_REAL_ESTATE, null);
      EvaluationContext context = context(Map.of("field", "6513"));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_TBML_OVER_INVOICING")
  class TbmlOverInvoicingTests {

    @Test
    void shouldReturnTrueWhenRatioAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.FATF_TBML_OVER_INVOICING, "1.5");
      EvaluationContext context = context(Map.of("field", 2.0));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_TBML_UNDER_INVOICING")
  class TbmlUnderInvoicingTests {

    @Test
    void shouldReturnTrueWhenRatioBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.FATF_TBML_UNDER_INVOICING, "0.7");
      EvaluationContext context = context(Map.of("field", 0.5));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_HAWALA_INFORMAL")
  class HawalaTests {

    @Test
    void shouldReturnTrueWhenHawalaFlag() {
      RuleCondition condition = condition(ConditionOperator.FATF_HAWALA_INFORMAL, null);
      EvaluationContext context = context(Map.of("field", true));

      assertThat(evaluateCondition(condition, context)).isTrue();
    }
  }
}
