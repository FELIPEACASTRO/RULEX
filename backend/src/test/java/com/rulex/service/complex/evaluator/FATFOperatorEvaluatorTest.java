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

@DisplayName("FATFOperatorEvaluator Tests")
class FATFOperatorEvaluatorTest {

  private FATFOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new FATFOperatorEvaluator();
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
    assertThat(evaluator.getSupportedOperators())
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

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_PLACEMENT_CASH_INTENSIVE")
  class PlacementCashIntensiveTests {

    @Test
    void shouldReturnTrueForCashIntensiveMcc() {
      RuleCondition condition = condition(ConditionOperator.FATF_PLACEMENT_CASH_INTENSIVE, null);
      EvaluationContext context = context(Map.of("field", "5812"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_LAYERING_SHELL_COMPANY")
  class LayeringShellCompanyTests {

    @Test
    void shouldReturnTrueWhenShellCompanyFlag() {
      RuleCondition condition = condition(ConditionOperator.FATF_LAYERING_SHELL_COMPANY, null);
      EvaluationContext context = context(Map.of("field", "SHELL"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_LAYERING_OFFSHORE")
  class LayeringOffshoreTests {

    @Test
    void shouldReturnTrueForOffshoreCountry() {
      RuleCondition condition = condition(ConditionOperator.FATF_LAYERING_OFFSHORE, null);
      EvaluationContext context = context(Map.of("field", "KY"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_INTEGRATION_REAL_ESTATE")
  class IntegrationRealEstateTests {

    @Test
    void shouldReturnTrueWhenRealEstateMcc() {
      RuleCondition condition = condition(ConditionOperator.FATF_INTEGRATION_REAL_ESTATE, null);
      EvaluationContext context = context(Map.of("field", "6513"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_TBML_OVER_INVOICING")
  class TbmlOverInvoicingTests {

    @Test
    void shouldReturnTrueWhenRatioAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.FATF_TBML_OVER_INVOICING, "1.5");
      EvaluationContext context = context(Map.of("field", 2.0));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_TBML_UNDER_INVOICING")
  class TbmlUnderInvoicingTests {

    @Test
    void shouldReturnTrueWhenRatioBelowThreshold() {
      RuleCondition condition = condition(ConditionOperator.FATF_TBML_UNDER_INVOICING, "0.7");
      EvaluationContext context = context(Map.of("field", 0.5));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("FATF_HAWALA_INFORMAL")
  class HawalaTests {

    @Test
    void shouldReturnTrueWhenHawalaFlag() {
      RuleCondition condition = condition(ConditionOperator.FATF_HAWALA_INFORMAL, null);
      EvaluationContext context = context(Map.of("field", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
