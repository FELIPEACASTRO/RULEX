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

@DisplayName("ComplianceOperatorEvaluator Tests")
class ComplianceOperatorEvaluatorTest {

  private ComplianceOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new ComplianceOperatorEvaluator();
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores de compliance")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.ADDRESS_VERIFICATION, ConditionOperator.PEP_LIST_CHECK);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("COMPLIANCE");
  }

  @Nested
  @DisplayName("ADDRESS_VERIFICATION")
  class AddressVerificationTests {

    @Test
    void shouldReturnTrueWhenNotVerified() {
      RuleCondition condition = condition(ConditionOperator.ADDRESS_VERIFICATION, null);
      EvaluationContext context = context(Map.of("addressVerified", false));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenVerified() {
      RuleCondition condition = condition(ConditionOperator.ADDRESS_VERIFICATION, null);
      EvaluationContext context = context(Map.of("addressVerified", true));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("EIDAS_ASSURANCE_LEVEL")
  class EidasAssuranceLevelTests {

    @Test
    void shouldReturnTrueWhenBelowRequiredLevel() {
      RuleCondition condition = condition(ConditionOperator.EIDAS_ASSURANCE_LEVEL, "3");
      EvaluationContext context = context(Map.of("eidasAssuranceLevel", 2));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("PEP_LIST_CHECK")
  class PepListTests {

    @Test
    void shouldReturnTrueWhenPepHit() {
      RuleCondition condition = condition(ConditionOperator.PEP_LIST_CHECK, null);
      EvaluationContext context = context(Map.of("pepListHit", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("OFAC_LIST_CHECK")
  class OfacListTests {

    @Test
    void shouldReturnTrueWhenOfacHit() {
      RuleCondition condition = condition(ConditionOperator.OFAC_LIST_CHECK, null);
      EvaluationContext context = context(Map.of("ofacMatch", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("SANCTIONS_COUNTRY_CHECK")
  class SanctionsCountryTests {

    @Test
    void shouldReturnTrueWhenCountrySanctioned() {
      RuleCondition condition = condition(ConditionOperator.SANCTIONS_COUNTRY_CHECK, null);
      EvaluationContext context = context(Map.of("sanctionsCountryHit", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("ALIAS_DETECTION")
  class AliasDetectionTests {

    @Test
    void shouldReturnTrueWhenAliasDetected() {
      RuleCondition condition = condition(ConditionOperator.ALIAS_DETECTION, null);
      EvaluationContext context = context(Map.of("aliasDetected", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("CASH_INTENSIVE_RATIO")
  class CashIntensiveTests {

    @Test
    void shouldReturnTrueWhenRatioAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.CASH_INTENSIVE_RATIO, "70");
      EvaluationContext context = context(Map.of("cashIntensiveRatio", 90));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("CREDIT_FILE_THIN")
  class CreditFileThinTests {

    @Test
    void shouldReturnTrueWhenThinCreditFile() {
      RuleCondition condition = condition(ConditionOperator.CREDIT_FILE_THIN, null);
      EvaluationContext context = context(Map.of("thinCreditFile", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
