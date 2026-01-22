package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

@DisplayName("DatabaseSyncOperatorEvaluator Tests")
class DatabaseSyncOperatorEvaluatorTest {

  private DatabaseSyncOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new DatabaseSyncOperatorEvaluator();
  }

  private RuleCondition condition(ConditionOperator operator, String value, String fieldName) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    condition.setFieldName(fieldName);
    return condition;
  }

  private EvaluationContext context(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores sincronizados")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.AND,
            ConditionOperator.NOT_IN_LIST,
            ConditionOperator.ROUND_AMOUNT,
            ConditionOperator.NAME_SIMILARITY_GT);
  }

  @Nested
  @DisplayName("AND")
  class AndTests {

    @Test
    void shouldReturnTrueWhenBothTrue() {
      RuleCondition condition = condition(ConditionOperator.AND, "true", "flag");
      EvaluationContext context = context(Map.of("flag", true));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("OR")
  class OrTests {

    @Test
    void shouldReturnTrueWhenAnyTrue() {
      RuleCondition condition = condition(ConditionOperator.OR, "true", "flag");
      EvaluationContext context = context(Map.of("flag", false));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("NOT")
  class NotTests {

    @Test
    void shouldReturnTrueWhenFieldFalse() {
      RuleCondition condition = condition(ConditionOperator.NOT, null, "flag");
      EvaluationContext context = context(Map.of("flag", false));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("NOT_IN_LIST")
  class NotInListTests {

    @Test
    void shouldReturnTrueWhenNotInList() {
      RuleCondition condition = condition(ConditionOperator.NOT_IN_LIST, null, "country");
      condition.setValueArray(List.of("US", "CA"));
      EvaluationContext context = context(Map.of("country", "BR"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("ACCOUNT_AGE_LT_DAYS")
  class AccountAgeTests {

    @Test
    void shouldReturnTrueWhenAgeBelowThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.ACCOUNT_AGE_LT_DAYS, "30", "accountAgeDays");
      EvaluationContext context = context(Map.of("accountAgeDays", 10));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("ROUND_AMOUNT")
  class RoundAmountTests {

    @Test
    void shouldReturnTrueWhenRoundHundreds() {
      RuleCondition condition = condition(ConditionOperator.ROUND_AMOUNT, null, "amount");
      EvaluationContext context = context(Map.of("amount", 200));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("VELOCITY_ANOMALY")
  class VelocityAnomalyTests {

    @Test
    void shouldReturnTrueWhenDeviationAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.VELOCITY_ANOMALY, "2", "velocity");
      EvaluationContext context =
          context(Map.of("velocity", 10, "avgVelocity", 2, "velocityStdDev", 1));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("TRANSFER_AMOUNT_GT")
  class TransferAmountTests {

    @Test
    void shouldReturnTrueWhenAmountAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.TRANSFER_AMOUNT_GT, "100", "amount");
      EvaluationContext context = context(Map.of("amount", 150));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("AMOUNT_ANOMALY")
  class AmountAnomalyTests {

    @Test
    void shouldReturnTrueWhenDeviationAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.AMOUNT_ANOMALY, "2", "amount");
      EvaluationContext context =
          context(Map.of("amount", 100, "avgAmount", 20, "amountStdDev", 10));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("NAME_SIMILARITY_GT")
  class NameSimilarityTests {

    @Test
    void shouldReturnTrueWhenSimilarityAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.NAME_SIMILARITY_GT, "0.8", "similarity");
      EvaluationContext context = context(Map.of("similarity", 0.9));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }
}
