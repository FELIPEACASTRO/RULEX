package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

@DisplayName("ListOperatorEvaluator Tests")
class ListOperatorEvaluatorTest {

  private ListOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new ListOperatorEvaluator();
  }

  private RuleCondition createCondition(String fieldName, ConditionOperator operator) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(fieldName);
    condition.setOperator(operator);
    return condition;
  }

  private EvaluationContext createContext(Map<String, Object> payload) {
    return EvaluationContext.builder().payload(payload).build();
  }

  @Test
  @DisplayName("deve suportar operadores de lista")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.IN_LIST, ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("LIST");
  }

  @Nested
  @DisplayName("IN_LIST")
  class InListTests {

    @Test
    void shouldReturnTrueWhenValueInList() {
      RuleCondition condition = createCondition("email", ConditionOperator.IN_LIST);
      condition.setValueArray(List.of("a@x.com", "b@x.com"));
      EvaluationContext context = createContext(Map.of("email", "b@x.com"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenValueNotInList() {
      RuleCondition condition = createCondition("email", ConditionOperator.IN_LIST);
      condition.setValueArray(List.of("a@x.com"));
      EvaluationContext context = createContext(Map.of("email", "c@x.com"));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("NOT_IN_HISTORICAL")
  class NotInHistoricalTests {

    @Test
    void shouldReturnTrueWhenHistoryMissing() {
      RuleCondition condition = createCondition("merchantId", ConditionOperator.NOT_IN_HISTORICAL);
      EvaluationContext context = createContext(Map.of());

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseWhenValueInHistory() {
      RuleCondition condition = createCondition("merchantId", ConditionOperator.NOT_IN_HISTORICAL);
      EvaluationContext context =
          createContext(Map.of("merchantId_history", List.of("m1", "m2"), "merchantId", "m2"));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("NOT_IN_CUSTOMER_HISTORY")
  class NotInCustomerHistoryTests {

    @Test
    void shouldReturnTrueWhenCustomerHistoryMissing() {
      RuleCondition condition =
          createCondition("deviceId", ConditionOperator.NOT_IN_CUSTOMER_HISTORY);
      EvaluationContext context = createContext(Map.of());

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("NOT_IN_CUSTOMER_USUAL_HOURS")
  class NotInCustomerUsualHoursTests {

    @Test
    void shouldReturnTrueWhenHourNotUsual() {
      RuleCondition condition =
          createCondition("transactionHour", ConditionOperator.NOT_IN_CUSTOMER_USUAL_HOURS);
      EvaluationContext context =
          createContext(Map.of("transactionHour", 3, "transactionHour_usual", List.of(9, 12, 18)));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }
  }

  @Nested
  @DisplayName("DOMAIN_IN_LIST")
  class DomainInListTests {

    @Test
    void shouldReturnTrueForSuspiciousDomain() {
      RuleCondition condition = createCondition("email", ConditionOperator.DOMAIN_IN_LIST);
      EvaluationContext context = createContext(Map.of("email", "user@tempmail.com"));

      assertThat(evaluator.evaluate(condition, context)).isTrue();
    }

    @Test
    void shouldReturnFalseForSafeDomain() {
      RuleCondition condition = createCondition("email", ConditionOperator.DOMAIN_IN_LIST);
      EvaluationContext context = createContext(Map.of("email", "user@example.com"));

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }

  @Nested
  @DisplayName("CONTAINS_SUSPICIOUS_KEYWORDS")
  class SuspiciousKeywordsTests {

    @ParameterizedTest
    @CsvSource({
      "transferir agora, true",
      "sua conta foi bloqueada, true",
      "hello world, false",
      "prize winner, true",
      "regular message, false",
      "confirmar senha urgente, true"
    })
    void shouldDetectSuspiciousKeywords(String text, boolean expected) {
      RuleCondition condition =
          createCondition("message", ConditionOperator.CONTAINS_SUSPICIOUS_KEYWORDS);
      EvaluationContext context = createContext(Map.of("message", text));

      assertThat(evaluator.evaluate(condition, context)).isEqualTo(expected);
    }
  }

  @Nested
  @DisplayName("Casos de borda")
  class EdgeCases {

    @Test
    void shouldHandleNullPayload() {
      RuleCondition condition = createCondition("email", ConditionOperator.DOMAIN_IN_LIST);
      EvaluationContext context = createContext(null);

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }

    @Test
    void shouldHandleMissingFieldGracefully() {
      RuleCondition condition = createCondition("email", ConditionOperator.IN_LIST);
      condition.setValueArray(List.of("a@x.com"));
      EvaluationContext context = createContext(new HashMap<>());

      assertThat(evaluator.evaluate(condition, context)).isFalse();
    }
  }
}
