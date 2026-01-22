package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("StubOperatorEvaluator Tests")
class StubOperatorEvaluatorTest {

  private StubOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new StubOperatorEvaluator();
  }

  private RuleCondition condition() {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(ConditionOperator.EQUALS);
    condition.setFieldName("field");
    condition.setValueSingle("value");
    return condition;
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("PLANNED");
  }

  @Test
  @DisplayName("deve lançar UnsupportedOperatorException")
  void shouldThrowUnsupportedOperatorException() {
    RuleCondition condition = condition();
    EvaluationContext context = EvaluationContext.builder().payload(Map.of()).build();

    assertThatThrownBy(() -> evaluator.evaluate(condition, context))
        .isInstanceOf(UnsupportedOperatorException.class);
  }

  @Test
  @DisplayName("deve manter lista de operadores planejados vazia")
  void shouldHaveEmptyPlannedList() {
    assertThat(evaluator.getSupportedOperators()).isEmpty();
    assertThat(evaluator.getPlannedCount()).isZero();
  }
}
