package com.rulex.service.complex;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleConditionGroup.GroupLogicOperator;
import com.rulex.service.OperatorMetricsService;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import com.rulex.service.complex.evaluation.ConditionGroupEvaluator;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o ComplexRuleEvaluator refatorado.
 *
 * <p>Estes testes focam na orquestração da avaliação, não na lógica de operadores individuais
 * (que é testada nos testes dos evaluators específicos).
 */
@DisplayName("ComplexRuleEvaluator Tests")
class ComplexRuleEvaluatorTest {

  private OperatorEvaluatorRegistry operatorEvaluatorRegistry;
  private OperatorEvaluator mockEvaluator;
  private ConditionGroupEvaluator conditionGroupEvaluator;
  private OperatorMetricsService operatorMetricsService;
  private ComplexRuleEvaluator evaluator;

  @BeforeEach
  void setUp() {
    operatorEvaluatorRegistry = Mockito.mock(OperatorEvaluatorRegistry.class);
    mockEvaluator = Mockito.mock(OperatorEvaluator.class);
    conditionGroupEvaluator = new ConditionGroupEvaluator();
    operatorMetricsService = Mockito.mock(OperatorMetricsService.class);

    when(operatorEvaluatorRegistry.getEvaluator(any(ConditionOperator.class)))
        .thenReturn(mockEvaluator);

    when(operatorMetricsService.recordTimed(any(), any())).thenAnswer(invocation -> {
      @SuppressWarnings("unchecked")
      Supplier<Boolean> supplier = (Supplier<Boolean>) invocation.getArgument(1);
      return supplier.get();
    });

    evaluator =
        new ComplexRuleEvaluator(
            operatorEvaluatorRegistry, conditionGroupEvaluator, operatorMetricsService);
  }

  @Nested
  @DisplayName("Delegação para Registry")
  class RegistryDelegation {

    @Test
    @DisplayName("Deve delegar avaliação para o registry")
    void shouldDelegateToRegistry() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      evaluator.evaluate(group, context);

      verify(operatorEvaluatorRegistry).getEvaluator(ConditionOperator.EQ);
      verify(mockEvaluator).evaluate(any(RuleCondition.class), any());
    }

    @Test
    @DisplayName("Deve delegar múltiplas condições para o registry")
    void shouldDelegateMultipleConditionsToRegistry() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("amount", ConditionOperator.GT, "100");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition1, condition2);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      evaluator.evaluate(group, context);

      verify(mockEvaluator, times(2)).evaluate(any(RuleCondition.class), any());
    }
  }

  @Nested
  @DisplayName("Lógica AND")
  class AndLogic {

    @Test
    @DisplayName("AND deve retornar true quando todas condições são verdadeiras")
    void andShouldReturnTrueWhenAllConditionsTrue() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("amount", ConditionOperator.GT, "100");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition1, condition2);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("AND deve retornar false quando uma condição é falsa")
    void andShouldReturnFalseWhenOneConditionFalse() {
      when(mockEvaluator.evaluate(any(), any()))
          .thenReturn(true)
          .thenReturn(false);

      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("amount", ConditionOperator.GT, "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition1, condition2);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }
  }

  @Nested
  @DisplayName("Lógica OR")
  class OrLogic {

    @Test
    @DisplayName("OR deve retornar true quando pelo menos uma condição é verdadeira")
    void orShouldReturnTrueWhenAtLeastOneConditionTrue() {
      when(mockEvaluator.evaluate(any(), any()))
          .thenReturn(false)
          .thenReturn(true);

      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "7995");
      RuleCondition condition2 = createCondition("amount", ConditionOperator.GT, "100");
      RuleConditionGroup group = createGroup(GroupLogicOperator.OR, condition1, condition2);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("OR deve retornar false quando todas condições são falsas")
    void orShouldReturnFalseWhenAllConditionsFalse() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(false);

      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "7995");
      RuleCondition condition2 = createCondition("amount", ConditionOperator.GT, "10000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.OR, condition1, condition2);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }
  }

  @Nested
  @DisplayName("Grupos Aninhados")
  class NestedGroups {

    @Test
    @DisplayName("Deve avaliar grupos aninhados corretamente")
    void shouldEvaluateNestedGroupsCorrectly() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("amount", ConditionOperator.GT, "100");
      RuleConditionGroup childGroup = createGroup(GroupLogicOperator.AND, condition2);
      
      RuleConditionGroup parentGroup = createGroup(GroupLogicOperator.AND, condition1);
      parentGroup.setChildren(List.of(childGroup));

      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(parentGroup, context);

      assertThat(result.isMatched()).isTrue();
      verify(mockEvaluator, times(2)).evaluate(any(), any());
    }
  }

  @Nested
  @DisplayName("Condições Desabilitadas")
  class DisabledConditions {

    @Test
    @DisplayName("Deve ignorar condições desabilitadas")
    void shouldIgnoreDisabledConditions() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition enabledCondition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition disabledCondition = createCondition("amount", ConditionOperator.GT, "10000");
      disabledCondition.setEnabled(false);

      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, enabledCondition, disabledCondition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411, "amount", 500));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
      verify(mockEvaluator, times(1)).evaluate(any(), any());
    }

    @Test
    @DisplayName("Deve retornar true para grupo desabilitado")
    void shouldReturnTrueForDisabledGroup() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "7995");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      group.setEnabled(false);

      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
      verify(mockEvaluator, never()).evaluate(any(), any());
    }
  }

  @Nested
  @DisplayName("Casos de Borda")
  class EdgeCases {

    @Test
    @DisplayName("Deve retornar true para grupo nulo")
    void shouldReturnTrueForNullGroup() {
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(null, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve retornar true para grupo sem condições")
    void shouldReturnTrueForGroupWithoutConditions() {
      RuleConditionGroup group = new RuleConditionGroup();
      group.setLogicOperator(GroupLogicOperator.AND);
      group.setEnabled(true);

      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve incluir tempo de execução no resultado")
    void shouldIncludeExecutionTimeInResult() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.getExecutionTimeMs()).isGreaterThanOrEqualTo(0);
    }

    @Test
    @DisplayName("Deve coletar detalhes de execução")
    void shouldCollectExecutionDetails() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.getExecutionDetails()).isNotNull();
      assertThat(result.getExecutionDetails()).hasSize(1);
    }
  }

  @Nested
  @DisplayName("Contexto com TransactionRequest")
  class TransactionRequestContext {

    @Test
    @DisplayName("Deve avaliar com TransactionRequest no contexto")
    void shouldEvaluateWithTransactionRequest() {
      when(mockEvaluator.evaluate(any(), any())).thenReturn(true);

      TransactionRequest request = TransactionRequest.builder()
          .externalTransactionId("tx-1")
          .mcc(5411)
          .transactionAmount(new BigDecimal("1500"))
          .build();

      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationContext context = ComplexRuleEvaluator.EvaluationContext.builder()
          .transactionRequest(request)
          .payload(Map.of("mcc", 5411))
          .build();

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Tratamento de Erros")
  class ErrorHandling {

    @Test
    @DisplayName("Deve capturar erro e retornar resultado com mensagem de erro")
    void shouldCaptureErrorAndReturnResultWithErrorMessage() {
      when(mockEvaluator.evaluate(any(), any()))
          .thenThrow(new RuntimeException("Erro de teste"));

      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
      // O erro é capturado na condição individual, não no grupo
      assertThat(result.getExecutionDetails()).hasSize(1);
    }
  }

  // ========== Helpers ==========

  private RuleCondition createCondition(String field, ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(field);
    condition.setOperator(operator);
    condition.setValueSingle(value);
    condition.setEnabled(true);
    return condition;
  }

  private RuleConditionGroup createGroup(GroupLogicOperator logicOperator, RuleCondition... conditions) {
    RuleConditionGroup group = new RuleConditionGroup();
    group.setLogicOperator(logicOperator);
    group.setConditions(List.of(conditions));
    group.setEnabled(true);
    return group;
  }

  private ComplexRuleEvaluator.EvaluationContext createContext(Map<String, Object> payload) {
    return ComplexRuleEvaluator.EvaluationContext.builder().payload(payload).build();
  }
}
