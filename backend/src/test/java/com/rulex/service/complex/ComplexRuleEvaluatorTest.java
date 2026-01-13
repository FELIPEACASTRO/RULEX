package com.rulex.service.complex;

import static org.assertj.core.api.Assertions.assertThat;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleCondition.ConditionOperator;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleConditionGroup.GroupLogicOperator;
import com.rulex.service.GeoService;
import com.rulex.service.OperatorDataService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.StatisticalAnalysisService;
import com.rulex.service.FuzzyLogicService;
import com.rulex.service.StringSimilarityService;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o ComplexRuleEvaluator.
 *
 * <p>GAP-FIX #3: Cobertura de testes para classes críticas.
 */
@DisplayName("ComplexRuleEvaluator Tests")
class ComplexRuleEvaluatorTest {

  private GeoService geoService;
  private VelocityService velocityService;
  private VelocityServiceFacade velocityServiceFacade;
  private OperatorDataService operatorDataService;
  private ComplexRuleEvaluator evaluator;

  @BeforeEach
  void setUp() {
    geoService = Mockito.mock(GeoService.class);
    velocityService = Mockito.mock(VelocityService.class);
    velocityServiceFacade = Mockito.mock(VelocityServiceFacade.class);
    operatorDataService = Mockito.mock(OperatorDataService.class);
    evaluator =
        new ComplexRuleEvaluator(
            geoService, velocityService, velocityServiceFacade, operatorDataService);
  }

  @Nested
  @DisplayName("Operadores de Comparação")
  class ComparisonOperators {

    @Test
    @DisplayName("EQ deve comparar valores iguais")
    void equalsOperator_shouldMatchEqualValues() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("NEQ deve comparar valores diferentes")
    void notEqualsOperator_shouldMatchDifferentValues() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.NEQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 7995));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("GT deve comparar valores numéricos")
    void greaterThanOperator_shouldCompareNumericValues() {
      RuleCondition condition = createCondition("transactionAmount", ConditionOperator.GT, "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("1500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("LT deve comparar valores numéricos")
    void lessThanOperator_shouldCompareNumericValues() {
      RuleCondition condition = createCondition("transactionAmount", ConditionOperator.LT, "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("GTE deve incluir valor igual")
    void greaterThanOrEqualsOperator_shouldIncludeEqualValue() {
      RuleCondition condition = createCondition("transactionAmount", ConditionOperator.GTE, "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("1000")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("LTE deve incluir valor igual")
    void lessThanOrEqualsOperator_shouldIncludeEqualValue() {
      RuleCondition condition = createCondition("transactionAmount", ConditionOperator.LTE, "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("1000")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Operadores de String")
  class StringOperators {

    @Test
    @DisplayName("CONTAINS deve verificar substring")
    void containsOperator_shouldCheckSubstring() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.CONTAINS, "FRAUD");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("merchantName", "SUSPECTED FRAUD MERCHANT"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("NOT_CONTAINS deve verificar ausência de substring")
    void notContainsOperator_shouldCheckAbsenceOfSubstring() {
      RuleCondition condition =
          createCondition("merchantName", ConditionOperator.NOT_CONTAINS, "FRAUD");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("merchantName", "LEGITIMATE MERCHANT"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("STARTS_WITH deve verificar prefixo")
    void startsWithOperator_shouldCheckPrefix() {
      RuleCondition condition = createCondition("pan", ConditionOperator.STARTS_WITH, "4111");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("pan", "4111111111111111"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("ENDS_WITH deve verificar sufixo")
    void endsWithOperator_shouldCheckSuffix() {
      RuleCondition condition = createCondition("pan", ConditionOperator.ENDS_WITH, "1111");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("pan", "4111111111111111"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("REGEX deve validar padrão regex")
    void matchesRegexOperator_shouldValidatePattern() {
      RuleCondition condition = createCondition("email", ConditionOperator.REGEX, ".*@test\\.com$");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("email", "user@test.com"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Operadores de Lista")
  class ListOperators {

    @Test
    @DisplayName("IN deve verificar se valor está na lista")
    void inOperator_shouldCheckValueInList() {
      RuleCondition condition =
          createConditionWithArray("mcc", ConditionOperator.IN, List.of("5411", "7995", "5812"));
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 7995));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("NOT_IN deve verificar se valor não está na lista")
    void notInOperator_shouldCheckValueNotInList() {
      RuleCondition condition =
          createConditionWithArray(
              "mcc", ConditionOperator.NOT_IN, List.of("5411", "7995", "5812"));
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 1234));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Operadores Nulos")
  class NullOperators {

    @Test
    @DisplayName("IS_NULL deve verificar valor nulo")
    void isNullOperator_shouldCheckNullValue() {
      RuleCondition condition = createCondition("cardExpireDate", ConditionOperator.IS_NULL, "");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("someOtherField", "value"));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("NOT_NULL deve verificar valor não nulo")
    void isNotNullOperator_shouldCheckNonNullValue() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.NOT_NULL, "");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Operadores Lógicos")
  class LogicalOperators {

    @Test
    @DisplayName("AND deve exigir todas as condições verdadeiras")
    void andOperator_shouldRequireAllConditionsTrue() {
      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("transactionAmount", ConditionOperator.GT, "100");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition1, condition2);

      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("mcc", 5411, "transactionAmount", new BigDecimal("500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("AND deve falhar se uma condição for falsa")
    void andOperator_shouldFailIfOneConditionFalse() {
      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("transactionAmount", ConditionOperator.GT, "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition1, condition2);

      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("mcc", 5411, "transactionAmount", new BigDecimal("500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("OR deve exigir pelo menos uma condição verdadeira")
    void orOperator_shouldRequireAtLeastOneConditionTrue() {
      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "7995");
      RuleCondition condition2 =
          createCondition("transactionAmount", ConditionOperator.GT, "10000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.OR, condition1, condition2);

      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("mcc", 5411, "transactionAmount", new BigDecimal("15000")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("OR deve falhar se todas as condições forem falsas")
    void orOperator_shouldFailIfAllConditionsFalse() {
      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "7995");
      RuleCondition condition2 =
          createCondition("transactionAmount", ConditionOperator.GT, "10000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.OR, condition1, condition2);

      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("mcc", 5411, "transactionAmount", new BigDecimal("500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isFalse();
    }

    @Test
    @DisplayName("XOR deve exigir exatamente uma condição verdadeira")
    void xorOperator_shouldRequireExactlyOneConditionTrue() {
      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 =
          createCondition("transactionAmount", ConditionOperator.GT, "10000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.XOR, condition1, condition2);

      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("mcc", 5411, "transactionAmount", new BigDecimal("500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("NOT deve inverter resultado")
    void notOperator_shouldInvertResult() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "7995");
      RuleConditionGroup group = createGroup(GroupLogicOperator.NOT, condition);

      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Cenários de Borda")
  class EdgeCases {

    @Test
    @DisplayName("Deve retornar true para grupo nulo")
    void shouldReturnTrueForNullGroup() {
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(null, context);

      assertThat(result.isMatched()).isTrue();
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
    }

    @Test
    @DisplayName("Deve ignorar condição desabilitada")
    void shouldIgnoreDisabledCondition() {
      RuleCondition condition1 = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleCondition condition2 = createCondition("mcc", ConditionOperator.EQ, "7995");
      condition2.setEnabled(false);

      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition1, condition2);

      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("Deve incluir tempo de execução no resultado")
    void shouldIncludeExecutionTimeInResult() {
      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("mcc", 5411));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.getExecutionTimeMs()).isGreaterThanOrEqualTo(0);
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
  }

  @Nested
  @DisplayName("Operadores de Range")
  class RangeOperators {

    @Test
    @DisplayName("BETWEEN deve verificar valor no intervalo")
    void betweenOperator_shouldCheckValueInRange() {
      RuleCondition condition =
          createConditionWithRange("transactionAmount", ConditionOperator.BETWEEN, "100", "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("NOT_BETWEEN deve verificar valor fora do intervalo")
    void notBetweenOperator_shouldCheckValueOutOfRange() {
      RuleCondition condition =
          createConditionWithRange(
              "transactionAmount", ConditionOperator.NOT_BETWEEN, "100", "1000");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context =
          createContext(Map.of("transactionAmount", new BigDecimal("1500")));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Avaliação com TransactionRequest")
  class TransactionRequestEvaluation {

    @Test
    @DisplayName("Deve avaliar condição com TransactionRequest")
    void shouldEvaluateConditionWithTransactionRequest() {
      TransactionRequest request =
          TransactionRequest.builder()
              .externalTransactionId("tx-1")
              .mcc(5411)
              .transactionAmount(new BigDecimal("1500"))
              .merchantCountryCode("076")
              .build();

      RuleCondition condition = createCondition("mcc", ConditionOperator.EQ, "5411");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);

      ComplexRuleEvaluator.EvaluationContext context =
          ComplexRuleEvaluator.EvaluationContext.builder()
              .transactionRequest(request)
              .payload(Map.of("mcc", 5411))
              .build();

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }
  }

  @Nested
  @DisplayName("Operadores Booleanos")
  class BooleanOperators {

    @Test
    @DisplayName("IS_TRUE deve verificar valor verdadeiro")
    void isTrueOperator_shouldCheckTrueValue() {
      RuleCondition condition = createCondition("isHighRisk", ConditionOperator.IS_TRUE, "");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("isHighRisk", true));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
    }

    @Test
    @DisplayName("IS_FALSE deve verificar valor falso")
    void isFalseOperator_shouldCheckFalseValue() {
      RuleCondition condition = createCondition("isHighRisk", ConditionOperator.IS_FALSE, "");
      RuleConditionGroup group = createGroup(GroupLogicOperator.AND, condition);
      ComplexRuleEvaluator.EvaluationContext context = createContext(Map.of("isHighRisk", false));

      ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

      assertThat(result.isMatched()).isTrue();
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

  private RuleCondition createConditionWithArray(
      String field, ConditionOperator operator, List<String> values) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(field);
    condition.setOperator(operator);
    condition.setValueArray(values);
    condition.setEnabled(true);
    return condition;
  }

  private RuleCondition createConditionWithRange(
      String field, ConditionOperator operator, String min, String max) {
    RuleCondition condition = new RuleCondition();
    condition.setFieldName(field);
    condition.setOperator(operator);
    condition.setValueMin(min);
    condition.setValueMax(max);
    condition.setEnabled(true);
    return condition;
  }

  private RuleConditionGroup createGroup(
      GroupLogicOperator logicOperator, RuleCondition... conditions) {
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
