package com.rulex.service.complex;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.service.FuzzyLogicService;
import com.rulex.service.GeoService;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.OperatorDataService;
import com.rulex.service.StatisticalAnalysisService;
import com.rulex.service.StringSimilarityService;
import com.rulex.service.VelocityService;
import com.rulex.service.VelocityServiceFacade;
import java.math.BigDecimal;
import java.util.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Testes unitários para os novos operadores de agregação temporal avançada no ComplexRuleEvaluator.
 */
@ExtendWith(MockitoExtension.class)
class ComplexRuleEvaluatorAggregationTest {

  @Mock private GeoService geoService;
  @Mock private VelocityService velocityService;
  @Mock private VelocityServiceFacade velocityServiceFacade;
  @Mock private OperatorDataService operatorDataService;
  @Mock private Neo4jGraphService neo4jGraphService;
  @Mock private StatisticalAnalysisService statisticalAnalysisService;
  @Mock private FuzzyLogicService fuzzyLogicService;
  @Mock private StringSimilarityService stringSimilarityService;

  private ComplexRuleEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator =
        new ComplexRuleEvaluator(
            geoService,
            velocityService,
            velocityServiceFacade,
            operatorDataService,
            neo4jGraphService,
            statisticalAnalysisService,
            fuzzyLogicService,
            stringSimilarityService);
  }

  @Test
  void testSumLastNDays_GreaterThan_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.SUM_LAST_N_DAYS,
            "amount|7|5000|GT" // Soma nos últimos 7 dias > 5000
            );

    Map<String, Object> payload =
        Map.of("cardNumber", "1234567890123456", "amount", new BigDecimal("1000"));

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando soma de 6000
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().totalAmount(new BigDecimal("6000")).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
    verify(velocityServiceFacade, times(1)).getStats(any(TransactionRequest.class), any(), any());
  }

  @Test
  void testCountLastNHours_LessThan_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.COUNT_LAST_N_HOURS,
            "24|100|LT" // Menos de 100 transações nas últimas 24 horas
            );

    Map<String, Object> payload = Map.of("cardNumber", "1234567890123456");

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando contagem de 50
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().transactionCount(50L).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
  }

  @Test
  void testAvgLastNDays_GreaterThanOrEqual_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.AVG_LAST_N_DAYS,
            "amount|30|500|GTE" // Média nos últimos 30 dias >= 500
            );

    Map<String, Object> payload = Map.of("accountId", "ACC123", "amount", new BigDecimal("600"));

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando média de 550
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().avgAmount(new BigDecimal("550")).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
  }

  @Test
  void testCountDistinctMerchantsLastNDays_GreaterThan_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS,
            "7|10|GT" // Mais de 10 merchants distintos nos últimos 7 dias
            );

    Map<String, Object> payload = Map.of("cardNumber", "1234567890123456");

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando 15 merchants distintos
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().distinctMerchants(15L).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
  }

  @Test
  void testCountDistinctCountriesLastNHours_LessThanOrEqual_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS,
            "24|5|LTE" // Até 5 países distintos nas últimas 24 horas
            );

    Map<String, Object> payload = Map.of("customerId", "CUST456");

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando 3 países distintos
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().distinctCountries(3L).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
  }

  @Test
  void testMaxAmountLastNDays_GreaterThan_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.MAX_AMOUNT_LAST_N_DAYS,
            "30|10000|GT" // Valor máximo nos últimos 30 dias > 10000
            );

    Map<String, Object> payload = Map.of("cardNumber", "1234567890123456");

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando valor máximo de 15000
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().maxAmount(new BigDecimal("15000")).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
  }

  @Test
  void testMinAmountLastNDays_LessThan_ShouldReturnTrue() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.MIN_AMOUNT_LAST_N_DAYS,
            "7|10|LT" // Valor mínimo nos últimos 7 dias < 10
            );

    Map<String, Object> payload = Map.of("accountId", "ACC789");

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Mock do VelocityServiceFacade retornando valor mínimo de 5
    VelocityService.VelocityStats stats =
        VelocityService.VelocityStats.builder().minAmount(new BigDecimal("5")).build();

    when(velocityServiceFacade.getStats(any(TransactionRequest.class), any(), any()))
        .thenReturn(stats);

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertTrue(result.isMatched());
  }

  @Test
  void testSumLastNDays_InvalidFormat_ShouldReturnFalse() {
    // Arrange
    RuleConditionGroup group =
        createGroupWithCondition(
            RuleCondition.ConditionOperator.SUM_LAST_N_DAYS, "invalid_format" // Formato inválido
            );

    Map<String, Object> payload = Map.of("cardNumber", "1234567890123456");

    ComplexRuleEvaluator.EvaluationContext context =
        ComplexRuleEvaluator.EvaluationContext.builder()
            .payload(payload)
            .transactionRequest(new TransactionRequest())
            .build();

    // Act
    ComplexRuleEvaluator.EvaluationResult result = evaluator.evaluate(group, context);

    // Assert
    assertFalse(result.isMatched());
  }

  // ========== Métodos Auxiliares ==========

  private RuleConditionGroup createGroupWithCondition(
      RuleCondition.ConditionOperator operator, String valueSingle) {

    RuleCondition condition =
        RuleCondition.builder()
            .id(UUID.randomUUID())
            .fieldName("testField")
            .operator(operator)
            .valueSingle(valueSingle)
            .enabled(true)
            .negate(false)
            .caseSensitive(true)
            .build();

    RuleConditionGroup group =
        RuleConditionGroup.builder()
            .id(UUID.randomUUID())
            .logicOperator(RuleConditionGroup.GroupLogicOperator.AND)
            .enabled(true)
            .conditions(List.of(condition))
            .build();

    condition.setGroup(group);

    return group;
  }
}
