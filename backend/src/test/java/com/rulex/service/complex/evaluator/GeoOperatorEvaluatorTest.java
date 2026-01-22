package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.GeoService;
import com.rulex.service.GeoService.GeoResult;
import com.rulex.service.ImpossibleTravelService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@DisplayName("GeoOperatorEvaluator Tests")
@ExtendWith(MockitoExtension.class)
class GeoOperatorEvaluatorTest {

  @Mock
  private GeoService geoService;

  @Mock
  private ImpossibleTravelService impossibleTravelService;

  private GeoOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new GeoOperatorEvaluator(geoService, impossibleTravelService);
  }

  private RuleCondition condition(ConditionOperator operator) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle("100");
    condition.setValueMin("-23.0");
    condition.setValueMax("-46.0");
    return condition;
  }

  private EvaluationContext context(TransactionRequest request) {
    return EvaluationContext.builder().transactionRequest(request).payload(Map.of()).build();
  }

  @Test
  @DisplayName("deve suportar operadores GEO")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(ConditionOperator.GEO_DISTANCE_LT, ConditionOperator.GEO_DISTANCE_GT, ConditionOperator.GEO_IN_POLYGON);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("GEO");
  }

  @Nested
  @DisplayName("GEO_DISTANCE_LT")
  class GeoDistanceLtTests {

    @Test
    void shouldReturnTrueWhenGeoServiceReturnsTrue() {
      RuleCondition condition = condition(ConditionOperator.GEO_DISTANCE_LT);
      TransactionRequest request = new TransactionRequest();

      when(geoService.evaluateDistanceLessThan(any(), anyDouble(), anyDouble(), anyDouble()))
          .thenReturn(GeoResult.builder().result(true).success(true).distance(10).build());

      assertThat(evaluator.evaluate(condition, context(request))).isTrue();
    }

    @Test
    void shouldReturnFalseWhenGeoServiceReturnsFalse() {
      RuleCondition condition = condition(ConditionOperator.GEO_DISTANCE_LT);
      TransactionRequest request = new TransactionRequest();

      when(geoService.evaluateDistanceLessThan(any(), anyDouble(), anyDouble(), anyDouble()))
          .thenReturn(GeoResult.builder().result(false).success(true).distance(150).build());

      assertThat(evaluator.evaluate(condition, context(request))).isFalse();
    }
  }

  @Nested
  @DisplayName("GEO_DISTANCE_GT")
  class GeoDistanceGtTests {

    @Test
    void shouldReturnTrueWhenGeoServiceReturnsTrue() {
      RuleCondition condition = condition(ConditionOperator.GEO_DISTANCE_GT);
      TransactionRequest request = new TransactionRequest();

      when(geoService.evaluateDistanceGreaterThan(any(), anyDouble(), anyDouble(), anyDouble()))
          .thenReturn(GeoResult.builder().result(true).success(true).distance(300).build());

      assertThat(evaluator.evaluate(condition, context(request))).isTrue();
    }

    @Test
    void shouldReturnFalseWhenRequestMissing() {
      RuleCondition condition = condition(ConditionOperator.GEO_DISTANCE_GT);

      assertThat(evaluator.evaluate(condition, context(null))).isFalse();
    }

    @Test
    void shouldReturnFalseWhenGeoServiceReturnsFalse() {
      RuleCondition condition = condition(ConditionOperator.GEO_DISTANCE_GT);
      TransactionRequest request = new TransactionRequest();

      when(geoService.evaluateDistanceGreaterThan(any(), anyDouble(), anyDouble(), anyDouble()))
          .thenReturn(GeoResult.builder().result(false).success(true).distance(20).build());

      assertThat(evaluator.evaluate(condition, context(request))).isFalse();
    }
  }

  @Nested
  @DisplayName("GEO_IN_POLYGON")
  class GeoInPolygonTests {

    @Test
    void shouldReturnTrueWhenInPolygon() {
      RuleCondition condition = condition(ConditionOperator.GEO_IN_POLYGON);
      condition.setValueSingle("RiskZone");
      TransactionRequest request = new TransactionRequest();

      when(geoService.evaluateInPolygon(any(), anyString()))
          .thenReturn(GeoResult.builder().result(true).success(true).distance(0).build());

      assertThat(evaluator.evaluate(condition, context(request))).isTrue();
    }

    @Test
    void shouldReturnFalseWhenPolygonMissing() {
      RuleCondition condition = condition(ConditionOperator.GEO_IN_POLYGON);
      condition.setValueSingle("");

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isFalse();
    }

    @Test
    void shouldReturnFalseWhenGeoServiceReturnsFalse() {
      RuleCondition condition = condition(ConditionOperator.GEO_IN_POLYGON);
      condition.setValueSingle("RiskZone");
      TransactionRequest request = new TransactionRequest();

      when(geoService.evaluateInPolygon(any(), anyString()))
          .thenReturn(GeoResult.builder().result(false).success(true).distance(0).build());

      assertThat(evaluator.evaluate(condition, context(request))).isFalse();
    }
  }
}
