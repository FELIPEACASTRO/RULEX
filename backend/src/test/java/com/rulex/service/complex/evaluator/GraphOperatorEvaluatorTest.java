package com.rulex.service.complex.evaluator;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.Neo4jGraphService;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@DisplayName("GraphOperatorEvaluator Tests")
@ExtendWith(MockitoExtension.class)
class GraphOperatorEvaluatorTest {

  @Mock private Neo4jGraphService neo4jService;

  private GraphOperatorEvaluator evaluator;

  @BeforeEach
  void setUp() {
    evaluator = new GraphOperatorEvaluator(neo4jService);
  }

  private RuleCondition condition(ConditionOperator operator, String value) {
    RuleCondition condition = new RuleCondition();
    condition.setOperator(operator);
    condition.setValueSingle(value);
    return condition;
  }

  private EvaluationContext context(TransactionRequest request) {
    return EvaluationContext.builder()
        .transactionRequest(request)
        .payload(Map.of("accountId", "acct-1"))
        .build();
  }

  @Test
  @DisplayName("deve suportar operadores Neo4j")
  void shouldSupportOperators() {
    assertThat(evaluator.getSupportedOperators())
        .contains(
            ConditionOperator.NEO4J_WEAKLY_CONNECTED_COMPONENTS,
            ConditionOperator.NEO4J_FRAUD_RING_DETECTION,
            ConditionOperator.NEO4J_PAGERANK_FRAUD_SCORE);
  }

  @Test
  @DisplayName("deve retornar categoria correta")
  void shouldReturnCategory() {
    assertThat(evaluator.getCategory()).isEqualTo("GRAPH");
  }

  @Nested
  @DisplayName("NEO4J_WEAKLY_CONNECTED_COMPONENTS")
  class WccTests {

    @Test
    void shouldReturnTrueWhenComponentIdAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.NEO4J_WEAKLY_CONNECTED_COMPONENTS, "2");
      when(neo4jService.getWeaklyConnectedComponentId(anyString())).thenReturn(5);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_FRAUD_RING_DETECTION")
  class FraudRingTests {

    @Test
    void shouldReturnTrueWhenInRing() {
      RuleCondition condition = condition(ConditionOperator.NEO4J_FRAUD_RING_DETECTION, "3");
      when(neo4jService.isInFraudRing(anyString(), anyInt())).thenReturn(true);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_PAGERANK_FRAUD_SCORE")
  class PageRankTests {

    @Test
    void shouldReturnTrueWhenScoreAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.NEO4J_PAGERANK_FRAUD_SCORE, "0.5");
      when(neo4jService.getPageRankScore(anyString())).thenReturn(0.9);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_BETWEENNESS_CENTRALITY_MULE")
  class BetweennessTests {

    @Test
    void shouldReturnTrueWhenCentralityAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.NEO4J_BETWEENNESS_CENTRALITY_MULE, "0.3");
      when(neo4jService.getBetweennessCentrality(anyString())).thenReturn(0.8);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_SHORTEST_PATH_AML_TRACKING")
  class ShortestPathTests {

    @Test
    void shouldReturnTrueWhenPathWithinThreshold() {
      RuleCondition condition = condition(ConditionOperator.NEO4J_SHORTEST_PATH_AML_TRACKING, "3");
      when(neo4jService.getShortestPathToHighRisk(anyString())).thenReturn(2);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_TRIANGLE_COUNT_COLLUSION")
  class TriangleCountTests {

    @Test
    void shouldReturnTrueWhenTriangleCountAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.NEO4J_TRIANGLE_COUNT_COLLUSION, "2");
      when(neo4jService.getTriangleCount(anyString())).thenReturn(3);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD")
  class FraudLabelTests {

    @Test
    void shouldReturnTrueWhenLabelMatches() {
      RuleCondition condition =
          condition(ConditionOperator.NEO4J_LABEL_PROPAGATION_FRAUD_SPREAD, "FRAUD");
      when(neo4jService.getFraudLabel(anyString())).thenReturn("FRAUD");

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_ENTITY_RESOLUTION_SHARED_PII")
  class SharedPiiTests {

    @Test
    void shouldReturnTrueWhenSharedPiiCountAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.NEO4J_ENTITY_RESOLUTION_SHARED_PII, "2");
      when(neo4jService.findAccountsWithSharedPii(anyString()))
          .thenReturn(List.of("acct-2", "acct-3"));

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_NODE_SIMILARITY_SYNTHETIC_ID")
  class NodeSimilarityTests {

    @Test
    void shouldReturnTrueWhenSimilarityAboveThreshold() {
      RuleCondition condition =
          condition(ConditionOperator.NEO4J_NODE_SIMILARITY_SYNTHETIC_ID, "0.7");
      condition.setValueFieldRef("acct-2");
      when(neo4jService.getNodeSimilarity(anyString(), anyString())).thenReturn(0.9);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }

  @Nested
  @DisplayName("NEO4J_TEMPORAL_MOTIF_PATTERN")
  class TemporalMotifTests {

    @Test
    void shouldReturnTrueWhenCountAboveThreshold() {
      RuleCondition condition = condition(ConditionOperator.NEO4J_TEMPORAL_MOTIF_PATTERN, "BURST");
      condition.setValueMin("2");
      when(neo4jService.getTemporalMotifCount(anyString(), anyString())).thenReturn(3);

      assertThat(evaluator.evaluate(condition, context(new TransactionRequest()))).isTrue();
    }
  }
}
