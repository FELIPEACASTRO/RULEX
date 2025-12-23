package com.rulex.v31.ast;

import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.util.PanMaskingUtil;
import com.rulex.v31.features.FeatureProvider;
import java.util.Optional;
import org.junit.jupiter.api.Test;

public class AstEvaluatorTest {

  private final ObjectMapper om = new ObjectMapper();

  @Test
  void evaluatesEqOnNumber() throws Exception {
    AstEvaluator e = new AstEvaluator();

    var ast =
        om.readTree(
            """
            {
              "type":"CONDITION",
              "left": {"type":"FIELD","jsonPath":"$.mcc","dataType":"number"},
              "operator":"EQ",
              "right": 3121
            }
            """);

    var payload = om.readTree("{\"mcc\":3121}");
    assertTrue(e.evaluate(ast, payload));

    var payload2 = om.readTree("{\"mcc\":1}");
    assertFalse(e.evaluate(ast, payload2));
  }

  @Test
  void shortCircuitsFraudInControllerStyle() throws Exception {
    // The evaluator itself doesn't short-circuit; this asserts NOT group works.
    AstEvaluator e = new AstEvaluator();

    var ast =
        om.readTree(
            """
            {"type":"GROUP","op":"NOT","children":[{"type":"CONDITION","left":{"type":"FIELD","jsonPath":"$.pan","dataType":"string"},"operator":"IS_NULL"}]}
            """);

    var payload = om.readTree("{\"pan\":\"123\"}");
    assertTrue(e.evaluate(ast, payload));
  }

  @Test
  void evaluatesEqOnDerivedFeatureAndTracesIt() throws Exception {
    String pan = "4111111111111111";
    String maskedPan = PanMaskingUtil.mask(pan);

    FeatureProvider fp =
        new FeatureProvider() {
          @Override
          public Optional<com.fasterxml.jackson.databind.JsonNode> getFeature(
              String entityKey, String featureName) {
            if (maskedPan.equals(entityKey) && "txn_count_1h".equals(featureName)) {
              try {
                return Optional.of(om.readTree("5"));
              } catch (Exception e) {
                return Optional.empty();
              }
            }
            return Optional.empty();
          }
        };

    AstEvaluator e = new AstEvaluator(fp);

    var ast =
        om.readTree(
            """
            {
              "type":"CONDITION",
              "left": {"type":"FIELD","jsonPath":"$feature.txn_count_1h","dataType":"number"},
              "operator":"EQ",
              "right": 5
            }
            """);

    var payload = om.readTree("{\"pan\":\"" + pan + "\"}");
    AstEvaluator.EvaluationTraceResult res = e.evaluateWithTrace(ast, payload);
    assertTrue(res.match());

    assertTrue(
        res.featuresUsed().stream()
            .anyMatch(
                f ->
                    "feature".equals(f.source())
                        && "txn_count_1h".equals(f.name())
                        && maskedPan.equals(f.entityKey())));
  }
}
