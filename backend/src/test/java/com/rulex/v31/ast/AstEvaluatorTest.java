package com.rulex.v31.ast;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

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
}
