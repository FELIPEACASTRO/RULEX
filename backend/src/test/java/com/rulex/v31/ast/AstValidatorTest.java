package com.rulex.v31.ast;

import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

public class AstValidatorTest {

  private final ObjectMapper om = new ObjectMapper();

  @Test
  void rejectsMissingType() throws Exception {
    AstValidator v = new AstValidator();
    var root = om.readTree("{\"op\":\"AND\",\"children\":[]}");
    var r = v.validate(root);
    assertFalse(r.isValid());
  }

  @Test
  void acceptsSimpleCondition() throws Exception {
    AstValidator v = new AstValidator();
    var root =
        om.readTree(
            """
            {
              "type":"CONDITION",
              "left": {"type":"FIELD","jsonPath":"$.mcc","dataType":"number"},
              "operator":"EQ",
              "right": 3121
            }
            """);
    var r = v.validate(root);
    assertTrue(r.isValid(), String.valueOf(r.getErrors()));
  }

  @Test
  void rejectsNotWithTwoChildren() throws Exception {
    AstValidator v = new AstValidator();
    var root =
        om.readTree(
            """
            {"type":"GROUP","op":"NOT","children":[{"type":"CONST","dataType":"boolean","value":true},{"type":"CONST","dataType":"boolean","value":false}]}
            """);
    var r = v.validate(root);
    assertFalse(r.isValid());
  }

  @Test
  void normalizeOperatorHandlesAliases() {
    // Test NEQ -> NE normalization (ConditionDTO uses NEQ, AstValidator uses NE)
    assertEquals("NE", AstValidator.normalizeOperator("NEQ"));
    assertEquals("NE", AstValidator.normalizeOperator("neq"));
    assertEquals("NE", AstValidator.normalizeOperator("NOT_EQ"));
    assertEquals("NE", AstValidator.normalizeOperator("NOT_EQUALS"));

    // Test EQ aliases
    assertEquals("EQ", AstValidator.normalizeOperator("EQ"));
    assertEquals("EQ", AstValidator.normalizeOperator("EQUALS"));

    // Test REGEX -> MATCHES_REGEX normalization
    assertEquals("MATCHES_REGEX", AstValidator.normalizeOperator("REGEX"));

    // Non-aliased operators should pass through unchanged (uppercased)
    assertEquals("GT", AstValidator.normalizeOperator("gt"));
    assertEquals("LTE", AstValidator.normalizeOperator("lte"));
    assertEquals("IN", AstValidator.normalizeOperator("in"));

    // Null handling
    assertNull(AstValidator.normalizeOperator(null));
  }

  @Test
  void acceptsNEQOperatorAlias() throws Exception {
    AstValidator v = new AstValidator();
    // NEQ is used by ConditionDTO.OperatorType, should be accepted via normalization
    var root =
        om.readTree(
            """
            {
              "type":"CONDITION",
              "left": {"type":"FIELD","jsonPath":"$.merchantId","dataType":"string"},
              "operator":"NEQ",
              "right": "blocked_merchant"
            }
            """);
    var r = v.validate(root);
    assertTrue(r.isValid(), "NEQ should be accepted as alias for NE: " + r.getErrors());
  }

  @Test
  void acceptsREGEXOperatorAlias() throws Exception {
    AstValidator v = new AstValidator();
    // REGEX is used by ConditionDTO.OperatorType, should map to MATCHES_REGEX
    var root =
        om.readTree(
            """
            {
              "type":"CONDITION",
              "left": {"type":"FIELD","jsonPath":"$.merchantName","dataType":"string"},
              "operator":"REGEX",
              "right": "^ACME.*$"
            }
            """);
    var r = v.validate(root);
    assertTrue(r.isValid(), "REGEX should be accepted as alias for MATCHES_REGEX: " + r.getErrors());
  }
}
