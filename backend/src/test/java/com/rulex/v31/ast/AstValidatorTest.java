package com.rulex.v31.ast;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

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
}
