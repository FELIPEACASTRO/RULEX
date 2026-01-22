package com.rulex.service.complex.context;

import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Map;

public final class FieldValueExtractor {

  private FieldValueExtractor() {}

  @SuppressWarnings("unchecked")
  public static Object getFieldValue(String fieldName, String fieldPath, EvaluationContext context) {
    if (context == null) {
      return null;
    }

    if (context.getVariables() != null && context.getVariables().containsKey(fieldName)) {
      return context.getVariables().get(fieldName);
    }

    if (context.getPayload() == null) {
      return null;
    }

    if (fieldPath != null && !fieldPath.isEmpty()) {
      String[] parts = fieldPath.split("\\.");
      Object current = context.getPayload();

      for (String part : parts) {
        if (current == null) return null;
        if (current instanceof Map) {
          current = ((Map<String, Object>) current).get(part);
        } else {
          return null;
        }
      }
      return current;
    }

    return context.getPayload().get(fieldName);
  }
}
