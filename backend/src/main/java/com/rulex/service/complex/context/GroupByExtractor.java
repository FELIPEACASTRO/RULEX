package com.rulex.service.complex.context;

import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;

public final class GroupByExtractor {

  private GroupByExtractor() {}

  public static String extractGroupBy(EvaluationContext context) {
    if (context == null || context.getPayload() == null) {
      return "unknown";
    }

    if (context.getPayload().containsKey("cardNumber")) {
      return String.valueOf(context.getPayload().get("cardNumber"));
    }
    if (context.getPayload().containsKey("accountId")) {
      return String.valueOf(context.getPayload().get("accountId"));
    }
    if (context.getPayload().containsKey("customerId")) {
      return String.valueOf(context.getPayload().get("customerId"));
    }
    return "unknown";
  }
}
