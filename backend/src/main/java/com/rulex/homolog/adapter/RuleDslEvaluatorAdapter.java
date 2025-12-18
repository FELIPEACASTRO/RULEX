package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.LogicOperator;
import com.rulex.homolog.SafeRuleDslEvaluator;
import com.rulex.homolog.port.RuleDslEvaluatorPort;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class RuleDslEvaluatorAdapter implements RuleDslEvaluatorPort {

  private final SafeRuleDslEvaluator evaluator;

  public RuleDslEvaluatorAdapter(SafeRuleDslEvaluator evaluator) {
    this.evaluator = evaluator;
  }

  @Override
  public boolean evaluate(Map<String, Object> payload, LogicOperator logic, String conditionsJson) {
    return evaluator.evaluate(payload, logic, conditionsJson);
  }
}
