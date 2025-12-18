package com.rulex.homolog.port;

import com.rulex.entity.homolog.LogicOperator;
import java.util.Map;

public interface RuleDslEvaluatorPort {
  boolean evaluate(Map<String, Object> payload, LogicOperator logic, String conditionsJson);
}
