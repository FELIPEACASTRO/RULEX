package com.rulex.service.complex.dispatch;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class OperatorDispatchService {

  private static final Logger log = LoggerFactory.getLogger(OperatorDispatchService.class);

  private OperatorDispatchService() {}

  public static boolean dispatch(
      OperatorEvaluatorRegistry registry,
      ConditionOperator operator,
      RuleCondition condition,
      EvaluationContext context) {
    try {
      OperatorEvaluator evaluator = registry.getEvaluator(operator);
      log.debug("Delegando operador {} para {}", operator, evaluator.getClass().getSimpleName());
      return evaluator.evaluate(condition, context);
    } catch (UnsupportedOperatorException e) {
      log.error("Operador não suportado: {} - nem no switch nem no registry", operator);
      throw e;
    } catch (Exception e) {
      log.error("Erro ao delegar operador {} para registry: {}", operator, e.getMessage());
      throw new UnsupportedOperatorException(
          operator,
          "Erro ao avaliar operador via registry: " + e.getMessage());
    }
  }
}
