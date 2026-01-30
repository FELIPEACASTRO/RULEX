package com.rulex.service.complex;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.entity.complex.RuleConditionGroup;
import com.rulex.entity.complex.RuleConditionGroup.GroupLogicOperator;
import com.rulex.entity.complex.RuleExecutionDetail;
import com.rulex.exception.UnsupportedOperatorException;
import com.rulex.service.OperatorMetricsService;
import com.rulex.service.complex.evaluation.ConditionGroupEvaluator;
import com.rulex.service.complex.evaluation.ExpectedValueFormatter;
import com.rulex.service.complex.evaluator.OperatorEvaluator;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador de regras complexas refatorado.
 *
 * <p>REFATORAÇÃO: Esta classe foi simplificada de 1695 linhas para ~250 linhas.
 * Toda a lógica de avaliação de operadores foi delegada para o {@link OperatorEvaluatorRegistry}
 * que gerencia avaliadores especializados por categoria de operador.
 *
 * <p>Responsabilidades desta classe:
 * <ul>
 *   <li>Orquestrar a avaliação de grupos de condições</li>
 *   <li>Delegar avaliação de operadores para o registry</li>
 *   <li>Coletar detalhes de execução para auditoria</li>
 *   <li>Gerenciar o contexto de avaliação</li>
 * </ul>
 *
 * @see OperatorEvaluatorRegistry
 * @see OperatorEvaluator
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleEvaluator {

  private final OperatorEvaluatorRegistry operatorEvaluatorRegistry;
  private final ConditionGroupEvaluator conditionGroupEvaluator;
  private final OperatorMetricsService operatorMetricsService;

  /** Resultado da avaliação de uma regra */
  @Data
  @Builder
  public static class EvaluationResult {
    private boolean matched;
    private List<RuleExecutionDetail> executionDetails;
    private long executionTimeMs;
    private String errorMessage;
  }

  /** Contexto de avaliação contendo os dados da transação */
  @Data
  @Builder
  public static class EvaluationContext {
    private Map<String, Object> payload;
    private Map<String, Object> variables;
    private UUID decisionLogId;
    private UUID ruleVersionId;
    private TransactionRequest transactionRequest;
  }

  /**
   * Avalia um grupo de condições contra um contexto.
   *
   * @param rootGroup O grupo raiz de condições
   * @param context O contexto de avaliação
   * @return O resultado da avaliação
   */
  public EvaluationResult evaluate(RuleConditionGroup rootGroup, EvaluationContext context) {
    long startTime = System.currentTimeMillis();
    List<RuleExecutionDetail> details = new ArrayList<>();

    try {
      boolean matched = evaluateGroup(rootGroup, context, details);
      long executionTime = System.currentTimeMillis() - startTime;

      log.debug(
          "Avaliação concluída em {}ms. Matched: {}. Detalhes: {}",
          executionTime,
          matched,
          details.size());

      return EvaluationResult.builder()
          .matched(matched)
          .executionDetails(details)
          .executionTimeMs(executionTime)
          .build();

    } catch (Exception e) {
      long executionTime = System.currentTimeMillis() - startTime;
      log.error("Erro na avaliação: {}", e.getMessage(), e);

      return EvaluationResult.builder()
          .matched(false)
          .executionDetails(details)
          .executionTimeMs(executionTime)
          .errorMessage(e.getMessage())
          .build();
    }
  }

  /**
   * Avalia um grupo de condições recursivamente.
   *
   * @param group O grupo de condições
   * @param context O contexto de avaliação
   * @param details Lista para coletar detalhes de execução
   * @return true se o grupo foi satisfeito
   */
  private boolean evaluateGroup(
      RuleConditionGroup group, EvaluationContext context, List<RuleExecutionDetail> details) {

    if (group == null) {
      return true;
    }

    // Delegar para o ConditionGroupEvaluator
    if (conditionGroupEvaluator != null) {
      return conditionGroupEvaluator.evaluateGroup(
          group, context, details, this::evaluateCondition);
    }

    // Fallback: avaliação inline
    return evaluateGroupInline(group, context, details);
  }

  /**
   * Avaliação inline de grupo (fallback).
   */
  private boolean evaluateGroupInline(
      RuleConditionGroup group, EvaluationContext context, List<RuleExecutionDetail> details) {

    GroupLogicOperator logicOp = group.getLogicOperator();
    boolean isAnd = logicOp == null || logicOp == GroupLogicOperator.AND;

    // Avaliar condições do grupo
    List<RuleCondition> conditions = group.getConditions();
    if (conditions != null && !conditions.isEmpty()) {
      for (RuleCondition condition : conditions) {
        if (!Boolean.TRUE.equals(condition.getEnabled())) {
          continue;
        }
        boolean result = evaluateCondition(condition, context, details);

        if (isAnd && !result) {
          return false; // AND: qualquer false retorna false
        }
        if (!isAnd && result) {
          return true; // OR: qualquer true retorna true
        }
      }
    }

    // Avaliar subgrupos (children)
    List<RuleConditionGroup> children = group.getChildren();
    if (children != null && !children.isEmpty()) {
      for (RuleConditionGroup child : children) {
        boolean result = evaluateGroup(child, context, details);

        if (isAnd && !result) {
          return false;
        }
        if (!isAnd && result) {
          return true;
        }
      }
    }

    // AND sem falhas = true, OR sem sucessos = false
    return isAnd;
  }

  /**
   * Avalia uma condição individual delegando para o registry de operadores.
   *
   * @param condition A condição a avaliar
   * @param context O contexto de avaliação
   * @param details Lista para coletar detalhes de execução
   * @return true se a condição foi satisfeita
   */
  public boolean evaluateCondition(
      RuleCondition condition, EvaluationContext context, List<RuleExecutionDetail> details) {

    long startTime = System.currentTimeMillis();
    ConditionOperator operator = condition.getOperator();
    String fieldName = condition.getFieldName();

    try {
      // Obter valor do campo do payload
      Object fieldValue = getFieldValue(fieldName, context);

      // Delegar avaliação para o registry
      boolean result = delegateToRegistry(operator, condition, context);

      // Registrar detalhe de execução
      long executionTime = System.currentTimeMillis() - startTime;
      details.add(createExecutionDetail(condition, context, fieldValue, result, executionTime, null));

      log.trace(
          "Condição avaliada: {} {} {} = {} ({}ms)",
          fieldName,
          operator,
          getExpectedValueString(condition),
          result,
          executionTime);

      return result;

    } catch (Exception e) {
      long executionTime = System.currentTimeMillis() - startTime;
      log.warn(
          "Erro ao avaliar condição {} {}: {}",
          fieldName,
          operator,
          e.getMessage());

      details.add(createExecutionDetail(condition, context, null, false, executionTime, e.getMessage()));
      return false;
    }
  }

  /**
   * Delega a avaliação do operador para o registry.
   *
   * @param operator O operador a avaliar
   * @param condition A condição completa
   * @param context O contexto de avaliação
   * @return O resultado da avaliação
   */
  private boolean delegateToRegistry(
      ConditionOperator operator,
      RuleCondition condition,
      EvaluationContext context) {

    try {
      OperatorEvaluator evaluator = operatorEvaluatorRegistry.getEvaluator(operator);

      log.debug(
          "Delegando operador {} para {}",
          operator,
          evaluator.getClass().getSimpleName());

        if (operatorMetricsService != null) {
        return operatorMetricsService.recordTimed(
          operator,
          () -> evaluator.evaluate(condition, context));
        }

        return evaluator.evaluate(condition, context);

    } catch (UnsupportedOperatorException e) {
      log.error("Operador não suportado no registry: {}", operator);
      throw e;

    } catch (Exception e) {
      log.error("Erro ao delegar operador {} para registry: {}", operator, e.getMessage());
      throw new UnsupportedOperatorException(
          operator, "Erro ao avaliar operador via registry: " + e.getMessage());
    }
  }

  /**
   * Obtém o valor de um campo do contexto.
   *
   * @param fieldName Nome do campo
   * @param context Contexto de avaliação
   * @return O valor do campo ou null
   */
  private Object getFieldValue(String fieldName, EvaluationContext context) {
    if (fieldName == null || context.getPayload() == null) {
      return null;
    }

    // Suporte a campos aninhados (ex: "card.bin")
    if (fieldName.contains(".")) {
      return getNestedFieldValue(fieldName, context.getPayload());
    }

    return context.getPayload().get(fieldName);
  }

  /**
   * Obtém valor de campo aninhado usando notação de ponto.
   */
  @SuppressWarnings("unchecked")
  private Object getNestedFieldValue(String fieldPath, Map<String, Object> payload) {
    String[] parts = fieldPath.split("\\.");
    Object current = payload;

    for (String part : parts) {
      if (current == null) {
        return null;
      }
      if (current instanceof Map) {
        current = ((Map<String, Object>) current).get(part);
      } else {
        return null;
      }
    }

    return current;
  }

  /**
   * Cria um detalhe de execução para auditoria.
   */
  private RuleExecutionDetail createExecutionDetail(
      RuleCondition condition,
      EvaluationContext context,
      Object actualValue,
      boolean matched,
      long executionTimeMs,
      String errorMessage) {

    UUID groupId = condition.getGroup() != null ? condition.getGroup().getId() : null;

    return RuleExecutionDetail.builder()
        .decisionLogId(context.getDecisionLogId())
        .ruleVersionId(context.getRuleVersionId())
        .conditionId(condition.getId())
        .groupId(groupId)
        .fieldName(condition.getFieldName())
        .fieldValue(actualValue != null ? String.valueOf(actualValue) : null)
        .operator(condition.getOperator().name())
        .expectedValue(getExpectedValueString(condition))
        .result(matched)
        .executionTimeMs((int) executionTimeMs)
        .errorMessage(errorMessage)
        .build();
  }

  /**
   * Formata o valor esperado da condição para exibição.
   */
  private String getExpectedValueString(RuleCondition condition) {
    return ExpectedValueFormatter.format(condition);
  }
}
