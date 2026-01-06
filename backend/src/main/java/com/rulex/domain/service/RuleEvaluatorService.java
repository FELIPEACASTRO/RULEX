package com.rulex.domain.service;

import com.rulex.domain.model.Classification;
import com.rulex.domain.model.Decision;
import com.rulex.domain.model.Decision.TriggeredRule;
import com.rulex.domain.model.Rule;
import com.rulex.domain.model.RuleCondition;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Domain Service para avaliação de regras de fraude.
 *
 * <p>Lógica pura de domínio sem dependências de framework. Coordena a avaliação de regras contra um
 * payload e calcula a decisão final.
 *
 * <p>Este serviço NÃO acessa banco de dados, cache ou serviços externos.
 */
public class RuleEvaluatorService {

  private final ConditionEvaluator conditionEvaluator;

  public RuleEvaluatorService() {
    this.conditionEvaluator = new ConditionEvaluator();
  }

  public RuleEvaluatorService(ConditionEvaluator conditionEvaluator) {
    this.conditionEvaluator = conditionEvaluator;
  }

  /**
   * Avalia uma lista de regras contra um payload e retorna a decisão.
   *
   * @param rules regras ativas a avaliar (em ordem)
   * @param payload dados da transação
   * @param externalTransactionId identificador da transação
   * @return decisão com classificação, score e regras acionadas
   */
  public Decision evaluate(
      List<Rule> rules, Map<String, Object> payload, String externalTransactionId) {

    long startTime = System.currentTimeMillis();

    if (rules == null || rules.isEmpty()) {
      return Decision.approved(externalTransactionId, System.currentTimeMillis() - startTime);
    }

    List<TriggeredRule> triggeredRules = new ArrayList<>();
    int totalScore = 0;
    Classification maxClassification = Classification.APPROVED;

    for (Rule rule : rules) {
      if (!rule.isActive()) {
        continue; // Pula regras desabilitadas ou em shadow mode
      }

      RuleEvaluationResult result = evaluateRule(rule, payload);

      if (result.triggered()) {
        int contribution = clampScore(rule.getWeight());
        totalScore += contribution;

        triggeredRules.add(
            TriggeredRule.builder()
                .ruleName(rule.getName())
                .ruleType(rule.getType() != null ? rule.getType().name() : "UNKNOWN")
                .weight(rule.getWeight())
                .contribution(contribution)
                .detail(result.explanation())
                .classification(rule.getClassification())
                .build());

        maxClassification = Classification.mostSevere(maxClassification, rule.getClassification());

        // Short-circuit em FRAUD
        if (maxClassification == Classification.FRAUD) {
          break;
        }
      }
    }

    // Normaliza score para 0-100
    totalScore = Math.min(totalScore, 100);

    long processingTime = System.currentTimeMillis() - startTime;

    return Decision.builder()
        .externalTransactionId(externalTransactionId)
        .classification(maxClassification)
        .riskScore(totalScore)
        .triggeredRules(triggeredRules)
        .reason(generateReason(maxClassification, triggeredRules))
        .processingTimeMs(processingTime)
        .createdAt(LocalDateTime.now())
        .build();
  }

  /**
   * Avalia uma única regra contra o payload.
   *
   * @param rule a regra a avaliar
   * @param payload dados da transação
   * @return resultado da avaliação
   */
  public RuleEvaluationResult evaluateRule(Rule rule, Map<String, Object> payload) {
    if (rule == null || rule.getConditions() == null || rule.getConditions().isEmpty()) {
      return new RuleEvaluationResult(false, "Regra sem condições definidas");
    }

    List<String> explanations = new ArrayList<>();
    Rule.LogicOperator op =
        rule.getLogicOperator() != null ? rule.getLogicOperator() : Rule.LogicOperator.AND;

    boolean triggered = (op == Rule.LogicOperator.AND);

    for (RuleCondition condition : rule.getConditions()) {
      ConditionEvaluator.EvaluationResult evalResult =
          conditionEvaluator.evaluateWithExplanation(condition, payload);

      explanations.add(evalResult.explanation());

      if (op == Rule.LogicOperator.AND) {
        triggered = triggered && evalResult.triggered();
        if (!triggered) {
          break; // AND: early exit
        }
      } else {
        triggered = triggered || evalResult.triggered();
        if (triggered) {
          break; // OR: early exit
        }
      }
    }

    return new RuleEvaluationResult(triggered, String.join(" | ", explanations));
  }

  /** Resultado de avaliação de regra */
  public record RuleEvaluationResult(boolean triggered, String explanation) {}

  /**
   * Avalia regras em modo shadow (não afeta decisão, apenas coleta métricas).
   *
   * @param shadowRules regras em shadow mode
   * @param payload dados da transação
   * @return lista de resultados shadow
   */
  public List<ShadowResult> evaluateShadow(List<Rule> shadowRules, Map<String, Object> payload) {
    List<ShadowResult> results = new ArrayList<>();

    for (Rule rule : shadowRules) {
      if (!rule.isShadowOnly() && !rule.isCanary()) {
        continue;
      }

      RuleEvaluationResult result = evaluateRule(rule, payload);
      results.add(
          new ShadowResult(rule.getName(), result.triggered(), result.explanation(), rule.getClassification()));
    }

    return results;
  }

  /** Resultado de avaliação shadow */
  public record ShadowResult(
      String ruleName, boolean triggered, String explanation, Classification classification) {}

  // ========== Utilitários ==========

  private int clampScore(Integer weight) {
    if (weight == null) return 0;
    return Math.max(0, Math.min(100, weight));
  }

  private String generateReason(
      Classification classification, List<TriggeredRule> triggeredRules) {
    List<String> ruleNames = triggeredRules.stream().map(TriggeredRule::getRuleName).toList();

    return switch (classification) {
      case APPROVED -> "Transação aprovada. Nenhuma regra crítica foi acionada.";
      case SUSPICIOUS ->
          String.format(
              "Transação suspeita. Regras acionadas: %s", String.join(", ", ruleNames));
      case FRAUD ->
          String.format(
              "Transação bloqueada como fraude. Regras acionadas: %s",
              String.join(", ", ruleNames));
    };
  }
}
