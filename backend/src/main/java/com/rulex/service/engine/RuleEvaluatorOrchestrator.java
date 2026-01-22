package com.rulex.service.engine;

import com.rulex.dto.TransactionRequest;
import com.rulex.service.engine.RuleEvaluator.EvaluationResult;
import com.rulex.service.engine.RuleEvaluator.RuleType;
import com.rulex.service.engine.RuleEvaluator.TriggeredRule;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * ARCH-003 FIX: Orquestrador central para avaliadores de regras.
 *
 * <p>Coordena a execucao de multiplos avaliadores e consolida resultados. Permite adicionar novos
 * avaliadores sem modificar codigo existente.
 *
 * <p>Fluxo: 1. Recebe transacao 2. Determina quais avaliadores executar 3. Executa avaliadores em
 * paralelo ou sequencial 4. Consolida resultados 5. Retorna decisao final
 */
@Service
@Slf4j
public class RuleEvaluatorOrchestrator {

  private final Map<RuleType, RuleEvaluator> evaluators;

  public RuleEvaluatorOrchestrator(List<RuleEvaluator> evaluatorList) {
    this.evaluators = new EnumMap<>(RuleType.class);

    for (RuleEvaluator evaluator : evaluatorList) {
      for (RuleType type : evaluator.getSupportedRuleTypes()) {
        if (evaluators.containsKey(type)) {
          log.warn(
              "Tipo de regra {} ja tem avaliador registrado. Sobrescrevendo com {}",
              type,
              evaluator.getName());
        }
        evaluators.put(type, evaluator);
      }
    }

    log.info(
        "RuleEvaluatorOrchestrator inicializado com {} avaliadores para {} tipos de regra",
        evaluatorList.size(),
        evaluators.size());
  }

  /**
   * Avalia uma transacao usando todos os avaliadores registrados.
   *
   * @param transaction Transacao a avaliar
   * @return Resultado consolidado
   */
  public EvaluationResult evaluate(TransactionRequest transaction) {
    long startTime = System.currentTimeMillis();

    List<TriggeredRule> allTriggeredRules = new ArrayList<>();
    double maxRiskScore = 0.0;
    boolean anyTriggered = false;

    for (Map.Entry<RuleType, RuleEvaluator> entry : evaluators.entrySet()) {
      RuleType type = entry.getKey();
      RuleEvaluator evaluator = entry.getValue();

      try {
        log.debug("Executando avaliador {} para tipo {}", evaluator.getName(), type);
        EvaluationResult result = evaluator.evaluate(transaction);

        if (result.triggered()) {
          anyTriggered = true;
          allTriggeredRules.addAll(result.triggeredRules());
          maxRiskScore = Math.max(maxRiskScore, result.riskScore());
        }
      } catch (Exception e) {
        log.error(
            "Erro ao executar avaliador {} para tipo {}: {}",
            evaluator.getName(),
            type,
            e.getMessage());
        // Continua com outros avaliadores
      }
    }

    long executionTime = System.currentTimeMillis() - startTime;

    if (anyTriggered) {
      return EvaluationResult.declined(
          "Orchestrator", maxRiskScore, allTriggeredRules, executionTime);
    }

    return EvaluationResult.approved("Orchestrator", executionTime);
  }

  /**
   * Avalia uma transacao usando apenas um tipo especifico de regra.
   *
   * @param transaction Transacao a avaliar
   * @param ruleType Tipo de regra a usar
   * @return Resultado da avaliacao
   */
  public Optional<EvaluationResult> evaluate(TransactionRequest transaction, RuleType ruleType) {
    RuleEvaluator evaluator = evaluators.get(ruleType);
    if (evaluator == null) {
      log.warn("Nenhum avaliador registrado para tipo de regra: {}", ruleType);
      return Optional.empty();
    }

    return Optional.of(evaluator.evaluate(transaction));
  }

  /** Retorna estatisticas dos avaliadores registrados. */
  public Map<String, Object> getStats() {
    Map<String, Object> stats = new java.util.HashMap<>();
    stats.put("totalEvaluators", evaluators.size());
    stats.put("ruleTypes", evaluators.keySet());

    Map<String, List<RuleType>> byEvaluator = new java.util.HashMap<>();
    for (Map.Entry<RuleType, RuleEvaluator> entry : evaluators.entrySet()) {
      String name = entry.getValue().getName();
      byEvaluator.computeIfAbsent(name, k -> new ArrayList<>()).add(entry.getKey());
    }
    stats.put("byEvaluator", byEvaluator);

    return stats;
  }
}
