package com.rulex.service.engine;

import com.rulex.dto.TransactionRequest;
import java.util.Set;

/**
 * ARCH-003 FIX: Interface unificada para avaliadores de regras.
 *
 * <p>Esta interface define o contrato para todos os engines de regras, permitindo consolidacao
 * futura dos 5 engines existentes: - ComplexRuleEvaluator - RuleEngineService -
 * DatabaseRuleExecutorService - AdvancedRuleEngineService - AstEvaluator
 *
 * <p>Uso do Strategy Pattern para permitir diferentes implementacoes.
 */
public interface RuleEvaluator {

  /**
   * Avalia uma transacao contra as regras suportadas.
   *
   * @param transaction Transacao a ser avaliada
   * @return Resultado da avaliacao
   */
  EvaluationResult evaluate(TransactionRequest transaction);

  /**
   * Retorna os tipos de regras suportados por este avaliador.
   *
   * @return Set de tipos de regras suportados
   */
  Set<RuleType> getSupportedRuleTypes();

  /**
   * Verifica se este avaliador suporta um tipo de regra.
   *
   * @param ruleType Tipo de regra
   * @return true se suportado
   */
  default boolean supports(RuleType ruleType) {
    return getSupportedRuleTypes().contains(ruleType);
  }

  /** Retorna o nome do avaliador para logging/metricas. */
  default String getName() {
    return getClass().getSimpleName();
  }

  /** Tipos de regras suportados pelo sistema. */
  enum RuleType {
    /** Regras simples com uma condicao */
    SIMPLE,
    /** Regras complexas com grupos aninhados */
    COMPLEX,
    /** Regras avancadas com AST */
    ADVANCED,
    /** Regras de velocidade */
    VELOCITY,
    /** Regras de grafo (Neo4j) */
    GRAPH,
    /** Regras estatisticas */
    STATISTICAL
  }

  /** Resultado da avaliacao de regras. */
  record EvaluationResult(
      boolean triggered,
      String decision,
      double riskScore,
      java.util.List<TriggeredRule> triggeredRules,
      long executionTimeMs,
      String evaluatorName) {
    public static EvaluationResult approved(String evaluatorName, long executionTimeMs) {
      return new EvaluationResult(
          false, "APPROVED", 0.0, java.util.List.of(), executionTimeMs, evaluatorName);
    }

    public static EvaluationResult declined(
        String evaluatorName,
        double riskScore,
        java.util.List<TriggeredRule> triggeredRules,
        long executionTimeMs) {
      return new EvaluationResult(
          true, "DECLINED", riskScore, triggeredRules, executionTimeMs, evaluatorName);
    }
  }

  /** Informacoes sobre uma regra disparada. */
  record TriggeredRule(String ruleId, String ruleName, String action, double score) {}
}
