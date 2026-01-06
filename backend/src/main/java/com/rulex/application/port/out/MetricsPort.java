package com.rulex.application.port.out;

import com.rulex.domain.model.Rule;
import java.util.Map;

/**
 * Port de métricas para monitoramento de performance.
 *
 * <p>Define como a camada de aplicação registra métricas. Implementação pode ser Micrometer,
 * Prometheus, etc.
 */
public interface MetricsPort {

  /**
   * Registra tempo de processamento de transação.
   *
   * @param processingTimeMs tempo em milissegundos
   * @param classification classificação final
   */
  void recordProcessingTime(long processingTimeMs, String classification);

  /**
   * Incrementa contador de transações processadas.
   *
   * @param classification classificação final
   */
  void incrementTransactionCount(String classification);

  /**
   * Registra regra acionada.
   *
   * @param ruleName nome da regra
   * @param ruleType tipo da regra
   */
  void recordRuleTriggered(String ruleName, String ruleType);

  /**
   * Registra regra shadow acionada (para A/B testing).
   *
   * @param ruleName nome da regra
   */
  void recordShadowRuleTriggered(String ruleName);

  /**
   * Incrementa contador de erros.
   *
   * @param errorType tipo do erro
   */
  void incrementErrorCount(String errorType);

  /**
   * Registra tamanho do cache de regras.
   *
   * @param size número de regras em cache
   */
  void recordCacheSize(int size);

  /**
   * Registra hit/miss de cache.
   *
   * @param hit true se cache hit
   */
  void recordCacheAccess(boolean hit);

  /**
   * Registra score de risco calculado.
   *
   * @param score score (0-100)
   */
  void recordRiskScore(int score);

  /**
   * Registra quantas regras foram avaliadas.
   *
   * @param count número de regras avaliadas
   */
  void recordRulesEvaluated(int count);

  /**
   * Registra métricas de uma avaliação completa.
   *
   * @param processingTimeMs tempo de processamento
   * @param classification classificação final
   * @param riskScore score de risco
   * @param rulesEvaluated número de regras avaliadas
   * @param rulesTriggered número de regras acionadas
   */
  default void recordEvaluation(
      long processingTimeMs,
      String classification,
      int riskScore,
      int rulesEvaluated,
      int rulesTriggered) {
    recordProcessingTime(processingTimeMs, classification);
    incrementTransactionCount(classification);
    recordRiskScore(riskScore);
    recordRulesEvaluated(rulesEvaluated);
  }
}
