package com.rulex.application.port.in;

import com.rulex.domain.model.Classification;
import com.rulex.domain.model.TransactionData;
import java.util.List;
import java.util.Map;

/**
 * Port de entrada para análise de transações.
 *
 * <p>Define o contrato para análise de fraude. Implementação em application/usecase.
 */
public interface AnalyzeTransactionUseCase {

  /**
   * Analisa uma transação para detecção de fraude.
   *
   * @param transactionData dados da transação
   * @return resultado da análise com classificação, score e regras acionadas
   */
  AnalysisResult analyze(TransactionData transactionData);

  /**
   * Analisa uma transação para detecção de fraude (forma legacy com comando).
   *
   * @param command comando com dados da transação
   * @return resultado da análise
   */
  default AnalysisResult analyze(AnalyzeCommand command) {
    TransactionData data =
        TransactionData.builder()
            .transactionId(command.externalTransactionId())
            .customerId((String) command.payload().get("customerId"))
            .amount(
                command.payload().get("amount") != null
                    ? new java.math.BigDecimal(command.payload().get("amount").toString())
                    : java.math.BigDecimal.ZERO)
            .timestamp(java.time.Instant.now())
            .data(command.payload())
            .build();
    return analyze(data);
  }

  /**
   * Comando para análise de transação (legacy).
   *
   * @param externalTransactionId identificador único da transação
   * @param payload dados brutos da transação
   */
  record AnalyzeCommand(String externalTransactionId, Map<String, Object> payload) {
    public AnalyzeCommand {
      if (externalTransactionId == null || externalTransactionId.isBlank()) {
        throw new IllegalArgumentException("externalTransactionId é obrigatório");
      }
      if (payload == null) {
        throw new IllegalArgumentException("payload é obrigatório");
      }
    }
  }

  /**
   * Resultado da análise de transação.
   *
   * @param classification classificação final (APPROVED, SUSPICIOUS, FRAUD)
   * @param totalScore score de risco total (0-100)
   * @param triggeredRules lista de regras acionadas
   * @param processingTimeMs tempo de processamento em millisegundos
   */
  record AnalysisResult(
      Classification classification,
      int totalScore,
      List<TriggeredRule> triggeredRules,
      long processingTimeMs) {}

  /**
   * Regra acionada durante a análise.
   *
   * @param ruleId identificador da regra
   * @param ruleName nome da regra
   * @param ruleType tipo da regra (SECURITY, VELOCITY, etc.)
   * @param score score adicionado por esta regra
   * @param isShadowMode se a regra estava em modo shadow
   * @param message mensagem descritiva
   */
  record TriggeredRule(
      String ruleId,
      String ruleName,
      String ruleType,
      int score,
      boolean isShadowMode,
      String message) {}
}
