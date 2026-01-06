package com.rulex.application.port.in;

import com.rulex.domain.model.Decision;
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
   * @param command comando com dados da transação
   * @return decisão com classificação, score e regras acionadas
   */
  Decision analyze(AnalyzeCommand command);

  /**
   * Comando para análise de transação.
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
}
