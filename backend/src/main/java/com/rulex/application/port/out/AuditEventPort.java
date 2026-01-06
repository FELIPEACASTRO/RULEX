package com.rulex.application.port.out;

import java.util.Map;

/**
 * Port de auditoria para rastrear ações de análise de fraude.
 *
 * <p>Define como a camada de aplicação registra eventos de auditoria. Implementação pode ser banco,
 * Kafka, etc.
 */
public interface AuditEventPort {

  /** Tipos de eventos de auditoria */
  enum EventType {
    TRANSACTION_ANALYZED,
    TRANSACTION_TAMPER_DETECTED,
    RULE_TRIGGERED,
    DECISION_CREATED,
    RULE_CREATED,
    RULE_UPDATED,
    RULE_DELETED,
    RULE_ENABLED,
    RULE_DISABLED
  }

  /**
   * Registra evento de auditoria.
   *
   * @param eventType tipo do evento
   * @param entityType tipo da entidade (TRANSACTION, RULE, DECISION)
   * @param entityId identificador da entidade
   * @param actor ator que executou a ação (user ou system)
   * @param details detalhes adicionais
   */
  void logEvent(
      EventType eventType,
      String entityType,
      String entityId,
      String actor,
      Map<String, Object> details);

  /**
   * Registra evento de análise de transação.
   *
   * @param externalTransactionId ID externo
   * @param classification classificação final
   * @param riskScore score de risco
   * @param processingTimeMs tempo de processamento
   */
  default void logTransactionAnalyzed(
      String externalTransactionId, String classification, int riskScore, long processingTimeMs) {
    logEvent(
        EventType.TRANSACTION_ANALYZED,
        "TRANSACTION",
        externalTransactionId,
        "SYSTEM",
        Map.of(
            "classification", classification,
            "riskScore", riskScore,
            "processingTimeMs", processingTimeMs));
  }

  /**
   * Registra detecção de tentativa de adulteração.
   *
   * @param externalTransactionId ID externo
   * @param originalHash hash original
   * @param receivedHash hash recebido
   */
  default void logTamperDetected(
      String externalTransactionId, String originalHash, String receivedHash) {
    logEvent(
        EventType.TRANSACTION_TAMPER_DETECTED,
        "TRANSACTION",
        externalTransactionId,
        "SYSTEM",
        Map.of(
            "originalHash", originalHash,
            "receivedHash", receivedHash,
            "severity", "CRITICAL"));
  }
}
