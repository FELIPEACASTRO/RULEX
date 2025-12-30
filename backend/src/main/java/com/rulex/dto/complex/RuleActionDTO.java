package com.rulex.dto.complex;

import jakarta.validation.constraints.NotNull;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** DTO para ações que podem ser executadas quando uma regra é acionada. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleActionDTO {

  private UUID id;

  @NotNull(message = "Tipo da ação é obrigatório")
  private ActionType actionType;

  /** Configuração específica da ação */
  @NotNull(message = "Configuração da ação é obrigatória")
  private Map<String, Object> actionConfig;

  private Integer position;

  /**
   * ID do grupo de condições que deve ser verdadeiro para executar esta ação Se null, a ação é
   * executada sempre que a regra é acionada
   */
  private UUID conditionGroupId;

  private Boolean enabled;

  private String description;

  /** Tipos de ações suportadas */
  public enum ActionType {
    /** Define a decisão final da transação Config: { "decision": "FRAUDE" } */
    SET_DECISION,

    /** Define ou modifica o score de risco Config: { "score": 85 } ou { "scoreModifier": "+10" } */
    SET_SCORE,

    /** Adiciona uma tag à transação Config: { "tag": "HIGH_RISK" } */
    ADD_TAG,

    /** Remove uma tag da transação Config: { "tag": "TRUSTED" } */
    REMOVE_TAG,

    /**
     * Define uma variável para uso em outras regras Config: { "variable": "customScore", "value":
     * "85" }
     */
    SET_VARIABLE,

    /**
     * Chama um webhook externo Config: { "url": "https://api.example.com/alert", "method": "POST",
     * "payload": {...} }
     */
    CALL_WEBHOOK,

    /**
     * Envia uma notificação Config: { "channel": "email", "template": "fraud_alert", "recipients":
     * ["analyst@company.com"] }
     */
    SEND_NOTIFICATION,

    /** Bloqueia a transação imediatamente Config: { "reason": "Suspected fraud" } */
    BLOCK_TRANSACTION,

    /**
     * Marca a transação para revisão manual Config: { "queue": "fraud_review", "priority": "high" }
     */
    FLAG_FOR_REVIEW,

    /**
     * Escala para um nível superior de análise Config: { "level": "senior_analyst", "reason":
     * "Complex case" }
     */
    ESCALATE,

    /** Executa uma regra específica Config: { "ruleKey": "ADDITIONAL_CHECK" } */
    EXECUTE_RULE,

    /** Para a execução das demais regras Config: { "reason": "Critical rule matched" } */
    STOP_PROCESSING,

    /**
     * Registra um log customizado Config: { "level": "WARN", "message": "Suspicious pattern
     * detected" }
     */
    LOG_CUSTOM
  }
}
