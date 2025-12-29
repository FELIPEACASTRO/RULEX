package com.rulex.dto;

import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para resposta de análise de transação. Inclui a classificação, score de risco, regras
 * aplicadas e detalhes da decisão.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionResponse {

  /** ID interno (DB). Opcional. */
  private Long id;

  /** ID externo da transação (canônico). */
  private String transactionId;

  /** Campos de contexto (para UI/listagem). Omitidos quando não disponíveis. */
  private String customerIdFromHeader;

  private String merchantId;

  private String merchantName;

  private BigDecimal transactionAmount;

  /** YYYYMMDD */
  private Integer transactionDate;

  /** HHMMSS */
  private Integer transactionTime;

  /** APPROVED, SUSPICIOUS, FRAUD */
  private String classification;

  /** 0-100 */
  private Integer riskScore;

  /** Detalhamento das regras que dispararam. */
  private List<TriggeredRuleDTO> triggeredRules;

  /** Texto explicativo (sintético). */
  private String reason;

  /** Versão do conjunto de regras usado na decisão. */
  private String rulesetVersion;

  /** Tempo de processamento em ms. */
  private Long processingTimeMs;

  private OffsetDateTime timestamp;

  private Boolean success;
}
