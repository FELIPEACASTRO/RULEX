package com.rulex.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO para resposta de análise de transação.
 * Inclui a classificação, score de risco, regras aplicadas e detalhes da decisão.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionResponse {

    /** ID externo da transação (canônico). */
    private String transactionId;

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

    private LocalDateTime timestamp;

    private Boolean success;

}
