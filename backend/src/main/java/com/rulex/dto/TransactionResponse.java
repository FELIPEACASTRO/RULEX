package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

/**
 * DTO para resposta de análise de transação.
 * Inclui a classificação, score de risco, regras aplicadas e detalhes da decisão.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionResponse {

    @JsonProperty("transactionId")
    private String externalTransactionId;

    @JsonProperty("classification")
    private String classification; // APPROVED, SUSPICIOUS, FRAUD

    @JsonProperty("riskScore")
    private Integer riskScore; // 0-100

    @JsonProperty("rulesApplied")
    private List<String> rulesApplied;

    @JsonProperty("scoreDetails")
    private Map<String, Object> scoreDetails;

    @JsonProperty("reason")
    private String reason;

    @JsonProperty("rulesVersion")
    private String rulesVersion;

    @JsonProperty("processingTime")
    private Long processingTime; // em milissegundos

    @JsonProperty("timestamp")
    private LocalDateTime timestamp;

    @JsonProperty("success")
    private Boolean success;

}
