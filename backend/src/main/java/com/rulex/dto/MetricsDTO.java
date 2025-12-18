package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** DTO para Métricas do Sistema. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MetricsDTO {

  @JsonProperty("totalTransactions")
  private Long totalTransactions;

  @JsonProperty("approvedTransactions")
  private Long approvedTransactions;

  @JsonProperty("suspiciousTransactions")
  private Long suspiciousTransactions;

  @JsonProperty("fraudTransactions")
  private Long fraudTransactions;

  @JsonProperty("approvalRate")
  private BigDecimal approvalRate; // Percentual

  @JsonProperty("fraudRate")
  private BigDecimal fraudRate; // Percentual

  @JsonProperty("suspiciousRate")
  private BigDecimal suspiciousRate; // Percentual

  @JsonProperty("totalVolume")
  private BigDecimal totalVolume;

  @JsonProperty("averageTransactionAmount")
  private BigDecimal averageTransactionAmount;

  @JsonProperty("highestTransactionAmount")
  private BigDecimal highestTransactionAmount;

  @JsonProperty("lowestTransactionAmount")
  private BigDecimal lowestTransactionAmount;

  @JsonProperty("period")
  private String period; // "24h", "7d", "30d", etc.

  @JsonProperty("timestamp")
  private LocalDateTime timestamp;

  @JsonProperty("mccDistribution")
  private Map<Integer, Long> mccDistribution;

  @JsonProperty("merchantDistribution")
  private Map<String, Long> merchantDistribution;
}
