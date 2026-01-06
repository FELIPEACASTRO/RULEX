package com.rulex.interfaces.rest.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.time.Instant;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Response DTO for transaction analysis.
 *
 * <p>Contains the fraud classification decision, risk score, and details about which rules were
 * triggered.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class AnalyzeTransactionResponse {

  /** Unique decision identifier. */
  private String decisionId;

  /** External transaction identifier (echoed from request). */
  private String transactionId;

  /** Final fraud classification: APPROVED, SUSPICIOUS, FRAUD. */
  private String classification;

  /** Risk score from 0 to 100. */
  private Integer riskScore;

  /** Human-readable reason for the decision. */
  private String reason;

  /** List of rules that triggered. */
  private List<TriggeredRuleInfo> triggeredRules;

  /** Processing time in milliseconds. */
  private Long processingTimeMs;

  /** Decision timestamp. */
  private Instant timestamp;

  /** Whether any shadow mode rules were evaluated. */
  private Boolean hasShadowModeRules;

  /** Detailed rule information. */
  @Data
  @Builder
  @NoArgsConstructor
  @AllArgsConstructor
  public static class TriggeredRuleInfo {
    private String ruleId;
    private String ruleName;
    private String ruleType;
    private Integer score;
    private Boolean isShadowMode;
    private String message;
  }
}
