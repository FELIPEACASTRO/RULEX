package com.rulex.dto;

import java.time.LocalDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Resposta do endpoint /evaluate: decisão final + rule hits + popups agregados. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class EvaluateResponse {

  private String transactionId;
  private String classification;
  private Integer riskScore;
  private String reason;
  private String rulesetVersion;
  private Long processingTimeMs;
  private LocalDateTime timestamp;

  /** Regras que dispararam (enriquecidas com metadados). */
  private List<RuleHitDTO> ruleHits;

  /** Popups agregados com motivos (1 popup -> 1..N regras). */
  private List<PopupDTO> popups;
}
