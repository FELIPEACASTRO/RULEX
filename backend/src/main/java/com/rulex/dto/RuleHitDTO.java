package com.rulex.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Detalhe de uma regra que impactou a decisão (rule hit). */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleHitDTO {

  private String ruleName;
  private String description;

  /** SECURITY, CONTEXT, VELOCITY, ANOMALY */
  private String ruleType;

  /** APPROVED, SUSPICIOUS, FRAUD */
  private String classification;

  private Integer threshold;

  /** Peso (0-100) configurado. */
  private Integer weight;

  /** Contribuição efetiva no score (0-100). */
  private Integer contribution;

  /** Explicação do match (ex.: "mcc == 7995 (actual=7995) => true"). */
  private String detail;
}
