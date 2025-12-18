package com.rulex.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Regra disparada durante a avaliação. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TriggeredRuleDTO {

  private String name;

  /** Peso (0-100). */
  private Integer weight;

  /** Contribuição efetiva no score (0-100). */
  private Integer contribution;

  /** Texto explicativo opcional. */
  private String detail;
}
