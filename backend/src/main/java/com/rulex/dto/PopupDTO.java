package com.rulex.dto;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Popup agregado: agrupa 1..N regras com um motivo resumido. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PopupDTO {

  /** Chave lógica do popup (ex.: FRAUD, SUSPICIOUS). */
  private String key;

  /** Título exibível (ex.: "Bloqueio", "Suspeita"). */
  private String title;

  /** Classificação final deste grupo. */
  private String classification;

  /** Soma das contribuições das regras dentro do popup. */
  private Integer totalContribution;

  /** Regras que compõem este popup. */
  private List<RuleHitDTO> rules;

  /** Motivo agregado (concatenação de detalhes/descrições). */
  private String reason;
}
