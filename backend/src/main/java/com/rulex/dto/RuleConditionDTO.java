package com.rulex.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Condição genérica de uma regra (persistida como JSON). */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleConditionDTO {

  @NotBlank private String field;

  @NotBlank private String operator;

  /**
   * Valor bruto para comparação.
   *
   * <p>Observação: alguns operadores são unários (ex.: IS_NULL/IS_NOT_NULL/IS_TRUE/IS_FALSE) e não
   * exigem value; por compatibilidade com o payload do FE, aceitamos string vazia (mas não null).
   */
  @NotNull private String value;
}
