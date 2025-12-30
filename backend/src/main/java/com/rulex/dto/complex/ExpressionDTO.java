package com.rulex.dto.complex;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para expressões calculadas que podem ser usadas como valores em condições. Exemplo:
 * "transactionAmount * 1.1" ou "field1 + field2"
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ExpressionDTO {

  private UUID id;

  @NotBlank(message = "Nome da expressão é obrigatório")
  private String name;

  @NotBlank(message = "Expressão é obrigatória")
  private String expression;

  @NotNull(message = "Tipo do resultado é obrigatório")
  private ConditionDTO.ValueType resultType;

  private String description;

  /**
   * Exemplos de expressões suportadas: - Aritméticas: "transactionAmount * 1.1", "field1 + field2"
   * - Funções: "ABS(transactionAmount)", "ROUND(value, 2)" - Condicionais: "IF(score > 50, 'HIGH',
   * 'LOW')" - Agregações: "SUM(transactions.amount)", "COUNT(transactions)" - Data/Tempo:
   * "DAYS_BETWEEN(transactionDate, NOW())"
   */
}
