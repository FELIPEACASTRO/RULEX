package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para requisições de avaliação de transações.
 * Aceita um payload JSON flexível para avaliação de regras.
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class EvaluateRequestDTO {

  /**
   * Payload da transação a ser avaliada.
   * Deve conter os campos necessários para avaliação das regras configuradas.
   */
  @NotNull(message = "O payload da transação é obrigatório")
  @Size(min = 1, message = "O payload não pode estar vazio")
  private Map<String, Object> payload;

  /**
   * ID externo da transação (opcional).
   * Usado para rastreabilidade e deduplicação.
   */
  @Size(max = 100, message = "O ID externo não pode exceder 100 caracteres")
  private String externalTransactionId;

  /**
   * Versão do ruleset a ser usado (opcional).
   * Se não especificado, usa a versão ativa.
   */
  @Size(max = 50, message = "A versão do ruleset não pode exceder 50 caracteres")
  private String rulesetVersion;

  /**
   * Flag para modo de simulação (opcional).
   * Se true, não persiste a transação.
   */
  private Boolean simulationMode;
}
