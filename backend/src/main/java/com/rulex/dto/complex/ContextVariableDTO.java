package com.rulex.dto.complex;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** DTO para variáveis de contexto que podem ser derivadas do payload ou calculadas. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContextVariableDTO {

  private UUID id;

  @NotBlank(message = "Nome da variável é obrigatório")
  private String name;

  @NotNull(message = "Tipo da fonte é obrigatório")
  private SourceType sourceType;

  /** Configuração específica do tipo de fonte */
  @NotNull(message = "Configuração da fonte é obrigatória")
  private Map<String, Object> sourceConfig;

  private String defaultValue;

  private String description;

  /** Tipos de fonte para variáveis de contexto */
  public enum SourceType {
    /** Valor direto do payload da transação */
    PAYLOAD,

    /** Resultado de uma expressão calculada */
    EXPRESSION,

    /**
     * Lookup em tabela externa ou cache Config: { "table": "blacklist", "key": "pan", "field":
     * "risk_level" }
     */
    LOOKUP,

    /**
     * Agregação de dados históricos Config: { "metric": "count", "field": "transactionAmount",
     * "period": "24h", "groupBy": "pan" }
     */
    AGGREGATION,

    /**
     * Chamada a serviço externo Config: { "url": "https://api.example.com/score", "method": "POST"
     * }
     */
    EXTERNAL_SERVICE,

    /** Valor de outra regra que já foi executada */
    RULE_RESULT
  }
}
