package com.rulex.dto.complex;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para regras complexas com suporte completo a grupos aninhados, expressões calculadas,
 * variáveis de contexto e ações.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ComplexRuleDTO {

  private UUID id;

  @NotBlank(message = "Chave da regra é obrigatória")
  private String key;

  @NotBlank(message = "Título da regra é obrigatório")
  private String title;

  private String description;

  private Integer version;

  @NotNull(message = "Status é obrigatório")
  private RuleStatusType status;

  @Min(value = 0, message = "Prioridade deve ser >= 0")
  @Max(value = 1000, message = "Prioridade deve ser <= 1000")
  private Integer priority;

  @Min(value = 0, message = "Severidade deve ser >= 0")
  @Max(value = 100, message = "Severidade deve ser <= 100")
  private Integer severity;

  @NotNull(message = "Decisão é obrigatória")
  private DecisionType decision;

  private String reasonTemplate;

  private Boolean enabled;

  /** Grupo raiz de condições (contém toda a lógica da regra) */
  @Valid
  @NotNull(message = "Grupo de condições é obrigatório")
  private ConditionGroupDTO rootConditionGroup;

  /** Expressões calculadas disponíveis para uso nas condições */
  @Valid private List<ExpressionDTO> expressions;

  /** Variáveis de contexto */
  @Valid private List<ContextVariableDTO> contextVariables;

  /** Ações a serem executadas quando a regra é acionada */
  @Valid private List<RuleActionDTO> actions;

  /** Tags para categorização */
  private List<String> tags;

  /** Campos utilizados pela regra (calculado automaticamente) */
  private List<String> fieldsUsed;

  private UUID createdBy;

  private OffsetDateTime createdAt;

  private OffsetDateTime updatedAt;

  /** Status da regra */
  public enum RuleStatusType {
    DRAFT, // Rascunho - não é executada
    PUBLISHED, // Publicada - em execução
    DEPRECATED, // Descontinuada - mantida para histórico
    ARCHIVED, // Arquivada - não é executada
    TESTING // Em teste - executa mas não afeta decisão final
  }

  /** Tipos de decisão */
  public enum DecisionType {
    APROVADO("Transação aprovada"),
    SUSPEITA_DE_FRAUDE("Transação suspeita - requer análise"),
    FRAUDE("Transação fraudulenta - bloquear");

    private final String description;

    DecisionType(String description) {
      this.description = description;
    }

    public String getDescription() {
      return description;
    }
  }
}
