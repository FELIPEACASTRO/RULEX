package com.rulex.dto;

import com.rulex.dto.complex.*;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import java.time.OffsetDateTime;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** DTO para exportação e importação de regras. Suporta tanto regras simples quanto complexas. */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleExportDTO {

  /** Metadados do export */
  private ExportMetadata metadata;

  /** Lista de regras exportadas */
  @Valid private List<RuleData> rules;

  /** Metadados do arquivo de export */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class ExportMetadata {
    private String version;
    private OffsetDateTime exportedAt;
    private String exportedBy;
    private String description;
    private Integer totalRules;
    private String sourceSystem;
  }

  /** Dados completos de uma regra */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class RuleData {

    // Identificação
    @NotBlank(message = "Chave da regra é obrigatória")
    private String key;

    @NotBlank(message = "Título da regra é obrigatório")
    private String title;

    private String description;

    // Configuração básica
    private Integer version;
    private String status;
    private Integer priority;
    private Integer severity;
    private String decision;
    private String reasonTemplate;
    private Boolean enabled;

    // Tipo de regra
    private RuleType ruleType;

    // Para regras simples (condições lineares)
    private SimpleRuleConfig simpleConfig;

    // Para regras complexas (grupos aninhados)
    private ComplexRuleConfig complexConfig;

    // Campos utilizados (calculado automaticamente)
    private List<String> fieldsUsed;

    // Tags para categorização
    private List<String> tags;

    // Metadados
    private OffsetDateTime createdAt;
    private String createdBy;
  }

  /** Tipo de regra */
  public enum RuleType {
    SIMPLE, // Condições lineares com AND/OR
    COMPLEX // Grupos aninhados com operadores avançados
  }

  /** Configuração para regras simples */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class SimpleRuleConfig {
    private String logicOperator; // AND ou OR
    private List<SimpleCondition> conditions;
    private Integer weight;
    private String classification;
  }

  /** Condição simples */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class SimpleCondition {
    private String field;
    private String operator;
    private String value;
    private List<String> values; // Para IN/NOT_IN
  }

  /** Configuração para regras complexas */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class ComplexRuleConfig {

    /** Grupo raiz de condições */
    @Valid private ConditionGroupDTO rootConditionGroup;

    /** Expressões calculadas */
    @Valid private List<ExpressionDTO> expressions;

    /** Variáveis de contexto */
    @Valid private List<ContextVariableDTO> contextVariables;

    /** Ações a serem executadas */
    @Valid private List<RuleActionDTO> actions;
  }

  /** Resultado da importação */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class ImportResult {
    private Integer totalProcessed;
    private Integer successCount;
    private Integer failureCount;
    private Integer skippedCount;
    private List<ImportError> errors;
    private List<String> importedRuleKeys;
    private OffsetDateTime importedAt;
  }

  /** Erro de importação */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class ImportError {
    private String ruleKey;
    private String errorType;
    private String message;
    private String field;
  }

  /** Opções de importação */
  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  @Builder
  public static class ImportOptions {
    /** Se true, sobrescreve regras existentes */
    private Boolean overwriteExisting;

    /** Se true, valida sem importar */
    private Boolean dryRun;

    /** Se true, importa regras desabilitadas */
    private Boolean importDisabled;

    /** Prefixo para adicionar às chaves das regras */
    private String keyPrefix;

    /** Sufixo para adicionar às chaves das regras */
    private String keySuffix;
  }
}
