package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.Map;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Entidade para variáveis de contexto que podem ser derivadas do payload ou calculadas. */
@Entity
@Table(
    name = "rule_context_variables",
    indexes = @Index(name = "idx_context_vars_rule_version", columnList = "rule_version_id"),
    uniqueConstraints = @UniqueConstraint(columnNames = {"rule_version_id", "name"}))
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleContextVariable {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "rule_version_id", nullable = false)
  private UUID ruleVersionId;

  @Column(nullable = false)
  private String name;

  @Enumerated(EnumType.STRING)
  @Column(name = "source_type", nullable = false)
  private SourceType sourceType;

  @JdbcTypeCode(SqlTypes.JSON)
  @Column(name = "source_config", nullable = false, columnDefinition = "JSONB")
  private Map<String, Object> sourceConfig;

  @Column(name = "default_value")
  private String defaultValue;

  private String description;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

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
