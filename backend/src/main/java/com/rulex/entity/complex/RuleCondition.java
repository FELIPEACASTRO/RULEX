package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/** Entidade para condições individuais com suporte a operadores avançados. */
@Entity
@Table(
    name = "rule_conditions",
    indexes = {
      @Index(name = "idx_conditions_group", columnList = "group_id"),
      @Index(name = "idx_conditions_field", columnList = "field_name")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleCondition {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "group_id", nullable = false)
  private RuleConditionGroup group;

  @Column(nullable = false)
  @Builder.Default
  private Integer position = 0;

  @Column(name = "field_name", nullable = false)
  private String fieldName;

  @Column(name = "field_path")
  private String fieldPath;

  @Enumerated(EnumType.STRING)
  @Column(nullable = false, columnDefinition = "condition_operator")
  private ConditionOperator operator;

  @Enumerated(EnumType.STRING)
  @Column(name = "value_type", nullable = false, columnDefinition = "condition_value_type")
  @Builder.Default
  private ConditionValueType valueType = ConditionValueType.STRING;

  @Column(name = "value_single")
  private String valueSingle;

  @Column(name = "value_array", columnDefinition = "TEXT[]")
  @JdbcTypeCode(SqlTypes.ARRAY)
  private List<String> valueArray;

  @Column(name = "value_min")
  private String valueMin;

  @Column(name = "value_max")
  private String valueMax;

  @Column(name = "value_field_ref")
  private String valueFieldRef;

  @Column(name = "value_expression")
  private String valueExpression;

  @Column(name = "case_sensitive", nullable = false)
  @Builder.Default
  private Boolean caseSensitive = true;

  @Column(nullable = false)
  @Builder.Default
  private Boolean negate = false;

  @Column(nullable = false)
  @Builder.Default
  private Boolean enabled = true;

  private String description;

  @Column(name = "error_message")
  private String errorMessage;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  /** Tipos de valor suportados */
  public enum ConditionValueType {
    STRING,
    NUMBER,
    BOOLEAN,
    DATE,
    TIME,
    DATETIME,
    ARRAY_STRING,
    ARRAY_NUMBER,
    FIELD_REFERENCE,
    EXPRESSION,
    GEO_POINT,
    GEO_POLYGON
  }
}
