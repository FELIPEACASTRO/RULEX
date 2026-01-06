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

  /** Operadores de comparação suportados */
  public enum ConditionOperator {
    // Comparação básica
    EQ,
    NEQ,
    GT,
    GTE,
    LT,
    LTE,
    // Listas
    IN,
    NOT_IN,
    // Strings
    CONTAINS,
    NOT_CONTAINS,
    STARTS_WITH,
    ENDS_WITH,
    REGEX,
    NOT_REGEX,
    // Nulos
    IS_NULL,
    NOT_NULL,
    // Booleanos
    IS_TRUE,
    IS_FALSE,
    // Range
    BETWEEN,
    NOT_BETWEEN,
    // Comparação entre campos
    FIELD_EQ,
    FIELD_NEQ,
    FIELD_GT,
    FIELD_GTE,
    FIELD_LT,
    FIELD_LTE,
    // Funções de data/tempo
    DATE_BEFORE,
    DATE_AFTER,
    DATE_BETWEEN,
    TIME_BEFORE,
    TIME_AFTER,
    TIME_BETWEEN,
    // Funções de lista/array
    ARRAY_CONTAINS,
    ARRAY_NOT_CONTAINS,
    ARRAY_SIZE_EQ,
    ARRAY_SIZE_GT,
    ARRAY_SIZE_LT,
    // Funções matemáticas
    MOD_EQ,
    MOD_NEQ,
    // Geolocalização
    GEO_DISTANCE_LT,
    GEO_DISTANCE_GT,
    GEO_IN_POLYGON,
    // Velocity (agregações temporais)
    VELOCITY_COUNT_GT,
    VELOCITY_COUNT_LT,
    VELOCITY_SUM_GT,
    VELOCITY_SUM_LT,
    VELOCITY_AVG_GT,
    VELOCITY_AVG_LT,
    VELOCITY_DISTINCT_GT,
    VELOCITY_DISTINCT_LT,
    // Agregações temporais avançadas (DSL expandida)
    SUM_LAST_N_DAYS,
    COUNT_LAST_N_HOURS,
    AVG_LAST_N_DAYS,
    COUNT_DISTINCT_MERCHANTS_LAST_N_DAYS,
    COUNT_DISTINCT_COUNTRIES_LAST_N_HOURS,
    MAX_AMOUNT_LAST_N_DAYS,
    MIN_AMOUNT_LAST_N_DAYS,
    // Operadores avançados de fraude (Triple Check V36)
    GT_FIELD_MULTIPLIER,       // Comparar com campo multiplicado (ex: amount > avgAmount * 5)
    DECIMAL_PLACES_GT,         // Verificar casas decimais (ex: amount tem mais de 2 casas)
    EXPIRES_WITHIN_DAYS,       // Verificar expiração (ex: cartão expira em N dias)
    IS_NEW,                    // Verificar se é novo (ex: deviceId é novo)
    IS_FIRST,                  // Verificar se é primeira ocorrência
    LT_CURRENT_DATE,           // Comparar com data atual
    GT_CURRENT_DATE,           // Comparar com data atual (futuro)
    NOT_IN_CUSTOMER_HISTORY,   // Verificar se não está no histórico do cliente
    IN_CUSTOMER_HISTORY,       // Verificar se está no histórico do cliente
    NOT_IN_CUSTOMER_USUAL_HOURS, // Verificar se não está no horário habitual
    IN_CUSTOMER_USUAL_HOURS,   // Verificar se está no horário habitual
    IN_CUSTOMER_CHARGEBACK_MERCHANTS, // Verificar se merchant teve chargeback
    PERCENTAGE_OF_FIELD,       // Calcular percentual de campo
    HOUR_BETWEEN,              // Verificar se hora está entre valores
    DAY_OF_WEEK_IN,            // Verificar dia da semana
    IS_WEEKEND,                // Verificar se é fim de semana
    IS_HOLIDAY,                // Verificar se é feriado
    DISTANCE_FROM_LAST_GT,     // Distância da última transação maior que
    TIME_SINCE_LAST_LT,        // Tempo desde última transação menor que
    COUNT_FAILURES_LAST_N_HOURS, // Contagem de falhas nas últimas N horas
    SUM_LAST_N_HOURS,          // Soma nas últimas N horas
    COUNT_DISTINCT_MERCHANTS_LAST_N_HOURS, // Merchants distintos nas últimas N horas
    VELOCITY_SPIKE,            // Spike de velocidade comparado com média
    AMOUNT_SPIKE,              // Spike de valor comparado com média
    PATTERN_ESCALATION,        // Padrão de escada (valores crescentes)
    PATTERN_ROUND_NUMBERS,     // Padrão de valores redondos
    PATTERN_SPLIT_TRANSACTION  // Padrão de split de transação
  }

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
