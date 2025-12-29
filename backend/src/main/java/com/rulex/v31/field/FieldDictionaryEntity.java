package com.rulex.v31.field;

import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "field_dictionary")
@Getter
@Setter
public class FieldDictionaryEntity {

  @Id private UUID id;

  @Column(name = "workflow")
  private String workflow;

  @Column(name = "record_type")
  private String recordType;

  @Column(name = "portfolio")
  private String portfolio;

  @Column(name = "json_path", nullable = false)
  private String jsonPath;

  @Column(name = "data_type", nullable = false)
  private String dataType;

  @Column(name = "domain_json", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String domainJson;

  @Column(name = "sentinel_values_json", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String sentinelValuesJson;

  @Column(name = "allowed_operators", columnDefinition = "text[]", nullable = false)
  private String[] allowedOperators;

  @Column(name = "allowed_functions", columnDefinition = "text[]", nullable = false)
  private String[] allowedFunctions;

  @Column(name = "requiredness_by_context", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String requirednessByContext;

  @Column(name = "security_constraints", columnDefinition = "jsonb")
  @JdbcTypeCode(SqlTypes.JSON)
  private String securityConstraints;

  @Column(name = "normalization_allowed", nullable = false)
  private boolean normalizationAllowed;

  @Column(name = "created_at", nullable = false)
  private OffsetDateTime createdAt;
}
