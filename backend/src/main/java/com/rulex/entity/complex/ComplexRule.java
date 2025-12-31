package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade principal para regras complexas.
 * Armazena metadados da regra, enquanto as condições são armazenadas em RuleConditionGroup.
 */
@Entity
@Table(name = "complex_rules", indexes = {
    @Index(name = "idx_complex_rules_key", columnList = "key", unique = true),
    @Index(name = "idx_complex_rules_status", columnList = "status"),
    @Index(name = "idx_complex_rules_enabled", columnList = "enabled")
})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ComplexRule {

    @Id
    @Column(name = "id", nullable = false, updatable = false)
    private UUID id;

    @Column(name = "key", nullable = false, unique = true, length = 100)
    private String key;

    @Column(name = "title", nullable = false, length = 255)
    private String title;

    @Column(name = "description", columnDefinition = "TEXT")
    private String description;

    @Column(name = "version", nullable = false)
    private Integer version;

    @Column(name = "status", nullable = false, length = 20)
    private String status;

    @Column(name = "priority", nullable = false)
    private Integer priority;

    @Column(name = "severity", nullable = false)
    private Integer severity;

    @Column(name = "decision", nullable = false, length = 50)
    private String decision;

    @Column(name = "reason_template", columnDefinition = "TEXT")
    private String reasonTemplate;

    @Column(name = "enabled", nullable = false)
    private Boolean enabled;

    @Column(name = "created_by")
    private UUID createdBy;

    @Column(name = "created_at", nullable = false)
    private OffsetDateTime createdAt;

    @Column(name = "updated_at", nullable = false)
    private OffsetDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        if (id == null) {
            id = UUID.randomUUID();
        }
        if (createdAt == null) {
            createdAt = OffsetDateTime.now();
        }
        if (updatedAt == null) {
            updatedAt = OffsetDateTime.now();
        }
        if (version == null) {
            version = 1;
        }
        if (status == null) {
            status = "DRAFT";
        }
        if (priority == null) {
            priority = 0;
        }
        if (severity == null) {
            severity = 0;
        }
        if (decision == null) {
            decision = "APROVADO";
        }
        if (enabled == null) {
            enabled = false;
        }
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = OffsetDateTime.now();
    }
}
