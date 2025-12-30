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

/**
 * Entidade para ações que podem ser executadas quando uma regra é acionada.
 */
@Entity
@Table(name = "rule_actions",
    indexes = @Index(name = "idx_actions_rule_version", columnList = "rule_version_id")
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleAction {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(name = "rule_version_id", nullable = false)
    private UUID ruleVersionId;

    @Enumerated(EnumType.STRING)
    @Column(name = "action_type", nullable = false, columnDefinition = "rule_action_type")
    private ActionType actionType;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "action_config", nullable = false, columnDefinition = "JSONB")
    private Map<String, Object> actionConfig;

    @Column(nullable = false)
    @Builder.Default
    private Integer position = 0;

    @Column(name = "condition_group_id")
    private UUID conditionGroupId;

    @Column(nullable = false)
    @Builder.Default
    private Boolean enabled = true;

    private String description;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private OffsetDateTime createdAt;

    /**
     * Tipos de ações suportadas
     */
    public enum ActionType {
        SET_DECISION,
        SET_SCORE,
        ADD_TAG,
        REMOVE_TAG,
        SET_VARIABLE,
        CALL_WEBHOOK,
        SEND_NOTIFICATION,
        BLOCK_TRANSACTION,
        FLAG_FOR_REVIEW,
        ESCALATE
    }
}
