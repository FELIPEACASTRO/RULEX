package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

/**
 * Entidade para expressões calculadas que podem ser usadas como valores em condições.
 * Exemplo: "transactionAmount * 1.1" ou "field1 + field2"
 */
@Entity
@Table(name = "rule_expressions", 
    indexes = @Index(name = "idx_expressions_rule_version", columnList = "rule_version_id"),
    uniqueConstraints = @UniqueConstraint(columnNames = {"rule_version_id", "name"})
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleExpression {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private UUID id;

    @Column(name = "rule_version_id", nullable = false)
    private UUID ruleVersionId;

    @Column(nullable = false)
    private String name;

    @Column(nullable = false, columnDefinition = "TEXT")
    private String expression;

    @Enumerated(EnumType.STRING)
    @Column(name = "result_type", nullable = false, columnDefinition = "condition_value_type")
    @Builder.Default
    private RuleCondition.ConditionValueType resultType = RuleCondition.ConditionValueType.NUMBER;

    private String description;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private OffsetDateTime createdAt;

    /**
     * Exemplos de expressões suportadas:
     * - Aritméticas: "transactionAmount * 1.1", "field1 + field2"
     * - Funções: "ABS(transactionAmount)", "ROUND(value, 2)"
     * - Condicionais: "IF(score > 50, 'HIGH', 'LOW')"
     * - Agregações: "SUM(transactions.amount)", "COUNT(transactions)"
     * - Data/Tempo: "DAYS_BETWEEN(transactionDate, NOW())"
     */
}
