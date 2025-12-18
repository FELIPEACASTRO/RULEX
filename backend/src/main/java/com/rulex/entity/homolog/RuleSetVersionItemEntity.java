package com.rulex.entity.homolog;

import jakarta.persistence.*;
import lombok.*;

import java.io.Serializable;
import java.util.UUID;

@Entity
@Table(name = "rule_set_version_items")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@IdClass(RuleSetVersionItemEntity.Pk.class)
public class RuleSetVersionItemEntity {

    @Id
    @Column(name = "rule_set_version_id", nullable = false)
    private UUID ruleSetVersionId;

    @Id
    @Column(name = "rule_version_id", nullable = false)
    private UUID ruleVersionId;

    @Column(name = "sort_order", nullable = false)
    private Integer sortOrder;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Pk implements Serializable {
        private static final long serialVersionUID = 1L;
        private UUID ruleSetVersionId;
        private UUID ruleVersionId;
    }
}
