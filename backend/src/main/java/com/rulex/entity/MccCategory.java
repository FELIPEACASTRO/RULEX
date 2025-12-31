package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

/**
 * Entidade para categorização de MCC (Merchant Category Code).
 * Permite identificar categoria e nível de risco do estabelecimento.
 */
@Entity
@Table(
    name = "mcc_category",
    indexes = {
        @Index(name = "idx_mcc_category_mcc", columnList = "mcc"),
        @Index(name = "idx_mcc_category_risk_level", columnList = "risk_level"),
        @Index(name = "idx_mcc_category_is_high_risk", columnList = "is_high_risk")
    }
)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class MccCategory {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, unique = true)
    private Integer mcc;

    @Column(nullable = false, length = 100)
    private String category;

    @Column(length = 100)
    private String subcategory;

    @Column(length = 500)
    private String description;

    @Column(name = "risk_level", length = 20)
    @Builder.Default
    private String riskLevel = "LOW";

    @Column(name = "is_high_risk")
    @Builder.Default
    private Boolean isHighRisk = false;

    @Column(name = "is_gambling")
    @Builder.Default
    private Boolean isGambling = false;

    @Column(name = "is_crypto")
    @Builder.Default
    private Boolean isCrypto = false;

    @Column(name = "is_adult")
    @Builder.Default
    private Boolean isAdult = false;

    @Column(name = "is_cash_advance")
    @Builder.Default
    private Boolean isCashAdvance = false;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private OffsetDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    private OffsetDateTime updatedAt;

    /**
     * Enum para níveis de risco.
     */
    public enum RiskLevel {
        LOW,
        MEDIUM,
        HIGH,
        CRITICAL
    }
}
