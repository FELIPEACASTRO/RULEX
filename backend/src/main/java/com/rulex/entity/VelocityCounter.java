package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para contadores de velocidade.
 * Armazena agregações pré-computadas por janela temporal.
 */
@Entity
@Table(name = "velocity_counters")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VelocityCounter {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "key_type", nullable = false, length = 50)
    private String keyType;

    @Column(name = "key_value", nullable = false, length = 255)
    private String keyValue;

    @Column(name = "window_type", nullable = false, length = 20)
    private String windowType;

    @Column(name = "window_start", nullable = false)
    private OffsetDateTime windowStart;

    @Column(name = "window_end", nullable = false)
    private OffsetDateTime windowEnd;

    @Column(name = "transaction_count")
    private Integer transactionCount;

    @Column(name = "total_amount", precision = 18, scale = 2)
    private BigDecimal totalAmount;

    @Column(name = "avg_amount", precision = 18, scale = 2)
    private BigDecimal avgAmount;

    @Column(name = "min_amount", precision = 18, scale = 2)
    private BigDecimal minAmount;

    @Column(name = "max_amount", precision = 18, scale = 2)
    private BigDecimal maxAmount;

    @Column(name = "approved_count")
    private Integer approvedCount;

    @Column(name = "suspicious_count")
    private Integer suspiciousCount;

    @Column(name = "fraud_count")
    private Integer fraudCount;

    @Column(name = "distinct_merchants")
    private Integer distinctMerchants;

    @Column(name = "distinct_mccs")
    private Integer distinctMccs;

    @Column(name = "distinct_countries")
    private Integer distinctCountries;

    @Column(name = "last_transaction_id", length = 64)
    private String lastTransactionId;

    @Column(name = "last_transaction_at")
    private OffsetDateTime lastTransactionAt;

    @Column(name = "created_at")
    private OffsetDateTime createdAt;

    @Column(name = "updated_at")
    private OffsetDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = OffsetDateTime.now();
        updatedAt = OffsetDateTime.now();
        if (transactionCount == null) transactionCount = 0;
        if (totalAmount == null) totalAmount = BigDecimal.ZERO;
        if (avgAmount == null) avgAmount = BigDecimal.ZERO;
        if (approvedCount == null) approvedCount = 0;
        if (suspiciousCount == null) suspiciousCount = 0;
        if (fraudCount == null) fraudCount = 0;
        if (distinctMerchants == null) distinctMerchants = 0;
        if (distinctMccs == null) distinctMccs = 0;
        if (distinctCountries == null) distinctCountries = 0;
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = OffsetDateTime.now();
    }

    /**
     * Tipos de chave suportados.
     */
    public enum KeyType {
        PAN,
        CUSTOMER_ID,
        MERCHANT_ID,
        PAN_MCC,
        PAN_MERCHANT,
        CUSTOMER_MERCHANT
    }

    /**
     * Tipos de janela temporal.
     */
    public enum WindowType {
        MINUTE_5(5),
        MINUTE_15(15),
        MINUTE_30(30),
        HOUR_1(60),
        HOUR_6(360),
        HOUR_12(720),
        HOUR_24(1440),
        DAY_7(10080),
        DAY_30(43200);

        private final int minutes;

        WindowType(int minutes) {
            this.minutes = minutes;
        }

        public int getMinutes() {
            return minutes;
        }
    }
}
