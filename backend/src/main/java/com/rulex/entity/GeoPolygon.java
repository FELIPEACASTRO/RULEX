package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

/**
 * Entidade para polígonos geográficos.
 * Usado para verificação de GEO_IN_POLYGON.
 */
@Entity
@Table(name = "geo_polygon")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GeoPolygon {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "name", nullable = false, unique = true, length = 100)
    private String name;

    @Column(name = "description", length = 500)
    private String description;

    @Column(name = "polygon_type", nullable = false, length = 50)
    private String polygonType;

    @Column(name = "country_code", length = 3)
    private String countryCode;

    @Column(name = "state_code", length = 10)
    private String stateCode;

    /**
     * Polígono armazenado como JSON array de pontos [[lat1,lon1],[lat2,lon2],...]
     */
    @Column(name = "polygon_points", nullable = false, columnDefinition = "jsonb")
    @JdbcTypeCode(SqlTypes.JSON)
    private String polygonPoints;

    @Column(name = "min_lat", precision = 10, scale = 7)
    private BigDecimal minLat;

    @Column(name = "max_lat", precision = 10, scale = 7)
    private BigDecimal maxLat;

    @Column(name = "min_lon", precision = 10, scale = 7)
    private BigDecimal minLon;

    @Column(name = "max_lon", precision = 10, scale = 7)
    private BigDecimal maxLon;

    @Column(name = "enabled")
    private Boolean enabled;

    @Column(name = "created_at")
    private OffsetDateTime createdAt;

    @Column(name = "updated_at")
    private OffsetDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = OffsetDateTime.now();
        updatedAt = OffsetDateTime.now();
        if (enabled == null) {
            enabled = true;
        }
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = OffsetDateTime.now();
    }
}
