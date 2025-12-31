package com.rulex.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para referência geográfica.
 * Mapeia cidades/estados/países para coordenadas (centroid).
 */
@Entity
@Table(name = "geo_reference")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GeoReference {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "country_code", nullable = false, length = 3)
    private String countryCode;

    @Column(name = "country_alpha2", length = 2)
    private String countryAlpha2;

    @Column(name = "country_alpha3", length = 3)
    private String countryAlpha3;

    @Column(name = "country_name", length = 100)
    private String countryName;

    @Column(name = "state_code", length = 10)
    private String stateCode;

    @Column(name = "state_name", length = 100)
    private String stateName;

    @Column(name = "city_name", length = 200)
    private String cityName;

    @Column(name = "latitude", nullable = false, precision = 10, scale = 7)
    private BigDecimal latitude;

    @Column(name = "longitude", nullable = false, precision = 10, scale = 7)
    private BigDecimal longitude;

    @Column(name = "timezone", length = 50)
    private String timezone;

    @Column(name = "population")
    private Integer population;

    @Column(name = "is_capital")
    private Boolean isCapital;

    @Column(name = "region", length = 50)
    private String region;

    @Column(name = "created_at")
    private OffsetDateTime createdAt;

    @Column(name = "updated_at")
    private OffsetDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = OffsetDateTime.now();
        updatedAt = OffsetDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = OffsetDateTime.now();
    }
}
