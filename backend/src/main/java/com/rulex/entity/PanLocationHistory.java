package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para histórico de localização de PANs. Usado para detecção de impossible travel
 * (velocidade de deslocamento impossível entre transações).
 */
@Entity
@Table(
    name = "pan_location_history",
    indexes = {
      @Index(name = "idx_location_pan", columnList = "pan_hash"),
      @Index(name = "idx_location_time", columnList = "pan_hash, transaction_time")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class PanLocationHistory {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "pan_hash", nullable = false, length = 64)
  private String panHash;

  // Location data
  @Column(name = "latitude", nullable = false)
  private Double latitude;

  @Column(name = "longitude", nullable = false)
  private Double longitude;

  @Column(name = "city", length = 100)
  private String city;

  @Column(name = "country", length = 3)
  private String country;

  // Transaction info
  @Column(name = "transaction_id")
  private Long transactionId;

  @Column(name = "transaction_time", nullable = false)
  private OffsetDateTime transactionTime;

  @Column(name = "is_card_present")
  @Builder.Default
  private Boolean isCardPresent = true;

  // Travel analysis (from previous location)
  @Column(name = "distance_km")
  private Double distanceKm;

  @Column(name = "elapsed_minutes")
  private Double elapsedMinutes;

  @Column(name = "speed_kmh")
  private Double speedKmh;

  @Column(name = "travel_risk", length = 20)
  @Enumerated(EnumType.STRING)
  private TravelRisk travelRisk;

  @Column(name = "created_at")
  private OffsetDateTime createdAt;

  @PrePersist
  protected void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
    if (isCardPresent == null) {
      isCardPresent = true;
    }
  }

  /** Calcula a velocidade de deslocamento em km/h. Retorna null se não houver dados suficientes. */
  public Double calculateSpeedKmh() {
    if (distanceKm == null || elapsedMinutes == null || elapsedMinutes <= 0) {
      return null;
    }
    return distanceKm / (elapsedMinutes / 60.0);
  }

  /**
   * Determina o risco de viagem baseado na velocidade.
   *
   * @param speedKmh velocidade em km/h
   * @return nível de risco
   */
  public static TravelRisk calculateTravelRisk(Double speedKmh) {
    if (speedKmh == null) {
      return TravelRisk.UNKNOWN;
    }
    if (speedKmh > 1000) { // > 1000 km/h é impossível (avião comercial ~900 km/h)
      return TravelRisk.IMPOSSIBLE;
    }
    if (speedKmh > 500) { // Possível apenas de avião
      return TravelRisk.HIGH;
    }
    if (speedKmh > 150) { // Possível de carro em alta velocidade
      return TravelRisk.MEDIUM;
    }
    return TravelRisk.LOW;
  }

  /** Níveis de risco de viagem. */
  public enum TravelRisk {
    /** Velocidade normal, sem risco */
    LOW,
    /** Velocidade alta mas possível (carro rápido) */
    MEDIUM,
    /** Velocidade muito alta, provavelmente avião */
    HIGH,
    /** Velocidade impossível fisicamente */
    IMPOSSIBLE,
    /** Não foi possível calcular */
    UNKNOWN
  }
}
