package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

@Entity
@Table(name = "voip_phone_ranges")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VoipPhoneRange {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "country_code", nullable = false, length = 5)
  private String countryCode;

  @Column(name = "area_code", length = 5)
  private String areaCode;

  @Column(name = "prefix_start", nullable = false, length = 10)
  private String prefixStart;

  @Column(name = "prefix_end", length = 10)
  private String prefixEnd;

  @Column(name = "carrier_name", length = 100)
  private String carrierName;

  @Column(name = "is_voip", nullable = false)
  @Builder.Default
  private Boolean isVoip = true;

  @Column(name = "risk_level", nullable = false, length = 20)
  @Builder.Default
  private String riskLevel = "HIGH";

  @Column(name = "notes", columnDefinition = "TEXT")
  private String notes;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  @UpdateTimestamp
  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;
}
