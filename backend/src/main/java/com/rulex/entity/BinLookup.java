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
 * Entidade para lookup de BIN (Bank Identification Number). Permite identificar emissor, bandeira e
 * país do cartão.
 */
@Entity
@Table(
    name = "bin_lookup",
    indexes = {
      @Index(name = "idx_bin_lookup_bin", columnList = "bin"),
      @Index(name = "idx_bin_lookup_card_brand", columnList = "card_brand"),
      @Index(name = "idx_bin_lookup_issuer_country", columnList = "issuer_country")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class BinLookup {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false, unique = true, length = 8)
  private String bin;

  @Column(name = "card_brand", length = 50)
  private String cardBrand;

  @Column(name = "card_type", length = 20)
  private String cardType;

  @Column(name = "card_level", length = 50)
  private String cardLevel;

  @Column(name = "issuer_name")
  private String issuerName;

  @Column(name = "issuer_country", length = 3)
  private String issuerCountry;

  @Column(name = "issuer_country_numeric", length = 3)
  private String issuerCountryNumeric;

  @Column(name = "is_regulated")
  @Builder.Default
  private Boolean isRegulated = false;

  @Column(name = "is_commercial")
  @Builder.Default
  private Boolean isCommercial = false;

  @Column(name = "is_prepaid")
  @Builder.Default
  private Boolean isPrepaid = false;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  @UpdateTimestamp
  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;
}
