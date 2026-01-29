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
 * Entidade para BINs e PANs de teste/problemáticos.
 * Permite configurar via banco de dados em vez de listas hardcoded.
 */
@Entity
@Table(
    name = "test_card_patterns",
    indexes = {
      @Index(name = "idx_test_card_pattern", columnList = "pattern"),
      @Index(name = "idx_test_card_type", columnList = "pattern_type"),
      @Index(name = "idx_test_card_active", columnList = "is_active")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TestCardPattern {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /**
   * Padrão a ser detectado. Pode ser:
   * - BIN (6-8 dígitos)
   * - PAN completo
   * - Regex pattern
   */
  @Column(nullable = false, length = 64)
  private String pattern;

  /**
   * Tipo do padrão: BIN, PAN, REGEX
   */
  @Column(name = "pattern_type", nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private PatternType patternType;

  /**
   * Categoria do padrão: TEST, BLACKLISTED, SUSPICIOUS
   */
  @Column(nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private Category category;

  /**
   * Descrição do padrão (ex: "Visa Test Card", "BIN de teste Mastercard")
   */
  @Column(length = 255)
  private String description;

  /**
   * Bandeira do cartão, se aplicável
   */
  @Column(name = "card_brand", length = 50)
  private String cardBrand;

  /**
   * Se o padrão está ativo
   */
  @Column(name = "is_active")
  @Builder.Default
  private Boolean isActive = true;

  /**
   * Quem criou o registro
   */
  @Column(name = "created_by", length = 100)
  private String createdBy;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  @UpdateTimestamp
  @Column(name = "updated_at", nullable = false)
  private OffsetDateTime updatedAt;

  public enum PatternType {
    /** BIN de 6-8 dígitos */
    BIN,
    /** PAN completo */
    PAN,
    /** Padrão regex */
    REGEX
  }

  public enum Category {
    /** Cartão de teste para desenvolvimento/QA */
    TEST,
    /** BIN/PAN em lista negra */
    BLACKLISTED,
    /** Padrão suspeito para monitoramento */
    SUSPICIOUS
  }
}
