package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/** Entidade para entradas das listas de bloqueio/permissão. */
@Entity
@Table(
    name = "rule_list_entries",
    indexes = {
      @Index(name = "idx_list_entries_list_id", columnList = "list_id"),
      @Index(name = "idx_list_entries_value", columnList = "entry_value"),
      @Index(name = "idx_list_entries_active", columnList = "is_active")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleListEntry {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "list_id", nullable = false)
  private RuleList list;

  @Column(name = "entry_value", nullable = false, length = 500)
  private String entryValue;

  @Column(columnDefinition = "TEXT")
  private String reason;

  @Column(name = "expires_at")
  private LocalDateTime expiresAt;

  @Column(name = "added_by", nullable = false, length = 100)
  private String addedBy;

  @Column(name = "added_at", nullable = false, updatable = false)
  private LocalDateTime addedAt;

  @Column(name = "is_active")
  @Builder.Default
  private Boolean isActive = true;

  @PrePersist
  protected void onCreate() {
    if (addedAt == null) {
      addedAt = LocalDateTime.now();
    }
  }

  /** Verifica se a entrada está expirada. */
  public boolean isExpired() {
    if (expiresAt == null) {
      return false;
    }
    return LocalDateTime.now().isAfter(expiresAt);
  }

  /** Verifica se a entrada está ativa e não expirada. */
  public boolean isEffective() {
    return Boolean.TRUE.equals(isActive) && !isExpired();
  }
}
