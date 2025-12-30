package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidade para listas de bloqueio/permissão (blacklist/whitelist).
 * Permite gerenciar listas de PANs, merchants, países, etc.
 */
@Entity
@Table(name = "rule_lists")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleList {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(name = "list_name", nullable = false, unique = true, length = 100)
  private String listName;

  @Column(name = "list_type", nullable = false, length = 20)
  @Enumerated(EnumType.STRING)
  private ListType listType;

  @Column(name = "entity_type", nullable = false, length = 50)
  @Enumerated(EnumType.STRING)
  private EntityType entityType;

  @Column(columnDefinition = "TEXT")
  private String description;

  @Column(name = "is_active")
  @Builder.Default
  private Boolean isActive = true;

  @Column(name = "created_by", nullable = false, length = 100)
  private String createdBy;

  @Column(name = "created_at", nullable = false, updatable = false)
  private LocalDateTime createdAt;

  @Column(name = "updated_at", nullable = false)
  private LocalDateTime updatedAt;

  @OneToMany(mappedBy = "list", cascade = CascadeType.ALL, orphanRemoval = true)
  @Builder.Default
  private List<RuleListEntry> entries = new ArrayList<>();

  @PrePersist
  protected void onCreate() {
    LocalDateTime now = LocalDateTime.now();
    if (createdAt == null) {
      createdAt = now;
    }
    if (updatedAt == null) {
      updatedAt = now;
    }
  }

  @PreUpdate
  protected void onUpdate() {
    updatedAt = LocalDateTime.now();
  }

  public enum ListType {
    BLACKLIST("Lista de Bloqueio"),
    WHITELIST("Lista de Permissão"),
    GREYLIST("Lista de Monitoramento");

    private final String label;

    ListType(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }

  public enum EntityType {
    PAN("Número do Cartão"),
    MERCHANT_ID("ID do Merchant"),
    CUSTOMER_ID("ID do Cliente"),
    MCC("Código MCC"),
    COUNTRY("País"),
    IP("Endereço IP"),
    DEVICE_ID("ID do Dispositivo"),
    EMAIL("E-mail"),
    PHONE("Telefone");

    private final String label;

    EntityType(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }
}
