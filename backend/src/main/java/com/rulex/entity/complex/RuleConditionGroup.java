package com.rulex.entity.complex;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

/**
 * Entidade para grupos de condições com suporte a aninhamento. Permite criar estruturas complexas
 * como: (A AND B) OR (C AND D)
 *
 * <p>Pode pertencer a uma rule_version (via ruleVersionId) OU a uma complex_rule (via
 * complexRuleId). Pelo menos um dos dois deve estar preenchido (constraint CHECK no banco).
 */
@Entity
@Table(
    name = "rule_condition_groups",
    indexes = {
      @Index(name = "idx_condition_groups_rule_version", columnList = "rule_version_id"),
      @Index(name = "idx_condition_groups_complex_rule", columnList = "complex_rule_id"),
      @Index(name = "idx_condition_groups_parent", columnList = "parent_group_id")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleConditionGroup {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  /**
   * FK para rule_versions (nullable desde V12). Usado quando o grupo pertence a uma versão de regra
   * tradicional.
   */
  @Column(name = "rule_version_id")
  private UUID ruleVersionId;

  /**
   * FK para complex_rules (adicionado em V12). Usado quando o grupo pertence a uma regra complexa.
   */
  @Column(name = "complex_rule_id")
  private UUID complexRuleId;

  @Column(name = "parent_group_id")
  private UUID parentGroupId;

  @Enumerated(EnumType.STRING)
  @Column(name = "logic_operator", nullable = false, columnDefinition = "group_logic_operator")
  private GroupLogicOperator logicOperator;

  @Column(nullable = false)
  @Builder.Default
  private Integer position = 0;

  private String name;

  private String description;

  @Column(nullable = false)
  @Builder.Default
  private Boolean enabled = true;

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;

  /** Condições diretas deste grupo */
  @OneToMany(mappedBy = "group", cascade = CascadeType.ALL, orphanRemoval = true)
  @OrderBy("position ASC")
  @Builder.Default
  private List<RuleCondition> conditions = new ArrayList<>();

  /** Grupos filhos (aninhados) */
  @OneToMany(cascade = CascadeType.ALL, orphanRemoval = true)
  @JoinColumn(name = "parent_group_id")
  @OrderBy("position ASC")
  @Builder.Default
  private List<RuleConditionGroup> children = new ArrayList<>();

  /** Operadores lógicos suportados para grupos */
  public enum GroupLogicOperator {
    AND, // Todas as condições devem ser verdadeiras
    OR, // Pelo menos uma condição deve ser verdadeira
    NOT, // Inverte o resultado do grupo
    XOR, // Exatamente uma condição deve ser verdadeira
    NAND, // NOT AND - pelo menos uma condição deve ser falsa
    NOR // NOT OR - todas as condições devem ser falsas
  }
}
