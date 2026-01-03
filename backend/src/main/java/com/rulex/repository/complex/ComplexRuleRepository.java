package com.rulex.repository.complex;

import com.rulex.entity.complex.ComplexRule;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

/** Repositório para regras complexas. */
@Repository
public interface ComplexRuleRepository extends JpaRepository<ComplexRule, UUID> {

  /** Busca uma regra por chave. */
  Optional<ComplexRule> findByKey(String key);

  /** Lista regras por status. */
  List<ComplexRule> findByStatus(String status);

  /** Lista regras habilitadas. */
  List<ComplexRule> findByEnabled(Boolean enabled);

  /** Lista regras habilitadas e publicadas. */
  @Query(
      "SELECT r FROM ComplexRule r WHERE r.enabled = true AND r.status = 'PUBLISHED' ORDER BY r.priority DESC")
  List<ComplexRule> findActiveRules();

  /** Verifica se existe uma regra com a chave especificada. */
  boolean existsByKey(String key);

  /** Lista regras ordenadas por prioridade. */
  List<ComplexRule> findAllByOrderByPriorityDesc();
}
