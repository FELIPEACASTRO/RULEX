package com.rulex.repository.complex;

import com.rulex.entity.complex.RuleTemplate;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

/** Repositório para templates de regras. */
@Repository
public interface RuleTemplateRepository extends JpaRepository<RuleTemplate, UUID> {

  /** Busca template por nome */
  Optional<RuleTemplate> findByName(String name);

  /** Busca templates por categoria */
  List<RuleTemplate> findByCategoryOrderByNameAsc(String category);

  /** Busca templates do sistema */
  List<RuleTemplate> findByIsSystemTrueOrderByNameAsc();

  /** Busca templates criados por um usuário */
  List<RuleTemplate> findByCreatedByOrderByNameAsc(UUID createdBy);

  /** Busca templates não-sistema (customizados) */
  List<RuleTemplate> findByIsSystemFalseOrderByNameAsc();

  /** Busca todas as categorias distintas */
  @Query(
      "SELECT DISTINCT r.category FROM RuleTemplate r WHERE r.category IS NOT NULL ORDER BY r.category")
  List<String> findDistinctCategories();

  /** Verifica se existe template com determinado nome */
  boolean existsByName(String name);
}
