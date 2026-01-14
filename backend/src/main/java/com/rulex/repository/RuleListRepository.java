package com.rulex.repository;

import com.rulex.entity.RuleList;
import com.rulex.entity.RuleList.EntityType;
import com.rulex.entity.RuleList.ListType;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para listas de bloqueio/permissão. */
@Repository
public interface RuleListRepository extends JpaRepository<RuleList, Long> {

  /** Busca lista por nome. */
  Optional<RuleList> findByListName(String listName);

  /** Busca listas por tipo. */
  List<RuleList> findByListTypeAndIsActiveTrueOrderByListNameAsc(ListType listType);

  /** Busca listas por tipo de entidade. */
  List<RuleList> findByEntityTypeAndIsActiveTrueOrderByListNameAsc(EntityType entityType);

  /** Busca listas ativas. */
  List<RuleList> findByIsActiveTrueOrderByListNameAsc();

  /** Verifica se existe lista com o nome. */
  boolean existsByListName(String listName);

  /** Busca blacklists ativas para um tipo de entidade. */
  @Query(
      "SELECT l FROM RuleList l WHERE l.listType = 'BLACKLIST' AND l.entityType = :entityType AND l.isActive = true")
  List<RuleList> findActiveBlacklistsByEntityType(@Param("entityType") EntityType entityType);

  /** Busca whitelists ativas para um tipo de entidade. */
  @Query(
      "SELECT l FROM RuleList l WHERE l.listType = 'WHITELIST' AND l.entityType = :entityType AND l.isActive = true")
  List<RuleList> findActiveWhitelistsByEntityType(@Param("entityType") EntityType entityType);
}
