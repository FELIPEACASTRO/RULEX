package com.rulex.repository;

import com.rulex.entity.RuleListEntry;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * Repositório para entradas das listas de bloqueio/permissão.
 */
@Repository
public interface RuleListEntryRepository extends JpaRepository<RuleListEntry, Long> {

  /**
   * Busca entradas de uma lista.
   */
  List<RuleListEntry> findByListIdAndIsActiveTrueOrderByAddedAtDesc(Long listId);

  /**
   * Busca entradas paginadas de uma lista.
   */
  Page<RuleListEntry> findByListIdAndIsActiveTrue(Long listId, Pageable pageable);

  /**
   * Busca entrada por valor em uma lista.
   */
  Optional<RuleListEntry> findByListIdAndEntryValueAndIsActiveTrue(Long listId, String entryValue);

  /**
   * Verifica se valor existe em uma lista.
   */
  boolean existsByListIdAndEntryValueAndIsActiveTrue(Long listId, String entryValue);

  /**
   * Busca entradas ativas não expiradas de uma lista.
   */
  @Query("SELECT e FROM RuleListEntry e WHERE e.list.id = :listId AND e.isActive = true " +
         "AND (e.expiresAt IS NULL OR e.expiresAt > :now)")
  List<RuleListEntry> findActiveNonExpiredEntries(
      @Param("listId") Long listId,
      @Param("now") LocalDateTime now);

  /**
   * Verifica se valor está em alguma blacklist ativa para o tipo de entidade.
   */
  @Query("SELECT COUNT(e) > 0 FROM RuleListEntry e " +
         "JOIN e.list l " +
         "WHERE l.listType = 'BLACKLIST' " +
         "AND l.entityType = :entityType " +
         "AND l.isActive = true " +
         "AND e.isActive = true " +
         "AND e.entryValue = :value " +
         "AND (e.expiresAt IS NULL OR e.expiresAt > :now)")
  boolean isValueBlacklisted(
      @Param("entityType") com.rulex.entity.RuleList.EntityType entityType,
      @Param("value") String value,
      @Param("now") LocalDateTime now);

  /**
   * Verifica se valor está em alguma whitelist ativa para o tipo de entidade.
   */
  @Query("SELECT COUNT(e) > 0 FROM RuleListEntry e " +
         "JOIN e.list l " +
         "WHERE l.listType = 'WHITELIST' " +
         "AND l.entityType = :entityType " +
         "AND l.isActive = true " +
         "AND e.isActive = true " +
         "AND e.entryValue = :value " +
         "AND (e.expiresAt IS NULL OR e.expiresAt > :now)")
  boolean isValueWhitelisted(
      @Param("entityType") com.rulex.entity.RuleList.EntityType entityType,
      @Param("value") String value,
      @Param("now") LocalDateTime now);

  /**
   * Desativa entradas expiradas.
   */
  @Modifying
  @Query("UPDATE RuleListEntry e SET e.isActive = false WHERE e.expiresAt < :now AND e.isActive = true")
  int deactivateExpiredEntries(@Param("now") LocalDateTime now);

  /**
   * Conta entradas ativas de uma lista.
   */
  long countByListIdAndIsActiveTrue(Long listId);
}
