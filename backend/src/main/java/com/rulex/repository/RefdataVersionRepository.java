package com.rulex.repository;

import com.rulex.entity.RefdataVersion;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

/**
 * Repository para versionamento de dados de referência.
 *
 * <p>Gerencia versões de dados de referência como listas de BIN, MCC, etc.
 */
@Repository
public interface RefdataVersionRepository extends JpaRepository<RefdataVersion, UUID> {

  /** Busca versão por chave. */
  Optional<RefdataVersion> findByKey(String key);

  /** Verifica se uma versão existe. */
  boolean existsByKey(String key);

  /** Busca a versão mais recente de um tipo de dado. */
  @Query(
      "SELECT r FROM RefdataVersion r WHERE r.key LIKE :prefix% "
          + "ORDER BY r.createdAt DESC LIMIT 1")
  Optional<RefdataVersion> findLatestByKeyPrefix(String prefix);
}
