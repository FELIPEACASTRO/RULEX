package com.rulex.repository;

import com.rulex.entity.MccCategory;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para categorização de MCC. */
@Repository
public interface MccCategoryRepository extends JpaRepository<MccCategory, Long> {

  /** Busca categoria por MCC. */
  Optional<MccCategory> findByMcc(Integer mcc);

  /** Busca todos os MCCs de alto risco. */
  List<MccCategory> findByIsHighRiskTrue();

  /** Busca todos os MCCs de uma categoria. */
  List<MccCategory> findByCategory(String category);

  /** Busca todos os MCCs por nível de risco. */
  List<MccCategory> findByRiskLevel(String riskLevel);

  /** Busca todos os MCCs de jogos/apostas. */
  List<MccCategory> findByIsGamblingTrue();

  /** Busca todos os MCCs de criptomoedas. */
  List<MccCategory> findByIsCryptoTrue();

  /** Busca todos os MCCs de conteúdo adulto. */
  List<MccCategory> findByIsAdultTrue();

  /** Busca todos os MCCs de saque/adiantamento. */
  List<MccCategory> findByIsCashAdvanceTrue();

  /** Verifica se um MCC é de alto risco. */
  @Query("SELECT COALESCE(m.isHighRisk, false) FROM MccCategory m WHERE m.mcc = :mcc")
  Boolean isHighRisk(@Param("mcc") Integer mcc);

  /** Retorna o nível de risco de um MCC. */
  @Query("SELECT COALESCE(m.riskLevel, 'UNKNOWN') FROM MccCategory m WHERE m.mcc = :mcc")
  String getRiskLevel(@Param("mcc") Integer mcc);

  /** Conta MCCs por nível de risco. */
  @Query("SELECT m.riskLevel, COUNT(m) FROM MccCategory m GROUP BY m.riskLevel")
  List<Object[]> countByRiskLevel();
}
