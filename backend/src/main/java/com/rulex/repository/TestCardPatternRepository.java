package com.rulex.repository;

import com.rulex.entity.TestCardPattern;
import com.rulex.entity.TestCardPattern.Category;
import com.rulex.entity.TestCardPattern.PatternType;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * Repositório para padrões de cartões de teste/blacklist.
 */
@Repository
public interface TestCardPatternRepository extends JpaRepository<TestCardPattern, Long> {

  /**
   * Busca todos os padrões ativos.
   */
  List<TestCardPattern> findByIsActiveTrue();

  /**
   * Busca padrões ativos por tipo.
   */
  List<TestCardPattern> findByPatternTypeAndIsActiveTrue(PatternType patternType);

  /**
   * Busca padrões ativos por categoria.
   */
  List<TestCardPattern> findByCategoryAndIsActiveTrue(Category category);

  /**
   * Busca padrões ativos por tipo e categoria.
   */
  List<TestCardPattern> findByPatternTypeAndCategoryAndIsActiveTrue(
      PatternType patternType, Category category);

  /**
   * Verifica se um BIN específico está na lista (match exato).
   */
  @Query("SELECT COUNT(t) > 0 FROM TestCardPattern t WHERE t.pattern = :bin " +
         "AND t.patternType = 'BIN' AND t.isActive = true")
  boolean isBinInList(@Param("bin") String bin);

  /**
   * Verifica se um PAN específico está na lista (match exato).
   */
  @Query("SELECT COUNT(t) > 0 FROM TestCardPattern t WHERE t.pattern = :pan " +
         "AND t.patternType = 'PAN' AND t.isActive = true")
  boolean isPanInList(@Param("pan") String pan);

  /**
   * Busca padrões de BIN blacklisted ativos.
   */
  @Query("SELECT t.pattern FROM TestCardPattern t WHERE t.patternType = 'BIN' " +
         "AND t.category = 'BLACKLISTED' AND t.isActive = true")
  List<String> findBlacklistedBins();

  /**
   * Busca PANs de teste ativos.
   */
  @Query("SELECT t.pattern FROM TestCardPattern t WHERE t.patternType = 'PAN' " +
         "AND t.category = 'TEST' AND t.isActive = true")
  List<String> findTestPans();

  /**
   * Busca todos os padrões regex ativos.
   */
  @Query("SELECT t.pattern FROM TestCardPattern t WHERE t.patternType = 'REGEX' " +
         "AND t.isActive = true")
  List<String> findActiveRegexPatterns();
}
