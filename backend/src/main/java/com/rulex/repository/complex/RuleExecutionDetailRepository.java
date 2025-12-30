package com.rulex.repository.complex;

import com.rulex.entity.complex.RuleExecutionDetail;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * Repositório para detalhes de execução de regras.
 */
@Repository
public interface RuleExecutionDetailRepository extends JpaRepository<RuleExecutionDetail, UUID> {

    /**
     * Busca detalhes por log de decisão
     */
    List<RuleExecutionDetail> findByDecisionLogIdOrderByCreatedAtAsc(UUID decisionLogId);

    /**
     * Busca detalhes por versão de regra
     */
    Page<RuleExecutionDetail> findByRuleVersionId(UUID ruleVersionId, Pageable pageable);

    /**
     * Busca detalhes que falharam (result = false)
     */
    List<RuleExecutionDetail> findByDecisionLogIdAndResultFalse(UUID decisionLogId);

    /**
     * Busca detalhes que passaram (result = true)
     */
    List<RuleExecutionDetail> findByDecisionLogIdAndResultTrue(UUID decisionLogId);

    /**
     * Busca detalhes com erro
     */
    List<RuleExecutionDetail> findByDecisionLogIdAndErrorMessageIsNotNull(UUID decisionLogId);

    /**
     * Conta execuções por resultado para uma regra
     */
    @Query("""
        SELECT d.result, COUNT(d) FROM RuleExecutionDetail d
        WHERE d.ruleVersionId = :ruleVersionId
        GROUP BY d.result
        """)
    List<Object[]> countByRuleVersionIdGroupByResult(@Param("ruleVersionId") UUID ruleVersionId);

    /**
     * Calcula tempo médio de execução por regra
     */
    @Query("""
        SELECT AVG(d.executionTimeMs) FROM RuleExecutionDetail d
        WHERE d.ruleVersionId = :ruleVersionId
        AND d.executionTimeMs IS NOT NULL
        """)
    Double findAverageExecutionTimeByRuleVersionId(@Param("ruleVersionId") UUID ruleVersionId);

    /**
     * Busca detalhes por campo
     */
    List<RuleExecutionDetail> findByFieldName(String fieldName);

    /**
     * Busca detalhes em um período
     */
    Page<RuleExecutionDetail> findByCreatedAtBetween(
            OffsetDateTime start, OffsetDateTime end, Pageable pageable);

    /**
     * Deleta detalhes antigos (para limpeza)
     */
    void deleteByCreatedAtBefore(OffsetDateTime before);
}
