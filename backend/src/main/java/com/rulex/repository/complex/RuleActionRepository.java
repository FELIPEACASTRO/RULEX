package com.rulex.repository.complex;

import com.rulex.entity.complex.RuleAction;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/**
 * Repositório para ações de regras.
 */
@Repository
public interface RuleActionRepository extends JpaRepository<RuleAction, UUID> {

    /**
     * Busca todas as ações de uma versão de regra ordenadas por posição
     */
    List<RuleAction> findByRuleVersionIdOrderByPositionAsc(UUID ruleVersionId);

    /**
     * Busca ações habilitadas de uma versão de regra
     */
    List<RuleAction> findByRuleVersionIdAndEnabledTrueOrderByPositionAsc(UUID ruleVersionId);

    /**
     * Busca ações por tipo
     */
    List<RuleAction> findByRuleVersionIdAndActionType(UUID ruleVersionId, RuleAction.ActionType actionType);

    /**
     * Busca ações condicionais (vinculadas a um grupo)
     */
    List<RuleAction> findByRuleVersionIdAndConditionGroupIdIsNotNull(UUID ruleVersionId);

    /**
     * Busca ações incondicionais (não vinculadas a um grupo)
     */
    List<RuleAction> findByRuleVersionIdAndConditionGroupIdIsNull(UUID ruleVersionId);

    /**
     * Conta ações em uma versão de regra
     */
    long countByRuleVersionId(UUID ruleVersionId);

    /**
     * Deleta todas as ações de uma versão de regra
     */
    void deleteByRuleVersionId(UUID ruleVersionId);
}
