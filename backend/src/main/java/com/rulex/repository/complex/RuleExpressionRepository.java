package com.rulex.repository.complex;

import com.rulex.entity.complex.RuleExpression;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/** Repositório para expressões calculadas. */
@Repository
public interface RuleExpressionRepository extends JpaRepository<RuleExpression, UUID> {

  /** Busca todas as expressões de uma versão de regra */
  List<RuleExpression> findByRuleVersionId(UUID ruleVersionId);

  /** Busca uma expressão por nome em uma versão de regra */
  Optional<RuleExpression> findByRuleVersionIdAndName(UUID ruleVersionId, String name);

  /** Verifica se existe uma expressão com determinado nome */
  boolean existsByRuleVersionIdAndName(UUID ruleVersionId, String name);

  /** Deleta todas as expressões de uma versão de regra */
  void deleteByRuleVersionId(UUID ruleVersionId);
}
