package com.rulex.repository.complex;

import com.rulex.entity.complex.RuleContextVariable;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/** Repositório para variáveis de contexto. */
@Repository
public interface RuleContextVariableRepository extends JpaRepository<RuleContextVariable, UUID> {

  /** Busca todas as variáveis de uma versão de regra */
  List<RuleContextVariable> findByRuleVersionId(UUID ruleVersionId);

  /** Busca uma variável por nome em uma versão de regra */
  Optional<RuleContextVariable> findByRuleVersionIdAndName(UUID ruleVersionId, String name);

  /** Busca variáveis por tipo de fonte */
  List<RuleContextVariable> findByRuleVersionIdAndSourceType(
      UUID ruleVersionId, RuleContextVariable.SourceType sourceType);

  /** Verifica se existe uma variável com determinado nome */
  boolean existsByRuleVersionIdAndName(UUID ruleVersionId, String name);

  /** Deleta todas as variáveis de uma versão de regra */
  void deleteByRuleVersionId(UUID ruleVersionId);
}
