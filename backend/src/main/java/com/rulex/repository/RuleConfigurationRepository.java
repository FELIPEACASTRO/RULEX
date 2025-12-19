package com.rulex.repository;

import com.rulex.entity.RuleConfiguration;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

/** Repositório para acesso aos dados de Configuração de Regras. */
@Repository
public interface RuleConfigurationRepository extends JpaRepository<RuleConfiguration, Long> {

  Optional<RuleConfiguration> findByRuleName(String ruleName);

  List<RuleConfiguration> findByRuleNameIn(Collection<String> ruleNames);

  List<RuleConfiguration> findByEnabled(Boolean enabled);

  List<RuleConfiguration> findByRuleType(RuleConfiguration.RuleType ruleType);

  List<RuleConfiguration> findByEnabledAndRuleType(
      Boolean enabled, RuleConfiguration.RuleType ruleType);
}
