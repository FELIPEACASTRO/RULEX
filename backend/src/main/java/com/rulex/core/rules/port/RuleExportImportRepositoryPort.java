package com.rulex.core.rules.port;

import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.homolog.RuleEntity;
import com.rulex.entity.homolog.RuleVersionEntity;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface RuleExportImportRepositoryPort {

  List<RuleConfiguration> findAllSimpleRules();

  Optional<RuleConfiguration> findSimpleRuleByName(String ruleName);

  RuleConfiguration saveSimpleRule(RuleConfiguration ruleConfiguration);

  List<RuleEntity> findAllHomologRules();

  Optional<RuleEntity> findHomologRuleByKey(String key);

  RuleEntity saveHomologRule(RuleEntity rule);

  List<RuleVersionEntity> findRuleVersionsByRuleId(UUID ruleId);

  RuleVersionEntity saveRuleVersion(RuleVersionEntity ruleVersion);
}
