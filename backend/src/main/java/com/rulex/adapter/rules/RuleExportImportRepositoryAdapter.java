package com.rulex.adapter.rules;

import com.rulex.core.rules.port.RuleExportImportRepositoryPort;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.homolog.RuleEntity;
import com.rulex.entity.homolog.RuleVersionEntity;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.homolog.RuleRepository;
import com.rulex.repository.homolog.RuleVersionRepository;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Component;

@Component
public class RuleExportImportRepositoryAdapter implements RuleExportImportRepositoryPort {

  private final RuleConfigurationRepository ruleConfigRepository;
  private final RuleRepository ruleRepository;
  private final RuleVersionRepository ruleVersionRepository;

  public RuleExportImportRepositoryAdapter(
      RuleConfigurationRepository ruleConfigRepository,
      RuleRepository ruleRepository,
      RuleVersionRepository ruleVersionRepository) {
    this.ruleConfigRepository = ruleConfigRepository;
    this.ruleRepository = ruleRepository;
    this.ruleVersionRepository = ruleVersionRepository;
  }

  @Override
  public List<RuleConfiguration> findAllSimpleRules() {
    return ruleConfigRepository.findAll();
  }

  @Override
  public Optional<RuleConfiguration> findSimpleRuleByName(String ruleName) {
    return ruleConfigRepository.findByRuleName(ruleName);
  }

  @Override
  public RuleConfiguration saveSimpleRule(RuleConfiguration ruleConfiguration) {
    return ruleConfigRepository.save(ruleConfiguration);
  }

  @Override
  public List<RuleEntity> findAllHomologRules() {
    return ruleRepository.findAll();
  }

  @Override
  public Optional<RuleEntity> findHomologRuleByKey(String key) {
    return ruleRepository.findByKey(key);
  }

  @Override
  public RuleEntity saveHomologRule(RuleEntity rule) {
    return ruleRepository.save(rule);
  }

  @Override
  public List<RuleVersionEntity> findRuleVersionsByRuleId(UUID ruleId) {
    return ruleVersionRepository.findByRuleIdOrderByVersionDesc(ruleId);
  }

  @Override
  public RuleVersionEntity saveRuleVersion(RuleVersionEntity ruleVersion) {
    return ruleVersionRepository.save(ruleVersion);
  }
}
