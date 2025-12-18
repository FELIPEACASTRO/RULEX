package com.rulex.homolog.usecase;

import com.rulex.homolog.port.ActiveRuleSetPersistencePort;
import com.rulex.homolog.port.ActorResolverPort;
import com.rulex.homolog.port.AuditPort;
import com.rulex.homolog.port.DecisionLogPersistencePort;
import com.rulex.homolog.port.JsonPort;
import com.rulex.homolog.port.PayloadSanitizerPort;
import com.rulex.homolog.port.RuleDslEvaluatorPort;
import com.rulex.homolog.port.RulePersistencePort;
import com.rulex.homolog.port.RuleSetPersistencePort;
import com.rulex.homolog.port.RuleSetVersionItemPersistencePort;
import com.rulex.homolog.port.RuleSetVersionPersistencePort;
import com.rulex.homolog.port.RuleVersionPersistencePort;
import com.rulex.homolog.port.SimulationRunPersistencePort;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class HomologUseCaseConfig {

  @Bean
  public HomologRuleUseCase homologRuleUseCase(
      RulePersistencePort rulePersistence,
      RuleVersionPersistencePort ruleVersionPersistence,
      JsonPort json,
      AuditPort audit,
      ActorResolverPort actorResolver) {
    return new HomologRuleUseCase(
        rulePersistence, ruleVersionPersistence, json, audit, actorResolver);
  }

  @Bean
  public HomologRuleSetUseCase homologRuleSetUseCase(
      RulePersistencePort rulePersistence,
      RuleSetPersistencePort ruleSetPersistence,
      RuleSetVersionPersistencePort ruleSetVersionPersistence,
      RuleSetVersionItemPersistencePort ruleSetVersionItemPersistence,
      RuleVersionPersistencePort ruleVersionPersistence,
      ActiveRuleSetPersistencePort activeRuleSetPersistence,
      DecisionLogPersistencePort decisionLogPersistence,
      SimulationRunPersistencePort simulationRunPersistence,
      RuleDslEvaluatorPort evaluator,
      PayloadSanitizerPort sanitizer,
      AuditPort audit,
      ActorResolverPort actorResolver,
      JsonPort json) {
    return new HomologRuleSetUseCase(
        rulePersistence,
        ruleSetPersistence,
        ruleSetVersionPersistence,
        ruleSetVersionItemPersistence,
        ruleVersionPersistence,
        activeRuleSetPersistence,
        decisionLogPersistence,
        simulationRunPersistence,
        evaluator,
        sanitizer,
        audit,
        actorResolver,
        json);
  }
}
