package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.core.engine.port.PayloadHashPort;
import com.rulex.core.engine.port.RuleEngineAuditPort;
import com.rulex.core.engine.port.RuleEngineConditionPort;
import com.rulex.core.engine.port.RuleEngineContractValidationPort;
import com.rulex.core.engine.port.RuleEngineDecisionPort;
import com.rulex.core.engine.port.RuleEngineDecisionRepositoryPort;
import com.rulex.core.engine.port.RuleEngineEnrichmentPort;
import com.rulex.core.engine.port.RuleEngineGraphPort;
import com.rulex.core.engine.port.RuleEngineInputPort;
import com.rulex.core.engine.port.RuleEngineLegacyRulePort;
import com.rulex.core.engine.port.RuleEnginePrecheckPort;
import com.rulex.core.engine.port.RuleEngineRawStorePort;
import com.rulex.core.engine.port.RuleEngineResponsePort;
import com.rulex.core.engine.port.RuleEngineRuleConfigurationPort;
import com.rulex.core.engine.port.RuleEngineShadowPort;
import com.rulex.core.engine.port.RuleEngineTransactionRepositoryPort;
import com.rulex.core.engine.port.RuleEngineVelocityPort;
import com.rulex.core.engine.port.RuleOrderingPort;
import com.rulex.core.engine.usecase.RuleEngineUseCase;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.service.engine.ConditionMatcher;
import com.rulex.service.engine.RuleCandidateIndexHelper;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.time.Clock;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço que implementa o motor de regras duras para análise de fraude. Avalia transações contra
 * regras configuráveis e retorna uma classificação.
 */
@Service
@Transactional
public class RuleEngineService implements RuleEngineInputPort {

  private final RuleEngineUseCase useCase;

  public RuleEngineService(
      RuleEngineTransactionRepositoryPort transactionRepository,
      RuleEngineDecisionRepositoryPort decisionRepository,
      RuleEngineRuleConfigurationPort ruleConfigRepository,
      RuleEngineAuditPort auditService,
      ObjectMapper objectMapper,
      Clock clock,
      PayloadHashPort payloadHashService,
      RuleEngineRawStorePort rawStoreService,
      RuleExecutionLogService ruleExecutionLogService,
      RuleOrderingPort ruleOrderingService,
      RuleEngineEnrichmentPort transactionEnrichmentFacade,
      RuleEngineResponsePort responseBuilder,
      RuleEngineConditionPort conditionHelper,
      RuleEngineContractValidationPort contractValidationHelper,
      RuleEngineDecisionPort decisionHelper,
      RuleEnginePrecheckPort precheckHelper,
      RuleEngineShadowPort shadowRuleExecutionHelper,
      RuleEngineLegacyRulePort legacyRuleHelper,
      RuleCandidateIndexHelper candidateIndexHelper,
      BloomFilterService bloomFilterService,
      ImpossibleTravelService impossibleTravelService,
      GeoService geoService,
      RuleEngineVelocityPort velocityServiceFacade,
      RuleEngineGraphPort neo4jGraphService,
      ConditionMatcher conditionMatcher,
      @Value("${rulex.engine.optimizedRuleOrder:false}") boolean optimizedRuleOrder,
      @Value("${rulex.engine.bloomFilter.enabled:true}") boolean bloomFilterEnabled,
      @Value("${rulex.engine.shadowMode.enabled:true}") boolean shadowModeEnabled,
      @Value("${rulex.engine.impossibleTravel.enabled:false}") boolean impossibleTravelEnabled,
      @Value("${rulex.engine.velocity.redis.enabled:false}") boolean redisVelocityEnabled,
      @Value("${rulex.neo4j.graph-tracking.enabled:true}") boolean neo4jGraphTrackingEnabled) {
    this.useCase =
        new RuleEngineUseCase(
            transactionRepository,
            decisionRepository,
            ruleConfigRepository,
            auditService,
            objectMapper,
            clock,
            payloadHashService,
            rawStoreService,
            ruleExecutionLogService,
            ruleOrderingService,
            transactionEnrichmentFacade,
            responseBuilder,
            conditionHelper,
            contractValidationHelper,
            decisionHelper,
            precheckHelper,
            shadowRuleExecutionHelper,
            legacyRuleHelper,
            candidateIndexHelper,
            bloomFilterService,
            impossibleTravelService,
            geoService,
            velocityServiceFacade,
            neo4jGraphService,
            conditionMatcher,
            optimizedRuleOrder,
            bloomFilterEnabled,
            shadowModeEnabled,
            impossibleTravelEnabled,
            redisVelocityEnabled,
            neo4jGraphTrackingEnabled);
  }

  /** Processa uma transação e retorna a classificação de fraude. */
  @Override
  public TransactionResponse analyzeTransaction(TransactionRequest request) {
    return useCase.analyzeTransaction(request);
  }

  @Override
  public TransactionResponse analyzeTransaction(
      TransactionRequest request, byte[] rawBytes, String contentType) {
    return useCase.analyzeTransaction(request, rawBytes, contentType);
  }

  @Override
  public EvaluateResponse evaluate(TransactionRequest request) {
    return useCase.evaluate(request);
  }

  @Override
  public EvaluateResponse evaluate(TransactionRequest request, byte[] rawBytes, String contentType) {
    return useCase.evaluate(request, rawBytes, contentType);
  }

  @Override
  public EvaluateResponse evaluateRaw(String rawBody, byte[] rawBytes, String contentType) {
    return useCase.evaluateRaw(rawBody, rawBytes, contentType);
  }
}
