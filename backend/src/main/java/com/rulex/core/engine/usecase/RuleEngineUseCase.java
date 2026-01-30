package com.rulex.core.engine.usecase;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.core.engine.model.RuleEvaluationResult;
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
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.entity.TransactionRawStore;
import com.rulex.service.BloomFilterService;
import com.rulex.service.DerivedContext;
import com.rulex.service.EnrichmentService;
import com.rulex.service.GeoService;
import com.rulex.service.ImpossibleTravelService;
import com.rulex.service.ShadowModeService;
import com.rulex.service.engine.ConditionMatcher;
import com.rulex.service.engine.RuleCandidateIndexHelper;
import com.rulex.service.engine.RuleEngineLegacyRuleHelper;
import com.rulex.util.PanMaskingUtil;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;

/**
 * Caso de uso do motor de regras duras para análise de fraude. Avalia transações contra regras
 * configuráveis e retorna uma classificação.
 */
@Slf4j
public class RuleEngineUseCase implements RuleEngineInputPort {

  private final RuleEngineTransactionRepositoryPort transactionRepository;
  private final RuleEngineDecisionRepositoryPort decisionRepository;
  private final RuleEngineRuleConfigurationPort ruleConfigRepository;
  private final RuleEngineAuditPort auditService;
  private final ObjectMapper objectMapper;
  private final Clock clock;
  private final PayloadHashPort payloadHashService;
  private final RuleEngineRawStorePort rawStoreService;
  private final RuleExecutionLogService ruleExecutionLogService;
  private final EnrichmentService enrichmentService;
  private final RuleOrderingPort ruleOrderingService;
  private final RuleEngineEnrichmentPort transactionEnrichmentFacade;
  private final RuleEngineResponsePort responseBuilder;
  private final RuleEngineConditionPort conditionHelper;
  private final RuleEngineContractValidationPort contractValidationHelper;
  private final RuleEngineDecisionPort decisionHelper;
  private final RuleEnginePrecheckPort precheckHelper;
  private final RuleEngineShadowPort shadowRuleExecutionHelper;
  private final RuleEngineLegacyRulePort legacyRuleHelper;
  private final RuleCandidateIndexHelper candidateIndexHelper;

  // V4.0: advanced hard-rule services (opt-in via config)
  private final BloomFilterService bloomFilterService;
  private final ImpossibleTravelService impossibleTravelService;
  private final GeoService geoService;
  private final RuleEngineVelocityPort velocityServiceFacade;
  private final RuleEngineGraphPort neo4jGraphService;

  // ARCH-003: Extracted helper classes
  private final ConditionMatcher conditionMatcher;

  private final boolean optimizedRuleOrder;
  private final boolean bloomFilterEnabled;
  private final boolean shadowModeEnabled;
  private final boolean impossibleTravelEnabled;
  private final boolean redisVelocityEnabled;
  private final boolean neo4jGraphTrackingEnabled;

  public RuleEngineUseCase(
      RuleEngineTransactionRepositoryPort transactionRepository,
      RuleEngineDecisionRepositoryPort decisionRepository,
      RuleEngineRuleConfigurationPort ruleConfigRepository,
      RuleEngineAuditPort auditService,
      ObjectMapper objectMapper,
      Clock clock,
      PayloadHashPort payloadHashService,
      RuleEngineRawStorePort rawStoreService,
      RuleExecutionLogService ruleExecutionLogService,
      EnrichmentService enrichmentService,
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
      boolean optimizedRuleOrder,
      boolean bloomFilterEnabled,
      boolean shadowModeEnabled,
      boolean impossibleTravelEnabled,
      boolean redisVelocityEnabled,
      boolean neo4jGraphTrackingEnabled) {
    this.transactionRepository = transactionRepository;
    this.decisionRepository = decisionRepository;
    this.ruleConfigRepository = ruleConfigRepository;
    this.auditService = auditService;
    this.objectMapper = objectMapper;
    this.clock = clock;
    this.payloadHashService = payloadHashService;
    this.rawStoreService = rawStoreService;
    this.ruleExecutionLogService = ruleExecutionLogService;
    this.enrichmentService = enrichmentService;
    this.ruleOrderingService = ruleOrderingService;
    this.transactionEnrichmentFacade = transactionEnrichmentFacade;
    this.responseBuilder = responseBuilder;
    this.conditionHelper = conditionHelper;
    this.contractValidationHelper = contractValidationHelper;
    this.decisionHelper = decisionHelper;
    this.precheckHelper = precheckHelper;
    this.shadowRuleExecutionHelper = shadowRuleExecutionHelper;
    this.legacyRuleHelper = legacyRuleHelper;
    this.candidateIndexHelper = candidateIndexHelper;
    this.bloomFilterService = bloomFilterService;
    this.impossibleTravelService = impossibleTravelService;
    this.geoService = geoService;
    this.velocityServiceFacade = velocityServiceFacade;
    this.neo4jGraphService = neo4jGraphService;
    this.conditionMatcher = conditionMatcher;
    this.optimizedRuleOrder = optimizedRuleOrder;
    this.bloomFilterEnabled = bloomFilterEnabled;
    this.shadowModeEnabled = shadowModeEnabled;
    this.impossibleTravelEnabled = impossibleTravelEnabled;
    this.redisVelocityEnabled = redisVelocityEnabled;
    this.neo4jGraphTrackingEnabled = neo4jGraphTrackingEnabled;
  }

  /** Processa uma transação e retorna a classificação de fraude. */
  @Override
  public TransactionResponse analyzeTransaction(TransactionRequest request) {
    try {
      byte[] derived = objectMapper.writeValueAsBytes(request);
      return analyzeTransaction(request, derived, "application/json; charset=utf-8");
    } catch (Exception e) {
      throw new RuntimeException("Erro ao serializar payload para fallback interno", e);
    }
  }

  @Override
  public TransactionResponse analyzeTransaction(
      TransactionRequest request, byte[] rawBytes, String contentType) {
    long startTime = System.currentTimeMillis();

    try {
      String externalTransactionId = request.getExternalTransactionId();
      byte[] effectiveRawBytes = rawBytes == null ? new byte[0] : rawBytes;
      String payloadHash = payloadHashService.sha256Hex(effectiveRawBytes);

      java.util.Optional<TransactionRawStore> existingRawOpt =
          rawStoreService.findByExternalTransactionId(externalTransactionId);
      if (existingRawOpt.isPresent()) {
        TransactionRawStore existingRaw = existingRawOpt.get();
        if (!existingRaw.getPayloadRawHash().equals(payloadHash)) {
          auditService.logTamperAttempt(
              externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);

          safeLogAntiTamper(externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);

          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
          return responseBuilder.buildResponseFromTamperDecision(tamperDecision, startTime);
        }

        Optional<Transaction> existingTxOpt =
            transactionRepository.findByExternalTransactionId(externalTransactionId);
        if (existingTxOpt.isPresent()) {
          return responseBuilder.buildResponseFromExisting(existingTxOpt.get(), startTime);
        }
        log.warn(
            "Raw store exists but transaction row missing for externalTransactionId={}. Recovering by creating transaction.",
            externalTransactionId);
      }

      if (!existingRawOpt.isPresent()) {
        rawStoreService.store(externalTransactionId, payloadHash, effectiveRawBytes, contentType);
      }

      Optional<Transaction> existingTxOpt =
          transactionRepository.findByExternalTransactionId(externalTransactionId);
      if (existingTxOpt.isPresent()) {
        Transaction existing = existingTxOpt.get();
        if (existing.getPayloadRawHash() != null
            && !existing.getPayloadRawHash().equals(payloadHash)) {
          auditService.logTamperAttempt(
              externalTransactionId, existing.getPayloadRawHash(), payloadHash);
          safeLogAntiTamper(externalTransactionId, existing.getPayloadRawHash(), payloadHash);
          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
          return responseBuilder.buildResponseFromTamperDecision(tamperDecision, startTime);
        }
        return responseBuilder.buildResponseFromExisting(existing, startTime);
      }

      Transaction transaction = convertRequestToEntity(request);
      transaction.setPayloadRawHash(payloadHash);
      try {
        transaction = transactionRepository.save(transaction);
      } catch (DataIntegrityViolationException e) {
        Transaction racedTx =
            transactionRepository
                .findByExternalTransactionId(externalTransactionId)
                .orElseThrow(() -> e);
        if (racedTx.getPayloadRawHash() != null
            && !racedTx.getPayloadRawHash().equals(payloadHash)) {
          auditService.logTamperAttempt(
              externalTransactionId, racedTx.getPayloadRawHash(), payloadHash);
          safeLogAntiTamper(externalTransactionId, racedTx.getPayloadRawHash(), payloadHash);
          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
          return responseBuilder.buildResponseFromTamperDecision(tamperDecision, startTime);
        }
        return responseBuilder.buildResponseFromExisting(racedTx, startTime);
      }

      RuleEvaluationResult result = evaluateRules(transaction, request);

      TransactionDecision decision = decisionHelper.createDecision(transaction, result);
      decision.setExternalTransactionId(transaction.getExternalTransactionId());
      decision.setPayloadRawHash(payloadHash);
      decisionRepository.save(decision);

      if (redisVelocityEnabled) {
        try {
          velocityServiceFacade.recordTransaction(
              request,
              decision.getClassification() != null ? decision.getClassification().name() : "UNKNOWN",
              decision.getRiskScore());
        } catch (Exception e) {
          log.warn("Erro best-effort velocity tracking (não bloqueia decisão): {}", e.getMessage());
        }
      }

      if (neo4jGraphTrackingEnabled && neo4jGraphService.isCurrentlyAvailable()) {
        try {
          recordTransactionInGraph(request, decision);
        } catch (Exception e) {
          log.warn(
              "Erro best-effort Neo4j graph tracking (não bloqueia decisão): {}", e.getMessage());
        }
      }

      responseBuilder.safeLogEvaluate(
          ExecutionEventType.EVALUATE,
          transaction.getExternalTransactionId(),
          payloadHash,
          decision,
          result.getTriggeredRules(),
          null);

      auditService.logTransactionProcessed(transaction, decision, result);

      long processingTime = System.currentTimeMillis() - startTime;
      return responseBuilder.buildResponse(transaction, decision, result, processingTime);

    } catch (Exception e) {
      log.error("Erro ao processar transação: {}", request.getExternalTransactionId(), e);
      auditService.logError(request.getExternalTransactionId(), e);
      throw new RuntimeException("Erro ao processar transação", e);
    }
  }

  @Override
  public EvaluateResponse evaluate(TransactionRequest request) {
    try {
      byte[] derived = objectMapper.writeValueAsBytes(request);
      return evaluate(request, derived, "application/json; charset=utf-8");
    } catch (Exception e) {
      throw new RuntimeException("Erro ao serializar payload para fallback interno", e);
    }
  }

  @Override
  public EvaluateResponse evaluate(TransactionRequest request, byte[] rawBytes, String contentType) {
    long startTime = System.currentTimeMillis();

    try {
      String externalTransactionId = request.getExternalTransactionId();
      byte[] effectiveRawBytes = rawBytes == null ? new byte[0] : rawBytes;
      String payloadHash = payloadHashService.sha256Hex(effectiveRawBytes);

      java.util.Optional<TransactionRawStore> existingRawOpt =
          rawStoreService.findByExternalTransactionId(externalTransactionId);
      if (existingRawOpt.isPresent()) {
        TransactionRawStore existingRaw = existingRawOpt.get();
        if (!existingRaw.getPayloadRawHash().equals(payloadHash)) {
          auditService.logTamperAttempt(
              externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);
          safeLogAntiTamper(externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);
          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
          return responseBuilder.buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }

        Optional<Transaction> existingTxOpt =
            transactionRepository.findByExternalTransactionId(externalTransactionId);
        if (existingTxOpt.isPresent()) {
          return responseBuilder.buildEvaluateResponseFromExisting(existingTxOpt.get(), startTime);
        }
        log.warn(
            "Raw store exists but transaction row missing for externalTransactionId={}. Recovering by creating transaction.",
            externalTransactionId);
      }

      if (!existingRawOpt.isPresent()) {
        rawStoreService.store(externalTransactionId, payloadHash, effectiveRawBytes, contentType);
      }

      Optional<Transaction> existingTxOpt =
          transactionRepository.findByExternalTransactionId(externalTransactionId);
      if (existingTxOpt.isPresent()) {
        Transaction existing = existingTxOpt.get();
        if (existing.getPayloadRawHash() != null
            && !existing.getPayloadRawHash().equals(payloadHash)) {
          auditService.logTamperAttempt(
              externalTransactionId, existing.getPayloadRawHash(), payloadHash);
          safeLogAntiTamper(externalTransactionId, existing.getPayloadRawHash(), payloadHash);
          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
          return responseBuilder.buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }
        return responseBuilder.buildEvaluateResponseFromExisting(existing, startTime);
      }

      Transaction transaction = convertRequestToEntity(request);
      transaction.setPayloadRawHash(payloadHash);
      try {
        transaction = transactionRepository.save(transaction);
      } catch (DataIntegrityViolationException e) {
        Transaction racedTx =
            transactionRepository
                .findByExternalTransactionId(externalTransactionId)
                .orElseThrow(() -> e);
        if (racedTx.getPayloadRawHash() != null
            && !racedTx.getPayloadRawHash().equals(payloadHash)) {
          auditService.logTamperAttempt(
              externalTransactionId, racedTx.getPayloadRawHash(), payloadHash);
          safeLogAntiTamper(externalTransactionId, racedTx.getPayloadRawHash(), payloadHash);
          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
          return responseBuilder.buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }
        return responseBuilder.buildEvaluateResponseFromExisting(racedTx, startTime);
      }

      RuleEvaluationResult result = evaluateRules(transaction, request);

      TransactionDecision decision = decisionHelper.createDecision(transaction, result);
      decision.setExternalTransactionId(transaction.getExternalTransactionId());
      decision.setPayloadRawHash(payloadHash);
      decisionRepository.save(decision);

      if (redisVelocityEnabled) {
        try {
          velocityServiceFacade.recordTransaction(
              request,
              decision.getClassification() != null ? decision.getClassification().name() : "UNKNOWN",
              decision.getRiskScore());
        } catch (Exception e) {
          log.warn("Erro best-effort velocity tracking (não bloqueia decisão): {}", e.getMessage());
        }
      }

      if (neo4jGraphTrackingEnabled && neo4jGraphService.isCurrentlyAvailable()) {
        try {
          recordTransactionInGraph(request, decision);
        } catch (Exception e) {
          log.warn(
              "Erro best-effort Neo4j graph tracking (não bloqueia decisão): {}", e.getMessage());
        }
      }

      responseBuilder.safeLogEvaluate(
          ExecutionEventType.EVALUATE,
          transaction.getExternalTransactionId(),
          payloadHash,
          decision,
          result.getTriggeredRules(),
          null);

      auditService.logTransactionProcessed(transaction, decision, result);

      long processingTime = System.currentTimeMillis() - startTime;
      return responseBuilder.buildEvaluateResponse(transaction, decision, result, processingTime);

    } catch (Exception e) {
      log.error("Erro ao avaliar transação: {}", request.getExternalTransactionId(), e);
      auditService.logError(request.getExternalTransactionId(), e);
      throw new RuntimeException("Erro ao avaliar transação", e);
    }
  }

  @Override
  public EvaluateResponse evaluateRaw(String rawBody, byte[] rawBytes, String contentType) {
    long startTime = System.currentTimeMillis();

    byte[] effectiveRawBytes = rawBytes;
    if (effectiveRawBytes == null) {
      effectiveRawBytes = rawBody == null ? new byte[0] : rawBody.getBytes(StandardCharsets.UTF_8);
    }
    String payloadHash = payloadHashService.sha256Hex(effectiveRawBytes);

    TransactionRequest parsed;
    try {
      parsed = objectMapper.readValue(rawBody, TransactionRequest.class);
    } catch (Exception e) {
      auditService.logError("/evaluate", e);
      contractValidationHelper.safeLogContractError(
          null, payloadHash, "CONTRACT_INVALID_JSON", "Payload JSON inválido (parse falhou)");
      return contractValidationHelper.buildContractErrorEvaluateResponse(
          "CONTRACT_INVALID_JSON",
          "Payload JSON inválido (parse falhou)",
          TransactionDecision.TransactionClassification.FRAUD,
          payloadHash,
          startTime);
    }

    String externalTransactionId = parsed == null ? null : parsed.getExternalTransactionId();
    if (externalTransactionId == null || externalTransactionId.isBlank()) {
      contractValidationHelper.safeLogContractError(
          null,
          payloadHash,
          "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID",
          "externalTransactionId ausente ou vazio");
      return contractValidationHelper.buildContractErrorEvaluateResponse(
          "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID",
          "externalTransactionId ausente ou vazio",
          TransactionDecision.TransactionClassification.FRAUD,
          payloadHash,
          startTime);
    }

    List<String> missing = contractValidationHelper.missingRequiredForPersistence(parsed);
    if (!missing.isEmpty()) {
      contractValidationHelper.safeLogContractError(
          externalTransactionId,
          payloadHash,
          "CONTRACT_MISSING_REQUIRED_FIELDS",
          "Campos obrigatórios ausentes para persistência: " + String.join(", ", missing));
      return contractValidationHelper.buildContractErrorEvaluateResponse(
          "CONTRACT_MISSING_REQUIRED_FIELDS",
          "Campos obrigatórios ausentes para persistência: " + String.join(", ", missing),
          TransactionDecision.TransactionClassification.SUSPICIOUS,
          payloadHash,
          startTime);
    }

    return evaluate(parsed, effectiveRawBytes, contentType);
  }

  private RuleEvaluationResult evaluateRules(Transaction transaction, TransactionRequest request) {
    RuleEvaluationResult result = new RuleEvaluationResult();
    List<RuleConfiguration> enabledRules =
        optimizedRuleOrder
            ? ruleOrderingService.getOptimizedRuleOrder()
            : ruleConfigRepository.findByEnabled(true);

    RuleCandidateIndexHelper.CandidateIndex index =
        candidateIndexHelper.buildOrReuseCandidateIndex(enabledRules);

    DerivedContext derivedContext = DerivedContext.from(request);
    log.debug(
        "DerivedContext criado: transactionTs={}, bin={}, maskedPan={}",
        derivedContext.getTransactionTimestamp(),
        derivedContext.getBin(),
        derivedContext.getMaskedPan());

    com.rulex.service.enrichment.TransactionEnrichmentFacade.FullEnrichmentContext enrichmentContext =
        transactionEnrichmentFacade.enrichFull(request);
    Map<String, Object> enrichedFields = enrichmentContext.toFlatMap();
    log.debug(
        "Enriquecimento completo em {}ms: {} campos disponíveis",
        enrichmentContext.getEnrichmentTimeMs(),
        enrichedFields.size());

    int totalScore = 0;
    List<TriggeredRuleDTO> triggeredRules = new ArrayList<>();
    Map<String, Object> scoreDetails = new HashMap<>();
    TransactionDecision.TransactionClassification maxByRule =
        TransactionDecision.TransactionClassification.APPROVED;

    TransactionDecision.TransactionClassification preClassification =
        precheckHelper.runPreChecks(
            request,
            derivedContext,
            triggeredRules,
            scoreDetails,
            bloomFilterEnabled,
            impossibleTravelEnabled);
    maxByRule = maxSeverity(maxByRule, preClassification);
    if (maxByRule == TransactionDecision.TransactionClassification.FRAUD) {
      result.setRiskScore(100);
      result.setTriggeredRules(triggeredRules);
      result.setScoreDetails(scoreDetails);
      result.setClassification(maxByRule);
      result.setReason(generateReason(maxByRule, triggeredRules));
      return result;
    }

    List<RuleConfiguration> decisionRules = new ArrayList<>();
    List<RuleConfiguration> shadowRules = new ArrayList<>();
    for (RuleConfiguration rule : enabledRules) {
      if (!shadowModeEnabled || rule.getShadowMode() == null) {
        decisionRules.add(rule);
        continue;
      }

      switch (rule.getShadowMode()) {
        case DISABLED -> decisionRules.add(rule);
        case SHADOW -> shadowRules.add(rule);
        case CANARY -> {
          int pct = rule.getCanaryPercentage() != null ? rule.getCanaryPercentage() : 0;
          String stableKey = request.getExternalTransactionId() + ":" + rule.getRuleName();
          if (isInCanary(stableKey, pct)) {
            decisionRules.add(rule);
          } else {
            shadowRules.add(rule);
          }
        }
      }
    }

    for (RuleConfiguration rule : decisionRules) {
      RuleCandidateIndexHelper.RulePreconditions pre = index.byRuleName().get(rule.getRuleName());
      if (pre != null && pre.canSkipWithMissingFields() && !hasAllRequiredFields(request, pre)) {
        scoreDetails.put(rule.getRuleName(), Map.of("triggered", false));
        continue;
      }

      long startNanos = optimizedRuleOrder ? System.nanoTime() : 0L;
      RuleMatch ruleMatch = evaluateRuleGeneric(transaction, request, rule);
      if (optimizedRuleOrder) {
        long elapsed = System.nanoTime() - startNanos;
        try {
          ruleOrderingService.recordExecution(rule.getRuleName(), elapsed, ruleMatch.triggered);
        } catch (Exception e) {
          // best-effort
        }
      }

      if (ruleMatch.triggered) {
        int contribution = clampScore(rule.getWeight());
        totalScore += contribution;

        triggeredRules.add(
            TriggeredRuleDTO.builder()
                .name(rule.getRuleName())
                .weight(rule.getWeight())
                .contribution(contribution)
                .detail(ruleMatch.detail)
                .build());

        maxByRule = maxSeverity(maxByRule, rule.getClassification());

        if (maxByRule == TransactionDecision.TransactionClassification.FRAUD) {
          break;
        }

        scoreDetails.put(
            rule.getRuleName(),
            Map.of(
                "triggered",
                true,
                "weight",
                rule.getWeight(),
                "contribution",
                contribution,
                "detail",
                ruleMatch.detail));
      } else {
        scoreDetails.put(rule.getRuleName(), Map.of("triggered", false));
      }
    }

    totalScore = Math.min(totalScore, 100);

    result.setRiskScore(totalScore);
    result.setTriggeredRules(triggeredRules);
    result.setScoreDetails(scoreDetails);
    result.setClassification(maxByRule);
    result.setReason(generateReason(maxByRule, triggeredRules));

    if (shadowModeEnabled && !shadowRules.isEmpty()) {
      String actualDecision = maxByRule.name();
      ShadowModeService.RuleEvaluator evaluator =
          (rule, tx) -> {
            RuleMatch rm = evaluateRuleGeneric(transaction, request, rule);
            if (!rm.triggered) {
              return ShadowModeService.RuleEvaluationResult.notTriggered();
            }
            int score = clampScore(rule.getWeight());
            String action =
                rule.getClassification() == TransactionDecision.TransactionClassification.FRAUD
                    ? "BLOCK"
                    : (rule.getClassification()
                            == TransactionDecision.TransactionClassification.SUSPICIOUS
                        ? "FLAG"
                        : "ALLOW");
            return ShadowModeService.RuleEvaluationResult.triggered(score, action, rm.detail);
          };

      shadowRuleExecutionHelper.executeShadowRules(shadowRules, request, actualDecision, evaluator);
    }

    return result;
  }

  private boolean isInCanary(String stableKey, int percentage) {
    if (percentage <= 0) return false;
    if (percentage >= 100) return true;
    int bucket = Math.floorMod(stableKey.hashCode(), 100);
    return bucket < percentage;
  }

  private RuleMatch evaluateRuleGeneric(
      Transaction transaction, TransactionRequest request, RuleConfiguration rule) {
    if (rule.getRuleType() == RuleConfiguration.RuleType.VELOCITY) {
      RuleMatch velocity = evaluateRuleVelocity(request, rule);
      if (velocity != null) {
        return velocity;
      }
    }

    if (rule.getConditionsJson() != null && !rule.getConditionsJson().isBlank()) {
      List<RuleConditionDTO> conditions =
          candidateIndexHelper.readConditions(rule.getConditionsJson());
      RuleConfiguration.LogicOperator op =
          rule.getLogicOperator() != null
              ? rule.getLogicOperator()
              : RuleConfiguration.LogicOperator.AND;

      if (!conditions.isEmpty()) {
        List<String> explanations = new ArrayList<>();

        boolean triggered = (op == RuleConfiguration.LogicOperator.AND);
        for (RuleConditionDTO condition : conditions) {
          boolean condResult = conditionHelper.evaluateCondition(request, condition);
          explanations.add(conditionHelper.explainCondition(request, condition, condResult));

          if (op == RuleConfiguration.LogicOperator.AND) {
            triggered = triggered && condResult;
            if (!triggered) {
              break;
            }
          } else {
            triggered = triggered || condResult;
            if (triggered) {
              break;
            }
          }
        }

        return new RuleMatch(triggered, String.join(" | ", explanations));
      }
    }

    RuleEngineLegacyRuleHelper.LegacyRuleResult legacyResult =
        legacyRuleHelper.evaluateLegacyRule(request, rule);

    return new RuleMatch(legacyResult.triggered(), legacyResult.detail());
  }

  private String generateReason(
      TransactionDecision.TransactionClassification classification,
      List<TriggeredRuleDTO> triggeredRules) {
    List<String> appliedRules = triggeredRules.stream().map(TriggeredRuleDTO::getName).toList();
    return switch (classification) {
      case APPROVED -> "Transação aprovada. Nenhuma regra crítica foi acionada.";
      case SUSPICIOUS ->
          String.format(
              "Transação suspeita. Regras acionadas: %s", String.join(", ", appliedRules));
      case FRAUD ->
          String.format(
              "Transação bloqueada como fraude. Regras acionadas: %s",
              String.join(", ", appliedRules));
    };
  }

  private void safeLogAntiTamper(
      String externalTransactionId, String storedPayloadHash, String attemptedPayloadHash) {
    try {
      var errorJson = objectMapper.createObjectNode();
      errorJson.put("code", "ANTI_TAMPER");
      errorJson.put("message", "externalTransactionId reutilizado com payload diferente");
      ruleExecutionLogService.logAntiTamper(
          externalTransactionId, storedPayloadHash, attemptedPayloadHash, errorJson);
    } catch (DataAccessException e) {
      log.debug("Falha ao registrar rule_execution_log anti-tamper (ignorado)", e);
    }
  }

  private Transaction convertRequestToEntity(TransactionRequest request) {
    return Transaction.builder()
        .externalTransactionId(request.getExternalTransactionId())
        .customerIdFromHeader(request.getCustomerIdFromHeader())
        .customerAcctNumber(request.getCustomerAcctNumber())
        .pan(PanMaskingUtil.mask(request.getPan()))
        .merchantId(request.getMerchantId())
        .merchantName(request.getMerchantName())
        .transactionAmount(request.getTransactionAmount())
        .transactionDate(request.getTransactionDate())
        .transactionTime(request.getTransactionTime())
        .gmtOffset(request.getGmtOffset())
        .transactionCurrencyCode(request.getTransactionCurrencyCode())
        .transactionCurrencyConversionRate(request.getTransactionCurrencyConversionRate())
        .merchantCountryCode(request.getMerchantCountryCode())
        .merchantCity(request.getMerchantCity())
        .merchantState(request.getMerchantState())
        .merchantPostalCode(request.getMerchantPostalCode())
        .mcc(request.getMcc())
        .posEntryMode(request.getPosEntryMode())
        .customerPresent(request.getCustomerPresent())
        .posOffPremises(request.getPosOffPremises())
        .posCardCapture(request.getPosCardCapture())
        .posSecurity(request.getPosSecurity())
        .cvvPinTryLimitExceeded(request.getCvvPinTryLimitExceeded())
        .cvrofflinePinVerificationPerformed(request.getCvrofflinePinVerificationPerformed())
        .cvrofflinePinVerificationFailed(request.getCvrofflinePinVerificationFailed())
        .cardMediaType(request.getCardMediaType())
        .consumerAuthenticationScore(request.getConsumerAuthenticationScore())
        .externalScore3(request.getExternalScore3())
        .cavvResult(request.getCavvResult())
        .cryptogramValid(request.getCryptogramValid())
        .cvv2Response(request.getCvv2Response())
        .cvv2Present(request.getCvv2Present())
        .pinVerifyCode(request.getPinVerifyCode())
        .cvvVerifyCode(request.getCvvVerifyCode())
        .eciIndicator(request.getEciIndicator())
        .atcCard(request.getAtcCard())
        .atcHost(request.getAtcHost())
        .tokenAssuranceLevel(request.getTokenAssuranceLevel())
        .tokenizationIndicator(request.getTokenizationIndicator())
        .availableCredit(request.getAvailableCredit())
        .cardCashBalance(request.getCardCashBalance())
        .cardDelinquentAmount(request.getCardDelinquentAmount())
        .workflow(request.getWorkflow())
        .recordType(request.getRecordType())
        .clientIdFromHeader(request.getClientIdFromHeader())
        .createdAt(LocalDateTime.now(clock))
        .updatedAt(LocalDateTime.now(clock))
        .build();
  }

  private int clampScore(Integer weight) {
    if (weight == null) {
      return 0;
    }
    return Math.max(0, Math.min(weight, 100));
  }

  private TransactionDecision.TransactionClassification maxSeverity(
      TransactionDecision.TransactionClassification a,
      TransactionDecision.TransactionClassification b) {
    int sa = severity(a);
    int sb = severity(b);
    return sa >= sb ? a : b;
  }

  private int severity(TransactionDecision.TransactionClassification c) {
    return switch (c) {
      case APPROVED -> 0;
      case SUSPICIOUS -> 1;
      case FRAUD -> 2;
    };
  }

  private boolean hasAllRequiredFields(
      TransactionRequest request, RuleCandidateIndexHelper.RulePreconditions pre) {
    if (pre.requiredFields().isEmpty()) {
      return true;
    }
    for (String f : pre.requiredFields()) {
      Object v = readFieldValue(request, TransactionRequest.class, f);
      if (v == null) {
        return false;
      }
    }
    return true;
  }

  private RuleMatch evaluateRuleVelocity(TransactionRequest request, RuleConfiguration rule) {
    if (rule == null) return null;
    String parameters = rule.getParameters();
    if (parameters == null || parameters.isBlank()) return null;

    VelocitySpec spec;
    try {
      var root = objectMapper.readTree(parameters);
      var v = root.get("velocity");
      if (v == null || v.isNull()) return null;
      spec = VelocitySpec.fromJson(v);
    } catch (Exception e) {
      return new RuleMatch(false, "velocity.parameters inválido (JSON parse falhou)");
    }

    LocalDateTime now = LocalDateTime.now(clock);
    LocalDateTime since = now.minusSeconds(Math.max(0L, spec.windowSeconds));

    String detailPrefix =
        "velocity " + spec.metric + " " + spec.dimension + " windowSeconds=" + spec.windowSeconds;

    return switch (spec.metric) {
      case COUNT -> {
        long count =
            switch (spec.dimension) {
              case CUSTOMER -> {
                String key = request.getCustomerIdFromHeader();
                if (key == null || key.isBlank()) yield 0L;
                Long c = transactionRepository.countTransactionsByCustomerSince(key, since);
                yield c == null ? 0L : c;
              }
              case MERCHANT -> {
                String key = request.getMerchantId();
                if (key == null || key.isBlank()) yield 0L;
                Long c = transactionRepository.countTransactionsByMerchantSince(key, since);
                yield c == null ? 0L : c;
              }
              case GLOBAL -> {
                Long c = transactionRepository.countSince(since);
                yield c == null ? 0L : c;
              }
            };
        boolean ok = compareLong(count, spec.operator, spec.thresholdLong);
        yield new RuleMatch(
            ok,
            detailPrefix
                + " operator="
                + spec.operator
                + " threshold="
                + spec.thresholdLong
                + " (actual="
                + count
                + ") => "
                + ok);
      }
      case SUM_AMOUNT -> {
        BigDecimal sum =
            switch (spec.dimension) {
              case CUSTOMER -> {
                String key = request.getCustomerIdFromHeader();
                if (key == null || key.isBlank()) yield BigDecimal.ZERO;
                BigDecimal s = transactionRepository.sumAmountByCustomerSince(key, since);
                yield s == null ? BigDecimal.ZERO : s;
              }
              case MERCHANT -> {
                String key = request.getMerchantId();
                if (key == null || key.isBlank()) yield BigDecimal.ZERO;
                BigDecimal s = transactionRepository.sumAmountByMerchantSince(key, since);
                yield s == null ? BigDecimal.ZERO : s;
              }
              case GLOBAL -> {
                BigDecimal s = transactionRepository.sumAmountSince(since);
                yield s == null ? BigDecimal.ZERO : s;
              }
            };
        boolean ok = compareBigDecimal(sum, spec.operator, spec.thresholdAmount);
        yield new RuleMatch(
            ok,
            detailPrefix
                + " operator="
                + spec.operator
                + " threshold="
                + spec.thresholdAmount
                + " (actual="
                + sum
                + ") => "
                + ok);
      }
    };
  }

  private boolean compareLong(long actual, String operator, long threshold) {
    return conditionMatcher.compareLong(actual, operator, threshold);
  }

  private boolean compareBigDecimal(BigDecimal actual, String operator, BigDecimal threshold) {
    return conditionMatcher.compareBigDecimal(actual, operator, threshold);
  }

  private static final class VelocitySpec {
    enum Metric {
      COUNT,
      SUM_AMOUNT
    }

    enum Dimension {
      CUSTOMER,
      MERCHANT,
      GLOBAL
    }

    final Metric metric;
    final Dimension dimension;
    final long windowSeconds;
    final String operator;
    final long thresholdLong;
    final BigDecimal thresholdAmount;

    private VelocitySpec(
        Metric metric,
        Dimension dimension,
        long windowSeconds,
        String operator,
        long thresholdLong,
        BigDecimal thresholdAmount) {
      this.metric = metric;
      this.dimension = dimension;
      this.windowSeconds = windowSeconds;
      this.operator = operator;
      this.thresholdLong = thresholdLong;
      this.thresholdAmount = thresholdAmount;
    }

    static VelocitySpec fromJson(com.fasterxml.jackson.databind.JsonNode v) {
      String metricRaw = v.path("metric").asText("COUNT");
      String dimRaw = v.path("dimension").asText("CUSTOMER");
      long windowSeconds = v.path("windowSeconds").asLong(3600);
      String operator = v.path("operator").asText("GT");

      Metric metric = Metric.valueOf(metricRaw.trim().toUpperCase(java.util.Locale.ROOT));
      Dimension dimension = Dimension.valueOf(dimRaw.trim().toUpperCase(java.util.Locale.ROOT));

      if (metric == Metric.SUM_AMOUNT) {
        BigDecimal threshold = new BigDecimal(v.path("threshold").asText("0"));
        return new VelocitySpec(metric, dimension, windowSeconds, operator, 0L, threshold);
      }
      long threshold = v.path("threshold").asLong(0L);
      return new VelocitySpec(metric, dimension, windowSeconds, operator, threshold, null);
    }
  }

  private Object readFieldValue(Object subject, Class<?> subjectClass, String fieldName) {
    try {
      PropertyDescriptor pd = new PropertyDescriptor(fieldName, subjectClass);
      return pd.getReadMethod().invoke(subject);
    } catch (Exception e) {
      return null;
    }
  }

  private void recordTransactionInGraph(TransactionRequest request, TransactionDecision decision) {
    String fromAccount = extractSourceAccount(request);
    String toAccount = extractDestinationEntity(request);

    if (fromAccount == null || toAccount == null || fromAccount.isBlank() || toAccount.isBlank()) {
      log.debug(
          "Neo4j graph tracking skipped: missing entity info (from={}, to={})",
          fromAccount,
          toAccount);
      return;
    }

    double amount =
        request.getTransactionAmount() != null ? request.getTransactionAmount().doubleValue() : 0.0;
    long timestamp = System.currentTimeMillis();

    neo4jGraphService.recordTransaction(fromAccount, toAccount, amount, timestamp);

    log.debug(
        "Neo4j graph: recorded transaction {} -> {} amount={} decision={}",
        fromAccount,
        toAccount,
        amount,
        decision.getClassification());
  }

  private String extractSourceAccount(TransactionRequest request) {
    if (request.getCustomerAcctNumber() != null) {
      return "ACCT_" + request.getCustomerAcctNumber();
    }
    if (request.getCustomerIdFromHeader() != null && !request.getCustomerIdFromHeader().isBlank()) {
      return "CUST_" + request.getCustomerIdFromHeader();
    }
    return null;
  }

  private String extractDestinationEntity(TransactionRequest request) {
    if (request.getMerchantId() != null && !request.getMerchantId().isBlank()) {
      return "MERCH_" + request.getMerchantId();
    }
    if (request.getAcquirerId() != null && !request.getAcquirerId().isBlank()) {
      return "ACQ_" + request.getAcquirerId();
    }
    return null;
  }

  private record RuleMatch(boolean triggered, String detail) {}
}
