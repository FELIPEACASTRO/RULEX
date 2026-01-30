package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.core.engine.model.RuleEvaluationResult;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.entity.TransactionRawStore;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import com.rulex.service.engine.ConditionMatcher;
import com.rulex.service.engine.ContractValidationHelper;
import com.rulex.service.engine.RuleCandidateIndexHelper;
import com.rulex.service.engine.RuleEngineConditionHelper;
import com.rulex.service.engine.RuleEngineDecisionHelper;
import com.rulex.service.engine.RuleEngineLegacyRuleHelper;
import com.rulex.service.engine.RuleEnginePrecheckHelper;
import com.rulex.service.engine.RuleEngineResponseBuilder;
import com.rulex.service.engine.ShadowRuleExecutionHelper;
import com.rulex.service.enrichment.TransactionEnrichmentFacade;
import com.rulex.util.PanMaskingUtil;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço que implementa o motor de regras duras para análise de fraude. Avalia transações contra
 * regras configuráveis e retorna uma classificação.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RuleEngineService implements com.rulex.core.engine.port.RuleEngineInputPort {

  private final TransactionRepository transactionRepository;
  private final TransactionDecisionRepository decisionRepository;
  private final RuleConfigurationRepository ruleConfigRepository;
  private final AuditService auditService;
  private final ObjectMapper objectMapper;
  private final Clock clock;
  private final PayloadHashService payloadHashService;
  private final TransactionRawStoreService rawStoreService;
  private final RuleExecutionLogService ruleExecutionLogService;
  private final EnrichmentService enrichmentService;
  private final RuleOrderingService ruleOrderingService;
  private final TransactionEnrichmentFacade transactionEnrichmentFacade;
  private final RuleEngineResponseBuilder responseBuilder;
  private final RuleEngineConditionHelper conditionHelper;
  private final ContractValidationHelper contractValidationHelper;
  private final RuleEngineDecisionHelper decisionHelper;
  private final RuleEnginePrecheckHelper precheckHelper;
  private final ShadowRuleExecutionHelper shadowRuleExecutionHelper;
  private final RuleEngineLegacyRuleHelper legacyRuleHelper;
  private final RuleCandidateIndexHelper candidateIndexHelper;

  // V4.0: advanced hard-rule services (opt-in via config)
  private final BloomFilterService bloomFilterService;
  private final ImpossibleTravelService impossibleTravelService;
  private final GeoService geoService;
  private final VelocityServiceFacade velocityServiceFacade;
  private final Neo4jGraphService neo4jGraphService;

  // ARCH-003: Extracted helper classes
  private final ConditionMatcher conditionMatcher;

  /**
   * When enabled, rules are evaluated in an optimized (cheap-first / hit-rate) order.
   *
   * <p>Default is false to preserve historical auditing semantics (triggeredRules/scoreDetails
   * ordering) unless explicitly enabled.
   */
  @Value("${rulex.engine.optimizedRuleOrder:false}")
  private boolean optimizedRuleOrder;

  @Value("${rulex.engine.bloomFilter.enabled:true}")
  private boolean bloomFilterEnabled;

  @Value("${rulex.engine.shadowMode.enabled:true}")
  private boolean shadowModeEnabled;

  @Value("${rulex.engine.impossibleTravel.enabled:false}")
  private boolean impossibleTravelEnabled;

  @Value("${rulex.engine.velocity.redis.enabled:false}")
  private boolean redisVelocityEnabled;

  @Value("${rulex.neo4j.graph-tracking.enabled:true}")
  private boolean neo4jGraphTrackingEnabled;

  /** Processa uma transação e retorna a classificação de fraude. */
  public TransactionResponse analyzeTransaction(TransactionRequest request) {
    try {
      // Fallback for non-HTTP/internal callers (e.g., unit tests). This is NOT "as received".
      byte[] derived = objectMapper.writeValueAsBytes(request);
      return analyzeTransaction(request, derived, "application/json; charset=utf-8");
    } catch (Exception e) {
      throw new RuntimeException("Erro ao serializar payload para fallback interno", e);
    }
  }

  /**
   * V3.1 entrypoint: receives raw HTTP body bytes (as received) to compute SHA-256 and persist. If
   * rawBytes is null (e.g., internal callers/tests), falls back to a canonical JSON string.
   */
  public TransactionResponse analyzeTransaction(
      TransactionRequest request, byte[] rawBytes, String contentType) {
    long startTime = System.currentTimeMillis();

    try {
      String externalTransactionId = request.getExternalTransactionId();
      byte[] effectiveRawBytes = rawBytes == null ? new byte[0] : rawBytes;
      String payloadHash = payloadHashService.sha256Hex(effectiveRawBytes);

      // V3.1: anti-tamper/idempotency source of truth is raw store keyed by externalTransactionId.
      java.util.Optional<TransactionRawStore> existingRawOpt =
          rawStoreService.findByExternalTransactionId(externalTransactionId);
      if (existingRawOpt.isPresent()) {
        TransactionRawStore existingRaw = existingRawOpt.get();
        if (!existingRaw.getPayloadRawHash().equals(payloadHash)) {
          // anti-tamper MUST return FRAUD, not HTTP 409
          auditService.logTamperAttempt(
              externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);

          safeLogAntiTamper(externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);

          TransactionDecision tamperDecision =
              decisionHelper.buildTamperDecision(externalTransactionId, payloadHash);
            return responseBuilder.buildResponseFromTamperDecision(tamperDecision, startTime);
        }

        // Idempotency: return same previous decision (no recomputation)
        Optional<Transaction> existingTxOpt =
            transactionRepository.findByExternalTransactionId(externalTransactionId);
        if (existingTxOpt.isPresent()) {
          return responseBuilder.buildResponseFromExisting(existingTxOpt.get(), startTime);
        }
        // Raw store exists but transaction row is missing (partial failure recovery).
        // Continue to create the transaction normally - raw store already validated hash match.
        log.warn(
            "Raw store exists but transaction row missing for externalTransactionId={}. Recovering by creating transaction.",
            externalTransactionId);
      }

      // Store raw payload in an independent transaction for auditability (skip if already present
      // from recovery case).
      if (!existingRawOpt.isPresent()) {
        rawStoreService.store(externalTransactionId, payloadHash, effectiveRawBytes, contentType);
      }

      // Backward compatibility: if transaction already exists (race), ensure hash matches.
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

      // 1. Salvar a transação
      Transaction transaction = convertRequestToEntity(request);
      transaction.setPayloadRawHash(payloadHash);
      try {
        transaction = transactionRepository.save(transaction);
      } catch (DataIntegrityViolationException e) {
        // Condição de corrida: outra thread/processo inseriu o mesmo externalTransactionId.
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

      // 2. Avaliar regras
      RuleEvaluationResult result = evaluateRules(transaction, request);

      // 3. Salvar decisão
      TransactionDecision decision = decisionHelper.createDecision(transaction, result);
      decision.setExternalTransactionId(transaction.getExternalTransactionId());
      decision.setPayloadRawHash(payloadHash);
      decisionRepository.save(decision);

      // V4.0: record transaction into VelocityServiceFacade (Redis real + DB)
      if (redisVelocityEnabled) {
        try {
          velocityServiceFacade.recordTransaction(
              request,
              decision.getClassification() != null ? decision.getClassification().name() : "UNKNOWN",
              decision.getRiskScore());
        } catch (Exception e) { // SEC-006 FIX
          log.warn("Erro best-effort velocity tracking (não bloqueia decisão): {}", e.getMessage());
        }
      }

      // V5.0: record transaction into Neo4j Graph for fraud ring detection
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

      // 4. Registrar auditoria
      auditService.logTransactionProcessed(transaction, decision, result);

      // 5. Construir resposta
      long processingTime = System.currentTimeMillis() - startTime;
      return responseBuilder.buildResponse(transaction, decision, result, processingTime);

    } catch (Exception e) {
      log.error("Erro ao processar transação: {}", request.getExternalTransactionId(), e);
      auditService.logError(request.getExternalTransactionId(), e);
      throw new RuntimeException("Erro ao processar transação", e);
    }
  }

  /**
   * Avalia uma transação e retorna decisão final + rule hits + popups agregados.
   *
   * <p>Observação: mantém o mesmo comportamento de persistência/idempotência de {@link
   * #analyzeTransaction(TransactionRequest)}.
   */
  public EvaluateResponse evaluate(TransactionRequest request) {
    try {
      // Fallback for non-HTTP/internal callers (e.g., unit tests). This is NOT "as received".
      byte[] derived = objectMapper.writeValueAsBytes(request);
      return evaluate(request, derived, "application/json; charset=utf-8");
    } catch (Exception e) {
      throw new RuntimeException("Erro ao serializar payload para fallback interno", e);
    }
  }

  public EvaluateResponse evaluate(
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
            return responseBuilder.buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }

        Optional<Transaction> existingTxOpt =
            transactionRepository.findByExternalTransactionId(externalTransactionId);
        if (existingTxOpt.isPresent()) {
          return responseBuilder.buildEvaluateResponseFromExisting(existingTxOpt.get(), startTime);
        }
        // Raw store exists but transaction row is missing (partial failure recovery).
        // Continue to create the transaction normally - raw store already validated hash match.
        log.warn(
            "Raw store exists but transaction row missing for externalTransactionId={}. Recovering by creating transaction.",
            externalTransactionId);
      }

      // Only store raw payload if not already present (recovery case skips this)
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

      // 1. Salvar a transação
      Transaction transaction = convertRequestToEntity(request);
      transaction.setPayloadRawHash(payloadHash);
      try {
        transaction = transactionRepository.save(transaction);
      } catch (DataIntegrityViolationException e) {
        // Condição de corrida: outra thread/processo inseriu o mesmo externalTransactionId.
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

      // 2. Avaliar regras
      RuleEvaluationResult result = evaluateRules(transaction, request);

      // 3. Salvar decisão
      TransactionDecision decision = decisionHelper.createDecision(transaction, result);
      decision.setExternalTransactionId(transaction.getExternalTransactionId());
      decision.setPayloadRawHash(payloadHash);
      decisionRepository.save(decision);

      // V4.0: record transaction into VelocityServiceFacade (Redis real + DB)
      if (redisVelocityEnabled) {
        try {
          velocityServiceFacade.recordTransaction(
              request,
              decision.getClassification() != null ? decision.getClassification().name() : "UNKNOWN",
              decision.getRiskScore());
        } catch (Exception e) { // SEC-006 FIX
          log.warn("Erro best-effort velocity tracking (não bloqueia decisão): {}", e.getMessage());
        }
      }

      // V5.0: record transaction into Neo4j Graph for fraud ring detection
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

      // 4. Registrar auditoria
      auditService.logTransactionProcessed(transaction, decision, result);

      // 5. Construir resposta
      long processingTime = System.currentTimeMillis() - startTime;
      return responseBuilder.buildEvaluateResponse(transaction, decision, result, processingTime);

    } catch (Exception e) {
      log.error("Erro ao avaliar transação: {}", request.getExternalTransactionId(), e);
      auditService.logError(request.getExternalTransactionId(), e);
      throw new RuntimeException("Erro ao avaliar transação", e);
    }
  }

  /**
   * V3.1 endpoint entrypoint for /evaluate.
   *
   * <p>Accepts the raw request body string (as received) and MUST NOT use Bean Validation to reject
   * requests with 400. Any malformed/partial payload is classified deterministically as
   * SUSPICIOUS/FRAUD via synthetic "contract" hits.
   */
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

    // If the payload is missing required fields for persistence, we still must return a decision
    // (no 400). In that scenario we classify as SUSPICIOUS and skip DB writes that would violate
    // NOT NULL constraints in transactions.
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

    // Normal path (raw bytes required by V3.1).
    return evaluate(parsed, effectiveRawBytes, contentType);
  }

  /** Avalia as regras configuradas contra a transação. */
  private RuleEvaluationResult evaluateRules(Transaction transaction, TransactionRequest request) {
    RuleEvaluationResult result = new RuleEvaluationResult();
    List<RuleConfiguration> enabledRules =
        optimizedRuleOrder
            ? ruleOrderingService.getOptimizedRuleOrder()
            : ruleConfigRepository.findByEnabled(true);

    RuleCandidateIndexHelper.CandidateIndex index =
      candidateIndexHelper.buildOrReuseCandidateIndex(enabledRules);

    // V3.2: Derivar tipos ricos do payload sem alterar o payload original
    DerivedContext derivedContext = DerivedContext.from(request);
    log.debug(
        "DerivedContext criado: transactionTs={}, bin={}, maskedPan={}",
        derivedContext.getTransactionTimestamp(),
        derivedContext.getBin(),
        derivedContext.getMaskedPan());

    // V4.1: Enriquecimento completo da transação usando TransactionEnrichmentFacade
    // Isso disponibiliza todos os 103+ campos derivados para avaliação de regras
    TransactionEnrichmentFacade.FullEnrichmentContext enrichmentContext =
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

    // V4.0: cheap deterministic pre-checks (short-circuit on FRAUD)
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

    // V4.0: separate decision rules from shadow rules
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
      RuleCandidateIndexHelper.RulePreconditions pre =
          index.byRuleName().get(rule.getRuleName());
      if (pre != null && pre.canSkipWithMissingFields() && !hasAllRequiredFields(request, pre)) {
        // Maintain parity with existing scoreDetails shape.
        scoreDetails.put(rule.getRuleName(), Map.of("triggered", false));
        continue;
      }

      long startNanos = optimizedRuleOrder ? System.nanoTime() : 0L;
      RuleMatch ruleMatch = evaluateRuleGeneric(transaction, request, rule);
      if (optimizedRuleOrder) {
        long elapsed = System.nanoTime() - startNanos;
        try {
          ruleOrderingService.recordExecution(rule.getRuleName(), elapsed, ruleMatch.triggered);
        } catch (Exception e) { // SEC-006 FIX
          // ordering is best-effort; never break fraud decisions
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

        // V3.1: short-circuit on FRAUD.
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

    // Normalizar score para 0-100
    totalScore = Math.min(totalScore, 100);

    result.setRiskScore(totalScore);
    result.setTriggeredRules(triggeredRules);
    result.setScoreDetails(scoreDetails);
    // V3.1: decision is ONLY based on severity of triggered rules (score is telemetry).
    result.setClassification(maxByRule);
    result.setReason(generateReason(maxByRule, triggeredRules));

    // V4.0: evaluate SHADOW/CANARY-not-selected rules asynchronously (never impacts decision)
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

      shadowRuleExecutionHelper.executeShadowRules(
          shadowRules, request, actualDecision, evaluator);
    }

    return result;
  }

  private boolean isInCanary(String stableKey, int percentage) {
    if (percentage <= 0) return false;
    if (percentage >= 100) return true;
    int bucket = Math.floorMod(stableKey.hashCode(), 100);
    return bucket < percentage;
  }

  /** Avalia uma regra específica contra a transação. */
  private RuleMatch evaluateRuleGeneric(
      Transaction transaction, TransactionRequest request, RuleConfiguration rule) {
    // 0) VELOCITY/state: regras com janela temporal (exigem persistência/consulta).
    if (rule.getRuleType() == RuleConfiguration.RuleType.VELOCITY) {
      RuleMatch velocity = evaluateRuleVelocity(request, rule);
      if (velocity != null) {
        return velocity;
      }
    }

    // 1) Preferir condições genéricas (configuráveis)
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
              // AND: early exit
              break;
            }
          } else {
            triggered = triggered || condResult;
            if (triggered) {
              // OR: early exit
              break;
            }
          }
        }

        return new RuleMatch(triggered, String.join(" | ", explanations));
      }
    }

    // 2) Fallback legada por nome (compatibilidade)
    RuleEngineLegacyRuleHelper.LegacyRuleResult legacyResult =
        legacyRuleHelper.evaluateLegacyRule(request, rule);

    return new RuleMatch(legacyResult.triggered(), legacyResult.detail());
  }

  /** Gera uma descrição do motivo da decisão. */
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

  /** Converte TransactionRequest para Transaction entity. */
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
  /**
   * VELOCITY/state (janela temporal) baseado em persistência.
   *
   * <p>Config em {@code RuleConfiguration.parameters} (JSON):
   *
   * <pre>
   * {
   *   "velocity": {
   *     "metric": "COUNT" | "SUM_AMOUNT",
   *     "dimension": "CUSTOMER" | "MERCHANT" | "GLOBAL",
   *     "windowSeconds": 3600,
   *     "operator": "GT" | "GTE" | "LT" | "LTE" | "EQ" | "NE",
   *     "threshold": 3
   *   }
   * }
   * </pre>
   */
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
      // parameters inválido: não dispara regra e não derruba o pipeline.
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

  /**
   * V5.0: Record transaction into Neo4j graph for fraud ring detection. Extracts source and
   * destination entities from the request and creates relationship in the graph database.
   */
  private void recordTransactionInGraph(TransactionRequest request, TransactionDecision decision) {
    String fromAccount = extractSourceAccount(request);
    String toAccount = extractDestinationEntity(request);

    // Skip if either entity is missing - can't create valid relationship
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

  /**
   * Extract source account identifier from transaction request. Uses customerAcctNumber or
   * customerIdFromHeader.
   */
  private String extractSourceAccount(TransactionRequest request) {
    // Prefer customerAcctNumber as primary account identifier
    if (request.getCustomerAcctNumber() != null) {
      return "ACCT_" + request.getCustomerAcctNumber();
    }
    // Fallback to customerIdFromHeader
    if (request.getCustomerIdFromHeader() != null && !request.getCustomerIdFromHeader().isBlank()) {
      return "CUST_" + request.getCustomerIdFromHeader();
    }
    return null;
  }

  /**
   * Extract destination entity from transaction request. For credit transactions, the destination
   * is typically the merchant.
   */
  private String extractDestinationEntity(TransactionRequest request) {
    // For most transactions, the destination is the merchant
    if (request.getMerchantId() != null && !request.getMerchantId().isBlank()) {
      return "MERCH_" + request.getMerchantId();
    }
    // Fallback to acquirer if merchant is not available
    if (request.getAcquirerId() != null && !request.getAcquirerId().isBlank()) {
      return "ACQ_" + request.getAcquirerId();
    }
    return null;
  }

  private record RuleMatch(boolean triggered, String detail) {}
}
