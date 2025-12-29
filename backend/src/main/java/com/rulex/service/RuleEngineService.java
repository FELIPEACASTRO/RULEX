package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.PopupDTO;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleHitDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.entity.TransactionRawStore;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import com.rulex.util.PanMaskingUtil;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
public class RuleEngineService {

  private final TransactionRepository transactionRepository;
  private final TransactionDecisionRepository decisionRepository;
  private final RuleConfigurationRepository ruleConfigRepository;
  private final AuditService auditService;
  private final ObjectMapper objectMapper;
  private final Clock clock;
  private final PayloadHashService payloadHashService;
  private final TransactionRawStoreService rawStoreService;
  private final RuleExecutionLogService ruleExecutionLogService;

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
              buildTamperDecision(externalTransactionId, payloadHash);
          return buildResponseFromTamperDecision(tamperDecision, startTime);
        }

        // Idempotency: return same previous decision (no recomputation)
        Transaction existingTx =
            transactionRepository
                .findByExternalTransactionId(externalTransactionId)
                .orElseThrow(
                    () ->
                        new IllegalStateException(
                            "Raw store exists but transaction row is missing for externalTransactionId="
                                + externalTransactionId));
        return buildResponseFromExisting(existingTx, startTime);
      }

      // Store raw payload in an independent transaction for auditability.
      rawStoreService.store(externalTransactionId, payloadHash, effectiveRawBytes, contentType);

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
              buildTamperDecision(externalTransactionId, payloadHash);
          return buildResponseFromTamperDecision(tamperDecision, startTime);
        }
        return buildResponseFromExisting(existing, startTime);
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
              buildTamperDecision(externalTransactionId, payloadHash);
          return buildResponseFromTamperDecision(tamperDecision, startTime);
        }
        return buildResponseFromExisting(racedTx, startTime);
      }

      // 2. Avaliar regras
      RuleEvaluationResult result = evaluateRules(transaction, request);

      // 3. Salvar decisão
      TransactionDecision decision = createDecision(transaction, result);
      decision.setExternalTransactionId(transaction.getExternalTransactionId());
      decision.setPayloadRawHash(payloadHash);
      decisionRepository.save(decision);

      safeLogEvaluate(
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
      return buildResponse(transaction, decision, result, processingTime);

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
              buildTamperDecision(externalTransactionId, payloadHash);
          return buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }

        Transaction existingTx =
            transactionRepository
                .findByExternalTransactionId(externalTransactionId)
                .orElseThrow(
                    () ->
                        new IllegalStateException(
                            "Raw store exists but transaction row is missing for externalTransactionId="
                                + externalTransactionId));
        return buildEvaluateResponseFromExisting(existingTx, startTime);
      }

      rawStoreService.store(externalTransactionId, payloadHash, effectiveRawBytes, contentType);

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
              buildTamperDecision(externalTransactionId, payloadHash);
          return buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }
        return buildEvaluateResponseFromExisting(existing, startTime);
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
              buildTamperDecision(externalTransactionId, payloadHash);
          return buildEvaluateResponseFromTamperDecision(tamperDecision, startTime);
        }
        return buildEvaluateResponseFromExisting(racedTx, startTime);
      }

      // 2. Avaliar regras
      RuleEvaluationResult result = evaluateRules(transaction, request);

      // 3. Salvar decisão
      TransactionDecision decision = createDecision(transaction, result);
      decision.setExternalTransactionId(transaction.getExternalTransactionId());
      decision.setPayloadRawHash(payloadHash);
      decisionRepository.save(decision);

      safeLogEvaluate(
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
      return buildEvaluateResponse(transaction, decision, result, processingTime);

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
      safeLogContractError(
          null, payloadHash, "CONTRACT_INVALID_JSON", "Payload JSON inválido (parse falhou)");
      return buildContractErrorEvaluateResponse(
          "CONTRACT_INVALID_JSON",
          "Payload JSON inválido (parse falhou)",
          TransactionDecision.TransactionClassification.FRAUD,
          payloadHash,
          startTime);
    }

    String externalTransactionId = parsed == null ? null : parsed.getExternalTransactionId();
    if (externalTransactionId == null || externalTransactionId.isBlank()) {
      safeLogContractError(
          null,
          payloadHash,
          "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID",
          "externalTransactionId ausente ou vazio");
      return buildContractErrorEvaluateResponse(
          "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID",
          "externalTransactionId ausente ou vazio",
          TransactionDecision.TransactionClassification.FRAUD,
          payloadHash,
          startTime);
    }

    // If the payload is missing required fields for persistence, we still must return a decision
    // (no 400). In that scenario we classify as SUSPICIOUS and skip DB writes that would violate
    // NOT NULL constraints in transactions.
    List<String> missing = contractMissingRequiredForPersistence(parsed);
    if (!missing.isEmpty()) {
      safeLogContractError(
          externalTransactionId,
          payloadHash,
          "CONTRACT_MISSING_REQUIRED_FIELDS",
          "Campos obrigatórios ausentes para persistência: " + String.join(", ", missing));
      return buildContractErrorEvaluateResponse(
          "CONTRACT_MISSING_REQUIRED_FIELDS",
          "Campos obrigatórios ausentes para persistência: " + String.join(", ", missing),
          TransactionDecision.TransactionClassification.SUSPICIOUS,
          payloadHash,
          startTime);
    }

    // Normal path (raw bytes required by V3.1).
    return evaluate(parsed, effectiveRawBytes, contentType);
  }

  private List<String> contractMissingRequiredForPersistence(TransactionRequest request) {
    List<String> missing = new ArrayList<>();
    if (isBlank(request.getCustomerIdFromHeader())) missing.add("customerIdFromHeader");
    if (request.getCustomerAcctNumber() == null) missing.add("customerAcctNumber");
    if (isBlank(request.getPan())) missing.add("pan");
    if (request.getTransactionCurrencyCode() == null) missing.add("transactionCurrencyCode");
    if (request.getTransactionAmount() == null) missing.add("transactionAmount");
    if (request.getTransactionDate() == null) missing.add("transactionDate");
    if (request.getTransactionTime() == null) missing.add("transactionTime");
    if (request.getMcc() == null) missing.add("mcc");
    if (request.getConsumerAuthenticationScore() == null)
      missing.add("consumerAuthenticationScore");
    if (request.getExternalScore3() == null) missing.add("externalScore3");
    if (request.getCavvResult() == null) missing.add("cavvResult");
    if (request.getEciIndicator() == null) missing.add("eciIndicator");
    if (request.getAtcCard() == null) missing.add("atcCard");
    if (request.getAtcHost() == null) missing.add("atcHost");
    if (request.getTokenAssuranceLevel() == null) missing.add("tokenAssuranceLevel");
    if (request.getAvailableCredit() == null) missing.add("availableCredit");
    if (request.getCardCashBalance() == null) missing.add("cardCashBalance");
    if (request.getCardDelinquentAmount() == null) missing.add("cardDelinquentAmount");
    return missing;
  }

  private boolean isBlank(String s) {
    return s == null || s.isBlank();
  }

  private EvaluateResponse buildContractErrorEvaluateResponse(
      String ruleName,
      String detail,
      TransactionDecision.TransactionClassification classification,
      String payloadHash,
      long startTime) {

    RuleHitDTO hit =
        RuleHitDTO.builder()
            .ruleName(ruleName)
            .description(detail)
            .ruleType("CONTRACT")
            .classification(classification.name())
            .threshold(null)
            .weight(100)
            .contribution(0)
            .detail(detail)
            .build();

    PopupDTO popup =
        PopupDTO.builder()
            .key(classification.name())
            .title(
                classification == TransactionDecision.TransactionClassification.FRAUD
                    ? "Bloqueio"
                    : "Suspeita")
            .classification(classification.name())
            .totalContribution(0)
            .rules(List.of(hit))
            .reason(detail)
            .build();

    long processingTime = System.currentTimeMillis() - startTime;
    return EvaluateResponse.builder()
        .transactionId(null)
        .classification(classification.name())
        .riskScore(0)
        .reason(detail)
        .rulesetVersion("contract")
        .processingTimeMs(processingTime)
        .timestamp(LocalDateTime.now(clock))
        .ruleHits(List.of(hit))
        .popups(List.of(popup))
        .build();
  }

  /** Avalia as regras configuradas contra a transação. */
  private RuleEvaluationResult evaluateRules(Transaction transaction, TransactionRequest request) {
    RuleEvaluationResult result = new RuleEvaluationResult();
    List<RuleConfiguration> enabledRules = ruleConfigRepository.findByEnabled(true);

    int totalScore = 0;
    List<TriggeredRuleDTO> triggeredRules = new ArrayList<>();
    Map<String, Object> scoreDetails = new HashMap<>();
    TransactionDecision.TransactionClassification maxByRule =
        TransactionDecision.TransactionClassification.APPROVED;

    for (RuleConfiguration rule : enabledRules) {
      RuleMatch ruleMatch = evaluateRuleGeneric(transaction, request, rule);

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

    return result;
  }

  /** Avalia uma regra específica contra a transação. */
  private RuleMatch evaluateRuleGeneric(
      Transaction transaction, TransactionRequest request, RuleConfiguration rule) {
    // 1) Preferir condições genéricas (configuráveis)
    if (rule.getConditionsJson() != null && !rule.getConditionsJson().isBlank()) {
      List<RuleConditionDTO> conditions = readConditions(rule.getConditionsJson());
      RuleConfiguration.LogicOperator op =
          rule.getLogicOperator() != null
              ? rule.getLogicOperator()
              : RuleConfiguration.LogicOperator.AND;

      if (!conditions.isEmpty()) {
        List<String> explanations = new ArrayList<>();

        boolean triggered = (op == RuleConfiguration.LogicOperator.AND);
        for (RuleConditionDTO condition : conditions) {
          boolean condResult = evaluateCondition(request, condition);
          explanations.add(explainCondition(request, condition, condResult));

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
    boolean legacy =
        switch (rule.getRuleName()) {
          case "LOW_AUTHENTICATION_SCORE" ->
              request.getConsumerAuthenticationScore() < rule.getThreshold();

          case "LOW_EXTERNAL_SCORE" -> request.getExternalScore3() < rule.getThreshold();

          case "INVALID_CAVV" -> request.getCavvResult() != 0;

          case "INVALID_CRYPTOGRAM" -> !"V".equals(request.getCryptogramValid());

          case "CVV_MISMATCH" -> "N".equals(request.getCvv2Response());

          case "HIGH_TRANSACTION_AMOUNT" ->
              request.getTransactionAmount().doubleValue() > rule.getThreshold();

          case "HIGH_RISK_MCC" -> isHighRiskMcc(request.getMcc());

          case "INTERNATIONAL_TRANSACTION" -> isInternationalTransaction(request);

          case "CARD_NOT_PRESENT" -> !"Y".equals(request.getCustomerPresent());

          case "PIN_VERIFICATION_FAILED" -> "I".equals(request.getPinVerifyCode());

            // Mapear para os campos corretos (sem depender de códigos textuais inconsistentes)
          case "CVV_PIN_LIMIT_EXCEEDED" ->
              Integer.valueOf(1).equals(request.getCvvPinTryLimitExceeded());

          case "OFFLINE_PIN_FAILED" ->
              Integer.valueOf(1).equals(request.getCvrofflinePinVerificationPerformed())
                  && Integer.valueOf(1).equals(request.getCvrofflinePinVerificationFailed());

          default -> false;
        };

    return new RuleMatch(legacy, legacy ? "regra legada por nome" : null);
  }

  /** Verifica se o MCC é de alto risco. */
  private boolean isHighRiskMcc(Integer mcc) {
    // MCCs de alto risco: Jogos, Criptomoedas, Transferências de dinheiro, etc.
    Set<Integer> highRiskMccs =
        Set.of(
            7995, // Gambling
            6211, // Securities Brokers
            6051, // Crypto
            7273, // Dating Services
            7994 // Video Amusement
            );
    return highRiskMccs.contains(mcc);
  }

  /** Verifica se é uma transação internacional. */
  private boolean isInternationalTransaction(TransactionRequest request) {
    // Assumir que 076 é o código do Brasil
    return request.getMerchantCountryCode() != null
        && !request.getMerchantCountryCode().equals("076");
  }

  /** Classifica o risco baseado no score. */
  private TransactionDecision.TransactionClassification classifyRiskByScore(int riskScore) {
    if (riskScore < 30) {
      return TransactionDecision.TransactionClassification.APPROVED;
    }
    if (riskScore < 70) {
      return TransactionDecision.TransactionClassification.SUSPICIOUS;
    }
    return TransactionDecision.TransactionClassification.FRAUD;
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

  private TransactionDecision buildTamperDecision(
      String externalTransactionId, String newPayloadHash) {
    return TransactionDecision.builder()
        .transaction(
            transactionRepository
                .findByExternalTransactionId(externalTransactionId)
                .orElseThrow(
                    () ->
                        new IllegalStateException(
                            "Tamper detected but transaction row is missing for externalTransactionId="
                                + externalTransactionId)))
        .externalTransactionId(externalTransactionId)
        .payloadRawHash(newPayloadHash)
        .classification(TransactionDecision.TransactionClassification.FRAUD)
        .riskScore(100)
        .rulesApplied(writeJson(java.util.List.of()))
        .scoreDetails(writeJson(java.util.Map.of()))
        .reason("ANTI_TAMPER: externalTransactionId reutilizado com payload diferente")
        .rulesVersion("ANTI_TAMPER_V1")
        .createdAt(LocalDateTime.now(clock))
        .build();
  }

  private TransactionResponse buildResponseFromTamperDecision(
      TransactionDecision decision, long startTime) {
    long processingTime = System.currentTimeMillis() - startTime;
    return TransactionResponse.builder()
        .id(decision.getTransaction() != null ? decision.getTransaction().getId() : null)
        .transactionId(decision.getExternalTransactionId())
        .customerIdFromHeader(
            decision.getTransaction() != null ? decision.getTransaction().getCustomerIdFromHeader() : null)
        .merchantId(decision.getTransaction() != null ? decision.getTransaction().getMerchantId() : null)
        .merchantName(decision.getTransaction() != null ? decision.getTransaction().getMerchantName() : null)
        .transactionAmount(
            decision.getTransaction() != null ? decision.getTransaction().getTransactionAmount() : null)
        .transactionDate(
            decision.getTransaction() != null ? decision.getTransaction().getTransactionDate() : null)
        .transactionTime(
            decision.getTransaction() != null ? decision.getTransaction().getTransactionTime() : null)
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(java.util.List.of())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(OffsetDateTime.now(clock))
        .success(true)
        .build();
  }

  private EvaluateResponse buildEvaluateResponseFromTamperDecision(
      TransactionDecision decision, long startTime) {
    long processingTime = System.currentTimeMillis() - startTime;
    return EvaluateResponse.builder()
        .transactionId(decision.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(LocalDateTime.now(clock))
        .ruleHits(java.util.List.of())
        .popups(java.util.List.of())
        .build();
  }

  /** Cria a entidade de decisão. */
  private TransactionDecision createDecision(Transaction transaction, RuleEvaluationResult result) {
    return TransactionDecision.builder()
        .transaction(transaction)
        .externalTransactionId(transaction.getExternalTransactionId())
        .payloadRawHash(transaction.getPayloadRawHash())
        .classification(result.getClassification())
        .riskScore(result.getRiskScore())
        .rulesApplied(writeJson(result.getTriggeredRules()))
        .scoreDetails(writeJson(result.getScoreDetails()))
        .reason(result.getReason())
        .rulesVersion("1")
        .createdAt(LocalDateTime.now(clock))
        .build();
  }

  private String writeJson(Object value) {
    try {
      return objectMapper.writeValueAsString(value);
    } catch (Exception e) {
      log.error("Erro ao serializar JSON", e);
      return "{}";
    }
  }

  private void safeLogEvaluate(
      ExecutionEventType eventType,
      String externalTransactionId,
      String payloadHash,
      TransactionDecision decision,
      List<?> rulesFired,
      com.fasterxml.jackson.databind.JsonNode errorJson) {
    try {
      ruleExecutionLogService.logEvaluate(
          eventType,
          externalTransactionId,
          payloadHash,
          decision != null ? decision.getClassification() : null,
          decision != null ? decision.getRiskScore() : 0,
          rulesFired,
          errorJson);
    } catch (DataAccessException e) {
      log.debug("Falha ao registrar rule_execution_log (ignorado)", e);
    }
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

  private void safeLogContractError(
      String externalTransactionId, String payloadHash, String code, String message) {
    try {
      var errorJson = objectMapper.createObjectNode();
      errorJson.put("code", code);
      errorJson.put("message", message);

      TransactionDecision.TransactionClassification classification =
          "CONTRACT_MISSING_REQUIRED_FIELDS".equals(code)
              ? TransactionDecision.TransactionClassification.SUSPICIOUS
              : TransactionDecision.TransactionClassification.FRAUD;

      ruleExecutionLogService.logEvaluate(
          ExecutionEventType.EVALUATE,
          externalTransactionId,
          payloadHash,
          classification,
          0,
          List.of(),
          errorJson);
    } catch (DataAccessException e) {
      log.debug("Falha ao registrar rule_execution_log contract error (ignorado)", e);
    }
  }

  /** Constrói a resposta da análise. */
  private TransactionResponse buildResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime) {
    return TransactionResponse.builder()
        .id(transaction.getId())
        .transactionId(transaction.getExternalTransactionId())
        .customerIdFromHeader(transaction.getCustomerIdFromHeader())
        .merchantId(transaction.getMerchantId())
        .merchantName(transaction.getMerchantName())
        .transactionAmount(transaction.getTransactionAmount())
        .transactionDate(transaction.getTransactionDate())
        .transactionTime(transaction.getTransactionTime())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(result.getTriggeredRules())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(OffsetDateTime.now(clock))
        .success(true)
        .build();
  }

  private EvaluateResponse buildEvaluateResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime) {
    List<RuleHitDTO> ruleHits = enrichRuleHits(result.getTriggeredRules());
    List<PopupDTO> popups = aggregatePopups(ruleHits);

    return EvaluateResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  private EvaluateResponse buildEvaluateResponseFromExisting(
      Transaction transaction, long startTime) {
    TransactionDecision decision =
        decisionRepository
            .findByTransactionId(transaction.getId())
            .orElseThrow(
                () -> new IllegalStateException("Transação existe sem decisão registrada"));

    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());
    List<RuleHitDTO> ruleHits = enrichRuleHits(triggeredRules);
    List<PopupDTO> popups = aggregatePopups(ruleHits);

    safeLogEvaluate(
        ExecutionEventType.IDEMPOTENT_REPLAY,
        transaction.getExternalTransactionId(),
        decision.getPayloadRawHash() != null
            ? decision.getPayloadRawHash()
            : transaction.getPayloadRawHash(),
        decision,
        triggeredRules,
        null);

    return EvaluateResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  private List<RuleHitDTO> enrichRuleHits(List<TriggeredRuleDTO> triggeredRules) {
    if (triggeredRules == null || triggeredRules.isEmpty()) {
      return List.of();
    }

    List<String> names =
        triggeredRules.stream()
            .map(TriggeredRuleDTO::getName)
            .filter(Objects::nonNull)
            .distinct()
            .toList();
    Map<String, RuleConfiguration> byName =
        ruleConfigRepository.findByRuleNameIn(names).stream()
            .collect(
                java.util.stream.Collectors.toMap(
                    RuleConfiguration::getRuleName, Function.identity(), (a, b) -> a));

    List<RuleHitDTO> hits = new ArrayList<>();
    for (TriggeredRuleDTO tr : triggeredRules) {
      RuleConfiguration rc = tr.getName() != null ? byName.get(tr.getName()) : null;
      hits.add(
          RuleHitDTO.builder()
              .ruleName(tr.getName())
              .description(rc != null ? rc.getDescription() : null)
              .ruleType(rc != null && rc.getRuleType() != null ? rc.getRuleType().name() : null)
              .classification(
                  rc != null && rc.getClassification() != null
                      ? rc.getClassification().name()
                      : null)
              .threshold(rc != null ? rc.getThreshold() : null)
              .weight(tr.getWeight())
              .contribution(tr.getContribution())
              .detail(tr.getDetail())
              .build());
    }
    return hits;
  }

  private List<PopupDTO> aggregatePopups(List<RuleHitDTO> ruleHits) {
    if (ruleHits == null || ruleHits.isEmpty()) {
      return List.of();
    }

    Map<String, List<RuleHitDTO>> grouped = new LinkedHashMap<>();
    for (RuleHitDTO hit : ruleHits) {
      String key = hit.getClassification() != null ? hit.getClassification() : "UNKNOWN";
      grouped.computeIfAbsent(key, k -> new ArrayList<>()).add(hit);
    }

    List<String> order = List.of("FRAUD", "SUSPICIOUS", "APPROVED", "UNKNOWN");
    List<PopupDTO> popups = new ArrayList<>();
    for (String key : order) {
      List<RuleHitDTO> hits = grouped.get(key);
      if (hits == null || hits.isEmpty()) continue;

      int total =
          hits.stream()
              .map(RuleHitDTO::getContribution)
              .filter(Objects::nonNull)
              .mapToInt(Integer::intValue)
              .sum();
      String title =
          switch (key) {
            case "FRAUD" -> "Bloqueio";
            case "SUSPICIOUS" -> "Suspeita";
            case "APPROVED" -> "Aprovada";
            default -> "Atenção";
          };

      String reason =
          hits.stream()
              .map(h -> h.getDetail() != null ? h.getDetail() : h.getDescription())
              .filter(s -> s != null && !s.isBlank())
              .distinct()
              .reduce((a, b) -> a + " | " + b)
              .orElse(null);

      popups.add(
          PopupDTO.builder()
              .key(key)
              .title(title)
              .classification(key)
              .totalContribution(total)
              .rules(hits)
              .reason(reason)
              .build());
    }

    // Qualquer classificação fora da ordem (extensão futura)
    for (Map.Entry<String, List<RuleHitDTO>> e : grouped.entrySet()) {
      if (order.contains(e.getKey())) continue;
      List<RuleHitDTO> hits = e.getValue();
      int total =
          hits.stream()
              .map(RuleHitDTO::getContribution)
              .filter(Objects::nonNull)
              .mapToInt(Integer::intValue)
              .sum();
      String reason =
          hits.stream()
              .map(h -> h.getDetail() != null ? h.getDetail() : h.getDescription())
              .filter(s -> s != null && !s.isBlank())
              .distinct()
              .reduce((a, b) -> a + " | " + b)
              .orElse(null);
      popups.add(
          PopupDTO.builder()
              .key(e.getKey())
              .title("Atenção")
              .classification(e.getKey())
              .totalContribution(total)
              .rules(hits)
              .reason(reason)
              .build());
    }

    return popups;
  }

  private TransactionResponse buildResponseFromExisting(Transaction transaction, long startTime) {
    TransactionDecision decision =
        decisionRepository
            .findByTransactionId(transaction.getId())
            .orElseThrow(
                () -> new IllegalStateException("Transação existe sem decisão registrada"));

    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());

    safeLogEvaluate(
        ExecutionEventType.IDEMPOTENT_REPLAY,
        transaction.getExternalTransactionId(),
        decision.getPayloadRawHash() != null
            ? decision.getPayloadRawHash()
            : transaction.getPayloadRawHash(),
        decision,
        triggeredRules,
        null);

    return TransactionResponse.builder()
        .id(transaction.getId())
        .transactionId(transaction.getExternalTransactionId())
        .customerIdFromHeader(transaction.getCustomerIdFromHeader())
        .merchantId(transaction.getMerchantId())
        .merchantName(transaction.getMerchantName())
        .transactionAmount(transaction.getTransactionAmount())
        .transactionDate(transaction.getTransactionDate())
        .transactionTime(transaction.getTransactionTime())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(triggeredRules)
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            toOffsetDateTime(
                decision.getCreatedAt() != null
                    ? decision.getCreatedAt()
                    : transaction.getCreatedAt()))
        .success(true)
        .build();
  }

  private TransactionResponse buildResponseFromExistingDecision(
      TransactionDecision decision, long startTime) {
    Transaction transaction = decision.getTransaction();
    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());

    return TransactionResponse.builder()
        .id(transaction != null ? transaction.getId() : null)
        .transactionId(transaction.getExternalTransactionId())
        .customerIdFromHeader(transaction.getCustomerIdFromHeader())
        .merchantId(transaction.getMerchantId())
        .merchantName(transaction.getMerchantName())
        .transactionAmount(transaction.getTransactionAmount())
        .transactionDate(transaction.getTransactionDate())
        .transactionTime(transaction.getTransactionTime())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(triggeredRules)
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            toOffsetDateTime(
                decision.getCreatedAt() != null
                    ? decision.getCreatedAt()
                    : transaction.getCreatedAt()))
        .success(true)
        .build();
  }

  private EvaluateResponse buildEvaluateResponseFromExistingDecision(
      TransactionDecision decision, long startTime) {
    Transaction transaction = decision.getTransaction();
    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());
    List<RuleHitDTO> ruleHits = enrichRuleHits(triggeredRules);
    List<PopupDTO> popups = aggregatePopups(ruleHits);

    return EvaluateResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(
            decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  private OffsetDateTime toOffsetDateTime(LocalDateTime dt) {
    if (dt == null) {
      return null;
    }
    return dt.atZone(ZoneId.systemDefault()).toOffsetDateTime();
  }

  private List<TriggeredRuleDTO> readTriggeredRules(String rulesApplied) {
    if (rulesApplied == null || rulesApplied.isBlank()) {
      return List.of();
    }
    try {
      return objectMapper.readValue(
          rulesApplied,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, TriggeredRuleDTO.class));
    } catch (Exception e) {
      return List.of(rulesApplied.split(",")).stream()
          .map(String::trim)
          .filter(s -> !s.isBlank())
          .map(name -> TriggeredRuleDTO.builder().name(name).weight(0).contribution(0).build())
          .toList();
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

  private List<RuleConditionDTO> readConditions(String conditionsJson) {
    try {
      if (conditionsJson == null || conditionsJson.isBlank()) {
        return List.of();
      }
      return objectMapper.readValue(
          conditionsJson,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, RuleConditionDTO.class));
    } catch (Exception e) {
      return List.of();
    }
  }

  private boolean evaluateCondition(TransactionRequest request, RuleConditionDTO condition) {
    String operatorRaw = condition.getOperator() == null ? "" : condition.getOperator().trim();
    String operator = normalizeOperator(operatorRaw);
    String rawValue = condition.getValue() == null ? "" : condition.getValue();

    Object leftValue = readComputedLeftValue(request, condition.getField());

    // Operadores unários (não exigem value)
    switch (operator) {
      case "IS_NULL":
        return leftValue == null;
      case "IS_NOT_NULL":
        return leftValue != null;
      case "IS_TRUE":
        return truthy(leftValue);
      case "IS_FALSE":
        return leftValue != null && !truthy(leftValue);
      default:
        // seguir
    }

    if (leftValue == null) {
      return false;
    }

    if (leftValue instanceof Number || leftValue instanceof BigDecimal) {
      BigDecimal left =
          (leftValue instanceof BigDecimal)
              ? (BigDecimal) leftValue
              : new BigDecimal(leftValue.toString());
      try {
        return switch (operator) {
          case "EQ" -> left.compareTo(new BigDecimal(rawValue)) == 0;
          case "NE" -> left.compareTo(new BigDecimal(rawValue)) != 0;
          case "GT" -> left.compareTo(new BigDecimal(rawValue)) > 0;
          case "LT" -> left.compareTo(new BigDecimal(rawValue)) < 0;
          case "GTE" -> left.compareTo(new BigDecimal(rawValue)) >= 0;
          case "LTE" -> left.compareTo(new BigDecimal(rawValue)) <= 0;
          case "IN" -> inListNumberFlexible(left, rawValue);
          case "NOT_IN" -> !inListNumberFlexible(left, rawValue);
          case "BETWEEN" -> betweenNumber(left, rawValue, true);
          case "NOT_BETWEEN" -> !betweenNumber(left, rawValue, true);
          default -> false;
        };
      } catch (Exception e) {
        return false;
      }
    }

    String left = String.valueOf(leftValue);
    String right = unquote(rawValue);
    return switch (operator) {
      case "EQ" -> left.equals(right);
      case "NE" -> !left.equals(right);
      case "CONTAINS" -> left.contains(right);
      case "NOT_CONTAINS" -> !left.contains(right);
      case "STARTS_WITH" -> left.startsWith(right);
      case "ENDS_WITH" -> left.endsWith(right);
      case "MATCHES_REGEX" -> matchesRegex(left, rawValue);
      case "IN" -> inListStringFlexible(left, rawValue);
      case "NOT_IN" -> !inListStringFlexible(left, rawValue);
      default -> false;
    };
  }

  private boolean truthy(Object v) {
    if (v == null) return false;
    if (v instanceof Boolean b) return b;
    if (v instanceof Number n) return n.intValue() != 0;
    String s = String.valueOf(v).trim().toLowerCase(java.util.Locale.ROOT);
    return s.equals("true") || s.equals("1") || s.equals("y") || s.equals("yes");
  }

  private String normalizeOperator(String raw) {
    if (raw == null) return "";
    String o = raw.trim();
    // aceitar operadores simbólicos (compatibilidade com FE/legado)
    return switch (o) {
      case "==" -> "EQ";
      case "!=" -> "NE";
      case ">" -> "GT";
      case "<" -> "LT";
      case ">=" -> "GTE";
      case "<=" -> "LTE";
      default -> o.toUpperCase(java.util.Locale.ROOT);
    };
  }

  private boolean betweenNumber(BigDecimal left, String raw, boolean inclusive) {
    java.util.List<String> parts = parseListTokens(raw);
    if (parts.size() < 2) {
      // aceitar "min..max"
      String s = raw == null ? "" : raw.trim();
      if (s.contains("..")) {
        String[] p = s.split("\\.\\.", 2);
        parts = java.util.List.of(p[0].trim(), p[1].trim());
      } else {
        return false;
      }
    }
    BigDecimal a = new BigDecimal(parts.get(0));
    BigDecimal b = new BigDecimal(parts.get(1));
    BigDecimal min = a.min(b);
    BigDecimal max = a.max(b);
    return inclusive
        ? left.compareTo(min) >= 0 && left.compareTo(max) <= 0
        : left.compareTo(min) > 0 && left.compareTo(max) < 0;
  }

  private boolean matchesRegex(String left, String rawRegex) {
    try {
      return java.util.regex.Pattern.compile(rawRegex).matcher(left).find();
    } catch (Exception e) {
      return false;
    }
  }

  private boolean inListNumberFlexible(BigDecimal left, String rawList) {
    for (String token : parseListTokens(rawList)) {
      if (token.isEmpty()) continue;
      if (left.compareTo(new BigDecimal(token)) == 0) return true;
    }
    return false;
  }

  private boolean inListStringFlexible(String left, String rawList) {
    for (String token : parseListTokens(rawList)) {
      if (left.equals(token)) return true;
    }
    return false;
  }

  /**
   * Aceita:
   * - "a,b,c"
   * - "[a,b,c]"
   * - "['RU','CN']"
   * - "[7995, 7994]"
   */
  private java.util.List<String> parseListTokens(String raw) {
    if (raw == null) return java.util.List.of();
    String s = raw.trim();
    if (s.startsWith("[") && s.endsWith("]") && s.length() >= 2) {
      s = s.substring(1, s.length() - 1).trim();
    }
    if (s.isEmpty()) return java.util.List.of();
    String[] tokens = s.split(",");
    java.util.List<String> out = new java.util.ArrayList<>(tokens.length);
    for (String t : tokens) {
      String v = t == null ? "" : t.trim();
      if ((v.startsWith("'") && v.endsWith("'")) || (v.startsWith("\"") && v.endsWith("\""))) {
        v = v.substring(1, v.length() - 1);
      }
      out.add(v.trim());
    }
    return out;
  }

  private String unquote(String raw) {
    if (raw == null) return "";
    String v = raw.trim();
    if ((v.startsWith("'") && v.endsWith("'")) || (v.startsWith("\"") && v.endsWith("\""))) {
      if (v.length() >= 2) {
        v = v.substring(1, v.length() - 1);
      }
    }
    return v;
  }

  private String explainCondition(
      TransactionRequest request, RuleConditionDTO condition, boolean result) {
    Object fieldValue = readComputedLeftValue(request, condition.getField());
    return condition.getField()
        + " "
        + condition.getOperator()
        + " "
        + condition.getValue()
        + " (actual="
        + fieldValue
        + ") => "
        + result;
  }

  private Object readComputedLeftValue(TransactionRequest request, String fieldExpr) {
    if (fieldExpr == null) {
      return null;
    }
    String raw = fieldExpr.trim();
    if (raw.isEmpty()) {
      return null;
    }

    // ABS(x)
    java.util.regex.Matcher unary =
        java.util.regex.Pattern.compile("^(ABS|LEN|LOWER|UPPER|TRIM)\\(([A-Za-z0-9_]+)\\)$")
            .matcher(raw);
    if (unary.matches()) {
      String fn = unary.group(1);
      String arg = unary.group(2);
      Object base = readFieldValue(request, TransactionRequest.class, arg);
      return applyUnary(fn, base);
    }

    // ABS_DIFF(a,b)
    java.util.regex.Matcher absDiff =
        java.util.regex.Pattern.compile("^ABS_DIFF\\(([A-Za-z0-9_]+)\\s*,\\s*([A-Za-z0-9_]+)\\)$")
            .matcher(raw);
    if (absDiff.matches()) {
      Object a = readFieldValue(request, TransactionRequest.class, absDiff.group(1));
      Object b = readFieldValue(request, TransactionRequest.class, absDiff.group(2));
      return absDiff(a, b);
    }

    // COALESCE(field, literal)
    java.util.regex.Matcher coalesce =
        java.util.regex.Pattern.compile("^COALESCE\\(([A-Za-z0-9_]+)\\s*,\\s*(.+)\\)$").matcher(raw);
    if (coalesce.matches()) {
      Object base = readFieldValue(request, TransactionRequest.class, coalesce.group(1));
      if (base != null) return base;
      String literal = coalesce.group(2).trim();
      if ((literal.startsWith("'") && literal.endsWith("'"))
          || (literal.startsWith("\"") && literal.endsWith("\""))) {
        literal = literal.substring(1, literal.length() - 1);
      }
      return literal;
    }

    return readFieldValue(request, TransactionRequest.class, raw);
  }

  private Object applyUnary(String fn, Object v) {
    if (v == null) return null;
    return switch (fn) {
      case "ABS" -> {
        try {
          BigDecimal n =
              (v instanceof BigDecimal) ? (BigDecimal) v : new BigDecimal(String.valueOf(v));
          yield n.abs();
        } catch (Exception e) {
          yield null;
        }
      }
      case "LEN" -> String.valueOf(v).length();
      case "LOWER" -> String.valueOf(v).toLowerCase(java.util.Locale.ROOT);
      case "UPPER" -> String.valueOf(v).toUpperCase(java.util.Locale.ROOT);
      case "TRIM" -> String.valueOf(v).trim();
      default -> v;
    };
  }

  private Object absDiff(Object a, Object b) {
    if (a == null || b == null) return null;
    try {
      BigDecimal na =
          (a instanceof BigDecimal) ? (BigDecimal) a : new BigDecimal(String.valueOf(a));
      BigDecimal nb =
          (b instanceof BigDecimal) ? (BigDecimal) b : new BigDecimal(String.valueOf(b));
      return na.subtract(nb).abs();
    } catch (Exception e) {
      return null;
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

  /** Classe interna para armazenar resultado da avaliação de regras. */
  @lombok.Data
  @lombok.NoArgsConstructor
  @lombok.AllArgsConstructor
  public static class RuleEvaluationResult {
    private int riskScore;
    private List<TriggeredRuleDTO> triggeredRules;
    private Map<String, Object> scoreDetails;
    private TransactionDecision.TransactionClassification classification;
    private String reason;
  }

  private record RuleMatch(boolean triggered, String detail) {}
}
