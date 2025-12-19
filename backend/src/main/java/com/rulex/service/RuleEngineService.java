package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.PopupDTO;
import com.rulex.dto.RuleHitDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import com.rulex.util.PanMaskingUtil;
import java.beans.PropertyDescriptor;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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

  /** Processa uma transação e retorna a classificação de fraude. */
  public TransactionResponse analyzeTransaction(TransactionRequest request) {
    long startTime = System.currentTimeMillis();

    try {
      PayloadHashService.CanonicalPayload canonical = payloadHashService.canonicalize(request);
      String payloadHash = canonical.sha256Hex();
      rawStoreService.store(request.getExternalTransactionId(), payloadHash, canonical.json());

      // Idempotência por (externalTransactionId, payloadHash)
      Optional<TransactionDecision> existingDecisionOpt =
          decisionRepository.findByExternalTransactionIdAndPayloadRawHash(
              request.getExternalTransactionId(), payloadHash);
      if (existingDecisionOpt.isPresent()) {
        return buildResponseFromExistingDecision(existingDecisionOpt.get(), startTime);
      }

      // Idempotência por externalTransactionId (canônico)
      Optional<Transaction> existingTxOpt =
          transactionRepository.findByExternalTransactionId(request.getExternalTransactionId());
      if (existingTxOpt.isPresent()) {
        Transaction existing = existingTxOpt.get();
        if (existing.getPayloadRawHash() != null && !existing.getPayloadRawHash().equals(payloadHash)) {
          throw new IllegalStateException(
              "Idempotency conflict: externalTransactionId reutilizado com payload diferente");
        }
        return buildResponseFromExisting(existingTxOpt.get(), startTime);
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
                .findByExternalTransactionId(request.getExternalTransactionId())
                .orElseThrow(() -> e);
        if (racedTx.getPayloadRawHash() != null && !racedTx.getPayloadRawHash().equals(payloadHash)) {
          throw new IllegalStateException(
              "Idempotency conflict: externalTransactionId reutilizado com payload diferente");
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
   * <p>Observação: mantém o mesmo comportamento de persistência/idempotência de
   * {@link #analyzeTransaction(TransactionRequest)}.
   */
  public EvaluateResponse evaluate(TransactionRequest request) {
    long startTime = System.currentTimeMillis();

    try {
      PayloadHashService.CanonicalPayload canonical = payloadHashService.canonicalize(request);
      String payloadHash = canonical.sha256Hex();
      rawStoreService.store(request.getExternalTransactionId(), payloadHash, canonical.json());

      // Idempotência por (externalTransactionId, payloadHash)
      Optional<TransactionDecision> existingDecisionOpt =
          decisionRepository.findByExternalTransactionIdAndPayloadRawHash(
              request.getExternalTransactionId(), payloadHash);
      if (existingDecisionOpt.isPresent()) {
        return buildEvaluateResponseFromExistingDecision(existingDecisionOpt.get(), startTime);
      }

      // Idempotência por externalTransactionId (canônico)
      Optional<Transaction> existingTxOpt =
          transactionRepository.findByExternalTransactionId(request.getExternalTransactionId());
      if (existingTxOpt.isPresent()) {
        Transaction existing = existingTxOpt.get();
        if (existing.getPayloadRawHash() != null && !existing.getPayloadRawHash().equals(payloadHash)) {
          throw new IllegalStateException(
              "Idempotency conflict: externalTransactionId reutilizado com payload diferente");
        }
        return buildEvaluateResponseFromExisting(existingTxOpt.get(), startTime);
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
                .findByExternalTransactionId(request.getExternalTransactionId())
                .orElseThrow(() -> e);
        if (racedTx.getPayloadRawHash() != null && !racedTx.getPayloadRawHash().equals(payloadHash)) {
          throw new IllegalStateException(
              "Idempotency conflict: externalTransactionId reutilizado com payload diferente");
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
    result.setClassification(maxSeverity(classifyRiskByScore(totalScore), maxByRule));
    result.setReason(generateReason(totalScore, triggeredRules));

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
    return request.getMerchantCountryCode() != null && !request.getMerchantCountryCode().equals("076");
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
  private String generateReason(int riskScore, List<TriggeredRuleDTO> triggeredRules) {
    List<String> appliedRules = triggeredRules.stream().map(TriggeredRuleDTO::getName).toList();
    if (riskScore < 30) {
      return "Transação aprovada. Score de risco baixo.";
    } else if (riskScore < 70) {
      return String.format(
          "Transação suspeita. Score de risco: %d. Regras acionadas: %s",
          riskScore, String.join(", ", appliedRules));
    } else {
      return String.format(
          "Transação bloqueada como fraude. Score de risco: %d. Regras acionadas: %s",
          riskScore, String.join(", ", appliedRules));
    }
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

  /** Constrói a resposta da análise. */
  private TransactionResponse buildResponse(
      Transaction transaction,
      TransactionDecision decision,
      RuleEvaluationResult result,
      long processingTime) {
    return TransactionResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(result.getTriggeredRules())
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
          .timestamp(LocalDateTime.now(clock))
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
        .timestamp(decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  private EvaluateResponse buildEvaluateResponseFromExisting(Transaction transaction, long startTime) {
    TransactionDecision decision =
        decisionRepository
            .findByTransactionId(transaction.getId())
            .orElseThrow(() -> new IllegalStateException("Transação existe sem decisão registrada"));

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
        .timestamp(decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
  }

  private List<RuleHitDTO> enrichRuleHits(List<TriggeredRuleDTO> triggeredRules) {
    if (triggeredRules == null || triggeredRules.isEmpty()) {
      return List.of();
    }

    List<String> names =
        triggeredRules.stream().map(TriggeredRuleDTO::getName).filter(Objects::nonNull).distinct().toList();
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
                  rc != null && rc.getClassification() != null ? rc.getClassification().name() : null)
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

      int total = hits.stream().map(RuleHitDTO::getContribution).filter(Objects::nonNull).mapToInt(Integer::intValue).sum();
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
      int total = hits.stream().map(RuleHitDTO::getContribution).filter(Objects::nonNull).mapToInt(Integer::intValue).sum();
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
            .orElseThrow(() -> new IllegalStateException("Transação existe sem decisão registrada"));

    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());

    return TransactionResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(triggeredRules)
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .success(true)
        .build();
  }

  private TransactionResponse buildResponseFromExistingDecision(
      TransactionDecision decision, long startTime) {
    Transaction transaction = decision.getTransaction();
    long processingTime = System.currentTimeMillis() - startTime;
    List<TriggeredRuleDTO> triggeredRules = readTriggeredRules(decision.getRulesApplied());

    return TransactionResponse.builder()
        .transactionId(transaction.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .triggeredRules(triggeredRules)
        .reason(decision.getReason())
        .rulesetVersion(decision.getRulesVersion())
        .processingTimeMs(processingTime)
        .timestamp(decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
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
        .timestamp(decision.getCreatedAt() != null ? decision.getCreatedAt() : transaction.getCreatedAt())
        .ruleHits(ruleHits)
        .popups(popups)
        .build();
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
    Object fieldValue = readFieldValue(request, TransactionRequest.class, condition.getField());
    if (fieldValue == null) {
      return false;
    }

    String operator = condition.getOperator();
    String rawValue = condition.getValue();

    if (fieldValue instanceof Number || fieldValue instanceof BigDecimal) {
      BigDecimal left =
          (fieldValue instanceof BigDecimal)
              ? (BigDecimal) fieldValue
              : new BigDecimal(fieldValue.toString());

      try {
        return switch (operator) {
          case "==" -> left.compareTo(new BigDecimal(rawValue)) == 0;
          case "!=" -> left.compareTo(new BigDecimal(rawValue)) != 0;
          case ">" -> left.compareTo(new BigDecimal(rawValue)) > 0;
          case "<" -> left.compareTo(new BigDecimal(rawValue)) < 0;
          case ">=" -> left.compareTo(new BigDecimal(rawValue)) >= 0;
          case "<=" -> left.compareTo(new BigDecimal(rawValue)) <= 0;
          case "IN" -> inListNumber(left, rawValue);
          case "NOT_IN" -> !inListNumber(left, rawValue);
          default -> false;
        };
      } catch (Exception e) {
        return false;
      }
    }

    String left = String.valueOf(fieldValue);
    return switch (operator) {
      case "==" -> left.equals(rawValue);
      case "!=" -> !left.equals(rawValue);
      case "CONTAINS" -> left.contains(rawValue);
      case "NOT_CONTAINS" -> !left.contains(rawValue);
      case "IN" -> inListString(left, rawValue);
      case "NOT_IN" -> !inListString(left, rawValue);
      default -> false;
    };
  }

  private boolean inListNumber(BigDecimal left, String csv) {
    for (String token : csv.split(",")) {
      String t = token.trim();
      if (t.isEmpty()) continue;
      if (left.compareTo(new BigDecimal(t)) == 0) {
        return true;
      }
    }
    return false;
  }

  private boolean inListString(String left, String csv) {
    for (String token : csv.split(",")) {
      if (left.equals(token.trim())) {
        return true;
      }
    }
    return false;
  }

  private String explainCondition(
      TransactionRequest request, RuleConditionDTO condition, boolean result) {
    Object fieldValue = readFieldValue(request, TransactionRequest.class, condition.getField());
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
