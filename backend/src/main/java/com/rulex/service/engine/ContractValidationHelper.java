package com.rulex.service.engine;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.PopupDTO;
import com.rulex.dto.RuleHitDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.TransactionDecision;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataAccessException;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class ContractValidationHelper {

  private final ObjectMapper objectMapper;
  private final RuleExecutionLogService ruleExecutionLogService;
  private final Clock clock;

  public List<String> missingRequiredForPersistence(TransactionRequest request) {
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

  public EvaluateResponse buildContractErrorEvaluateResponse(
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

  public void safeLogContractError(
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

  private boolean isBlank(String s) {
    return s == null || s.isBlank();
  }
}
