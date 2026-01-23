package com.rulex.service.engine;

import com.rulex.dto.TransactionRequest;
import com.rulex.entity.RuleConfiguration;
import com.rulex.service.EnrichmentService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class RuleEngineLegacyRuleHelper {

  private final EnrichmentService enrichmentService;

  public LegacyRuleResult evaluateLegacyRule(TransactionRequest request, RuleConfiguration rule) {
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

    return new LegacyRuleResult(legacy, legacy ? "regra legada por nome" : null);
  }

  private boolean isHighRiskMcc(Integer mcc) {
    return enrichmentService.isHighRiskMcc(mcc);
  }

  private boolean isInternationalTransaction(TransactionRequest request) {
    // Assumir que 076 é o código do Brasil
    return request.getMerchantCountryCode() != null
        && !request.getMerchantCountryCode().equals("076");
  }

  public record LegacyRuleResult(boolean triggered, String detail) {}
}
