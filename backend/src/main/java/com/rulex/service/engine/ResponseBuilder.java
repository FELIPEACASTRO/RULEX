package com.rulex.service.engine;

import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.PopupDTO;
import com.rulex.dto.RuleHitDTO;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

/**
 * ARCH-003: Responsável por construir respostas de transação.
 * Extraído do RuleEngineService para Single Responsibility.
 */
@Component
@RequiredArgsConstructor
public class ResponseBuilder {

  private final Clock clock;

  /**
   * Constrói resposta de erro de contrato.
   */
  public EvaluateResponse buildContractErrorResponse(
      String ruleName,
      String detail,
      TransactionDecision.TransactionClassification classification,
      String payloadHash,
      long startTime) {

    RuleHitDTO hit = RuleHitDTO.builder()
        .ruleName(ruleName)
        .description(detail)
        .ruleType("CONTRACT")
        .classification(classification.name())
        .threshold(null)
        .weight(100)
        .contribution(0)
        .detail(detail)
        .build();

    PopupDTO popup = PopupDTO.builder()
        .key(classification.name())
        .title(classification == TransactionDecision.TransactionClassification.FRAUD
            ? "Bloqueio" : "Suspeita")
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

  /**
   * Constrói resposta de tamper detection.
   */
  public TransactionResponse buildTamperResponse(
      TransactionDecision decision, long startTime) {
    long processingTime = System.currentTimeMillis() - startTime;
    OffsetDateTime timestamp = decision.getCreatedAt() != null
        ? decision.getCreatedAt().atOffset(ZoneOffset.UTC)
        : OffsetDateTime.now(clock);
    return TransactionResponse.builder()
        .transactionId(decision.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .triggeredRules(List.of())
        .processingTimeMs(processingTime)
        .timestamp(timestamp)
        .build();
  }

  /**
   * Constrói EvaluateResponse de tamper detection.
   */
  public EvaluateResponse buildTamperEvaluateResponse(
      TransactionDecision decision, long startTime) {
    long processingTime = System.currentTimeMillis() - startTime;

    RuleHitDTO hit = RuleHitDTO.builder()
        .ruleName("ANTI_TAMPER")
        .description("Payload hash mismatch detected")
        .ruleType("SECURITY")
        .classification(decision.getClassification().name())
        .weight(100)
        .contribution(100)
        .detail("Tentativa de adulteração detectada")
        .build();

    PopupDTO popup = PopupDTO.builder()
        .key("FRAUD")
        .title("Bloqueio de Segurança")
        .classification(decision.getClassification().name())
        .totalContribution(100)
        .rules(List.of(hit))
        .reason("Tentativa de adulteração detectada")
        .build();

    return EvaluateResponse.builder()
        .transactionId(decision.getExternalTransactionId())
        .classification(decision.getClassification().name())
        .riskScore(decision.getRiskScore())
        .reason(decision.getReason())
        .rulesetVersion("security")
        .processingTimeMs(processingTime)
        .timestamp(decision.getCreatedAt())
        .ruleHits(List.of(hit))
        .popups(List.of(popup))
        .build();
  }

  /**
   * Agrupa regras disparadas em popups por classificação.
   */
  public List<PopupDTO> buildPopups(
      List<TriggeredRuleDTO> triggeredRules,
      Map<String, Object> scoreDetails) {

    if (triggeredRules == null || triggeredRules.isEmpty()) {
      return List.of();
    }

    // Agrupa por classificação inferida do scoreDetails
    Map<String, List<TriggeredRuleDTO>> byClassification = triggeredRules.stream()
        .collect(Collectors.groupingBy(rule -> {
          Object detail = scoreDetails.get(rule.getName());
          if (detail instanceof Map) {
            Object classification = ((Map<?, ?>) detail).get("classification");
            return classification != null ? classification.toString() : "SUSPICIOUS";
          }
          return "SUSPICIOUS";
        }));

    List<PopupDTO> popups = new ArrayList<>();
    for (Map.Entry<String, List<TriggeredRuleDTO>> entry : byClassification.entrySet()) {
      String classification = entry.getKey();
      List<TriggeredRuleDTO> rules = entry.getValue();

      int totalContribution = rules.stream()
          .mapToInt(TriggeredRuleDTO::getContribution)
          .sum();

      List<RuleHitDTO> hits = rules.stream()
          .map(r -> RuleHitDTO.builder()
              .ruleName(r.getName())
              .description(r.getDetail())
              .ruleType("RULE")
              .classification(classification)
              .weight(r.getWeight())
              .contribution(r.getContribution())
              .detail(r.getDetail())
              .build())
          .toList();

      popups.add(PopupDTO.builder()
          .key(classification)
          .title(getTitleForClassification(classification))
          .classification(classification)
          .totalContribution(totalContribution)
          .rules(hits)
          .reason(buildReasonFromRules(rules))
          .build());
    }

    return popups;
  }

  private String getTitleForClassification(String classification) {
    return switch (classification) {
      case "FRAUD" -> "Bloqueio";
      case "SUSPICIOUS" -> "Suspeita";
      case "APPROVED" -> "Aprovado";
      default -> classification;
    };
  }

  private String buildReasonFromRules(List<TriggeredRuleDTO> rules) {
    if (rules.isEmpty()) return "";
    return rules.stream()
        .map(TriggeredRuleDTO::getName)
        .collect(Collectors.joining(", "));
  }
}
