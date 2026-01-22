package com.rulex.service.engine.response;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.EvaluateResponse;
import com.rulex.dto.PopupDTO;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.RuleHitDTO;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço responsável pela construção de respostas da API.
 * 
 * <p>Extraído de RuleEngineService para seguir Single Responsibility Principle.
 * 
 * <p>Responsabilidades:
 * <ul>
 *   <li>Construir TransactionResponse</li>
 *   <li>Construir EvaluateResponse</li>
 *   <li>Agregar popups por classificação</li>
 *   <li>Enriquecer rule hits com detalhes</li>
 * </ul>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ResponseBuilderService {

    private final ObjectMapper objectMapper;
    private final Clock clock;

    /**
     * Constrói TransactionResponse a partir de uma transação processada.
     */
    public TransactionResponse buildTransactionResponse(
            Transaction transaction,
            TransactionDecision decision,
            List<TriggeredRuleDTO> triggeredRules,
            long processingTimeMs) {
        
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
            .rulesetVersion("v3.1")
            .processingTimeMs(processingTimeMs)
            .timestamp(OffsetDateTime.now(clock))
            .success(true)
            .build();
    }

    /**
     * Constrói TransactionResponse para decisão de tamper.
     */
    public TransactionResponse buildTamperResponse(TransactionDecision tamperDecision, long processingTimeMs) {
        List<TriggeredRuleDTO> triggeredRules = List.of(
            TriggeredRuleDTO.builder()
                .name("ANTI_TAMPER")
                .weight(100)
                .contribution(100)
                .detail("Payload hash mismatch detected")
                .build()
        );

        return TransactionResponse.builder()
            .transactionId(tamperDecision.getExternalTransactionId())
            .classification(tamperDecision.getClassification().name())
            .riskScore(tamperDecision.getRiskScore())
            .triggeredRules(triggeredRules)
            .reason(tamperDecision.getReason())
            .rulesetVersion("anti-tamper")
            .processingTimeMs(processingTimeMs)
            .timestamp(OffsetDateTime.now(clock))
            .success(true)
            .build();
    }

    /**
     * Constrói TransactionResponse a partir de transação existente (idempotência).
     */
    public TransactionResponse buildFromExisting(Transaction transaction, TransactionDecision decision, long processingTimeMs) {
        List<TriggeredRuleDTO> triggeredRules = parseTriggeredRules(decision.getRulesApplied());
        
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
            .rulesetVersion("v3.1")
            .processingTimeMs(processingTimeMs)
            .timestamp(toOffsetDateTime(decision.getCreatedAt()))
            .success(true)
            .build();
    }

    /**
     * Constrói EvaluateResponse completo com rule hits e popups.
     */
    public EvaluateResponse buildEvaluateResponse(
            Transaction transaction,
            TransactionDecision decision,
            List<TriggeredRuleDTO> triggeredRules,
            Map<String, Object> scoreDetails,
            long processingTimeMs) {
        
        List<RuleHitDTO> ruleHits = enrichRuleHits(triggeredRules);
        List<PopupDTO> popups = aggregatePopups(ruleHits);

        return EvaluateResponse.builder()
            .transactionId(transaction.getExternalTransactionId())
            .classification(decision.getClassification().name())
            .riskScore(decision.getRiskScore())
            .reason(decision.getReason())
            .rulesetVersion("v3.1")
            .processingTimeMs(processingTimeMs)
            .timestamp(LocalDateTime.now(clock))
            .ruleHits(ruleHits)
            .popups(popups)
            .build();
    }

    /**
     * Constrói EvaluateResponse para decisão de tamper.
     */
    public EvaluateResponse buildTamperEvaluateResponse(TransactionDecision tamperDecision, long processingTimeMs) {
        RuleHitDTO tamperHit = RuleHitDTO.builder()
            .ruleName("ANTI_TAMPER")
            .description("Payload hash mismatch detected - possible tampering attempt")
            .ruleType("SECURITY")
            .classification("FRAUD")
            .threshold(null)
            .weight(100)
            .contribution(100)
            .detail("Hash mismatch")
            .build();

        PopupDTO tamperPopup = PopupDTO.builder()
            .key("FRAUD")
            .title("Bloqueio por Segurança")
            .classification("FRAUD")
            .totalContribution(100)
            .rules(List.of(tamperHit))
            .reason("Tentativa de tamper detectada")
            .build();

        return EvaluateResponse.builder()
            .transactionId(tamperDecision.getExternalTransactionId())
            .classification(tamperDecision.getClassification().name())
            .riskScore(tamperDecision.getRiskScore())
            .reason(tamperDecision.getReason())
            .rulesetVersion("anti-tamper")
            .processingTimeMs(processingTimeMs)
            .timestamp(LocalDateTime.now(clock))
            .ruleHits(List.of(tamperHit))
            .popups(List.of(tamperPopup))
            .build();
    }

    /**
     * Constrói EvaluateResponse para erro de contrato.
     */
    public EvaluateResponse buildContractErrorResponse(
            String ruleName,
            String detail,
            TransactionDecision.TransactionClassification classification,
            String payloadHash,
            long processingTimeMs) {
        
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
            .title(classification == TransactionDecision.TransactionClassification.FRAUD ? "Bloqueio" : "Suspeita")
            .classification(classification.name())
            .totalContribution(0)
            .rules(List.of(hit))
            .reason(detail)
            .build();

        return EvaluateResponse.builder()
            .transactionId(null)
            .classification(classification.name())
            .riskScore(0)
            .reason(detail)
            .rulesetVersion("contract")
            .processingTimeMs(processingTimeMs)
            .timestamp(LocalDateTime.now(clock))
            .ruleHits(List.of(hit))
            .popups(List.of(popup))
            .build();
    }

    /**
     * Enriquece triggered rules com informações adicionais para RuleHitDTO.
     */
    public List<RuleHitDTO> enrichRuleHits(List<TriggeredRuleDTO> triggeredRules) {
        if (triggeredRules == null || triggeredRules.isEmpty()) {
            return List.of();
        }

        return triggeredRules.stream()
            .map(rule -> RuleHitDTO.builder()
                .ruleName(rule.getName())
                .description(rule.getDetail())
                .ruleType(inferRuleType(rule.getName()))
                .classification(inferClassification(rule.getWeight()))
                .threshold(null)
                .weight(rule.getWeight())
                .contribution(rule.getContribution())
                .detail(rule.getDetail())
                .build())
            .collect(Collectors.toList());
    }

    /**
     * Agrega rule hits em popups por classificação.
     */
    public List<PopupDTO> aggregatePopups(List<RuleHitDTO> ruleHits) {
        if (ruleHits == null || ruleHits.isEmpty()) {
            return List.of();
        }

        Map<String, List<RuleHitDTO>> byClassification = ruleHits.stream()
            .collect(Collectors.groupingBy(RuleHitDTO::getClassification));

        List<PopupDTO> popups = new ArrayList<>();
        
        for (Map.Entry<String, List<RuleHitDTO>> entry : byClassification.entrySet()) {
            String classification = entry.getKey();
            List<RuleHitDTO> rules = entry.getValue();
            
            int totalContribution = rules.stream()
                .mapToInt(RuleHitDTO::getContribution)
                .sum();

            String title = switch (classification) {
                case "FRAUD" -> "Bloqueio";
                case "SUSPICIOUS" -> "Suspeita";
                default -> "Informação";
            };

            String reason = rules.stream()
                .map(RuleHitDTO::getRuleName)
                .collect(Collectors.joining(", "));

            popups.add(PopupDTO.builder()
                .key(classification)
                .title(title)
                .classification(classification)
                .totalContribution(totalContribution)
                .rules(rules)
                .reason("Regras acionadas: " + reason)
                .build());
        }

        return popups;
    }

    /**
     * Infere o tipo de regra pelo nome.
     */
    private String inferRuleType(String ruleName) {
        if (ruleName == null) return "UNKNOWN";
        
        String upper = ruleName.toUpperCase();
        if (upper.contains("VELOCITY")) return "VELOCITY";
        if (upper.contains("GEO")) return "GEO";
        if (upper.contains("DEVICE")) return "DEVICE";
        if (upper.contains("AML")) return "AML";
        if (upper.contains("MERCHANT")) return "MERCHANT";
        if (upper.contains("AMOUNT")) return "AMOUNT";
        return "GENERAL";
    }

    /**
     * Infere classificação pelo peso da regra.
     */
    private String inferClassification(Integer weight) {
        if (weight == null) return "APPROVED";
        if (weight >= 80) return "FRAUD";
        if (weight >= 40) return "SUSPICIOUS";
        return "APPROVED";
    }

    /**
     * Converte LocalDateTime para OffsetDateTime.
     */
    private OffsetDateTime toOffsetDateTime(LocalDateTime dt) {
        if (dt == null) {
            return OffsetDateTime.now(clock);
        }
        return dt.atZone(ZoneId.systemDefault()).toOffsetDateTime();
    }

    /**
     * Parse triggered rules do JSON armazenado.
     */
    private List<TriggeredRuleDTO> parseTriggeredRules(String rulesApplied) {
        if (rulesApplied == null || rulesApplied.isBlank()) {
            return List.of();
        }
        try {
            return objectMapper.readValue(rulesApplied, new TypeReference<List<TriggeredRuleDTO>>() {});
        } catch (Exception e) {
            log.warn("Failed to parse triggered rules: {}", e.getMessage());
            return List.of();
        }
    }
}
