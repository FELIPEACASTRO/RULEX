package com.rulex.service;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TransactionResponse;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.*;

/**
 * Serviço que implementa o motor de regras duras para análise de fraude.
 * Avalia transações contra regras configuráveis e retorna uma classificação.
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

    /**
     * Processa uma transação e retorna a classificação de fraude.
     */
    public TransactionResponse analyzeTransaction(TransactionRequest request) {
        long startTime = System.currentTimeMillis();
        
        try {
            // 1. Salvar a transação
            Transaction transaction = convertRequestToEntity(request);
            transaction = transactionRepository.save(transaction);
            
            // 2. Avaliar regras
            RuleEvaluationResult result = evaluateRules(transaction);
            
            // 3. Salvar decisão
            TransactionDecision decision = createDecision(transaction, result);
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
     * Avalia as regras configuradas contra a transação.
     */
    private RuleEvaluationResult evaluateRules(Transaction transaction) {
        RuleEvaluationResult result = new RuleEvaluationResult();
        List<RuleConfiguration> enabledRules = ruleConfigRepository.findByEnabled(true);
        
        int totalScore = 0;
        List<String> appliedRules = new ArrayList<>();
        Map<String, Object> scoreDetails = new HashMap<>();
        
        for (RuleConfiguration rule : enabledRules) {
            boolean ruleTriggered = evaluateRule(transaction, rule);
            
            if (ruleTriggered) {
                appliedRules.add(rule.getRuleName());
                int contribution = (rule.getWeight() * rule.getThreshold()) / 100;
                totalScore += contribution;
                scoreDetails.put(rule.getRuleName(), Map.of(
                    "triggered", true,
                    "weight", rule.getWeight(),
                    "contribution", contribution
                ));
            } else {
                scoreDetails.put(rule.getRuleName(), Map.of("triggered", false));
            }
        }
        
        // Normalizar score para 0-100
        totalScore = Math.min(totalScore, 100);
        
        result.setRiskScore(totalScore);
        result.setAppliedRules(appliedRules);
        result.setScoreDetails(scoreDetails);
        result.setClassification(classifyRisk(totalScore));
        result.setReason(generateReason(totalScore, appliedRules));
        
        return result;
    }

    /**
     * Avalia uma regra específica contra a transação.
     */
    private boolean evaluateRule(Transaction transaction, RuleConfiguration rule) {
        return switch (rule.getRuleName()) {
            case "LOW_AUTHENTICATION_SCORE" -> 
                transaction.getConsumerAuthenticationScore() < rule.getThreshold();
            
            case "LOW_EXTERNAL_SCORE" -> 
                transaction.getExternalScore3() < rule.getThreshold();
            
            case "INVALID_CAVV" -> 
                transaction.getCavvResult() != 0;
            
            case "INVALID_CRYPTOGRAM" -> 
                !"V".equals(transaction.getCryptogramValid());
            
            case "CVV_MISMATCH" -> 
                "N".equals(transaction.getCvv2Response());
            
            case "HIGH_TRANSACTION_AMOUNT" -> 
                transaction.getTransactionAmount().doubleValue() > rule.getThreshold();
            
            case "HIGH_RISK_MCC" -> 
                isHighRiskMcc(transaction.getMcc());
            
            case "INTERNATIONAL_TRANSACTION" -> 
                isInternationalTransaction(transaction);
            
            case "CARD_NOT_PRESENT" -> 
                !"Y".equals(transaction.getCustomerPresent());
            
            case "PIN_VERIFICATION_FAILED" -> 
                "I".equals(transaction.getPinVerifyCode());
            
            case "CVV_PIN_LIMIT_EXCEEDED" -> 
                "1".equals(transaction.getCvvVerifyCode());
            
            case "OFFLINE_PIN_FAILED" -> 
                "1".equals(transaction.getCvvVerifyCode());
            
            default -> false;
        };
    }

    /**
     * Verifica se o MCC é de alto risco.
     */
    private boolean isHighRiskMcc(Integer mcc) {
        // MCCs de alto risco: Jogos, Criptomoedas, Transferências de dinheiro, etc.
        Set<Integer> highRiskMccs = Set.of(
            7995, // Gambling
            6211, // Securities Brokers
            6051, // Crypto
            7273, // Dating Services
            7994  // Video Amusement
        );
        return highRiskMccs.contains(mcc);
    }

    /**
     * Verifica se é uma transação internacional.
     */
    private boolean isInternationalTransaction(Transaction transaction) {
        // Assumir que 076 é o código do Brasil
        return transaction.getMerchantCountryCode() != null && 
               !transaction.getMerchantCountryCode().equals("076");
    }

    /**
     * Classifica o risco baseado no score.
     */
    private TransactionDecision.TransactionClassification classifyRisk(int riskScore) {
        if (riskScore < 30) {
            return TransactionDecision.TransactionClassification.APPROVED;
        } else if (riskScore < 70) {
            return TransactionDecision.TransactionClassification.SUSPICIOUS;
        } else {
            return TransactionDecision.TransactionClassification.FRAUD;
        }
    }

    /**
     * Gera uma descrição do motivo da decisão.
     */
    private String generateReason(int riskScore, List<String> appliedRules) {
        if (riskScore < 30) {
            return "Transação aprovada. Score de risco baixo.";
        } else if (riskScore < 70) {
            return String.format("Transação suspeita. Score de risco: %d. Regras acionadas: %s", 
                riskScore, String.join(", ", appliedRules));
        } else {
            return String.format("Transação bloqueada como fraude. Score de risco: %d. Regras acionadas: %s", 
                riskScore, String.join(", ", appliedRules));
        }
    }

    /**
     * Cria a entidade de decisão.
     */
    private TransactionDecision createDecision(Transaction transaction, RuleEvaluationResult result) {
        return TransactionDecision.builder()
            .transaction(transaction)
            .classification(result.getClassification())
            .riskScore(result.getRiskScore())
            .rulesApplied(String.join(", ", result.getAppliedRules()))
            .scoreDetails(convertMapToJson(result.getScoreDetails()))
            .reason(result.getReason())
            .rulesVersion("1.0.0")
            .build();
    }

    /**
     * Converte um mapa para JSON string.
     */
    private String convertMapToJson(Map<String, Object> map) {
        try {
            return new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(map);
        } catch (Exception e) {
            log.error("Erro ao converter mapa para JSON", e);
            return "{}";
        }
    }

    /**
     * Constrói a resposta da análise.
     */
    private TransactionResponse buildResponse(Transaction transaction, TransactionDecision decision, 
                                            RuleEvaluationResult result, long processingTime) {
        return TransactionResponse.builder()
            .externalTransactionId(transaction.getExternalTransactionId())
            .classification(decision.getClassification().name())
            .riskScore(decision.getRiskScore())
            .rulesApplied(result.getAppliedRules())
            .scoreDetails(result.getScoreDetails())
            .reason(decision.getReason())
            .rulesVersion(decision.getRulesVersion())
            .processingTime(processingTime)
            .timestamp(LocalDateTime.now())
            .success(true)
            .build();
    }

    /**
     * Converte TransactionRequest para Transaction entity.
     */
    private Transaction convertRequestToEntity(TransactionRequest request) {
        return Transaction.builder()
            .externalTransactionId(request.getExternalTransactionId())
            .customerIdFromHeader(request.getCustomerIdFromHeader())
            .customerAcctNumber(request.getCustomerAcctNumber())
            .pan(request.getPan())
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
            .build();
    }

    /**
     * Classe interna para armazenar resultado da avaliação de regras.
     */
    @lombok.Data
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class RuleEvaluationResult {
        private int riskScore;
        private List<String> appliedRules;
        private Map<String, Object> scoreDetails;
        private TransactionDecision.TransactionClassification classification;
        private String reason;
    }

}
