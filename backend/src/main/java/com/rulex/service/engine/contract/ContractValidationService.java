package com.rulex.service.engine.contract;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.AuditService;

import java.util.ArrayList;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço responsável pela validação de contratos de entrada.
 *
 * <p>Extraído de RuleEngineService para seguir Single Responsibility Principle.
 *
 * <p>Responsabilidades:
 * <ul>
 *   <li>Validar campos obrigatórios</li>
 *   <li>Validar formato do payload</li>
 *   <li>Classificar payloads inválidos sem retornar 400</li>
 *   <li>Registrar erros de contrato para auditoria</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ContractValidationService {

    private final ObjectMapper objectMapper;
    private final AuditService auditService;

    /**
     * Resultado da validação de contrato.
     */
    public sealed interface ContractValidationResult {

        /**
         * Payload válido, pode prosseguir.
         */
        record Valid(TransactionRequest request) implements ContractValidationResult {}

        /**
         * JSON inválido (parse falhou).
         */
        record InvalidJson(String errorMessage, String payloadHash) implements ContractValidationResult {}

        /**
         * ExternalTransactionId ausente ou vazio.
         */
        record MissingExternalId(String payloadHash) implements ContractValidationResult {}

        /**
         * Campos obrigatórios ausentes para persistência.
         */
        record MissingRequiredFields(List<String> missingFields, String payloadHash) implements ContractValidationResult {}
    }

    /**
     * Valida o payload raw e retorna o resultado.
     *
     * <p>Diferente de Bean Validation, este método NUNCA retorna 400.
     * Payloads inválidos são classificados como SUSPICIOUS/FRAUD.
     *
     * @param rawBody corpo da requisição como string
     * @param payloadHash hash do payload
     * @return resultado da validação
     */
    public ContractValidationResult validateRawPayload(String rawBody, String payloadHash) {
        // 1. Tentar fazer parse do JSON
        TransactionRequest parsed;
        try {
            parsed = objectMapper.readValue(rawBody, TransactionRequest.class);
        } catch (Exception e) {
            log.warn("Contract validation failed - invalid JSON: {}", e.getMessage());
            auditService.logError("/evaluate", e);
            logContractError(null, payloadHash, "CONTRACT_INVALID_JSON", "Payload JSON inválido (parse falhou)");
            return new ContractValidationResult.InvalidJson(e.getMessage(), payloadHash);
        }

        // 2. Verificar externalTransactionId
        String externalTransactionId = parsed == null ? null : parsed.getExternalTransactionId();
        if (externalTransactionId == null || externalTransactionId.isBlank()) {
            log.warn("Contract validation failed - missing externalTransactionId");
            logContractError(null, payloadHash, "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID",
                "externalTransactionId ausente ou vazio");
            return new ContractValidationResult.MissingExternalId(payloadHash);
        }

        // 3. Verificar campos obrigatórios para persistência
        List<String> missingFields = checkRequiredFieldsForPersistence(parsed);
        if (!missingFields.isEmpty()) {
            log.warn("Contract validation failed - missing required fields: {}", missingFields);
            logContractError(externalTransactionId, payloadHash, "CONTRACT_MISSING_REQUIRED_FIELDS",
                "Campos obrigatórios ausentes: " + String.join(", ", missingFields));
            return new ContractValidationResult.MissingRequiredFields(missingFields, payloadHash);
        }

        return new ContractValidationResult.Valid(parsed);
    }

    /**
     * Verifica campos obrigatórios para persistência no banco.
     *
     * @param request requisição parseada
     * @return lista de campos ausentes
     */
    public List<String> checkRequiredFieldsForPersistence(TransactionRequest request) {
        List<String> missing = new ArrayList<>();

        if (isBlank(request.getCustomerIdFromHeader())) missing.add("customerIdFromHeader");
        if (request.getCustomerAcctNumber() == null) missing.add("customerAcctNumber");
        if (isBlank(request.getPan())) missing.add("pan");
        if (request.getTransactionCurrencyCode() == null) missing.add("transactionCurrencyCode");
        if (request.getTransactionAmount() == null) missing.add("transactionAmount");
        if (request.getTransactionDate() == null) missing.add("transactionDate");
        if (request.getTransactionTime() == null) missing.add("transactionTime");
        if (request.getMcc() == null) missing.add("mcc");
        if (request.getConsumerAuthenticationScore() == null) missing.add("consumerAuthenticationScore");
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

    /**
     * Determina a classificação para um erro de contrato.
     */
    public TransactionDecision.TransactionClassification getClassificationForError(ContractValidationResult result) {
        return switch (result) {
            case ContractValidationResult.InvalidJson ignored ->
                TransactionDecision.TransactionClassification.FRAUD;
            case ContractValidationResult.MissingExternalId ignored ->
                TransactionDecision.TransactionClassification.FRAUD;
            case ContractValidationResult.MissingRequiredFields ignored ->
                TransactionDecision.TransactionClassification.SUSPICIOUS;
            case ContractValidationResult.Valid ignored ->
                TransactionDecision.TransactionClassification.APPROVED;
        };
    }

    /**
     * Gera mensagem de erro para o resultado de validação.
     */
    public String getErrorMessage(ContractValidationResult result) {
        return switch (result) {
            case ContractValidationResult.InvalidJson r ->
                "Payload JSON inválido (parse falhou): " + r.errorMessage();
            case ContractValidationResult.MissingExternalId ignored ->
                "externalTransactionId ausente ou vazio";
            case ContractValidationResult.MissingRequiredFields r ->
                "Campos obrigatórios ausentes para persistência: " + String.join(", ", r.missingFields());
            case ContractValidationResult.Valid ignored ->
                null;
        };
    }

    /**
     * Gera nome da regra de contrato para o resultado.
     */
    public String getContractRuleName(ContractValidationResult result) {
        return switch (result) {
            case ContractValidationResult.InvalidJson ignored -> "CONTRACT_INVALID_JSON";
            case ContractValidationResult.MissingExternalId ignored -> "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID";
            case ContractValidationResult.MissingRequiredFields ignored -> "CONTRACT_MISSING_REQUIRED_FIELDS";
            case ContractValidationResult.Valid ignored -> null;
        };
    }

    /**
     * Verifica se string é nula ou vazia.
     */
    private boolean isBlank(String s) {
        return s == null || s.isBlank();
    }

    /**
     * Registra erro de contrato no log de execução.
     */
    private void logContractError(String externalTransactionId, String payloadHash, String ruleName, String detail) {
        try {
            // Log contract errors via standard logging - execution log service doesn't have a specific method for this
            log.warn("Contract error for {}: {} - {}", externalTransactionId, ruleName, detail);
        } catch (Exception e) {
            log.warn("Failed to log contract error: {}", e.getMessage());
        }
    }
}
