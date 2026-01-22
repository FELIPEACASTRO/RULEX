package com.rulex.service.engine.decision;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import com.rulex.service.AuditService;
import com.rulex.util.PanMaskingUtil;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço responsável pela persistência de transações e decisões.
 *
 * <p>Extraído de RuleEngineService para seguir Single Responsibility Principle.
 *
 * <p>Responsabilidades:
 * <ul>
 *   <li>Converter TransactionRequest para entidade Transaction</li>
 *   <li>Persistir transações no banco</li>
 *   <li>Criar e persistir decisões (TransactionDecision)</li>
 *   <li>Tratar condições de corrida (race conditions)</li>
 * </ul>
 *
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DecisionPersistenceService {

    private final TransactionRepository transactionRepository;
    private final TransactionDecisionRepository decisionRepository;
    private final AuditService auditService;
    private final ObjectMapper objectMapper;
    private final Clock clock;

    /**
     * Resultado da persistência de transação.
     */
    public sealed interface PersistenceResult {

        /**
         * Transação criada com sucesso.
         */
        record Created(Transaction transaction) implements PersistenceResult {}

        /**
         * Transação já existia (idempotência).
         */
        record AlreadyExists(Transaction existingTransaction) implements PersistenceResult {}

        /**
         * Condição de corrida detectada - outra thread criou primeiro.
         */
        record RaceCondition(Transaction existingTransaction) implements PersistenceResult {}
    }

    /**
     * Busca transação existente por ID externo.
     */
    public Optional<Transaction> findByExternalId(String externalTransactionId) {
        return transactionRepository.findByExternalTransactionId(externalTransactionId);
    }

    /**
     * Persiste uma nova transação, tratando condições de corrida.
     *
     * @param request dados da requisição
     * @param payloadHash hash do payload
     * @return resultado da persistência
     */
    @Transactional
    public PersistenceResult persistTransaction(TransactionRequest request, String payloadHash) {
        // Verificar se já existe
        Optional<Transaction> existingOpt =
            transactionRepository.findByExternalTransactionId(request.getExternalTransactionId());

        if (existingOpt.isPresent()) {
            log.debug("Transaction already exists: {}", request.getExternalTransactionId());
            return new PersistenceResult.AlreadyExists(existingOpt.get());
        }

        // Converter e salvar
        Transaction transaction = convertRequestToEntity(request);
        transaction.setPayloadRawHash(payloadHash);

        try {
            transaction = transactionRepository.save(transaction);
            log.debug("Transaction persisted: id={}, externalId={}",
                transaction.getId(), transaction.getExternalTransactionId());
            return new PersistenceResult.Created(transaction);

        } catch (DataIntegrityViolationException e) {
            // Race condition - outra thread inseriu primeiro
            log.warn("Race condition detected for externalTransactionId={}",
                request.getExternalTransactionId());

            Transaction racedTx = transactionRepository
                .findByExternalTransactionId(request.getExternalTransactionId())
                .orElseThrow(() -> e);

            return new PersistenceResult.RaceCondition(racedTx);
        }
    }

    /**
     * Cria e persiste uma decisão para a transação.
     */
    @Transactional
    public TransactionDecision persistDecision(
            Transaction transaction,
            TransactionDecision.TransactionClassification classification,
            int riskScore,
            String reason,
            List<TriggeredRuleDTO> triggeredRules,
            Map<String, Object> scoreDetails,
            String payloadHash) {

        TransactionDecision decision = new TransactionDecision();
        decision.setTransaction(transaction);
        decision.setClassification(classification);
        decision.setRiskScore(riskScore);
        decision.setReason(reason);
        decision.setRulesApplied(writeJson(triggeredRules));
        decision.setScoreDetails(writeJson(scoreDetails));
        decision.setExternalTransactionId(transaction.getExternalTransactionId());
        decision.setPayloadRawHash(payloadHash);
        decision.setCreatedAt(LocalDateTime.now(clock));

        decision = decisionRepository.save(decision);
        log.debug("Decision persisted: id={}, classification={}", decision.getId(), classification);

        return decision;
    }

    /**
     * Persiste uma decisão de tamper (sem transação associada).
     */
    @Transactional
    public TransactionDecision persistTamperDecision(TransactionDecision tamperDecision) {
        return decisionRepository.save(tamperDecision);
    }

    /**
     * Busca a última decisão para uma transação.
     */
    public Optional<TransactionDecision> findLatestDecision(Transaction transaction) {
        return decisionRepository.findByTransactionId(transaction.getId())
            .stream()
            .max(java.util.Comparator.comparing(TransactionDecision::getCreatedAt));
    }

    /**
     * Converte TransactionRequest para entidade Transaction.
     */
    public Transaction convertRequestToEntity(TransactionRequest request) {
        Transaction tx = new Transaction();

        tx.setExternalTransactionId(request.getExternalTransactionId());
        tx.setCustomerIdFromHeader(request.getCustomerIdFromHeader());
        tx.setCustomerAcctNumber(request.getCustomerAcctNumber());

        // Mascarar PAN antes de armazenar
        String pan = request.getPan();
        tx.setPan(PanMaskingUtil.mask(pan));

        tx.setMerchantId(request.getMerchantId());
        tx.setMerchantName(request.getMerchantName());
        tx.setTransactionCurrencyCode(request.getTransactionCurrencyCode());
        tx.setTransactionAmount(request.getTransactionAmount());

        // Armazenar data e hora como Integer (formato original)
        tx.setTransactionDate(request.getTransactionDate());
        tx.setTransactionTime(request.getTransactionTime());

        tx.setMerchantCountryCode(request.getMerchantCountryCode());
        tx.setMerchantCity(request.getMerchantCity());
        tx.setMcc(request.getMcc());
        tx.setPosEntryMode(truncatePosEntryMode(request.getPosEntryMode()));

        tx.setCreatedAt(LocalDateTime.now(clock));

        return tx;
    }

    /**
     * Trunca posEntryMode para caber no campo do banco (max 10 chars).
     */
    private String truncatePosEntryMode(String posEntryMode) {
        if (posEntryMode == null) {
            return null;
        }
        return posEntryMode.length() > 10 ? posEntryMode.substring(0, 10) : posEntryMode;
    }

    /**
     * Serializa objeto para JSON.
     */
    private String writeJson(Object value) {
        if (value == null) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(value);
        } catch (JsonProcessingException e) {
            log.warn("Failed to serialize to JSON: {}", e.getMessage());
            return "{}";
        }
    }
}
