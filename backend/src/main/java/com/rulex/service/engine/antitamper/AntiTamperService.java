package com.rulex.service.engine.antitamper;

import com.rulex.entity.TransactionDecision;
import com.rulex.entity.TransactionRawStore;
import com.rulex.service.AuditService;
import com.rulex.service.PayloadHashService;
import com.rulex.service.TransactionRawStoreService;
import com.rulex.v31.execlog.ExecutionEventType;
import com.rulex.v31.execlog.RuleExecutionLogService;
import java.time.Clock;
import java.time.LocalDateTime;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço responsável por detecção de tamper e garantia de idempotência.
 * 
 * <p>Extraído de RuleEngineService para seguir Single Responsibility Principle.
 * 
 * <p>Responsabilidades:
 * <ul>
 *   <li>Calcular hash SHA-256 do payload</li>
 *   <li>Detectar tentativas de tamper (mesmo ID, payload diferente)</li>
 *   <li>Garantir idempotência (mesmo ID, mesmo payload = mesma resposta)</li>
 *   <li>Armazenar payload raw para auditoria</li>
 * </ul>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class AntiTamperService {

    private final PayloadHashService payloadHashService;
    private final TransactionRawStoreService rawStoreService;
    private final AuditService auditService;
    private final RuleExecutionLogService ruleExecutionLogService;
    private final Clock clock;

    /**
     * Resultado da verificação anti-tamper.
     */
    public sealed interface TamperCheckResult {
        
        /**
         * Payload é novo, pode prosseguir com processamento.
         */
        record NewPayload(String payloadHash) implements TamperCheckResult {}
        
        /**
         * Payload já existe e hash confere - idempotência.
         */
        record IdempotentPayload(String payloadHash, TransactionRawStore existingRaw) implements TamperCheckResult {}
        
        /**
         * Tentativa de tamper detectada - mesmo ID, hash diferente.
         */
        record TamperDetected(String payloadHash, String existingHash, TransactionDecision tamperDecision) implements TamperCheckResult {}
    }

    /**
     * Verifica se o payload é novo, idempotente ou uma tentativa de tamper.
     * 
     * @param externalTransactionId ID externo da transação
     * @param rawBytes bytes do payload como recebido
     * @param contentType content-type da requisição
     * @return resultado da verificação
     */
    public TamperCheckResult checkPayload(String externalTransactionId, byte[] rawBytes, String contentType) {
        byte[] effectiveRawBytes = rawBytes == null ? new byte[0] : rawBytes;
        String payloadHash = payloadHashService.sha256Hex(effectiveRawBytes);

        Optional<TransactionRawStore> existingRawOpt = 
            rawStoreService.findByExternalTransactionId(externalTransactionId);

        if (existingRawOpt.isPresent()) {
            TransactionRawStore existingRaw = existingRawOpt.get();
            
            if (!existingRaw.getPayloadRawHash().equals(payloadHash)) {
                // TAMPER DETECTADO!
                log.warn("🚨 TAMPER DETECTED: externalTransactionId={}, existingHash={}, newHash={}",
                    externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);
                
                auditService.logTamperAttempt(externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);
                logAntiTamperEvent(externalTransactionId, existingRaw.getPayloadRawHash(), payloadHash);
                
                TransactionDecision tamperDecision = buildTamperDecision(externalTransactionId, payloadHash);
                return new TamperCheckResult.TamperDetected(payloadHash, existingRaw.getPayloadRawHash(), tamperDecision);
            }
            
            // Idempotência - mesmo payload
            log.debug("Idempotent request detected: externalTransactionId={}", externalTransactionId);
            return new TamperCheckResult.IdempotentPayload(payloadHash, existingRaw);
        }

        // Payload novo - armazenar para auditoria
        rawStoreService.store(externalTransactionId, payloadHash, effectiveRawBytes, contentType);
        return new TamperCheckResult.NewPayload(payloadHash);
    }

    /**
     * Calcula o hash SHA-256 do payload.
     */
    public String calculateHash(byte[] rawBytes) {
        byte[] effectiveRawBytes = rawBytes == null ? new byte[0] : rawBytes;
        return payloadHashService.sha256Hex(effectiveRawBytes);
    }

    /**
     * Verifica se um hash existente confere com o novo payload.
     */
    public boolean hashMatches(String existingHash, byte[] rawBytes) {
        if (existingHash == null) {
            return true; // Sem hash anterior = aceita
        }
        String newHash = calculateHash(rawBytes);
        return existingHash.equals(newHash);
    }

    /**
     * Constrói uma decisão de FRAUD para tentativa de tamper.
     */
    public TransactionDecision buildTamperDecision(String externalTransactionId, String payloadHash) {
        TransactionDecision decision = new TransactionDecision();
        decision.setClassification(TransactionDecision.TransactionClassification.FRAUD);
        decision.setRiskScore(100);
        decision.setReason("ANTI-TAMPER: Payload hash mismatch detected. Transaction blocked.");
        decision.setRulesApplied("[{\"name\":\"ANTI_TAMPER\",\"weight\":100,\"contribution\":100,\"detail\":\"Hash mismatch\"}]");
        decision.setExternalTransactionId(externalTransactionId);
        decision.setPayloadRawHash(payloadHash);
        decision.setCreatedAt(LocalDateTime.now(clock));
        return decision;
    }

    /**
     * Registra evento de anti-tamper no log de execução.
     */
    private void logAntiTamperEvent(String externalTransactionId, String existingHash, String newHash) {
        try {
            ruleExecutionLogService.logAntiTamper(
                externalTransactionId,
                existingHash,
                newHash,
                null
            );
        } catch (Exception e) {
            log.warn("Failed to log anti-tamper event: {}", e.getMessage());
        }
    }
}
