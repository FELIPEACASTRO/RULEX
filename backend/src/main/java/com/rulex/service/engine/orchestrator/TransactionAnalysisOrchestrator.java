package com.rulex.service.engine.orchestrator;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.service.DerivedContext;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Orquestrador principal para análise de transações.
 * 
 * <p>Este serviço coordena o fluxo completo de análise, delegando responsabilidades
 * específicas para serviços especializados.
 * 
 * <p>NOTA: Esta é uma versão simplificada. A integração completa com os serviços
 * extraídos será feita na próxima fase de refatoração.
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TransactionAnalysisOrchestrator {

    // Serviços serão integrados na próxima fase
    // Por enquanto, este é um placeholder para a arquitetura

    /**
     * Interface funcional para avaliação de regras.
     * Permite injetar a lógica de avaliação do RuleEngineService.
     */
    @FunctionalInterface
    public interface RuleEvaluator {
        RuleEvaluationOutput evaluate(Transaction transaction, TransactionRequest request, DerivedContext context);
    }

    /**
     * Output da avaliação de regras.
     */
    public record RuleEvaluationOutput(
        TransactionDecision.TransactionClassification classification,
        int riskScore,
        List<TriggeredRuleDTO> triggeredRules,
        Map<String, Object> scoreDetails
    ) {}

    /**
     * Placeholder para análise de transação.
     * A implementação completa será feita na próxima fase.
     */
    public void placeholder() {
        log.info("TransactionAnalysisOrchestrator initialized - ready for integration");
    }
}
