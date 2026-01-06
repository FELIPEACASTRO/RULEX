package com.rulex.application.usecase;

import com.rulex.application.port.in.AnalyzeTransactionUseCase;
import com.rulex.application.port.out.AuditEventPort;
import com.rulex.application.port.out.DecisionPersistencePort;
import com.rulex.application.port.out.MetricsPort;
import com.rulex.application.port.out.RuleCachePort;
import com.rulex.application.port.out.RulePersistencePort;
import com.rulex.application.port.out.TransactionPersistencePort;
import com.rulex.domain.exception.TamperDetectedException;
import com.rulex.domain.model.Classification;
import com.rulex.domain.model.Decision;
import com.rulex.domain.model.Rule;
import com.rulex.domain.model.TransactionData;
import com.rulex.domain.service.RuleEvaluatorService;
import com.rulex.domain.service.TamperDetector;
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Use Case para análise de transações.
 *
 * <p>Orquestra a análise de fraude coordenando Domain Services e Ports. Implementa
 * AnalyzeTransactionUseCase.
 *
 * <p><b>Responsabilidades:</b>
 *
 * <ul>
 *   <li>Verificar adulteração de payload (anti-tamper)
 *   <li>Carregar regras ativas (com cache)
 *   <li>Delegar avaliação para RuleEvaluatorService
 *   <li>Persistir decisão
 *   <li>Registrar métricas e auditoria
 * </ul>
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class AnalyzeTransactionUseCaseImpl implements AnalyzeTransactionUseCase {

  // Ports
  private final RulePersistencePort rulePersistencePort;
  private final TransactionPersistencePort transactionPersistencePort;
  private final DecisionPersistencePort decisionPersistencePort;
  private final RuleCachePort ruleCachePort;
  private final MetricsPort metricsPort;
  private final AuditEventPort auditEventPort;

  // Domain Services (sem anotações Spring)
  private final RuleEvaluatorService ruleEvaluatorService = new RuleEvaluatorService();
  private final TamperDetector tamperDetector = new TamperDetector();

  @Override
  @Transactional
  public AnalysisResult analyze(TransactionData transactionData) {
    long startTime = System.currentTimeMillis();
    String externalTransactionId = transactionData.getTransactionId();
    Map<String, Object> payload = transactionData.getData();

    log.debug("Iniciando análise: externalTransactionId={}", externalTransactionId);

    try {
      // 1. Calcular hash do payload para anti-tamper
      String payloadHash = calculatePayloadHash(payload);

      // 2. Verificar se já existe decisão (idempotência + anti-tamper)
      Optional<String> existingHash =
          transactionPersistencePort.findPayloadHashByExternalId(externalTransactionId);

      if (existingHash.isPresent()) {
        // Verificar adulteração
        if (tamperDetector.isTampered(existingHash.get(), payloadHash)) {
          auditEventPort.logTamperDetected(externalTransactionId, existingHash.get(), payloadHash);
          throw new TamperDetectedException(
              externalTransactionId, existingHash.get(), payloadHash);
        }

        // Retornar decisão existente (idempotência)
        Optional<Decision> existingDecision =
            decisionPersistencePort.findByExternalTransactionId(externalTransactionId);
        if (existingDecision.isPresent()) {
          log.debug("Retornando decisão existente (idempotência): {}", externalTransactionId);
          return toAnalysisResult(existingDecision.get(), System.currentTimeMillis() - startTime);
        }
      }

      // 3. Carregar regras ativas (com cache)
      List<Rule> activeRules = loadActiveRules();

      // 4. Avaliar regras usando Domain Service
      Decision decision = ruleEvaluatorService.evaluate(activeRules, payload, externalTransactionId);

      // 5. Avaliar regras shadow (métricas apenas)
      List<Rule> shadowRules = loadShadowRules();
      List<TriggeredRule> shadowTriggeredRules = new ArrayList<>();
      if (!shadowRules.isEmpty()) {
        var shadowResults = ruleEvaluatorService.evaluateShadow(shadowRules, payload);
        shadowResults.forEach(
            r -> {
              if (r.triggered()) {
                metricsPort.recordShadowRuleTriggered(r.ruleName());
                shadowTriggeredRules.add(
                    new TriggeredRule(
                        r.ruleId().toString(),
                        r.ruleName(),
                        "SHADOW",
                        r.score(),
                        true,
                        r.message()));
              }
            });
      }

      // 6. Registrar métricas
      long processingTime = System.currentTimeMillis() - startTime;
      metricsPort.recordEvaluation(
          processingTime,
          decision.getClassification().name(),
          decision.getRiskScore(),
          activeRules.size(),
          decision.getTriggeredRules().size());

      // 7. Registrar auditoria
      auditEventPort.logTransactionAnalyzed(
          externalTransactionId,
          decision.getClassification().name(),
          decision.getRiskScore(),
          processingTime);

      log.info(
          "Análise concluída: externalTransactionId={}, classification={}, score={}, time={}ms",
          externalTransactionId,
          decision.getClassification(),
          decision.getRiskScore(),
          processingTime);

      // 8. Converter para AnalysisResult incluindo shadow rules
      AnalysisResult result = toAnalysisResult(decision, processingTime);

      // Adicionar shadow rules ao resultado
      if (!shadowTriggeredRules.isEmpty()) {
        List<TriggeredRule> allRules = new ArrayList<>(result.triggeredRules());
        allRules.addAll(shadowTriggeredRules);
        result = new AnalysisResult(result.classification(), result.totalScore(), allRules, result.processingTimeMs());
      }

      return result;

    } catch (TamperDetectedException e) {
      // Re-throw exceções de domínio
      metricsPort.incrementErrorCount("TAMPER_DETECTED");
      throw e;
    } catch (Exception e) {
      metricsPort.incrementErrorCount("ANALYSIS_ERROR");
      log.error(
          "Erro na análise: externalTransactionId={}, error={}",
          externalTransactionId,
          e.getMessage(),
          e);
      throw new RuntimeException("Erro ao analisar transação: " + e.getMessage(), e);
    }
  }

  /**
   * Converte uma Decision de domínio para AnalysisResult.
   */
  private AnalysisResult toAnalysisResult(Decision decision, long processingTimeMs) {
    List<TriggeredRule> triggeredRules =
        decision.getTriggeredRules().stream()
            .map(
                tr ->
                    new TriggeredRule(
                        tr.getRuleName(), // ruleId não existe, usar ruleName
                        tr.getRuleName(),
                        tr.getRuleType(),
                        tr.getWeight() != null ? tr.getWeight() : 0,
                        false,
                        tr.getDetail()))
            .toList();

    return new AnalysisResult(
        decision.getClassification(), decision.getRiskScore(), triggeredRules, processingTimeMs);
  }

  // ========== Métodos de Cache ==========

  @SuppressWarnings("unchecked")
  private List<Rule> loadActiveRules() {
    // Tentar cache primeiro
    Optional<List<Rule>> cached =
        ruleCachePort.get(RuleCachePort.ACTIVE_RULES_KEY, (Class<List<Rule>>) (Class<?>) List.class);

    if (cached.isPresent()) {
      metricsPort.recordCacheAccess(true);
      return cached.get();
    }

    // Cache miss - carregar do banco
    metricsPort.recordCacheAccess(false);
    List<Rule> rules = rulePersistencePort.findAllActiveOrderedByPriority();
    ruleCachePort.put(RuleCachePort.ACTIVE_RULES_KEY, rules);
    metricsPort.recordCacheSize(rules.size());

    return rules;
  }

  @SuppressWarnings("unchecked")
  private List<Rule> loadShadowRules() {
    Optional<List<Rule>> cached =
        ruleCachePort.get(RuleCachePort.SHADOW_RULES_KEY, (Class<List<Rule>>) (Class<?>) List.class);

    if (cached.isPresent()) {
      return cached.get();
    }

    List<Rule> rules = rulePersistencePort.findShadowRules();
    ruleCachePort.put(RuleCachePort.SHADOW_RULES_KEY, rules);
    return rules;
  }

  // ========== Utilitários ==========

  private String calculatePayloadHash(Map<String, Object> payload) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      String json = payload.toString(); // Simplificado - ideal usar Jackson
      byte[] hash = digest.digest(json.getBytes());
      return HexFormat.of().formatHex(hash);
    } catch (Exception e) {
      log.warn("Erro ao calcular hash do payload: {}", e.getMessage());
      return "";
    }
  }
}
