package com.rulex.infrastructure.persistence;

import com.rulex.application.port.out.DecisionPersistencePort;
import com.rulex.domain.model.Classification;
import com.rulex.domain.model.Decision;
import com.rulex.domain.model.Decision.TriggeredRule;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Adapter JPA para persistência de decisões.
 *
 * <p>Implementa DecisionPersistencePort convertendo entre Domain Model (Decision) e JPA Entity
 * (TransactionDecision).
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class DecisionPersistenceAdapter implements DecisionPersistencePort {

  private final TransactionDecisionRepository repository;
  private final TransactionRepository transactionRepository;

  @Override
  public Optional<Decision> findByExternalTransactionId(String externalTransactionId) {
    // Busca via Transaction entity que tem o externalTransactionId
    return transactionRepository
        .findByExternalTransactionId(externalTransactionId)
        .flatMap(tx -> repository.findByTransactionId(tx.getId()))
        .map(entity -> toDomain(entity, externalTransactionId));
  }

  @Override
  public Decision save(Decision decision, Long transactionId) {
    Transaction transaction =
        transactionRepository
            .findById(transactionId)
            .orElseThrow(() -> new IllegalArgumentException("Transaction not found: " + transactionId));

    TransactionDecision entity = toEntity(decision, transaction);
    TransactionDecision saved = repository.save(entity);
    return toDomain(saved, decision.getExternalTransactionId());
  }

  @Override
  public boolean existsByExternalTransactionId(String externalTransactionId) {
    return transactionRepository
        .findByExternalTransactionId(externalTransactionId)
        .flatMap(tx -> repository.findByTransactionId(tx.getId()))
        .isPresent();
  }

  // ========== Mapeamento Entity <-> Domain ==========

  private Decision toDomain(TransactionDecision entity, String externalTransactionId) {
    return Decision.builder()
        .externalTransactionId(externalTransactionId)
        .classification(toDomainClassification(entity.getClassification()))
        .riskScore(entity.getRiskScore())
        .triggeredRules(parseTriggeredRules(entity.getRulesApplied()))
        .reason(entity.getReason())
        .processingTimeMs(0L) // Entity não tem este campo
        .createdAt(entity.getCreatedAt())
        .build();
  }

  private TransactionDecision toEntity(Decision decision, Transaction transaction) {
    return TransactionDecision.builder()
        .transaction(transaction)
        .externalTransactionId(decision.getExternalTransactionId())
        .classification(toEntityClassification(decision.getClassification()))
        .riskScore(decision.getRiskScore())
        .rulesApplied(serializeTriggeredRules(decision.getTriggeredRules()))
        .reason(decision.getReason())
        .createdAt(LocalDateTime.now())
        .build();
  }

  private Classification toDomainClassification(
      TransactionDecision.TransactionClassification classification) {
    if (classification == null) return Classification.APPROVED;
    return switch (classification) {
      case APPROVED -> Classification.APPROVED;
      case SUSPICIOUS -> Classification.SUSPICIOUS;
      case FRAUD -> Classification.FRAUD;
    };
  }

  private TransactionDecision.TransactionClassification toEntityClassification(
      Classification classification) {
    if (classification == null) return TransactionDecision.TransactionClassification.APPROVED;
    return switch (classification) {
      case APPROVED -> TransactionDecision.TransactionClassification.APPROVED;
      case SUSPICIOUS -> TransactionDecision.TransactionClassification.SUSPICIOUS;
      case FRAUD -> TransactionDecision.TransactionClassification.FRAUD;
    };
  }

  private List<TriggeredRule> parseTriggeredRules(String json) {
    // TODO: Implementar parsing de JSON
    return Collections.emptyList();
  }

  private String serializeTriggeredRules(List<TriggeredRule> rules) {
    if (rules == null || rules.isEmpty()) return "[]";
    StringBuilder sb = new StringBuilder("[");
    for (int i = 0; i < rules.size(); i++) {
      TriggeredRule r = rules.get(i);
      if (i > 0) sb.append(",");
      sb.append(String.format("{\"name\":\"%s\",\"weight\":%d}", r.getRuleName(), r.getWeight()));
    }
    sb.append("]");
    return sb.toString();
  }
}
