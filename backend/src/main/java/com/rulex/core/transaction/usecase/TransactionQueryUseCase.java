package com.rulex.core.transaction.usecase;

import com.rulex.core.transaction.port.TransactionQueryInputPort;
import com.rulex.core.transaction.port.TransactionQueryRepositoryPort;
import com.rulex.core.transaction.port.TriggeredRuleParserPort;
import com.rulex.dto.TransactionResponse;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

/** Caso de uso para consulta de transações e decisões. */
public class TransactionQueryUseCase implements TransactionQueryInputPort {

  private final TransactionQueryRepositoryPort repositoryPort;
  private final TriggeredRuleParserPort triggeredRuleParser;

  public TransactionQueryUseCase(
      TransactionQueryRepositoryPort repositoryPort, TriggeredRuleParserPort triggeredRuleParser) {
    this.repositoryPort = repositoryPort;
    this.triggeredRuleParser = triggeredRuleParser;
  }

  /** Busca transações com filtros. */
  @Override
  public Page<TransactionResponse> findTransactions(
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable pageable) {

    TransactionDecision.TransactionClassification classificationEnum = null;
    if (classification != null && !classification.isBlank()) {
      try {
        classificationEnum = TransactionDecision.TransactionClassification.valueOf(classification);
      } catch (IllegalArgumentException e) {
        throw new IllegalArgumentException(
            "classification inválida (use APPROVED, SUSPICIOUS ou FRAUD)");
      }
    }

    Page<Transaction> transactions =
        repositoryPort.findByFiltersWithClassification(
            customerId,
            merchantId,
            mcc,
            minAmount,
            maxAmount,
            startDate,
            endDate,
            classificationEnum,
            pageable);

    List<Transaction> rows = transactions.getContent();
    Map<Long, TransactionDecision> decisions = loadDecisionsByTransactionId(rows);
    List<TransactionResponse> responses =
        rows.stream().map(t -> convertToResponse(t, decisions.get(t.getId()))).toList();
    return new PageImpl<>(responses, pageable, transactions.getTotalElements());
  }

  /** Obtém uma transação pelo ID. */
  @Override
  public Optional<TransactionResponse> getTransactionById(Long id) {
    return repositoryPort.findById(id).map(this::convertToResponse);
  }

  /** Obtém uma transação pelo ID externo. */
  @Override
  public Optional<TransactionResponse> getTransactionByExternalId(String externalId) {
    return repositoryPort.findByExternalTransactionId(externalId).map(this::convertToResponse);
  }

  /** Exporta transações em lista (JSON) com paginação interna e limite máximo. */
  @Override
  public List<TransactionResponse> exportAsList(
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit) {
    java.util.ArrayList<TransactionResponse> out = new java.util.ArrayList<>();
    Pageable pageable = firstPage;
    while (out.size() < limit) {
      Page<TransactionResponse> page =
          findTransactions(
              customerId,
              merchantId,
              classification,
              mcc,
              minAmount,
              maxAmount,
              startDate,
              endDate,
              pageable);
      out.addAll(page.getContent());
      if (!page.hasNext()) break;
      pageable =
          PageRequest.of(pageable.getPageNumber() + 1, pageable.getPageSize(), pageable.getSort());
    }
    if (out.size() > limit) {
      return out.subList(0, limit);
    }
    return out;
  }

  /** Exporta transações como CSV (streaming) com paginação interna. */
  @Override
  public void exportAsCsv(
      Writer w,
      String customerId,
      String merchantId,
      String classification,
      Integer mcc,
      BigDecimal minAmount,
      BigDecimal maxAmount,
      LocalDateTime startDate,
      LocalDateTime endDate,
      Pageable firstPage,
      int limit)
      throws IOException {
    int written = 0;
    Pageable pageable = firstPage;
    while (written < limit) {
      Page<TransactionResponse> page =
          findTransactions(
              customerId,
              merchantId,
              classification,
              mcc,
              minAmount,
              maxAmount,
              startDate,
              endDate,
              pageable);
      for (TransactionResponse r : page.getContent()) {
        if (written >= limit) break;
        w.write(toCsvLine(r));
        w.write("\n");
        written++;
      }
      if (!page.hasNext()) break;
      pageable =
          PageRequest.of(pageable.getPageNumber() + 1, pageable.getPageSize(), pageable.getSort());
    }
  }

  /** Converte uma entidade Transaction para TransactionResponse. */
  private TransactionResponse convertToResponse(Transaction transaction) {
    TransactionDecision decision =
        repositoryPort.findDecisionByTransactionId(transaction.getId()).orElse(null);
    return convertToResponse(transaction, decision);
  }

  private TransactionResponse convertToResponse(
      Transaction transaction, TransactionDecision decision) {
    return TransactionResponse.builder()
        .id(transaction.getId())
        .transactionId(transaction.getExternalTransactionId())
        .customerIdFromHeader(transaction.getCustomerIdFromHeader())
        .merchantId(transaction.getMerchantId())
        .merchantName(transaction.getMerchantName())
        .transactionAmount(transaction.getTransactionAmount())
        .transactionDate(transaction.getTransactionDate())
        .transactionTime(transaction.getTransactionTime())
        .classification(decision != null ? decision.getClassification().name() : "UNKNOWN")
        .riskScore(decision != null ? decision.getRiskScore() : 0)
        .triggeredRules(
            decision != null ? triggeredRuleParser.parseTriggeredRules(decision.getRulesApplied()) : List.of())
        .reason(decision != null ? decision.getReason() : "Sem decisão registrada")
        .rulesetVersion(decision != null ? decision.getRulesVersion() : "1")
        .processingTimeMs(0L)
        .timestamp(
            toOffsetDateTime(
                decision != null ? decision.getCreatedAt() : transaction.getCreatedAt()))
        .success(true)
        .build();
  }

  private Map<Long, TransactionDecision> loadDecisionsByTransactionId(Collection<Transaction> txs) {
    Map<Long, TransactionDecision> byId = new HashMap<>();
    if (txs == null || txs.isEmpty()) {
      return byId;
    }
    List<Long> ids = txs.stream().map(Transaction::getId).filter(Objects::nonNull).toList();
    if (ids.isEmpty()) {
      return byId;
    }
    for (TransactionDecision d : repositoryPort.findDecisionsByTransactionIdIn(ids)) {
      if (d != null && d.getTransaction() != null && d.getTransaction().getId() != null) {
        byId.put(d.getTransaction().getId(), d);
      }
    }
    return byId;
  }

  private OffsetDateTime toOffsetDateTime(LocalDateTime dt) {
    if (dt == null) {
      return null;
    }
    return dt.atZone(ZoneId.systemDefault()).toOffsetDateTime();
  }

  private String toCsvLine(TransactionResponse r) {
    // keep in sync with header in controller
    return csv(r.getId())
        + ","
        + csv(r.getTransactionId())
        + ","
        + csv(r.getCustomerIdFromHeader())
        + ","
        + csv(r.getMerchantId())
        + ","
        + csv(r.getMerchantName())
        + ","
        + csv(r.getTransactionAmount() != null ? r.getTransactionAmount().toPlainString() : null)
        + ","
        + csv(r.getTransactionDate() != null ? r.getTransactionDate().toString() : null)
        + ","
        + csv(r.getTransactionTime() != null ? r.getTransactionTime().toString() : null)
        + ","
        + csv(r.getClassification())
        + ","
        + csv(r.getRiskScore() != null ? r.getRiskScore().toString() : null)
        + ","
        + csv(r.getTimestamp() != null ? r.getTimestamp().toString() : null)
        + ","
        + csv(r.getReason());
  }

  private static String csv(Object v) {
    if (v == null) return "";
    String s = String.valueOf(v).replace("\r", " ").replace("\n", " ");
    if (s.contains(",") || s.contains("\"")) {
      s = "\"" + s.replace("\"", "\"\"") + "\"";
    }
    return s;
  }
}
