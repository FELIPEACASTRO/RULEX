package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.NotFoundException;
import com.rulex.dto.TransactionResponse;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.Transaction;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Serviço para consulta de transações e decisões. */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class TransactionQueryService {

  private final TransactionRepository transactionRepository;
  private final TransactionDecisionRepository decisionRepository;
  private final ObjectMapper objectMapper;

  /** Busca transações com filtros. */
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
        transactionRepository.findByFiltersWithClassification(
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
  public TransactionResponse getTransactionById(Long id) {
    Transaction transaction =
        transactionRepository
            .findById(id)
            .orElseThrow(() -> new NotFoundException("Transação não encontrada"));

    return convertToResponse(transaction);
  }

  /** Obtém uma transação pelo ID externo. */
  public TransactionResponse getTransactionByExternalId(String externalId) {
    Transaction transaction =
        transactionRepository
            .findByExternalTransactionId(externalId)
            .orElseThrow(() -> new NotFoundException("Transação não encontrada"));

    return convertToResponse(transaction);
  }

  /** Converte uma entidade Transaction para TransactionResponse. */
  private TransactionResponse convertToResponse(Transaction transaction) {
    TransactionDecision decision = decisionRepository.findByTransactionId(transaction.getId()).orElse(null);
    return convertToResponse(transaction, decision);
  }

  private TransactionResponse convertToResponse(Transaction transaction, TransactionDecision decision) {
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
        .triggeredRules(decision != null ? readTriggeredRules(decision.getRulesApplied()) : List.of())
        .reason(decision != null ? decision.getReason() : "Sem decisão registrada")
        .rulesetVersion(decision != null ? decision.getRulesVersion() : "1")
        .processingTimeMs(0L)
        .timestamp(toOffsetDateTime(decision != null ? decision.getCreatedAt() : transaction.getCreatedAt()))
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
    for (TransactionDecision d : decisionRepository.findByTransactionIdIn(ids)) {
      if (d != null && d.getTransaction() != null && d.getTransaction().getId() != null) {
        byId.put(d.getTransaction().getId(), d);
      }
    }
    return byId;
  }

  private List<TriggeredRuleDTO> readTriggeredRules(String rulesApplied) {
    if (rulesApplied == null || rulesApplied.isBlank()) {
      return List.of();
    }
    try {
      return objectMapper.readValue(
          rulesApplied,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, TriggeredRuleDTO.class));
    } catch (Exception e) {
      // fallback legado: string CSV
      return List.of(rulesApplied.split(",")).stream()
          .map(String::trim)
          .filter(s -> !s.isBlank())
          .map(name -> TriggeredRuleDTO.builder().name(name).weight(0).contribution(0).build())
          .toList();
    }
  }

  private OffsetDateTime toOffsetDateTime(LocalDateTime dt) {
    if (dt == null) {
      return null;
    }
    return dt.atZone(ZoneId.systemDefault()).toOffsetDateTime();
  }

  /**
   * Exporta transações em lista (JSON) com paginação interna e limite máximo.
   *
   * <p>Uso: endpoints de export.
   */
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
      pageable = PageRequest.of(pageable.getPageNumber() + 1, pageable.getPageSize(), pageable.getSort());
    }
    if (out.size() > limit) {
      return out.subList(0, limit);
    }
    return out;
  }

  /**
   * Exporta transações como CSV (streaming) com paginação interna.
   *
   * <p>Escreve linhas CSV (sem header).
   */
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
      pageable = PageRequest.of(pageable.getPageNumber() + 1, pageable.getPageSize(), pageable.getSort());
    }
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
