package com.rulex.repository;

import com.rulex.entity.Transaction;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para acesso aos dados de Transações. */
@Repository
public interface TransactionRepository extends JpaRepository<Transaction, Long> {

  Optional<Transaction> findByExternalTransactionId(String externalTransactionId);

  Page<Transaction> findByCustomerIdFromHeader(String customerId, Pageable pageable);

  Page<Transaction> findByMerchantId(String merchantId, Pageable pageable);

  @Query(
      "SELECT t FROM Transaction t WHERE "
          + "(:customerIdFromHeader IS NULL OR t.customerIdFromHeader = :customerIdFromHeader) AND "
          + "(:merchantId IS NULL OR t.merchantId = :merchantId) AND "
          + "(:mcc IS NULL OR t.mcc = :mcc) AND "
          + "(:minAmount IS NULL OR t.transactionAmount >= :minAmount) AND "
          + "(:maxAmount IS NULL OR t.transactionAmount <= :maxAmount) AND "
          + "(:startDate IS NULL OR t.createdAt >= :startDate) AND "
          + "(:endDate IS NULL OR t.createdAt <= :endDate)")
  Page<Transaction> findByFilters(
      @Param("customerIdFromHeader") String customerIdFromHeader,
      @Param("merchantId") String merchantId,
      @Param("mcc") Integer mcc,
      @Param("minAmount") BigDecimal minAmount,
      @Param("maxAmount") BigDecimal maxAmount,
      @Param("startDate") LocalDateTime startDate,
      @Param("endDate") LocalDateTime endDate,
      Pageable pageable);

  @Query(
      "SELECT COUNT(t) FROM Transaction t WHERE "
          + "t.customerIdFromHeader = :customerId AND "
          + "t.createdAt >= :since")
  Long countTransactionsByCustomerSince(
      @Param("customerId") String customerId, @Param("since") LocalDateTime since);

  @Query(
      "SELECT COUNT(t) FROM Transaction t WHERE "
          + "t.merchantId = :merchantId AND "
          + "t.createdAt >= :since")
  Long countTransactionsByMerchantSince(
      @Param("merchantId") String merchantId, @Param("since") LocalDateTime since);

  @Query("SELECT COUNT(t) FROM Transaction t WHERE t.createdAt >= :since")
  Long countSince(@Param("since") LocalDateTime since);

  // ==================== MÉTODOS PARA AS 28 NOVAS REGRAS ====================

  @Query(
      "SELECT COUNT(t) FROM Transaction t WHERE t.pan = :pan AND t.posCardCapture = 1 AND t.createdAt >= :since")
  long countCardCapturesSince(@Param("pan") String pan, @Param("since") LocalDateTime since);

  @Query(
      "SELECT COUNT(t) FROM Transaction t WHERE t.externalTransactionId = :externalTransactionId AND t.transactionDate = :transactionDate")
  long countDuplicateTransactions(
      @Param("externalTransactionId") String externalTransactionId,
      @Param("transactionDate") int transactionDate);

  @Query(
      "SELECT COUNT(t) FROM Transaction t WHERE t.customerIdFromHeader = :customerId AND t.transactionDate = :transactionDate")
  long countDailyTransactions(
      @Param("customerId") String customerId, @Param("transactionDate") int transactionDate);

  @Query(
      "SELECT AVG(t.transactionAmount) FROM Transaction t WHERE t.customerIdFromHeader = :customerId AND t.createdAt >= CURRENT_DATE - :days")
  Optional<BigDecimal> getCustomerAverageAmount(
      @Param("customerId") String customerId, @Param("days") int days);

  @Query(
      "SELECT AVG(t.transactionCurrencyConversionRate) FROM Transaction t WHERE t.transactionCurrencyCode = :currencyCode AND t.createdAt >= CURRENT_DATE - :days")
  Optional<BigDecimal> getAverageCurrencyConversionRate(
      @Param("currencyCode") int currencyCode, @Param("days") int days);
}
