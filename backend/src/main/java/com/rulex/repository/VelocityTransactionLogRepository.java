package com.rulex.repository;

import com.rulex.entity.VelocityTransactionLog;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repositório para log de transações de velocidade. */
@Repository
public interface VelocityTransactionLogRepository
    extends JpaRepository<VelocityTransactionLog, Long> {

  /** Busca por ID externo. */
  Optional<VelocityTransactionLog> findByExternalTransactionId(String externalTransactionId);

  /** Conta transações por PAN em janela temporal. */
  @Query(
      "SELECT COUNT(v) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since")
  long countByPanHashSince(@Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Soma valores por PAN em janela temporal. */
  @Query(
      "SELECT COALESCE(SUM(v.amount), 0) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since")
  BigDecimal sumAmountByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Média de valores por PAN em janela temporal. */
  @Query(
      "SELECT COALESCE(AVG(v.amount), 0) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since")
  BigDecimal avgAmountByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Conta transações por customerId em janela temporal. */
  @Query(
      "SELECT COUNT(v) FROM VelocityTransactionLog v "
          + "WHERE v.customerId = :customerId AND v.transactionAt >= :since")
  long countByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /** Soma valores por customerId em janela temporal. */
  @Query(
      "SELECT COALESCE(SUM(v.amount), 0) FROM VelocityTransactionLog v "
          + "WHERE v.customerId = :customerId AND v.transactionAt >= :since")
  BigDecimal sumAmountByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /** Conta merchants distintos por PAN em janela temporal. */
  @Query(
      "SELECT COUNT(DISTINCT v.merchantId) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since")
  long countDistinctMerchantsByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Conta MCCs distintos por PAN em janela temporal. */
  @Query(
      "SELECT COUNT(DISTINCT v.mcc) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since")
  long countDistinctMccsByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Conta países distintos por PAN em janela temporal. */
  @Query(
      "SELECT COUNT(DISTINCT v.merchantCountry) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since")
  long countDistinctCountriesByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Busca transações recentes por PAN. */
  @Query(
      "SELECT v FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.transactionAt >= :since "
          + "ORDER BY v.transactionAt DESC")
  List<VelocityTransactionLog> findRecentByPanHash(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Conta fraudes por PAN em janela temporal. */
  @Query(
      "SELECT COUNT(v) FROM VelocityTransactionLog v "
          + "WHERE v.panHash = :panHash AND v.decision = 'FRAUD' AND v.transactionAt >= :since")
  long countFraudByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /** Deleta logs antigos para limpeza. */
  @Query("DELETE FROM VelocityTransactionLog v WHERE v.transactionAt < :cutoff")
  void deleteOlderThan(@Param("cutoff") OffsetDateTime cutoff);
}
