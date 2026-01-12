package com.rulex.repository;

import com.rulex.entity.VelocityTransactionLog;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
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
  @Modifying
  @Query("DELETE FROM VelocityTransactionLog v WHERE v.transactionAt < :cutoff")
  void deleteOlderThan(@Param("cutoff") OffsetDateTime cutoff);

  // =====================================================
  // QUERY ÚNICA AGREGADA - Elimina problema N+1
  // Retorna todas as estatísticas de velocity em uma única query
  // =====================================================

  /**
   * Query única agregada que retorna todas as estatísticas de velocity por PAN.
   * Elimina o problema N+1 ao calcular COUNT, SUM, AVG, DISTINCT em uma query.
   *
   * @return Object[] com: [0]=count, [1]=sum, [2]=avg, [3]=distinctMerchants,
   *                       [4]=distinctMccs, [5]=distinctCountries, [6]=fraudCount
   */
  @Query(
      value =
          "SELECT "
              + "  COUNT(*) as tx_count, "
              + "  COALESCE(SUM(amount), 0) as total_amount, "
              + "  COALESCE(AVG(amount), 0) as avg_amount, "
              + "  COUNT(DISTINCT merchant_id) as distinct_merchants, "
              + "  COUNT(DISTINCT mcc) as distinct_mccs, "
              + "  COUNT(DISTINCT merchant_country) as distinct_countries, "
              + "  COUNT(*) FILTER (WHERE decision = 'FRAUD') as fraud_count, "
              + "  COUNT(*) FILTER (WHERE transaction_status = 'DECLINED') as decline_count, "
              + "  COUNT(*) FILTER (WHERE transaction_status = 'CHARGEBACK') as chargeback_count, "
              + "  MAX(amount) as max_amount, "
              + "  MIN(amount) as min_amount, "
              + "  COUNT(DISTINCT device_fingerprint) as distinct_devices, "
              + "  COUNT(DISTINCT ip_address) as distinct_ips "
              + "FROM velocity_transaction_log "
              + "WHERE pan_hash = :panHash AND transaction_at >= :since",
      nativeQuery = true)
  Object[] getAggregatedStatsByPanHash(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /**
   * Query única agregada que retorna todas as estatísticas de velocity por customerId.
   * Elimina o problema N+1 ao calcular COUNT, SUM, AVG, DISTINCT em uma query.
   *
   * @return Object[] com: [0]=count, [1]=sum, [2]=avg, [3]=distinctMerchants,
   *                       [4]=distinctMccs, [5]=distinctCountries, [6]=fraudCount
   */
  @Query(
      value =
          "SELECT "
              + "  COUNT(*) as tx_count, "
              + "  COALESCE(SUM(amount), 0) as total_amount, "
              + "  COALESCE(AVG(amount), 0) as avg_amount, "
              + "  COUNT(DISTINCT merchant_id) as distinct_merchants, "
              + "  COUNT(DISTINCT mcc) as distinct_mccs, "
              + "  COUNT(DISTINCT merchant_country) as distinct_countries, "
              + "  COUNT(*) FILTER (WHERE decision = 'FRAUD') as fraud_count, "
              + "  COUNT(*) FILTER (WHERE transaction_status = 'DECLINED') as decline_count, "
              + "  COUNT(*) FILTER (WHERE transaction_status = 'CHARGEBACK') as chargeback_count, "
              + "  MAX(amount) as max_amount, "
              + "  MIN(amount) as min_amount, "
              + "  COUNT(DISTINCT device_fingerprint) as distinct_devices, "
              + "  COUNT(DISTINCT ip_address) as distinct_ips, "
              + "  COUNT(DISTINCT beneficiary_id) as distinct_beneficiaries "
              + "FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since",
      nativeQuery = true)
  Object[] getAggregatedStatsByCustomerId(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /**
   * Conta devices distintos usados pelo customer em janela temporal.
   * Usado para detectar device hopping.
   */
  @Query(
      value =
          "SELECT COUNT(DISTINCT device_fingerprint) FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since "
              + "AND device_fingerprint IS NOT NULL",
      nativeQuery = true)
  long countDistinctDevicesByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /**
   * Conta IPs distintos usados pelo customer em janela temporal.
   * Usado para detectar IP hopping ou uso de VPN/proxies.
   */
  @Query(
      value =
          "SELECT COUNT(DISTINCT ip_address) FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since "
              + "AND ip_address IS NOT NULL",
      nativeQuery = true)
  long countDistinctIpsByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /**
   * Conta beneficiários distintos para detectar money mule patterns.
   */
  @Query(
      value =
          "SELECT COUNT(DISTINCT beneficiary_id) FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since "
              + "AND beneficiary_id IS NOT NULL",
      nativeQuery = true)
  long countDistinctBeneficiariesByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /**
   * Conta transações crypto (MCCs 6051, 6012, 6211) em janela temporal.
   */
  @Query(
      value =
          "SELECT COUNT(*) FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since "
              + "AND (mcc IN ('6051', '6012', '6211') OR is_crypto_transaction = true)",
      nativeQuery = true)
  long countCryptoTransactionsByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /**
   * Soma valores de transações crypto em janela temporal.
   */
  @Query(
      value =
          "SELECT COALESCE(SUM(amount), 0) FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since "
              + "AND (mcc IN ('6051', '6012', '6211') OR is_crypto_transaction = true)",
      nativeQuery = true)
  BigDecimal sumCryptoAmountByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  /**
   * Conta chargebacks em janela temporal.
   */
  @Query(
      value =
          "SELECT COUNT(*) FROM velocity_transaction_log "
              + "WHERE pan_hash = :panHash AND transaction_at >= :since "
              + "AND transaction_status = 'CHARGEBACK'",
      nativeQuery = true)
  long countChargebacksByPanHashSince(
      @Param("panHash") String panHash, @Param("since") OffsetDateTime since);

  /**
   * Verifica se device mudou na sessão atual.
   * Retorna TRUE se o device_fingerprint atual é diferente do último usado.
   */
  @Query(
      value =
          "SELECT device_fingerprint FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId "
              + "AND device_fingerprint IS NOT NULL "
              + "ORDER BY transaction_at DESC LIMIT 1",
      nativeQuery = true)
  Optional<String> findLastDeviceFingerprintByCustomerId(@Param("customerId") String customerId);

  /**
   * Busca última atividade do customer para calcular days_since_last_activity.
   */
  @Query(
      value =
          "SELECT MAX(transaction_at) FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId",
      nativeQuery = true)
  Optional<OffsetDateTime> findLastActivityByCustomerId(@Param("customerId") String customerId);

  /**
   * Calcula outflow rate (saída vs entrada) em janela temporal.
   * Retorna a razão de valor de saída sobre entrada.
   */
  @Query(
      value =
          "SELECT "
              + "  COALESCE(SUM(CASE WHEN amount < 0 THEN ABS(amount) ELSE 0 END), 0) as outflow, "
              + "  COALESCE(SUM(CASE WHEN amount > 0 THEN amount ELSE 0 END), 0) as inflow "
              + "FROM velocity_transaction_log "
              + "WHERE customer_id = :customerId AND transaction_at >= :since",
      nativeQuery = true)
  Object[] getOutflowInflowByCustomerIdSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);
}
