package com.rulex.repository;

import com.rulex.entity.MerchantChargeback;
import java.math.BigDecimal;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface MerchantChargebackRepository extends JpaRepository<MerchantChargeback, UUID> {

  Optional<MerchantChargeback> findByMerchantId(String merchantId);

  @Query("SELECT m.chargebackRate FROM MerchantChargeback m WHERE m.merchantId = :merchantId")
  Optional<BigDecimal> findChargebackRateByMerchantId(@Param("merchantId") String merchantId);

  @Query(
      "SELECT CASE WHEN m.chargebackRate > :threshold THEN true ELSE false END "
          + "FROM MerchantChargeback m WHERE m.merchantId = :merchantId")
  boolean hasHighChargebackRate(
      @Param("merchantId") String merchantId, @Param("threshold") BigDecimal threshold);

  @Query(
      "SELECT CASE WHEN m.isBlocked = true THEN true ELSE false END "
          + "FROM MerchantChargeback m WHERE m.merchantId = :merchantId")
  boolean isMerchantBlocked(@Param("merchantId") String merchantId);
}
