package com.rulex.repository;

import com.rulex.entity.CustomerChargebackHistory;
import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CustomerChargebackHistoryRepository
    extends JpaRepository<CustomerChargebackHistory, UUID> {

  List<CustomerChargebackHistory> findByCustomerId(String customerId);

  @Query(
      "SELECT DISTINCT h.merchantId FROM CustomerChargebackHistory h "
          + "WHERE h.customerId = :customerId")
  List<String> findMerchantIdsWithChargebackByCustomer(@Param("customerId") String customerId);

  @Query(
      "SELECT CASE WHEN COUNT(h) > 0 THEN true ELSE false END "
          + "FROM CustomerChargebackHistory h "
          + "WHERE h.customerId = :customerId AND h.merchantId = :merchantId")
  boolean hasChargebackWithMerchant(
      @Param("customerId") String customerId, @Param("merchantId") String merchantId);

  @Query("SELECT COUNT(h) FROM CustomerChargebackHistory h WHERE h.customerId = :customerId")
  long countByCustomerId(@Param("customerId") String customerId);
}
