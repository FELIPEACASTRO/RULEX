package com.rulex.repository;

import com.rulex.entity.CustomerIncomingTransfer;
import java.math.BigDecimal;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CustomerIncomingTransferRepository
    extends JpaRepository<CustomerIncomingTransfer, UUID> {

  @Query(
      "SELECT t FROM CustomerIncomingTransfer t "
          + "WHERE t.customerId = :customerId "
          + "ORDER BY t.transferDate DESC "
          + "LIMIT 1")
  Optional<CustomerIncomingTransfer> findLastIncomingTransfer(
      @Param("customerId") String customerId);

  @Query(
      "SELECT t.transferAmount FROM CustomerIncomingTransfer t "
          + "WHERE t.customerId = :customerId "
          + "ORDER BY t.transferDate DESC "
          + "LIMIT 1")
  Optional<BigDecimal> findLastIncomingAmount(@Param("customerId") String customerId);

  @Query(
      value =
          "SELECT COALESCE(SUM(t.transfer_amount), 0) FROM customer_incoming_transfers t "
              + "WHERE t.customer_id = :customerId "
              + "AND t.transfer_date >= CURRENT_TIMESTAMP - CAST(:days || ' days' AS INTERVAL)",
      nativeQuery = true)
  BigDecimal sumIncomingAmountLastNDays(
      @Param("customerId") String customerId, @Param("days") int days);
}
