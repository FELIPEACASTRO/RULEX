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
      "SELECT COALESCE(SUM(t.transferAmount), 0) FROM CustomerIncomingTransfer t "
          + "WHERE t.customerId = :customerId "
          + "AND t.transferDate >= CURRENT_TIMESTAMP - :days * INTERVAL '1 day'")
  BigDecimal sumIncomingAmountLastNDays(
      @Param("customerId") String customerId, @Param("days") int days);
}
