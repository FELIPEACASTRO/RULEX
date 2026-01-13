package com.rulex.repository;

import com.rulex.entity.CustomerLastTransaction;
import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CustomerLastTransactionRepository
    extends JpaRepository<CustomerLastTransaction, UUID> {

  Optional<CustomerLastTransaction> findByCustomerId(String customerId);

  @Query(
      "SELECT t.lastTransactionDate FROM CustomerLastTransaction t "
          + "WHERE t.customerId = :customerId")
  Optional<OffsetDateTime> findLastTransactionDateByCustomerId(
      @Param("customerId") String customerId);

  @Query(
      "SELECT CASE WHEN t.lastTransactionDate IS NOT NULL THEN true ELSE false END "
          + "FROM CustomerLastTransaction t WHERE t.customerId = :customerId")
  boolean hasTransactionHistory(@Param("customerId") String customerId);
}
