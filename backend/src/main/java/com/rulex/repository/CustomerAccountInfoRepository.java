package com.rulex.repository;

import com.rulex.entity.CustomerAccountInfo;
import java.time.OffsetDateTime;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface CustomerAccountInfoRepository extends JpaRepository<CustomerAccountInfo, UUID> {

  Optional<CustomerAccountInfo> findByCustomerId(String customerId);

  @Query(
      "SELECT a.accountCreatedAt FROM CustomerAccountInfo a " + "WHERE a.customerId = :customerId")
  Optional<OffsetDateTime> findAccountCreatedAtByCustomerId(@Param("customerId") String customerId);

  @Query(
      "SELECT CASE WHEN a.accountCreatedAt > :threshold THEN true ELSE false END "
          + "FROM CustomerAccountInfo a WHERE a.customerId = :customerId")
  boolean isAccountNewerThan(
      @Param("customerId") String customerId, @Param("threshold") OffsetDateTime threshold);

  @Query(
      "SELECT EXTRACT(EPOCH FROM (CURRENT_TIMESTAMP - a.accountCreatedAt)) / 60 "
          + "FROM CustomerAccountInfo a WHERE a.customerId = :customerId")
  Optional<Long> getAccountAgeInMinutes(@Param("customerId") String customerId);
}
