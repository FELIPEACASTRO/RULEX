package com.rulex.repository;

import com.rulex.entity.AuthenticationFailure;
import java.time.OffsetDateTime;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface AuthenticationFailureRepository
    extends JpaRepository<AuthenticationFailure, UUID> {

  @Query(
      "SELECT COUNT(f) FROM AuthenticationFailure f "
          + "WHERE f.customerId = :customerId "
          + "AND f.failureTimestamp >= :since")
  long countFailuresSince(
      @Param("customerId") String customerId, @Param("since") OffsetDateTime since);

  @Query(
      "SELECT COUNT(f) FROM AuthenticationFailure f "
          + "WHERE f.cardNumberHash = :cardHash "
          + "AND f.failureTimestamp >= :since")
  long countFailuresByCardSince(
      @Param("cardHash") String cardHash, @Param("since") OffsetDateTime since);

  @Query(
      "SELECT COUNT(f) FROM AuthenticationFailure f "
          + "WHERE f.customerId = :customerId "
          + "AND f.failureType = :failureType "
          + "AND f.failureTimestamp >= :since")
  long countFailuresByTypeSince(
      @Param("customerId") String customerId,
      @Param("failureType") String failureType,
      @Param("since") OffsetDateTime since);

  @Query(
      "SELECT COUNT(f) FROM AuthenticationFailure f "
          + "WHERE f.ipAddress = :ipAddress "
          + "AND f.failureTimestamp >= :since")
  long countFailuresByIpSince(
      @Param("ipAddress") String ipAddress, @Param("since") OffsetDateTime since);
}
