package com.rulex.repository;

import com.rulex.entity.AccessLog;
import com.rulex.entity.AccessLog.AccessEventType;
import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/** Repository for access log entries. */
@Repository
public interface AccessLogRepository extends JpaRepository<AccessLog, Long> {

  /** Find access logs by username */
  Page<AccessLog> findByUsername(String username, Pageable pageable);

  /** Find access logs by event type */
  Page<AccessLog> findByEventType(AccessEventType eventType, Pageable pageable);

  /** Find access logs by source IP */
  Page<AccessLog> findBySourceIp(String sourceIp, Pageable pageable);

  /** Find access logs within a time range */
  Page<AccessLog> findByTimestampBetween(LocalDateTime start, LocalDateTime end, Pageable pageable);

  /** Find failed login attempts for a username within a time window */
  @Query(
      "SELECT a FROM AccessLog a WHERE a.username = :username "
          + "AND a.eventType = 'LOGIN_FAILURE' "
          + "AND a.timestamp >= :since "
          + "ORDER BY a.timestamp DESC")
  List<AccessLog> findFailedLoginAttempts(
      @Param("username") String username, @Param("since") LocalDateTime since);

  /** Count failed login attempts for a username within a time window */
  @Query(
      "SELECT COUNT(a) FROM AccessLog a WHERE a.username = :username "
          + "AND a.eventType = 'LOGIN_FAILURE' "
          + "AND a.timestamp >= :since")
  long countFailedLoginAttempts(
      @Param("username") String username, @Param("since") LocalDateTime since);

  /** Find access logs by username and event type */
  Page<AccessLog> findByUsernameAndEventType(
      String username, AccessEventType eventType, Pageable pageable);

  /** Find recent access logs for security monitoring */
  @Query(
      "SELECT a FROM AccessLog a WHERE a.eventType IN :eventTypes "
          + "AND a.timestamp >= :since "
          + "ORDER BY a.timestamp DESC")
  List<AccessLog> findRecentSecurityEvents(
      @Param("eventTypes") List<AccessEventType> eventTypes, @Param("since") LocalDateTime since);

  /** Delete old access logs for retention policy */
  @Query("DELETE FROM AccessLog a WHERE a.timestamp < :before")
  void deleteOldLogs(@Param("before") LocalDateTime before);

  /** Get statistics for access events by type */
  @Query(
      "SELECT a.eventType, COUNT(a) FROM AccessLog a "
          + "WHERE a.timestamp >= :since "
          + "GROUP BY a.eventType")
  List<Object[]> getEventStatistics(@Param("since") LocalDateTime since);
}
