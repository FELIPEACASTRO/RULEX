package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.AccessLog;
import com.rulex.entity.AccessLog.AccessEventType;
import com.rulex.repository.AccessLogRepository;
import jakarta.servlet.http.HttpServletRequest;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 * Service for logging HTTP access and authentication events.
 *
 * <p>This service handles: - Login success/failure logging - API access logging - Security event
 * tracking - Brute force detection
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class AccessLogService {

  private final AccessLogRepository accessLogRepository;
  private final ObjectMapper objectMapper;

  /** Maximum failed login attempts before account lockout warning */
  private static final int MAX_FAILED_ATTEMPTS = 5;

  /** Time window for failed login attempts (in minutes) */
  private static final int LOCKOUT_WINDOW_MINUTES = 15;

  /**
   * Logs a successful login event.
   *
   * @param username the username
   * @param request the HTTP request
   */
  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logLoginSuccess(String username, HttpServletRequest request) {
    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.LOGIN_SUCCESS)
            .httpMethod(request.getMethod())
            .requestUri(request.getRequestURI())
            .sourceIp(getClientIp(request))
            .userAgent(request.getHeader("User-Agent"))
            .sessionId(request.getSession(false) != null ? request.getSession().getId() : null)
            .build();

    accessLogRepository.save(accessLog);
    log.info("Login success logged for user: {} from IP: {}", username, accessLog.getSourceIp());
  }

  /**
   * Logs a failed login event.
   *
   * @param username the attempted username
   * @param request the HTTP request
   * @param reason the reason for failure
   */
  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logLoginFailure(String username, HttpServletRequest request, String reason) {
    Map<String, Object> details = new HashMap<>();
    details.put("reason", reason);

    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.LOGIN_FAILURE)
            .httpMethod(request.getMethod())
            .requestUri(request.getRequestURI())
            .sourceIp(getClientIp(request))
            .userAgent(request.getHeader("User-Agent"))
            .details(toJson(details))
            .build();

    accessLogRepository.save(accessLog);
    log.warn("Login failure logged for user: {} from IP: {}", username, accessLog.getSourceIp());

    // Check for brute force
    checkBruteForce(username);
  }

  /**
   * Logs an API access event.
   *
   * @param username the authenticated username
   * @param request the HTTP request
   * @param responseStatus the HTTP response status
   * @param durationMs the request duration in milliseconds
   */
  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logApiAccess(
      String username, HttpServletRequest request, int responseStatus, long durationMs) {
    AccessEventType eventType = determineEventType(responseStatus);

    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(eventType)
            .httpMethod(request.getMethod())
            .requestUri(request.getRequestURI())
            .responseStatus(responseStatus)
            .sourceIp(getClientIp(request))
            .userAgent(request.getHeader("User-Agent"))
            .sessionId(request.getSession(false) != null ? request.getSession().getId() : null)
            .durationMs(durationMs)
            .build();

    accessLogRepository.save(accessLog);

    if (eventType != AccessEventType.API_ACCESS) {
      log.warn(
          "Security event {} for user: {} on {}", eventType, username, request.getRequestURI());
    }
  }

  /**
   * Logs a rate limiting event.
   *
   * @param username the username (may be null)
   * @param request the HTTP request
   */
  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logRateLimited(String username, HttpServletRequest request) {
    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.RATE_LIMITED)
            .httpMethod(request.getMethod())
            .requestUri(request.getRequestURI())
            .responseStatus(429)
            .sourceIp(getClientIp(request))
            .userAgent(request.getHeader("User-Agent"))
            .build();

    accessLogRepository.save(accessLog);
    log.warn(
        "Rate limit exceeded for user: {} from IP: {} on {}",
        username,
        accessLog.getSourceIp(),
        request.getRequestURI());
  }

  /**
   * Logs a logout event.
   *
   * @param username the username
   * @param request the HTTP request
   */
  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logLogout(String username, HttpServletRequest request) {
    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.LOGOUT)
            .httpMethod(request.getMethod())
            .requestUri(request.getRequestURI())
            .sourceIp(getClientIp(request))
            .userAgent(request.getHeader("User-Agent"))
            .build();

    accessLogRepository.save(accessLog);
    log.info("Logout logged for user: {}", username);
  }

  /**
   * Gets access logs with pagination.
   *
   * @param pageable pagination parameters
   * @return page of access logs
   */
  @Transactional(readOnly = true)
  public Page<AccessLog> getAccessLogs(Pageable pageable) {
    return accessLogRepository.findAll(pageable);
  }

  /**
   * Gets access logs for a specific user.
   *
   * @param username the username
   * @param pageable pagination parameters
   * @return page of access logs
   */
  @Transactional(readOnly = true)
  public Page<AccessLog> getAccessLogsByUsername(String username, Pageable pageable) {
    return accessLogRepository.findByUsername(username, pageable);
  }

  /**
   * Gets access logs by event type.
   *
   * @param eventType the event type
   * @param pageable pagination parameters
   * @return page of access logs
   */
  @Transactional(readOnly = true)
  public Page<AccessLog> getAccessLogsByEventType(AccessEventType eventType, Pageable pageable) {
    return accessLogRepository.findByEventType(eventType, pageable);
  }

  /**
   * Checks for brute force attempts and logs warning if threshold exceeded.
   *
   * @param username the username to check
   */
  private void checkBruteForce(String username) {
    LocalDateTime windowStart = LocalDateTime.now().minusMinutes(LOCKOUT_WINDOW_MINUTES);
    long failedAttempts = accessLogRepository.countFailedLoginAttempts(username, windowStart);

    if (failedAttempts >= MAX_FAILED_ATTEMPTS) {
      log.error(
          "SECURITY ALERT: Possible brute force attack detected for user: {}. "
              + "{} failed attempts in the last {} minutes",
          username,
          failedAttempts,
          LOCKOUT_WINDOW_MINUTES);
    }
  }

  /** Determines the event type based on HTTP response status. */
  private AccessEventType determineEventType(int status) {
    if (status == 401) {
      return AccessEventType.UNAUTHORIZED_ACCESS;
    } else if (status == 403) {
      return AccessEventType.FORBIDDEN_ACCESS;
    } else if (status == 429) {
      return AccessEventType.RATE_LIMITED;
    }
    return AccessEventType.API_ACCESS;
  }

  /** Extracts the real client IP considering proxies. */
  private String getClientIp(HttpServletRequest request) {
    String xForwardedFor = request.getHeader("X-Forwarded-For");
    if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
      return xForwardedFor.split(",")[0].trim();
    }
    String xRealIp = request.getHeader("X-Real-IP");
    if (xRealIp != null && !xRealIp.isEmpty()) {
      return xRealIp;
    }
    return request.getRemoteAddr();
  }

  /** Converts an object to JSON string. */
  private String toJson(Object obj) {
    try {
      return objectMapper.writeValueAsString(obj);
    } catch (Exception e) {
      log.error("Failed to serialize object to JSON", e);
      return "{}";
    }
  }
}
