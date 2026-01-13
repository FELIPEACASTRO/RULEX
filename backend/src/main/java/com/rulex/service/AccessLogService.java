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
   * DTO to capture request data before async execution.
   * This prevents "request object has been recycled" errors.
   */
  public record RequestData(
      String method,
      String requestUri,
      String sourceIp,
      String userAgent,
      String sessionId) {

    public static RequestData from(HttpServletRequest request) {
      return new RequestData(
          request.getMethod(),
          request.getRequestURI(),
          getClientIpStatic(request),
          request.getHeader("User-Agent"),
          request.getSession(false) != null ? request.getSession().getId() : null);
    }

    private static String getClientIpStatic(HttpServletRequest request) {
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
  }

  /**
   * Logs a successful login event.
   *
   * @param username the username
   * @param request the HTTP request
   */
  public void logLoginSuccess(String username, HttpServletRequest request) {
    RequestData requestData = RequestData.from(request);
    logLoginSuccessAsync(username, requestData);
  }

  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logLoginSuccessAsync(String username, RequestData requestData) {
    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.LOGIN_SUCCESS)
            .httpMethod(requestData.method())
            .requestUri(requestData.requestUri())
            .sourceIp(requestData.sourceIp())
            .userAgent(requestData.userAgent())
            .sessionId(requestData.sessionId())
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
  public void logLoginFailure(String username, HttpServletRequest request, String reason) {
    RequestData requestData = RequestData.from(request);
    logLoginFailureAsync(username, requestData, reason);
  }

  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logLoginFailureAsync(String username, RequestData requestData, String reason) {
    Map<String, Object> details = new HashMap<>();
    details.put("reason", reason);

    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.LOGIN_FAILURE)
            .httpMethod(requestData.method())
            .requestUri(requestData.requestUri())
            .sourceIp(requestData.sourceIp())
            .userAgent(requestData.userAgent())
            .details(toJson(details))
            .build();

    accessLogRepository.save(accessLog);
    log.warn("Login failure logged for user: {} from IP: {}", username, accessLog.getSourceIp());

    // Check for brute force
    checkBruteForce(username);
  }

  /**
   * Logs an API access event.
   * This method captures request data synchronously and delegates to async processing.
   *
   * @param username the authenticated username
   * @param request the HTTP request
   * @param responseStatus the HTTP response status
   * @param durationMs the request duration in milliseconds
   */
  public void logApiAccess(
      String username, HttpServletRequest request, int responseStatus, long durationMs) {
    // Capture request data synchronously before async execution
    RequestData requestData = RequestData.from(request);
    logApiAccessAsync(username, requestData, responseStatus, durationMs);
  }

  /**
   * Async implementation of API access logging.
   */
  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logApiAccessAsync(
      String username, RequestData requestData, int responseStatus, long durationMs) {
    AccessEventType eventType = determineEventType(responseStatus);

    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(eventType)
            .httpMethod(requestData.method())
            .requestUri(requestData.requestUri())
            .responseStatus(responseStatus)
            .sourceIp(requestData.sourceIp())
            .userAgent(requestData.userAgent())
            .sessionId(requestData.sessionId())
            .durationMs(durationMs)
            .build();

    accessLogRepository.save(accessLog);

    if (eventType != AccessEventType.API_ACCESS) {
      log.warn(
          "Security event {} for user: {} on {}", eventType, username, requestData.requestUri());
    }
  }

  /**
   * Logs a rate limiting event.
   *
   * @param username the username (may be null)
   * @param request the HTTP request
   */
  public void logRateLimited(String username, HttpServletRequest request) {
    RequestData requestData = RequestData.from(request);
    logRateLimitedAsync(username, requestData);
  }

  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logRateLimitedAsync(String username, RequestData requestData) {
    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.RATE_LIMITED)
            .httpMethod(requestData.method())
            .requestUri(requestData.requestUri())
            .responseStatus(429)
            .sourceIp(requestData.sourceIp())
            .userAgent(requestData.userAgent())
            .build();

    accessLogRepository.save(accessLog);
    log.warn(
        "Rate limit exceeded for user: {} from IP: {} on {}",
        username,
        accessLog.getSourceIp(),
        requestData.requestUri());
  }

  /**
   * Logs a logout event.
   *
   * @param username the username
   * @param request the HTTP request
   */
  public void logLogout(String username, HttpServletRequest request) {
    RequestData requestData = RequestData.from(request);
    logLogoutAsync(username, requestData);
  }

  @Async
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void logLogoutAsync(String username, RequestData requestData) {
    AccessLog accessLog =
        AccessLog.builder()
            .username(username)
            .eventType(AccessEventType.LOGOUT)
            .httpMethod(requestData.method())
            .requestUri(requestData.requestUri())
            .sourceIp(requestData.sourceIp())
            .userAgent(requestData.userAgent())
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
