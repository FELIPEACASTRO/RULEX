package com.rulex.config;

import com.rulex.service.AccessLogService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * Filter that logs all HTTP access for auditing purposes.
 *
 * <p>This filter captures: - Request method, URI, and timing - Authenticated username - Response
 * status - Client IP and User-Agent
 *
 * <p>Runs at low priority to capture the final response status.
 */
@Component
@Order(Ordered.LOWEST_PRECEDENCE - 10)
@Slf4j
@ConditionalOnProperty(
    name = "rulex.access-log.enabled",
    havingValue = "true",
    matchIfMissing = true)
public class AccessLogFilter extends OncePerRequestFilter {

  private final AccessLogService accessLogService;
  private final double successSamplingRate;

  public AccessLogFilter(
      AccessLogService accessLogService,
      @Value("${rulex.access-log.success-sampling-rate:0.1}") double successSamplingRate) {
    this.accessLogService = accessLogService;
    this.successSamplingRate = Math.max(0.0, Math.min(1.0, successSamplingRate));
    log.info(
        "AccessLogFilter initialized with success sampling rate: {}", this.successSamplingRate);
  }

  /** Paths to exclude from access logging (without context-path prefix) */
  private static final Set<String> EXCLUDED_PATHS =
      Set.of(
          "/actuator/health",
          "/actuator/prometheus",
          "/actuator/info",
          "/actuator/metrics",
          "/actuator",
          "/health",
          "/favicon.ico",
          "/static",
          "/assets");

  /** Paths to exclude WITH context-path prefix (for when context-path=/api) */
  private static final Set<String> EXCLUDED_PATHS_WITH_CONTEXT =
      Set.of(
          "/api/actuator/health",
          "/api/actuator/prometheus",
          "/api/actuator/info",
          "/api/actuator/metrics",
          "/api/actuator",
          "/api/health");

  @Override
  protected void doFilterInternal(
      HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
      throws ServletException, IOException {

    // Skip excluded paths
    if (shouldSkip(request)) {
      filterChain.doFilter(request, response);
      return;
    }

    long startTime = System.currentTimeMillis();

    try {
      filterChain.doFilter(request, response);
    } finally {
      long duration = System.currentTimeMillis() - startTime;
      logAccess(request, response, duration);
    }
  }

  /** Determines if the request should be logged. */
  private boolean shouldSkip(HttpServletRequest request) {
    String requestUri = request.getRequestURI();
    String servletPath = request.getServletPath();

    // Use servletPath if available (excludes context-path), otherwise use requestURI
    String pathToCheck = (servletPath != null && !servletPath.isEmpty()) ? servletPath : requestUri;

    // Skip actuator and health endpoints (check both with and without context-path)
    for (String excluded : EXCLUDED_PATHS) {
      if (pathToCheck.startsWith(excluded)) {
        return true;
      }
    }

    // Also check full requestURI against paths with context-path prefix
    for (String excluded : EXCLUDED_PATHS_WITH_CONTEXT) {
      if (requestUri.startsWith(excluded)) {
        return true;
      }
    }

    // Skip OPTIONS requests (CORS preflight)
    if ("OPTIONS".equalsIgnoreCase(request.getMethod())) {
      return true;
    }

    return false;
  }

  /** Logs the access event with sampling for success responses. */
  private void logAccess(HttpServletRequest request, HttpServletResponse response, long duration) {
    try {
      String username = getUsername();
      int status = response.getStatus();

      // SEMPRE logar: erros de segurança, erros de servidor, rate limiting
      if (status == 401 || status == 403 || status == 429 || status >= 500) {
        accessLogService.logApiAccess(username, request, status, duration);
        return;
      }

      // SEMPRE logar: requisições que modificam dados
      if (isModifyingRequest(request)) {
        accessLogService.logApiAccess(username, request, status, duration);
        return;
      }

      // SEMPRE logar: erros do cliente (4xx)
      if (status >= 400) {
        accessLogService.logApiAccess(username, request, status, duration);
        return;
      }

      // SAMPLING: para requisições de sucesso (2xx/3xx), aplicar taxa de amostragem
      if (shouldSample()) {
        accessLogService.logApiAccess(username, request, status, duration);
      }
    } catch (Exception e) {
      log.error("Failed to log access event", e);
    }
  }

  /** Determina se deve amostrar baseado na taxa configurada. */
  private boolean shouldSample() {
    if (successSamplingRate >= 1.0) {
      return true;
    }
    if (successSamplingRate <= 0.0) {
      return false;
    }
    return ThreadLocalRandom.current().nextDouble() < successSamplingRate;
  }

  /** Gets the authenticated username, or "anonymous" if not authenticated. */
  private String getUsername() {
    Authentication auth = SecurityContextHolder.getContext().getAuthentication();
    if (auth != null && auth.isAuthenticated() && !"anonymousUser".equals(auth.getPrincipal())) {
      return auth.getName();
    }
    return "anonymous";
  }

  /** Checks if the request is a modifying request. */
  private boolean isModifyingRequest(HttpServletRequest request) {
    String method = request.getMethod();
    return "POST".equalsIgnoreCase(method)
        || "PUT".equalsIgnoreCase(method)
        || "DELETE".equalsIgnoreCase(method)
        || "PATCH".equalsIgnoreCase(method);
  }
}
