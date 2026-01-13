package com.rulex.config;

import com.rulex.service.AccessLogService;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
@RequiredArgsConstructor
@Slf4j
public class AccessLogFilter extends OncePerRequestFilter {

  private final AccessLogService accessLogService;

  /** Paths to exclude from access logging */
  private static final Set<String> EXCLUDED_PATHS =
      Set.of(
          "/actuator/health",
          "/actuator/prometheus",
          "/actuator/info",
          "/health",
          "/favicon.ico",
          "/static",
          "/assets");

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
    String path = request.getRequestURI();

    // Skip actuator and health endpoints
    for (String excluded : EXCLUDED_PATHS) {
      if (path.startsWith(excluded)) {
        return true;
      }
    }

    // Skip OPTIONS requests (CORS preflight)
    if ("OPTIONS".equalsIgnoreCase(request.getMethod())) {
      return true;
    }

    return false;
  }

  /** Logs the access event. */
  private void logAccess(HttpServletRequest request, HttpServletResponse response, long duration) {
    try {
      String username = getUsername();
      int status = response.getStatus();

      // Log based on response status
      if (status == 401 || status == 403 || status == 429 || status >= 500) {
        // Log security and error events
        accessLogService.logApiAccess(username, request, status, duration);
      } else if (isModifyingRequest(request)) {
        // Log all modifying requests (POST, PUT, DELETE, PATCH)
        accessLogService.logApiAccess(username, request, status, duration);
      } else if (log.isDebugEnabled()) {
        // Log all requests in debug mode
        accessLogService.logApiAccess(username, request, status, duration);
      }
    } catch (Exception e) {
      log.error("Failed to log access event", e);
    }
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
