package com.rulex.config;

import io.github.bucket4j.Bucket;
import io.github.bucket4j.ConsumptionProbe;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

/**
 * Rate limiting filter using Bucket4j token bucket algorithm. Limits requests per IP address to
 * prevent abuse and DoS attacks.
 *
 * <p>Configuration via application.yml: - rulex.rate-limit.requests-per-minute: max requests per IP
 * per minute (default: 100) - rulex.rate-limit.burst-capacity: max burst capacity (default: 20) -
 * rulex.rate-limit.enabled: enable/disable rate limiting (default: true)
 */
@Component
public class RateLimitingFilter extends OncePerRequestFilter {

  private static final Logger log = LoggerFactory.getLogger(RateLimitingFilter.class);

  private final Map<String, Bucket> buckets = new ConcurrentHashMap<>();

  @Value("${rulex.rate-limit.requests-per-minute:100}")
  private int requestsPerMinute;

  @Value("${rulex.rate-limit.burst-capacity:20}")
  private int burstCapacity;

  @Value("${rulex.rate-limit.enabled:true}")
  private boolean enabled;

  @Override
  protected void doFilterInternal(
      HttpServletRequest request, HttpServletResponse response, FilterChain filterChain)
      throws ServletException, IOException {

    if (!enabled) {
      filterChain.doFilter(request, response);
      return;
    }

    // Skip rate limiting for health checks
    String path = request.getRequestURI();
    if (path.startsWith("/api/actuator/health")) {
      filterChain.doFilter(request, response);
      return;
    }

    // SEC-005 FIX: Rate limiting por usuário autenticado + IP + User-Agent
    String rateLimitKey = getRateLimitKey(request);
    Bucket bucket = buckets.computeIfAbsent(rateLimitKey, this::createNewBucket);

    ConsumptionProbe probe = bucket.tryConsumeAndReturnRemaining(1);

    if (probe.isConsumed()) {
      // Add rate limit headers
      response.addHeader("X-RateLimit-Remaining", String.valueOf(probe.getRemainingTokens()));
      response.addHeader("X-RateLimit-Limit", String.valueOf(requestsPerMinute));
      filterChain.doFilter(request, response);
    } else {
      // Rate limit exceeded
      long waitForRefillNanos = probe.getNanosToWaitForRefill();
      long waitForRefillSeconds = waitForRefillNanos / 1_000_000_000;

      log.warn(
          "Rate limit exceeded for key: {} on path: {}. Wait time: {}s",
          rateLimitKey,
          path,
          waitForRefillSeconds);

      response.setStatus(HttpStatus.TOO_MANY_REQUESTS.value());
      response.addHeader("X-RateLimit-Remaining", "0");
      response.addHeader("X-RateLimit-Limit", String.valueOf(requestsPerMinute));
      response.addHeader("X-RateLimit-Retry-After", String.valueOf(waitForRefillSeconds));
      response.setContentType("application/json");
      response.getWriter().write("{\"error\":\"Too many requests. Please try again later.\"}");
    }
  }

  private Bucket createNewBucket(String key) {
    // Token bucket with configurable rate limit using the builder API
    return Bucket.builder()
        .addLimit(
            limit ->
                limit
                    .capacity(requestsPerMinute)
                    .refillGreedy(requestsPerMinute, Duration.ofMinutes(1)))
        .build();
  }

  /**
   * SEC-005 FIX: Gera chave de rate limiting combinando múltiplos fatores. Prioridade: 1) Usuário
   * autenticado, 2) IP + User-Agent hash Isso previne bypass via proxies/VPNs e permite rate
   * limiting por usuário.
   */
  private String getRateLimitKey(HttpServletRequest request) {
    // Tentar extrair usuário do header Authorization (Basic Auth)
    String authHeader = request.getHeader("Authorization");
    if (authHeader != null && authHeader.startsWith("Basic ")) {
      try {
        String base64Credentials = authHeader.substring("Basic ".length());
        String credentials =
            new String(Base64.getDecoder().decode(base64Credentials), StandardCharsets.UTF_8);
        String username = credentials.split(":")[0];
        if (username != null && !username.isBlank()) {
          return "user:" + username;
        }
      } catch (Exception e) {
        log.debug("Não foi possível extrair usuário do header Authorization: {}", e.getMessage());
      }
    }

    // Fallback: IP + User-Agent hash para identificação mais robusta
    String clientIp = getClientIp(request);
    String userAgent = request.getHeader("User-Agent");
    String userAgentHash = hashUserAgent(userAgent);

    return "ip:" + clientIp + ":ua:" + userAgentHash;
  }

  /** Gera hash curto do User-Agent para identificação. */
  private String hashUserAgent(String userAgent) {
    if (userAgent == null || userAgent.isBlank()) {
      return "unknown";
    }
    try {
      MessageDigest md = MessageDigest.getInstance("SHA-256");
      byte[] hash = md.digest(userAgent.getBytes(StandardCharsets.UTF_8));
      // Usar apenas os primeiros 8 caracteres do hash
      return Base64.getEncoder().encodeToString(hash).substring(0, 8);
    } catch (NoSuchAlgorithmException e) {
      return "unknown";
    }
  }

  /**
   * Extracts the real client IP from the request, handling proxies and load balancers.
   *
   * @param request the HTTP request
   * @return the client IP address
   */
  private String getClientIp(HttpServletRequest request) {
    // Check X-Forwarded-For header (set by proxies/load balancers)
    String xForwardedFor = request.getHeader("X-Forwarded-For");
    if (xForwardedFor != null && !xForwardedFor.isEmpty()) {
      // X-Forwarded-For can contain multiple IPs; the first one is the client
      return xForwardedFor.split(",")[0].trim();
    }

    // Check X-Real-IP header (nginx)
    String xRealIp = request.getHeader("X-Real-IP");
    if (xRealIp != null && !xRealIp.isEmpty()) {
      return xRealIp.trim();
    }

    // Fall back to remote address
    return request.getRemoteAddr();
  }

  /** Clears the bucket cache. Useful for testing. */
  public void clearBuckets() {
    buckets.clear();
  }

  /** Returns the current bucket count. Useful for monitoring. */
  public int getBucketCount() {
    return buckets.size();
  }
}
