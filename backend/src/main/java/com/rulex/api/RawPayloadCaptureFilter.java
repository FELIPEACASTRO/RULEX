package com.rulex.api;

import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

/** Captures the raw HTTP request body bytes for critical endpoints ("as received"). */
@Component
@Order(Ordered.HIGHEST_PRECEDENCE + 10)
public class RawPayloadCaptureFilter extends OncePerRequestFilter {

  public static final String RAW_BYTES_ATTR = "RAW_PAYLOAD_BYTES";

  private final boolean enabled;

  public RawPayloadCaptureFilter(
      @Value("${rulex.rawPayloadCapture.enabled:true}") boolean enabled) {
    this.enabled = enabled;
  }

  @Override
  protected boolean shouldNotFilter(@org.springframework.lang.NonNull HttpServletRequest request) {
    if (!enabled) {
      return true;
    }
    if (!"POST".equalsIgnoreCase(request.getMethod())) {
      return true;
    }

    String path = request.getRequestURI();
    // NOTE: endpoints are mounted without /api in controllers; the reverse proxy may add /api.
    return !(path.endsWith("/evaluate")
        || path.contains("/evaluate/")
        || path.endsWith("/transactions/analyze")
        || path.contains("/transactions/analyze")
        || path.endsWith("/transactions/analyze-advanced")
        || path.contains("/transactions/analyze-advanced"));
  }

  @Override
  protected void doFilterInternal(
      @org.springframework.lang.NonNull HttpServletRequest request,
      @org.springframework.lang.NonNull HttpServletResponse response,
      @org.springframework.lang.NonNull FilterChain filterChain)
      throws ServletException, IOException {

    CachedBodyHttpServletRequest cached = new CachedBodyHttpServletRequest(request);
    request.setAttribute(RAW_BYTES_ATTR, cached.getCachedBody());
    filterChain.doFilter(cached, response);
  }
}
