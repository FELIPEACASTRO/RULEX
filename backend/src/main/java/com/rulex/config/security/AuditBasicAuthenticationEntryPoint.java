package com.rulex.config.security;

import com.rulex.entity.AuthenticationFailure;
import com.rulex.repository.AuthenticationFailureRepository;
import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.OffsetDateTime;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint;
import org.springframework.stereotype.Component;

/**
 * SEC-005 FIX: Entry point customizado para Basic Auth que registra falhas de autenticação.
 *
 * <p>Estende BasicAuthenticationEntryPoint para adicionar auditoria de tentativas falhadas.
 */
@Component
@Slf4j
public class AuditBasicAuthenticationEntryPoint extends BasicAuthenticationEntryPoint {

  private final AuthenticationFailureRepository authenticationFailureRepository;

  public AuditBasicAuthenticationEntryPoint(
      AuthenticationFailureRepository authenticationFailureRepository) {
    this.authenticationFailureRepository = authenticationFailureRepository;
  }

  @PostConstruct
  public void init() {
    setRealmName("RULEX Fraud Detection API");
  }

  @Override
  public void commence(
      HttpServletRequest request,
      HttpServletResponse response,
      AuthenticationException authException)
      throws IOException {

    // Verificar se é uma tentativa de autenticação falhada (tem header Authorization)
    String authHeader = request.getHeader("Authorization");
    if (authHeader != null && authHeader.startsWith("Basic ")) {
      recordAuthenticationFailure(request, authException);
    }

    // Chamar comportamento padrão (retorna 401 com WWW-Authenticate header)
    super.commence(request, response, authException);
  }

  private void recordAuthenticationFailure(
      HttpServletRequest request, AuthenticationException exception) {
    String username = extractUsername(request);
    String ipAddress = extractClientIp(request);
    String userAgent = request.getHeader("User-Agent");

    log.warn(
        "SEC-005: Basic Auth failure - User: {}, IP: {}, Path: {}, Reason: {}",
        maskUsername(username),
        ipAddress,
        request.getRequestURI(),
        exception.getMessage());

    try {
      AuthenticationFailure failure =
          AuthenticationFailure.builder()
              .customerId(username)
              .failureType("BASIC_AUTH_FAILURE")
              .failureReason(truncate(exception.getMessage(), 100))
              .ipAddress(ipAddress)
              .userAgent(truncate(userAgent, 500))
              .failureTimestamp(OffsetDateTime.now())
              .build();

      authenticationFailureRepository.save(failure);
    } catch (Exception e) {
      log.error("Failed to persist auth failure audit: {}", e.getMessage());
    }
  }

  private String extractUsername(HttpServletRequest request) {
    String authHeader = request.getHeader("Authorization");
    if (authHeader != null && authHeader.startsWith("Basic ")) {
      try {
        String base64Credentials = authHeader.substring(6);
        String credentials =
            new String(java.util.Base64.getDecoder().decode(base64Credentials));
        return credentials.split(":")[0];
      } catch (Exception e) {
        return "invalid_encoding";
      }
    }
    return "no_credentials";
  }

  private String extractClientIp(HttpServletRequest request) {
    String[] headers = {"X-Forwarded-For", "X-Real-IP", "Proxy-Client-IP"};
    for (String header : headers) {
      String ip = request.getHeader(header);
      if (ip != null && !ip.isEmpty() && !"unknown".equalsIgnoreCase(ip)) {
        return ip.split(",")[0].trim();
      }
    }
    return request.getRemoteAddr();
  }

  private String maskUsername(String username) {
    if (username == null || username.length() <= 3) {
      return "***";
    }
    return username.substring(0, 2) + "***" + username.substring(username.length() - 1);
  }

  private String truncate(String value, int maxLength) {
    if (value == null) return null;
    return value.length() > maxLength ? value.substring(0, maxLength) : value;
  }
}
