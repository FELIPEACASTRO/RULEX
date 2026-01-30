package com.rulex.config.security;

import com.rulex.entity.AuthenticationFailure;
import com.rulex.repository.AuthenticationFailureRepository;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.OffsetDateTime;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.LockedException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;
import org.springframework.stereotype.Component;

/**
 * SEC-005 FIX: Handler para registrar falhas de autenticação no banco de dados.
 *
 * <p>Registra tentativas de login falhadas para:
 * - Auditoria de segurança
 * - Detecção de ataques de força bruta
 * - Compliance (PCI-DSS, SOX)
 *
 * <p>Informações registradas:
 * - IP de origem
 * - User-Agent
 * - Tipo de falha (BAD_CREDENTIALS, LOCKED, etc.)
 * - Timestamp
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class AuditAuthenticationFailureHandler implements AuthenticationFailureHandler {

  private final AuthenticationFailureRepository authenticationFailureRepository;

  @Override
  public void onAuthenticationFailure(
      HttpServletRequest request,
      HttpServletResponse response,
      AuthenticationException exception)
      throws IOException {

    String username = extractUsername(request);
    String ipAddress = extractClientIp(request);
    String userAgent = request.getHeader("User-Agent");
    String failureType = determineFailureType(exception);
    String failureReason = exception.getMessage();

    // Log para monitoramento em tempo real
    log.warn(
        "SEC-005: Authentication failure - User: {}, IP: {}, Type: {}, Reason: {}",
        maskUsername(username),
        ipAddress,
        failureType,
        failureReason);

    // Persistir no banco para auditoria
    try {
      AuthenticationFailure failure =
          AuthenticationFailure.builder()
              .customerId(username)
              .failureType(failureType)
              .failureReason(truncate(failureReason, 100))
              .ipAddress(ipAddress)
              .userAgent(truncate(userAgent, 500))
              .failureTimestamp(OffsetDateTime.now())
              .build();

      authenticationFailureRepository.save(failure);
      log.debug("Authentication failure persisted for audit: {}", failure.getId());
    } catch (Exception e) {
      // Não falhar a resposta se o registro de auditoria falhar
      log.error("Failed to persist authentication failure audit: {}", e.getMessage());
    }

    // Retornar 401 Unauthorized
    response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
    response.setContentType("application/json");
    response.getWriter()
        .write(
            String.format(
                "{\"error\":\"Unauthorized\",\"message\":\"%s\",\"timestamp\":\"%s\"}",
                "Authentication failed", OffsetDateTime.now()));
  }

  private String extractUsername(HttpServletRequest request) {
    // Tentar extrair do header Authorization (Basic Auth)
    String authHeader = request.getHeader("Authorization");
    if (authHeader != null && authHeader.startsWith("Basic ")) {
      try {
        String base64Credentials = authHeader.substring(6);
        String credentials =
            new String(java.util.Base64.getDecoder().decode(base64Credentials));
        return credentials.split(":")[0];
      } catch (Exception e) {
        return "unknown";
      }
    }
    return request.getParameter("username") != null ? request.getParameter("username") : "unknown";
  }

  private String extractClientIp(HttpServletRequest request) {
    // Verificar headers de proxy reverso
    String[] headerNames = {
      "X-Forwarded-For", "X-Real-IP", "Proxy-Client-IP", "WL-Proxy-Client-IP"
    };

    for (String header : headerNames) {
      String ip = request.getHeader(header);
      if (ip != null && !ip.isEmpty() && !"unknown".equalsIgnoreCase(ip)) {
        // X-Forwarded-For pode conter múltiplos IPs, pegar o primeiro
        return ip.split(",")[0].trim();
      }
    }

    return request.getRemoteAddr();
  }

  private String determineFailureType(AuthenticationException exception) {
    if (exception instanceof BadCredentialsException) {
      return "BAD_CREDENTIALS";
    } else if (exception instanceof LockedException) {
      return "ACCOUNT_LOCKED";
    } else if (exception.getClass().getSimpleName().contains("Disabled")) {
      return "ACCOUNT_DISABLED";
    } else if (exception.getClass().getSimpleName().contains("Expired")) {
      return "CREDENTIALS_EXPIRED";
    }
    return "UNKNOWN";
  }

  private String maskUsername(String username) {
    if (username == null || username.length() <= 3) {
      return "***";
    }
    return username.substring(0, 2) + "***" + username.substring(username.length() - 1);
  }

  private String truncate(String value, int maxLength) {
    if (value == null) {
      return null;
    }
    return value.length() > maxLength ? value.substring(0, maxLength) : value;
  }
}
