package com.rulex.config.security;

import jakarta.annotation.PostConstruct;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.www.BasicAuthenticationEntryPoint;
import org.springframework.stereotype.Component;

/**
 * SEC-005 FIX: Entry point customizado para Basic Auth que registra falhas de autenticação.
 *
 * <p>Estende BasicAuthenticationEntryPoint para adicionar auditoria de tentativas falhadas.
 */
@Component
public class AuditBasicAuthenticationEntryPoint extends BasicAuthenticationEntryPoint {

  private final AuditAuthenticationFailureHandler auditFailureHandler;

  public AuditBasicAuthenticationEntryPoint(
      AuditAuthenticationFailureHandler auditFailureHandler) {
    this.auditFailureHandler = auditFailureHandler;
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
      auditFailureHandler.recordFailure(request, authException);
    }

    // Chamar comportamento padrão (retorna 401 com WWW-Authenticate header)
    super.commence(request, response, authException);
  }

}
