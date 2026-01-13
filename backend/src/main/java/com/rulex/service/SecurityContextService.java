package com.rulex.service;

import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * Serviço para obter informações do contexto de segurança. Centraliza a lógica de identificação do
 * usuário para auditoria e compliance.
 */
@Service
@Slf4j
public class SecurityContextService {

  private static final String ANONYMOUS_USER = "anonymous";
  private static final String SYSTEM_USER = "SYSTEM";

  /** Obtém o nome do usuário autenticado atual. Retorna "anonymous" se não houver autenticação. */
  public String getCurrentUsername() {
    try {
      Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
      if (authentication != null && authentication.isAuthenticated()) {
        String name = authentication.getName();
        if (name != null && !name.isBlank() && !"anonymousUser".equals(name)) {
          return name;
        }
      }
    } catch (Exception e) {
      log.warn("Erro ao obter usuário do contexto de segurança: {}", e.getMessage());
    }
    return ANONYMOUS_USER;
  }

  /** Obtém o nome do usuário ou SYSTEM se for uma operação de sistema. */
  public String getCurrentUsernameOrSystem() {
    String username = getCurrentUsername();
    return ANONYMOUS_USER.equals(username) ? SYSTEM_USER : username;
  }

  /** Obtém o endereço IP do cliente da requisição atual. */
  public String getCurrentClientIp() {
    try {
      ServletRequestAttributes attrs =
          (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
      if (attrs != null) {
        HttpServletRequest request = attrs.getRequest();

        // Verificar headers de proxy reverso
        String xForwardedFor = request.getHeader("X-Forwarded-For");
        if (xForwardedFor != null && !xForwardedFor.isBlank()) {
          // Pegar o primeiro IP da lista (cliente original)
          return xForwardedFor.split(",")[0].trim();
        }

        String xRealIp = request.getHeader("X-Real-IP");
        if (xRealIp != null && !xRealIp.isBlank()) {
          return xRealIp;
        }

        return request.getRemoteAddr();
      }
    } catch (Exception e) {
      log.warn("Erro ao obter IP do cliente: {}", e.getMessage());
    }
    return "unknown";
  }

  /** Obtém o User-Agent do cliente da requisição atual. */
  public String getCurrentUserAgent() {
    try {
      ServletRequestAttributes attrs =
          (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
      if (attrs != null) {
        HttpServletRequest request = attrs.getRequest();
        String userAgent = request.getHeader("User-Agent");
        return userAgent != null ? userAgent : "unknown";
      }
    } catch (Exception e) {
      log.warn("Erro ao obter User-Agent: {}", e.getMessage());
    }
    return "unknown";
  }

  /** Verifica se o usuário atual tem uma role específica. */
  public boolean hasRole(String role) {
    try {
      Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
      if (authentication != null && authentication.isAuthenticated()) {
        return authentication.getAuthorities().stream()
            .anyMatch(
                auth ->
                    auth.getAuthority().equals("ROLE_" + role) || auth.getAuthority().equals(role));
      }
    } catch (Exception e) {
      log.warn("Erro ao verificar role: {}", e.getMessage());
    }
    return false;
  }

  /** Verifica se o usuário atual é admin. */
  public boolean isAdmin() {
    return hasRole("ADMIN");
  }

  /** Verifica se o usuário atual é analyst. */
  public boolean isAnalyst() {
    return hasRole("ANALYST");
  }
}
