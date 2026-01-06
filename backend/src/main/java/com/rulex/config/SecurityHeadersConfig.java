package com.rulex.config;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;

/**
 * Configuração de Security Headers para proteção contra vulnerabilidades web.
 *
 * <p>Implementa headers recomendados pelo OWASP:
 * - X-Frame-Options: Proteção contra clickjacking
 * - X-Content-Type-Options: Proteção contra MIME sniffing
 * - X-XSS-Protection: Proteção contra XSS (legacy)
 * - Content-Security-Policy: Política de segurança de conteúdo
 * - Strict-Transport-Security: Força uso de HTTPS
 * - Referrer-Policy: Controle de informações de referência
 * - Permissions-Policy: Controle de features do browser
 *
 * <p>FIX: VULN-001 (Missing Security Headers)
 *
 * @version 1.0.0
 */
@Configuration
public class SecurityHeadersConfig {

  /**
   * Filtro para adicionar security headers em todas as respostas HTTP.
   *
   * @return Filter configurado com security headers
   */
  @Bean
  @Order(Ordered.HIGHEST_PRECEDENCE)
  public Filter securityHeadersFilter() {
    return new Filter() {
      @Override
      public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
          throws IOException, ServletException {

        HttpServletResponse httpResponse = (HttpServletResponse) response;

        // X-Frame-Options: Previne clickjacking
        // DENY: Não permite que a página seja exibida em frames
        httpResponse.setHeader("X-Frame-Options", "DENY");

        // X-Content-Type-Options: Previne MIME sniffing
        // nosniff: Browser não deve tentar detectar o tipo de conteúdo
        httpResponse.setHeader("X-Content-Type-Options", "nosniff");

        // X-XSS-Protection: Proteção XSS (legacy, mas ainda recomendado)
        // 1; mode=block: Ativa proteção XSS e bloqueia página se detectado
        httpResponse.setHeader("X-XSS-Protection", "1; mode=block");

        // Content-Security-Policy: Política de segurança de conteúdo
        // default-src 'self': Apenas recursos da mesma origem
        // script-src 'self' 'unsafe-inline': Scripts da mesma origem + inline (React)
        // style-src 'self' 'unsafe-inline': Estilos da mesma origem + inline
        // img-src 'self' data: https:: Imagens da mesma origem + data URIs + HTTPS
        // font-src 'self': Fontes da mesma origem
        // connect-src 'self': Conexões (AJAX, WebSocket) apenas mesma origem
        // frame-ancestors 'none': Não permite ser embedado em frames
        httpResponse.setHeader(
            "Content-Security-Policy",
            "default-src 'self'; "
                + "script-src 'self' 'unsafe-inline' 'unsafe-eval'; "
                + "style-src 'self' 'unsafe-inline'; "
                + "img-src 'self' data: https:; "
                + "font-src 'self'; "
                + "connect-src 'self'; "
                + "frame-ancestors 'none'; "
                + "base-uri 'self'; "
                + "form-action 'self'");

        // Strict-Transport-Security: Força uso de HTTPS
        // max-age=31536000: 1 ano
        // includeSubDomains: Aplica a subdomínios
        // preload: Permite inclusão na lista de preload do browser
        httpResponse.setHeader(
            "Strict-Transport-Security", "max-age=31536000; includeSubDomains; preload");

        // Referrer-Policy: Controle de informações de referência
        // strict-origin-when-cross-origin: Envia apenas origem em cross-origin
        httpResponse.setHeader("Referrer-Policy", "strict-origin-when-cross-origin");

        // Permissions-Policy: Controle de features do browser
        // Desabilita features desnecessárias para reduzir superfície de ataque
        httpResponse.setHeader(
            "Permissions-Policy",
            "geolocation=(), "
                + "microphone=(), "
                + "camera=(), "
                + "payment=(), "
                + "usb=(), "
                + "magnetometer=(), "
                + "gyroscope=(), "
                + "accelerometer=()");

        // Cache-Control: Previne cache de dados sensíveis
        // no-store: Não armazena em cache
        // no-cache: Revalida antes de usar cache
        // must-revalidate: Obriga revalidação
        // private: Apenas cache do browser, não de proxies
        if (httpResponse.getContentType() != null
            && httpResponse.getContentType().contains("application/json")) {
          httpResponse.setHeader("Cache-Control", "no-store, no-cache, must-revalidate, private");
          httpResponse.setHeader("Pragma", "no-cache");
          httpResponse.setHeader("Expires", "0");
        }

        chain.doFilter(request, response);
      }
    };
  }
}
