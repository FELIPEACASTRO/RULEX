package com.rulex.config;

import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.boot.web.servlet.server.CookieSameSiteSupplier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.session.web.http.CookieSerializer;
import org.springframework.session.web.http.DefaultCookieSerializer;

/**
 * Configuração de Cookies Seguros para proteção contra ataques.
 *
 * <p>Implementa flags de segurança em cookies:
 * - Secure: Apenas transmitido via HTTPS
 * - HttpOnly: Não acessível via JavaScript
 * - SameSite: Proteção contra CSRF
 *
 * <p>FIX: VULN-002 (Cookie Without Secure Flag)
 *
 * @version 1.0.0
 */
@Configuration
public class SecureCookieConfig {

  /**
   * Configura serialização de cookies com flags de segurança.
   *
   * @return CookieSerializer configurado
   */
  @Bean
  public CookieSerializer cookieSerializer() {
    DefaultCookieSerializer serializer = new DefaultCookieSerializer();

    // Secure: Cookie apenas transmitido via HTTPS
    // Em produção, deve ser true
    // Em desenvolvimento (HTTP), pode ser false
    serializer.setUseSecureCookie(true);

    // HttpOnly: Cookie não acessível via JavaScript
    // Previne roubo de cookie via XSS
    serializer.setUseHttpOnlyCookie(true);

    // SameSite: Proteção contra CSRF
    // Strict: Cookie nunca enviado em cross-site requests
    // Lax: Cookie enviado apenas em navegação top-level
    // None: Cookie sempre enviado (requer Secure=true)
    serializer.setSameSite("Strict");

    // Nome do cookie de sessão
    serializer.setCookieName("RULEX_SESSION");

    // Path do cookie
    serializer.setCookiePath("/");

    // Max age: 30 minutos (1800 segundos)
    serializer.setCookieMaxAge(1800);

    return serializer;
  }

  /**
   * Configura SameSite para todos os cookies da aplicação.
   *
   * @return CookieSameSiteSupplier configurado
   */
  @Bean
  public CookieSameSiteSupplier applicationCookieSameSiteSupplier() {
    return CookieSameSiteSupplier.ofStrict();
  }

  /**
   * Utilitário para criar cookies seguros.
   *
   * @param name Nome do cookie
   * @param value Valor do cookie
   * @param maxAge Tempo de vida em segundos
   * @param secure Se deve ser transmitido apenas via HTTPS
   * @return Cookie configurado com flags de segurança
   */
  public static Cookie createSecureCookie(
      String name, String value, int maxAge, boolean secure) {
    Cookie cookie = new Cookie(name, value);
    cookie.setHttpOnly(true);
    cookie.setSecure(secure);
    cookie.setPath("/");
    cookie.setMaxAge(maxAge);
    // SameSite é configurado via header Set-Cookie
    return cookie;
  }

  /**
   * Adiciona cookie seguro à resposta HTTP.
   *
   * @param response HttpServletResponse
   * @param name Nome do cookie
   * @param value Valor do cookie
   * @param maxAge Tempo de vida em segundos
   * @param secure Se deve ser transmitido apenas via HTTPS
   */
  public static void addSecureCookie(
      HttpServletResponse response, String name, String value, int maxAge, boolean secure) {
    Cookie cookie = createSecureCookie(name, value, maxAge, secure);
    response.addCookie(cookie);

    // Adiciona SameSite via header (não suportado nativamente em Cookie)
    String cookieHeader =
        String.format(
            "%s=%s; Path=/; Max-Age=%d; HttpOnly; %s SameSite=Strict",
            name, value, maxAge, secure ? "Secure;" : "");
    response.addHeader("Set-Cookie", cookieHeader);
  }

  /**
   * Remove cookie de forma segura.
   *
   * @param response HttpServletResponse
   * @param name Nome do cookie
   */
  public static void removeCookie(HttpServletResponse response, String name) {
    Cookie cookie = new Cookie(name, null);
    cookie.setPath("/");
    cookie.setMaxAge(0);
    cookie.setHttpOnly(true);
    response.addCookie(cookie);
  }

  /**
   * Obtém valor de cookie de forma segura.
   *
   * @param request HttpServletRequest
   * @param name Nome do cookie
   * @return Valor do cookie ou null se não encontrado
   */
  public static String getCookieValue(HttpServletRequest request, String name) {
    Cookie[] cookies = request.getCookies();
    if (cookies != null) {
      for (Cookie cookie : cookies) {
        if (cookie.getName().equals(name)) {
          return cookie.getValue();
        }
      }
    }
    return null;
  }
}
