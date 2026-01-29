package com.rulex.config;

import static org.springframework.security.config.Customizer.withDefaults;

import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.csrf.CookieCsrfTokenRepository;
import org.springframework.security.web.csrf.CsrfTokenRequestAttributeHandler;

@Configuration
@EnableWebSecurity
@EnableConfigurationProperties(RulexSecurityProperties.class)
@Slf4j
public class SecurityConfig {

  /** Senhas conhecidas como fracas/default que não devem ser usadas em produção */
  private static final Set<String> WEAK_PASSWORDS =
      Set.of(
          "admin",
          "password",
          "123456",
          "rulex",
          "rulex123",
          "changeme",
          "secret",
          "test",
          "demo",
          "default",
          "postgres",
          "neo4j");

  @Value("${spring.profiles.active:dev}")
  private String activeProfile;

  @Bean
  PasswordEncoder passwordEncoder() {
    return new BCryptPasswordEncoder();
  }

  @Bean
  UserDetailsService userDetailsService(RulexSecurityProperties props, PasswordEncoder encoder) {
    if (!props.enabled()) {
      return new InMemoryUserDetailsManager();
    }

    RulexSecurityProperties.User admin = props.admin();
    RulexSecurityProperties.User analyst = props.analyst();

    if (admin == null
        || admin.username() == null
        || admin.username().isBlank()
        || admin.password() == null
        || admin.password().isBlank()) {
      throw new IllegalStateException(
          "rulex.security.admin.username/password devem estar configurados quando rulex.security.enabled=true");
    }

    if (analyst == null
        || analyst.username() == null
        || analyst.username().isBlank()
        || analyst.password() == null
        || analyst.password().isBlank()) {
      throw new IllegalStateException(
          "rulex.security.analyst.username/password devem estar configurados quando rulex.security.enabled=true");
    }

    // Validar senhas fracas/default em produção
    validatePasswordStrength(admin.password(), "admin", activeProfile);
    validatePasswordStrength(analyst.password(), "analyst", activeProfile);

    return new InMemoryUserDetailsManager(
        User.withUsername(admin.username())
            .password(encoder.encode(admin.password()))
            .roles("ADMIN")
            .build(),
        User.withUsername(analyst.username())
            .password(encoder.encode(analyst.password()))
            .roles("ANALYST")
            .build());
  }

  @Bean
  SecurityFilterChain securityFilterChain(HttpSecurity http, RulexSecurityProperties props)
      throws Exception {

    if (!props.enabled()) {
      // When security is disabled, disable CSRF as well
      http.csrf(csrf -> csrf.disable());
      http.authorizeHttpRequests(auth -> auth.anyRequest().permitAll());
      return http.build();
    }

    // Configure CSRF with cookie-based token repository
    // The token is stored in a cookie (XSRF-TOKEN) and must be sent back
    // in the X-XSRF-TOKEN header for state-changing requests
    CookieCsrfTokenRepository csrfTokenRepository = CookieCsrfTokenRepository.withHttpOnlyFalse();
    CsrfTokenRequestAttributeHandler requestHandler = new CsrfTokenRequestAttributeHandler();
    // Set the name of the attribute to use for the CSRF token
    requestHandler.setCsrfRequestAttributeName("_csrf");

    http.csrf(
        csrf ->
            csrf.csrfTokenRepository(csrfTokenRepository)
                .csrfTokenRequestHandler(requestHandler)
                // Ignore CSRF for all API endpoints (they use Basic Auth which is stateless)
                // CSRF protection is primarily for browser-based sessions with cookies
                .ignoringRequestMatchers(
                    "/transactions/**",
                    "/evaluate/**",
                    "/rules/**",
                    "/complex-rules/**",
                    "/audit/**",
                    "/metrics/**",
                    "/field-dictionary/**",
                    "/homolog/**",
                    "/admin/**",
                    "/api/**",
                    "/actuator/**"));

    http.authorizeHttpRequests(
            auth ->
                auth
                    // Health probes (K8s / Container Apps / HML ops) - only health endpoints are
                    // public
                    .requestMatchers("/actuator/health/**")
                    .permitAll()

                    // Transaction analysis endpoints - require at least ANALYST role
                    .requestMatchers(HttpMethod.POST, "/transactions/analyze")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.POST, "/transactions/analyze-advanced")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.POST, "/evaluate")
                    .hasAnyRole("ANALYST", "ADMIN")

                    // Read-only access for analysts
                    .requestMatchers(HttpMethod.GET, "/transactions/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/rules/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/audit/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/metrics/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/field-dictionary/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/complex-rules/**")
                    .hasAnyRole("ANALYST", "ADMIN")

                    // V31 rules tools: validate/lint for analysts; simulate admin-only
                    .requestMatchers(HttpMethod.POST, "/rules/validate")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.POST, "/rules/lint")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.POST, "/rules/simulate")
                    .hasRole("ADMIN")

                    // Mutations are admin-only
                    .requestMatchers("/homolog/**")
                    .hasRole("ADMIN")
                    .requestMatchers("/rules/**")
                    .hasRole("ADMIN")
                    .requestMatchers("/complex-rules/**")
                    .hasRole("ADMIN")
                    // FASE-1: Endpoint de diagnóstico Redis - admin only
                    .requestMatchers("/admin/**")
                    .hasRole("ADMIN")
                    .anyRequest()
                    .authenticated())
        .httpBasic(withDefaults());

    return http.build();
  }

  /**
   * Valida força da senha. Em produção/staging/homolog, rejeita senhas fracas/default. Em dev/test,
   * apenas emite warning.
   *
   * <p>SEC-003 FIX: Expandido para incluir staging e homolog na validação estrita.
   */
  private void validatePasswordStrength(String password, String userType, String profile) {
    // SEC-003 FIX: Incluir staging, homolog, uat na validação estrita (não apenas prod)
    boolean isStrictEnvironment =
        Set.of("prod", "production", "staging", "homolog", "hml", "uat")
            .contains(profile.toLowerCase());
    boolean isDevelopment =
        Set.of("dev", "development", "test", "local").contains(profile.toLowerCase());

    // Verificar senhas conhecidas como fracas
    if (WEAK_PASSWORDS.contains(password.toLowerCase())) {
      String message =
          String.format(
              "⚠️ ALERTA DE SEGURANÇA: Senha fraca/default detectada para usuário '%s'. "
                  + "Senhas como '%s' são conhecidas e facilmente comprometidas.",
              userType, password);

      if (isStrictEnvironment) {
        throw new IllegalStateException(
            message + " Configure uma senha forte via variável de ambiente.");
      } else if (isDevelopment) {
        log.warn(message + " Isso é aceitável apenas em ambiente de desenvolvimento.");
      } else {
        // Profile desconhecido - tratar como produção por segurança
        log.error("Profile '{}' desconhecido. Tratando como ambiente de produção.", profile);
        throw new IllegalStateException(
            message + " Configure uma senha forte via variável de ambiente.");
      }
    }

    // Verificar comprimento mínimo em ambientes estritos
    if (isStrictEnvironment && password.length() < 12) {
      throw new IllegalStateException(
          String.format(
              "Senha do usuário '%s' deve ter no mínimo 12 caracteres em %s (atual: %d)",
              userType, profile, password.length()));
    }

    // Verificar complexidade em ambientes estritos
    if (isStrictEnvironment && !isStrongPassword(password)) {
      throw new IllegalStateException(
          String.format(
              "Senha do usuário '%s' deve conter letras maiúsculas, minúsculas, números e caracteres especiais em %s",
              userType, profile));
    }
  }

  /** Verifica se a senha atende requisitos de complexidade. */
  private boolean isStrongPassword(String password) {
    boolean hasUpper = password.chars().anyMatch(Character::isUpperCase);
    boolean hasLower = password.chars().anyMatch(Character::isLowerCase);
    boolean hasDigit = password.chars().anyMatch(Character::isDigit);
    boolean hasSpecial =
        password.chars().anyMatch(c -> "!@#$%^&*()_+-=[]{}|;':\",./<>?".indexOf(c) >= 0);
    return hasUpper && hasLower && hasDigit && hasSpecial;
  }
}
