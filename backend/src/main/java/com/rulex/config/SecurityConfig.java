package com.rulex.config;

import static org.springframework.security.config.Customizer.withDefaults;

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
public class SecurityConfig {

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

    http.csrf(csrf -> csrf
        .csrfTokenRepository(csrfTokenRepository)
        .csrfTokenRequestHandler(requestHandler)
        // Ignore CSRF for public analyze endpoints (stateless transaction analysis)
        .ignoringRequestMatchers(
            "/transactions/analyze",
            "/transactions/analyze-advanced",
            "/evaluate",
            "/actuator/**"
        )
    );

    http.authorizeHttpRequests(
            auth ->
                auth
                    // Public endpoints (kept open for demos / tests)
                    .requestMatchers(HttpMethod.POST, "/transactions/analyze")
                    .permitAll()
                    .requestMatchers(HttpMethod.POST, "/transactions/analyze-advanced")
                    .permitAll()
                    .requestMatchers(HttpMethod.POST, "/evaluate")
                    .permitAll()

                    // Health probes (K8s / Container Apps / HML ops)
                    .requestMatchers("/actuator/health/**")
                    .permitAll()

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
                    .anyRequest()
                    .authenticated())
        .httpBasic(withDefaults());

    return http.build();
  }
}
