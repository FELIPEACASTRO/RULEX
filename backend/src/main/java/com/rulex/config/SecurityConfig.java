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

    http.csrf(csrf -> csrf.disable());

    if (!props.enabled()) {
      http.authorizeHttpRequests(auth -> auth.anyRequest().permitAll());
      return http.build();
    }

    http.authorizeHttpRequests(
            auth ->
                auth
                    // Public endpoints (kept open for demos / tests)
                    .requestMatchers(HttpMethod.POST, "/api/transactions/analyze").permitAll()
                    .requestMatchers(HttpMethod.POST, "/api/transactions/analyze-advanced")
                    .permitAll()
                    .requestMatchers(HttpMethod.POST, "/api/evaluate").permitAll()

                    // Read-only access for analysts
                    .requestMatchers(HttpMethod.GET, "/api/transactions/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/api/rules/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/api/audit/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/api/metrics/**")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.GET, "/api/field-dictionary/**")
                    .hasAnyRole("ANALYST", "ADMIN")

                    // V31 rules tools: validate/lint for analysts; simulate admin-only
                    .requestMatchers(HttpMethod.POST, "/api/rules/validate")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.POST, "/api/rules/lint")
                    .hasAnyRole("ANALYST", "ADMIN")
                    .requestMatchers(HttpMethod.POST, "/api/rules/simulate").hasRole("ADMIN")

                    // Mutations are admin-only
                    .requestMatchers("/api/homolog/**").hasRole("ADMIN")
                    .requestMatchers("/api/rules/**").hasRole("ADMIN")

                    .anyRequest().authenticated())
        .httpBasic(withDefaults());

    return http.build();
  }
}
