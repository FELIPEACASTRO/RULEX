package com.rulex.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "rulex.security")
public record RulexSecurityProperties(boolean enabled, User admin, User analyst) {

  public record User(String username, String password) {}
}
