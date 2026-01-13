package com.rulex.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entity for logging HTTP access/authentication events. Separate from AuditLog to handle
 * high-volume access logging with different retention policies.
 */
@Entity
@Table(
    name = "access_logs",
    indexes = {
      @Index(name = "idx_access_username", columnList = "username"),
      @Index(name = "idx_access_event_type", columnList = "event_type"),
      @Index(name = "idx_access_timestamp", columnList = "timestamp"),
      @Index(name = "idx_access_source_ip", columnList = "source_ip")
    })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AccessLog {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  /** Username attempting access (may be null for unauthenticated requests) */
  @Column(length = 100)
  private String username;

  /** Type of access event */
  @Column(name = "event_type", nullable = false, length = 50)
  @Enumerated(EnumType.STRING)
  private AccessEventType eventType;

  /** HTTP method (GET, POST, PUT, DELETE, etc.) */
  @Column(length = 10)
  private String httpMethod;

  /** Request URI */
  @Column(length = 500)
  private String requestUri;

  /** HTTP response status code */
  @Column private Integer responseStatus;

  /** Source IP address */
  @Column(name = "source_ip", length = 45)
  private String sourceIp;

  /** User-Agent header */
  @Column(name = "user_agent", length = 500)
  private String userAgent;

  /** Session ID (if applicable) */
  @Column(name = "session_id", length = 100)
  private String sessionId;

  /** Additional details in JSON format */
  @Column(columnDefinition = "TEXT")
  private String details;

  /** Request duration in milliseconds */
  @Column(name = "duration_ms")
  private Long durationMs;

  /** Timestamp of the access event */
  @Column(nullable = false, updatable = false)
  private LocalDateTime timestamp;

  @PrePersist
  protected void onCreate() {
    if (timestamp == null) {
      timestamp = LocalDateTime.now();
    }
  }

  /** Types of access events */
  public enum AccessEventType {
    /** Successful login */
    LOGIN_SUCCESS("Login bem-sucedido"),

    /** Failed login attempt */
    LOGIN_FAILURE("Falha de login"),

    /** User logout */
    LOGOUT("Logout"),

    /** Session expired */
    SESSION_EXPIRED("Sessão expirada"),

    /** Successful API access */
    API_ACCESS("Acesso à API"),

    /** Unauthorized access attempt (401) */
    UNAUTHORIZED_ACCESS("Acesso não autorizado"),

    /** Forbidden access attempt (403) */
    FORBIDDEN_ACCESS("Acesso proibido"),

    /** Rate limit exceeded */
    RATE_LIMITED("Limite de taxa excedido"),

    /** Invalid token */
    INVALID_TOKEN("Token inválido"),

    /** Password change */
    PASSWORD_CHANGE("Alteração de senha"),

    /** Account locked */
    ACCOUNT_LOCKED("Conta bloqueada");

    private final String label;

    AccessEventType(String label) {
      this.label = label;
    }

    public String getLabel() {
      return label;
    }
  }
}
