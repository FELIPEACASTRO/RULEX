package com.rulex.entity;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

@Entity
@Table(name = "authentication_failures")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuthenticationFailure {

  @Id
  @GeneratedValue(strategy = GenerationType.UUID)
  private UUID id;

  @Column(name = "customer_id", length = 100)
  private String customerId;

  @Column(name = "account_number", length = 50)
  private String accountNumber;

  @Column(name = "card_number_hash", length = 64)
  private String cardNumberHash;

  @Column(name = "failure_type", nullable = false, length = 50)
  private String failureType;

  @Column(name = "failure_reason", length = 100)
  private String failureReason;

  @Column(name = "ip_address", length = 45)
  private String ipAddress;

  @Column(name = "device_id")
  private String deviceId;

  @Column(name = "user_agent", columnDefinition = "TEXT")
  private String userAgent;

  @Column(name = "failure_timestamp", nullable = false)
  @Builder.Default
  private OffsetDateTime failureTimestamp = OffsetDateTime.now();

  @CreationTimestamp
  @Column(name = "created_at", nullable = false, updatable = false)
  private OffsetDateTime createdAt;
}
