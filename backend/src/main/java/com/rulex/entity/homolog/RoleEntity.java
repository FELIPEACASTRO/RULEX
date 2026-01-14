package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.time.OffsetDateTime;
import java.util.UUID;
import lombok.*;
import org.hibernate.annotations.UuidGenerator;

@Entity
@Table(name = "roles")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RoleEntity {

  @Id @UuidGenerator @GeneratedValue private UUID id;

  @Column(nullable = false, unique = true)
  private String name;

  @Column(nullable = false)
  private OffsetDateTime createdAt;

  @PrePersist
  void onCreate() {
    if (createdAt == null) {
      createdAt = OffsetDateTime.now();
    }
  }
}
