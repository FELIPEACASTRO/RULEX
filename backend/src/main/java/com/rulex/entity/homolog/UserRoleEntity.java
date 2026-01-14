package com.rulex.entity.homolog;

import jakarta.persistence.*;
import java.io.Serializable;
import java.util.UUID;
import lombok.*;

@Entity
@Table(name = "user_roles")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@IdClass(UserRoleEntity.Pk.class)
public class UserRoleEntity {

  @Id
  @Column(name = "user_id", nullable = false)
  private UUID userId;

  @Id
  @Column(name = "role_id", nullable = false)
  private UUID roleId;

  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  public static class Pk implements Serializable {
    private static final long serialVersionUID = 1L;
    private UUID userId;
    private UUID roleId;
  }
}
