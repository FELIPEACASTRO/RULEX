package com.rulex.repository.homolog;

import com.rulex.entity.homolog.UserRoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface UserRoleRepository extends JpaRepository<UserRoleEntity, UserRoleEntity.Pk> {
}
