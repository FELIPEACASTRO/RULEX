package com.rulex.repository.homolog;

import com.rulex.entity.homolog.RuleEntity;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RuleRepository extends JpaRepository<RuleEntity, UUID> {
  Optional<RuleEntity> findByKey(String key);
}
