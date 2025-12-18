package com.rulex.repository.homolog;

import com.rulex.entity.homolog.RuleSetEntity;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RuleSetRepository extends JpaRepository<RuleSetEntity, UUID> {
  Optional<RuleSetEntity> findByKey(String key);
}
