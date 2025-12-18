package com.rulex.repository.homolog;

import com.rulex.entity.homolog.ActiveRuleSetEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ActiveRuleSetRepository extends JpaRepository<ActiveRuleSetEntity, Short> {
}
