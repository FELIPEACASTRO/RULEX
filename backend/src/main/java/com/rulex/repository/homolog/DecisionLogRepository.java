package com.rulex.repository.homolog;

import com.rulex.entity.homolog.DecisionLogEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface DecisionLogRepository extends JpaRepository<DecisionLogEntity, UUID> {
}
