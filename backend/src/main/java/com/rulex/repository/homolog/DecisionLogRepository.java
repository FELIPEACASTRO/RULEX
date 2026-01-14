package com.rulex.repository.homolog;

import com.rulex.entity.homolog.DecisionLogEntity;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DecisionLogRepository extends JpaRepository<DecisionLogEntity, UUID> {}
