package com.rulex.repository.homolog;

import com.rulex.entity.homolog.SimulationRunEntity;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface SimulationRunRepository extends JpaRepository<SimulationRunEntity, UUID> {}
