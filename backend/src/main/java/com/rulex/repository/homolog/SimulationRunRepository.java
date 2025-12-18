package com.rulex.repository.homolog;

import com.rulex.entity.homolog.SimulationRunEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface SimulationRunRepository extends JpaRepository<SimulationRunEntity, UUID> {
}
