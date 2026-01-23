package com.rulex.repository.homolog;

import com.rulex.entity.homolog.DecisionLogEntity;
import java.time.OffsetDateTime;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface DecisionLogRepository extends JpaRepository<DecisionLogEntity, UUID> {

	/** Delete old decision logs for retention policy */
	@Modifying
	@Query("DELETE FROM DecisionLogEntity d WHERE d.createdAt < :before")
	void deleteOldDecisionLogs(@Param("before") OffsetDateTime before);
}
