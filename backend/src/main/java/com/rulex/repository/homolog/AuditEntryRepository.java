package com.rulex.repository.homolog;

import com.rulex.entity.homolog.AuditEntryEntity;
import java.time.OffsetDateTime;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AuditEntryRepository extends JpaRepository<AuditEntryEntity, UUID> {

	/** Delete old audit entries for retention policy */
	@Modifying
	@Query("DELETE FROM AuditEntryEntity a WHERE a.createdAt < :before")
	void deleteOldEntries(@Param("before") OffsetDateTime before);
}
