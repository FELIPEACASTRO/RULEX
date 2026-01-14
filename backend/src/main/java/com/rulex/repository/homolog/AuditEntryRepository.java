package com.rulex.repository.homolog;

import com.rulex.entity.homolog.AuditEntryEntity;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AuditEntryRepository extends JpaRepository<AuditEntryEntity, UUID> {}
