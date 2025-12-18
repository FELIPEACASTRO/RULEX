package com.rulex.repository.homolog;

import com.rulex.entity.homolog.AuditEntryEntity;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface AuditEntryRepository extends JpaRepository<AuditEntryEntity, UUID> {
}
