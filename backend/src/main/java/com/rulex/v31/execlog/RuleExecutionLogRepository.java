package com.rulex.v31.execlog;

import java.util.List;
import java.util.UUID;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RuleExecutionLogRepository extends JpaRepository<RuleExecutionLogEntity, UUID> {
  List<RuleExecutionLogEntity> findByExternalTransactionIdOrderByCreatedAtAsc(
      String externalTransactionId);
}
