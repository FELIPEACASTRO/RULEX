package com.rulex.service;

import com.rulex.repository.AuditLogRepository;
import com.rulex.repository.TransactionDecisionRepository;
import com.rulex.repository.TransactionRawStoreRepository;
import com.rulex.repository.complex.RuleExecutionDetailRepository;
import com.rulex.repository.homolog.AuditEntryRepository;
import com.rulex.repository.homolog.DecisionLogRepository;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
@ConditionalOnProperty(name = "rulex.retention.enabled", havingValue = "true", matchIfMissing = true)
public class DataRetentionService {

  private final AuditLogRepository auditLogRepository;
  private final TransactionDecisionRepository transactionDecisionRepository;
  private final TransactionRawStoreRepository transactionRawStoreRepository;
  private final AuditEntryRepository auditEntryRepository;
  private final DecisionLogRepository decisionLogRepository;
  private final RuleExecutionDetailRepository ruleExecutionDetailRepository;

  @Value("${rulex.retention.audit-log-days:90}")
  private int auditLogDays;

  @Value("${rulex.retention.audit-log-cleanup-enabled:true}")
  private boolean auditLogCleanupEnabled;

  @Value("${rulex.retention.decision-days:90}")
  private int decisionDays;

  @Value("${rulex.retention.decision-cleanup-enabled:true}")
  private boolean decisionCleanupEnabled;

  @Value("${rulex.retention.raw-payload-days:30}")
  private int rawPayloadDays;

  @Value("${rulex.retention.raw-payload-cleanup-enabled:true}")
  private boolean rawPayloadCleanupEnabled;

  @Value("${rulex.retention.homolog-audit-days:30}")
  private int homologAuditDays;

  @Value("${rulex.retention.homolog-audit-cleanup-enabled:true}")
  private boolean homologAuditCleanupEnabled;

  @Value("${rulex.retention.homolog-decision-days:30}")
  private int homologDecisionDays;

  @Value("${rulex.retention.homolog-decision-cleanup-enabled:true}")
  private boolean homologDecisionCleanupEnabled;

  @Value("${rulex.retention.rule-exec-details-days:30}")
  private int ruleExecDetailsDays;

  @Value("${rulex.retention.rule-exec-details-cleanup-enabled:true}")
  private boolean ruleExecDetailsCleanupEnabled;

  /**
   * Periodic cleanup to enforce data retention policy.
   */
  @Scheduled(fixedRate = 86_400_000) // Daily
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void cleanupRetentionData() {
    LocalDateTime now = LocalDateTime.now();
    OffsetDateTime nowOffset = OffsetDateTime.now();

    if (auditLogCleanupEnabled && auditLogDays > 0) {
      LocalDateTime cutoff = now.minusDays(auditLogDays);
      auditLogRepository.deleteOldLogs(cutoff);
      log.info("Audit log cleanup complete. RetentionDays={}", auditLogDays);
    }

    if (decisionCleanupEnabled && decisionDays > 0) {
      LocalDateTime cutoff = now.minusDays(decisionDays);
      transactionDecisionRepository.deleteOldDecisions(cutoff);
      log.info("Decision cleanup complete. RetentionDays={}", decisionDays);
    }

    if (rawPayloadCleanupEnabled && rawPayloadDays > 0) {
      LocalDateTime cutoff = now.minusDays(rawPayloadDays);
      transactionRawStoreRepository.deleteOldRawPayloads(cutoff);
      log.info("Raw payload cleanup complete. RetentionDays={}", rawPayloadDays);
    }

    if (ruleExecDetailsCleanupEnabled && ruleExecDetailsDays > 0) {
      OffsetDateTime cutoff = nowOffset.minusDays(ruleExecDetailsDays);
      ruleExecutionDetailRepository.deleteByCreatedAtBefore(cutoff);
      log.info("Rule execution detail cleanup complete. RetentionDays={}", ruleExecDetailsDays);
    }

    if (homologDecisionCleanupEnabled && homologDecisionDays > 0) {
      OffsetDateTime cutoff = nowOffset.minusDays(homologDecisionDays);
      decisionLogRepository.deleteOldDecisionLogs(cutoff);
      log.info("Homolog decision log cleanup complete. RetentionDays={}", homologDecisionDays);
    }

    if (homologAuditCleanupEnabled && homologAuditDays > 0) {
      OffsetDateTime cutoff = nowOffset.minusDays(homologAuditDays);
      auditEntryRepository.deleteOldEntries(cutoff);
      log.info("Homolog audit log cleanup complete. RetentionDays={}", homologAuditDays);
    }
  }
}
