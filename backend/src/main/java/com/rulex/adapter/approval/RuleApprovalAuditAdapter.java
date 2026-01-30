package com.rulex.adapter.approval;

import com.rulex.core.approval.port.RuleApprovalAuditPort;
import com.rulex.service.AuditService;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class RuleApprovalAuditAdapter implements RuleApprovalAuditPort {

  private final AuditService auditService;

  public RuleApprovalAuditAdapter(AuditService auditService) {
    this.auditService = auditService;
  }

  @Override
  public void logRuleCreated(String ruleName, String actor) {
    auditService.logRuleCreated(ruleName, actor);
  }

  @Override
  public void logRuleUpdated(String ruleName, Map<String, Object> details, String actor) {
    auditService.logRuleUpdated(ruleName, details, actor);
  }

  @Override
  public void logRuleDeleted(String ruleName, String actor) {
    auditService.logRuleDeleted(ruleName, actor);
  }
}
