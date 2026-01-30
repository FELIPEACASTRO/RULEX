package com.rulex.core.approval.port;

import java.util.Map;

public interface RuleApprovalAuditPort {

  void logRuleCreated(String ruleName, String actor);

  void logRuleUpdated(String ruleName, Map<String, Object> details, String actor);

  void logRuleDeleted(String ruleName, String actor);
}
