package com.rulex.core.approval.port;

import com.rulex.entity.RuleApproval;
import com.rulex.entity.RuleApproval.ApprovalStatus;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface RuleApprovalRepositoryPort {

  boolean existsByRuleIdAndStatus(Long ruleId, ApprovalStatus status);

  RuleApproval save(RuleApproval approval);

  Optional<RuleApproval> findById(Long id);

  List<RuleApproval> findByStatusOrderByRequestedAtDesc(ApprovalStatus status);

  Page<RuleApproval> findByStatus(ApprovalStatus status, Pageable pageable);

  long countByStatus(ApprovalStatus status);

  List<RuleApproval> findByRuleIdOrderByRequestedAtDesc(Long ruleId);
}
