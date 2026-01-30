package com.rulex.adapter.approval;

import com.rulex.core.approval.port.RuleApprovalRepositoryPort;
import com.rulex.entity.RuleApproval;
import com.rulex.entity.RuleApproval.ApprovalStatus;
import com.rulex.repository.RuleApprovalRepository;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

@Component
public class RuleApprovalRepositoryAdapter implements RuleApprovalRepositoryPort {

  private final RuleApprovalRepository repository;

  public RuleApprovalRepositoryAdapter(RuleApprovalRepository repository) {
    this.repository = repository;
  }

  @Override
  public boolean existsByRuleIdAndStatus(Long ruleId, ApprovalStatus status) {
    return repository.existsByRuleIdAndStatus(ruleId, status);
  }

  @Override
  public RuleApproval save(RuleApproval approval) {
    return repository.save(approval);
  }

  @Override
  public Optional<RuleApproval> findById(Long id) {
    return repository.findById(id);
  }

  @Override
  public List<RuleApproval> findByStatusOrderByRequestedAtDesc(ApprovalStatus status) {
    return repository.findByStatusOrderByRequestedAtDesc(status);
  }

  @Override
  public Page<RuleApproval> findByStatus(ApprovalStatus status, Pageable pageable) {
    return repository.findByStatus(status, pageable);
  }

  @Override
  public long countByStatus(ApprovalStatus status) {
    return repository.countByStatus(status);
  }

  @Override
  public List<RuleApproval> findByRuleIdOrderByRequestedAtDesc(Long ruleId) {
    return repository.findByRuleIdOrderByRequestedAtDesc(ruleId);
  }
}
