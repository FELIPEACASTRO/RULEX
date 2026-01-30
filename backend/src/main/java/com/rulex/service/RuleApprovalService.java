package com.rulex.service;

import com.rulex.core.approval.port.RuleApprovalAuditPort;
import com.rulex.core.approval.port.RuleApprovalInputPort;
import com.rulex.core.approval.port.RuleApprovalRepositoryPort;
import com.rulex.core.approval.port.RuleApprovalSerializerPort;
import com.rulex.core.approval.port.RuleConfigurationPort;
import com.rulex.core.approval.port.SecurityContextPort;
import com.rulex.core.approval.usecase.RuleApprovalUseCase;
import com.rulex.core.approval.usecase.RuleApprovalUseCase.ApprovalResult;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleApproval;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/** Adapter Spring para workflow de aprovação de regras. */
@Service
@Transactional
public class RuleApprovalService implements RuleApprovalInputPort {

  private final RuleApprovalUseCase useCase;

  public RuleApprovalService(
      RuleApprovalRepositoryPort approvalRepository,
      RuleConfigurationPort ruleConfigPort,
      SecurityContextPort securityContextPort,
      RuleApprovalAuditPort auditPort,
      RuleApprovalSerializerPort serializerPort) {
    this.useCase =
        new RuleApprovalUseCase(
            approvalRepository, ruleConfigPort, securityContextPort, auditPort, serializerPort);
  }

  @Override
  public RuleApproval requestCreateApproval(RuleConfigurationDTO ruleDto) {
    return useCase.requestCreateApproval(ruleDto);
  }

  @Override
  public RuleApproval requestUpdateApproval(Long ruleId, RuleConfigurationDTO ruleDto) {
    return useCase.requestUpdateApproval(ruleId, ruleDto);
  }

  @Override
  public RuleApproval requestDeleteApproval(Long ruleId) {
    return useCase.requestDeleteApproval(ruleId);
  }

  @Override
  public ApprovalResult approve(Long approvalId, String comments) {
    return useCase.approve(approvalId, comments);
  }

  @Override
  public RuleApproval reject(Long approvalId, String reason) {
    return useCase.reject(approvalId, reason);
  }

  @Override
  public RuleApproval cancel(Long approvalId) {
    return useCase.cancel(approvalId);
  }

  @Override
  public List<RuleApproval> listPendingApprovals() {
    return useCase.listPendingApprovals();
  }

  @Override
  public Page<RuleApproval> listPendingApprovals(Pageable pageable) {
    return useCase.listPendingApprovals(pageable);
  }

  @Override
  public long countPendingApprovals() {
    return useCase.countPendingApprovals();
  }

  @Override
  public RuleApproval getApprovalById(Long id) {
    return useCase.getApprovalById(id);
  }

  @Override
  public List<RuleApproval> getApprovalHistory(Long ruleId) {
    return useCase.getApprovalHistory(ruleId);
  }
}

