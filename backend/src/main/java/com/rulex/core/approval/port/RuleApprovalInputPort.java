package com.rulex.core.approval.port;

import com.rulex.core.approval.usecase.RuleApprovalUseCase.ApprovalResult;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleApproval;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface RuleApprovalInputPort {

  RuleApproval requestCreateApproval(RuleConfigurationDTO ruleDto);

  RuleApproval requestUpdateApproval(Long ruleId, RuleConfigurationDTO ruleDto);

  RuleApproval requestDeleteApproval(Long ruleId);

  ApprovalResult approve(Long approvalId, String comments);

  RuleApproval reject(Long approvalId, String reason);

  RuleApproval cancel(Long approvalId);

  List<RuleApproval> listPendingApprovals();

  Page<RuleApproval> listPendingApprovals(Pageable pageable);

  long countPendingApprovals();

  RuleApproval getApprovalById(Long id);

  List<RuleApproval> getApprovalHistory(Long ruleId);
}
