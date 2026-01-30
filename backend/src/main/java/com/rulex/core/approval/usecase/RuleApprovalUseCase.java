package com.rulex.core.approval.usecase;

import com.rulex.api.NotFoundException;
import com.rulex.core.approval.port.RuleApprovalAuditPort;
import com.rulex.core.approval.port.RuleApprovalInputPort;
import com.rulex.core.approval.port.RuleApprovalRepositoryPort;
import com.rulex.core.approval.port.RuleApprovalSerializerPort;
import com.rulex.core.approval.port.RuleConfigurationPort;
import com.rulex.core.approval.port.SecurityContextPort;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleApproval;
import com.rulex.entity.RuleApproval.ActionType;
import com.rulex.entity.RuleApproval.ApprovalStatus;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Builder;
import lombok.Data;

/** Caso de uso para workflow de aprovação de regras (4 olhos). */
public class RuleApprovalUseCase implements RuleApprovalInputPort {

  private final RuleApprovalRepositoryPort approvalRepository;
  private final RuleConfigurationPort ruleConfigPort;
  private final SecurityContextPort securityContextPort;
  private final RuleApprovalAuditPort auditPort;
  private final RuleApprovalSerializerPort serializerPort;

  public RuleApprovalUseCase(
      RuleApprovalRepositoryPort approvalRepository,
      RuleConfigurationPort ruleConfigPort,
      SecurityContextPort securityContextPort,
      RuleApprovalAuditPort auditPort,
      RuleApprovalSerializerPort serializerPort) {
    this.approvalRepository = approvalRepository;
    this.ruleConfigPort = ruleConfigPort;
    this.securityContextPort = securityContextPort;
    this.auditPort = auditPort;
    this.serializerPort = serializerPort;
  }

  @Override
  public RuleApproval requestCreateApproval(RuleConfigurationDTO ruleDto) {
    String currentUser = securityContextPort.getCurrentUsernameOrSystem();
    String clientIp = securityContextPort.getCurrentClientIp();

    // Verificar se já existe solicitação pendente
    if (approvalRepository.existsByRuleIdAndStatus(0L, ApprovalStatus.PENDING)) {
      throw new IllegalStateException("Já existe uma solicitação pendente para esta regra");
    }

    String payloadJson = serializerPort.serialize(ruleDto);

    RuleApproval approval =
        RuleApproval.builder()
            .ruleId(0L) // Será atualizado após criação
            .ruleName(ruleDto.getRuleName())
            .actionType(ActionType.CREATE)
            .requestedBy(currentUser)
            .status(ApprovalStatus.PENDING)
            .payloadJson(payloadJson)
            .clientIp(clientIp)
            .build();

    approval = approvalRepository.save(approval);

    auditPort.logRuleCreated("APPROVAL_REQUEST:" + ruleDto.getRuleName(), currentUser);

    return approval;
  }

  @Override
  public RuleApproval requestUpdateApproval(Long ruleId, RuleConfigurationDTO ruleDto) {
    String currentUser = securityContextPort.getCurrentUsernameOrSystem();
    String clientIp = securityContextPort.getCurrentClientIp();

    // Verificar se regra existe
    ruleConfigPort.getRuleById(ruleId);

    // Verificar se já existe solicitação pendente
    if (approvalRepository.existsByRuleIdAndStatus(ruleId, ApprovalStatus.PENDING)) {
      throw new IllegalStateException("Já existe uma solicitação pendente para esta regra");
    }

    String payloadJson = serializerPort.serialize(ruleDto);

    RuleApproval approval =
        RuleApproval.builder()
            .ruleId(ruleId)
            .ruleName(ruleDto.getRuleName())
            .actionType(ActionType.UPDATE)
            .requestedBy(currentUser)
            .status(ApprovalStatus.PENDING)
            .payloadJson(payloadJson)
            .clientIp(clientIp)
            .build();

    approval = approvalRepository.save(approval);

    auditPort.logRuleUpdated(
        "APPROVAL_REQUEST:" + ruleDto.getRuleName(),
        java.util.Map.of("action", "UPDATE_REQUEST"),
        currentUser);

    return approval;
  }

  @Override
  public RuleApproval requestDeleteApproval(Long ruleId) {
    String currentUser = securityContextPort.getCurrentUsernameOrSystem();
    String clientIp = securityContextPort.getCurrentClientIp();

    RuleConfigurationDTO rule = ruleConfigPort.getRuleById(ruleId);

    // Verificar se já existe solicitação pendente
    if (approvalRepository.existsByRuleIdAndStatus(ruleId, ApprovalStatus.PENDING)) {
      throw new IllegalStateException("Já existe uma solicitação pendente para esta regra");
    }

    RuleApproval approval =
        RuleApproval.builder()
            .ruleId(ruleId)
            .ruleName(rule.getRuleName())
            .actionType(ActionType.DELETE)
            .requestedBy(currentUser)
            .status(ApprovalStatus.PENDING)
            .clientIp(clientIp)
            .build();

    approval = approvalRepository.save(approval);

    auditPort.logRuleDeleted("APPROVAL_REQUEST:" + rule.getRuleName(), currentUser);

    return approval;
  }

  @Override
  public ApprovalResult approve(Long approvalId, String comments) {
    String currentUser = securityContextPort.getCurrentUsernameOrSystem();

    RuleApproval approval =
        approvalRepository
            .findById(approvalId)
            .orElseThrow(() -> new NotFoundException("Solicitação não encontrada"));

    // Verificar se está pendente
    if (approval.getStatus() != ApprovalStatus.PENDING) {
      throw new IllegalStateException("Solicitação não está pendente");
    }

    // Verificar segregação de funções (não pode aprovar própria solicitação)
    if (approval.getRequestedBy().equals(currentUser)) {
      throw new IllegalStateException("Não é permitido aprovar sua própria solicitação");
    }

    // Executar a ação
    RuleConfigurationDTO result = executeApprovedAction(approval);

    // Atualizar aprovação
    approval.setApprovedBy(currentUser);
    approval.setApprovedAt(LocalDateTime.now());
    approval.setStatus(ApprovalStatus.APPROVED);
    approval.setComments(comments);
    approvalRepository.save(approval);

    auditPort.logRuleUpdated(
        "APPROVAL_APPROVED:" + approval.getRuleName(),
        java.util.Map.of("approvalId", approvalId, "action", approval.getActionType().name()),
        currentUser);

    return ApprovalResult.builder()
        .approval(approval)
        .executedRule(result)
        .success(true)
        .message("Solicitação aprovada e executada com sucesso")
        .build();
  }

  @Override
  public RuleApproval reject(Long approvalId, String reason) {
    String currentUser = securityContextPort.getCurrentUsernameOrSystem();

    RuleApproval approval =
        approvalRepository
            .findById(approvalId)
            .orElseThrow(() -> new NotFoundException("Solicitação não encontrada"));

    if (approval.getStatus() != ApprovalStatus.PENDING) {
      throw new IllegalStateException("Solicitação não está pendente");
    }

    approval.setRejectedBy(currentUser);
    approval.setRejectedAt(LocalDateTime.now());
    approval.setStatus(ApprovalStatus.REJECTED);
    approval.setComments(reason);

    approval = approvalRepository.save(approval);

    auditPort.logRuleUpdated(
        "APPROVAL_REJECTED:" + approval.getRuleName(),
        java.util.Map.of("approvalId", approvalId, "reason", reason),
        currentUser);

    return approval;
  }

  @Override
  public RuleApproval cancel(Long approvalId) {
    String currentUser = securityContextPort.getCurrentUsernameOrSystem();

    RuleApproval approval =
        approvalRepository
            .findById(approvalId)
            .orElseThrow(() -> new NotFoundException("Solicitação não encontrada"));

    if (approval.getStatus() != ApprovalStatus.PENDING) {
      throw new IllegalStateException("Solicitação não está pendente");
    }

    if (!approval.getRequestedBy().equals(currentUser) && !securityContextPort.isAdmin()) {
      throw new IllegalStateException("Apenas o solicitante ou admin pode cancelar");
    }

    approval.setStatus(ApprovalStatus.CANCELLED);
    approval.setComments("Cancelado por " + currentUser);

    return approvalRepository.save(approval);
  }

  @Override
  public List<RuleApproval> listPendingApprovals() {
    return approvalRepository.findByStatusOrderByRequestedAtDesc(ApprovalStatus.PENDING);
  }

  @Override
  public org.springframework.data.domain.Page<RuleApproval> listPendingApprovals(
      org.springframework.data.domain.Pageable pageable) {
    return approvalRepository.findByStatus(ApprovalStatus.PENDING, pageable);
  }

  @Override
  public long countPendingApprovals() {
    return approvalRepository.countByStatus(ApprovalStatus.PENDING);
  }

  @Override
  public RuleApproval getApprovalById(Long id) {
    return approvalRepository
        .findById(id)
        .orElseThrow(() -> new NotFoundException("Solicitação não encontrada"));
  }

  @Override
  public List<RuleApproval> getApprovalHistory(Long ruleId) {
    return approvalRepository.findByRuleIdOrderByRequestedAtDesc(ruleId);
  }

  private RuleConfigurationDTO executeApprovedAction(RuleApproval approval) {
    switch (approval.getActionType()) {
      case CREATE:
        RuleConfigurationDTO createDto = serializerPort.deserialize(approval.getPayloadJson());
        RuleConfigurationDTO created = ruleConfigPort.createRule(createDto);
        // Atualizar o ruleId na aprovação
        approval.setRuleId(created.getId());
        return created;

      case UPDATE:
        RuleConfigurationDTO updateDto = serializerPort.deserialize(approval.getPayloadJson());
        return ruleConfigPort.updateRule(approval.getRuleId(), updateDto);

      case DELETE:
        RuleConfigurationDTO toDelete = ruleConfigPort.getRuleById(approval.getRuleId());
        ruleConfigPort.deleteRule(approval.getRuleId());
        return toDelete;

      case TOGGLE:
        return ruleConfigPort.toggleRule(approval.getRuleId());

      default:
        throw new IllegalStateException("Tipo de ação desconhecido: " + approval.getActionType());
    }
  }

  @Data
  @Builder
  public static class ApprovalResult {
    private RuleApproval approval;
    private RuleConfigurationDTO executedRule;
    private boolean success;
    private String message;
  }
}
