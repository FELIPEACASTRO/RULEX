package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.NotFoundException;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleApproval;
import com.rulex.entity.RuleApproval.ActionType;
import com.rulex.entity.RuleApproval.ApprovalStatus;
import com.rulex.repository.RuleApprovalRepository;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Serviço para workflow de aprovação de regras (4 olhos). Implementa segregação de funções para
 * compliance.
 */
@Service
@RequiredArgsConstructor
@Slf4j
@Transactional
public class RuleApprovalService {

  private final RuleApprovalRepository approvalRepository;
  private final RuleConfigurationService ruleConfigService;
  private final SecurityContextService securityContextService;
  private final AuditService auditService;
  private final ObjectMapper objectMapper;

  /** Solicita aprovação para criar uma regra. */
  public RuleApproval requestCreateApproval(RuleConfigurationDTO ruleDto) {
    String currentUser = securityContextService.getCurrentUsernameOrSystem();
    String clientIp = securityContextService.getCurrentClientIp();

    log.info(
        "Usuário {} solicitando aprovação para criar regra: {}",
        currentUser,
        ruleDto.getRuleName());

    // Verificar se já existe solicitação pendente
    if (approvalRepository.existsByRuleIdAndStatus(0L, ApprovalStatus.PENDING)) {
      throw new IllegalStateException("Já existe uma solicitação pendente para esta regra");
    }

    String payloadJson = serializePayload(ruleDto);

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

    auditService.logRuleCreated("APPROVAL_REQUEST:" + ruleDto.getRuleName(), currentUser);
    log.info("Solicitação de aprovação criada: {} por {}", approval.getId(), currentUser);

    return approval;
  }

  /** Solicita aprovação para atualizar uma regra. */
  public RuleApproval requestUpdateApproval(Long ruleId, RuleConfigurationDTO ruleDto) {
    String currentUser = securityContextService.getCurrentUsernameOrSystem();
    String clientIp = securityContextService.getCurrentClientIp();

    log.info("Usuário {} solicitando aprovação para atualizar regra: {}", currentUser, ruleId);

    // Verificar se regra existe
    ruleConfigService.getRuleById(ruleId);

    // Verificar se já existe solicitação pendente
    if (approvalRepository.existsByRuleIdAndStatus(ruleId, ApprovalStatus.PENDING)) {
      throw new IllegalStateException("Já existe uma solicitação pendente para esta regra");
    }

    String payloadJson = serializePayload(ruleDto);

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

    auditService.logRuleUpdated(
        "APPROVAL_REQUEST:" + ruleDto.getRuleName(),
        java.util.Map.of("action", "UPDATE_REQUEST"),
        currentUser);

    return approval;
  }

  /** Solicita aprovação para excluir uma regra. */
  public RuleApproval requestDeleteApproval(Long ruleId) {
    String currentUser = securityContextService.getCurrentUsernameOrSystem();
    String clientIp = securityContextService.getCurrentClientIp();

    log.info("Usuário {} solicitando aprovação para excluir regra: {}", currentUser, ruleId);

    RuleConfigurationDTO rule = ruleConfigService.getRuleById(ruleId);

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

    auditService.logRuleDeleted("APPROVAL_REQUEST:" + rule.getRuleName(), currentUser);

    return approval;
  }

  /** Aprova uma solicitação. */
  public ApprovalResult approve(Long approvalId, String comments) {
    String currentUser = securityContextService.getCurrentUsernameOrSystem();

    log.info("Usuário {} aprovando solicitação: {}", currentUser, approvalId);

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

    auditService.logRuleUpdated(
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

  /** Rejeita uma solicitação. */
  public RuleApproval reject(Long approvalId, String reason) {
    String currentUser = securityContextService.getCurrentUsernameOrSystem();

    log.info("Usuário {} rejeitando solicitação: {}", currentUser, approvalId);

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

    auditService.logRuleUpdated(
        "APPROVAL_REJECTED:" + approval.getRuleName(),
        java.util.Map.of("approvalId", approvalId, "reason", reason),
        currentUser);

    return approval;
  }

  /** Cancela uma solicitação (apenas pelo solicitante). */
  public RuleApproval cancel(Long approvalId) {
    String currentUser = securityContextService.getCurrentUsernameOrSystem();

    RuleApproval approval =
        approvalRepository
            .findById(approvalId)
            .orElseThrow(() -> new NotFoundException("Solicitação não encontrada"));

    if (approval.getStatus() != ApprovalStatus.PENDING) {
      throw new IllegalStateException("Solicitação não está pendente");
    }

    if (!approval.getRequestedBy().equals(currentUser) && !securityContextService.isAdmin()) {
      throw new IllegalStateException("Apenas o solicitante ou admin pode cancelar");
    }

    approval.setStatus(ApprovalStatus.CANCELLED);
    approval.setComments("Cancelado por " + currentUser);

    return approvalRepository.save(approval);
  }

  /** Lista solicitações pendentes. */
  public List<RuleApproval> listPendingApprovals() {
    return approvalRepository.findByStatusOrderByRequestedAtDesc(ApprovalStatus.PENDING);
  }

  /** Lista solicitações pendentes paginadas. */
  public Page<RuleApproval> listPendingApprovals(Pageable pageable) {
    return approvalRepository.findByStatus(ApprovalStatus.PENDING, pageable);
  }

  /** Conta solicitações pendentes. */
  public long countPendingApprovals() {
    return approvalRepository.countByStatus(ApprovalStatus.PENDING);
  }

  /** Busca solicitação por ID. */
  public RuleApproval getApprovalById(Long id) {
    return approvalRepository
        .findById(id)
        .orElseThrow(() -> new NotFoundException("Solicitação não encontrada"));
  }

  /** Lista histórico de aprovações de uma regra. */
  public List<RuleApproval> getApprovalHistory(Long ruleId) {
    return approvalRepository.findByRuleIdOrderByRequestedAtDesc(ruleId);
  }

  // Métodos privados

  private RuleConfigurationDTO executeApprovedAction(RuleApproval approval) {
    switch (approval.getActionType()) {
      case CREATE:
        RuleConfigurationDTO createDto = deserializePayload(approval.getPayloadJson());
        RuleConfigurationDTO created = ruleConfigService.createRule(createDto);
        // Atualizar o ruleId na aprovação
        approval.setRuleId(created.getId());
        return created;

      case UPDATE:
        RuleConfigurationDTO updateDto = deserializePayload(approval.getPayloadJson());
        return ruleConfigService.updateRule(approval.getRuleId(), updateDto);

      case DELETE:
        RuleConfigurationDTO toDelete = ruleConfigService.getRuleById(approval.getRuleId());
        ruleConfigService.deleteRule(approval.getRuleId());
        return toDelete;

      case TOGGLE:
        return ruleConfigService.toggleRule(approval.getRuleId());

      default:
        throw new IllegalStateException("Tipo de ação desconhecido: " + approval.getActionType());
    }
  }

  private String serializePayload(RuleConfigurationDTO dto) {
    try {
      return objectMapper.writeValueAsString(dto);
    } catch (Exception e) {
      throw new RuntimeException("Erro ao serializar payload", e);
    }
  }

  private RuleConfigurationDTO deserializePayload(String json) {
    try {
      return objectMapper.readValue(json, RuleConfigurationDTO.class);
    } catch (Exception e) {
      throw new RuntimeException("Erro ao deserializar payload", e);
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
