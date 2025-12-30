package com.rulex.controller;

import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleApproval;
import com.rulex.service.RuleApprovalService;
import com.rulex.service.RuleApprovalService.ApprovalResult;
import jakarta.validation.Valid;
import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para workflow de aprovação de regras. Implementa segregação de funções (4 olhos) para
 * compliance.
 */
@RestController
@RequestMapping("/rules/approvals")
@RequiredArgsConstructor
@Slf4j
public class RuleApprovalController {

  private final RuleApprovalService approvalService;

  /** Solicita aprovação para criar uma regra. POST /api/rules/approvals/create */
  @PostMapping("/create")
  public ResponseEntity<RuleApproval> requestCreateApproval(
      @Valid @RequestBody RuleConfigurationDTO ruleDto) {
    log.info("Solicitação de aprovação para criar regra: {}", ruleDto.getRuleName());
    RuleApproval approval = approvalService.requestCreateApproval(ruleDto);
    return ResponseEntity.status(HttpStatus.CREATED).body(approval);
  }

  /** Solicita aprovação para atualizar uma regra. POST /api/rules/approvals/update/{ruleId} */
  @PostMapping("/update/{ruleId}")
  public ResponseEntity<RuleApproval> requestUpdateApproval(
      @PathVariable Long ruleId, @Valid @RequestBody RuleConfigurationDTO ruleDto) {
    log.info("Solicitação de aprovação para atualizar regra: {}", ruleId);
    RuleApproval approval = approvalService.requestUpdateApproval(ruleId, ruleDto);
    return ResponseEntity.status(HttpStatus.CREATED).body(approval);
  }

  /** Solicita aprovação para excluir uma regra. POST /api/rules/approvals/delete/{ruleId} */
  @PostMapping("/delete/{ruleId}")
  public ResponseEntity<RuleApproval> requestDeleteApproval(@PathVariable Long ruleId) {
    log.info("Solicitação de aprovação para excluir regra: {}", ruleId);
    RuleApproval approval = approvalService.requestDeleteApproval(ruleId);
    return ResponseEntity.status(HttpStatus.CREATED).body(approval);
  }

  /** Aprova uma solicitação. POST /api/rules/approvals/{id}/approve */
  @PostMapping("/{id}/approve")
  public ResponseEntity<ApprovalResult> approve(
      @PathVariable Long id, @RequestBody(required = false) ApprovalRequest request) {
    log.info("Aprovando solicitação: {}", id);
    String comments = request != null ? request.getComments() : null;
    ApprovalResult result = approvalService.approve(id, comments);
    return ResponseEntity.ok(result);
  }

  /** Rejeita uma solicitação. POST /api/rules/approvals/{id}/reject */
  @PostMapping("/{id}/reject")
  public ResponseEntity<RuleApproval> reject(
      @PathVariable Long id, @RequestBody RejectRequest request) {
    log.info("Rejeitando solicitação: {}", id);
    RuleApproval approval = approvalService.reject(id, request.getReason());
    return ResponseEntity.ok(approval);
  }

  /** Cancela uma solicitação. POST /api/rules/approvals/{id}/cancel */
  @PostMapping("/{id}/cancel")
  public ResponseEntity<RuleApproval> cancel(@PathVariable Long id) {
    log.info("Cancelando solicitação: {}", id);
    RuleApproval approval = approvalService.cancel(id);
    return ResponseEntity.ok(approval);
  }

  /** Lista solicitações pendentes. GET /api/rules/approvals/pending */
  @GetMapping("/pending")
  public ResponseEntity<List<RuleApproval>> listPendingApprovals() {
    List<RuleApproval> approvals = approvalService.listPendingApprovals();
    return ResponseEntity.ok(approvals);
  }

  /** Lista solicitações pendentes paginadas. GET /api/rules/approvals/pending/page */
  @GetMapping("/pending/page")
  public ResponseEntity<Page<RuleApproval>> listPendingApprovalsPaged(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size) {
    Page<RuleApproval> approvals = approvalService.listPendingApprovals(PageRequest.of(page, size));
    return ResponseEntity.ok(approvals);
  }

  /** Conta solicitações pendentes. GET /api/rules/approvals/pending/count */
  @GetMapping("/pending/count")
  public ResponseEntity<CountResponse> countPendingApprovals() {
    long count = approvalService.countPendingApprovals();
    return ResponseEntity.ok(new CountResponse(count));
  }

  /** Busca solicitação por ID. GET /api/rules/approvals/{id} */
  @GetMapping("/{id}")
  public ResponseEntity<RuleApproval> getApprovalById(@PathVariable Long id) {
    RuleApproval approval = approvalService.getApprovalById(id);
    return ResponseEntity.ok(approval);
  }

  /** Lista histórico de aprovações de uma regra. GET /api/rules/approvals/history/{ruleId} */
  @GetMapping("/history/{ruleId}")
  public ResponseEntity<List<RuleApproval>> getApprovalHistory(@PathVariable Long ruleId) {
    List<RuleApproval> history = approvalService.getApprovalHistory(ruleId);
    return ResponseEntity.ok(history);
  }

  // Request/Response DTOs

  @Data
  public static class ApprovalRequest {
    private String comments;
  }

  @Data
  public static class RejectRequest {
    private String reason;
  }

  @Data
  public static class CountResponse {
    private final long count;
  }
}
