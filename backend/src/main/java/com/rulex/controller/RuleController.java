package com.rulex.controller;

import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.service.RuleConfigurationService;
import jakarta.validation.Valid;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/** Controller REST para gerenciamento de regras de fraude. */
@RestController
@RequestMapping("/rules")
@RequiredArgsConstructor
@Slf4j
public class RuleController {

  private final RuleConfigurationService ruleConfigurationService;

  /** Lista todas as regras configuradas. GET /api/rules?page=0&size=20 */
  @GetMapping
  public ResponseEntity<Page<RuleConfigurationDTO>> listRules(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size) {

    log.info("Listando regras: page={}, size={}", page, size);

    Pageable pageable = PageRequest.of(page, size);
    Page<RuleConfigurationDTO> rules = ruleConfigurationService.listRules(pageable);
    return ResponseEntity.ok(rules);
  }

  /** Obtém uma regra específica. GET /api/rules/{id} */
  @GetMapping("/{id}")
  public ResponseEntity<RuleConfigurationDTO> getRule(@PathVariable Long id) {
    log.info("Obtendo regra: {}", id);

    RuleConfigurationDTO rule = ruleConfigurationService.getRuleById(id);
    return ResponseEntity.ok(rule);
  }

  /** Cria uma nova regra. POST /api/rules */
  @PostMapping
  public ResponseEntity<RuleConfigurationDTO> createRule(
      @Valid @RequestBody RuleConfigurationDTO request) {

    log.info("Criando nova regra: {}", request.getRuleName());

    RuleConfigurationDTO rule = ruleConfigurationService.createRule(request);
    return ResponseEntity.status(HttpStatus.CREATED).body(rule);
  }

  /** Atualiza uma regra existente. PUT /api/rules/{id} */
  @PutMapping("/{id}")
  public ResponseEntity<RuleConfigurationDTO> updateRule(
      @PathVariable Long id, @Valid @RequestBody RuleConfigurationDTO request) {

    log.info("Atualizando regra: {}", id);

    RuleConfigurationDTO rule = ruleConfigurationService.updateRule(id, request);
    return ResponseEntity.ok(rule);
  }

  /** Deleta uma regra. DELETE /api/rules/{id} */
  @DeleteMapping("/{id}")
  public ResponseEntity<Void> deleteRule(@PathVariable Long id) {
    log.info("Deletando regra: {}", id);

    ruleConfigurationService.deleteRule(id);
    return ResponseEntity.noContent().build();
  }

  /** Ativa/desativa uma regra. PATCH /api/rules/{id}/toggle */
  @PatchMapping("/{id}/toggle")
  public ResponseEntity<RuleConfigurationDTO> toggleRule(
      @PathVariable Long id, @RequestBody(required = false) ToggleRuleRequest request) {
    log.info("Alternando status da regra: {}", id);

    RuleConfigurationDTO rule =
        (request != null && request.enabled != null)
            ? ruleConfigurationService.setRuleEnabled(id, request.enabled)
            : ruleConfigurationService.toggleRule(id);
    return ResponseEntity.ok(rule);
  }

  static class ToggleRuleRequest {
    public Boolean enabled;
  }

  /** Lista regras habilitadas. GET /api/rules/enabled/true */
  @GetMapping("/enabled/{enabled}")
  public ResponseEntity<List<RuleConfigurationDTO>> listEnabledRules(
      @PathVariable Boolean enabled) {

    log.info("Listando regras habilitadas: {}", enabled);

    List<RuleConfigurationDTO> rules = ruleConfigurationService.listRulesByEnabled(enabled);
    return ResponseEntity.ok(rules);
  }

  /** Retorna histórico (append-only) de uma regra. GET /api/rules/{id}/history */
  @GetMapping("/{id}/history")
  public ResponseEntity<?> getRuleHistory(@PathVariable Long id) {
    return ResponseEntity.ok(ruleConfigurationService.getRuleHistory(id));
  }
}
