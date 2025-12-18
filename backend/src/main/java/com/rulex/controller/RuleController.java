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

    try {
      Pageable pageable = PageRequest.of(page, size);
      Page<RuleConfigurationDTO> rules = ruleConfigurationService.listRules(pageable);
      return ResponseEntity.ok(rules);
    } catch (Exception e) {
      log.error("Erro ao listar regras", e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
    }
  }

  /** Obtém uma regra específica. GET /api/rules/{id} */
  @GetMapping("/{id}")
  public ResponseEntity<RuleConfigurationDTO> getRule(@PathVariable Long id) {
    log.info("Obtendo regra: {}", id);

    try {
      RuleConfigurationDTO rule = ruleConfigurationService.getRuleById(id);
      return ResponseEntity.ok(rule);
    } catch (Exception e) {
      log.error("Erro ao obter regra: {}", id, e);
      return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
    }
  }

  /** Cria uma nova regra. POST /api/rules */
  @PostMapping
  public ResponseEntity<RuleConfigurationDTO> createRule(
      @Valid @RequestBody RuleConfigurationDTO request) {

    log.info("Criando nova regra: {}", request.getRuleName());

    try {
      RuleConfigurationDTO rule = ruleConfigurationService.createRule(request);
      return ResponseEntity.status(HttpStatus.CREATED).body(rule);
    } catch (Exception e) {
      log.error("Erro ao criar regra", e);
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
    }
  }

  /** Atualiza uma regra existente. PUT /api/rules/{id} */
  @PutMapping("/{id}")
  public ResponseEntity<RuleConfigurationDTO> updateRule(
      @PathVariable Long id, @Valid @RequestBody RuleConfigurationDTO request) {

    log.info("Atualizando regra: {}", id);

    try {
      RuleConfigurationDTO rule = ruleConfigurationService.updateRule(id, request);
      return ResponseEntity.ok(rule);
    } catch (Exception e) {
      log.error("Erro ao atualizar regra: {}", id, e);
      return ResponseEntity.status(HttpStatus.BAD_REQUEST).build();
    }
  }

  /** Deleta uma regra. DELETE /api/rules/{id} */
  @DeleteMapping("/{id}")
  public ResponseEntity<Void> deleteRule(@PathVariable Long id) {
    log.info("Deletando regra: {}", id);

    try {
      ruleConfigurationService.deleteRule(id);
      return ResponseEntity.noContent().build();
    } catch (Exception e) {
      log.error("Erro ao deletar regra: {}", id, e);
      return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
    }
  }

  /** Ativa/desativa uma regra. PATCH /api/rules/{id}/toggle */
  @PatchMapping("/{id}/toggle")
  public ResponseEntity<RuleConfigurationDTO> toggleRule(@PathVariable Long id) {
    log.info("Alternando status da regra: {}", id);

    try {
      RuleConfigurationDTO rule = ruleConfigurationService.toggleRule(id);
      return ResponseEntity.ok(rule);
    } catch (Exception e) {
      log.error("Erro ao alternar regra: {}", id, e);
      return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
    }
  }

  /** Lista regras habilitadas. GET /api/rules/enabled/true */
  @GetMapping("/enabled/{enabled}")
  public ResponseEntity<List<RuleConfigurationDTO>> listEnabledRules(
      @PathVariable Boolean enabled) {

    log.info("Listando regras habilitadas: {}", enabled);

    try {
      List<RuleConfigurationDTO> rules = ruleConfigurationService.listRulesByEnabled(enabled);
      return ResponseEntity.ok(rules);
    } catch (Exception e) {
      log.error("Erro ao listar regras habilitadas", e);
      return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
    }
  }

  /** Retorna histórico (append-only) de uma regra. GET /api/rules/{id}/history */
  @GetMapping("/{id}/history")
  public ResponseEntity<?> getRuleHistory(@PathVariable Long id) {
    try {
      return ResponseEntity.ok(ruleConfigurationService.getRuleHistory(id));
    } catch (Exception e) {
      log.error("Erro ao obter histórico da regra: {}", id, e);
      return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
    }
  }
}
