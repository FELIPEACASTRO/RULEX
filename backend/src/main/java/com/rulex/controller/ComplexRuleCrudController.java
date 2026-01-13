package com.rulex.controller;

import com.rulex.dto.complex.ComplexRuleDTO;
import com.rulex.service.complex.ComplexRuleCrudService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/** Controller REST para CRUD de regras complexas. Endpoint: /api/complex-rules */
@RestController
@RequestMapping("/complex-rules")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Regras Complexas", description = "CRUD de regras complexas com múltiplas condições")
public class ComplexRuleCrudController {

  private final ComplexRuleCrudService complexRuleCrudService;

  /** Lista todas as regras complexas. GET /api/complex-rules */
  @Operation(summary = "Listar regras", description = "Lista todas as regras complexas")
  @ApiResponse(responseCode = "200", description = "Lista de regras")
  @GetMapping
  public ResponseEntity<List<ComplexRuleDTO>> listAll() {
    log.info("Listando todas as regras complexas");
    List<ComplexRuleDTO> rules = complexRuleCrudService.listAll();
    return ResponseEntity.ok(rules);
  }

  /** Obtém uma regra complexa por ID. GET /api/complex-rules/{id} */
  @Operation(summary = "Obter por ID", description = "Busca uma regra complexa pelo ID")
  @ApiResponses({
    @ApiResponse(responseCode = "200", description = "Regra encontrada"),
    @ApiResponse(responseCode = "404", description = "Regra não encontrada")
  })
  @GetMapping("/{id}")
  public ResponseEntity<ComplexRuleDTO> getById(
      @Parameter(description = "ID da regra") @PathVariable UUID id) {
    log.info("Buscando regra complexa por ID: {}", id);
    ComplexRuleDTO rule = complexRuleCrudService.getById(id);
    if (rule == null) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.ok(rule);
  }

  /** Obtém uma regra complexa por chave. GET /api/complex-rules/key/{key} */
  @Operation(summary = "Obter por chave", description = "Busca uma regra complexa pela chave")
  @ApiResponse(responseCode = "200", description = "Regra encontrada")
  @GetMapping("/key/{key}")
  public ResponseEntity<ComplexRuleDTO> getByKey(
      @Parameter(description = "Chave da regra") @PathVariable String key) {
    log.info("Buscando regra complexa por chave: {}", key);
    ComplexRuleDTO rule = complexRuleCrudService.getByKey(key);
    if (rule == null) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.ok(rule);
  }

  /** Cria uma nova regra complexa. POST /api/complex-rules */
  @Operation(summary = "Criar regra", description = "Cria uma nova regra complexa")
  @ApiResponse(responseCode = "201", description = "Regra criada")
  @PostMapping
  public ResponseEntity<ComplexRuleDTO> create(@Valid @RequestBody ComplexRuleDTO rule) {
    log.info("Criando nova regra complexa: {}", rule.getKey());
    ComplexRuleDTO created = complexRuleCrudService.create(rule);
    return ResponseEntity.status(HttpStatus.CREATED).body(created);
  }

  /** Atualiza uma regra complexa existente. PUT /api/complex-rules/{id} */
  @Operation(summary = "Atualizar regra", description = "Atualiza uma regra complexa existente")
  @ApiResponse(responseCode = "200", description = "Regra atualizada")
  @PutMapping("/{id}")
  public ResponseEntity<ComplexRuleDTO> update(
      @Parameter(description = "ID da regra") @PathVariable UUID id,
      @Valid @RequestBody ComplexRuleDTO rule) {
    log.info("Atualizando regra complexa: {}", id);
    ComplexRuleDTO updated = complexRuleCrudService.update(id, rule);
    if (updated == null) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.ok(updated);
  }

  /** Deleta uma regra complexa. DELETE /api/complex-rules/{id} */
  @Operation(summary = "Deletar regra", description = "Remove uma regra complexa")
  @ApiResponse(responseCode = "204", description = "Regra deletada")
  @DeleteMapping("/{id}")
  public ResponseEntity<Void> delete(
      @Parameter(description = "ID da regra") @PathVariable UUID id) {
    log.info("Deletando regra complexa: {}", id);
    boolean deleted = complexRuleCrudService.delete(id);
    if (!deleted) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.noContent().build();
  }

  /**
   * Alterna o status habilitado/desabilitado de uma regra. PATCH
   * /api/complex-rules/{id}/toggle?enabled=true
   */
  @Operation(summary = "Alternar status", description = "Ativa ou desativa uma regra")
  @ApiResponse(responseCode = "200", description = "Status alterado")
  @PatchMapping("/{id}/toggle")
  public ResponseEntity<ComplexRuleDTO> toggle(
      @Parameter(description = "ID da regra") @PathVariable UUID id,
      @RequestParam boolean enabled) {
    log.info("Alternando status da regra {}: enabled={}", id, enabled);
    ComplexRuleDTO updated = complexRuleCrudService.setEnabled(id, enabled);
    if (updated == null) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.ok(updated);
  }

  /**
   * Duplica uma regra complexa com nova chave. POST /api/complex-rules/{id}/duplicate?newKey=xxx
   */
  @Operation(summary = "Duplicar regra", description = "Cria uma cópia da regra com nova chave")
  @ApiResponse(responseCode = "201", description = "Regra duplicada")
  @PostMapping("/{id}/duplicate")
  public ResponseEntity<ComplexRuleDTO> duplicate(
      @Parameter(description = "ID da regra") @PathVariable UUID id, @RequestParam String newKey) {
    log.info("Duplicando regra {} com nova chave: {}", id, newKey);
    ComplexRuleDTO duplicated = complexRuleCrudService.duplicate(id, newKey);
    if (duplicated == null) {
      return ResponseEntity.notFound().build();
    }
    return ResponseEntity.status(HttpStatus.CREATED).body(duplicated);
  }

  /** Valida uma regra complexa sem salvar. POST /api/complex-rules/validate */
  @Operation(summary = "Validar regra", description = "Valida uma regra sem salvar")
  @ApiResponse(responseCode = "200", description = "Resultado da validação")
  @PostMapping("/validate")
  public ResponseEntity<Map<String, Object>> validate(@RequestBody ComplexRuleDTO rule) {
    log.info("Validando regra complexa: {}", rule.getKey());
    var result = complexRuleCrudService.validate(rule);
    return ResponseEntity.ok(result);
  }
}
