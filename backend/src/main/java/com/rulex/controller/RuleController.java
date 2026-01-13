package com.rulex.controller;

import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.service.RuleConfigurationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "Regras", description = "CRUD de regras de detecção de fraude")
public class RuleController {

  private final RuleConfigurationService ruleConfigurationService;

  /** Lista todas as regras configuradas. GET /api/rules?page=0&size=20 */
  @Operation(summary = "Listar regras", description = "Retorna lista paginada de todas as regras")
  @ApiResponses({@ApiResponse(responseCode = "200", description = "Lista de regras retornada")})
  @GetMapping
  public ResponseEntity<Page<RuleConfigurationDTO>> listRules(
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "20") int size) {

    log.info("Listando regras: page={}, size={}", page, size);

    Pageable pageable = PageRequest.of(page, size);
    Page<RuleConfigurationDTO> rules = ruleConfigurationService.listRules(pageable);
    return ResponseEntity.ok(rules);
  }

  /** Obtém uma regra específica. GET /api/rules/{id} */
  @Operation(summary = "Obter regra", description = "Retorna uma regra pelo ID")
  @ApiResponses({
    @ApiResponse(responseCode = "200", description = "Regra encontrada"),
    @ApiResponse(responseCode = "404", description = "Regra não encontrada")
  })
  @GetMapping("/{id}")
  public ResponseEntity<RuleConfigurationDTO> getRule(
      @Parameter(description = "ID da regra") @PathVariable Long id) {
    log.info("Obtendo regra: {}", id);

    RuleConfigurationDTO rule = ruleConfigurationService.getRuleById(id);
    return ResponseEntity.ok(rule);
  }

  /** Cria uma nova regra. POST /api/rules */
  @Operation(summary = "Criar regra", description = "Cria uma nova regra de detecção")
  @ApiResponses({
    @ApiResponse(responseCode = "201", description = "Regra criada"),
    @ApiResponse(responseCode = "400", description = "Dados inválidos")
  })
  @PostMapping
  public ResponseEntity<RuleConfigurationDTO> createRule(
      @Valid @RequestBody RuleConfigurationDTO request) {

    log.info("Criando nova regra: {}", request.getRuleName());

    RuleConfigurationDTO rule = ruleConfigurationService.createRule(request);
    return ResponseEntity.status(HttpStatus.CREATED).body(rule);
  }

  /** Atualiza uma regra existente. PUT /api/rules/{id} */
  @Operation(summary = "Atualizar regra", description = "Atualiza uma regra existente")
  @ApiResponses({
    @ApiResponse(responseCode = "200", description = "Regra atualizada"),
    @ApiResponse(responseCode = "404", description = "Regra não encontrada")
  })
  @PutMapping("/{id}")
  public ResponseEntity<RuleConfigurationDTO> updateRule(
      @Parameter(description = "ID da regra") @PathVariable Long id,
      @Valid @RequestBody RuleConfigurationDTO request) {

    log.info("Atualizando regra: {}", id);

    RuleConfigurationDTO rule = ruleConfigurationService.updateRule(id, request);
    return ResponseEntity.ok(rule);
  }

  /** Deleta uma regra. DELETE /api/rules/{id} */
  @Operation(summary = "Deletar regra", description = "Remove uma regra do sistema")
  @ApiResponses({
    @ApiResponse(responseCode = "204", description = "Regra deletada"),
    @ApiResponse(responseCode = "404", description = "Regra não encontrada")
  })
  @DeleteMapping("/{id}")
  public ResponseEntity<Void> deleteRule(
      @Parameter(description = "ID da regra") @PathVariable Long id) {
    log.info("Deletando regra: {}", id);

    ruleConfigurationService.deleteRule(id);
    return ResponseEntity.noContent().build();
  }

  /** Ativa/desativa uma regra. PATCH /api/rules/{id}/toggle */
  @Operation(summary = "Alternar status", description = "Ativa ou desativa uma regra")
  @ApiResponse(responseCode = "200", description = "Status alterado")
  @PatchMapping("/{id}/toggle")
  public ResponseEntity<RuleConfigurationDTO> toggleRule(
      @Parameter(description = "ID da regra") @PathVariable Long id,
      @RequestBody(required = false) ToggleRuleRequest request) {
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
  @Operation(
      summary = "Listar por status",
      description = "Lista regras por status habilitado/desabilitado")
  @ApiResponse(responseCode = "200", description = "Lista de regras")
  @GetMapping("/enabled/{enabled}")
  public ResponseEntity<List<RuleConfigurationDTO>> listEnabledRules(
      @Parameter(description = "Status habilitado") @PathVariable Boolean enabled) {

    log.info("Listando regras habilitadas: {}", enabled);

    List<RuleConfigurationDTO> rules = ruleConfigurationService.listRulesByEnabled(enabled);
    return ResponseEntity.ok(rules);
  }

  /** Retorna histórico (append-only) de uma regra. GET /api/rules/{id}/history */
  @Operation(
      summary = "Histórico da regra",
      description = "Retorna histórico de alterações da regra")
  @ApiResponse(responseCode = "200", description = "Histórico retornado")
  @GetMapping("/{id}/history")
  public ResponseEntity<?> getRuleHistory(
      @Parameter(description = "ID da regra") @PathVariable Long id) {
    return ResponseEntity.ok(ruleConfigurationService.getRuleHistory(id));
  }
}
