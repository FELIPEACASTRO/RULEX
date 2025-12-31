package com.rulex.controller;

import com.rulex.dto.complex.ComplexRuleDTO;
import com.rulex.dto.complex.ConditionGroupDTO;
import com.rulex.service.complex.ComplexRuleCrudService;
import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Controller REST para CRUD de regras complexas.
 * Endpoint: /api/complex-rules
 */
@RestController
@RequestMapping("/complex-rules")
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleCrudController {

    private final ComplexRuleCrudService complexRuleCrudService;

    /**
     * Lista todas as regras complexas.
     * GET /api/complex-rules
     */
    @GetMapping
    public ResponseEntity<List<ComplexRuleDTO>> listAll() {
        log.info("Listando todas as regras complexas");
        List<ComplexRuleDTO> rules = complexRuleCrudService.listAll();
        return ResponseEntity.ok(rules);
    }

    /**
     * Obtém uma regra complexa por ID.
     * GET /api/complex-rules/{id}
     */
    @GetMapping("/{id}")
    public ResponseEntity<ComplexRuleDTO> getById(@PathVariable UUID id) {
        log.info("Buscando regra complexa por ID: {}", id);
        ComplexRuleDTO rule = complexRuleCrudService.getById(id);
        if (rule == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(rule);
    }

    /**
     * Obtém uma regra complexa por chave.
     * GET /api/complex-rules/key/{key}
     */
    @GetMapping("/key/{key}")
    public ResponseEntity<ComplexRuleDTO> getByKey(@PathVariable String key) {
        log.info("Buscando regra complexa por chave: {}", key);
        ComplexRuleDTO rule = complexRuleCrudService.getByKey(key);
        if (rule == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(rule);
    }

    /**
     * Cria uma nova regra complexa.
     * POST /api/complex-rules
     */
    @PostMapping
    public ResponseEntity<ComplexRuleDTO> create(@Valid @RequestBody ComplexRuleDTO rule) {
        log.info("Criando nova regra complexa: {}", rule.getKey());
        ComplexRuleDTO created = complexRuleCrudService.create(rule);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    /**
     * Atualiza uma regra complexa existente.
     * PUT /api/complex-rules/{id}
     */
    @PutMapping("/{id}")
    public ResponseEntity<ComplexRuleDTO> update(
            @PathVariable UUID id,
            @Valid @RequestBody ComplexRuleDTO rule) {
        log.info("Atualizando regra complexa: {}", id);
        ComplexRuleDTO updated = complexRuleCrudService.update(id, rule);
        if (updated == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(updated);
    }

    /**
     * Deleta uma regra complexa.
     * DELETE /api/complex-rules/{id}
     */
    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(@PathVariable UUID id) {
        log.info("Deletando regra complexa: {}", id);
        boolean deleted = complexRuleCrudService.delete(id);
        if (!deleted) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.noContent().build();
    }

    /**
     * Alterna o status habilitado/desabilitado de uma regra.
     * PATCH /api/complex-rules/{id}/toggle?enabled=true
     */
    @PatchMapping("/{id}/toggle")
    public ResponseEntity<ComplexRuleDTO> toggle(
            @PathVariable UUID id,
            @RequestParam boolean enabled) {
        log.info("Alternando status da regra {}: enabled={}", id, enabled);
        ComplexRuleDTO updated = complexRuleCrudService.setEnabled(id, enabled);
        if (updated == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(updated);
    }

    /**
     * Duplica uma regra complexa com nova chave.
     * POST /api/complex-rules/{id}/duplicate?newKey=xxx
     */
    @PostMapping("/{id}/duplicate")
    public ResponseEntity<ComplexRuleDTO> duplicate(
            @PathVariable UUID id,
            @RequestParam String newKey) {
        log.info("Duplicando regra {} com nova chave: {}", id, newKey);
        ComplexRuleDTO duplicated = complexRuleCrudService.duplicate(id, newKey);
        if (duplicated == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.status(HttpStatus.CREATED).body(duplicated);
    }

    /**
     * Valida uma regra complexa sem salvar.
     * POST /api/complex-rules/validate
     */
    @PostMapping("/validate")
    public ResponseEntity<Map<String, Object>> validate(@RequestBody ComplexRuleDTO rule) {
        log.info("Validando regra complexa: {}", rule.getKey());
        var result = complexRuleCrudService.validate(rule);
        return ResponseEntity.ok(result);
    }
}
