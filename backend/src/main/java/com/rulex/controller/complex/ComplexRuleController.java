package com.rulex.controller.complex;

import com.rulex.dto.complex.*;
import com.rulex.entity.complex.RuleTemplate;
import com.rulex.service.complex.ComplexRuleEvaluator;
import com.rulex.service.complex.ComplexRuleService;
import jakarta.validation.Valid;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para gerenciamento de regras complexas.
 * API para gerenciamento de regras complexas com grupos aninhados.
 */
@RestController
@RequestMapping("/api/v1/complex-rules")
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleController {

    private final ComplexRuleService complexRuleService;
    private final ComplexRuleEvaluator complexRuleEvaluator;

    // ========== Condition Groups ==========

    /**
     * Salva a estrutura completa de condições de uma regra
     */
    @PostMapping("/{ruleVersionId}/conditions")
    public ResponseEntity<ConditionGroupDTO> saveConditionGroup(
            @PathVariable UUID ruleVersionId,
            @Valid @RequestBody ConditionGroupDTO conditionGroup) {
        log.info("Salvando grupo de condições para regra: {}", ruleVersionId);
        ConditionGroupDTO saved = complexRuleService.saveConditionGroup(ruleVersionId, conditionGroup);
        return ResponseEntity.ok(saved);
    }

    /**
     * Retorna a estrutura completa de condições de uma regra
     */
    @GetMapping("/{ruleVersionId}/conditions")
    public ResponseEntity<ConditionGroupDTO> getConditionGroup(@PathVariable UUID ruleVersionId) {
        ConditionGroupDTO group = complexRuleService.getConditionGroup(ruleVersionId);
        if (group == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(group);
    }

    // ========== Expressions ==========

    /**
     * Salva as expressões calculadas de uma regra
     */
    @PostMapping("/{ruleVersionId}/expressions")
    public ResponseEntity<List<ExpressionDTO>> saveExpressions(
            @PathVariable UUID ruleVersionId,
            @Valid @RequestBody List<ExpressionDTO> expressions) {
        log.info("Salvando {} expressões para regra: {}", expressions.size(), ruleVersionId);
        List<ExpressionDTO> saved = complexRuleService.saveExpressions(ruleVersionId, expressions);
        return ResponseEntity.ok(saved);
    }

    /**
     * Retorna as expressões calculadas de uma regra
     */
    @GetMapping("/{ruleVersionId}/expressions")
    public ResponseEntity<List<ExpressionDTO>> getExpressions(@PathVariable UUID ruleVersionId) {
        return ResponseEntity.ok(complexRuleService.getExpressions(ruleVersionId));
    }

    // ========== Context Variables ==========

    /**
     * Salva as variáveis de contexto de uma regra
     */
    @PostMapping("/{ruleVersionId}/variables")
    public ResponseEntity<List<ContextVariableDTO>> saveContextVariables(
            @PathVariable UUID ruleVersionId,
            @Valid @RequestBody List<ContextVariableDTO> variables) {
        log.info("Salvando {} variáveis de contexto para regra: {}", variables.size(), ruleVersionId);
        List<ContextVariableDTO> saved = complexRuleService.saveContextVariables(ruleVersionId, variables);
        return ResponseEntity.ok(saved);
    }

    /**
     * Retorna as variáveis de contexto de uma regra
     */
    @GetMapping("/{ruleVersionId}/variables")
    public ResponseEntity<List<ContextVariableDTO>> getContextVariables(@PathVariable UUID ruleVersionId) {
        return ResponseEntity.ok(complexRuleService.getContextVariables(ruleVersionId));
    }

    // ========== Actions ==========

    /**
     * Salva as ações de uma regra
     */
    @PostMapping("/{ruleVersionId}/actions")
    public ResponseEntity<List<RuleActionDTO>> saveActions(
            @PathVariable UUID ruleVersionId,
            @Valid @RequestBody List<RuleActionDTO> actions) {
        log.info("Salvando {} ações para regra: {}", actions.size(), ruleVersionId);
        List<RuleActionDTO> saved = complexRuleService.saveActions(ruleVersionId, actions);
        return ResponseEntity.ok(saved);
    }

    /**
     * Retorna as ações de uma regra
     */
    @GetMapping("/{ruleVersionId}/actions")
    public ResponseEntity<List<RuleActionDTO>> getActions(@PathVariable UUID ruleVersionId) {
        return ResponseEntity.ok(complexRuleService.getActions(ruleVersionId));
    }

    // ========== Fields Used ==========

    /**
     * Retorna os campos usados nas condições de uma regra
     */
    @GetMapping("/{ruleVersionId}/fields")
    public ResponseEntity<List<String>> getFieldsUsed(@PathVariable UUID ruleVersionId) {
        return ResponseEntity.ok(complexRuleService.getFieldsUsed(ruleVersionId));
    }

    // ========== Validation ==========

    /**
     * Verifica se a profundidade de aninhamento está dentro do limite
     */
    @GetMapping("/{ruleVersionId}/validate-depth")
    public ResponseEntity<Map<String, Object>> validateDepth(
            @PathVariable UUID ruleVersionId,
            @RequestParam(defaultValue = "10") int maxDepth) {
        boolean valid = complexRuleService.validateMaxDepth(ruleVersionId, maxDepth);
        return ResponseEntity.ok(Map.of(
                "valid", valid,
                "maxAllowedDepth", maxDepth
        ));
    }

    // ========== Templates ==========

    /**
     * Retorna todos os templates de regras
     */
    @GetMapping("/templates")
    public ResponseEntity<List<RuleTemplate>> getAllTemplates() {
        return ResponseEntity.ok(complexRuleService.getAllTemplates());
    }

    /**
     * Retorna os templates pré-configurados do sistema
     */
    @GetMapping("/templates/system")
    public ResponseEntity<List<RuleTemplate>> getSystemTemplates() {
        return ResponseEntity.ok(complexRuleService.getSystemTemplates());
    }

    /**
     * Retorna templates de uma categoria específica
     */
    @GetMapping("/templates/category/{category}")
    public ResponseEntity<List<RuleTemplate>> getTemplatesByCategory(@PathVariable String category) {
        return ResponseEntity.ok(complexRuleService.getTemplatesByCategory(category));
    }

    /**
     * Retorna um template específico
     */
    @GetMapping("/templates/{name}")
    public ResponseEntity<RuleTemplate> getTemplateByName(@PathVariable String name) {
        RuleTemplate template = complexRuleService.getTemplateByName(name);
        if (template == null) {
            return ResponseEntity.notFound().build();
        }
        return ResponseEntity.ok(template);
    }

    // ========== Evaluation (Test) ==========

    /**
     * Avalia uma regra contra um payload de teste
     */
    @PostMapping("/{ruleVersionId}/evaluate")
    public ResponseEntity<ComplexRuleEvaluator.EvaluationResult> evaluateRule(
            @PathVariable UUID ruleVersionId,
            @RequestBody Map<String, Object> payload) {
        log.info("Avaliando regra {} com payload de teste", ruleVersionId);

        // Buscar grupo de condições
        ConditionGroupDTO conditionGroup = complexRuleService.getConditionGroup(ruleVersionId);
        if (conditionGroup == null) {
            return ResponseEntity.notFound().build();
        }

        // Criar contexto de avaliação
        ComplexRuleEvaluator.EvaluationContext context = ComplexRuleEvaluator.EvaluationContext.builder()
                .payload(payload)
                .ruleVersionId(ruleVersionId)
                .build();

        // Converter DTO para entidade para avaliação
        // Nota: Em produção, isso seria feito de forma mais eficiente
        var mapper = new com.rulex.service.complex.ComplexRuleMapper();
        var groupEntity = mapper.toEntity(conditionGroup, ruleVersionId, null);

        // Avaliar
        ComplexRuleEvaluator.EvaluationResult result = complexRuleEvaluator.evaluate(groupEntity, context);

        return ResponseEntity.ok(result);
    }

    // ========== Delete ==========

    /**
     * Remove toda a estrutura de uma regra complexa
     */
    @DeleteMapping("/{ruleVersionId}")
    public ResponseEntity<Void> deleteRuleStructure(@PathVariable UUID ruleVersionId) {
        log.info("Deletando estrutura da regra: {}", ruleVersionId);
        complexRuleService.deleteRuleStructure(ruleVersionId);
        return ResponseEntity.noContent().build();
    }
}
