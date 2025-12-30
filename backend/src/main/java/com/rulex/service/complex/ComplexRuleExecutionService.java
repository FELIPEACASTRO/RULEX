package com.rulex.service.complex;

import com.rulex.dto.complex.*;
import com.rulex.entity.complex.*;
import com.rulex.repository.complex.*;
import jakarta.transaction.Transactional;
import java.util.*;
import java.util.stream.Collectors;
import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * Serviço de execução de regras complexas.
 * Coordena a avaliação de regras com variáveis de contexto, expressões e ações.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ComplexRuleExecutionService {

    private final ComplexRuleService complexRuleService;
    private final ComplexRuleEvaluator ruleEvaluator;
    private final ExpressionEvaluator expressionEvaluator;
    private final ComplexRuleMapper mapper;
    private final RuleConditionGroupRepository conditionGroupRepository;
    private final RuleContextVariableRepository contextVariableRepository;
    private final RuleExpressionRepository expressionRepository;
    private final RuleActionRepository actionRepository;
    private final RuleExecutionDetailRepository executionDetailRepository;

    /**
     * Resultado completo da execução de uma regra
     */
    @Data
    @Builder
    public static class ComplexRuleExecutionResult {
        private UUID ruleVersionId;
        private boolean matched;
        private String decision;
        private Integer riskScore;
        private List<String> tags;
        private Map<String, Object> variables;
        private List<RuleExecutionDetail> executionDetails;
        private List<ActionExecutionResult> actionResults;
        private long executionTimeMs;
        private String errorMessage;
    }

    /**
     * Resultado da execução de uma ação
     */
    @Data
    @Builder
    public static class ActionExecutionResult {
        private UUID actionId;
        private RuleAction.ActionType actionType;
        private boolean executed;
        private Map<String, Object> result;
        private String errorMessage;
    }

    /**
     * Executa uma regra complexa contra um payload
     */
    @Transactional
    public ComplexRuleExecutionResult executeRule(UUID ruleVersionId, Map<String, Object> payload, UUID decisionLogId) {
        long startTime = System.currentTimeMillis();
        log.info("Executando regra complexa: {}", ruleVersionId);

        try {
            // 1. Preparar contexto com variáveis
            Map<String, Object> variables = prepareContextVariables(ruleVersionId, payload);

            // 2. Avaliar expressões
            Map<String, Object> expressions = evaluateExpressions(ruleVersionId, payload, variables);
            variables.putAll(expressions);

            // 3. Criar contexto de avaliação
            ComplexRuleEvaluator.EvaluationContext context = ComplexRuleEvaluator.EvaluationContext.builder()
                    .payload(payload)
                    .variables(variables)
                    .decisionLogId(decisionLogId)
                    .ruleVersionId(ruleVersionId)
                    .build();

            // 4. Buscar e avaliar grupo de condições
            RuleConditionGroup rootGroup = conditionGroupRepository
                    .findByRuleVersionIdAndParentGroupIdIsNull(ruleVersionId)
                    .orElse(null);

            if (rootGroup == null) {
                log.warn("Nenhum grupo de condições encontrado para regra: {}", ruleVersionId);
                return ComplexRuleExecutionResult.builder()
                        .ruleVersionId(ruleVersionId)
                        .matched(false)
                        .executionTimeMs(System.currentTimeMillis() - startTime)
                        .errorMessage("Nenhum grupo de condições configurado")
                        .build();
            }

            // 5. Avaliar condições
            ComplexRuleEvaluator.EvaluationResult evalResult = ruleEvaluator.evaluate(rootGroup, context);

            // 6. Salvar detalhes de execução
            if (decisionLogId != null && evalResult.getExecutionDetails() != null) {
                executionDetailRepository.saveAll(evalResult.getExecutionDetails());
            }

            // 7. Se a regra foi acionada, executar ações
            List<ActionExecutionResult> actionResults = new ArrayList<>();
            String decision = null;
            Integer riskScore = null;
            List<String> tags = new ArrayList<>();

            if (evalResult.isMatched()) {
                List<RuleAction> actions = actionRepository.findByRuleVersionIdAndEnabledTrueOrderByPositionAsc(ruleVersionId);
                
                for (RuleAction action : actions) {
                    ActionExecutionResult actionResult = executeAction(action, payload, variables, context);
                    actionResults.add(actionResult);

                    // Processar resultados das ações
                    if (actionResult.isExecuted() && actionResult.getResult() != null) {
                        switch (action.getActionType()) {
                            case SET_DECISION -> decision = (String) actionResult.getResult().get("decision");
                            case SET_SCORE -> riskScore = (Integer) actionResult.getResult().get("score");
                            case ADD_TAG -> tags.add((String) actionResult.getResult().get("tag"));
                            case SET_VARIABLE -> {
                                String varName = (String) actionResult.getResult().get("variable");
                                Object varValue = actionResult.getResult().get("value");
                                variables.put(varName, varValue);
                            }
                            default -> {} // Outras ações são tratadas externamente
                        }
                    }
                }
            }

            return ComplexRuleExecutionResult.builder()
                    .ruleVersionId(ruleVersionId)
                    .matched(evalResult.isMatched())
                    .decision(decision)
                    .riskScore(riskScore)
                    .tags(tags)
                    .variables(variables)
                    .executionDetails(evalResult.getExecutionDetails())
                    .actionResults(actionResults)
                    .executionTimeMs(System.currentTimeMillis() - startTime)
                    .build();

        } catch (Exception e) {
            log.error("Erro ao executar regra {}: {}", ruleVersionId, e.getMessage(), e);
            return ComplexRuleExecutionResult.builder()
                    .ruleVersionId(ruleVersionId)
                    .matched(false)
                    .executionTimeMs(System.currentTimeMillis() - startTime)
                    .errorMessage(e.getMessage())
                    .build();
        }
    }

    /**
     * Prepara variáveis de contexto
     */
    private Map<String, Object> prepareContextVariables(UUID ruleVersionId, Map<String, Object> payload) {
        Map<String, Object> variables = new HashMap<>();
        
        List<RuleContextVariable> contextVars = contextVariableRepository.findByRuleVersionId(ruleVersionId);
        
        for (RuleContextVariable var : contextVars) {
            try {
                Object value = resolveContextVariable(var, payload, variables);
                if (value != null) {
                    variables.put(var.getName(), value);
                } else if (var.getDefaultValue() != null) {
                    variables.put(var.getName(), var.getDefaultValue());
                }
            } catch (Exception e) {
                log.warn("Erro ao resolver variável {}: {}", var.getName(), e.getMessage());
                if (var.getDefaultValue() != null) {
                    variables.put(var.getName(), var.getDefaultValue());
                }
            }
        }
        
        return variables;
    }

    /**
     * Resolve uma variável de contexto
     */
    @SuppressWarnings("unchecked")
    private Object resolveContextVariable(RuleContextVariable var, Map<String, Object> payload, Map<String, Object> variables) {
        Map<String, Object> config = var.getSourceConfig();
        
        return switch (var.getSourceType()) {
            case PAYLOAD -> {
                String fieldPath = (String) config.get("field");
                yield resolveNestedValue(fieldPath, payload);
            }
            case EXPRESSION -> {
                String expression = (String) config.get("expression");
                Map<String, Object> context = new HashMap<>(payload);
                context.putAll(variables);
                yield expressionEvaluator.evaluate(expression, context);
            }
            case LOOKUP -> {
                // Implementação simplificada - em produção, faria lookup real
                log.debug("Lookup não implementado para variável: {}", var.getName());
                yield null;
            }
            case AGGREGATION -> {
                // Implementação simplificada - em produção, faria agregação real
                log.debug("Agregação não implementada para variável: {}", var.getName());
                yield null;
            }
            case EXTERNAL_SERVICE -> {
                // Implementação simplificada - em produção, chamaria serviço externo
                log.debug("Serviço externo não implementado para variável: {}", var.getName());
                yield null;
            }
            case RULE_RESULT -> {
                String ruleKey = (String) config.get("ruleKey");
                yield variables.get("rule_result_" + ruleKey);
            }
        };
    }

    /**
     * Avalia expressões definidas na regra
     */
    private Map<String, Object> evaluateExpressions(UUID ruleVersionId, Map<String, Object> payload, Map<String, Object> variables) {
        Map<String, Object> results = new HashMap<>();
        
        List<RuleExpression> expressions = expressionRepository.findByRuleVersionId(ruleVersionId);
        
        // Criar contexto combinado
        Map<String, Object> context = new HashMap<>(payload);
        context.putAll(variables);
        
        for (RuleExpression expr : expressions) {
            try {
                Object result = expressionEvaluator.evaluate(expr.getExpression(), context);
                if (result != null) {
                    results.put(expr.getName(), result);
                    context.put(expr.getName(), result); // Disponibilizar para próximas expressões
                }
            } catch (Exception e) {
                log.warn("Erro ao avaliar expressão {}: {}", expr.getName(), e.getMessage());
            }
        }
        
        return results;
    }

    /**
     * Executa uma ação
     */
    @SuppressWarnings("unchecked")
    private ActionExecutionResult executeAction(RuleAction action, Map<String, Object> payload, 
                                                 Map<String, Object> variables, 
                                                 ComplexRuleEvaluator.EvaluationContext context) {
        Map<String, Object> config = action.getActionConfig();
        
        try {
            // Verificar condição da ação se houver
            if (action.getConditionGroupId() != null) {
                RuleConditionGroup conditionGroup = conditionGroupRepository
                        .findById(action.getConditionGroupId())
                        .orElse(null);
                
                if (conditionGroup != null) {
                    ComplexRuleEvaluator.EvaluationResult condResult = ruleEvaluator.evaluate(conditionGroup, context);
                    if (!condResult.isMatched()) {
                        return ActionExecutionResult.builder()
                                .actionId(action.getId())
                                .actionType(action.getActionType())
                                .executed(false)
                                .build();
                    }
                }
            }

            Map<String, Object> result = new HashMap<>();
            
            switch (action.getActionType()) {
                case SET_DECISION -> {
                    result.put("decision", config.get("decision"));
                }
                case SET_SCORE -> {
                    Object scoreValue = config.get("score");
                    Object scoreModifier = config.get("scoreModifier");
                    
                    if (scoreValue != null) {
                        result.put("score", Integer.parseInt(String.valueOf(scoreValue)));
                    } else if (scoreModifier != null) {
                        String modifier = String.valueOf(scoreModifier);
                        int currentScore = variables.containsKey("riskScore") 
                                ? Integer.parseInt(String.valueOf(variables.get("riskScore"))) 
                                : 0;
                        
                        if (modifier.startsWith("+")) {
                            result.put("score", currentScore + Integer.parseInt(modifier.substring(1)));
                        } else if (modifier.startsWith("-")) {
                            result.put("score", currentScore - Integer.parseInt(modifier.substring(1)));
                        }
                    }
                }
                case ADD_TAG -> {
                    result.put("tag", config.get("tag"));
                }
                case REMOVE_TAG -> {
                    result.put("removeTag", config.get("tag"));
                }
                case SET_VARIABLE -> {
                    result.put("variable", config.get("variable"));
                    
                    Object value = config.get("value");
                    if (value instanceof String && ((String) value).contains("${")) {
                        // Avaliar expressão
                        Map<String, Object> ctx = new HashMap<>(payload);
                        ctx.putAll(variables);
                        value = expressionEvaluator.evaluate((String) value, ctx);
                    }
                    result.put("value", value);
                }
                case CALL_WEBHOOK -> {
                    // Implementação simplificada - em produção, faria chamada HTTP real
                    log.info("Webhook seria chamado: {}", config.get("url"));
                    result.put("webhookUrl", config.get("url"));
                    result.put("status", "PENDING");
                }
                case SEND_NOTIFICATION -> {
                    log.info("Notificação seria enviada: canal={}, template={}", 
                            config.get("channel"), config.get("template"));
                    result.put("channel", config.get("channel"));
                    result.put("template", config.get("template"));
                    result.put("status", "PENDING");
                }
                case BLOCK_TRANSACTION -> {
                    result.put("blocked", true);
                    result.put("reason", config.get("reason"));
                }
                case FLAG_FOR_REVIEW -> {
                    result.put("flagged", true);
                    result.put("queue", config.get("queue"));
                    result.put("priority", config.get("priority"));
                }
                case ESCALATE -> {
                    result.put("escalated", true);
                    result.put("level", config.get("level"));
                    result.put("reason", config.get("reason"));
                }
            }

            return ActionExecutionResult.builder()
                    .actionId(action.getId())
                    .actionType(action.getActionType())
                    .executed(true)
                    .result(result)
                    .build();

        } catch (Exception e) {
            log.error("Erro ao executar ação {}: {}", action.getActionType(), e.getMessage());
            return ActionExecutionResult.builder()
                    .actionId(action.getId())
                    .actionType(action.getActionType())
                    .executed(false)
                    .errorMessage(e.getMessage())
                    .build();
        }
    }

    /**
     * Resolve valor aninhado
     */
    @SuppressWarnings("unchecked")
    private Object resolveNestedValue(String path, Map<String, Object> data) {
        if (path == null || data == null) return null;
        
        String[] parts = path.split("\\.");
        Object current = data;

        for (String part : parts) {
            if (current == null) return null;
            if (current instanceof Map) {
                current = ((Map<String, Object>) current).get(part);
            } else {
                return null;
            }
        }
        return current;
    }

    /**
     * Executa múltiplas regras e retorna resultados agregados
     */
    public List<ComplexRuleExecutionResult> executeRules(List<UUID> ruleVersionIds, Map<String, Object> payload, UUID decisionLogId) {
        return ruleVersionIds.stream()
                .map(ruleVersionId -> executeRule(ruleVersionId, payload, decisionLogId))
                .collect(Collectors.toList());
    }
}
