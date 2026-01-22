package com.rulex.service.engine.operator.strategy;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Strategy para operadores de string.
 * 
 * <p>Operadores suportados:
 * <ul>
 *   <li>CONTAINS - contém substring</li>
 *   <li>NOT_CONTAINS - não contém substring</li>
 *   <li>STARTS_WITH - começa com</li>
 *   <li>ENDS_WITH - termina com</li>
 *   <li>REGEX - corresponde a expressão regular</li>
 *   <li>NOT_REGEX - não corresponde a expressão regular</li>
 * </ul>
 * 
 * @author Refactoring - RULEX Team
 * @since 2.0.0
 */
@Component
@Slf4j
public class StringOperatorStrategy implements OperatorStrategy {

    private static final Set<ConditionOperator> SUPPORTED = Set.of(
        ConditionOperator.CONTAINS,
        ConditionOperator.NOT_CONTAINS,
        ConditionOperator.STARTS_WITH,
        ConditionOperator.ENDS_WITH,
        ConditionOperator.REGEX,
        ConditionOperator.NOT_REGEX
    );

    // Cache de patterns compilados para performance
    private final java.util.concurrent.ConcurrentHashMap<String, Pattern> patternCache = 
        new java.util.concurrent.ConcurrentHashMap<>();

    @Override
    public Set<ConditionOperator> supportedOperators() {
        return SUPPORTED;
    }

    @Override
    public String category() {
        return "STRING";
    }

    @Override
    public int priority() {
        return 20; // Alta prioridade - operadores simples
    }

    @Override
    public boolean evaluate(Object fieldValue, RuleCondition condition, EvaluationContext context) {
        ConditionOperator operator = condition.getOperator();
        String expectedValue = condition.getValueSingle();
        Boolean caseSensitive = condition.getCaseSensitive();

        if (fieldValue == null) {
            return false;
        }

        String fieldStr = String.valueOf(fieldValue);

        return switch (operator) {
            case CONTAINS -> evaluateContains(fieldStr, expectedValue, caseSensitive);
            case NOT_CONTAINS -> !evaluateContains(fieldStr, expectedValue, caseSensitive);
            case STARTS_WITH -> evaluateStartsWith(fieldStr, expectedValue, caseSensitive);
            case ENDS_WITH -> evaluateEndsWith(fieldStr, expectedValue, caseSensitive);
            case REGEX -> evaluateRegex(fieldStr, expectedValue);
            case NOT_REGEX -> !evaluateRegex(fieldStr, expectedValue);
            default -> {
                log.warn("Unexpected operator in StringOperatorStrategy: {}", operator);
                yield false;
            }
        };
    }

    /**
     * Verifica se string contém substring.
     */
    private boolean evaluateContains(String fieldValue, String substring, Boolean caseSensitive) {
        if (substring == null || substring.isEmpty()) {
            return true;
        }

        boolean isCaseSensitive = caseSensitive != null && caseSensitive;
        
        if (isCaseSensitive) {
            return fieldValue.contains(substring);
        } else {
            return fieldValue.toLowerCase().contains(substring.toLowerCase());
        }
    }

    /**
     * Verifica se string começa com prefixo.
     */
    private boolean evaluateStartsWith(String fieldValue, String prefix, Boolean caseSensitive) {
        if (prefix == null || prefix.isEmpty()) {
            return true;
        }

        boolean isCaseSensitive = caseSensitive != null && caseSensitive;
        
        if (isCaseSensitive) {
            return fieldValue.startsWith(prefix);
        } else {
            return fieldValue.toLowerCase().startsWith(prefix.toLowerCase());
        }
    }

    /**
     * Verifica se string termina com sufixo.
     */
    private boolean evaluateEndsWith(String fieldValue, String suffix, Boolean caseSensitive) {
        if (suffix == null || suffix.isEmpty()) {
            return true;
        }

        boolean isCaseSensitive = caseSensitive != null && caseSensitive;
        
        if (isCaseSensitive) {
            return fieldValue.endsWith(suffix);
        } else {
            return fieldValue.toLowerCase().endsWith(suffix.toLowerCase());
        }
    }

    /**
     * Verifica se string corresponde a expressão regular.
     */
    private boolean evaluateRegex(String fieldValue, String pattern) {
        if (pattern == null || pattern.isEmpty()) {
            return true;
        }

        try {
            Pattern compiledPattern = patternCache.computeIfAbsent(pattern, p -> {
                try {
                    return Pattern.compile(p);
                } catch (PatternSyntaxException e) {
                    log.warn("Invalid regex pattern: {}", p);
                    return null;
                }
            });

            if (compiledPattern == null) {
                return false;
            }

            return compiledPattern.matcher(fieldValue).matches();
        } catch (Exception e) {
            log.warn("Error evaluating regex '{}' against value '{}': {}", 
                pattern, fieldValue, e.getMessage());
            return false;
        }
    }
}
