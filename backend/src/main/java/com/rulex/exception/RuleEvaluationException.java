package com.rulex.exception;

/**
 * Exceção lançada quando ocorre erro durante a avaliação de uma regra.
 */
public class RuleEvaluationException extends RulexException {
    
    private static final long serialVersionUID = 1L;
    
    private final String ruleId;
    private final String ruleName;
    
    public RuleEvaluationException(String ruleId, String message) {
        super(String.format("Erro ao avaliar regra '%s': %s", ruleId, message));
        this.ruleId = ruleId;
        this.ruleName = null;
    }
    
    public RuleEvaluationException(String ruleId, String ruleName, String message) {
        super(String.format("Erro ao avaliar regra '%s' (%s): %s", ruleId, ruleName, message));
        this.ruleId = ruleId;
        this.ruleName = ruleName;
    }
    
    public RuleEvaluationException(String ruleId, String message, Throwable cause) {
        super(String.format("Erro ao avaliar regra '%s': %s", ruleId, message), cause);
        this.ruleId = ruleId;
        this.ruleName = null;
    }
    
    public String getRuleId() {
        return ruleId;
    }
    
    public String getRuleName() {
        return ruleName;
    }
}
