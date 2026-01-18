package com.rulex.exception;

import com.rulex.entity.complex.RuleCondition.ConditionOperator;

/**
 * Exceção lançada quando um operador não está implementado.
 * 
 * <p>Esta exceção é lançada pelo StubOperatorEvaluator para operadores
 * que estão declarados mas ainda não foram implementados (status: PLANNED).
 * 
 * <p>O cliente deve tratar esta exceção e:
 * <ul>
 *   <li>Informar ao usuário que o operador não está disponível</li>
 *   <li>Sugerir operadores alternativos via endpoint /api/operators/status</li>
 * </ul>
 */
public class UnsupportedOperatorException extends RulexException {
    
    private static final long serialVersionUID = 1L;
    
    private final ConditionOperator operator;
    private final String status;
    
    public UnsupportedOperatorException(ConditionOperator operator) {
        super(String.format("Operador '%s' não está implementado. Status: PLANNED. " +
            "Consulte /api/operators/status para ver operadores disponíveis.", operator.name()));
        this.operator = operator;
        this.status = "PLANNED";
    }
    
    public UnsupportedOperatorException(ConditionOperator operator, String additionalInfo) {
        super(String.format("Operador '%s' não está implementado. %s", operator.name(), additionalInfo));
        this.operator = operator;
        this.status = "PLANNED";
    }
    
    public UnsupportedOperatorException(ConditionOperator operator, String status, String message) {
        super(message);
        this.operator = operator;
        this.status = status;
    }
    
    public ConditionOperator getOperator() {
        return operator;
    }
    
    public String getStatus() {
        return status;
    }
}
