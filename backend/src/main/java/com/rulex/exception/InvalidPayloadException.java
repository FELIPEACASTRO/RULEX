package com.rulex.exception;

/**
 * Exceção lançada quando o payload da transação é inválido.
 */
public class InvalidPayloadException extends RulexException {
    
    private static final long serialVersionUID = 1L;
    
    private final String field;
    
    public InvalidPayloadException(String message) {
        super(message);
        this.field = null;
    }
    
    public InvalidPayloadException(String field, String message) {
        super(String.format("Campo '%s' inválido: %s", field, message));
        this.field = field;
    }
    
    public InvalidPayloadException(String message, Throwable cause) {
        super(message, cause);
        this.field = null;
    }
    
    public String getField() {
        return field;
    }
}
