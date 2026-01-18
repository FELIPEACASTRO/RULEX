package com.rulex.exception;

/**
 * Exceção lançada quando ocorre erro em cálculos de velocidade.
 */
public class VelocityCalculationException extends RulexException {
    
    private static final long serialVersionUID = 1L;
    
    private final String metric;
    private final String dimension;
    
    public VelocityCalculationException(String message) {
        super(message);
        this.metric = null;
        this.dimension = null;
    }
    
    public VelocityCalculationException(String message, Throwable cause) {
        super(message, cause);
        this.metric = null;
        this.dimension = null;
    }
    
    public VelocityCalculationException(String metric, String dimension, String message) {
        super(String.format("Erro no cálculo de velocidade [%s/%s]: %s", metric, dimension, message));
        this.metric = metric;
        this.dimension = dimension;
    }
    
    public String getMetric() {
        return metric;
    }
    
    public String getDimension() {
        return dimension;
    }
}
