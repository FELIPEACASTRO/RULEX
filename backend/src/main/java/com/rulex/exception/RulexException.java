package com.rulex.exception;

/**
 * Exceção base para todas as exceções do RULEX.
 * 
 * <p>Hierarquia de exceções:
 * <ul>
 *   <li>RulexException (esta classe)
 *     <ul>
 *       <li>RuleEvaluationException - erros na avaliação de regras</li>
 *       <li>UnsupportedOperatorException - operador não implementado</li>
 *       <li>VelocityCalculationException - erros em cálculos de velocidade</li>
 *       <li>GeoServiceException - erros no serviço de geolocalização</li>
 *       <li>Neo4jConnectionException - erros de conexão com Neo4j</li>
 *       <li>RedisConnectionException - erros de conexão com Redis</li>
 *       <li>InvalidPayloadException - payload inválido</li>
 *       <li>ConfigurationException - erro de configuração</li>
 *     </ul>
 *   </li>
 * </ul>
 */
public class RulexException extends RuntimeException {
    
    private static final long serialVersionUID = 1L;
    
    public RulexException(String message) {
        super(message);
    }
    
    public RulexException(String message, Throwable cause) {
        super(message, cause);
    }
    
    public RulexException(Throwable cause) {
        super(cause);
    }
}
