package com.rulex.exception;

/**
 * Exceção base para todas as exceções do RULEX.
 *
 * <p>Hierarquia de exceções:
 *
 * <ul>
 *   <li>RulexException (esta classe)
 *       <ul>
 *         <li>RuleEvaluationException - erros na avaliação de regras
 *         <li>UnsupportedOperatorException - operador não implementado
 *         <li>VelocityCalculationException - erros em cálculos de velocidade
 *         <li>GeoServiceException - erros no serviço de geolocalização
 *         <li>Neo4jConnectionException - erros de conexão com Neo4j
 *         <li>RedisConnectionException - erros de conexão com Redis
 *         <li>InvalidPayloadException - payload inválido
 *         <li>ConfigurationException - erro de configuração
 *       </ul>
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
