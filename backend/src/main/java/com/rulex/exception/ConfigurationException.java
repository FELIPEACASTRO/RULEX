package com.rulex.exception;

/**
 * Exceção lançada quando há erro de configuração do sistema.
 *
 * <p>Exemplos:
 *
 * <ul>
 *   <li>Senha de admin não configurada
 *   <li>Senha muito fraca
 *   <li>Configuração de banco inválida
 * </ul>
 */
public class ConfigurationException extends RulexException {

  private static final long serialVersionUID = 1L;

  private final String configKey;

  public ConfigurationException(String message) {
    super(message);
    this.configKey = null;
  }

  public ConfigurationException(String configKey, String message) {
    super(String.format("Erro de configuração [%s]: %s", configKey, message));
    this.configKey = configKey;
  }

  public ConfigurationException(String message, Throwable cause) {
    super(message, cause);
    this.configKey = null;
  }

  public String getConfigKey() {
    return configKey;
  }
}
