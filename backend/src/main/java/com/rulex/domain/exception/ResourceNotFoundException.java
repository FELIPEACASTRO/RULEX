package com.rulex.domain.exception;

import java.io.Serial;

/**
 * Exceção lançada quando recurso não é encontrado.
 *
 * <p>Usada para transações, regras ou outros recursos que não existem no sistema.
 */
public class ResourceNotFoundException extends DomainException {

  @Serial private static final long serialVersionUID = 1L;

  private final String resourceType;
  private final String resourceId;

  public ResourceNotFoundException(String resourceType, String resourceId) {
    super("RESOURCE_NOT_FOUND", String.format("%s não encontrado: %s", resourceType, resourceId));
    this.resourceType = resourceType;
    this.resourceId = resourceId;
  }

  public String getResourceType() {
    return resourceType;
  }

  public String getResourceId() {
    return resourceId;
  }

  /** Cria exceção para transação não encontrada */
  public static ResourceNotFoundException transaction(String externalTransactionId) {
    return new ResourceNotFoundException("Transação", externalTransactionId);
  }

  /** Cria exceção para regra não encontrada */
  public static ResourceNotFoundException rule(Long ruleId) {
    return new ResourceNotFoundException("Regra", String.valueOf(ruleId));
  }

  /** Cria exceção para regra não encontrada por nome */
  public static ResourceNotFoundException ruleByName(String ruleName) {
    return new ResourceNotFoundException("Regra", ruleName);
  }
}
