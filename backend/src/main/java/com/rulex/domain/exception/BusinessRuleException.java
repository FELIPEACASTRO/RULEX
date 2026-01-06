package com.rulex.domain.exception;

import java.io.Serial;
import java.util.HashMap;
import java.util.Map;

/**
 * Exceção lançada quando regra de negócio é violada.
 *
 * <p>Diferente de ContractViolationException (payload malformado), esta exceção indica violação de
 * regras de negócio do sistema.
 */
public class BusinessRuleException extends DomainException {

  @Serial private static final long serialVersionUID = 1L;

  private final String ruleName;
  private final HashMap<String, Object> context;

  public BusinessRuleException(String message) {
    super("BUSINESS_RULE_VIOLATION", message);
    this.ruleName = null;
    this.context = null;
  }

  public BusinessRuleException(String code, String message) {
    super(code, message);
    this.ruleName = null;
    this.context = null;
  }

  public BusinessRuleException(String code, String message, String ruleName) {
    super(code, message);
    this.ruleName = ruleName;
    this.context = null;
  }

  public BusinessRuleException(
      String code, String message, String ruleName, Map<String, Object> context) {
    super(code, message);
    this.ruleName = ruleName;
    this.context = context != null ? new HashMap<>(context) : null;
  }

  public String getRuleName() {
    return ruleName;
  }

  public Map<String, Object> getContext() {
    return context;
  }

  /** Cria exceção para regra duplicada */
  public static BusinessRuleException duplicateRule(String ruleName) {
    return new BusinessRuleException(
        "DUPLICATE_RULE", String.format("Já existe uma regra com o nome: %s", ruleName), ruleName);
  }

  /** Cria exceção para regra em uso (não pode ser deletada) */
  public static BusinessRuleException ruleInUse(String ruleName) {
    return new BusinessRuleException(
        "RULE_IN_USE",
        String.format("Regra '%s' não pode ser removida pois está em uso", ruleName),
        ruleName);
  }

  /** Cria exceção para workflow de aprovação */
  public static BusinessRuleException approvalRequired(String action) {
    return new BusinessRuleException(
        "APPROVAL_REQUIRED",
        String.format("Ação '%s' requer aprovação de outro usuário (4 olhos)", action));
  }

  /** Cria exceção para self-approval */
  public static BusinessRuleException selfApprovalNotAllowed() {
    return new BusinessRuleException(
        "SELF_APPROVAL_NOT_ALLOWED", "Usuário não pode aprovar sua própria solicitação");
  }
}
