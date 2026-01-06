package com.rulex.domain.exception;

import java.io.Serial;
import java.util.ArrayList;
import java.util.List;

/**
 * Exceção lançada quando o payload viola o contrato esperado.
 *
 * <p>Ocorre quando campos obrigatórios estão ausentes ou malformados. A transação deve ser
 * classificada como SUSPICIOUS ou FRAUD dependendo da severidade.
 */
public class ContractViolationException extends DomainException {

  @Serial private static final long serialVersionUID = 1L;

  private final ArrayList<String> missingFields;
  private final ArrayList<String> violations;
  private final String detail;

  public ContractViolationException(String detail) {
    super("CONTRACT_VIOLATION", "Violação de contrato: " + detail);
    this.missingFields = new ArrayList<>();
    this.violations = new ArrayList<>();
    this.violations.add(detail);
    this.detail = detail;
  }

  public ContractViolationException(List<String> missingFields) {
    super(
        "CONTRACT_MISSING_FIELDS",
        "Campos obrigatórios ausentes: " + String.join(", ", missingFields));
    this.missingFields = new ArrayList<>(missingFields);
    this.violations =
        new ArrayList<>(missingFields.stream().map(f -> "Campo ausente: " + f).toList());
    this.detail = "Campos obrigatórios ausentes para persistência";
  }

  public ContractViolationException(String code, String detail) {
    super(code, detail);
    this.missingFields = new ArrayList<>();
    this.violations = new ArrayList<>();
    this.violations.add(detail);
    this.detail = detail;
  }

  public List<String> getMissingFields() {
    return missingFields;
  }

  public List<String> getViolations() {
    return violations;
  }

  public String getDetail() {
    return detail;
  }

  /** Verifica se é erro de JSON inválido */
  public boolean isInvalidJson() {
    return "CONTRACT_INVALID_JSON".equals(getErrorCode());
  }

  /** Verifica se é erro de campos faltantes */
  public boolean isMissingFields() {
    return "CONTRACT_MISSING_FIELDS".equals(getErrorCode());
  }

  /** Cria exceção para JSON inválido */
  public static ContractViolationException invalidJson(String detail) {
    return new ContractViolationException(
        "CONTRACT_INVALID_JSON", "Payload JSON inválido: " + detail);
  }

  /** Cria exceção para externalTransactionId ausente */
  public static ContractViolationException missingExternalTransactionId() {
    return new ContractViolationException(
        "CONTRACT_MISSING_EXTERNAL_TRANSACTION_ID", "externalTransactionId ausente ou vazio");
  }
}
