package com.rulex.dto.validation;

/**
 * QUAL-001 FIX: Grupos de validacao para TransactionRequest.
 *
 * <p>Permite validacao condicional baseada no contexto de uso: - BasicValidation: Campos minimos
 * para qualquer operacao - RuleEvaluationValidation: Campos necessarios para avaliacao de regras de
 * fraude - SimulationValidation: Campos para simulacao (menos restritivo)
 */
public final class ValidationGroups {

  private ValidationGroups() {
    // Utility class
  }

  /** Validacao basica - campos minimos obrigatorios. Usado para operacoes simples como consulta. */
  public interface BasicValidation {}

  /**
   * Validacao para avaliacao de regras de fraude. Inclui campos criticos para deteccao de fraude.
   * Extende BasicValidation.
   */
  public interface RuleEvaluationValidation extends BasicValidation {}

  /** Validacao para simulacao de regras. Menos restritivo, permite campos opcionais. */
  public interface SimulationValidation {}

  /** Validacao completa - todos os campos. Usado para persistencia e auditoria. */
  public interface FullValidation extends RuleEvaluationValidation {}
}
