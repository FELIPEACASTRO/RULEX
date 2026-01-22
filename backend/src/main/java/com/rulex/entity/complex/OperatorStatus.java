package com.rulex.entity.complex;

/**
 * Status de implementação de um operador.
 *
 * <p>Usado para informar aos consumidores da API quais operadores estão disponíveis para uso em
 * regras.
 */
public enum OperatorStatus {

  /** Operador totalmente implementado e testado - pronto para produção */
  STABLE("Estável", "Operador totalmente implementado e testado"),

  /** Operador implementado mas em fase de validação - usar com cautela */
  BETA("Beta", "Operador implementado mas em validação"),

  /** Operador declarado mas NÃO implementado - lança exceção se usado */
  PLANNED("Planejado", "Operador planejado para implementação futura"),

  /** Operador será removido em versão futura - migrar para alternativa */
  DEPRECATED("Deprecado", "Operador será removido em versão futura");

  private final String label;
  private final String description;

  OperatorStatus(String label, String description) {
    this.label = label;
    this.description = description;
  }

  public String getLabel() {
    return label;
  }

  public String getDescription() {
    return description;
  }
}
