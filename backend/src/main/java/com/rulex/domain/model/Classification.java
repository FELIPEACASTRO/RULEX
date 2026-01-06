package com.rulex.domain.model;

/**
 * Classificação de risco de uma transação.
 *
 * <p>Domain enum puro, sem dependências de framework.
 */
public enum Classification {

  /** Transação aprovada - baixo risco */
  APPROVED("Aprovada", 0),

  /** Transação suspeita - risco médio, requer análise */
  SUSPICIOUS("Suspeita de Fraude", 50),

  /** Fraude confirmada - alto risco, bloquear */
  FRAUD("Fraude", 100);

  private final String label;
  private final int baseScore;

  Classification(String label, int baseScore) {
    this.label = label;
    this.baseScore = baseScore;
  }

  public String getLabel() {
    return label;
  }

  public int getBaseScore() {
    return baseScore;
  }

  /** Verifica se a classificação bloqueia a transação */
  public boolean isBlocking() {
    return this == FRAUD;
  }

  /** Verifica se a classificação requer análise manual */
  public boolean requiresReview() {
    return this == SUSPICIOUS;
  }

  /** Retorna a classificação mais severa entre duas */
  public static Classification mostSevere(Classification a, Classification b) {
    if (a == null) return b;
    if (b == null) return a;
    return a.ordinal() > b.ordinal() ? a : b;
  }

  /** Converte de string (case-insensitive) */
  public static Classification fromString(String value) {
    if (value == null || value.isBlank()) {
      return APPROVED;
    }
    try {
      return Classification.valueOf(value.toUpperCase().trim());
    } catch (IllegalArgumentException e) {
      return APPROVED;
    }
  }
}
