package com.rulex.domain.service;

import com.rulex.domain.model.Classification;

/**
 * Domain Service para cálculo de score de risco.
 *
 * <p>Lógica pura de domínio sem dependências de framework.
 */
public class ScoreCalculator {

  /** Score mínimo (aprovação automática) */
  public static final int MIN_SCORE = 0;

  /** Score máximo (fraude confirmada) */
  public static final int MAX_SCORE = 100;

  /** Threshold para suspeita */
  public static final int SUSPICIOUS_THRESHOLD = 30;

  /** Threshold para fraude */
  public static final int FRAUD_THRESHOLD = 70;

  /**
   * Calcula score total a partir de contribuições individuais.
   *
   * @param contributions array de contribuições de cada regra
   * @return score normalizado (0-100)
   */
  public int calculateTotal(int... contributions) {
    int total = 0;
    for (int c : contributions) {
      total += clamp(c);
    }
    return clamp(total);
  }

  /**
   * Classifica baseado no score.
   *
   * @param score score de risco (0-100)
   * @return classificação correspondente
   */
  public Classification classifyByScore(int score) {
    if (score < SUSPICIOUS_THRESHOLD) {
      return Classification.APPROVED;
    }
    if (score < FRAUD_THRESHOLD) {
      return Classification.SUSPICIOUS;
    }
    return Classification.FRAUD;
  }

  /**
   * Calcula contribuição de uma regra para o score.
   *
   * @param weight peso da regra (0-100)
   * @param multiplier multiplicador opcional (ex: severidade)
   * @return contribuição normalizada
   */
  public int calculateContribution(int weight, double multiplier) {
    return clamp((int) (weight * multiplier));
  }

  /**
   * Normaliza valor para range 0-100.
   *
   * @param value valor a normalizar
   * @return valor entre 0 e 100
   */
  public int clamp(int value) {
    return Math.max(MIN_SCORE, Math.min(MAX_SCORE, value));
  }

  /**
   * Verifica se score indica aprovação.
   *
   * @param score score de risco
   * @return true se deve aprovar
   */
  public boolean shouldApprove(int score) {
    return score < SUSPICIOUS_THRESHOLD;
  }

  /**
   * Verifica se score indica bloqueio.
   *
   * @param score score de risco
   * @return true se deve bloquear
   */
  public boolean shouldBlock(int score) {
    return score >= FRAUD_THRESHOLD;
  }

  /**
   * Verifica se score indica revisão manual.
   *
   * @param score score de risco
   * @return true se deve revisar
   */
  public boolean shouldReview(int score) {
    return score >= SUSPICIOUS_THRESHOLD && score < FRAUD_THRESHOLD;
  }
}
