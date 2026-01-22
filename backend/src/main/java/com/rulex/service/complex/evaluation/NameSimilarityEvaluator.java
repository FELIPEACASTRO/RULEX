package com.rulex.service.complex.evaluation;

import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.service.complex.context.FieldValueExtractor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public final class NameSimilarityEvaluator {

  private NameSimilarityEvaluator() {}

  public static boolean evaluateNameSimilarityLt(
      RuleCondition condition, EvaluationContext context) {
    try {
      String[] parts = condition.getValueSingle().split(":");
      if (parts.length < 2) {
        log.warn("Formato inválido para NAME_SIMILARITY_LT. Esperado: otherField:threshold");
        return false;
      }

      String otherField = parts[0].trim();
      int threshold = Integer.parseInt(parts[1].trim());

      Object fieldValue =
          FieldValueExtractor.getFieldValue(condition.getFieldName(), null, context);
      Object otherValue = FieldValueExtractor.getFieldValue(otherField, null, context);

      if (fieldValue == null || otherValue == null) {
        return false;
      }

      String name1 = String.valueOf(fieldValue).toLowerCase().trim();
      String name2 = String.valueOf(otherValue).toLowerCase().trim();

      int similarity = calculateSimilarity(name1, name2);
      return similarity < threshold;
    } catch (Exception e) {
      log.error("Erro ao avaliar NAME_SIMILARITY_LT: {}", e.getMessage());
      return false;
    }
  }

  public static boolean evaluateNameSimilarityGt(Object fieldValue, RuleCondition condition) {
    try {
      double similarity = Double.parseDouble(String.valueOf(fieldValue));
      double threshold = Double.parseDouble(condition.getValueSingle());
      return similarity > threshold;
    } catch (Exception e) {
      return false;
    }
  }

  /** Calcula similaridade entre duas strings usando distância de Levenshtein (0-100) */
  public static int calculateSimilarity(String s1, String s2) {
    if (s1.equals(s2)) return 100;
    if (s1.isEmpty() || s2.isEmpty()) return 0;

    int[][] dp = new int[s1.length() + 1][s2.length() + 1];

    for (int i = 0; i <= s1.length(); i++) dp[i][0] = i;
    for (int j = 0; j <= s2.length(); j++) dp[0][j] = j;

    for (int i = 1; i <= s1.length(); i++) {
      for (int j = 1; j <= s2.length(); j++) {
        int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;
        dp[i][j] = Math.min(Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1), dp[i - 1][j - 1] + cost);
      }
    }

    int maxLen = Math.max(s1.length(), s2.length());
    return (int) ((1.0 - (double) dp[s1.length()][s2.length()] / maxLen) * 100);
  }
}
