package com.rulex.service.complex.evaluator;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.RuleCondition;
import com.rulex.service.complex.ComplexRuleEvaluator.EvaluationContext;
import com.rulex.util.RegexValidator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * Avaliador para operadores de string.
 *
 * <p>Operadores suportados:
 *
 * <ul>
 *   <li>CONTAINS, NOT_CONTAINS - substring
 *   <li>STARTS_WITH, ENDS_WITH - prefixo/sufixo
 *   <li>REGEX_MATCH - expressão regular
 *   <li>LIKE, NOT_LIKE - padrão SQL LIKE
 *   <li>SOUNDEX_MATCH - similaridade fonética
 *   <li>LEVENSHTEIN_LT - distância de edição
 *   <li>LENGTH_EQ, LENGTH_GT, LENGTH_LT - tamanho
 * </ul>
 */
@Component
@Slf4j
public class StringOperatorEvaluator implements OperatorEvaluator {

  private static final Set<ConditionOperator> SUPPORTED =
      Set.of(
          ConditionOperator.CONTAINS,
          ConditionOperator.NOT_CONTAINS,
          ConditionOperator.STARTS_WITH,
          ConditionOperator.ENDS_WITH,
          ConditionOperator.REGEX,
          ConditionOperator.NOT_REGEX);

  @Override
  public Set<ConditionOperator> getSupportedOperators() {
    return SUPPORTED;
  }

  @Override
  public boolean evaluate(RuleCondition condition, EvaluationContext context) {
    ConditionOperator op = condition.getOperator();
    String fieldName = condition.getFieldName();
    Object fieldValue = getFieldValue(context, fieldName);

    log.debug("StringOperatorEvaluator: op={}, field={}, value={}", op, fieldName, fieldValue);

    if (fieldValue == null) {
      return false;
    }

    String fieldStr = String.valueOf(fieldValue);
    String expected = condition.getValueSingle();
    Boolean caseSensitive = condition.getCaseSensitive();

    return switch (op) {
      case CONTAINS -> evaluateContains(fieldStr, expected, caseSensitive);
      case NOT_CONTAINS -> !evaluateContains(fieldStr, expected, caseSensitive);
      case STARTS_WITH -> evaluateStartsWith(fieldStr, expected, caseSensitive);
      case ENDS_WITH -> evaluateEndsWith(fieldStr, expected, caseSensitive);
      case REGEX -> evaluateRegex(fieldStr, expected);
      case NOT_REGEX -> !evaluateRegex(fieldStr, expected);
      default -> false;
    };
  }

  private Object getFieldValue(EvaluationContext context, String fieldName) {
    if (context == null || fieldName == null) {
      return null;
    }

    Map<String, Object> payload = context.getPayload();
    if (payload != null && payload.containsKey(fieldName)) {
      return payload.get(fieldName);
    }

    if (context.getTransactionRequest() != null) {
      try {
        var request = context.getTransactionRequest();
        var field = request.getClass().getDeclaredField(fieldName);
        field.setAccessible(true);
        return field.get(request);
      } catch (Exception e) {
        log.trace("Campo {} não encontrado no TransactionRequest", fieldName);
      }
    }

    return null;
  }

  private boolean evaluateContains(String fieldValue, String substring, Boolean caseSensitive) {
    if (substring == null) return false;

    if (caseSensitive != null && caseSensitive) {
      return fieldValue.contains(substring);
    } else {
      return fieldValue.toLowerCase(Locale.ROOT).contains(substring.toLowerCase(Locale.ROOT));
    }
  }

  private boolean evaluateStartsWith(String fieldValue, String prefix, Boolean caseSensitive) {
    if (prefix == null) return false;

    if (caseSensitive != null && caseSensitive) {
      return fieldValue.startsWith(prefix);
    } else {
      return fieldValue.toLowerCase(Locale.ROOT).startsWith(prefix.toLowerCase(Locale.ROOT));
    }
  }

  private boolean evaluateEndsWith(String fieldValue, String suffix, Boolean caseSensitive) {
    if (suffix == null) return false;

    if (caseSensitive != null && caseSensitive) {
      return fieldValue.endsWith(suffix);
    } else {
      return fieldValue.toLowerCase(Locale.ROOT).endsWith(suffix.toLowerCase(Locale.ROOT));
    }
  }

  private boolean evaluateRegex(String fieldValue, String pattern) {
    if (pattern == null) return false;

    try {
      // GAP-C FIX: Usar RegexValidator.safeMatches() com timeout e validação anti-ReDoS
      return RegexValidator.safeMatches(pattern, fieldValue);
    } catch (Exception e) {
      log.warn("Erro ao avaliar regex '{}': {}", pattern, e.getMessage());
      return false;
    }
  }

  private boolean evaluateLike(String fieldValue, String pattern, Boolean caseSensitive) {
    if (pattern == null) return false;

    // Converter padrão SQL LIKE para regex
    String regex =
        pattern
            .replace(".", "\\.")
            .replace("*", ".*")
            .replace("%", ".*")
            .replace("_", ".")
            .replace("?", ".");

    try {
      // GAP-C FIX: Usar RegexValidator para LIKE também (regex gerado é seguro mas input pode ser
      // malicioso)
      String fullRegex =
          (caseSensitive != null && caseSensitive) ? "^" + regex + "$" : "(?i)^" + regex + "$";
      return RegexValidator.safeMatches(fullRegex, fieldValue);
    } catch (Exception e) {
      log.warn("Erro ao avaliar LIKE '{}': {}", pattern, e.getMessage());
      return false;
    }
  }

  private boolean evaluateSoundex(String fieldValue, String expected) {
    if (expected == null) return false;

    String soundex1 = soundex(fieldValue);
    String soundex2 = soundex(expected);
    return soundex1.equals(soundex2);
  }

  private String soundex(String s) {
    if (s == null || s.isEmpty()) return "";

    s = s.toUpperCase(Locale.ROOT);
    char first = s.charAt(0);

    StringBuilder sb = new StringBuilder();
    sb.append(first);

    char lastCode = soundexCode(first);
    for (int i = 1; i < s.length() && sb.length() < 4; i++) {
      char c = s.charAt(i);
      char code = soundexCode(c);
      if (code != '0' && code != lastCode) {
        sb.append(code);
      }
      lastCode = code;
    }

    while (sb.length() < 4) {
      sb.append('0');
    }

    return sb.toString();
  }

  private char soundexCode(char c) {
    return switch (c) {
      case 'B', 'F', 'P', 'V' -> '1';
      case 'C', 'G', 'J', 'K', 'Q', 'S', 'X', 'Z' -> '2';
      case 'D', 'T' -> '3';
      case 'L' -> '4';
      case 'M', 'N' -> '5';
      case 'R' -> '6';
      default -> '0';
    };
  }

  private boolean evaluateLevenshtein(String fieldValue, String expected, Integer threshold) {
    if (expected == null) return false;

    int distance = levenshteinDistance(fieldValue, expected);
    int maxDistance = threshold != null ? threshold : 3;
    return distance < maxDistance;
  }

  private int levenshteinDistance(String s1, String s2) {
    int[][] dp = new int[s1.length() + 1][s2.length() + 1];

    for (int i = 0; i <= s1.length(); i++) {
      dp[i][0] = i;
    }
    for (int j = 0; j <= s2.length(); j++) {
      dp[0][j] = j;
    }

    for (int i = 1; i <= s1.length(); i++) {
      for (int j = 1; j <= s2.length(); j++) {
        int cost = s1.charAt(i - 1) == s2.charAt(j - 1) ? 0 : 1;
        dp[i][j] = Math.min(Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1), dp[i - 1][j - 1] + cost);
      }
    }

    return dp[s1.length()][s2.length()];
  }

  private int parseIntSafe(String value, int defaultValue) {
    try {
      return Integer.parseInt(value);
    } catch (Exception e) {
      return defaultValue;
    }
  }

  @Override
  public String getCategory() {
    return "STRING";
  }
}
