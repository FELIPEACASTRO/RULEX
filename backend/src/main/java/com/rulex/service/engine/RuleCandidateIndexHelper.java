package com.rulex.service.engine;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.entity.RuleConfiguration;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class RuleCandidateIndexHelper {

  private final ObjectMapper objectMapper;
  private final ConditionMatcher conditionMatcher;

  private volatile CandidateIndexCache candidateIndexCache;

  public CandidateIndex buildOrReuseCandidateIndex(List<RuleConfiguration> enabledRules) {
    if (enabledRules == null || enabledRules.isEmpty()) {
      return CandidateIndex.EMPTY;
    }

    LocalDateTime maxUpdatedAt = null;
    int size = enabledRules.size();
    for (RuleConfiguration r : enabledRules) {
      if (r != null && r.getUpdatedAt() != null) {
        if (maxUpdatedAt == null || r.getUpdatedAt().isAfter(maxUpdatedAt)) {
          maxUpdatedAt = r.getUpdatedAt();
        }
      }
    }
    CandidateIndexSignature sig = new CandidateIndexSignature(size, maxUpdatedAt);

    CandidateIndexCache cached = candidateIndexCache;
    if (cached != null && cached.signature.equals(sig)) {
      return cached.index;
    }

    CandidateIndex built = buildCandidateIndex(enabledRules);
    candidateIndexCache = new CandidateIndexCache(sig, built);
    return built;
  }

  public List<RuleConditionDTO> readConditions(String conditionsJson) {
    try {
      if (conditionsJson == null || conditionsJson.isBlank()) {
        return List.of();
      }
      return objectMapper.readValue(
          conditionsJson,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, RuleConditionDTO.class));
    } catch (Exception e) {
      return List.of();
    }
  }

  private CandidateIndex buildCandidateIndex(List<RuleConfiguration> enabledRules) {
    Map<String, RulePreconditions> out = new HashMap<>();
    for (RuleConfiguration rule : enabledRules) {
      if (rule == null || rule.getRuleName() == null) continue;

      String cj = rule.getConditionsJson();
      if (cj == null || cj.isBlank()) {
        continue;
      }

      List<RuleConditionDTO> conditions = readConditions(cj);
      if (conditions.isEmpty()) {
        continue;
      }

      RuleConfiguration.LogicOperator op =
          rule.getLogicOperator() != null
              ? rule.getLogicOperator()
              : RuleConfiguration.LogicOperator.AND;

      Set<String> required = new HashSet<>();
      boolean hasIsNull = false;

      for (RuleConditionDTO c : conditions) {
        if (c == null) continue;
        String operator = conditionMatcher.normalizeOperator(c.getOperator());
        if ("IS_NULL".equals(operator)) {
          hasIsNull = true;
          continue;
        }
        required.addAll(extractFieldDependencies(c.getField()));
      }

      // Conservative skipping:
      // - AND rules: safe to skip when any required field is missing (would evaluate to false)
      // - OR rules: do not skip (could still trigger via IS_NULL or any present condition)
      boolean canSkip = (op == RuleConfiguration.LogicOperator.AND);
      if (op == RuleConfiguration.LogicOperator.OR && hasIsNull) {
        canSkip = false;
      }

      out.put(rule.getRuleName(), new RulePreconditions(required, canSkip));
    }
    return new CandidateIndex(Collections.unmodifiableMap(out));
  }

  private Set<String> extractFieldDependencies(String fieldExpr) {
    if (fieldExpr == null) return Set.of();
    String raw = fieldExpr.trim();
    if (raw.isEmpty()) return Set.of();

    java.util.regex.Matcher unary =
        java.util.regex.Pattern.compile("^(ABS|LEN|LOWER|UPPER|TRIM)\\(([A-Za-z0-9_]+)\\)$")
            .matcher(raw);
    if (unary.matches()) {
      return Set.of(unary.group(2));
    }

    java.util.regex.Matcher absExpr =
        java.util.regex.Pattern.compile("^ABS\\((.+)\\)$").matcher(raw);
    if (absExpr.matches()) {
      return extractDepsFromNumericExpr(absExpr.group(1));
    }

    java.util.regex.Matcher absDiff =
        java.util.regex.Pattern.compile(
                "^ABS_DIFF\\(([A-Za-z0-9_]+)\\s*,\\s*([A-Za-z0-9_]+)\\)$")
            .matcher(raw);
    if (absDiff.matches()) {
      return Set.of(absDiff.group(1), absDiff.group(2));
    }

    java.util.regex.Matcher coalesce =
        java.util.regex.Pattern.compile("^COALESCE\\(([A-Za-z0-9_]+)\\s*,\\s*(.+)\\)$")
            .matcher(raw);
    if (coalesce.matches()) {
      return Set.of(coalesce.group(1));
    }

    return Set.of(raw);
  }

  private Set<String> extractDepsFromNumericExpr(String expr) {
    if (expr == null) return Set.of();
    String e = expr.trim();
    if (e.isEmpty()) return Set.of();

    int minus = indexOfTopLevelMinus(e);
    if (minus > 0) {
      String left = e.substring(0, minus).trim();
      String right = e.substring(minus + 1).trim();
      Set<String> out = new HashSet<>();
      String a = depToken(left);
      String b = depToken(right);
      if (a != null) out.add(a);
      if (b != null) out.add(b);
      return out;
    }

    String single = depToken(e);
    return single == null ? Set.of() : Set.of(single);
  }

  private int indexOfTopLevelMinus(String e) {
    // Evita confundir número negativo "-5" no primeiro char.
    for (int i = 1; i < e.length(); i++) {
      if (e.charAt(i) == '-') return i;
    }
    return -1;
  }

  private String depToken(String token) {
    if (token == null) return null;
    String t = token.trim();
    if (t.isEmpty()) return null;
    try {
      new BigDecimal(t);
      return null;
    } catch (Exception e) { // SEC-006 FIX
      return t;
    }
  }

  public record CandidateIndex(Map<String, RulePreconditions> byRuleName) {
    public static final CandidateIndex EMPTY = new CandidateIndex(Map.of());
  }

  public record RulePreconditions(Set<String> requiredFields, boolean canSkipWithMissingFields) {}

  private record CandidateIndexSignature(int ruleCount, LocalDateTime maxUpdatedAt) {}

  private record CandidateIndexCache(CandidateIndexSignature signature, CandidateIndex index) {}
}
