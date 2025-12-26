package com.rulex.homolog.adapter;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.entity.homolog.LogicOperator;
import com.rulex.homolog.port.RuleDslEvaluatorPort;
import java.math.BigDecimal;
import java.time.OffsetDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import org.springframework.stereotype.Component;

@Component
public class RuleDslEvaluatorAdapter implements RuleDslEvaluatorPort {

  private final ObjectMapper objectMapper;

  public RuleDslEvaluatorAdapter(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  @Override
  public boolean evaluate(Map<String, Object> payload, LogicOperator logic, String conditionsJson) {
    List<RuleConditionDTO> conditions;
    try {
      conditions = objectMapper.readValue(conditionsJson, new TypeReference<>() {});
    } catch (Exception e) {
      throw new IllegalArgumentException("conditions_json inválido", e);
    }

    if (conditions == null || conditions.isEmpty()) {
      return false;
    }

    Map<String, Object> safePayload = payload == null ? Map.of() : payload;

    return switch (logic) {
      case AND -> conditions.stream().allMatch(c -> match(safePayload, c));
      case OR -> conditions.stream().anyMatch(c -> match(safePayload, c));
    };
  }

  private boolean match(Map<String, Object> payload, RuleConditionDTO c) {
    if (c == null) {
      return false;
    }
    String field = normalizeField(c.getField());

    Object actual = payload.get(field);
    String operator =
        (c.getOperator() == null) ? "" : c.getOperator().trim().toUpperCase(Locale.ROOT);
    String expectedRaw = c.getValue();

    return switch (operator) {
      case "EQ" -> Objects.equals(coerce(actual, expectedRaw), actual);
      case "NEQ" -> !Objects.equals(coerce(actual, expectedRaw), actual);
      case "GT" -> compare(actual, expectedRaw) > 0;
      case "GTE" -> compare(actual, expectedRaw) >= 0;
      case "LT" -> compare(actual, expectedRaw) < 0;
      case "LTE" -> compare(actual, expectedRaw) <= 0;
      case "IN" -> in(actual, expectedRaw, true);
      case "NOT_IN" -> in(actual, expectedRaw, false);
      case "CONTAINS" -> contains(actual, expectedRaw);
      case "STARTS_WITH" -> startsWith(actual, expectedRaw);
      case "ENDS_WITH" -> endsWith(actual, expectedRaw);
      case "IS_NULL" -> actual == null;
      case "NOT_NULL" -> actual != null;
      default -> throw new IllegalArgumentException("Operador não permitido: " + operator);
    };
  }

  private String normalizeField(String field) {
    if (field == null) {
      throw new IllegalArgumentException("field obrigatório");
    }
    String normalized = field.trim();
    if (!normalized.matches("[A-Za-z0-9_]+")) {
      throw new IllegalArgumentException("field inválido");
    }
    return normalized;
  }

  private Object coerce(Object actual, String expectedRaw) {
    if (actual == null) {
      return expectedRaw;
    }
    if (expectedRaw == null) {
      return null;
    }

    if (actual instanceof Integer) {
      try {
        return Integer.parseInt(expectedRaw);
      } catch (Exception e) {
        return expectedRaw;
      }
    }
    if (actual instanceof Long) {
      try {
        return Long.parseLong(expectedRaw);
      } catch (Exception e) {
        return expectedRaw;
      }
    }
    if (actual instanceof BigDecimal) {
      try {
        return new BigDecimal(expectedRaw);
      } catch (Exception e) {
        return expectedRaw;
      }
    }
    if (actual instanceof Boolean) {
      return Boolean.parseBoolean(expectedRaw);
    }
    if (actual instanceof OffsetDateTime) {
      try {
        return OffsetDateTime.parse(expectedRaw);
      } catch (Exception e) {
        return expectedRaw;
      }
    }
    return expectedRaw;
  }

  private int compare(Object actual, String expectedRaw) {
    if (actual == null || expectedRaw == null) {
      return -1;
    }

    if (actual instanceof BigDecimal bd) {
      return bd.compareTo(new BigDecimal(expectedRaw));
    }
    if (actual instanceof Integer i) {
      return Integer.compare(i, Integer.parseInt(expectedRaw));
    }
    if (actual instanceof Long l) {
      return Long.compare(l, Long.parseLong(expectedRaw));
    }
    if (actual instanceof String s) {
      return s.compareTo(expectedRaw);
    }
    throw new IllegalArgumentException(
        "Comparação não suportada para tipo: " + actual.getClass().getSimpleName());
  }

  private boolean in(Object actual, String expectedRaw, boolean positive) {
    if (expectedRaw == null) {
      return !positive;
    }
    Set<String> set = new HashSet<>();
    for (String part : expectedRaw.split(",")) {
      String v = part.trim();
      if (!v.isEmpty()) {
        set.add(v);
      }
    }
    String actualStr = (actual == null) ? null : String.valueOf(actual);
    boolean contains = actualStr != null && set.contains(actualStr);
    return positive == contains;
  }

  private boolean contains(Object actual, String expectedRaw) {
    if (!(actual instanceof String s) || expectedRaw == null) {
      return false;
    }
    return s.contains(expectedRaw);
  }

  private boolean startsWith(Object actual, String expectedRaw) {
    if (!(actual instanceof String s) || expectedRaw == null) {
      return false;
    }
    return s.startsWith(expectedRaw);
  }

  private boolean endsWith(Object actual, String expectedRaw) {
    if (!(actual instanceof String s) || expectedRaw == null) {
      return false;
    }
    return s.endsWith(expectedRaw);
  }
}
