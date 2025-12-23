package com.rulex.v31.field;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import java.lang.reflect.Field;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * Seeds a baseline field_dictionary for CRTRAN25 so the FE can be fully catalog-driven.
 *
 * <p>This intentionally derives the catalog from the DTO to avoid hardcoding per-screen lists.
 */
@Component
@Slf4j
public class FieldDictionarySeeder implements ApplicationRunner {

  private static final String DEFAULT_WORKFLOW = "BRZLCREDIT";
  private static final String DEFAULT_RECORD_TYPE = "CRTRAN25";
  private static final String DEFAULT_PORTFOLIO = "*";

  private final FieldDictionaryRepository repository;
  private final ObjectMapper objectMapper;

  public FieldDictionarySeeder(FieldDictionaryRepository repository, ObjectMapper objectMapper) {
    this.repository = repository;
    this.objectMapper = objectMapper;
  }

  @Override
  public void run(ApplicationArguments args) {
    // Best-effort seed; never fail app startup.
    try {
      seedFromTransactionRequest();
    } catch (Exception e) {
      log.warn("FieldDictionary seed skipped due to error: {}", e.getMessage());
    }
  }

  private void seedFromTransactionRequest() {
    List<Field> fields = List.of(TransactionRequest.class.getDeclaredFields());

    for (Field f : fields) {
      JsonProperty jp = f.getAnnotation(JsonProperty.class);
      if (jp == null || jp.value() == null || jp.value().isBlank()) {
        continue;
      }

      String jsonPath = "$." + jp.value().trim();
      if (repository.existsByWorkflowAndRecordTypeAndPortfolioAndJsonPath(
          DEFAULT_WORKFLOW, DEFAULT_RECORD_TYPE, DEFAULT_PORTFOLIO, jsonPath)) {
        continue;
      }

      FieldDictionaryEntity e = new FieldDictionaryEntity();
      e.setId(UUID.randomUUID());
      e.setWorkflow(DEFAULT_WORKFLOW);
      e.setRecordType(DEFAULT_RECORD_TYPE);
      e.setPortfolio(DEFAULT_PORTFOLIO);
      e.setJsonPath(jsonPath);
      e.setDataType(mapType(f.getType()));
      e.setAllowedOperators(defaultOperators(e.getDataType()));
      e.setAllowedFunctions(defaultFunctions(e.getDataType()));
      e.setNormalizationAllowed(false);
      e.setSecurityConstraints(defaultSecurityConstraints(jp.value()));
      e.setCreatedAt(OffsetDateTime.now());
      repository.save(e);
    }

    log.info(
        "FieldDictionary seeded (best-effort) for workflow={}, recordType={}, portfolio={}",
        DEFAULT_WORKFLOW,
        DEFAULT_RECORD_TYPE,
        DEFAULT_PORTFOLIO);
  }

  private String mapType(Class<?> t) {
    if (t == null) {
      return "unknown";
    }
    if (t == String.class) {
      return "string";
    }
    if (Number.class.isAssignableFrom(t)
        || t == int.class
        || t == long.class
        || t == double.class
        || t == float.class) {
      return "number";
    }
    if (t == Boolean.class || t == boolean.class) {
      return "boolean";
    }
    return "unknown";
  }

  private String[] defaultOperators(String type) {
    String normalized = type == null ? "" : type.toLowerCase(Locale.ROOT);
    List<String> ops = new ArrayList<>();

    // Null operators are always allowed.
    ops.add("IS_NULL");
    ops.add("IS_NOT_NULL");

    switch (normalized) {
      case "number" ->
          ops.addAll(
              List.of(
                  "EQ", "NE", "GT", "LT", "GTE", "LTE", "IN", "NOT_IN", "BETWEEN", "NOT_BETWEEN"));
      case "boolean" -> ops.addAll(List.of("IS_TRUE", "IS_FALSE"));
      case "string" ->
          ops.addAll(
              List.of(
                  "EQ",
                  "NE",
                  "IN",
                  "NOT_IN",
                  "CONTAINS",
                  "NOT_CONTAINS",
                  "STARTS_WITH",
                  "ENDS_WITH",
                  "MATCHES_REGEX"));
      default -> ops.addAll(List.of("EQ", "NE"));
    }

    return ops.toArray(String[]::new);
  }

  private String[] defaultFunctions(String type) {
    String normalized = type == null ? "" : type.toLowerCase(Locale.ROOT);
    return switch (normalized) {
      case "string" -> new String[] {"TRIM", "LOWER", "UPPER", "LEN", "COALESCE"};
      case "number" -> new String[] {"ABS", "COALESCE"};
      default -> new String[] {"COALESCE"};
    };
  }

  private JsonNode defaultSecurityConstraints(String jsonFieldName) {
    if (jsonFieldName == null) {
      return null;
    }
    String f = jsonFieldName.trim().toLowerCase(Locale.ROOT);
    if (f.equals("pan") || f.contains("paymentinstrument")) {
      var node = objectMapper.createObjectNode();
      node.put("pci", true);
      node.put("mask", "PAN");
      node.put("neverLog", true);
      return node;
    }
    return null;
  }
}
