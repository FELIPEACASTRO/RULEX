package com.rulex.golden;

import static org.junit.jupiter.api.Assertions.*;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.rulex.dto.complex.ConditionDTO.OperatorType;
import com.rulex.entity.complex.ConditionOperator;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Map;
import java.util.TreeMap;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Golden Master / Baseline Tests for Rule Engine.
 *
 * <p>These tests capture the expected behavior of the rule engine and detect any regressions. If
 * the baseline changes intentionally, update the baseline file and document the change.
 */
@DisplayName("Golden Master Baseline Tests")
class GoldenMasterBaselineTest {

  private static final Path BASELINE_DIR = Paths.get("src/test/resources/golden-master");
  private static final ObjectMapper MAPPER =
      new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);

  @BeforeAll
  static void setup() throws IOException {
    Files.createDirectories(BASELINE_DIR);
  }

  @Test
  @DisplayName("Operator inventory should match baseline")
  void operatorInventoryShouldMatchBaseline() throws IOException {
    // Capture current state
    Map<String, Object> currentState = new TreeMap<>();

    // Entity operators
    ConditionOperator[] entityOperators = ConditionOperator.values();
    currentState.put("entityOperatorCount", entityOperators.length);
    currentState.put(
        "entityOperators", Arrays.stream(entityOperators).map(Enum::name).sorted().toList());

    // DTO operators
    OperatorType[] dtoOperators = OperatorType.values();
    currentState.put("dtoOperatorCount", dtoOperators.length);
    currentState.put("dtoOperators", Arrays.stream(dtoOperators).map(Enum::name).sorted().toList());

    // Sync check
    currentState.put("operatorsInSync", entityOperators.length == dtoOperators.length);

    Path baselinePath = BASELINE_DIR.resolve("operator-inventory.json");
    String currentJson = MAPPER.writeValueAsString(currentState);

    if (Files.exists(baselinePath)) {
        String baselineJson = Files.readString(baselinePath);
        Map<String, Object> baselineState =
          MAPPER.readValue(baselineJson, new TypeReference<Map<String, Object>>() {});
        Map<String, Object> currentStateNormalized =
          MAPPER.readValue(currentJson, new TypeReference<Map<String, Object>>() {});
        assertEquals(
          baselineState,
          currentStateNormalized,
          "Operator inventory changed! If intentional, update baseline and document in CHANGELOG.");
    } else {
      // Create baseline
      Files.writeString(baselinePath, currentJson);
      System.out.println("Baseline created at: " + baselinePath);
    }
  }

  @Test
  @DisplayName("Operator categories should match baseline")
  void operatorCategoriesShouldMatchBaseline() throws IOException {
    Map<String, Integer> categories = new TreeMap<>();

    for (ConditionOperator op : ConditionOperator.values()) {
      String name = op.name();
      String category = categorizeOperator(name);
      categories.merge(category, 1, Integer::sum);
    }

    Path baselinePath = BASELINE_DIR.resolve("operator-categories.json");
    String currentJson = MAPPER.writeValueAsString(categories);

    if (Files.exists(baselinePath)) {
        String baselineJson = Files.readString(baselinePath);
        Map<String, Integer> baselineCategories =
          MAPPER.readValue(baselineJson, new TypeReference<Map<String, Integer>>() {});
        Map<String, Integer> currentCategories =
          MAPPER.readValue(currentJson, new TypeReference<Map<String, Integer>>() {});
        assertEquals(
          baselineCategories,
          currentCategories,
          "Operator categories changed! If intentional, update baseline.");
    } else {
      Files.writeString(baselinePath, currentJson);
      System.out.println("Baseline created at: " + baselinePath);
    }
  }

  @Test
  @DisplayName("Critical operators should exist")
  void criticalOperatorsShouldExist() {
    // These operators are critical for fraud detection and must always exist
    // Using actual operator names from ConditionOperator enum
    String[] criticalOperators = {
      // Comparison operators
      "EQ",
      "NEQ",
      "GT",
      "GTE",
      "LT",
      "LTE",
      "BETWEEN",
      // List operators
      "IN",
      "NOT_IN",
      "CONTAINS",
      // String operators
      "STARTS_WITH",
      "ENDS_WITH",
      "REGEX",
      // Null checks
      "IS_NULL",
      "NOT_NULL",
      // Boolean checks
      "IS_TRUE",
      "IS_FALSE",
      // Velocity checks
      "VELOCITY_COUNT_GT",
      "VELOCITY_SUM_GT"
    };

    for (String opName : criticalOperators) {
      boolean exists =
          Arrays.stream(ConditionOperator.values()).anyMatch(op -> op.name().equals(opName));
      assertTrue(exists, "Critical operator missing: " + opName);
    }
  }

  @Test
  @DisplayName("Operator count should not decrease")
  void operatorCountShouldNotDecrease() {
    // Minimum expected operators (based on current implementation: 447)
    int minimumExpected = 440;
    int actual = ConditionOperator.values().length;

    assertTrue(
        actual >= minimumExpected,
        String.format(
            "Operator count decreased! Expected >= %d, got %d. "
                + "Removing operators may break existing rules.",
            minimumExpected, actual));
  }

  private String categorizeOperator(String name) {
    if (name.startsWith("VELOCITY_")) return "VELOCITY";
    if (name.startsWith("NEO4J_")) return "NEO4J";
    if (name.startsWith("GEO_")) return "GEO";
    if (name.startsWith("FATF_")) return "FATF";
    if (name.startsWith("DEVICE_")) return "DEVICE";
    if (name.startsWith("RISK_")) return "RISK";
    if (name.startsWith("AGG_")) return "AGGREGATION";
    if (name.contains("STRING") || name.contains("TEXT")) return "STRING";
    if (name.contains("DATE") || name.contains("TIME")) return "DATE_TIME";
    if (name.contains("LIST") || name.contains("ARRAY")) return "LIST";
    if (name.contains("NULL")) return "NULL_CHECK";
    if (name.contains("REGEX")) return "REGEX";
    return "COMPARISON";
  }
}
