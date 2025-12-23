package com.rulex.v31.field;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/field-dictionary")
public class FieldDictionaryController {

  private final FieldDictionaryService service;
  private final ObjectMapper objectMapper;

  public FieldDictionaryController(FieldDictionaryService service, ObjectMapper objectMapper) {
    this.service = service;
    this.objectMapper = objectMapper;
  }

  @GetMapping
  public ResponseEntity<?> list(
      @RequestParam(required = false) String workflow,
      @RequestParam(required = false) String recordType,
      @RequestParam(required = false) String portfolio) {

    List<FieldDictionaryEntity> items = service.list(workflow, recordType, portfolio);

    // Keep response shape stable for FE: serialize JSONB columns as JSON nodes, not raw strings.
    List<Map<String, Object>> response = new ArrayList<>(items.size());
    for (FieldDictionaryEntity f : items) {
      Map<String, Object> row = new LinkedHashMap<>();
      row.put("workflow", f.getWorkflow());
      row.put("recordType", f.getRecordType());
      row.put("portfolio", f.getPortfolio());
      row.put("jsonPath", f.getJsonPath());
      row.put("type", f.getDataType());
      row.put("domain", readJsonOrNull(f.getDomainJson()));
      row.put("sentinelValues", readJsonOrNull(f.getSentinelValuesJson()));
      row.put(
          "allowedOperators",
          f.getAllowedOperators() == null ? List.of() : List.of(f.getAllowedOperators()));
      row.put(
          "allowedFunctions",
          f.getAllowedFunctions() == null ? List.of() : List.of(f.getAllowedFunctions()));
      row.put("requirednessByContext", readJsonOrNull(f.getRequirednessByContext()));
      row.put("securityConstraints", readJsonOrNull(f.getSecurityConstraints()));
      row.put("normalizationAllowed", f.isNormalizationAllowed());
      response.add(row);
    }

    return ResponseEntity.ok(response);
  }

  private Object readJsonOrNull(Object json) {
    if (json == null) {
      return null;
    }
    if (json instanceof JsonNode node) {
      return node;
    }
    if (json instanceof String s) {
      if (s.isBlank()) {
        return null;
      }
      try {
        return objectMapper.readTree(s);
      } catch (Exception e) {
        return s;
      }
    }
    return json;
  }
}
