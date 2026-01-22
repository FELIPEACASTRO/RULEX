package com.rulex.controller;

import com.rulex.entity.complex.ConditionOperator;
import com.rulex.entity.complex.OperatorStatus;
import com.rulex.service.complex.evaluator.OperatorEvaluatorRegistry;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Controller para consulta de status dos operadores.
 *
 * <p>Permite verificar quais operadores estão implementados (STABLE) e quais estão planejados
 * (PLANNED).
 */
@RestController
@RequestMapping("/operators")
@RequiredArgsConstructor
@Tag(name = "Operadores", description = "Informações sobre operadores disponíveis")
public class OperatorStatusController {

  private final OperatorEvaluatorRegistry registry;

  @GetMapping("/status")
  @Operation(
      summary = "Lista todos os operadores e seus status",
      description =
          "Retorna estatísticas e lista de operadores agrupados por status (STABLE, PLANNED, etc.)")
  @ApiResponses({
    @ApiResponse(responseCode = "200", description = "Lista de operadores retornada com sucesso")
  })
  public ResponseEntity<Map<String, Object>> getOperatorStatus() {
    Map<String, Object> response = new HashMap<>();

    // Estatísticas gerais
    int total = ConditionOperator.values().length;
    Map<OperatorStatus, List<String>> byStatus = new EnumMap<>(OperatorStatus.class);

    for (ConditionOperator op : ConditionOperator.values()) {
      OperatorStatus status = registry.getStatus(op);
      byStatus.computeIfAbsent(status, k -> new ArrayList<>()).add(op.name());
    }

    response.put("total", total);
    response.put("byStatus", byStatus);
    response.put("stable", byStatus.getOrDefault(OperatorStatus.STABLE, List.of()).size());
    response.put("beta", byStatus.getOrDefault(OperatorStatus.BETA, List.of()).size());
    response.put("planned", byStatus.getOrDefault(OperatorStatus.PLANNED, List.of()).size());
    response.put("deprecated", byStatus.getOrDefault(OperatorStatus.DEPRECATED, List.of()).size());

    // Agrupar por avaliador
    response.put("byEvaluator", registry.getOperatorsByEvaluator());

    return ResponseEntity.ok(response);
  }

  @GetMapping("/status/{operator}")
  @Operation(
      summary = "Retorna status de um operador específico",
      description =
          "Retorna detalhes sobre um operador, incluindo status, avaliador e se está implementado")
  @ApiResponses({
    @ApiResponse(responseCode = "200", description = "Detalhes do operador"),
    @ApiResponse(responseCode = "404", description = "Operador não encontrado")
  })
  public ResponseEntity<Map<String, Object>> getOperatorDetail(@PathVariable String operator) {
    try {
      ConditionOperator op = ConditionOperator.valueOf(operator.toUpperCase());

      Map<String, Object> detail = new HashMap<>();
      detail.put("operator", op.name());
      detail.put("status", registry.getStatus(op));
      detail.put("statusDescription", registry.getStatus(op).getDescription());
      detail.put("isImplemented", registry.isImplemented(op));

      try {
        var evaluator = registry.getEvaluator(op);
        detail.put("evaluator", evaluator.getClass().getSimpleName());
        detail.put("category", evaluator.getCategory());
      } catch (Exception e) {
        detail.put("evaluator", "NONE");
        detail.put("category", "UNKNOWN");
      }

      return ResponseEntity.ok(detail);
    } catch (IllegalArgumentException e) {
      return ResponseEntity.notFound().build();
    }
  }

  @GetMapping("/list")
  @Operation(
      summary = "Lista operadores com filtros",
      description = "Lista operadores filtrados por status ou categoria")
  public ResponseEntity<Map<String, Object>> listOperators(
      @RequestParam(required = false) String status,
      @RequestParam(required = false) String category,
      @RequestParam(required = false, defaultValue = "false") boolean implementedOnly) {

    List<Map<String, Object>> operators = new ArrayList<>();

    for (ConditionOperator op : ConditionOperator.values()) {
      OperatorStatus opStatus = registry.getStatus(op);

      // Filtrar por status
      if (status != null && !opStatus.name().equalsIgnoreCase(status)) {
        continue;
      }

      // Filtrar por implementado
      if (implementedOnly && !registry.isImplemented(op)) {
        continue;
      }

      String evaluatorName = "NONE";
      String opCategory = "UNKNOWN";
      try {
        var evaluator = registry.getEvaluator(op);
        evaluatorName = evaluator.getClass().getSimpleName();
        opCategory = evaluator.getCategory();
      } catch (Exception ignored) {
      }

      // Filtrar por categoria
      if (category != null && !opCategory.equalsIgnoreCase(category)) {
        continue;
      }

      Map<String, Object> opInfo = new HashMap<>();
      opInfo.put("name", op.name());
      opInfo.put("status", opStatus.name());
      opInfo.put("category", opCategory);
      opInfo.put("evaluator", evaluatorName);
      opInfo.put("implemented", registry.isImplemented(op));

      operators.add(opInfo);
    }

    Map<String, Object> response = new HashMap<>();
    response.put("count", operators.size());
    response.put("operators", operators);

    return ResponseEntity.ok(response);
  }

  @GetMapping("/categories")
  @Operation(
      summary = "Lista categorias de operadores",
      description = "Retorna lista de categorias disponíveis com contagem de operadores")
  public ResponseEntity<Map<String, Object>> getCategories() {
    Map<String, Integer> categories = new HashMap<>();

    for (ConditionOperator op : ConditionOperator.values()) {
      try {
        var evaluator = registry.getEvaluator(op);
        String category = evaluator.getCategory();
        categories.merge(category, 1, Integer::sum);
      } catch (Exception e) {
        categories.merge("UNREGISTERED", 1, Integer::sum);
      }
    }

    Map<String, Object> response = new HashMap<>();
    response.put("categories", categories);
    response.put("totalCategories", categories.size());

    return ResponseEntity.ok(response);
  }
}
