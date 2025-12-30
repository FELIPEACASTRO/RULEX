package com.rulex.controller;

import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.dto.TransactionRequest;
import com.rulex.service.RuleSimulationService;
import com.rulex.service.RuleSimulationService.BacktestResult;
import com.rulex.service.RuleSimulationService.ComparisonResult;
import com.rulex.service.RuleSimulationService.SimulationResult;
import jakarta.validation.Valid;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Controller para simulação e teste de regras.
 * Permite testar regras antes de ativá-las.
 */
@RestController
@RequestMapping("/rules/simulation")
@RequiredArgsConstructor
@Slf4j
public class RuleSimulationController {

  private final RuleSimulationService simulationService;

  /**
   * Simula uma regra contra um payload de teste.
   * POST /api/rules/simulation/test
   */
  @PostMapping("/test")
  public ResponseEntity<SimulationResult> simulateRule(@Valid @RequestBody SimulationRequest request) {
    log.info("Simulando regra: {}", request.getRule().getRuleName());

    SimulationResult result = simulationService.simulateRule(request.getRule(), request.getTestPayload());
    return ResponseEntity.ok(result);
  }

  /**
   * Faz backtesting de uma regra existente.
   * POST /api/rules/simulation/backtest/{ruleId}
   */
  @PostMapping("/backtest/{ruleId}")
  public ResponseEntity<BacktestResult> backtestRule(
      @PathVariable Long ruleId,
      @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime startDate,
      @RequestParam @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME) LocalDateTime endDate,
      @RequestParam(defaultValue = "1000") int sampleSize) {

    log.info("Backtesting regra {} de {} a {}", ruleId, startDate, endDate);

    BacktestResult result = simulationService.backtestRule(ruleId, startDate, endDate, sampleSize);
    return ResponseEntity.ok(result);
  }

  /**
   * Compara duas regras.
   * POST /api/rules/simulation/compare
   */
  @PostMapping("/compare")
  public ResponseEntity<ComparisonResult> compareRules(@Valid @RequestBody ComparisonRequest request) {
    log.info("Comparando regras: {} vs {}", request.getRuleA().getRuleName(), request.getRuleB().getRuleName());

    ComparisonResult result = simulationService.compareRules(
        request.getRuleA(), 
        request.getRuleB(), 
        request.getTestPayloads());
    return ResponseEntity.ok(result);
  }

  /**
   * Simula múltiplas regras contra um payload.
   * POST /api/rules/simulation/batch
   */
  @PostMapping("/batch")
  public ResponseEntity<List<SimulationResult>> simulateBatch(@Valid @RequestBody BatchSimulationRequest request) {
    log.info("Simulando {} regras contra payload", request.getRules().size());

    List<SimulationResult> results = request.getRules().stream()
        .map(rule -> simulationService.simulateRule(rule, request.getTestPayload()))
        .toList();

    return ResponseEntity.ok(results);
  }

  // Request DTOs

  @Data
  public static class SimulationRequest {
    @Valid
    private RuleConfigurationDTO rule;
    @Valid
    private TransactionRequest testPayload;
  }

  @Data
  public static class ComparisonRequest {
    @Valid
    private RuleConfigurationDTO ruleA;
    @Valid
    private RuleConfigurationDTO ruleB;
    @Valid
    private List<TransactionRequest> testPayloads;
  }

  @Data
  public static class BatchSimulationRequest {
    @Valid
    private List<RuleConfigurationDTO> rules;
    @Valid
    private TransactionRequest testPayload;
  }
}
