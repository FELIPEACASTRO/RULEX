package com.rulex.controller.homolog;

import com.rulex.dto.homolog.SimulationRequest;
import com.rulex.dto.homolog.SimulationResponse;
import com.rulex.homolog.application.HomologRuleSetApplicationService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/homolog/simulations")
public class HomologSimulationController {

  private final HomologRuleSetApplicationService ruleSetApplicationService;

  public HomologSimulationController(HomologRuleSetApplicationService ruleSetApplicationService) {
    this.ruleSetApplicationService = ruleSetApplicationService;
  }

  @PostMapping("/run")
  public ResponseEntity<SimulationResponse> run(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @Valid @RequestBody SimulationRequest req) {
    return ResponseEntity.ok(
        ruleSetApplicationService.simulate(actorEmail, req.ruleSetVersionId(), req.payload()));
  }
}
