package com.rulex.controller.homolog;

import com.rulex.dto.homolog.*;
import com.rulex.homolog.application.HomologRuleSetApplicationService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/homolog/rulesets")
public class HomologRuleSetController {

  private final HomologRuleSetApplicationService ruleSetApplicationService;

  public HomologRuleSetController(HomologRuleSetApplicationService ruleSetApplicationService) {
    this.ruleSetApplicationService = ruleSetApplicationService;
  }

  @PostMapping
  public ResponseEntity<RuleSetVersionResponse> createDraft(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @Valid @RequestBody CreateRuleSetRequest req) {
    return ResponseEntity.ok(ruleSetApplicationService.createDraft(actorEmail, req));
  }

  @PostMapping("/versions/{ruleSetVersionId}/publish")
  public ResponseEntity<RuleSetVersionResponse> publish(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @PathVariable java.util.UUID ruleSetVersionId) {
    return ResponseEntity.ok(ruleSetApplicationService.publish(actorEmail, ruleSetVersionId));
  }

  @PostMapping("/activate")
  public ResponseEntity<Void> activate(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @Valid @RequestBody ActivateRuleSetRequest req) {
    ruleSetApplicationService.activate(actorEmail, req.ruleSetVersionId());
    return ResponseEntity.ok().build();
  }
}
