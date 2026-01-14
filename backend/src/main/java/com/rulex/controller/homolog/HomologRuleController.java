package com.rulex.controller.homolog;

import com.rulex.api.NotFoundException;
import com.rulex.dto.homolog.CreateRuleRequest;
import com.rulex.dto.homolog.RuleVersionResponse;
import com.rulex.homolog.application.HomologRuleApplicationService;
import jakarta.validation.Valid;
import java.util.UUID;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/homolog/rules")
public class HomologRuleController {

  private final HomologRuleApplicationService homologRuleApplicationService;

  public HomologRuleController(HomologRuleApplicationService homologRuleApplicationService) {
    this.homologRuleApplicationService = homologRuleApplicationService;
  }

  @PostMapping
  public ResponseEntity<RuleVersionResponse> create(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @Valid @RequestBody CreateRuleRequest req) {
    return ResponseEntity.ok(homologRuleApplicationService.create(actorEmail, req));
  }

  @GetMapping("/{ruleId}/latest")
  public ResponseEntity<RuleVersionResponse> getLatest(@PathVariable UUID ruleId) {
    RuleVersionResponse latest =
        homologRuleApplicationService
            .getLatest(ruleId)
            .orElseThrow(() -> new NotFoundException("RuleId não encontrado"));
    return ResponseEntity.ok(latest);
  }

  @PostMapping("/versions/{ruleVersionId}/publish")
  public ResponseEntity<RuleVersionResponse> publish(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @PathVariable UUID ruleVersionId) {
    return ResponseEntity.ok(homologRuleApplicationService.publish(actorEmail, ruleVersionId));
  }

  @PostMapping("/{ruleId}/rollback/{version}")
  public ResponseEntity<RuleVersionResponse> rollback(
      @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
      @PathVariable UUID ruleId,
      @PathVariable int version) {
    return ResponseEntity.ok(
        homologRuleApplicationService.rollbackToVersion(actorEmail, ruleId, version));
  }
}
