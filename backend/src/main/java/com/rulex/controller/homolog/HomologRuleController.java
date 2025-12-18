package com.rulex.controller.homolog;

import com.rulex.dto.homolog.CreateRuleRequest;
import com.rulex.dto.homolog.RuleVersionResponse;
import com.rulex.homolog.HomologRuleService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/homolog/rules")
public class HomologRuleController {

    private final HomologRuleService homologRuleService;

    public HomologRuleController(HomologRuleService homologRuleService) {
        this.homologRuleService = homologRuleService;
    }

    @PostMapping
    public ResponseEntity<RuleVersionResponse> create(
            @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
            @Valid @RequestBody CreateRuleRequest req
    ) {
        return ResponseEntity.ok(homologRuleService.create(actorEmail, req));
    }

    @GetMapping("/{ruleId}/latest")
    public ResponseEntity<RuleVersionResponse> getLatest(@PathVariable UUID ruleId) {
        return homologRuleService.getLatest(ruleId)
                .map(ResponseEntity::ok)
                .orElseGet(() -> ResponseEntity.notFound().build());
    }

    @PostMapping("/versions/{ruleVersionId}/publish")
    public ResponseEntity<RuleVersionResponse> publish(
            @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
            @PathVariable UUID ruleVersionId
    ) {
        return ResponseEntity.ok(homologRuleService.publish(actorEmail, ruleVersionId));
    }

    @PostMapping("/{ruleId}/rollback/{version}")
    public ResponseEntity<RuleVersionResponse> rollback(
            @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
            @PathVariable UUID ruleId,
            @PathVariable int version
    ) {
        return ResponseEntity.ok(homologRuleService.rollbackToVersion(actorEmail, ruleId, version));
    }
}
