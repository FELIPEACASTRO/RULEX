package com.rulex.controller.homolog;

import com.rulex.dto.homolog.*;
import com.rulex.homolog.HomologRuleSetService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/homolog/rulesets")
public class HomologRuleSetController {

    private final HomologRuleSetService ruleSetService;

    public HomologRuleSetController(HomologRuleSetService ruleSetService) {
        this.ruleSetService = ruleSetService;
    }

    @PostMapping
    public ResponseEntity<RuleSetVersionResponse> createDraft(
            @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
            @Valid @RequestBody CreateRuleSetRequest req
    ) {
        return ResponseEntity.ok(ruleSetService.createDraft(actorEmail, req));
    }

    @PostMapping("/versions/{ruleSetVersionId}/publish")
    public ResponseEntity<RuleSetVersionResponse> publish(
            @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
            @PathVariable java.util.UUID ruleSetVersionId
    ) {
        return ResponseEntity.ok(ruleSetService.publish(actorEmail, ruleSetVersionId));
    }

    @PostMapping("/activate")
    public ResponseEntity<Void> activate(
            @RequestHeader(value = "X-Actor-Email", required = false) String actorEmail,
            @Valid @RequestBody ActivateRuleSetRequest req
    ) {
        ruleSetService.activate(actorEmail, req.ruleSetVersionId());
        return ResponseEntity.ok().build();
    }
}
