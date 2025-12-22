package com.rulex.v31.rules;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.homolog.DecisionOutcome;
import com.rulex.v31.ast.AstEvaluator;
import com.rulex.v31.ast.AstValidationResult;
import com.rulex.v31.ast.AstValidator;
import com.rulex.v31.execlog.RuleExecutionLogService;
import com.rulex.v31.features.FeatureProvider;
import com.rulex.v31.trace.FeatureUsed;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/rules")
public class RulesV31Controller {

  private final ObjectMapper objectMapper;
  private final AstValidator validator;
  private final AstEvaluator evaluator;
  private final RuleExecutionLogService ruleExecutionLogService;

  public RulesV31Controller(
      ObjectMapper objectMapper,
      FeatureProvider featureProvider,
      RuleExecutionLogService ruleExecutionLogService) {
    this.objectMapper = objectMapper;
    this.validator = new AstValidator();
    this.evaluator = new AstEvaluator(featureProvider);
    this.ruleExecutionLogService = ruleExecutionLogService;
  }

  @PostMapping("/validate")
  public ResponseEntity<AstValidationResult> validate(@RequestBody JsonNode body) {
    JsonNode ast = body == null ? null : body.get("ast");
    return ResponseEntity.ok(validator.validate(ast));
  }

  @PostMapping("/lint")
  public ResponseEntity<?> lint(@RequestBody JsonNode body) {
    // Minimal lint: reuse validator only, return warnings bucket for FE.
    AstValidationResult r = validator.validate(body == null ? null : body.get("ast"));
    Map<String, Object> resp = new LinkedHashMap<>();
    resp.put("valid", r.isValid());
    resp.put("errors", r.getErrors());
    resp.put("warnings", List.of());
    return ResponseEntity.ok(resp);
  }

  @PostMapping("/simulate")
  public ResponseEntity<?> simulate(@RequestBody JsonNode body) {
    JsonNode payload = body == null ? null : body.get("payload");
    JsonNode rules = body == null ? null : body.get("rules");

    if (payload == null) {
      return ResponseEntity.badRequest().body(Map.of("error", "payload obrigatório"));
    }
    if (rules == null || !rules.isArray()) {
      return ResponseEntity.badRequest().body(Map.of("error", "rules deve ser array"));
    }

    List<Map<String, Object>> fired = new ArrayList<>();
    List<Map<String, Object>> whyNot = new ArrayList<>();

    List<FeatureUsed> featuresUsedAll = new ArrayList<>();

    String finalDecision = "APROVADA";

    for (JsonNode rule : rules) {
      String ruleName = text(rule.get("ruleName"));
      String decision = normalizeDecision(text(rule.get("decision")));
      JsonNode ast = rule.get("ast");

      AstValidationResult vr = validator.validate(ast);
      if (!vr.isValid()) {
        whyNot.add(
            Map.of("ruleName", ruleName, "reason", "AST inválida", "errors", vr.getErrors()));
        continue;
      }

      AstEvaluator.EvaluationTraceResult trace = evaluator.evaluateWithTrace(ast, payload);
      boolean match = trace.match();
      if (trace.featuresUsed() != null) {
        featuresUsedAll.addAll(trace.featuresUsed());
      }
      if (match) {
        fired.add(Map.of("ruleName", ruleName, "decision", decision));
        finalDecision = mergeDecision(finalDecision, decision);
      } else {
        whyNot.add(Map.of("ruleName", ruleName, "reason", "Não casou"));
      }

      if ("FRAUDE".equals(finalDecision)) {
        break; // short-circuit
      }
    }

    Map<String, Object> decisionPath = new LinkedHashMap<>();
    decisionPath.put("strategy", "severity-only");
    decisionPath.put("firedCount", fired.size());

    Map<String, Object> resp = new LinkedHashMap<>();
    resp.put("status", finalDecision);
    resp.put("rulesFired", fired);
    resp.put("decisionPath", decisionPath);
    resp.put("whyNotFired", whyNot);
    resp.put("featuresUsed", featuresUsedAll);

    // Best-effort audit for simulation (no externalTransactionId/raw hash in this endpoint).
    try {
      ruleExecutionLogService.logSimulate(
          null,
          null,
          toOutcome(finalDecision),
          riskScoreFor(finalDecision),
          fired,
          whyNot,
          featuresUsedAll);
    } catch (Exception ignored) {
      // Never fail simulation because of audit.
    }

    return ResponseEntity.ok(resp);
  }

  private DecisionOutcome toOutcome(String status) {
    if ("FRAUDE".equals(status)) {
      return DecisionOutcome.FRAUDE;
    }
    if ("SUSPEITA_DE_FRAUDE".equals(status)) {
      return DecisionOutcome.SUSPEITA_DE_FRAUDE;
    }
    return DecisionOutcome.APROVADO;
  }

  private int riskScoreFor(String status) {
    if ("FRAUDE".equals(status)) {
      return 100;
    }
    if ("SUSPEITA_DE_FRAUDE".equals(status)) {
      return 50;
    }
    return 0;
  }

  private String text(JsonNode n) {
    if (n == null || n.isNull()) {
      return null;
    }
    return n.asText();
  }

  private String normalizeDecision(String d) {
    if (d == null) {
      return "APROVADA";
    }
    String x = d.trim().toUpperCase(Locale.ROOT);
    return switch (x) {
      case "FRAUDE" -> "FRAUDE";
      case "SUSPEITA_DE_FRAUDE", "SUSPEITA", "SUSPICIOUS" -> "SUSPEITA_DE_FRAUDE";
      case "APROVADA", "APPROVED" -> "APROVADA";
      default -> "APROVADA";
    };
  }

  private String mergeDecision(String current, String next) {
    if ("FRAUDE".equals(current) || "FRAUDE".equals(next)) {
      return "FRAUDE";
    }
    if ("SUSPEITA_DE_FRAUDE".equals(current) || "SUSPEITA_DE_FRAUDE".equals(next)) {
      return "SUSPEITA_DE_FRAUDE";
    }
    return "APROVADA";
  }
}
