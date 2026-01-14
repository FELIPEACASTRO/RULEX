package com.rulex.homolog.usecase;

import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.dto.homolog.CreateRuleSetRequest;
import com.rulex.dto.homolog.RuleSetVersionResponse;
import com.rulex.dto.homolog.SimulationResponse;
import com.rulex.entity.homolog.*;
import com.rulex.homolog.port.ActiveRuleSetPersistencePort;
import com.rulex.homolog.port.ActorResolverPort;
import com.rulex.homolog.port.AuditPort;
import com.rulex.homolog.port.DecisionLogPersistencePort;
import com.rulex.homolog.port.JsonPort;
import com.rulex.homolog.port.PayloadSanitizerPort;
import com.rulex.homolog.port.RuleDslEvaluatorPort;
import com.rulex.homolog.port.RulePersistencePort;
import com.rulex.homolog.port.RuleSetPersistencePort;
import com.rulex.homolog.port.RuleSetVersionItemPersistencePort;
import com.rulex.homolog.port.RuleSetVersionPersistencePort;
import com.rulex.homolog.port.RuleVersionPersistencePort;
import com.rulex.homolog.port.SimulationRunPersistencePort;
import java.time.OffsetDateTime;
import java.util.*;

public class HomologRuleSetUseCase {

  private final RulePersistencePort rulePersistence;
  private final RuleSetPersistencePort ruleSetPersistence;
  private final RuleSetVersionPersistencePort ruleSetVersionPersistence;
  private final RuleSetVersionItemPersistencePort ruleSetVersionItemPersistence;
  private final RuleVersionPersistencePort ruleVersionPersistence;
  private final ActiveRuleSetPersistencePort activeRuleSetPersistence;
  private final DecisionLogPersistencePort decisionLogPersistence;
  private final SimulationRunPersistencePort simulationRunPersistence;
  private final RuleDslEvaluatorPort evaluator;
  private final PayloadSanitizerPort sanitizer;
  private final AuditPort audit;
  private final ActorResolverPort actorResolver;
  private final JsonPort json;

  public HomologRuleSetUseCase(
      RulePersistencePort rulePersistence,
      RuleSetPersistencePort ruleSetPersistence,
      RuleSetVersionPersistencePort ruleSetVersionPersistence,
      RuleSetVersionItemPersistencePort ruleSetVersionItemPersistence,
      RuleVersionPersistencePort ruleVersionPersistence,
      ActiveRuleSetPersistencePort activeRuleSetPersistence,
      DecisionLogPersistencePort decisionLogPersistence,
      SimulationRunPersistencePort simulationRunPersistence,
      RuleDslEvaluatorPort evaluator,
      PayloadSanitizerPort sanitizer,
      AuditPort audit,
      ActorResolverPort actorResolver,
      JsonPort json) {
    this.rulePersistence = rulePersistence;
    this.ruleSetPersistence = ruleSetPersistence;
    this.ruleSetVersionPersistence = ruleSetVersionPersistence;
    this.ruleSetVersionItemPersistence = ruleSetVersionItemPersistence;
    this.ruleVersionPersistence = ruleVersionPersistence;
    this.activeRuleSetPersistence = activeRuleSetPersistence;
    this.decisionLogPersistence = decisionLogPersistence;
    this.simulationRunPersistence = simulationRunPersistence;
    this.evaluator = evaluator;
    this.sanitizer = sanitizer;
    this.audit = audit;
    this.actorResolver = actorResolver;
    this.json = json;
  }

  public RuleSetVersionResponse createDraft(String actorEmail, CreateRuleSetRequest req) {
    RuleSetEntity ruleSet = new RuleSetEntity();
    ruleSet.setKey(req.key());
    ruleSet.setTitle(req.title());
    ruleSet.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    ruleSet.setCreatedAt(OffsetDateTime.now());
    ruleSet = ruleSetPersistence.save(ruleSet);

    RuleSetVersionEntity version = new RuleSetVersionEntity();
    version.setRuleSetId(ruleSet.getId());
    version.setVersion(1);
    version.setStatus(RuleStatus.DRAFT);
    version.setNotes(req.notes());
    version.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    version.setCreatedAt(OffsetDateTime.now());
    version = ruleSetVersionPersistence.save(version);

    int i = 0;
    for (UUID ruleVersionId : req.ruleVersionIds()) {
      RuleSetVersionItemEntity item = new RuleSetVersionItemEntity();
      item.setRuleSetVersionId(version.getId());
      item.setRuleVersionId(ruleVersionId);
      item.setSortOrder(i++);
      ruleSetVersionItemPersistence.save(item);
    }

    audit.success(
        actorEmail,
        AuditActionType.RULE_CREATED,
        "rule_sets",
        ruleSet.getId().toString(),
        Map.of("key", req.key(), "version", 1),
        Map.of("message", "✅ RuleSet criado (rascunho)"));

    return toResponse(ruleSet, version, req.ruleVersionIds());
  }

  public RuleSetVersionResponse publish(String actorEmail, UUID ruleSetVersionId) {
    RuleSetVersionEntity version =
        ruleSetVersionPersistence
            .findById(ruleSetVersionId)
            .orElseThrow(() -> new IllegalArgumentException("rulesetVersion não encontrado"));

    if (version.getStatus() == RuleStatus.PUBLISHED) {
      return toResponse(version);
    }

    version.setStatus(RuleStatus.PUBLISHED);
    ruleSetVersionPersistence.save(version);

    audit.success(
        actorEmail,
        AuditActionType.RULESET_PUBLISHED,
        "rule_set_versions",
        version.getId().toString(),
        Map.of("status", "PUBLISHED"),
        Map.of("message", "🚀 RuleSet publicado"));

    return toResponse(version);
  }

  public void activate(String actorEmail, UUID ruleSetVersionId) {
    RuleSetVersionEntity version =
        ruleSetVersionPersistence
            .findById(ruleSetVersionId)
            .orElseThrow(() -> new IllegalArgumentException("rulesetVersion não encontrado"));
    if (version.getStatus() != RuleStatus.PUBLISHED) {
      throw new IllegalStateException("Somente RuleSet PUBLISHED pode ser ativado");
    }

    ActiveRuleSetEntity active =
        activeRuleSetPersistence
            .findById((short) 1)
            .orElseGet(
                () -> {
                  ActiveRuleSetEntity a = new ActiveRuleSetEntity();
                  a.setId((short) 1);
                  return a;
                });
    active.setRuleSetVersionId(version.getId());
    active.setActivatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    active.setActivatedAt(OffsetDateTime.now());
    activeRuleSetPersistence.save(active);

    audit.success(
        actorEmail,
        AuditActionType.RULESET_ACTIVATED,
        "active_rule_set",
        "1",
        Map.of("ruleSetVersionId", version.getId().toString()),
        Map.of("message", "✅ RuleSet ativo atualizado"));
  }

  public SimulationResponse simulate(
      String actorEmail, UUID maybeRuleSetVersionId, TransactionRequest payload) {
    RuleSetVersionEntity version;
    if (maybeRuleSetVersionId != null) {
      version =
          ruleSetVersionPersistence
              .findById(maybeRuleSetVersionId)
              .orElseThrow(() -> new IllegalArgumentException("rulesetVersion não encontrado"));
    } else {
      ActiveRuleSetEntity active =
          activeRuleSetPersistence
              .findById((short) 1)
              .orElseThrow(() -> new IllegalStateException("Nenhum RuleSet ativo configurado"));
      version =
          ruleSetVersionPersistence
              .findById(active.getRuleSetVersionId())
              .orElseThrow(() -> new IllegalStateException("RuleSet ativo inválido"));
    }

    List<RuleSetVersionItemEntity> items =
        ruleSetVersionItemPersistence.findByRuleSetVersionIdOrderBySortOrderAsc(version.getId());
    List<TriggeredRuleDTO> triggered = new ArrayList<>();
    int risk = 0;
    DecisionOutcome finalDecision = DecisionOutcome.APROVADO;

    for (RuleSetVersionItemEntity item : items) {
      RuleVersionEntity ruleVersion =
          ruleVersionPersistence
              .findById(item.getRuleVersionId())
              .orElseThrow(
                  () ->
                      new IllegalStateException(
                          "ruleVersion referenciada não encontrada: " + item.getRuleVersionId()));
      if (!Boolean.TRUE.equals(ruleVersion.getEnabled())) {
        continue;
      }
      boolean match =
          evaluator.evaluate(
              sanitizer.sanitize(payload), ruleVersion.getLogic(), ruleVersion.getConditionsJson());
      if (!match) {
        continue;
      }

      RuleEntity rule = rulePersistence.findById(ruleVersion.getRuleId()).orElse(null);
      String ruleName = rule == null ? ruleVersion.getRuleId().toString() : rule.getKey();

      TriggeredRuleDTO tr =
          TriggeredRuleDTO.builder()
              .name(ruleName)
              .weight(ruleVersion.getSeverity())
              .contribution(ruleVersion.getSeverity())
              .detail(ruleVersion.getReasonTemplate())
              .build();
      triggered.add(tr);

      risk += ruleVersion.getSeverity() == null ? 0 : ruleVersion.getSeverity();
      if (ruleVersion.getDecision() == DecisionOutcome.FRAUDE) {
        finalDecision = DecisionOutcome.FRAUDE;
      } else if (ruleVersion.getDecision() == DecisionOutcome.SUSPEITA_DE_FRAUDE
          && finalDecision != DecisionOutcome.FRAUDE) {
        finalDecision = DecisionOutcome.SUSPEITA_DE_FRAUDE;
      }
    }

    Map<String, Object> sanitized = sanitizer.sanitize(payload);

    Map<String, Object> explain =
        Map.of("ruleSetVersionId", version.getId(), "triggeredCount", triggered.size());

    SimulationRunEntity sim = new SimulationRunEntity();
    sim.setName("SIMULAÇÃO");
    sim.setRequestedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    sim.setRuleSetVersionId(version.getId());
    sim.setPayloadJson(toJson(sanitized));
    sim.setDecision(finalDecision);
    sim.setRiskScore(Math.min(100, Math.max(0, risk)));
    sim.setTriggeredRulesJson(toJson(triggered));
    sim.setExplainJson(toJson(explain));
    sim.setCreatedAt(OffsetDateTime.now());
    simulationRunPersistence.save(sim);

    audit.success(
        actorEmail,
        AuditActionType.SIMULATION_RUN,
        "simulation_runs",
        sim.getId().toString(),
        Map.of("decision", finalDecision.toString(), "riskScore", sim.getRiskScore()),
        Map.of("message", "🧪 Simulação executada"));

    DecisionLogEntity decisionLog = new DecisionLogEntity();
    decisionLog.setExternalTransactionId(payload.getExternalTransactionId());
    decisionLog.setCustomerId(payload.getCustomerIdFromHeader());
    decisionLog.setMerchantId(payload.getMerchantId());
    decisionLog.setAmount(payload.getTransactionAmount());
    decisionLog.setCurrencyCode(payload.getTransactionCurrencyCode());
    decisionLog.setDecision(finalDecision);
    decisionLog.setRiskScore(Math.min(100, Math.max(0, risk)));
    decisionLog.setTriggeredRulesJson(toJson(triggered));
    decisionLog.setExplainJson(toJson(explain));
    decisionLog.setPayloadJson(toJson(sanitized));
    decisionLog.setPanMasked(
        sanitized.get("pan") == null ? null : String.valueOf(sanitized.get("pan")));
    decisionLog.setRulesetVersionId(version.getId());
    decisionLog.setCreatedAt(OffsetDateTime.now());
    decisionLogPersistence.save(decisionLog);

    return new SimulationResponse(
        finalDecision, Math.min(100, Math.max(0, risk)), triggered, explain);
  }

  private RuleSetVersionResponse toResponse(RuleSetVersionEntity version) {
    List<RuleSetVersionItemEntity> items =
        ruleSetVersionItemPersistence.findByRuleSetVersionIdOrderBySortOrderAsc(version.getId());
    List<UUID> ids = items.stream().map(RuleSetVersionItemEntity::getRuleVersionId).toList();
    RuleSetEntity ruleSet = ruleSetPersistence.findById(version.getRuleSetId()).orElseThrow();
    return toResponse(ruleSet, version, ids);
  }

  private RuleSetVersionResponse toResponse(
      RuleSetEntity ruleSet, RuleSetVersionEntity version, List<UUID> ruleVersionIds) {
    return new RuleSetVersionResponse(
        version.getId(),
        ruleSet.getId(),
        ruleSet.getKey(),
        ruleSet.getTitle(),
        version.getVersion(),
        version.getStatus(),
        version.getNotes(),
        ruleVersionIds,
        version.getCreatedAt());
  }

  private String toJson(Object o) {
    return json.write(o);
  }
}
