package com.rulex.homolog;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.dto.homolog.CreateRuleSetRequest;
import com.rulex.dto.homolog.RuleSetVersionResponse;
import com.rulex.dto.homolog.SimulationResponse;
import com.rulex.entity.homolog.*;
import com.rulex.repository.homolog.*;
import jakarta.transaction.Transactional;
import java.time.OffsetDateTime;
import java.util.*;
import org.springframework.stereotype.Service;

@Service
public class HomologRuleSetService {

  private final RuleRepository ruleRepository;
  private final RuleSetRepository ruleSetRepository;
  private final RuleSetVersionRepository ruleSetVersionRepository;
  private final RuleSetVersionItemRepository ruleSetVersionItemRepository;
  private final RuleVersionRepository ruleVersionRepository;
  private final ActiveRuleSetRepository activeRuleSetRepository;
  private final DecisionLogRepository decisionLogRepository;
  private final SimulationRunRepository simulationRunRepository;
  private final SafeRuleDslEvaluator evaluator;
  private final PayloadSanitizer sanitizer;
  private final HomologAuditService audit;
  private final ActorResolver actorResolver;
  private final ObjectMapper objectMapper;

  public HomologRuleSetService(
      RuleRepository ruleRepository,
      RuleSetRepository ruleSetRepository,
      RuleSetVersionRepository ruleSetVersionRepository,
      RuleSetVersionItemRepository ruleSetVersionItemRepository,
      RuleVersionRepository ruleVersionRepository,
      ActiveRuleSetRepository activeRuleSetRepository,
      DecisionLogRepository decisionLogRepository,
      SimulationRunRepository simulationRunRepository,
      SafeRuleDslEvaluator evaluator,
      PayloadSanitizer sanitizer,
      HomologAuditService audit,
      ActorResolver actorResolver,
      ObjectMapper objectMapper) {
    this.ruleRepository = ruleRepository;
    this.ruleSetRepository = ruleSetRepository;
    this.ruleSetVersionRepository = ruleSetVersionRepository;
    this.ruleSetVersionItemRepository = ruleSetVersionItemRepository;
    this.ruleVersionRepository = ruleVersionRepository;
    this.activeRuleSetRepository = activeRuleSetRepository;
    this.decisionLogRepository = decisionLogRepository;
    this.simulationRunRepository = simulationRunRepository;
    this.evaluator = evaluator;
    this.sanitizer = sanitizer;
    this.audit = audit;
    this.actorResolver = actorResolver;
    this.objectMapper = objectMapper;
  }

  @Transactional
  public RuleSetVersionResponse createDraft(String actorEmail, CreateRuleSetRequest req) {
    RuleSetEntity rs = new RuleSetEntity();
    rs.setKey(req.key());
    rs.setTitle(req.title());
    rs.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    rs.setCreatedAt(OffsetDateTime.now());
    rs = ruleSetRepository.save(rs);

    RuleSetVersionEntity v = new RuleSetVersionEntity();
    v.setRuleSetId(rs.getId());
    v.setVersion(1);
    v.setStatus(RuleStatus.DRAFT);
    v.setNotes(req.notes());
    v.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    v.setCreatedAt(OffsetDateTime.now());
    v = ruleSetVersionRepository.save(v);

    int i = 0;
    for (UUID ruleVersionId : req.ruleVersionIds()) {
      RuleSetVersionItemEntity item = new RuleSetVersionItemEntity();
      item.setRuleSetVersionId(v.getId());
      item.setRuleVersionId(ruleVersionId);
      item.setSortOrder(i++);
      ruleSetVersionItemRepository.save(item);
    }

    audit.success(
        actorEmail,
        AuditActionType.RULE_CREATED,
        "rule_sets",
        rs.getId().toString(),
        Map.of("key", req.key(), "version", 1),
        Map.of("message", "✅ RuleSet criado (rascunho)"));

    return toResponse(rs, v, req.ruleVersionIds());
  }

  @Transactional
  public RuleSetVersionResponse publish(String actorEmail, UUID ruleSetVersionId) {
    RuleSetVersionEntity v =
        ruleSetVersionRepository
            .findById(ruleSetVersionId)
            .orElseThrow(() -> new IllegalArgumentException("rulesetVersion não encontrado"));

    if (v.getStatus() == RuleStatus.PUBLISHED) {
      return toResponse(v);
    }

    v.setStatus(RuleStatus.PUBLISHED);
    ruleSetVersionRepository.save(v);

    audit.success(
        actorEmail,
        AuditActionType.RULESET_PUBLISHED,
        "rule_set_versions",
        v.getId().toString(),
        Map.of("status", "PUBLISHED"),
        Map.of("message", "🚀 RuleSet publicado"));

    return toResponse(v);
  }

  @Transactional
  public void activate(String actorEmail, UUID ruleSetVersionId) {
    RuleSetVersionEntity v =
        ruleSetVersionRepository
            .findById(ruleSetVersionId)
            .orElseThrow(() -> new IllegalArgumentException("rulesetVersion não encontrado"));
    if (v.getStatus() != RuleStatus.PUBLISHED) {
      throw new IllegalStateException("Somente RuleSet PUBLISHED pode ser ativado");
    }

    ActiveRuleSetEntity active =
        activeRuleSetRepository
            .findById((short) 1)
            .orElseGet(
                () -> {
                  ActiveRuleSetEntity a = new ActiveRuleSetEntity();
                  a.setId((short) 1);
                  return a;
                });
    active.setRuleSetVersionId(v.getId());
    active.setActivatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    active.setActivatedAt(OffsetDateTime.now());
    activeRuleSetRepository.save(active);

    audit.success(
        actorEmail,
        AuditActionType.RULESET_ACTIVATED,
        "active_rule_set",
        "1",
        Map.of("ruleSetVersionId", v.getId().toString()),
        Map.of("message", "✅ RuleSet ativo atualizado"));
  }

  public SimulationResponse simulate(
      String actorEmail, UUID maybeRuleSetVersionId, TransactionRequest payload) {
    RuleSetVersionEntity v;
    if (maybeRuleSetVersionId != null) {
      v =
          ruleSetVersionRepository
              .findById(maybeRuleSetVersionId)
              .orElseThrow(() -> new IllegalArgumentException("rulesetVersion não encontrado"));
    } else {
      ActiveRuleSetEntity active =
          activeRuleSetRepository
              .findById((short) 1)
              .orElseThrow(() -> new IllegalStateException("Nenhum RuleSet ativo configurado"));
      v =
          ruleSetVersionRepository
              .findById(active.getRuleSetVersionId())
              .orElseThrow(() -> new IllegalStateException("RuleSet ativo inválido"));
    }

    List<RuleSetVersionItemEntity> items =
        ruleSetVersionItemRepository.findByRuleSetVersionIdOrderBySortOrderAsc(v.getId());
    List<TriggeredRuleDTO> triggered = new ArrayList<>();
    int risk = 0;
    DecisionOutcome finalDecision = DecisionOutcome.APROVADO;

    for (RuleSetVersionItemEntity item : items) {
      RuleVersionEntity rv =
          ruleVersionRepository
              .findById(item.getRuleVersionId())
              .orElseThrow(
                  () ->
                      new IllegalStateException(
                          "ruleVersion referenciada não encontrada: " + item.getRuleVersionId()));
      if (!Boolean.TRUE.equals(rv.getEnabled())) {
        continue;
      }
      boolean match =
          evaluator.evaluate(sanitizer.sanitize(payload), rv.getLogic(), rv.getConditionsJson());
      if (!match) {
        continue;
      }

      RuleEntity rule = ruleRepository.findById(rv.getRuleId()).orElse(null);
      String ruleName = rule == null ? rv.getRuleId().toString() : rule.getKey();

      TriggeredRuleDTO tr =
          TriggeredRuleDTO.builder()
              .name(ruleName)
              .weight(rv.getSeverity())
              .contribution(rv.getSeverity())
              .detail(rv.getReasonTemplate())
              .build();
      triggered.add(tr);

      risk += rv.getSeverity() == null ? 0 : rv.getSeverity();
      if (rv.getDecision() == DecisionOutcome.FRAUDE) {
        finalDecision = DecisionOutcome.FRAUDE;
      } else if (rv.getDecision() == DecisionOutcome.SUSPEITA_DE_FRAUDE
          && finalDecision != DecisionOutcome.FRAUDE) {
        finalDecision = DecisionOutcome.SUSPEITA_DE_FRAUDE;
      }
    }

    Map<String, Object> sanitized = sanitizer.sanitize(payload);

    Map<String, Object> explain =
        Map.of(
            "ruleSetVersionId", v.getId(),
            "triggeredCount", triggered.size());

    SimulationRunEntity sim = new SimulationRunEntity();
    sim.setName("SIMULAÇÃO");
    sim.setRequestedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    sim.setRuleSetVersionId(v.getId());
    sim.setPayloadJson(toJson(sanitized));
    sim.setDecision(finalDecision);
    sim.setRiskScore(Math.min(100, Math.max(0, risk)));
    sim.setTriggeredRulesJson(toJson(triggered));
    sim.setExplainJson(toJson(explain));
    sim.setCreatedAt(OffsetDateTime.now());
    simulationRunRepository.save(sim);

    audit.success(
        actorEmail,
        AuditActionType.SIMULATION_RUN,
        "simulation_runs",
        sim.getId().toString(),
        Map.of("decision", finalDecision.toString(), "riskScore", sim.getRiskScore()),
        Map.of("message", "🧪 Simulação executada"));

    DecisionLogEntity d = new DecisionLogEntity();
    d.setExternalTransactionId(payload.getExternalTransactionId());
    d.setCustomerId(payload.getCustomerIdFromHeader());
    d.setMerchantId(payload.getMerchantId());
    d.setAmount(payload.getTransactionAmount());
    d.setCurrencyCode(payload.getTransactionCurrencyCode());
    d.setDecision(finalDecision);
    d.setRiskScore(Math.min(100, Math.max(0, risk)));
    d.setTriggeredRulesJson(toJson(triggered));
    d.setExplainJson(toJson(explain));
    d.setPayloadJson(toJson(sanitized));
    d.setPanMasked(sanitized.get("pan") == null ? null : String.valueOf(sanitized.get("pan")));
    d.setRulesetVersionId(v.getId());
    d.setCreatedAt(OffsetDateTime.now());
    decisionLogRepository.save(d);

    return new SimulationResponse(
        finalDecision, Math.min(100, Math.max(0, risk)), triggered, explain);
  }

  private RuleSetVersionResponse toResponse(RuleSetVersionEntity v) {
    List<RuleSetVersionItemEntity> items =
        ruleSetVersionItemRepository.findByRuleSetVersionIdOrderBySortOrderAsc(v.getId());
    List<UUID> ids = items.stream().map(RuleSetVersionItemEntity::getRuleVersionId).toList();
    RuleSetEntity rs = ruleSetRepository.findById(v.getRuleSetId()).orElseThrow();
    return toResponse(rs, v, ids);
  }

  private RuleSetVersionResponse toResponse(
      RuleSetEntity rs, RuleSetVersionEntity v, List<UUID> ruleVersionIds) {
    return new RuleSetVersionResponse(
        v.getId(),
        rs.getId(),
        rs.getKey(),
        rs.getTitle(),
        v.getVersion(),
        v.getStatus(),
        v.getNotes(),
        ruleVersionIds,
        v.getCreatedAt());
  }

  private String toJson(Object o) {
    try {
      return objectMapper.writeValueAsString(o);
    } catch (Exception e) {
      return "{}";
    }
  }
}
