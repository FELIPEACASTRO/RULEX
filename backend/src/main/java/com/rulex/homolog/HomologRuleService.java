package com.rulex.homolog;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.homolog.CreateRuleRequest;
import com.rulex.dto.homolog.RuleVersionResponse;
import com.rulex.entity.homolog.*;
import com.rulex.repository.homolog.RuleRepository;
import com.rulex.repository.homolog.RuleVersionRepository;
import jakarta.transaction.Transactional;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;
import org.springframework.stereotype.Service;

@Service
public class HomologRuleService {

  private final RuleRepository ruleRepository;
  private final RuleVersionRepository ruleVersionRepository;
  private final ObjectMapper objectMapper;
  private final HomologAuditService audit;
  private final ActorResolver actorResolver;

  public HomologRuleService(
      RuleRepository ruleRepository,
      RuleVersionRepository ruleVersionRepository,
      ObjectMapper objectMapper,
      HomologAuditService audit,
      ActorResolver actorResolver) {
    this.ruleRepository = ruleRepository;
    this.ruleVersionRepository = ruleVersionRepository;
    this.objectMapper = objectMapper;
    this.audit = audit;
    this.actorResolver = actorResolver;
  }

  @Transactional
  public RuleVersionResponse create(String actorEmail, CreateRuleRequest req) {
    validateFields(req.fieldsUsed());

    RuleEntity rule = new RuleEntity();
    rule.setKey(req.key());
    rule.setTitle(req.title());
    rule.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    rule.setCreatedAt(OffsetDateTime.now());
    rule = ruleRepository.save(rule);

    RuleVersionEntity v = new RuleVersionEntity();
    v.setRuleId(rule.getId());
    v.setVersion(1);
    v.setStatus(RuleStatus.DRAFT);
    v.setPriority(req.priority());
    v.setSeverity(req.severity());
    v.setDecision(req.decision());
    v.setReasonTemplate(req.reasonTemplate());
    v.setFieldsUsed(req.fieldsUsed().toArray(String[]::new));
    v.setLogic(req.logic());
    v.setConditionsJson(writeJson(req.conditions()));
    v.setEnabled(Boolean.TRUE.equals(req.enabled()));
    v.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    v.setCreatedAt(OffsetDateTime.now());

    v = ruleVersionRepository.save(v);

    audit.success(
        actorEmail,
        AuditActionType.RULE_CREATED,
        "rule",
        rule.getId().toString(),
        List.of("key", req.key(), "version", 1),
        Map.of("message", "✅ Regra criada (rascunho)"));

    return toResponse(rule, v);
  }

  public Optional<RuleVersionResponse> getLatest(UUID ruleId) {
    RuleEntity rule = ruleRepository.findById(ruleId).orElse(null);
    if (rule == null) {
      return Optional.empty();
    }
    List<RuleVersionEntity> versions = ruleVersionRepository.findByRuleIdOrderByVersionDesc(ruleId);
    if (versions.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(toResponse(rule, versions.getFirst()));
  }

  @Transactional
  public RuleVersionResponse publish(String actorEmail, UUID ruleVersionId) {
    RuleVersionEntity v =
        ruleVersionRepository
            .findById(ruleVersionId)
            .orElseThrow(() -> new IllegalArgumentException("ruleVersion não encontrada"));
    if (v.getStatus() == RuleStatus.PUBLISHED) {
      RuleEntity rule = ruleRepository.findById(v.getRuleId()).orElse(null);
      if (rule == null) {
        throw new IllegalStateException("rule não encontrada para version");
      }
      return toResponse(rule, v);
    }
    v.setStatus(RuleStatus.PUBLISHED);
    ruleVersionRepository.save(v);

    audit.success(
        actorEmail,
        AuditActionType.RULE_PUBLISHED,
        "rule_versions",
        v.getId().toString(),
        List.of("status", "PUBLISHED"),
        Map.of("message", "🚀 Versão da regra publicada"));

    RuleEntity rule = ruleRepository.findById(v.getRuleId()).orElse(null);
    if (rule == null) {
      throw new IllegalStateException("rule não encontrada para version");
    }
    return toResponse(rule, v);
  }

  @Transactional
  public RuleVersionResponse rollbackToVersion(String actorEmail, UUID ruleId, int version) {
    RuleEntity rule =
        ruleRepository
            .findById(ruleId)
            .orElseThrow(() -> new IllegalArgumentException("rule não encontrada"));

    RuleVersionEntity base =
        ruleVersionRepository
            .findByRuleIdAndVersion(ruleId, version)
            .orElseThrow(() -> new IllegalArgumentException("versão alvo não encontrada"));

    List<RuleVersionEntity> versions = ruleVersionRepository.findByRuleIdOrderByVersionDesc(ruleId);
    if (versions.isEmpty()) {
      throw new IllegalArgumentException("nenhuma versão encontrada");
    }
    RuleVersionEntity latest = versions.getFirst();

    RuleVersionEntity v = new RuleVersionEntity();
    v.setRuleId(rule.getId());
    v.setVersion(latest.getVersion() + 1);
    v.setStatus(RuleStatus.DRAFT);
    v.setPriority(base.getPriority());
    v.setSeverity(base.getSeverity());
    v.setDecision(base.getDecision());
    v.setReasonTemplate(base.getReasonTemplate());
    v.setFieldsUsed(base.getFieldsUsed());
    v.setLogic(base.getLogic());
    v.setConditionsJson(base.getConditionsJson());
    v.setEnabled(Boolean.TRUE.equals(base.getEnabled()));
    v.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    v.setCreatedAt(OffsetDateTime.now());

    v = ruleVersionRepository.save(v);

    audit.success(
        actorEmail,
        AuditActionType.RULE_UPDATED,
        "rules",
        ruleId.toString(),
        List.of("fromVersion", version, "newDraftVersion", v.getVersion()),
        Map.of("message", "↩️ Rollback criado como novo rascunho"));

    return toResponse(rule, v);
  }

  private void validateFields(List<String> fieldsUsed) {
    if (fieldsUsed == null || fieldsUsed.isEmpty()) {
      throw new IllegalArgumentException("fieldsUsed obrigatório");
    }
    for (String f : fieldsUsed) {
      if (f == null || f.isBlank() || !f.trim().matches("[A-Za-z0-9_]+")) {
        throw new IllegalArgumentException("fieldsUsed contém campo inválido: " + f);
      }
    }
  }

  private String writeJson(Object o) {
    try {
      return objectMapper.writeValueAsString(o);
    } catch (Exception e) {
      throw new IllegalArgumentException("Falha ao serializar JSON", e);
    }
  }

  private RuleVersionResponse toResponse(RuleEntity rule, RuleVersionEntity v) {
    return new RuleVersionResponse(
        v.getId(),
        rule.getId(),
        rule.getKey(),
        rule.getTitle(),
        v.getVersion(),
        v.getStatus(),
        v.getPriority(),
        v.getSeverity(),
        v.getDecision(),
        v.getReasonTemplate(),
        v.getFieldsUsed() == null ? List.of() : List.of(v.getFieldsUsed()),
        v.getLogic(),
        v.getConditionsJson(),
        v.getEnabled(),
        v.getCreatedAt());
  }
}
