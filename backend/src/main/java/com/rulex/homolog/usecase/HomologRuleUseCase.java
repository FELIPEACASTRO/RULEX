package com.rulex.homolog.usecase;

import com.rulex.dto.homolog.CreateRuleRequest;
import com.rulex.dto.homolog.RuleVersionResponse;
import com.rulex.entity.homolog.AuditActionType;
import com.rulex.entity.homolog.RuleEntity;
import com.rulex.entity.homolog.RuleStatus;
import com.rulex.entity.homolog.RuleVersionEntity;
import com.rulex.homolog.port.ActorResolverPort;
import com.rulex.homolog.port.AuditPort;
import com.rulex.homolog.port.JsonPort;
import com.rulex.homolog.port.RulePersistencePort;
import com.rulex.homolog.port.RuleVersionPersistencePort;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public class HomologRuleUseCase {

  private final RulePersistencePort rulePersistence;
  private final RuleVersionPersistencePort ruleVersionPersistence;
  private final JsonPort json;
  private final AuditPort audit;
  private final ActorResolverPort actorResolver;

  public HomologRuleUseCase(
      RulePersistencePort rulePersistence,
      RuleVersionPersistencePort ruleVersionPersistence,
      JsonPort json,
      AuditPort audit,
      ActorResolverPort actorResolver) {
    this.rulePersistence = rulePersistence;
    this.ruleVersionPersistence = ruleVersionPersistence;
    this.json = json;
    this.audit = audit;
    this.actorResolver = actorResolver;
  }

  public RuleVersionResponse create(String actorEmail, CreateRuleRequest req) {
    validateFields(req.fieldsUsed());

    RuleEntity rule = new RuleEntity();
    rule.setKey(req.key());
    rule.setTitle(req.title());
    rule.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    rule.setCreatedAt(OffsetDateTime.now());
    rule = rulePersistence.save(rule);

    RuleVersionEntity version = new RuleVersionEntity();
    version.setRuleId(rule.getId());
    version.setVersion(1);
    version.setStatus(RuleStatus.DRAFT);
    version.setPriority(req.priority());
    version.setSeverity(req.severity());
    version.setDecision(req.decision());
    version.setReasonTemplate(req.reasonTemplate());
    version.setFieldsUsed(req.fieldsUsed().toArray(String[]::new));
    version.setLogic(req.logic());
    version.setConditionsJson(writeJson(req.conditions()));
    version.setEnabled(Boolean.TRUE.equals(req.enabled()));
    version.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    version.setCreatedAt(OffsetDateTime.now());

    version = ruleVersionPersistence.save(version);

    audit.success(
        actorEmail,
        AuditActionType.RULE_CREATED,
        "rule",
        rule.getId().toString(),
        List.of("key", req.key(), "version", 1),
        Map.of("message", "✅ Regra criada (rascunho)"));

    return toResponse(rule, version);
  }

  public Optional<RuleVersionResponse> getLatest(UUID ruleId) {
    RuleEntity rule = rulePersistence.findById(ruleId).orElse(null);
    if (rule == null) {
      return Optional.empty();
    }
    List<RuleVersionEntity> versions =
        ruleVersionPersistence.findByRuleIdOrderByVersionDesc(ruleId);
    if (versions.isEmpty()) {
      return Optional.empty();
    }
    return Optional.of(toResponse(rule, versions.getFirst()));
  }

  public RuleVersionResponse publish(String actorEmail, UUID ruleVersionId) {
    RuleVersionEntity version =
        ruleVersionPersistence
            .findById(ruleVersionId)
            .orElseThrow(() -> new IllegalArgumentException("ruleVersion não encontrada"));
    if (version.getStatus() == RuleStatus.PUBLISHED) {
      RuleEntity rule = rulePersistence.findById(version.getRuleId()).orElse(null);
      if (rule == null) {
        throw new IllegalStateException("rule não encontrada para version");
      }
      return toResponse(rule, version);
    }
    version.setStatus(RuleStatus.PUBLISHED);
    ruleVersionPersistence.save(version);

    audit.success(
        actorEmail,
        AuditActionType.RULE_PUBLISHED,
        "rule_versions",
        version.getId().toString(),
        List.of("status", "PUBLISHED"),
        Map.of("message", "🚀 Versão da regra publicada"));

    RuleEntity rule = rulePersistence.findById(version.getRuleId()).orElse(null);
    if (rule == null) {
      throw new IllegalStateException("rule não encontrada para version");
    }
    return toResponse(rule, version);
  }

  public RuleVersionResponse rollbackToVersion(String actorEmail, UUID ruleId, int version) {
    RuleEntity rule =
        rulePersistence
            .findById(ruleId)
            .orElseThrow(() -> new IllegalArgumentException("rule não encontrada"));

    RuleVersionEntity base =
        ruleVersionPersistence
            .findByRuleIdAndVersion(ruleId, version)
            .orElseThrow(() -> new IllegalArgumentException("versão alvo não encontrada"));

    List<RuleVersionEntity> versions =
        ruleVersionPersistence.findByRuleIdOrderByVersionDesc(ruleId);
    if (versions.isEmpty()) {
      throw new IllegalArgumentException("nenhuma versão encontrada");
    }
    RuleVersionEntity latest = versions.getFirst();

    RuleVersionEntity newVersion = new RuleVersionEntity();
    newVersion.setRuleId(rule.getId());
    newVersion.setVersion(latest.getVersion() + 1);
    newVersion.setStatus(RuleStatus.DRAFT);
    newVersion.setPriority(base.getPriority());
    newVersion.setSeverity(base.getSeverity());
    newVersion.setDecision(base.getDecision());
    newVersion.setReasonTemplate(base.getReasonTemplate());
    newVersion.setFieldsUsed(base.getFieldsUsed());
    newVersion.setLogic(base.getLogic());
    newVersion.setConditionsJson(base.getConditionsJson());
    newVersion.setEnabled(Boolean.TRUE.equals(base.getEnabled()));
    newVersion.setCreatedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    newVersion.setCreatedAt(OffsetDateTime.now());

    newVersion = ruleVersionPersistence.save(newVersion);

    audit.success(
        actorEmail,
        AuditActionType.RULE_UPDATED,
        "rules",
        ruleId.toString(),
        List.of("fromVersion", version, "newDraftVersion", newVersion.getVersion()),
        Map.of("message", "↩️ Rollback criado como novo rascunho"));

    return toResponse(rule, newVersion);
  }

  private void validateFields(List<String> fieldsUsed) {
    if (fieldsUsed == null || fieldsUsed.isEmpty()) {
      throw new IllegalArgumentException("fieldsUsed obrigatório");
    }
    for (String field : fieldsUsed) {
      if (field == null || field.isBlank() || !field.trim().matches("[A-Za-z0-9_]+")) {
        throw new IllegalArgumentException("fieldsUsed contém campo inválido: " + field);
      }
    }
  }

  private String writeJson(Object o) {
    try {
      return json.write(o);
    } catch (RuntimeException e) {
      throw new IllegalArgumentException("Falha ao serializar JSON", e);
    }
  }

  private RuleVersionResponse toResponse(RuleEntity rule, RuleVersionEntity version) {
    return new RuleVersionResponse(
        version.getId(),
        rule.getId(),
        rule.getKey(),
        rule.getTitle(),
        version.getVersion(),
        version.getStatus(),
        version.getPriority(),
        version.getSeverity(),
        version.getDecision(),
        version.getReasonTemplate(),
        version.getFieldsUsed() == null ? List.of() : List.of(version.getFieldsUsed()),
        version.getLogic(),
        version.getConditionsJson(),
        version.getEnabled(),
        version.getCreatedAt());
  }
}
