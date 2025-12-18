package com.rulex.homolog.adapter;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.homolog.AuditActionType;
import com.rulex.entity.homolog.AuditEntryEntity;
import com.rulex.entity.homolog.AuditResult;
import com.rulex.homolog.port.ActorResolverPort;
import com.rulex.homolog.port.AuditPort;
import com.rulex.repository.homolog.AuditEntryRepository;
import java.time.OffsetDateTime;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class AuditAdapter implements AuditPort {

  private final AuditEntryRepository auditEntryRepository;
  private final ObjectMapper objectMapper;
  private final ActorResolverPort actorResolver;

  public AuditAdapter(
      AuditEntryRepository auditEntryRepository,
      ObjectMapper objectMapper,
      ActorResolverPort actorResolver) {
    this.auditEntryRepository = auditEntryRepository;
    this.objectMapper = objectMapper;
    this.actorResolver = actorResolver;
  }

  @Override
  public void success(
      String actorEmail,
      AuditActionType action,
      String entityType,
      String entityId,
      Object details,
      Map<String, Object> result) {
    write(actorEmail, action, entityType, entityId, AuditResult.SUCCESS, details, result, null);
  }

  private void write(
      String actorEmail,
      AuditActionType actionType,
      String entityType,
      String entityId,
      AuditResult result,
      Object diff,
      Object detailsJson,
      String errorMessage) {
    AuditEntryEntity e = new AuditEntryEntity();
    e.setActionType(actionType);
    e.setEntityType(entityType);
    try {
      e.setEntityId(entityId == null ? null : java.util.UUID.fromString(entityId));
    } catch (Exception ex) {
      e.setEntityId(null);
    }
    e.setPerformedBy(actorResolver.resolveUserIdOrNull(actorEmail));
    e.setResult(result);
    try {
      e.setDiffJson(diff == null ? null : objectMapper.writeValueAsString(diff));
    } catch (Exception ex) {
      e.setDiffJson(null);
    }
    try {
      e.setDetailsJson(detailsJson == null ? null : objectMapper.writeValueAsString(detailsJson));
    } catch (Exception ex) {
      e.setDetailsJson(null);
    }
    e.setErrorMessage(errorMessage);
    e.setCreatedAt(OffsetDateTime.now());
    auditEntryRepository.save(e);
  }
}
