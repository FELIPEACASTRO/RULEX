package com.rulex.homolog;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.homolog.AuditActionType;
import com.rulex.entity.homolog.AuditEntryEntity;
import com.rulex.entity.homolog.AuditResult;
import com.rulex.repository.homolog.AuditEntryRepository;
import java.time.OffsetDateTime;
import org.springframework.stereotype.Service;

@Service
public class HomologAuditService {

  private final AuditEntryRepository auditEntryRepository;
  private final ObjectMapper objectMapper;
  private final ActorResolver actorResolver;

  public HomologAuditService(
      AuditEntryRepository auditEntryRepository,
      ObjectMapper objectMapper,
      ActorResolver actorResolver) {
    this.auditEntryRepository = auditEntryRepository;
    this.objectMapper = objectMapper;
    this.actorResolver = actorResolver;
  }

  public void success(
      String actorEmail,
      AuditActionType actionType,
      String entityType,
      String entityId,
      Object diff,
      Object detailsJson) {
    write(
        actorEmail, actionType, entityType, entityId, AuditResult.SUCCESS, diff, detailsJson, null);
  }

  public void failure(
      String actorEmail,
      AuditActionType actionType,
      String entityType,
      String entityId,
      Object diff,
      Object detailsJson,
      String errorMessage) {
    write(
        actorEmail,
        actionType,
        entityType,
        entityId,
        AuditResult.FAILURE,
        diff,
        detailsJson,
        errorMessage);
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
