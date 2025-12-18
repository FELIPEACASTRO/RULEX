package com.rulex.homolog.adapter;

import com.rulex.entity.homolog.AuditActionType;
import com.rulex.homolog.HomologAuditService;
import com.rulex.homolog.port.AuditPort;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class AuditAdapter implements AuditPort {

  private final HomologAuditService audit;

  public AuditAdapter(HomologAuditService audit) {
    this.audit = audit;
  }

  @Override
  public void success(
      String actorEmail,
      AuditActionType action,
      String entityType,
      String entityId,
      Object details,
      Map<String, Object> result) {
    if (details instanceof Map<?, ?> detailsMap) {
      @SuppressWarnings("unchecked")
      Map<String, Object> typed = (Map<String, Object>) detailsMap;
      audit.success(actorEmail, action, entityType, entityId, typed, result);
      return;
    }
    if (details instanceof Iterable<?> detailsIterable) {
      audit.success(actorEmail, action, entityType, entityId, detailsIterable, result);
      return;
    }
    audit.success(actorEmail, action, entityType, entityId, details, result);
  }
}
