package com.rulex.homolog.port;

import com.rulex.entity.homolog.AuditActionType;
import java.util.Map;

public interface AuditPort {
  void success(
      String actorEmail,
      AuditActionType action,
      String entityType,
      String entityId,
      Object details,
      Map<String, Object> result);
}
