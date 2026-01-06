package com.rulex.infrastructure.audit;

import com.rulex.application.port.out.AuditEventPort;
import com.rulex.entity.AuditLog;
import com.rulex.repository.AuditLogRepository;
import java.time.LocalDateTime;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

/**
 * Adapter para persistência de eventos de auditoria.
 *
 * <p>Implementa AuditEventPort gravando eventos no banco de dados de forma assíncrona.
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class AuditEventAdapter implements AuditEventPort {

  private final AuditLogRepository repository;

  @Override
  @Async
  public void logEvent(
      EventType eventType,
      String entityType,
      String entityId,
      String actor,
      Map<String, Object> details) {

    try {
      AuditLog auditLog =
          AuditLog.builder()
              .actionType(toAuditActionType(eventType))
              .description(String.format("%s on %s/%s", eventType, entityType, entityId))
              .details(serializeDetails(details))
              .performedBy(actor)
              .result(AuditLog.AuditResult.SUCCESS)
              .createdAt(LocalDateTime.now())
              .build();

      repository.save(auditLog);
      log.debug(
          "Audit event logged: type={}, entity={}/{}, actor={}",
          eventType,
          entityType,
          entityId,
          actor);
    } catch (Exception e) {
      // Não falha a operação principal por erro de auditoria
      log.error(
          "Erro ao gravar evento de auditoria: type={}, entity={}/{}, error={}",
          eventType,
          entityType,
          entityId,
          e.getMessage());
    }
  }

  private AuditLog.AuditActionType toAuditActionType(EventType eventType) {
    return switch (eventType) {
      case TRANSACTION_ANALYZED, TRANSACTION_TAMPER_DETECTED ->
          AuditLog.AuditActionType.TRANSACTION_PROCESSED;
      case RULE_TRIGGERED -> AuditLog.AuditActionType.DECISION_MADE;
      case DECISION_CREATED -> AuditLog.AuditActionType.DECISION_MADE;
      case RULE_CREATED -> AuditLog.AuditActionType.RULE_CREATED;
      case RULE_UPDATED, RULE_ENABLED, RULE_DISABLED -> AuditLog.AuditActionType.RULE_UPDATED;
      case RULE_DELETED -> AuditLog.AuditActionType.RULE_DELETED;
    };
  }

  private String serializeDetails(Map<String, Object> details) {
    if (details == null || details.isEmpty()) return "{}";

    StringBuilder sb = new StringBuilder("{");
    boolean first = true;
    for (Map.Entry<String, Object> entry : details.entrySet()) {
      if (!first) sb.append(",");
      sb.append("\"").append(entry.getKey()).append("\":");
      Object v = entry.getValue();
      if (v instanceof String) {
        sb.append("\"").append(v).append("\"");
      } else {
        sb.append(v);
      }
      first = false;
    }
    sb.append("}");
    return sb.toString();
  }
}
