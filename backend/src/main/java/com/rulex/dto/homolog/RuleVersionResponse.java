package com.rulex.dto.homolog;

import com.rulex.entity.homolog.DecisionOutcome;
import com.rulex.entity.homolog.LogicOperator;
import com.rulex.entity.homolog.RuleStatus;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

public record RuleVersionResponse(
    UUID id,
    UUID ruleId,
    String key,
    String title,
    Integer version,
    RuleStatus status,
    Integer priority,
    Integer severity,
    DecisionOutcome decision,
    String reasonTemplate,
    List<String> fieldsUsed,
    LogicOperator logic,
    String conditionsJson,
    Boolean enabled,
    OffsetDateTime createdAt) {}
