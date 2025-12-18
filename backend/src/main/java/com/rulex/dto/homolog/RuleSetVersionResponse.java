package com.rulex.dto.homolog;

import com.rulex.entity.homolog.RuleStatus;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

public record RuleSetVersionResponse(
        UUID id,
        UUID ruleSetId,
        String key,
        String title,
        Integer version,
        RuleStatus status,
        String notes,
        List<UUID> ruleVersionIds,
        OffsetDateTime createdAt
) {
}
