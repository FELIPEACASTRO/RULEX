package com.rulex.dto.homolog;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record ActivateRuleSetRequest(
        @NotNull UUID ruleSetVersionId
) {
}
