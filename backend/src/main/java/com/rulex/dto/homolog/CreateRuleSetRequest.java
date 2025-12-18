package com.rulex.dto.homolog;

import jakarta.validation.constraints.*;
import java.util.List;
import java.util.UUID;

public record CreateRuleSetRequest(
    @NotBlank String key,
    @NotBlank String title,
    @NotNull @Size(min = 1) List<UUID> ruleVersionIds,
    String notes) {}
