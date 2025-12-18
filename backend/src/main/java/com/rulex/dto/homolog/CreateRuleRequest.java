package com.rulex.dto.homolog;

import com.rulex.dto.RuleConditionDTO;
import com.rulex.entity.homolog.DecisionOutcome;
import com.rulex.entity.homolog.LogicOperator;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;

import java.util.List;

public record CreateRuleRequest(
        @NotBlank String key,
        @NotBlank String title,
        @NotNull @Min(0) Integer priority,
        @NotNull @Min(0) @Max(100) Integer severity,
        @NotNull DecisionOutcome decision,
        @NotBlank String reasonTemplate,
        @NotNull @Size(min = 1) List<@NotBlank String> fieldsUsed,
        @NotNull LogicOperator logic,
        @NotNull @Size(min = 1) List<@Valid RuleConditionDTO> conditions,
        @NotNull Boolean enabled
) {
}
