package com.rulex.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Condição genérica de uma regra (persistida como JSON).
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleConditionDTO {

    @NotBlank
    private String field;

    @NotBlank
    private String operator;

    @NotBlank
    private String value;
}
