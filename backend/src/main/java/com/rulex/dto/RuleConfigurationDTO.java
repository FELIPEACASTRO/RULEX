package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * DTO para RuleConfiguration.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RuleConfigurationDTO {

    @JsonProperty("id")
    private Long id;

    @NotBlank(message = "ruleName é obrigatório")
    @JsonProperty("ruleName")
    private String ruleName;

    @JsonProperty("description")
    private String description;

    @NotNull(message = "ruleType é obrigatório")
    @JsonProperty("ruleType")
    private String ruleType; // SECURITY, CONTEXT, VELOCITY, ANOMALY

    @NotNull(message = "threshold é obrigatório")
    @Min(value = 0, message = "threshold deve ser >= 0")
    @JsonProperty("threshold")
    private Integer threshold;

    @NotNull(message = "weight é obrigatório")
    @Min(value = 0, message = "weight deve ser >= 0")
    @Max(value = 100, message = "weight deve ser <= 100")
    @JsonProperty("weight")
    private Integer weight;

    @NotNull(message = "enabled é obrigatório")
    @JsonProperty("enabled")
    private Boolean enabled;

    @NotNull(message = "classification é obrigatório")
    @JsonProperty("classification")
    private String classification; // APPROVED, SUSPICIOUS, FRAUD

    @JsonProperty("parameters")
    private String parameters;

    @NotNull(message = "conditions é obrigatório")
    @JsonProperty("conditions")
    private List<RuleConditionDTO> conditions;

    @NotBlank(message = "logicOperator é obrigatório")
    @JsonProperty("logicOperator")
    private String logicOperator; // AND | OR

    @JsonProperty("version")
    private Integer version;

}
