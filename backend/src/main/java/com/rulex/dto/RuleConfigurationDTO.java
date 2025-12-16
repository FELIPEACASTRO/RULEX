package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

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

    @JsonProperty("version")
    private Integer version;

}
