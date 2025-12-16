package com.rulex.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * DTO para AuditLog.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuditLogDTO {

    @JsonProperty("id")
    private Long id;

    @JsonProperty("transactionId")
    private Long transactionId;

    @JsonProperty("actionType")
    private String actionType;

    @JsonProperty("description")
    private String description;

    @JsonProperty("details")
    private String details;

    @JsonProperty("performedBy")
    private String performedBy;

    @JsonProperty("result")
    private String result;

    @JsonProperty("errorMessage")
    private String errorMessage;

    @JsonProperty("sourceIp")
    private String sourceIp;

    @JsonProperty("createdAt")
    private LocalDateTime createdAt;

}
