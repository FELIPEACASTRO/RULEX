package com.rulex.dto.homolog;

import com.rulex.dto.TransactionRequest;
import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record SimulationRequest(
        String name,
        UUID ruleSetVersionId,
        @NotNull TransactionRequest payload
) {
}
