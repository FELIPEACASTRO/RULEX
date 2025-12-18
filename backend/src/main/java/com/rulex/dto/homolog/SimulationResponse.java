package com.rulex.dto.homolog;

import com.rulex.dto.TriggeredRuleDTO;
import com.rulex.entity.homolog.DecisionOutcome;
import java.util.List;

public record SimulationResponse(
    DecisionOutcome decision,
    Integer riskScore,
    List<TriggeredRuleDTO> triggeredRules,
    Object explain) {}
