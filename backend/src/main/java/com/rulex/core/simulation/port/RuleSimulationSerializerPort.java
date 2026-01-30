package com.rulex.core.simulation.port;

import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import java.util.List;
import java.util.Map;

public interface RuleSimulationSerializerPort {

  List<RuleConditionDTO> readConditions(String json);

  Map<String, Object> toPayloadMap(TransactionRequest payload);
}
