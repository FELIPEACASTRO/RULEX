package com.rulex.adapter.simulation;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.core.simulation.port.RuleSimulationSerializerPort;
import com.rulex.dto.RuleConditionDTO;
import com.rulex.dto.TransactionRequest;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class RuleSimulationSerializerAdapter implements RuleSimulationSerializerPort {

  private final ObjectMapper objectMapper;

  public RuleSimulationSerializerAdapter(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  @Override
  public List<RuleConditionDTO> readConditions(String json) {
    try {
      return objectMapper.readValue(
          json,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, RuleConditionDTO.class));
    } catch (Exception e) {
      return List.of();
    }
  }

  @Override
  public Map<String, Object> toPayloadMap(TransactionRequest payload) {
    @SuppressWarnings("unchecked")
    Map<String, Object> map = objectMapper.convertValue(payload, Map.class);
    return map;
  }
}
