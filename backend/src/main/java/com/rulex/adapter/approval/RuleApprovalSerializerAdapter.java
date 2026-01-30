package com.rulex.adapter.approval;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.core.approval.port.RuleApprovalSerializerPort;
import com.rulex.dto.RuleConfigurationDTO;
import org.springframework.stereotype.Component;

@Component
public class RuleApprovalSerializerAdapter implements RuleApprovalSerializerPort {

  private final ObjectMapper objectMapper;

  public RuleApprovalSerializerAdapter(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  @Override
  public String serialize(RuleConfigurationDTO dto) {
    try {
      return objectMapper.writeValueAsString(dto);
    } catch (Exception e) {
      throw new RuntimeException("Erro ao serializar payload", e);
    }
  }

  @Override
  public RuleConfigurationDTO deserialize(String json) {
    try {
      return objectMapper.readValue(json, RuleConfigurationDTO.class);
    } catch (Exception e) {
      throw new RuntimeException("Erro ao deserializar payload", e);
    }
  }
}
