package com.rulex.adapter.transaction;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.core.transaction.port.TriggeredRuleParserPort;
import com.rulex.dto.TriggeredRuleDTO;
import java.util.List;
import org.springframework.stereotype.Component;

@Component
public class TriggeredRuleParserAdapter implements TriggeredRuleParserPort {

  private final ObjectMapper objectMapper;

  public TriggeredRuleParserAdapter(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  @Override
  public List<TriggeredRuleDTO> parseTriggeredRules(String rulesApplied) {
    if (rulesApplied == null || rulesApplied.isBlank()) {
      return List.of();
    }
    try {
      return objectMapper.readValue(
          rulesApplied,
          objectMapper
              .getTypeFactory()
              .constructCollectionType(List.class, TriggeredRuleDTO.class));
    } catch (Exception e) {
      // fallback legado: string CSV
      return List.of(rulesApplied.split(",")).stream()
          .map(String::trim)
          .filter(s -> !s.isBlank())
          .map(name -> TriggeredRuleDTO.builder().name(name).weight(0).contribution(0).build())
          .toList();
    }
  }
}
