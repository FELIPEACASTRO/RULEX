package com.rulex.adapter.rules;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLGenerator;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.rulex.core.rules.port.RuleExportSerializerPort;
import com.rulex.dto.RuleExportDTO;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Component;

@Component
public class JacksonRuleExportSerializerAdapter implements RuleExportSerializerPort {

  private final ObjectMapper jsonMapper;
  private final ObjectMapper yamlMapper;

  public JacksonRuleExportSerializerAdapter(ObjectMapper jsonMapper) {
    this.jsonMapper = jsonMapper;
    this.yamlMapper =
        new ObjectMapper(
                new YAMLFactory()
                    .disable(YAMLGenerator.Feature.WRITE_DOC_START_MARKER)
                    .enable(YAMLGenerator.Feature.MINIMIZE_QUOTES))
            .registerModule(new JavaTimeModule());
  }

  @Override
  public String toJson(RuleExportDTO export) throws JsonProcessingException {
    return jsonMapper.writerWithDefaultPrettyPrinter().writeValueAsString(export);
  }

  @Override
  public String toYaml(RuleExportDTO export) throws JsonProcessingException {
    return yamlMapper.writerWithDefaultPrettyPrinter().writeValueAsString(export);
  }

  @Override
  public RuleExportDTO fromJson(String json) throws JsonProcessingException {
    return jsonMapper.readValue(json, RuleExportDTO.class);
  }

  @Override
  public RuleExportDTO fromYaml(String yaml) throws JsonProcessingException {
    return yamlMapper.readValue(yaml, RuleExportDTO.class);
  }

  @Override
  public List<Map<String, Object>> readConditionsJson(String json) {
    try {
      return jsonMapper.readValue(
          json, jsonMapper.getTypeFactory().constructCollectionType(List.class, Map.class));
    } catch (Exception e) {
      return List.of();
    }
  }

  @Override
  public String writeConditionsJson(Object value) throws JsonProcessingException {
    return jsonMapper.writeValueAsString(value);
  }
}
