package com.rulex.core.rules.port;

import com.rulex.dto.RuleExportDTO;
import java.util.List;
import java.util.Map;

public interface RuleExportSerializerPort {

  String toJson(RuleExportDTO export) throws com.fasterxml.jackson.core.JsonProcessingException;

  String toYaml(RuleExportDTO export) throws com.fasterxml.jackson.core.JsonProcessingException;

  RuleExportDTO fromJson(String json) throws com.fasterxml.jackson.core.JsonProcessingException;

  RuleExportDTO fromYaml(String yaml) throws com.fasterxml.jackson.core.JsonProcessingException;

  List<Map<String, Object>> readConditionsJson(String json);

  String writeConditionsJson(Object value) throws com.fasterxml.jackson.core.JsonProcessingException;
}
