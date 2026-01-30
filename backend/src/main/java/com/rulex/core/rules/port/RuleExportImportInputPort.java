package com.rulex.core.rules.port;

import com.rulex.dto.RuleExportDTO;
import com.rulex.dto.RuleExportDTO.ImportError;
import com.rulex.dto.RuleExportDTO.ImportOptions;
import com.rulex.dto.RuleExportDTO.ImportResult;
import java.util.List;

public interface RuleExportImportInputPort {

  RuleExportDTO exportAllRules(String format, String exportedBy);

  RuleExportDTO exportRulesByKeys(List<String> keys, String format, String exportedBy);

  RuleExportDTO exportComplexRules(String exportedBy);

  String toJson(RuleExportDTO export) throws com.fasterxml.jackson.core.JsonProcessingException;

  String toYaml(RuleExportDTO export) throws com.fasterxml.jackson.core.JsonProcessingException;

  ImportResult importFromJson(String json, ImportOptions options, String importedBy)
      throws com.fasterxml.jackson.core.JsonProcessingException;

  ImportResult importFromYaml(String yaml, ImportOptions options, String importedBy)
      throws com.fasterxml.jackson.core.JsonProcessingException;

  ImportResult importRules(RuleExportDTO exportData, ImportOptions options, String importedBy);

  List<ImportError> validateRuleData(RuleExportDTO.RuleData ruleData);
}
