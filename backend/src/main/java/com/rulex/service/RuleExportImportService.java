package com.rulex.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.rulex.core.rules.port.ComplexRulePort;
import com.rulex.core.rules.port.RuleExportImportInputPort;
import com.rulex.core.rules.port.RuleExportImportRepositoryPort;
import com.rulex.core.rules.port.RuleExportSerializerPort;
import com.rulex.core.rules.usecase.RuleExportImportUseCase;
import com.rulex.dto.RuleExportDTO;
import com.rulex.dto.RuleExportDTO.ImportOptions;
import com.rulex.dto.RuleExportDTO.ImportResult;
import jakarta.transaction.Transactional;
import java.util.List;
import org.springframework.stereotype.Service;

/** Adapter Spring para exportação e importação de regras. */
@Service
public class RuleExportImportService implements RuleExportImportInputPort {

  private final RuleExportImportUseCase useCase;

  public RuleExportImportService(
      RuleExportImportRepositoryPort repositoryPort,
      ComplexRulePort complexRulePort,
      RuleExportSerializerPort serializerPort) {
    this.useCase = new RuleExportImportUseCase(repositoryPort, complexRulePort, serializerPort);
  }

  @Override
  public RuleExportDTO exportAllRules(String format, String exportedBy) {
    return useCase.exportAllRules(format, exportedBy);
  }

  @Override
  public RuleExportDTO exportRulesByKeys(List<String> keys, String format, String exportedBy) {
    return useCase.exportRulesByKeys(keys, format, exportedBy);
  }

  @Override
  public RuleExportDTO exportComplexRules(String exportedBy) {
    return useCase.exportComplexRules(exportedBy);
  }

  @Override
  public String toJson(RuleExportDTO export) throws JsonProcessingException {
    return useCase.toJson(export);
  }

  @Override
  public String toYaml(RuleExportDTO export) throws JsonProcessingException {
    return useCase.toYaml(export);
  }

  @Override
  @Transactional
  public ImportResult importFromJson(String json, ImportOptions options, String importedBy)
      throws JsonProcessingException {
    return useCase.importFromJson(json, options, importedBy);
  }

  @Override
  @Transactional
  public ImportResult importFromYaml(String yaml, ImportOptions options, String importedBy)
      throws JsonProcessingException {
    return useCase.importFromYaml(yaml, options, importedBy);
  }

  @Override
  @Transactional
  public ImportResult importRules(RuleExportDTO exportData, ImportOptions options, String importedBy) {
    return useCase.importRules(exportData, options, importedBy);
  }

  @Override
  public java.util.List<RuleExportDTO.ImportError> validateRuleData(
      RuleExportDTO.RuleData ruleData) {
    return useCase.validateRuleData(ruleData);
  }
}

