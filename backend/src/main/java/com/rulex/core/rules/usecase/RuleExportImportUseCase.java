package com.rulex.core.rules.usecase;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.rulex.core.rules.port.ComplexRulePort;
import com.rulex.core.rules.port.RuleExportImportInputPort;
import com.rulex.core.rules.port.RuleExportImportRepositoryPort;
import com.rulex.core.rules.port.RuleExportSerializerPort;
import com.rulex.dto.RuleExportDTO;
import com.rulex.dto.RuleExportDTO.*;
import com.rulex.dto.complex.*;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import com.rulex.entity.homolog.*;
import java.time.OffsetDateTime;
import java.util.*;

/** Caso de uso para exportação e importação de regras. */
public class RuleExportImportUseCase implements RuleExportImportInputPort {

  private final RuleExportImportRepositoryPort repositoryPort;
  private final ComplexRulePort complexRulePort;
  private final RuleExportSerializerPort serializerPort;

  public RuleExportImportUseCase(
      RuleExportImportRepositoryPort repositoryPort,
      ComplexRulePort complexRulePort,
      RuleExportSerializerPort serializerPort) {
    this.repositoryPort = repositoryPort;
    this.complexRulePort = complexRulePort;
    this.serializerPort = serializerPort;
  }

  /** Exporta todas as regras ativas */
  @Override
  public RuleExportDTO exportAllRules(String format, String exportedBy) {
    List<RuleData> ruleDataList = new ArrayList<>();

    // Exportar regras simples (rule_configurations)
    List<RuleConfiguration> simpleRules = repositoryPort.findAllSimpleRules();
    for (RuleConfiguration rule : simpleRules) {
      ruleDataList.add(convertSimpleRuleToExport(rule));
    }

    // Exportar regras do sistema de homologação
    List<RuleEntity> homologRules = repositoryPort.findAllHomologRules();
    for (RuleEntity rule : homologRules) {
      List<RuleVersionEntity> versions = repositoryPort.findRuleVersionsByRuleId(rule.getId());
      if (!versions.isEmpty()) {
        RuleVersionEntity latestVersion = versions.get(0);
        ruleDataList.add(convertHomologRuleToExport(rule, latestVersion));
      }
    }

    return RuleExportDTO.builder()
        .metadata(
            ExportMetadata.builder()
                .version("1.0")
                .exportedAt(OffsetDateTime.now())
                .exportedBy(exportedBy)
                .description("Export completo de regras RULEX")
                .totalRules(ruleDataList.size())
                .sourceSystem("RULEX")
                .build())
        .rules(ruleDataList)
        .build();
  }

  /** Exporta regras específicas por chaves */
  @Override
  public RuleExportDTO exportRulesByKeys(List<String> keys, String format, String exportedBy) {
    List<RuleData> ruleDataList = new ArrayList<>();

    for (String key : keys) {
      // Tentar encontrar em rule_configurations
      Optional<RuleConfiguration> simpleRule = repositoryPort.findSimpleRuleByName(key);
      if (simpleRule.isPresent()) {
        ruleDataList.add(convertSimpleRuleToExport(simpleRule.get()));
        continue;
      }

      // Tentar encontrar em rules (homologação)
      Optional<RuleEntity> homologRule = repositoryPort.findHomologRuleByKey(key);
      if (homologRule.isPresent()) {
        List<RuleVersionEntity> versions =
            repositoryPort.findRuleVersionsByRuleId(homologRule.get().getId());
        if (!versions.isEmpty()) {
          ruleDataList.add(convertHomologRuleToExport(homologRule.get(), versions.get(0)));
        }
      }
    }

    return RuleExportDTO.builder()
        .metadata(
            ExportMetadata.builder()
                .version("1.0")
                .exportedAt(OffsetDateTime.now())
                .exportedBy(exportedBy)
                .description("Export seletivo de regras RULEX")
                .totalRules(ruleDataList.size())
                .sourceSystem("RULEX")
                .build())
        .rules(ruleDataList)
        .build();
  }

  /** Exporta regras com estrutura complexa (grupos aninhados) */
  @Override
  public RuleExportDTO exportComplexRules(String exportedBy) {
    List<RuleData> ruleDataList = new ArrayList<>();

    List<RuleEntity> rules = repositoryPort.findAllHomologRules();
    for (RuleEntity rule : rules) {
      List<RuleVersionEntity> versions = repositoryPort.findRuleVersionsByRuleId(rule.getId());

      for (RuleVersionEntity version : versions) {
        // Verificar se tem estrutura complexa
        ConditionGroupDTO conditionGroup = complexRulePort.getConditionGroup(version.getId());
        if (conditionGroup != null) {
          ruleDataList.add(convertToComplexExport(rule, version, conditionGroup));
        }
      }
    }

    return RuleExportDTO.builder()
        .metadata(
            ExportMetadata.builder()
                .version("1.0")
                .exportedAt(OffsetDateTime.now())
                .exportedBy(exportedBy)
                .description("Export de regras complexas RULEX")
                .totalRules(ruleDataList.size())
                .sourceSystem("RULEX")
                .build())
        .rules(ruleDataList)
        .build();
  }

  /** Converte export para JSON */
  @Override
  public String toJson(RuleExportDTO export) throws JsonProcessingException {
    return serializerPort.toJson(export);
  }

  /** Converte export para YAML */
  @Override
  public String toYaml(RuleExportDTO export) throws JsonProcessingException {
    return serializerPort.toYaml(export);
  }

  /** Importa regras de JSON */
  @Override
  public ImportResult importFromJson(String json, ImportOptions options, String importedBy)
      throws JsonProcessingException {
    RuleExportDTO exportData = serializerPort.fromJson(json);
    return importRules(exportData, options, importedBy);
  }

  /** Importa regras de YAML */
  @Override
  public ImportResult importFromYaml(String yaml, ImportOptions options, String importedBy)
      throws JsonProcessingException {
    RuleExportDTO exportData = serializerPort.fromYaml(yaml);
    return importRules(exportData, options, importedBy);
  }

  /** Importa regras do DTO */
  @Override
  public ImportResult importRules(
      RuleExportDTO exportData, ImportOptions options, String importedBy) {
    if (exportData == null || exportData.getRules() == null) {
      return ImportResult.builder()
          .totalProcessed(0)
          .successCount(0)
          .failureCount(1)
          .errors(
              List.of(
                  ImportError.builder()
                      .errorType("VALIDATION_ERROR")
                      .message("Nenhuma regra encontrada")
                      .build()))
          .build();
    }

    if (options == null) {
      options =
          ImportOptions.builder()
              .overwriteExisting(false)
              .dryRun(false)
              .importDisabled(true)
              .build();
    }

    int successCount = 0;
    int failureCount = 0;
    int skippedCount = 0;
    List<ImportError> errors = new ArrayList<>();
    List<String> importedKeys = new ArrayList<>();

    for (RuleData ruleData : exportData.getRules()) {
      try {
        // Aplicar prefixo/sufixo
        String key = ruleData.getKey();
        if (options.getKeyPrefix() != null) {
          key = options.getKeyPrefix() + key;
        }
        if (options.getKeySuffix() != null) {
          key = key + options.getKeySuffix();
        }
        ruleData.setKey(key);

        // Verificar se já existe
        boolean exists =
            repositoryPort.findSimpleRuleByName(key).isPresent()
                || repositoryPort.findHomologRuleByKey(key).isPresent();

        if (exists && !Boolean.TRUE.equals(options.getOverwriteExisting())) {
          skippedCount++;
          continue;
        }

        // Validar regra
        List<ImportError> validationErrors = validateRuleData(ruleData);
        if (!validationErrors.isEmpty()) {
          errors.addAll(validationErrors);
          failureCount++;
          continue;
        }

        // Se é dry run, não importar
        if (Boolean.TRUE.equals(options.getDryRun())) {
          successCount++;
          importedKeys.add(key);
          continue;
        }

        // Importar baseado no tipo
        if (ruleData.getRuleType() == RuleType.COMPLEX && ruleData.getComplexConfig() != null) {
          importComplexRule(ruleData, importedBy, exists);
        } else {
          importSimpleRule(ruleData, importedBy, exists);
        }

        successCount++;
        importedKeys.add(key);

      } catch (Exception e) {
        errors.add(
            ImportError.builder()
                .ruleKey(ruleData.getKey())
                .errorType("IMPORT_ERROR")
                .message(e.getMessage())
                .build());
        failureCount++;
      }
    }

    return ImportResult.builder()
        .totalProcessed(exportData.getRules().size())
        .successCount(successCount)
        .failureCount(failureCount)
        .skippedCount(skippedCount)
        .errors(errors)
        .importedRuleKeys(importedKeys)
        .importedAt(OffsetDateTime.now())
        .build();
  }

  /** Valida dados de uma regra antes de importar */
  @Override
  public List<ImportError> validateRuleData(RuleData ruleData) {
    List<ImportError> errors = new ArrayList<>();

    if (ruleData.getKey() == null || ruleData.getKey().isBlank()) {
      errors.add(
          ImportError.builder()
              .ruleKey(ruleData.getKey())
              .errorType("VALIDATION_ERROR")
              .message("Chave da regra é obrigatória")
              .field("key")
              .build());
    }

    if (ruleData.getTitle() == null || ruleData.getTitle().isBlank()) {
      errors.add(
          ImportError.builder()
              .ruleKey(ruleData.getKey())
              .errorType("VALIDATION_ERROR")
              .message("Título da regra é obrigatório")
              .field("title")
              .build());
    }

    // Validar configuração simples ou complexa
    if (ruleData.getRuleType() == RuleType.SIMPLE) {
      if (ruleData.getSimpleConfig() == null
          || ruleData.getSimpleConfig().getConditions() == null
          || ruleData.getSimpleConfig().getConditions().isEmpty()) {
        errors.add(
            ImportError.builder()
                .ruleKey(ruleData.getKey())
                .errorType("VALIDATION_ERROR")
                .message("Regra simples deve ter pelo menos uma condição")
                .field("simpleConfig.conditions")
                .build());
      }
    } else if (ruleData.getRuleType() == RuleType.COMPLEX) {
      if (ruleData.getComplexConfig() == null
          || ruleData.getComplexConfig().getRootConditionGroup() == null) {
        errors.add(
            ImportError.builder()
                .ruleKey(ruleData.getKey())
                .errorType("VALIDATION_ERROR")
                .message("Regra complexa deve ter grupo de condições raiz")
                .field("complexConfig.rootConditionGroup")
                .build());
      }
    }

    return errors;
  }

  // ========== MÉTODOS AUXILIARES ==========

  private RuleData convertSimpleRuleToExport(RuleConfiguration rule) {
    List<SimpleCondition> conditions = new ArrayList<>();

    // Parsear condições do JSON
    if (rule.getConditionsJson() != null) {
      try {
        List<Map<String, Object>> condList = serializerPort.readConditionsJson(rule.getConditionsJson());

        for (Map<String, Object> cond : condList) {
          conditions.add(
              SimpleCondition.builder()
                  .field((String) cond.get("field"))
                  .operator((String) cond.get("operator"))
                  .value((String) cond.get("value"))
                  .build());
        }
      } catch (Exception ignored) {
        // Ignorar erros de parse
      }
    }

    return RuleData.builder()
        .key(rule.getRuleName())
        .title(rule.getRuleName())
        .description(rule.getDescription())
        .version(rule.getVersion())
        .status(rule.getEnabled() ? "PUBLISHED" : "DRAFT")
        .priority(rule.getThreshold())
        .severity(rule.getWeight())
        .decision(rule.getClassification() != null ? rule.getClassification().name() : null)
        .enabled(rule.getEnabled())
        .ruleType(RuleType.SIMPLE)
        .simpleConfig(
            SimpleRuleConfig.builder()
                .logicOperator(
                    rule.getLogicOperator() != null ? rule.getLogicOperator().name() : "AND")
                .conditions(conditions)
                .weight(rule.getWeight())
                .classification(
                    rule.getClassification() != null ? rule.getClassification().name() : null)
                .build())
        .createdAt(
            rule.getCreatedAt() != null
                ? rule.getCreatedAt().atOffset(java.time.ZoneOffset.UTC)
                : null)
        .build();
  }

  private RuleData convertHomologRuleToExport(RuleEntity rule, RuleVersionEntity version) {
    List<SimpleCondition> conditions = new ArrayList<>();

    // Parsear condições do JSON
    if (version.getConditionsJson() != null) {
      try {
        List<Map<String, Object>> condList = serializerPort.readConditionsJson(version.getConditionsJson());

        for (Map<String, Object> cond : condList) {
          conditions.add(
              SimpleCondition.builder()
                  .field((String) cond.get("field"))
                  .operator((String) cond.get("operator"))
                  .value((String) cond.get("value"))
                  .build());
        }
      } catch (Exception ignored) {
        // Ignorar erros de parse
      }
    }

    return RuleData.builder()
        .key(rule.getKey())
        .title(rule.getTitle())
        .description(version.getReasonTemplate())
        .version(version.getVersion())
        .status(version.getStatus() != null ? version.getStatus().name() : null)
        .priority(version.getPriority())
        .severity(version.getSeverity())
        .decision(version.getDecision() != null ? version.getDecision().name() : null)
        .enabled(version.getEnabled())
        .ruleType(RuleType.SIMPLE)
        .simpleConfig(
            SimpleRuleConfig.builder()
                .logicOperator(version.getLogic() != null ? version.getLogic().name() : "AND")
                .conditions(conditions)
                .weight(version.getSeverity())
                .classification(version.getDecision() != null ? version.getDecision().name() : null)
                .build())
        .fieldsUsed(version.getFieldsUsed() != null ? Arrays.asList(version.getFieldsUsed()) : null)
        .createdAt(version.getCreatedAt())
        .build();
  }

  private RuleData convertToComplexExport(
      RuleEntity rule, RuleVersionEntity version, ConditionGroupDTO conditionGroup) {

    // Buscar expressões, variáveis e ações
    List<ExpressionDTO> expressions = complexRulePort.getExpressions(version.getId());
    List<ContextVariableDTO> variables = complexRulePort.getContextVariables(version.getId());
    List<RuleActionDTO> actions = complexRulePort.getActions(version.getId());

    return RuleData.builder()
        .key(rule.getKey())
        .title(rule.getTitle())
        .description(version.getReasonTemplate())
        .version(version.getVersion())
        .status(version.getStatus().name())
        .priority(version.getPriority())
        .severity(version.getSeverity())
        .decision(version.getDecision().name())
        .enabled(version.getEnabled())
        .ruleType(RuleType.COMPLEX)
        .complexConfig(
            ComplexRuleConfig.builder()
                .rootConditionGroup(conditionGroup)
                .expressions(expressions)
                .contextVariables(variables)
                .actions(actions)
                .build())
        .fieldsUsed(complexRulePort.getFieldsUsed(version.getId()))
        .createdAt(version.getCreatedAt())
        .build();
  }

  private void importSimpleRule(RuleData ruleData, String importedBy, boolean exists) {
    SimpleRuleConfig config = ruleData.getSimpleConfig();

    // Converter condições para JSON
    String conditionsJson;
    try {
      conditionsJson = serializerPort.writeConditionsJson(config.getConditions());
    } catch (JsonProcessingException e) {
      conditionsJson = "[]";
    }

    // Converter strings para enums
    RuleConfiguration.LogicOperator logicOp =
        config.getLogicOperator() != null
            ? RuleConfiguration.LogicOperator.valueOf(config.getLogicOperator())
            : RuleConfiguration.LogicOperator.AND;

    TransactionDecision.TransactionClassification classification =
        config.getClassification() != null
            ? TransactionDecision.TransactionClassification.valueOf(config.getClassification())
            : TransactionDecision.TransactionClassification.SUSPICIOUS;

    if (exists) {
      // Atualizar regra existente
      final String finalConditionsJson = conditionsJson;
      repositoryPort
          .findSimpleRuleByName(ruleData.getKey())
          .ifPresent(
              rule -> {
                rule.setDescription(ruleData.getDescription());
                rule.setConditionsJson(finalConditionsJson);
                rule.setLogicOperator(logicOp);
                rule.setWeight(config.getWeight());
                rule.setClassification(classification);
                rule.setEnabled(ruleData.getEnabled());
                rule.setVersion(rule.getVersion() + 1);
                repositoryPort.saveSimpleRule(rule);
              });
    } else {
      // Criar nova regra
      RuleConfiguration newRule = new RuleConfiguration();
      newRule.setRuleName(ruleData.getKey());
      newRule.setDescription(ruleData.getDescription());
      newRule.setRuleType(RuleConfiguration.RuleType.SECURITY);
      newRule.setConditionsJson(conditionsJson);
      newRule.setLogicOperator(logicOp);
      newRule.setThreshold(ruleData.getPriority() != null ? ruleData.getPriority() : 0);
      newRule.setWeight(config.getWeight() != null ? config.getWeight() : 50);
      newRule.setClassification(classification);
      newRule.setEnabled(ruleData.getEnabled() != null ? ruleData.getEnabled() : true);
      newRule.setVersion(1);
      repositoryPort.saveSimpleRule(newRule);
    }
  }

  private void importComplexRule(RuleData ruleData, String importedBy, boolean exists) {
    ComplexRuleConfig config = ruleData.getComplexConfig();

    RuleEntity rule;
    if (exists) {
      rule = repositoryPort.findHomologRuleByKey(ruleData.getKey()).orElseThrow();
    } else {
      rule = RuleEntity.builder().key(ruleData.getKey()).title(ruleData.getTitle()).build();
      rule = repositoryPort.saveHomologRule(rule);
    }

    // Criar nova versão
    RuleVersionEntity version =
        RuleVersionEntity.builder()
            .ruleId(rule.getId())
            .version(exists ? getNextVersion(rule.getId()) : 1)
            .status(RuleStatus.DRAFT)
            .priority(ruleData.getPriority() != null ? ruleData.getPriority() : 0)
            .severity(ruleData.getSeverity() != null ? ruleData.getSeverity() : 50)
            .decision(
                DecisionOutcome.valueOf(
                    ruleData.getDecision() != null ? ruleData.getDecision() : "SUSPEITA_DE_FRAUDE"))
            .reasonTemplate(ruleData.getDescription())
            .fieldsUsed(
                ruleData.getFieldsUsed() != null
                    ? ruleData.getFieldsUsed().toArray(new String[0])
                    : new String[0])
            .logic(LogicOperator.AND)
            .conditionsJson("{}")
            .enabled(ruleData.getEnabled() != null ? ruleData.getEnabled() : true)
            .build();
    version = repositoryPort.saveRuleVersion(version);

    // Salvar estrutura complexa
    if (config.getRootConditionGroup() != null) {
      complexRulePort.saveConditionGroup(version.getId(), config.getRootConditionGroup());
    }
    if (config.getExpressions() != null && !config.getExpressions().isEmpty()) {
      complexRulePort.saveExpressions(version.getId(), config.getExpressions());
    }
    if (config.getContextVariables() != null && !config.getContextVariables().isEmpty()) {
      complexRulePort.saveContextVariables(version.getId(), config.getContextVariables());
    }
    if (config.getActions() != null && !config.getActions().isEmpty()) {
      complexRulePort.saveActions(version.getId(), config.getActions());
    }
  }

  private int getNextVersion(UUID ruleId) {
    return repositoryPort.findRuleVersionsByRuleId(ruleId).stream()
        .findFirst()
        .map(v -> v.getVersion() + 1)
        .orElse(1);
  }
}
