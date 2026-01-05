package com.rulex.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.rulex.dto.RuleExportDTO;
import com.rulex.dto.RuleExportDTO.*;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import com.rulex.entity.homolog.*;
import com.rulex.repository.RuleConfigurationRepository;
import com.rulex.repository.homolog.*;
import com.rulex.service.complex.ComplexRuleService;
import java.time.OffsetDateTime;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

/**
 * Testes para o RuleExportImportService.
 *
 * <p>GAP-FIX #3: Cobertura de testes para classes críticas.
 *
 * <p>O RuleExportImportService permite:
 *
 * <ul>
 *   <li>Exportar regras em formato JSON ou YAML
 *   <li>Importar regras de arquivos JSON ou YAML
 *   <li>Validar regras antes da importação
 *   <li>Suportar regras simples e complexas
 * </ul>
 */
@DisplayName("RuleExportImportService Tests")
class RuleExportImportServiceTest {

  private RuleConfigurationRepository ruleConfigRepository;
  private RuleRepository ruleRepository;
  private RuleVersionRepository ruleVersionRepository;
  private ComplexRuleService complexRuleService;
  private ObjectMapper objectMapper;
  private RuleExportImportService exportImportService;

  @BeforeEach
  void setUp() {
    ruleConfigRepository = Mockito.mock(RuleConfigurationRepository.class);
    ruleRepository = Mockito.mock(RuleRepository.class);
    ruleVersionRepository = Mockito.mock(RuleVersionRepository.class);
    complexRuleService = Mockito.mock(ComplexRuleService.class);
    objectMapper = new ObjectMapper().registerModule(new JavaTimeModule());

    exportImportService =
        new RuleExportImportService(
            ruleConfigRepository,
            ruleRepository,
            ruleVersionRepository,
            complexRuleService,
            objectMapper);
  }

  @Nested
  @DisplayName("Exportação de Regras")
  class RuleExport {

    @Test
    @DisplayName("Deve exportar todas as regras")
    void shouldExportAllRules() {
      RuleConfiguration simpleRule = createSimpleRule("RULE_1");
      when(ruleConfigRepository.findAll()).thenReturn(List.of(simpleRule));
      when(ruleRepository.findAll()).thenReturn(List.of());

      RuleExportDTO export = exportImportService.exportAllRules("JSON", "test-user");

      assertThat(export).isNotNull();
      assertThat(export.getMetadata()).isNotNull();
      assertThat(export.getMetadata().getVersion()).isEqualTo("1.0");
      assertThat(export.getMetadata().getExportedBy()).isEqualTo("test-user");
      assertThat(export.getMetadata().getSourceSystem()).isEqualTo("RULEX");
      assertThat(export.getRules()).hasSize(1);
    }

    @Test
    @DisplayName("Deve exportar regras específicas por chaves")
    void shouldExportRulesByKeys() {
      RuleConfiguration rule1 = createSimpleRule("RULE_1");
      when(ruleConfigRepository.findByRuleName("RULE_1")).thenReturn(Optional.of(rule1));
      when(ruleConfigRepository.findByRuleName("RULE_2")).thenReturn(Optional.empty());
      when(ruleRepository.findByKey("RULE_2")).thenReturn(Optional.empty());

      RuleExportDTO export =
          exportImportService.exportRulesByKeys(List.of("RULE_1", "RULE_2"), "JSON", "test-user");

      assertThat(export.getRules()).hasSize(1);
      assertThat(export.getRules().get(0).getKey()).isEqualTo("RULE_1");
    }

    @Test
    @DisplayName("Deve incluir metadata completa na exportação")
    void shouldIncludeCompleteMetadata() {
      when(ruleConfigRepository.findAll()).thenReturn(List.of());
      when(ruleRepository.findAll()).thenReturn(List.of());

      RuleExportDTO export = exportImportService.exportAllRules("JSON", "admin");

      assertThat(export.getMetadata().getExportedAt()).isNotNull();
      assertThat(export.getMetadata().getTotalRules()).isEqualTo(0);
      assertThat(export.getMetadata().getDescription()).contains("RULEX");
    }

    @Test
    @DisplayName("Deve exportar regras do sistema de homologação")
    void shouldExportHomologRules() {
      UUID ruleId = UUID.randomUUID();
      RuleEntity homologRule =
          RuleEntity.builder().id(ruleId).key("HOMOLOG_RULE").title("Homolog Rule Title").build();

      RuleVersionEntity version =
          RuleVersionEntity.builder()
              .id(UUID.randomUUID())
              .ruleId(ruleId)
              .version(1)
              .status(RuleStatus.PUBLISHED)
              .priority(10)
              .severity(50)
              .decision(DecisionOutcome.SUSPEITA_DE_FRAUDE)
              .reasonTemplate("Test reason")
              .logic(LogicOperator.AND)
              .conditionsJson("[{\"field\":\"mcc\",\"operator\":\"==\",\"value\":\"5411\"}]")
              .enabled(true)
              .build();

      when(ruleConfigRepository.findAll()).thenReturn(List.of());
      when(ruleRepository.findAll()).thenReturn(List.of(homologRule));
      when(ruleVersionRepository.findByRuleIdOrderByVersionDesc(ruleId))
          .thenReturn(List.of(version));

      RuleExportDTO export = exportImportService.exportAllRules("JSON", "test-user");

      assertThat(export.getRules()).hasSize(1);
      assertThat(export.getRules().get(0).getKey()).isEqualTo("HOMOLOG_RULE");
    }
  }

  @Nested
  @DisplayName("Conversão de Formatos")
  class FormatConversion {

    @Test
    @DisplayName("Deve converter export para JSON")
    void shouldConvertExportToJson() throws JsonProcessingException {
      RuleExportDTO export = createSampleExport();

      String json = exportImportService.toJson(export);

      assertThat(json).isNotNull();
      assertThat(json).contains("\"version\"");
      assertThat(json).contains("\"rules\"");
      assertThat(json).contains("TEST_RULE");
    }

    @Test
    @DisplayName("Deve converter export para YAML")
    void shouldConvertExportToYaml() throws JsonProcessingException {
      RuleExportDTO export = createSampleExport();

      String yaml = exportImportService.toYaml(export);

      assertThat(yaml).isNotNull();
      assertThat(yaml).contains("version:");
      assertThat(yaml).contains("rules:");
      assertThat(yaml).contains("TEST_RULE");
    }
  }

  @Nested
  @DisplayName("Importação de Regras")
  class RuleImport {

    @Test
    @DisplayName("Deve importar regras de JSON")
    void shouldImportRulesFromJson() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options =
          ImportOptions.builder()
              .overwriteExisting(false)
              .dryRun(false)
              .importDisabled(true)
              .build();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());
      when(ruleConfigRepository.save(any())).thenAnswer(inv -> inv.getArgument(0));

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result).isNotNull();
      assertThat(result.getTotalProcessed()).isGreaterThan(0);
    }

    @Test
    @DisplayName("Deve pular regras existentes quando overwrite é false")
    void shouldSkipExistingRulesWhenOverwriteIsFalse() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options =
          ImportOptions.builder().overwriteExisting(false).dryRun(false).build();

      // Regra já existe
      when(ruleConfigRepository.findByRuleName("TEST_RULE"))
          .thenReturn(Optional.of(createSimpleRule("TEST_RULE")));

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getSkippedCount()).isGreaterThan(0);
    }

    @Test
    @DisplayName("Deve sobrescrever regras existentes quando overwrite é true")
    void shouldOverwriteExistingRulesWhenOverwriteIsTrue() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options = ImportOptions.builder().overwriteExisting(true).dryRun(false).build();

      RuleConfiguration existingRule = createSimpleRule("TEST_RULE");
      when(ruleConfigRepository.findByRuleName("TEST_RULE")).thenReturn(Optional.of(existingRule));
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());
      when(ruleConfigRepository.save(any())).thenAnswer(inv -> inv.getArgument(0));

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getSuccessCount()).isGreaterThan(0);
    }

    @Test
    @DisplayName("Deve executar dry run sem persistir")
    void shouldExecuteDryRunWithoutPersisting() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options = ImportOptions.builder().overwriteExisting(false).dryRun(true).build();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getSuccessCount()).isGreaterThan(0);
      // Não deve ter chamado save
      Mockito.verify(ruleConfigRepository, Mockito.never()).save(any());
    }

    @Test
    @DisplayName("Deve aplicar prefixo às chaves")
    void shouldApplyPrefixToKeys() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options =
          ImportOptions.builder()
              .overwriteExisting(false)
              .dryRun(false)
              .keyPrefix("PREFIX_")
              .build();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());
      when(ruleConfigRepository.save(any())).thenAnswer(inv -> inv.getArgument(0));

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getImportedRuleKeys()).anyMatch(key -> key.startsWith("PREFIX_"));
    }

    @Test
    @DisplayName("Deve aplicar sufixo às chaves")
    void shouldApplySuffixToKeys() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options =
          ImportOptions.builder()
              .overwriteExisting(false)
              .dryRun(false)
              .keySuffix("_SUFFIX")
              .build();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());
      when(ruleConfigRepository.save(any())).thenAnswer(inv -> inv.getArgument(0));

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getImportedRuleKeys()).anyMatch(key -> key.endsWith("_SUFFIX"));
    }
  }

  @Nested
  @DisplayName("Validação de Regras")
  class RuleValidation {

    @Test
    @DisplayName("Deve validar regra sem chave")
    void shouldValidateRuleWithoutKey() {
      RuleData ruleData =
          RuleData.builder()
              .key(null)
              .title("Test Rule")
              .ruleType(RuleType.SIMPLE)
              .simpleConfig(
                  SimpleRuleConfig.builder()
                      .conditions(
                          List.of(
                              SimpleCondition.builder()
                                  .field("mcc")
                                  .operator("==")
                                  .value("5411")
                                  .build()))
                      .build())
              .build();

      List<ImportError> errors = exportImportService.validateRuleData(ruleData);

      assertThat(errors).isNotEmpty();
      assertThat(errors).anyMatch(e -> e.getField().equals("key"));
    }

    @Test
    @DisplayName("Deve validar regra sem título")
    void shouldValidateRuleWithoutTitle() {
      RuleData ruleData =
          RuleData.builder()
              .key("TEST_RULE")
              .title(null)
              .ruleType(RuleType.SIMPLE)
              .simpleConfig(
                  SimpleRuleConfig.builder()
                      .conditions(
                          List.of(
                              SimpleCondition.builder()
                                  .field("mcc")
                                  .operator("==")
                                  .value("5411")
                                  .build()))
                      .build())
              .build();

      List<ImportError> errors = exportImportService.validateRuleData(ruleData);

      assertThat(errors).isNotEmpty();
      assertThat(errors).anyMatch(e -> e.getField().equals("title"));
    }

    @Test
    @DisplayName("Deve validar regra simples sem condições")
    void shouldValidateSimpleRuleWithoutConditions() {
      RuleData ruleData =
          RuleData.builder()
              .key("TEST_RULE")
              .title("Test Rule")
              .ruleType(RuleType.SIMPLE)
              .simpleConfig(SimpleRuleConfig.builder().conditions(List.of()).build())
              .build();

      List<ImportError> errors = exportImportService.validateRuleData(ruleData);

      assertThat(errors).isNotEmpty();
      assertThat(errors).anyMatch(e -> e.getField().equals("simpleConfig.conditions"));
    }

    @Test
    @DisplayName("Deve validar regra complexa sem grupo raiz")
    void shouldValidateComplexRuleWithoutRootGroup() {
      RuleData ruleData =
          RuleData.builder()
              .key("TEST_RULE")
              .title("Test Rule")
              .ruleType(RuleType.COMPLEX)
              .complexConfig(ComplexRuleConfig.builder().rootConditionGroup(null).build())
              .build();

      List<ImportError> errors = exportImportService.validateRuleData(ruleData);

      assertThat(errors).isNotEmpty();
      assertThat(errors).anyMatch(e -> e.getField().equals("complexConfig.rootConditionGroup"));
    }

    @Test
    @DisplayName("Deve passar validação para regra válida")
    void shouldPassValidationForValidRule() {
      RuleData ruleData =
          RuleData.builder()
              .key("VALID_RULE")
              .title("Valid Rule")
              .ruleType(RuleType.SIMPLE)
              .simpleConfig(
                  SimpleRuleConfig.builder()
                      .conditions(
                          List.of(
                              SimpleCondition.builder()
                                  .field("mcc")
                                  .operator("==")
                                  .value("5411")
                                  .build()))
                      .build())
              .build();

      List<ImportError> errors = exportImportService.validateRuleData(ruleData);

      assertThat(errors).isEmpty();
    }
  }

  @Nested
  @DisplayName("Resultado de Importação")
  class ImportResultTests {

    @Test
    @DisplayName("Deve incluir timestamp na importação")
    void shouldIncludeTimestampInImport() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options = ImportOptions.builder().dryRun(true).build();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getImportedAt()).isNotNull();
    }

    @Test
    @DisplayName("Deve incluir lista de chaves importadas")
    void shouldIncludeImportedKeysList() throws JsonProcessingException {
      String json = createSampleJson();
      ImportOptions options = ImportOptions.builder().dryRun(true).build();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());

      ImportResult result = exportImportService.importFromJson(json, options, "test-user");

      assertThat(result.getImportedRuleKeys()).isNotNull();
      assertThat(result.getImportedRuleKeys()).isNotEmpty();
    }

    @Test
    @DisplayName("Deve incluir erros de validação")
    void shouldIncludeValidationErrors() throws JsonProcessingException {
      String invalidJson =
          """
          {
            "metadata": {
              "version": "1.0",
              "exportedAt": "2025-01-05T12:00:00Z",
              "exportedBy": "test",
              "totalRules": 1,
              "sourceSystem": "RULEX"
            },
            "rules": [
              {
                "key": "",
                "title": "",
                "ruleType": "SIMPLE",
                "simpleConfig": {
                  "conditions": []
                }
              }
            ]
          }
          """;

      ImportOptions options = ImportOptions.builder().dryRun(false).build();

      ImportResult result = exportImportService.importFromJson(invalidJson, options, "test-user");

      assertThat(result.getErrors()).isNotEmpty();
      assertThat(result.getFailureCount()).isGreaterThan(0);
    }
  }

  @Nested
  @DisplayName("Opções de Importação Padrão")
  class DefaultImportOptions {

    @Test
    @DisplayName("Deve usar opções padrão quando null")
    void shouldUseDefaultOptionsWhenNull() throws JsonProcessingException {
      String json = createSampleJson();

      when(ruleConfigRepository.findByRuleName(any())).thenReturn(Optional.empty());
      when(ruleRepository.findByKey(any())).thenReturn(Optional.empty());
      when(ruleConfigRepository.save(any())).thenAnswer(inv -> inv.getArgument(0));

      ImportResult result = exportImportService.importFromJson(json, null, "test-user");

      assertThat(result).isNotNull();
    }
  }

  // ========== Helpers ==========

  private RuleConfiguration createSimpleRule(String name) {
    return RuleConfiguration.builder()
        .id(1L)
        .ruleName(name)
        .description("Test rule description")
        .ruleType(RuleConfiguration.RuleType.SECURITY)
        .threshold(100)
        .weight(50)
        .enabled(true)
        .version(1)
        .classification(TransactionDecision.TransactionClassification.SUSPICIOUS)
        .logicOperator(RuleConfiguration.LogicOperator.AND)
        .conditionsJson("[{\"field\":\"mcc\",\"operator\":\"==\",\"value\":\"5411\"}]")
        .build();
  }

  private RuleExportDTO createSampleExport() {
    return RuleExportDTO.builder()
        .metadata(
            ExportMetadata.builder()
                .version("1.0")
                .exportedAt(OffsetDateTime.now())
                .exportedBy("test-user")
                .description("Test export")
                .totalRules(1)
                .sourceSystem("RULEX")
                .build())
        .rules(
            List.of(
                RuleData.builder()
                    .key("TEST_RULE")
                    .title("Test Rule")
                    .description("Test description")
                    .version(1)
                    .status("PUBLISHED")
                    .priority(10)
                    .severity(50)
                    .decision("SUSPICIOUS")
                    .enabled(true)
                    .ruleType(RuleType.SIMPLE)
                    .simpleConfig(
                        SimpleRuleConfig.builder()
                            .logicOperator("AND")
                            .conditions(
                                List.of(
                                    SimpleCondition.builder()
                                        .field("mcc")
                                        .operator("==")
                                        .value("5411")
                                        .build()))
                            .weight(50)
                            .classification("SUSPICIOUS")
                            .build())
                    .build()))
        .build();
  }

  private String createSampleJson() {
    return """
        {
          "metadata": {
            "version": "1.0",
            "exportedAt": "2025-01-05T12:00:00Z",
            "exportedBy": "test",
            "description": "Test export",
            "totalRules": 1,
            "sourceSystem": "RULEX"
          },
          "rules": [
            {
              "key": "TEST_RULE",
              "title": "Test Rule",
              "description": "Test description",
              "version": 1,
              "status": "PUBLISHED",
              "priority": 10,
              "severity": 50,
              "decision": "SUSPICIOUS",
              "enabled": true,
              "ruleType": "SIMPLE",
              "simpleConfig": {
                "logicOperator": "AND",
                "conditions": [
                  {
                    "field": "mcc",
                    "operator": "==",
                    "value": "5411"
                  }
                ],
                "weight": 50,
                "classification": "SUSPICIOUS"
              }
            }
          ]
        }
        """;
  }
}
