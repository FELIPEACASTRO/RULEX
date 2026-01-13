package com.rulex.controller;

import com.rulex.dto.RuleExportDTO;
import com.rulex.dto.RuleExportDTO.*;
import com.rulex.service.RuleExportImportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/** Controller para exportação e importação de regras. Suporta formatos JSON e YAML. */
@RestController
@RequestMapping("/v1/rules/export-import")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Rule Export/Import", description = "APIs para exportação e importação de regras")
public class RuleExportImportController {

  private final RuleExportImportService exportImportService;

  // ========== EXPORT ==========

  @GetMapping("/export")
  @Operation(
      summary = "Exporta todas as regras",
      description = "Exporta todas as regras do sistema em JSON ou YAML")
  public ResponseEntity<String> exportAllRules(
      @Parameter(description = "Formato de saída: json ou yaml")
          @RequestParam(defaultValue = "json")
          String format,
      @Parameter(description = "Nome do usuário que está exportando")
          @RequestParam(required = false)
          String exportedBy,
      @Parameter(description = "Se true, faz download do arquivo")
          @RequestParam(defaultValue = "false")
          boolean download) {

    try {
      RuleExportDTO export = exportImportService.exportAllRules(format, exportedBy);
      String content =
          "yaml".equalsIgnoreCase(format)
              ? exportImportService.toYaml(export)
              : exportImportService.toJson(export);

      if (download) {
        String filename = "rulex-rules-export." + format.toLowerCase();
        return ResponseEntity.ok()
            .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
            .contentType(
                "yaml".equalsIgnoreCase(format)
                    ? MediaType.parseMediaType("application/x-yaml")
                    : MediaType.APPLICATION_JSON)
            .body(content);
      }

      return ResponseEntity.ok()
          .contentType(
              "yaml".equalsIgnoreCase(format)
                  ? MediaType.parseMediaType("application/x-yaml")
                  : MediaType.APPLICATION_JSON)
          .body(content);

    } catch (Exception e) {
      log.error("Erro ao exportar regras: {}", e.getMessage(), e);
      return ResponseEntity.internalServerError().body("{\"error\": \"" + e.getMessage() + "\"}");
    }
  }

  @PostMapping("/export/selective")
  @Operation(
      summary = "Exporta regras selecionadas",
      description = "Exporta apenas as regras especificadas por suas chaves")
  public ResponseEntity<String> exportSelectedRules(
      @RequestBody List<String> ruleKeys,
      @RequestParam(defaultValue = "json") String format,
      @RequestParam(required = false) String exportedBy,
      @RequestParam(defaultValue = "false") boolean download) {

    try {
      RuleExportDTO export = exportImportService.exportRulesByKeys(ruleKeys, format, exportedBy);
      String content =
          "yaml".equalsIgnoreCase(format)
              ? exportImportService.toYaml(export)
              : exportImportService.toJson(export);

      if (download) {
        String filename = "rulex-rules-selective-export." + format.toLowerCase();
        return ResponseEntity.ok()
            .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
            .contentType(
                "yaml".equalsIgnoreCase(format)
                    ? MediaType.parseMediaType("application/x-yaml")
                    : MediaType.APPLICATION_JSON)
            .body(content);
      }

      return ResponseEntity.ok()
          .contentType(
              "yaml".equalsIgnoreCase(format)
                  ? MediaType.parseMediaType("application/x-yaml")
                  : MediaType.APPLICATION_JSON)
          .body(content);

    } catch (Exception e) {
      log.error("Erro ao exportar regras selecionadas: {}", e.getMessage(), e);
      return ResponseEntity.internalServerError().body("{\"error\": \"" + e.getMessage() + "\"}");
    }
  }

  @GetMapping("/export/complex")
  @Operation(
      summary = "Exporta regras complexas",
      description = "Exporta apenas regras com estrutura complexa (grupos aninhados)")
  public ResponseEntity<String> exportComplexRules(
      @RequestParam(defaultValue = "json") String format,
      @RequestParam(required = false) String exportedBy,
      @RequestParam(defaultValue = "false") boolean download) {

    try {
      RuleExportDTO export = exportImportService.exportComplexRules(exportedBy);
      String content =
          "yaml".equalsIgnoreCase(format)
              ? exportImportService.toYaml(export)
              : exportImportService.toJson(export);

      if (download) {
        String filename = "rulex-complex-rules-export." + format.toLowerCase();
        return ResponseEntity.ok()
            .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + filename + "\"")
            .contentType(
                "yaml".equalsIgnoreCase(format)
                    ? MediaType.parseMediaType("application/x-yaml")
                    : MediaType.APPLICATION_JSON)
            .body(content);
      }

      return ResponseEntity.ok()
          .contentType(
              "yaml".equalsIgnoreCase(format)
                  ? MediaType.parseMediaType("application/x-yaml")
                  : MediaType.APPLICATION_JSON)
          .body(content);

    } catch (Exception e) {
      log.error("Erro ao exportar regras complexas: {}", e.getMessage(), e);
      return ResponseEntity.internalServerError().body("{\"error\": \"" + e.getMessage() + "\"}");
    }
  }

  // ========== IMPORT ==========

  @PostMapping("/import")
  @Operation(
      summary = "Importa regras de JSON/YAML",
      description = "Importa regras de um arquivo JSON ou YAML")
  public ResponseEntity<ImportResult> importRules(
      @RequestBody String content,
      @Parameter(description = "Formato do conteúdo: json ou yaml")
          @RequestParam(defaultValue = "json")
          String format,
      @Parameter(description = "Se true, sobrescreve regras existentes")
          @RequestParam(defaultValue = "false")
          boolean overwriteExisting,
      @Parameter(description = "Se true, apenas valida sem importar")
          @RequestParam(defaultValue = "false")
          boolean dryRun,
      @Parameter(description = "Prefixo para adicionar às chaves") @RequestParam(required = false)
          String keyPrefix,
      @Parameter(description = "Sufixo para adicionar às chaves") @RequestParam(required = false)
          String keySuffix,
      @RequestParam(required = false) String importedBy) {

    try {
      ImportOptions options =
          ImportOptions.builder()
              .overwriteExisting(overwriteExisting)
              .dryRun(dryRun)
              .importDisabled(true)
              .keyPrefix(keyPrefix)
              .keySuffix(keySuffix)
              .build();

      ImportResult result =
          "yaml".equalsIgnoreCase(format)
              ? exportImportService.importFromYaml(content, options, importedBy)
              : exportImportService.importFromJson(content, options, importedBy);

      return ResponseEntity.ok(result);

    } catch (Exception e) {
      log.error("Erro ao importar regras: {}", e.getMessage(), e);
      return ResponseEntity.badRequest()
          .body(
              ImportResult.builder()
                  .totalProcessed(0)
                  .successCount(0)
                  .failureCount(1)
                  .errors(
                      List.of(
                          ImportError.builder()
                              .errorType("PARSE_ERROR")
                              .message(e.getMessage())
                              .build()))
                  .build());
    }
  }

  @PostMapping(value = "/import/file", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
  @Operation(
      summary = "Importa regras de arquivo",
      description = "Importa regras de um arquivo enviado")
  public ResponseEntity<ImportResult> importFromFile(
      @RequestParam("file") MultipartFile file,
      @RequestParam(defaultValue = "false") boolean overwriteExisting,
      @RequestParam(defaultValue = "false") boolean dryRun,
      @RequestParam(required = false) String keyPrefix,
      @RequestParam(required = false) String keySuffix,
      @RequestParam(required = false) String importedBy) {

    try {
      String content = new String(file.getBytes());
      String filename = file.getOriginalFilename();
      String format =
          filename != null && filename.endsWith(".yaml")
                  || filename != null && filename.endsWith(".yml")
              ? "yaml"
              : "json";

      ImportOptions options =
          ImportOptions.builder()
              .overwriteExisting(overwriteExisting)
              .dryRun(dryRun)
              .importDisabled(true)
              .keyPrefix(keyPrefix)
              .keySuffix(keySuffix)
              .build();

      ImportResult result =
          "yaml".equalsIgnoreCase(format)
              ? exportImportService.importFromYaml(content, options, importedBy)
              : exportImportService.importFromJson(content, options, importedBy);

      return ResponseEntity.ok(result);

    } catch (Exception e) {
      log.error("Erro ao importar arquivo: {}", e.getMessage(), e);
      return ResponseEntity.badRequest()
          .body(
              ImportResult.builder()
                  .totalProcessed(0)
                  .successCount(0)
                  .failureCount(1)
                  .errors(
                      List.of(
                          ImportError.builder()
                              .errorType("FILE_ERROR")
                              .message(e.getMessage())
                              .build()))
                  .build());
    }
  }

  // ========== VALIDATION ==========

  @PostMapping("/validate")
  @Operation(
      summary = "Valida arquivo de regras",
      description = "Valida um arquivo de regras sem importar")
  public ResponseEntity<ImportResult> validateRules(
      @RequestBody String content, @RequestParam(defaultValue = "json") String format) {

    try {
      ImportOptions options = ImportOptions.builder().dryRun(true).build();

      ImportResult result =
          "yaml".equalsIgnoreCase(format)
              ? exportImportService.importFromYaml(content, options, null)
              : exportImportService.importFromJson(content, options, null);

      return ResponseEntity.ok(result);

    } catch (Exception e) {
      log.error("Erro ao validar regras: {}", e.getMessage(), e);
      return ResponseEntity.badRequest()
          .body(
              ImportResult.builder()
                  .totalProcessed(0)
                  .successCount(0)
                  .failureCount(1)
                  .errors(
                      List.of(
                          ImportError.builder()
                              .errorType("VALIDATION_ERROR")
                              .message(e.getMessage())
                              .build()))
                  .build());
    }
  }

  // ========== TEMPLATES ==========

  @GetMapping("/template/simple")
  @Operation(
      summary = "Template de regra simples",
      description = "Retorna um template de regra simples para referência")
  public ResponseEntity<RuleData> getSimpleRuleTemplate() {
    return ResponseEntity.ok(
        RuleData.builder()
            .key("EXAMPLE_SIMPLE_RULE")
            .title("Exemplo de Regra Simples")
            .description("Esta é uma regra de exemplo com condições lineares")
            .version(1)
            .status("DRAFT")
            .priority(50)
            .severity(70)
            .decision("SUSPEITA_DE_FRAUDE")
            .enabled(true)
            .ruleType(RuleType.SIMPLE)
            .simpleConfig(
                SimpleRuleConfig.builder()
                    .logicOperator("AND")
                    .conditions(
                        List.of(
                            SimpleCondition.builder()
                                .field("transactionAmount")
                                .operator("GT")
                                .value("5000")
                                .build(),
                            SimpleCondition.builder()
                                .field("merchantCountryCode")
                                .operator("NEQ")
                                .value("076")
                                .build()))
                    .weight(70)
                    .classification("SUSPICIOUS")
                    .build())
            .fieldsUsed(List.of("transactionAmount", "merchantCountryCode"))
            .tags(List.of("HIGH_VALUE", "INTERNATIONAL"))
            .build());
  }

  @GetMapping("/template/complex")
  @Operation(
      summary = "Template de regra complexa",
      description = "Retorna um template de regra complexa para referência")
  public ResponseEntity<RuleData> getComplexRuleTemplate() {
    return ResponseEntity.ok(
        RuleData.builder()
            .key("EXAMPLE_COMPLEX_RULE")
            .title("Exemplo de Regra Complexa")
            .description("Esta é uma regra de exemplo com grupos aninhados")
            .version(1)
            .status("DRAFT")
            .priority(80)
            .severity(90)
            .decision("FRAUDE")
            .enabled(true)
            .ruleType(RuleType.COMPLEX)
            .complexConfig(
                ComplexRuleConfig.builder()
                    .rootConditionGroup(
                        com.rulex.dto.complex.ConditionGroupDTO.builder()
                            .logicOperator(
                                com.rulex.dto.complex.ConditionGroupDTO.LogicOperatorType.OR)
                            .name("Grupo Raiz")
                            .description("(Condição A AND B) OR (Condição C AND D)")
                            .enabled(true)
                            .conditions(List.of())
                            .children(
                                List.of(
                                    com.rulex.dto.complex.ConditionGroupDTO.builder()
                                        .logicOperator(
                                            com.rulex.dto.complex.ConditionGroupDTO
                                                .LogicOperatorType.AND)
                                        .name("Grupo 1")
                                        .conditions(
                                            List.of(
                                                com.rulex.dto.complex.ConditionDTO.builder()
                                                    .fieldName("transactionAmount")
                                                    .operator(
                                                        com.rulex.dto.complex.ConditionDTO
                                                            .OperatorType.GT)
                                                    .valueType(
                                                        com.rulex.dto.complex.ConditionDTO.ValueType
                                                            .NUMBER)
                                                    .valueSingle("10000")
                                                    .enabled(true)
                                                    .build(),
                                                com.rulex.dto.complex.ConditionDTO.builder()
                                                    .fieldName("consumerAuthenticationScore")
                                                    .operator(
                                                        com.rulex.dto.complex.ConditionDTO
                                                            .OperatorType.LT)
                                                    .valueType(
                                                        com.rulex.dto.complex.ConditionDTO.ValueType
                                                            .NUMBER)
                                                    .valueSingle("30")
                                                    .enabled(true)
                                                    .build()))
                                        .enabled(true)
                                        .build(),
                                    com.rulex.dto.complex.ConditionGroupDTO.builder()
                                        .logicOperator(
                                            com.rulex.dto.complex.ConditionGroupDTO
                                                .LogicOperatorType.AND)
                                        .name("Grupo 2")
                                        .conditions(
                                            List.of(
                                                com.rulex.dto.complex.ConditionDTO.builder()
                                                    .fieldName("mcc")
                                                    .operator(
                                                        com.rulex.dto.complex.ConditionDTO
                                                            .OperatorType.IN)
                                                    .valueType(
                                                        com.rulex.dto.complex.ConditionDTO.ValueType
                                                            .ARRAY_NUMBER)
                                                    .valueArray(List.of("7995", "6211", "6051"))
                                                    .enabled(true)
                                                    .build(),
                                                com.rulex.dto.complex.ConditionDTO.builder()
                                                    .fieldName("cavvResult")
                                                    .operator(
                                                        com.rulex.dto.complex.ConditionDTO
                                                            .OperatorType.NEQ)
                                                    .valueType(
                                                        com.rulex.dto.complex.ConditionDTO.ValueType
                                                            .NUMBER)
                                                    .valueSingle("0")
                                                    .enabled(true)
                                                    .build()))
                                        .enabled(true)
                                        .build()))
                            .build())
                    .build())
            .fieldsUsed(
                List.of("transactionAmount", "consumerAuthenticationScore", "mcc", "cavvResult"))
            .tags(List.of("COMPLEX", "HIGH_RISK"))
            .build());
  }
}
