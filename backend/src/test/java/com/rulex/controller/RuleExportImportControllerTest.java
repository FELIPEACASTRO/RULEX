package com.rulex.controller;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.rulex.dto.RuleExportDTO;
import com.rulex.dto.RuleExportDTO.ImportResult;
import com.rulex.dto.RuleExportDTO.RuleData;
import com.rulex.dto.RuleExportDTO.RuleType;
import com.rulex.core.rules.port.RuleExportImportInputPort;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.flyway.FlywayAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;

@SpringBootTest(classes = RuleExportImportControllerTest.TestApplication.class)
@AutoConfigureMockMvc(addFilters = false)
class RuleExportImportControllerTest {

  @SpringBootConfiguration
  @EnableAutoConfiguration(
      exclude = {
        DataSourceAutoConfiguration.class,
        FlywayAutoConfiguration.class,
        HibernateJpaAutoConfiguration.class
      })
  @Import(RuleExportImportController.class)
  static class TestApplication {}

  @Autowired private MockMvc mockMvc;

  @SuppressWarnings("removal")
  @MockBean private RuleExportImportInputPort exportImportService;

  @Test
  void exportAllRulesDownloadReturnsOk() throws Exception {
    RuleExportDTO export =
        RuleExportDTO.builder()
            .rules(List.of(RuleData.builder().key("RULE_1").title("Rule 1").ruleType(RuleType.SIMPLE).build()))
            .build();

    when(exportImportService.exportAllRules(eq("json"), eq("user"))).thenReturn(export);
    when(exportImportService.toJson(eq(export))).thenReturn("{}");

    mockMvc
        .perform(
            get("/v1/rules/export-import/export")
                .param("format", "json")
                .param("exportedBy", "user")
                .param("download", "true"))
        .andExpect(status().isOk())
        .andExpect(header().string(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"rulex-rules-export.json\""));

    verify(exportImportService).exportAllRules("json", "user");
  }

  @Test
  void importRulesReturnsOk() throws Exception {
    ImportResult result = ImportResult.builder().totalProcessed(1).successCount(1).build();
    when(exportImportService.importFromJson(anyString(), any(), eq("user"))).thenReturn(result);

    mockMvc
        .perform(
            post("/v1/rules/export-import/import")
                .contentType(MediaType.APPLICATION_JSON)
                .param("format", "json")
                .param("importedBy", "user")
                .content("{}"))
        .andExpect(status().isOk());

    verify(exportImportService).importFromJson(anyString(), any(), eq("user"));
  }

  @Test
  void importFromFileReturnsOk() throws Exception {
    ImportResult result = ImportResult.builder().totalProcessed(1).successCount(1).build();
    when(exportImportService.importFromJson(anyString(), any(), eq("user"))).thenReturn(result);

    MockMultipartFile file =
        new MockMultipartFile(
            "file",
            "rules.json",
            MediaType.APPLICATION_JSON_VALUE,
            "{}".getBytes(StandardCharsets.UTF_8));

    mockMvc
        .perform(
            multipart("/v1/rules/export-import/import/file")
                .file(file)
                .param("importedBy", "user"))
        .andExpect(status().isOk());

    verify(exportImportService).importFromJson(anyString(), any(), eq("user"));
  }

  @Test
  void validateRulesReturnsOk() throws Exception {
    ImportResult result = ImportResult.builder().totalProcessed(1).successCount(1).build();
    when(exportImportService.importFromJson(anyString(), any(), eq(null))).thenReturn(result);

    mockMvc
        .perform(
            post("/v1/rules/export-import/validate")
                .contentType(MediaType.APPLICATION_JSON)
                .param("format", "json")
                .content("{}"))
        .andExpect(status().isOk());

    verify(exportImportService).importFromJson(anyString(), any(), eq(null));
  }

  @Test
  void templateEndpointsReturnOk() throws Exception {
    mockMvc.perform(get("/v1/rules/export-import/template/simple")).andExpect(status().isOk());
    mockMvc.perform(get("/v1/rules/export-import/template/complex")).andExpect(status().isOk());
  }
}
