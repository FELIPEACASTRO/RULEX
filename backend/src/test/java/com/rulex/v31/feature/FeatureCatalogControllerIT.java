package com.rulex.v31.feature;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.testsupport.CorePostgresITSupport;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;

/**
 * Integration test for Feature Catalog API. Verifies that feature definitions seeded by V9
 * migration are accessible via REST endpoints.
 */
@SpringBootTest
@AutoConfigureMockMvc
class FeatureCatalogControllerIT extends CorePostgresITSupport {

  @Autowired private MockMvc mockMvc;

  @Autowired private ObjectMapper objectMapper;

  @Test
  void listAllFeatures_returnsSeededDefinitions() throws Exception {
    MvcResult result =
        mockMvc
            .perform(get("/feature-catalog"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$").isArray())
            .andReturn();

    List<Map<String, Object>> features =
        objectMapper.readValue(
            result.getResponse().getContentAsString(), new TypeReference<>() {});

    assertThat(features).isNotEmpty();

    // V9 seeds at least VELOCITY and TEMPORAL features
    assertThat(features)
        .anyMatch(f -> "VELOCITY".equals(f.get("featureType")))
        .anyMatch(f -> "TEMPORAL".equals(f.get("featureType")));
  }

  @Test
  void listFeatures_filterByType_returnsOnlyMatchingType() throws Exception {
    MvcResult result =
        mockMvc
            .perform(get("/feature-catalog").param("featureType", "VELOCITY"))
            .andExpect(status().isOk())
            .andReturn();

    List<Map<String, Object>> features =
        objectMapper.readValue(
            result.getResponse().getContentAsString(), new TypeReference<>() {});

    assertThat(features).isNotEmpty().allMatch(f -> "VELOCITY".equals(f.get("featureType")));
  }

  @Test
  void listFeatures_filterByEntityType_returnsOnlyMatchingEntity() throws Exception {
    MvcResult result =
        mockMvc
            .perform(get("/feature-catalog").param("entityType", "card"))
            .andExpect(status().isOk())
            .andReturn();

    List<Map<String, Object>> features =
        objectMapper.readValue(
            result.getResponse().getContentAsString(), new TypeReference<>() {});

    assertThat(features).isNotEmpty().allMatch(f -> "card".equals(f.get("entityType")));
  }

  @Test
  void getFeatureByName_existingFeature_returnsFeature() throws Exception {
    // First get a feature name from the list
    MvcResult listResult =
        mockMvc.perform(get("/feature-catalog")).andExpect(status().isOk()).andReturn();

    List<Map<String, Object>> features =
        objectMapper.readValue(
            listResult.getResponse().getContentAsString(), new TypeReference<>() {});

    assertThat(features).isNotEmpty();
    String featureName = (String) features.get(0).get("featureName");

    // Then fetch it by name
    mockMvc
        .perform(get("/feature-catalog/{featureName}", featureName))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.featureName").value(featureName));
  }

  @Test
  void getFeatureByName_nonExistent_returns404() throws Exception {
    mockMvc
        .perform(get("/feature-catalog/{featureName}", "non_existent_feature_xyz"))
        .andExpect(status().isNotFound());
  }

  @Test
  void getFeatureTypes_returnsValidTypes() throws Exception {
    mockMvc
        .perform(get("/feature-catalog/types"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").isArray())
        .andExpect(jsonPath("$[?(@=='VELOCITY')]").exists())
        .andExpect(jsonPath("$[?(@=='TEMPORAL')]").exists())
        .andExpect(jsonPath("$[?(@=='PAYLOAD_FIELD')]").exists());
  }

  @Test
  void getEntityTypes_returnsValidEntities() throws Exception {
    mockMvc
        .perform(get("/feature-catalog/entity-types"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").isArray())
        .andExpect(jsonPath("$[?(@=='card')]").exists())
        .andExpect(jsonPath("$[?(@=='customer')]").exists())
        .andExpect(jsonPath("$[?(@=='merchant')]").exists());
  }

  @Test
  void getSources_returnsValidSources() throws Exception {
    mockMvc
        .perform(get("/feature-catalog/sources"))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$").isArray())
        .andExpect(jsonPath("$[?(@=='payload')]").exists())
        .andExpect(jsonPath("$[?(@=='velocity_store')]").exists())
        .andExpect(jsonPath("$[?(@=='feature_store')]").exists());
  }
}
