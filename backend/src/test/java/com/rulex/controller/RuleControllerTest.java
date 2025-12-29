package com.rulex.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.RuleEntity;
import com.rulex.service.RuleConfigurationService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(RuleController.class)
@DisplayName("RuleController Tests")
class RuleControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private RuleConfigurationService ruleConfigurationService;

    private RuleEntity testRule;

    @BeforeEach
    void setUp() {
        testRule = new RuleEntity();
        testRule.setId(1L);
        testRule.setName("HIGH_VALUE_RULE");
        testRule.setDescription("Block high value transactions");
        testRule.setEnabled(true);
        testRule.setExpression("amount > 10000");
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should get all rules")
    void shouldGetAllRules() throws Exception {
        List<RuleEntity> rules = Arrays.asList(testRule);
        Page<RuleEntity> page = new PageImpl<>(rules);
        when(ruleConfigurationService.getAllRules(any(PageRequest.class))).thenReturn(page);

        mockMvc.perform(get("/rules")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.content").isArray())
                .andExpect(jsonPath("$.content[0].name").value("HIGH_VALUE_RULE"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should get rule by ID")
    void shouldGetRuleById() throws Exception {
        when(ruleConfigurationService.getRuleById(1L)).thenReturn(Optional.of(testRule));

        mockMvc.perform(get("/rules/1")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.name").value("HIGH_VALUE_RULE"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should return 404 when rule not found")
    void shouldReturn404WhenRuleNotFound() throws Exception {
        when(ruleConfigurationService.getRuleById(999L)).thenReturn(Optional.empty());

        mockMvc.perform(get("/rules/999")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isNotFound());
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should create new rule")
    void shouldCreateNewRule() throws Exception {
        when(ruleConfigurationService.createRule(any(RuleEntity.class))).thenReturn(testRule);

        mockMvc.perform(post("/rules")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testRule)))
                .andExpect(status().isCreated())
                .andExpect(jsonPath("$.name").value("HIGH_VALUE_RULE"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should update existing rule")
    void shouldUpdateExistingRule() throws Exception {
        when(ruleConfigurationService.updateRule(eq(1L), any(RuleEntity.class)))
                .thenReturn(Optional.of(testRule));

        mockMvc.perform(put("/rules/1")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testRule)))
                .andExpect(status().isOk());
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should toggle rule enabled status")
    void shouldToggleRuleEnabledStatus() throws Exception {
        testRule.setEnabled(false);
        when(ruleConfigurationService.toggleRuleEnabled(eq(1L), eq(false)))
                .thenReturn(Optional.of(testRule));

        mockMvc.perform(patch("/rules/1/toggle")
                .with(csrf())
                .param("enabled", "false")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk());
    }

    @Test
    @DisplayName("Should require authentication")
    void shouldRequireAuthentication() throws Exception {
        mockMvc.perform(get("/rules")
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isUnauthorized());
    }
}
