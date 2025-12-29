package com.rulex.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.AnalyzeRequest;
import com.rulex.dto.AnalyzeResponse;
import com.rulex.service.RuleEngineService;
import com.rulex.service.AdvancedRuleEngineService;
import com.rulex.service.TransactionQueryService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(TransactionController.class)
@DisplayName("TransactionController Tests")
class TransactionControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private RuleEngineService ruleEngineService;

    @MockBean
    private AdvancedRuleEngineService advancedRuleEngineService;

    @MockBean
    private TransactionQueryService transactionQueryService;

    private Map<String, Object> testPayload;

    @BeforeEach
    void setUp() {
        testPayload = new HashMap<>();
        testPayload.put("transactionId", "TXN-001");
        testPayload.put("amount", 5000.00);
        testPayload.put("customerId", "CUST-123");
        testPayload.put("merchantId", "MERCH-456");
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should analyze transaction successfully")
    void shouldAnalyzeTransactionSuccessfully() throws Exception {
        AnalyzeResponse response = new AnalyzeResponse();
        response.setDecision("APPROVED");
        response.setScore(0.2);
        
        when(ruleEngineService.analyze(any())).thenReturn(response);

        mockMvc.perform(post("/transactions/analyze")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testPayload)))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.decision").value("APPROVED"));
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should analyze transaction with advanced rules")
    void shouldAnalyzeTransactionWithAdvancedRules() throws Exception {
        AnalyzeResponse response = new AnalyzeResponse();
        response.setDecision("BLOCKED");
        response.setScore(0.9);
        
        when(advancedRuleEngineService.analyzeAdvanced(any())).thenReturn(response);

        mockMvc.perform(post("/transactions/analyze-advanced")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testPayload)))
                .andExpect(status().isOk());
    }

    @Test
    @WithMockUser(roles = "ANALYST")
    @DisplayName("Should allow analyst to analyze transactions")
    void shouldAllowAnalystToAnalyzeTransactions() throws Exception {
        AnalyzeResponse response = new AnalyzeResponse();
        response.setDecision("APPROVED");
        
        when(ruleEngineService.analyze(any())).thenReturn(response);

        mockMvc.perform(post("/transactions/analyze")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testPayload)))
                .andExpect(status().isOk());
    }

    @Test
    @DisplayName("Should require authentication for analyze")
    void shouldRequireAuthenticationForAnalyze() throws Exception {
        mockMvc.perform(post("/transactions/analyze")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(testPayload)))
                .andExpect(status().isUnauthorized());
    }

    @Test
    @WithMockUser(roles = "ADMIN")
    @DisplayName("Should return bad request for invalid payload")
    void shouldReturnBadRequestForInvalidPayload() throws Exception {
        mockMvc.perform(post("/transactions/analyze")
                .with(csrf())
                .contentType(MediaType.APPLICATION_JSON)
                .content("invalid json"))
                .andExpect(status().isBadRequest());
    }
}
