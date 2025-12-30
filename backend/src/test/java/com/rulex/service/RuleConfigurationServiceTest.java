package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.api.NotFoundException;
import com.rulex.dto.RuleConfigurationDTO;
import com.rulex.entity.RuleConfiguration;
import com.rulex.entity.TransactionDecision;
import com.rulex.repository.RuleConfigurationHistoryRepository;
import com.rulex.repository.RuleConfigurationRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("RuleConfigurationService Tests")
class RuleConfigurationServiceTest {

    @Mock
    private RuleConfigurationRepository ruleConfigRepository;

    @Mock
    private RuleConfigurationHistoryRepository historyRepository;

    @Mock
    private AuditService auditService;

    private RuleConfigurationService ruleConfigurationService;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        objectMapper = new ObjectMapper();
        ruleConfigurationService = new RuleConfigurationService(
            ruleConfigRepository,
            historyRepository,
            auditService,
            objectMapper
        );
    }

    @Test
    @DisplayName("Should list all rules with pagination")
    void shouldListAllRulesWithPagination() {
        List<RuleConfiguration> rules = Arrays.asList(
            createTestRule("RULE_1"),
            createTestRule("RULE_2")
        );
        Page<RuleConfiguration> page = new PageImpl<>(rules);
        when(ruleConfigRepository.findAll(any(Pageable.class))).thenReturn(page);

        Page<RuleConfigurationDTO> result = ruleConfigurationService.listRules(PageRequest.of(0, 10));

        assertEquals(2, result.getContent().size());
        verify(ruleConfigRepository).findAll(any(Pageable.class));
    }

    @Test
    @DisplayName("Should get rule by ID")
    void shouldGetRuleById() {
        RuleConfiguration rule = createTestRule("RULE_1");
        rule.setId(1L);
        when(ruleConfigRepository.findById(1L)).thenReturn(Optional.of(rule));

        RuleConfigurationDTO result = ruleConfigurationService.getRuleById(1L);

        assertNotNull(result);
        assertEquals("RULE_1", result.getRuleName());
    }

    @Test
    @DisplayName("Should throw NotFoundException when rule not found")
    void shouldThrowNotFoundExceptionWhenRuleNotFound() {
        when(ruleConfigRepository.findById(999L)).thenReturn(Optional.empty());

        assertThrows(NotFoundException.class, () -> 
            ruleConfigurationService.getRuleById(999L)
        );
    }

    @Test
    @DisplayName("Should create new rule")
    void shouldCreateNewRule() {
        RuleConfiguration savedRule = createTestRule("NEW_RULE");
        savedRule.setId(1L);
        
        when(ruleConfigRepository.findByRuleName("NEW_RULE")).thenReturn(Optional.empty());
        when(ruleConfigRepository.save(any(RuleConfiguration.class))).thenReturn(savedRule);
        when(historyRepository.save(any())).thenReturn(null);

        RuleConfigurationDTO dto = createTestDTO("NEW_RULE");
        RuleConfigurationDTO result = ruleConfigurationService.createRule(dto);

        assertNotNull(result);
        assertEquals("NEW_RULE", result.getRuleName());
        verify(ruleConfigRepository).save(any(RuleConfiguration.class));
        verify(auditService).logRuleCreated(eq("NEW_RULE"), anyString());
    }

    @Test
    @DisplayName("Should throw exception when creating duplicate rule")
    void shouldThrowExceptionWhenCreatingDuplicateRule() {
        when(ruleConfigRepository.findByRuleName("EXISTING_RULE"))
            .thenReturn(Optional.of(createTestRule("EXISTING_RULE")));

        RuleConfigurationDTO dto = createTestDTO("EXISTING_RULE");

        assertThrows(IllegalStateException.class, () -> 
            ruleConfigurationService.createRule(dto)
        );
    }

    @Test
    @DisplayName("Should toggle rule enabled status")
    void shouldToggleRuleEnabledStatus() {
        RuleConfiguration rule = createTestRule("RULE_1");
        rule.setId(1L);
        rule.setEnabled(true);
        
        when(ruleConfigRepository.findById(1L)).thenReturn(Optional.of(rule));
        when(ruleConfigRepository.save(any(RuleConfiguration.class))).thenAnswer(inv -> inv.getArgument(0));
        when(historyRepository.save(any())).thenReturn(null);

        RuleConfigurationDTO result = ruleConfigurationService.toggleRule(1L);

        assertNotNull(result);
        assertFalse(result.getEnabled());
        verify(ruleConfigRepository).save(any(RuleConfiguration.class));
    }

    @Test
    @DisplayName("Should list rules by enabled status")
    void shouldListRulesByEnabledStatus() {
        List<RuleConfiguration> enabledRules = Arrays.asList(createTestRule("ENABLED_RULE"));
        when(ruleConfigRepository.findByEnabled(true)).thenReturn(enabledRules);

        List<RuleConfigurationDTO> result = ruleConfigurationService.listRulesByEnabled(true);

        assertEquals(1, result.size());
        verify(ruleConfigRepository).findByEnabled(true);
    }

    @Test
    @DisplayName("Should delete rule")
    void shouldDeleteRule() {
        RuleConfiguration rule = createTestRule("RULE_TO_DELETE");
        rule.setId(1L);
        
        when(ruleConfigRepository.findById(1L)).thenReturn(Optional.of(rule));
        doNothing().when(ruleConfigRepository).delete(any(RuleConfiguration.class));
        when(historyRepository.save(any())).thenReturn(null);

        assertDoesNotThrow(() -> ruleConfigurationService.deleteRule(1L));
        
        verify(ruleConfigRepository).delete(rule);
        verify(auditService).logRuleDeleted(eq("RULE_TO_DELETE"), anyString());
    }

    private RuleConfiguration createTestRule(String name) {
        return RuleConfiguration.builder()
            .ruleName(name)
            .description("Test rule: " + name)
            .ruleType(RuleConfiguration.RuleType.SECURITY)
            .threshold(1000)
            .weight(50)
            .enabled(true)
            .classification(TransactionDecision.TransactionClassification.SUSPICIOUS)
            .conditionsJson("[]")
            .logicOperator(RuleConfiguration.LogicOperator.AND)
            .version(1)
            .createdAt(LocalDateTime.now())
            .updatedAt(LocalDateTime.now())
            .build();
    }

    private RuleConfigurationDTO createTestDTO(String name) {
        return RuleConfigurationDTO.builder()
            .ruleName(name)
            .description("Test rule: " + name)
            .ruleType("SECURITY")
            .threshold(1000)
            .weight(50)
            .enabled(true)
            .classification("SUSPICIOUS")
            .conditions(List.of())
            .logicOperator("AND")
            .build();
    }
}
