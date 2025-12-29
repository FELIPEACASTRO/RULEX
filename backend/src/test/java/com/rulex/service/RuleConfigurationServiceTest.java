package com.rulex.service;

import com.rulex.entity.RuleEntity;
import com.rulex.repository.RuleRepository;
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
    private RuleRepository ruleRepository;

    private RuleConfigurationService ruleConfigurationService;

    @BeforeEach
    void setUp() {
        ruleConfigurationService = new RuleConfigurationService(ruleRepository);
    }

    @Test
    @DisplayName("Should get all rules with pagination")
    void shouldGetAllRulesWithPagination() {
        List<RuleEntity> rules = Arrays.asList(createTestRule("RULE_1"), createTestRule("RULE_2"));
        Page<RuleEntity> page = new PageImpl<>(rules);
        when(ruleRepository.findAll(any(Pageable.class))).thenReturn(page);

        Page<RuleEntity> result = ruleConfigurationService.getAllRules(PageRequest.of(0, 10));

        assertEquals(2, result.getContent().size());
        verify(ruleRepository).findAll(any(Pageable.class));
    }

    @Test
    @DisplayName("Should get rule by ID")
    void shouldGetRuleById() {
        RuleEntity rule = createTestRule("RULE_1");
        when(ruleRepository.findById(1L)).thenReturn(Optional.of(rule));

        Optional<RuleEntity> result = ruleConfigurationService.getRuleById(1L);

        assertTrue(result.isPresent());
        assertEquals("RULE_1", result.get().getName());
    }

    @Test
    @DisplayName("Should return empty when rule not found")
    void shouldReturnEmptyWhenRuleNotFound() {
        when(ruleRepository.findById(999L)).thenReturn(Optional.empty());

        Optional<RuleEntity> result = ruleConfigurationService.getRuleById(999L);

        assertTrue(result.isEmpty());
    }

    @Test
    @DisplayName("Should create new rule")
    void shouldCreateNewRule() {
        RuleEntity rule = createTestRule("NEW_RULE");
        when(ruleRepository.save(any(RuleEntity.class))).thenReturn(rule);

        RuleEntity result = ruleConfigurationService.createRule(rule);

        assertNotNull(result);
        assertEquals("NEW_RULE", result.getName());
        verify(ruleRepository).save(rule);
    }

    @Test
    @DisplayName("Should update existing rule")
    void shouldUpdateExistingRule() {
        RuleEntity existingRule = createTestRule("RULE_1");
        existingRule.setId(1L);
        when(ruleRepository.findById(1L)).thenReturn(Optional.of(existingRule));
        when(ruleRepository.save(any(RuleEntity.class))).thenReturn(existingRule);

        RuleEntity updatedRule = createTestRule("RULE_1_UPDATED");
        Optional<RuleEntity> result = ruleConfigurationService.updateRule(1L, updatedRule);

        assertTrue(result.isPresent());
        verify(ruleRepository).save(any(RuleEntity.class));
    }

    @Test
    @DisplayName("Should toggle rule enabled status")
    void shouldToggleRuleEnabledStatus() {
        RuleEntity rule = createTestRule("RULE_1");
        rule.setId(1L);
        rule.setEnabled(true);
        when(ruleRepository.findById(1L)).thenReturn(Optional.of(rule));
        when(ruleRepository.save(any(RuleEntity.class))).thenReturn(rule);

        Optional<RuleEntity> result = ruleConfigurationService.toggleRuleEnabled(1L, false);

        assertTrue(result.isPresent());
        verify(ruleRepository).save(any(RuleEntity.class));
    }

    @Test
    @DisplayName("Should get only enabled rules")
    void shouldGetOnlyEnabledRules() {
        List<RuleEntity> enabledRules = Arrays.asList(createTestRule("ENABLED_RULE"));
        when(ruleRepository.findByEnabledTrue()).thenReturn(enabledRules);

        List<RuleEntity> result = ruleConfigurationService.getEnabledRules();

        assertEquals(1, result.size());
        verify(ruleRepository).findByEnabledTrue();
    }

    private RuleEntity createTestRule(String name) {
        RuleEntity rule = new RuleEntity();
        rule.setName(name);
        rule.setDescription("Test rule: " + name);
        rule.setEnabled(true);
        rule.setExpression("amount > 1000");
        return rule;
    }
}
