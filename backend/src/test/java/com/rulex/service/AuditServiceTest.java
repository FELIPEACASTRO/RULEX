package com.rulex.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.AuditLog;
import com.rulex.repository.AuditLogRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("AuditService Tests")
class AuditServiceTest {

    @Mock
    private AuditLogRepository auditLogRepository;

    private AuditService auditService;
    private ObjectMapper objectMapper;
    private Clock fixedClock;

    @BeforeEach
    void setUp() {
        objectMapper = new ObjectMapper();
        fixedClock = Clock.fixed(Instant.parse("2024-12-30T10:00:00Z"), ZoneId.of("UTC"));
        auditService = new AuditService(auditLogRepository, objectMapper, fixedClock);
    }

    @Test
    @DisplayName("Should log rule created successfully")
    void shouldLogRuleCreatedSuccessfully() {
        when(auditLogRepository.save(any(AuditLog.class))).thenAnswer(inv -> inv.getArgument(0));

        auditService.logRuleCreated("HIGH_VALUE_RULE", "admin");

        ArgumentCaptor<AuditLog> captor = ArgumentCaptor.forClass(AuditLog.class);
        verify(auditLogRepository, times(1)).save(captor.capture());
        
        AuditLog saved = captor.getValue();
        assertEquals(AuditLog.AuditActionType.RULE_CREATED, saved.getActionType());
        assertEquals("admin", saved.getPerformedBy());
        assertEquals(AuditLog.AuditResult.SUCCESS, saved.getResult());
        assertTrue(saved.getDescription().contains("HIGH_VALUE_RULE"));
    }

    @Test
    @DisplayName("Should log rule updated successfully")
    void shouldLogRuleUpdatedSuccessfully() {
        when(auditLogRepository.save(any(AuditLog.class))).thenAnswer(inv -> inv.getArgument(0));

        auditService.logRuleUpdated(
            "HIGH_VALUE_RULE",
            java.util.Map.of("threshold", 5000, "enabled", true),
            "admin"
        );

        ArgumentCaptor<AuditLog> captor = ArgumentCaptor.forClass(AuditLog.class);
        verify(auditLogRepository, times(1)).save(captor.capture());
        
        AuditLog saved = captor.getValue();
        assertEquals(AuditLog.AuditActionType.RULE_UPDATED, saved.getActionType());
        assertEquals("admin", saved.getPerformedBy());
        assertNotNull(saved.getDetails());
    }

    @Test
    @DisplayName("Should log rule deleted successfully")
    void shouldLogRuleDeletedSuccessfully() {
        when(auditLogRepository.save(any(AuditLog.class))).thenAnswer(inv -> inv.getArgument(0));

        auditService.logRuleDeleted("OLD_RULE", "admin");

        ArgumentCaptor<AuditLog> captor = ArgumentCaptor.forClass(AuditLog.class);
        verify(auditLogRepository, times(1)).save(captor.capture());
        
        AuditLog saved = captor.getValue();
        assertEquals(AuditLog.AuditActionType.RULE_DELETED, saved.getActionType());
        assertTrue(saved.getDescription().contains("OLD_RULE"));
    }

    @Test
    @DisplayName("Should log error gracefully")
    void shouldLogErrorGracefully() {
        when(auditLogRepository.save(any(AuditLog.class))).thenAnswer(inv -> inv.getArgument(0));

        auditService.logError("TXN-001", new RuntimeException("Test error"));

        ArgumentCaptor<AuditLog> captor = ArgumentCaptor.forClass(AuditLog.class);
        verify(auditLogRepository, times(1)).save(captor.capture());
        
        AuditLog saved = captor.getValue();
        assertEquals(AuditLog.AuditActionType.TRANSACTION_PROCESSED, saved.getActionType());
        assertEquals(AuditLog.AuditResult.FAILURE, saved.getResult());
        assertEquals("Test error", saved.getErrorMessage());
    }

    @Test
    @DisplayName("Should handle repository exception gracefully")
    void shouldHandleRepositoryExceptionGracefully() {
        when(auditLogRepository.save(any(AuditLog.class))).thenThrow(new RuntimeException("DB error"));

        // Should not throw - logs error internally
        assertDoesNotThrow(() -> auditService.logRuleCreated("RULE", "admin"));
    }
}
