package com.rulex.service;

import com.rulex.entity.DecisionLogEntity;
import com.rulex.repository.DecisionLogRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@DisplayName("AuditService Tests")
class AuditServiceTest {

    @Mock
    private DecisionLogRepository decisionLogRepository;

    private AuditService auditService;

    @BeforeEach
    void setUp() {
        auditService = new AuditService(decisionLogRepository);
    }

    @Test
    @DisplayName("Should log decision successfully")
    void shouldLogDecisionSuccessfully() {
        DecisionLogEntity entity = createTestDecisionLog();
        when(decisionLogRepository.save(any(DecisionLogEntity.class))).thenReturn(entity);

        auditService.logDecision(
            "TXN-001",
            "APPROVED",
            "HIGH_VALUE_RULE",
            "Transaction approved"
        );

        verify(decisionLogRepository, times(1)).save(any(DecisionLogEntity.class));
    }

    @Test
    @DisplayName("Should retrieve decision logs by transaction ID")
    void shouldRetrieveDecisionLogsByTransactionId() {
        List<DecisionLogEntity> logs = Arrays.asList(
            createTestDecisionLog(),
            createTestDecisionLog()
        );
        when(decisionLogRepository.findByTransactionId("TXN-001")).thenReturn(logs);

        List<DecisionLogEntity> result = auditService.getDecisionsByTransactionId("TXN-001");

        assertEquals(2, result.size());
        verify(decisionLogRepository).findByTransactionId("TXN-001");
    }

    @Test
    @DisplayName("Should handle empty result gracefully")
    void shouldHandleEmptyResultGracefully() {
        when(decisionLogRepository.findByTransactionId("UNKNOWN")).thenReturn(List.of());

        List<DecisionLogEntity> result = auditService.getDecisionsByTransactionId("UNKNOWN");

        assertTrue(result.isEmpty());
    }

    private DecisionLogEntity createTestDecisionLog() {
        DecisionLogEntity entity = new DecisionLogEntity();
        entity.setId(1L);
        entity.setTransactionId("TXN-001");
        entity.setDecision("APPROVED");
        entity.setRuleName("HIGH_VALUE_RULE");
        entity.setDetails("Test decision");
        entity.setCreatedAt(LocalDateTime.now());
        return entity;
    }
}
