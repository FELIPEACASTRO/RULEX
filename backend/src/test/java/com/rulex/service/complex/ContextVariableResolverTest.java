package com.rulex.service.complex;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.dto.TransactionRequest;
import com.rulex.entity.complex.RuleContextVariable;
import com.rulex.repository.TransactionRepository;
import com.rulex.service.VelocityService;
import com.rulex.service.WebhookClient;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.jdbc.core.JdbcTemplate;

@ExtendWith(MockitoExtension.class)
@DisplayName("ContextVariableResolver Tests")
class ContextVariableResolverTest {

  @Mock private TransactionRepository transactionRepository;
  @Mock private VelocityService velocityService;
  @Mock private WebhookClient webhookClient;
  @Mock private JdbcTemplate jdbcTemplate;

  private ContextVariableResolver resolver;

  @BeforeEach
  void setUp() {
    resolver =
        new ContextVariableResolver(
            transactionRepository, velocityService, webhookClient, jdbcTemplate, new ObjectMapper());
  }

  @Test
  @DisplayName("resolveLookup should return value from table when identifiers are safe")
  void resolveLookupTableReturnsValue() {
    Map<String, Object> config =
        Map.of(
            "type",
            "table",
            "table",
            "blacklist",
            "keyField",
            "pan",
            "valueField",
            "risk_level",
            "key",
            "pan");

    RuleContextVariable variable =
        RuleContextVariable.builder()
            .name("riskLevel")
            .sourceType(RuleContextVariable.SourceType.LOOKUP)
            .sourceConfig(config)
            .build();

    Map<String, Object> payload = Map.of("pan", "4111111111111111");

    when(jdbcTemplate.queryForList(anyString(), any(Object[].class)))
        .thenReturn(List.of(Map.of("risk_level", "HIGH")));

    Object result = resolver.resolveLookup(variable, payload, Map.of());

    assertEquals("HIGH", result);
    verify(jdbcTemplate).queryForList(anyString(), any(Object[].class));
  }

  @Test
  @DisplayName("resolveLookup should reject unsafe table identifier")
  void resolveLookupTableRejectsUnsafeIdentifier() {
    Map<String, Object> config =
        Map.of(
            "type",
            "table",
            "table",
            "transactions;drop",
            "keyField",
            "pan",
            "valueField",
            "risk_level",
            "key",
            "pan");

    RuleContextVariable variable =
        RuleContextVariable.builder()
            .name("riskLevel")
            .sourceType(RuleContextVariable.SourceType.LOOKUP)
            .sourceConfig(config)
            .build();

    Map<String, Object> payload = Map.of("pan", "4111111111111111");

    Object result = resolver.resolveLookup(variable, payload, Map.of());

    assertNull(result);
    verify(jdbcTemplate, never()).queryForList(anyString(), any(Object[].class));
  }

  @Test
  @DisplayName("resolveAggregation should reject unsafe identifiers")
  void resolveAggregationRejectsUnsafeIdentifier() {
    Map<String, Object> config =
        Map.of(
            "function",
            "sum",
            "field",
            "transaction_amount",
            "groupBy",
            "merchant_id;drop",
            "periodHours",
            24);

    RuleContextVariable variable =
        RuleContextVariable.builder()
            .name("sumAmount")
            .sourceType(RuleContextVariable.SourceType.AGGREGATION)
            .sourceConfig(config)
            .build();

    Map<String, Object> payload = Map.of("merchant_id;drop", "M1");

    Object result = resolver.resolveAggregation(variable, payload, Map.of());

    assertEquals(BigDecimal.ZERO, result);
    verify(jdbcTemplate, never()).queryForObject(anyString(), eq(BigDecimal.class), any(), any());
  }

  @Test
  @DisplayName("resolveExternalService should extract response field")
  void resolveExternalServiceExtractsField() {
    Map<String, Object> config =
        Map.of(
            "url",
            "http://example.com/${customerId}",
            "method",
            "GET",
            "responseField",
            "score");

    RuleContextVariable variable =
        RuleContextVariable.builder()
            .name("externalScore")
            .sourceType(RuleContextVariable.SourceType.EXTERNAL_SERVICE)
            .sourceConfig(config)
            .build();

    Map<String, Object> payload = Map.of("customerId", "cust-1");

    WebhookClient.WebhookResult response =
        WebhookClient.WebhookResult.builder()
            .success(true)
            .statusCode(200)
            .responseBody("{\"score\": 42}")
            .build();

    when(webhookClient.callExternalService(eq("http://example.com/cust-1"), any()))
        .thenReturn(response);

    Object result = resolver.resolveExternalService(variable, payload, Map.of());

    assertEquals(42, result);
    verify(webhookClient).callExternalService(eq("http://example.com/cust-1"), any());
  }
}