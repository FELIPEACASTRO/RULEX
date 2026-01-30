package com.rulex.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.rulex.entity.WebhookDeliveryFailure;
import com.rulex.repository.WebhookDeliveryFailureRepository;
import io.micrometer.core.instrument.simple.SimpleMeterRegistry;
import java.time.OffsetDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
class WebhookRetryServiceTest {

  @Mock private WebhookDeliveryFailureRepository repository;
  @Mock private WebhookClient webhookClient;
  @Mock private ObjectMapper objectMapper;

  @Test
  void processRetries_usesExternalServiceForGet() throws Exception {
    WebhookRetryService service =
        new WebhookRetryService(repository, webhookClient, objectMapper, new SimpleMeterRegistry());

    ReflectionTestUtils.setField(service, "dlqEnabled", true);
    ReflectionTestUtils.setField(service, "batchSize", 10);
    ReflectionTestUtils.setField(service, "retryBackoffMs", 1000L);

    WebhookDeliveryFailure failure =
        WebhookDeliveryFailure.builder()
            .webhookUrl("https://example.com/resource")
            .payload("{}")
            .headers(null)
            .httpMethod("GET")
            .status(WebhookDeliveryFailure.STATUS_PENDING)
            .retryCount(0)
            .maxRetries(3)
            .nextRetryAt(OffsetDateTime.now().minusMinutes(1))
            .build();

    when(repository.findPendingRetriesLocked(any(), any())).thenReturn(List.of(failure));
    when(objectMapper.readValue(eq("{}"),
        org.mockito.ArgumentMatchers.<TypeReference<Map<String, Object>>>any()))
      .thenReturn(new HashMap<>());
    when(webhookClient.callExternalService(eq("https://example.com/resource"), eq(null)))
        .thenReturn(
            WebhookClient.WebhookResult.builder().success(true).statusCode(200).build());

    service.processRetries();

    verify(webhookClient).callExternalService(eq("https://example.com/resource"), eq(null));
    verify(webhookClient, never()).callWebhook(any(), any(), any());
    verify(repository).save(failure);
    assertEquals(WebhookDeliveryFailure.STATUS_SUCCESS, failure.getStatus());
  }
}
