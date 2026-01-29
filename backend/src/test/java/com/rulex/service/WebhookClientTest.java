package com.rulex.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.lang.reflect.Field;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

@DisplayName("WebhookClient Tests")
class WebhookClientTest {

  @Test
  @DisplayName("Should short-circuit when webhooks are disabled")
  void callWebhookDisabled() throws Exception {
    WebhookClient client = new WebhookClient(new ObjectMapper());
    setField(client, "webhooksEnabled", false);

    WebhookClient.WebhookResult result =
        client.callWebhook("http://example.com", Map.of("a", "b"), Map.of());

    assertFalse(result.isSuccess());
    assertEquals("Webhooks desabilitados", result.getErrorMessage());
  }

  @Test
  @DisplayName("Should short-circuit external calls when disabled")
  void callExternalServiceDisabled() throws Exception {
    WebhookClient client = new WebhookClient(new ObjectMapper());
    setField(client, "webhooksEnabled", false);

    WebhookClient.WebhookResult result = client.callExternalService("http://example.com", Map.of());

    assertFalse(result.isSuccess());
    assertEquals("Serviços externos desabilitados", result.getErrorMessage());
  }

  private static void setField(Object target, String name, Object value) throws Exception {
    Field field = target.getClass().getDeclaredField(name);
    field.setAccessible(true);
    field.set(target, value);
  }
}